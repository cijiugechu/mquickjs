use crate::array_buffer::ArrayBuffer;
use crate::array_data::ArrayData;
use crate::containers::{StringHeader, ValueArrayHeader, VarRefHeader, JS_VALUE_ARRAY_SIZE_MAX};
use crate::context::JSContext;
use crate::conversion;
use crate::enums::{JSObjectClass, JSPropType};
use crate::jsvalue::{
    from_bits, is_int, new_short_int, raw_bits, value_from_ptr, value_get_int, value_to_ptr,
    JSValue, JSWord, JSW, JS_NULL, JS_UNDEFINED, JS_UNINITIALIZED,
};
use crate::memblock::{MbHeader, MTag};
use crate::object::{Object, ObjectHeader};
use crate::string::runtime::string_view;
use crate::typed_array::TypedArray;
use core::mem::size_of;
use core::ptr::{self, NonNull};

// Size log2 for each TypedArray type (indexed by class_id - Uint8CArray)
// Uint8C, Int8, Uint8, Int16, Uint16, Int32, Uint32, Float32, Float64
const TYPED_ARRAY_SIZE_LOG2: [u8; 9] = [0, 0, 0, 1, 1, 2, 2, 2, 3];

// C: `JSProperty` bitfields in mquickjs.c (`hash_next:30`, `prop_type:2`).
#[repr(transparent)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct PropertyMeta(u32);

impl PropertyMeta {
    pub const HASH_NEXT_BITS: u32 = 30;
    pub const HASH_NEXT_MASK: u32 = (1 << Self::HASH_NEXT_BITS) - 1;

    pub fn new(hash_next: u32, prop_type: JSPropType) -> Self {
        debug_assert!(hash_next <= Self::HASH_NEXT_MASK);
        let type_bits = (prop_type as u32) & 0x3;
        Self((hash_next & Self::HASH_NEXT_MASK) | (type_bits << Self::HASH_NEXT_BITS))
    }

    pub const fn from_raw(raw: u32) -> Self {
        Self(raw)
    }

    pub const fn raw(self) -> u32 {
        self.0
    }

    pub const fn hash_next(self) -> u32 {
        self.0 & Self::HASH_NEXT_MASK
    }

    pub fn prop_type(self) -> JSPropType {
        match (self.0 >> Self::HASH_NEXT_BITS) & 0x3 {
            0 => JSPropType::Normal,
            1 => JSPropType::GetSet,
            2 => JSPropType::VarRef,
            _ => JSPropType::Special,
        }
    }

    pub fn with_hash_next(self, hash_next: u32) -> Self {
        debug_assert!(hash_next <= Self::HASH_NEXT_MASK);
        let raw = (self.0 & !Self::HASH_NEXT_MASK) | (hash_next & Self::HASH_NEXT_MASK);
        Self(raw)
    }

    pub fn with_prop_type(self, prop_type: JSPropType) -> Self {
        let raw = (self.0 & Self::HASH_NEXT_MASK) | ((prop_type as u32 & 0x3) << Self::HASH_NEXT_BITS);
        Self(raw)
    }
}

// C: `JSProperty` in mquickjs.c.
#[repr(C)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Property {
    key: JSValue,
    value: JSValue,
    meta: PropertyMeta,
}

impl Property {
    pub const fn new(key: JSValue, value: JSValue, meta: PropertyMeta) -> Self {
        Self { key, value, meta }
    }

    pub const fn key(self) -> JSValue {
        self.key
    }

    pub const fn value(self) -> JSValue {
        self.value
    }

    pub const fn meta(self) -> PropertyMeta {
        self.meta
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PropertyError {
    OutOfMemory,
    NotObject,
    InvalidValueArray,
    Unsupported(&'static str),
}

// Property array layout (JSValueArray):
// - arr[0]: prop_count (short int), count of non-deleted properties.
// - arr[1]: hash_mask = hash_size - 1 (short int).
// - arr[2..=2+hash_mask]: hash table heads (encoded indices, 0 = end).
// - arr[prop_base..]: JSProperty entries packed into 3 JSValue slots each.
//   Deleted entries have key == JS_UNINITIALIZED.
//   If last entry key == JS_UNINITIALIZED, its hash_next encodes first_free << 1.
struct ValueArrayRaw {
    base: NonNull<u8>,
    arr: NonNull<JSValue>,
    size: usize,
}

impl ValueArrayRaw {
    unsafe fn from_value(value: JSValue) -> Result<Self, PropertyError> {
        let ptr = value_to_ptr::<u8>(value).ok_or(PropertyError::InvalidValueArray)?;
        unsafe { Self::from_ptr(ptr) }
    }

    unsafe fn from_ptr(base: NonNull<u8>) -> Result<Self, PropertyError> {
        let header_word = unsafe { ptr::read_unaligned(base.as_ptr().cast::<JSWord>()) };
        let header = MbHeader::from_word(header_word);
        if header.tag() != MTag::ValueArray {
            return Err(PropertyError::InvalidValueArray);
        }
        let header = ValueArrayHeader::from(header);
        let size = header.size() as usize;
        let arr_ptr = unsafe { base.as_ptr().add(size_of::<JSWord>()) as *mut JSValue };
        let arr = NonNull::new(arr_ptr).ok_or(PropertyError::InvalidValueArray)?;
        Ok(Self { base, arr, size })
    }

    fn size(&self) -> usize {
        self.size
    }

    fn read(&self, index: usize) -> JSValue {
        debug_assert!(index < self.size);
        unsafe {
            // SAFETY: caller ensures index is within the value array size.
            ptr::read_unaligned(self.arr.as_ptr().add(index))
        }
    }

    fn write(&self, index: usize, value: JSValue) {
        debug_assert!(index < self.size);
        unsafe {
            // SAFETY: caller ensures index is within the value array size.
            ptr::write_unaligned(self.arr.as_ptr().add(index), value);
        }
    }

    fn set_size(&mut self, size: usize) {
        debug_assert!(size <= JS_VALUE_ARRAY_SIZE_MAX as usize);
        let header_word = unsafe {
            // SAFETY: base points to a readable ValueArray header word.
            ptr::read_unaligned(self.base.as_ptr().cast::<JSWord>())
        };
        let header = MbHeader::from_word(header_word);
        let gc_mark = header.gc_mark();
        let new_header = ValueArrayHeader::new(size as JSWord, gc_mark);
        unsafe {
            // SAFETY: base points to a writable ValueArray header word.
            ptr::write_unaligned(self.base.as_ptr().cast::<JSWord>(), MbHeader::from(new_header).word());
        }
        self.size = size;
    }
}

struct PropertyList {
    array: ValueArrayRaw,
}

impl PropertyList {
    fn from_value(value: JSValue) -> Result<Self, PropertyError> {
        let array = unsafe { ValueArrayRaw::from_value(value)? };
        Ok(Self { array })
    }

    fn prop_count(&self) -> usize {
        let val = self.array.read(0);
        debug_assert!(is_int(val));
        let count = value_get_int(val);
        debug_assert!(count >= 0);
        count as usize
    }

    fn set_prop_count(&self, count: usize) {
        debug_assert!(count <= JS_VALUE_ARRAY_SIZE_MAX as usize);
        let value = new_short_int(count as i32);
        self.array.write(0, value);
    }

    fn hash_mask(&self) -> usize {
        let val = self.array.read(1);
        debug_assert!(is_int(val));
        let mask = value_get_int(val);
        debug_assert!(mask >= 0);
        mask as usize
    }

    fn set_hash_mask(&self, mask: usize) {
        debug_assert!(mask <= JS_VALUE_ARRAY_SIZE_MAX as usize);
        let value = new_short_int(mask as i32);
        self.array.write(1, value);
    }

    fn prop_base(&self) -> usize {
        2 + self.hash_mask() + 1
    }

    fn last_prop_index(&self) -> usize {
        self.array.size() - 3
    }

    fn hash_head_raw(&self, h: usize) -> u32 {
        raw_bits(self.array.read(2 + h)) as u32
    }

    fn set_hash_head_raw(&self, h: usize, raw: u32) {
        self.array.write(2 + h, from_bits(raw as JSWord));
    }

    fn property_ref(&self, index: usize) -> PropertyRef {
        debug_assert!(index + 3 <= self.array.size());
        let ptr = unsafe {
            // SAFETY: index is within the array bounds and points to a property entry.
            self.array.arr.as_ptr().add(index) as *mut Property
        };
        PropertyRef {
            ptr: NonNull::new(ptr).expect("property entry pointer must be non-null"),
        }
    }
}

#[derive(Copy, Clone)]
struct PropertyRef {
    ptr: NonNull<Property>,
}

impl PropertyRef {
    fn key(self) -> JSValue {
        unsafe {
            // SAFETY: property pointer is valid for reads.
            ptr::read_unaligned(ptr::addr_of!((*self.ptr.as_ptr()).key))
        }
    }

    fn set_key(self, value: JSValue) {
        unsafe {
            // SAFETY: property pointer is valid for writes.
            ptr::write_unaligned(ptr::addr_of_mut!((*self.ptr.as_ptr()).key), value);
        }
    }

    fn value(self) -> JSValue {
        unsafe {
            // SAFETY: property pointer is valid for reads.
            ptr::read_unaligned(ptr::addr_of!((*self.ptr.as_ptr()).value))
        }
    }

    fn set_value(self, value: JSValue) {
        unsafe {
            // SAFETY: property pointer is valid for writes.
            ptr::write_unaligned(ptr::addr_of_mut!((*self.ptr.as_ptr()).value), value);
        }
    }

    fn meta(self) -> PropertyMeta {
        unsafe {
            // SAFETY: property pointer is valid for reads.
            ptr::read_unaligned(ptr::addr_of!((*self.ptr.as_ptr()).meta))
        }
    }

    fn set_meta(self, meta: PropertyMeta) {
        unsafe {
            // SAFETY: property pointer is valid for writes.
            ptr::write_unaligned(ptr::addr_of_mut!((*self.ptr.as_ptr()).meta), meta);
        }
    }
}

fn encode_prop_index(index: usize) -> u32 {
    debug_assert!(index <= (PropertyMeta::HASH_NEXT_MASK as usize / 2));
    (index as u32) << 1
}

fn decode_prop_index(raw: u32) -> Option<usize> {
    if raw == 0 {
        None
    } else {
        Some((raw >> 1) as usize)
    }
}

fn hash_prop(prop: JSValue) -> u32 {
    let raw = raw_bits(prop);
    let word = JSW as JSWord;
    ((raw / word) ^ (raw % word)) as u32
}

fn get_prop_hash_size_log2(prop_count: usize) -> usize {
    if prop_count <= 1 {
        return 0;
    }
    let value = (prop_count - 1) as u32;
    (u32::BITS - value.leading_zeros() - 1) as usize
}

fn object_ptr(value: JSValue) -> Result<NonNull<Object>, PropertyError> {
    let ptr = value_to_ptr::<u8>(value).ok_or(PropertyError::NotObject)?;
    let header_word = unsafe {
        // SAFETY: ptr points to a readable memblock header.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    let header = ObjectHeader::from_word(header_word);
    if header.tag() != MTag::Object {
        return Err(PropertyError::NotObject);
    }
    Ok(unsafe {
        // SAFETY: header tag confirms ptr is an Object allocation.
        NonNull::new_unchecked(ptr.as_ptr() as *mut Object)
    })
}

fn object_header(ptr: NonNull<Object>) -> ObjectHeader {
    let header_word = unsafe {
        // SAFETY: ptr points to a readable Object header word.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    ObjectHeader::from_word(header_word)
}

fn object_proto(ptr: NonNull<Object>) -> JSValue {
    unsafe {
        // SAFETY: ptr points to a readable Object proto field.
        ptr::read_unaligned(Object::proto_ptr(ptr.as_ptr()))
    }
}

fn object_props(ptr: NonNull<Object>) -> JSValue {
    unsafe {
        // SAFETY: ptr points to a readable Object props field.
        ptr::read_unaligned(Object::props_ptr(ptr.as_ptr()))
    }
}

fn set_object_props(ptr: NonNull<Object>, value: JSValue) {
    unsafe {
        // SAFETY: ptr points to a writable Object props field.
        ptr::write_unaligned(Object::props_ptr(ptr.as_ptr()), value);
    }
}

fn array_data_ptr(ptr: NonNull<Object>) -> *mut ArrayData {
    let payload = unsafe {
        // SAFETY: ptr points to a valid Object payload.
        Object::payload_ptr(ptr.as_ptr())
    };
    unsafe { ptr::addr_of_mut!((*payload).array) }
}

fn typed_array_ptr(ptr: NonNull<Object>) -> *mut TypedArray {
    let payload = unsafe {
        // SAFETY: ptr points to a valid Object payload.
        Object::payload_ptr(ptr.as_ptr())
    };
    unsafe { ptr::addr_of_mut!((*payload).typed_array) }
}

fn array_buffer_ptr(ptr: NonNull<Object>) -> *mut ArrayBuffer {
    let payload = unsafe {
        // SAFETY: ptr points to a valid Object payload.
        Object::payload_ptr(ptr.as_ptr())
    };
    unsafe { ptr::addr_of_mut!((*payload).array_buffer) }
}

// Get pointer to byte array data from ArrayBuffer's byte_buffer field
fn get_byte_array_ptr(byte_buffer: JSValue) -> Option<NonNull<u8>> {
    let ptr = value_to_ptr::<u8>(byte_buffer)?;
    let header_word = unsafe { ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>()) };
    let header = MbHeader::from_word(header_word);
    if header.tag() != MTag::ByteArray {
        return None;
    }
    // Payload starts after the header
    let data_ptr = unsafe { ptr.as_ptr().add(size_of::<JSWord>()) };
    NonNull::new(data_ptr)
}

// Read element from TypedArray with context (for float allocation)
fn typed_array_get_element_ctx(
    ctx: &JSContext,
    ta: TypedArray,
    idx: u32,
    class_id: u8,
) -> Result<JSValue, PropertyError> {
    if idx >= ta.len() {
        return Ok(JS_UNDEFINED);
    }
    
    // Get ArrayBuffer object
    let ab_ptr = value_to_ptr::<Object>(ta.buffer()).ok_or(PropertyError::InvalidValueArray)?;
    let ab_data = unsafe {
        ptr::read_unaligned(array_buffer_ptr(ab_ptr))
    };
    
    // Get byte array data
    let data_ptr = get_byte_array_ptr(ab_data.byte_buffer())
        .ok_or(PropertyError::InvalidValueArray)?;
    
    let size_log2 = TYPED_ARRAY_SIZE_LOG2[(class_id - JSObjectClass::Uint8CArray as u8) as usize];
    let byte_idx = ((ta.offset() + idx) as usize) << size_log2;
    
    let result = unsafe {
        let ptr = data_ptr.as_ptr().add(byte_idx);
        match class_id {
            c if c == JSObjectClass::Uint8CArray as u8 => {
                new_short_int(*ptr as i32)
            }
            c if c == JSObjectClass::Uint8Array as u8 => {
                new_short_int(*ptr as i32)
            }
            c if c == JSObjectClass::Int8Array as u8 => {
                new_short_int(*(ptr as *const i8) as i32)
            }
            c if c == JSObjectClass::Uint16Array as u8 => {
                new_short_int(ptr::read_unaligned(ptr as *const u16) as i32)
            }
            c if c == JSObjectClass::Int16Array as u8 => {
                new_short_int(ptr::read_unaligned(ptr as *const i16) as i32)
            }
            c if c == JSObjectClass::Uint32Array as u8 => {
                let v = ptr::read_unaligned(ptr as *const u32);
                if v <= i32::MAX as u32 {
                    new_short_int(v as i32)
                } else {
                    // Need to allocate as float for large values
                    // This requires mutable context, so we handle it specially
                    return Err(PropertyError::Unsupported("large uint32"));
                }
            }
            c if c == JSObjectClass::Int32Array as u8 => {
                new_short_int(ptr::read_unaligned(ptr as *const i32))
            }
            c if c == JSObjectClass::Float32Array as u8 => {
                let f = ptr::read_unaligned(ptr as *const f32);
                // For now, return as short int if it fits, else error
                // Full float support requires mutable context for allocation
                let f64_val = f as f64;
                if f64_val.fract() == 0.0 && f64_val >= i32::MIN as f64 && f64_val <= i32::MAX as f64 {
                    new_short_int(f64_val as i32)
                } else {
                    return Err(PropertyError::Unsupported("float value"));
                }
            }
            c if c == JSObjectClass::Float64Array as u8 => {
                let f = ptr::read_unaligned(ptr as *const f64);
                if f.fract() == 0.0 && f >= i32::MIN as f64 && f <= i32::MAX as f64 {
                    new_short_int(f as i32)
                } else {
                    return Err(PropertyError::Unsupported("float value"));
                }
            }
            _ => return Err(PropertyError::Unsupported("unknown typed array type")),
        }
    };
    let _ = ctx; // suppress unused warning
    Ok(result)
}

// Write element to TypedArray at given index
fn typed_array_set_element(
    ctx: &mut JSContext,
    ta: TypedArray,
    idx: u32,
    class_id: u8,
    val: JSValue,
) -> Result<(), PropertyError> {
    if idx >= ta.len() {
        return Err(PropertyError::Unsupported("invalid typed array subscript"));
    }
    
    // Get ArrayBuffer object
    let ab_ptr = value_to_ptr::<Object>(ta.buffer()).ok_or(PropertyError::InvalidValueArray)?;
    let ab_data = unsafe {
        ptr::read_unaligned(array_buffer_ptr(ab_ptr))
    };
    
    // Get byte array data
    let data_ptr = get_byte_array_ptr(ab_data.byte_buffer())
        .ok_or(PropertyError::InvalidValueArray)?;
    
    let size_log2 = TYPED_ARRAY_SIZE_LOG2[(class_id - JSObjectClass::Uint8CArray as u8) as usize];
    let byte_idx = ((ta.offset() + idx) as usize) << size_log2;
    
    // Convert value based on type
    unsafe {
        let ptr = data_ptr.as_ptr().add(byte_idx);
        match class_id {
            c if c == JSObjectClass::Uint8CArray as u8 => {
                // Uint8ClampedArray: clamp to 0-255
                let n = conversion::to_number(ctx, val).map_err(|_| PropertyError::Unsupported("number conversion"))? as i32;
                let clamped = n.clamp(0, 255) as u8;
                *ptr = clamped;
            }
            c if c == JSObjectClass::Uint8Array as u8 => {
                let n = conversion::to_int32(ctx, val).map_err(|_| PropertyError::Unsupported("number conversion"))?;
                *ptr = n as u8;
            }
            c if c == JSObjectClass::Int8Array as u8 => {
                let n = conversion::to_int32(ctx, val).map_err(|_| PropertyError::Unsupported("number conversion"))?;
                *(ptr as *mut i8) = n as i8;
            }
            c if c == JSObjectClass::Uint16Array as u8 => {
                let n = conversion::to_int32(ctx, val).map_err(|_| PropertyError::Unsupported("number conversion"))?;
                ptr::write_unaligned(ptr as *mut u16, n as u16);
            }
            c if c == JSObjectClass::Int16Array as u8 => {
                let n = conversion::to_int32(ctx, val).map_err(|_| PropertyError::Unsupported("number conversion"))?;
                ptr::write_unaligned(ptr as *mut i16, n as i16);
            }
            c if c == JSObjectClass::Uint32Array as u8 => {
                let n = conversion::to_uint32(ctx, val).map_err(|_| PropertyError::Unsupported("number conversion"))?;
                ptr::write_unaligned(ptr as *mut u32, n);
            }
            c if c == JSObjectClass::Int32Array as u8 => {
                let n = conversion::to_int32(ctx, val).map_err(|_| PropertyError::Unsupported("number conversion"))?;
                ptr::write_unaligned(ptr as *mut i32, n);
            }
            c if c == JSObjectClass::Float32Array as u8 => {
                let n = conversion::to_number(ctx, val).map_err(|_| PropertyError::Unsupported("number conversion"))?;
                ptr::write_unaligned(ptr as *mut f32, n as f32);
            }
            c if c == JSObjectClass::Float64Array as u8 => {
                let n = conversion::to_number(ctx, val).map_err(|_| PropertyError::Unsupported("number conversion"))?;
                ptr::write_unaligned(ptr as *mut f64, n);
            }
            _ => return Err(PropertyError::Unsupported("unknown typed array type")),
        }
    }
    Ok(())
}

fn key_to_string(ctx: &mut JSContext, key: JSValue) -> Result<JSValue, PropertyError> {
    if is_int(key) {
        let text = value_get_int(key).to_string();
        return ctx.new_string(&text).map_err(|_| PropertyError::OutOfMemory);
    }
    let mut scratch = [0u8; 5];
    if string_view(key, &mut scratch).is_some() {
        return Ok(key);
    }
    Err(PropertyError::Unsupported("property key"))
}

fn is_numeric_property(prop: JSValue) -> bool {
    let Some(ptr) = value_to_ptr::<u8>(prop) else {
        return false;
    };
    let header_word = unsafe {
        // SAFETY: ptr points to a readable memblock header.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    let header = MbHeader::from_word(header_word);
    if header.tag() != MTag::String {
        return false;
    }
    StringHeader::from(header).is_numeric()
}

fn prop_index_from_value(prop: JSValue) -> Option<u32> {
    if !is_int(prop) {
        return None;
    }
    let idx = value_get_int(prop);
    if idx < 0 {
        None
    } else {
        Some(idx as u32)
    }
}

fn alloc_value_array(ctx: &mut JSContext, size: usize) -> Result<NonNull<u8>, PropertyError> {
    if size > JS_VALUE_ARRAY_SIZE_MAX as usize {
        return Err(PropertyError::OutOfMemory);
    }
    ctx.alloc_value_array(size).map_err(|_| PropertyError::OutOfMemory)
}

fn resize_value_array(
    ctx: &mut JSContext,
    mut val: JSValue,
    new_size: usize,
    prop_base: Option<usize>,
) -> Result<JSValue, PropertyError> {
    let (old_size, old_ptr) = if val == JS_NULL {
        (0usize, None)
    } else {
        let ptr = value_to_ptr::<u8>(val).ok_or(PropertyError::InvalidValueArray)?;
        let array = unsafe { ValueArrayRaw::from_ptr(ptr)? };
        (array.size(), Some(ptr))
    };
    if new_size <= old_size {
        return Ok(val);
    }
    let next_size = old_size + old_size / 2;
    let mut target = new_size;
    if next_size > target {
        target = next_size;
        if let Some(base) = prop_base {
            let align = (target - base) % 3;
            if align != 0 {
                target += 3 - align;
            }
        }
    }
    target = target.max(next_size);
    if target > JS_VALUE_ARRAY_SIZE_MAX as usize {
        return Err(PropertyError::OutOfMemory);
    }
    let new_ptr = alloc_value_array(ctx, target)?;
    if let Some(old_ptr) = old_ptr {
        let old_arr = unsafe { ValueArrayRaw::from_ptr(old_ptr)? };
        let new_arr = unsafe { ValueArrayRaw::from_ptr(new_ptr)? };
        unsafe {
            // SAFETY: both arrays are valid and non-overlapping.
            ptr::copy_nonoverlapping(old_arr.arr.as_ptr(), new_arr.arr.as_ptr(), old_size);
        }
    }
    val = value_from_ptr(new_ptr);
    Ok(val)
}

fn shrink_value_array(ctx: &mut JSContext, val: &mut JSValue, new_size: usize) -> Result<(), PropertyError> {
    if *val == JS_NULL {
        return Ok(());
    }
    let ptr = value_to_ptr::<u8>(*val).ok_or(PropertyError::InvalidValueArray)?;
    let mut array = unsafe { ValueArrayRaw::from_ptr(ptr)? };
    debug_assert!(new_size <= array.size());
    if new_size == 0 {
        unsafe {
            // SAFETY: ptr points to a heap allocation returned by HeapLayout::malloc.
            ctx.heap_mut().free_last(ptr);
        }
        *val = JS_NULL;
        return Ok(());
    }
    let new_bytes = size_of::<JSWord>() + new_size * size_of::<JSValue>();
    unsafe {
        // SAFETY: ptr is a valid memblock start and new_bytes is <= current size.
        ctx.heap_mut().shrink(ptr, new_bytes);
    }
    array.set_size(new_size);
    Ok(())
}

fn alloc_props(ctx: &mut JSContext, n: usize) -> Result<JSValue, PropertyError> {
    debug_assert!(n >= 1);
    let hash_size_log2 = get_prop_hash_size_log2(n);
    let hash_mask = (1 << hash_size_log2) - 1;
    let first_free = 2 + hash_mask + 1;
    let size = first_free + 3 * n;
    let ptr = alloc_value_array(ctx, size)?;
    let list = PropertyList::from_value(value_from_ptr(ptr))?;
    list.set_prop_count(0);
    list.set_hash_mask(hash_mask);
    for i in 0..=hash_mask {
        list.set_hash_head_raw(i, 0);
    }
    for i in 0..n {
        let index = first_free + 3 * i;
        let prop = list.property_ref(index);
        prop.set_key(JS_UNINITIALIZED);
    }
    let last_prop = list.property_ref(size - 3);
    let meta = last_prop.meta().with_hash_next(encode_prop_index(first_free));
    last_prop.set_meta(meta);
    Ok(value_from_ptr(ptr))
}

fn rehash_props(ctx: &JSContext, obj_ptr: NonNull<Object>, gc_rehash: bool) -> Result<(), PropertyError> {
    let props = object_props(obj_ptr);
    let list = PropertyList::from_value(props)?;
    if ctx.is_rom_ptr(list.array.base) {
        return Ok(());
    }
    let hash_mask = list.hash_mask();
    if hash_mask == 0 && gc_rehash {
        return Ok(());
    }
    let prop_count = list.prop_count();
    for i in 0..=hash_mask {
        list.set_hash_head_raw(i, 0);
    }
    let mut i = 0usize;
    let mut j = 0usize;
    let prop_base = list.prop_base();
    while j < prop_count {
        let index = prop_base + 3 * i;
        let prop = list.property_ref(index);
        if prop.key() != JS_UNINITIALIZED {
            let h = (hash_prop(prop.key()) as usize) & hash_mask;
            let head = list.hash_head_raw(h);
            prop.set_meta(prop.meta().with_hash_next(head));
            list.set_hash_head_raw(h, encode_prop_index(index));
            j += 1;
        }
        i += 1;
    }
    Ok(())
}

fn compact_props(ctx: &mut JSContext, obj_ptr: NonNull<Object>) -> Result<(), PropertyError> {
    let mut props = object_props(obj_ptr);
    let list = PropertyList::from_value(props)?;
    let prop_count = list.prop_count();
    if prop_count == 0 {
        if props != ctx.empty_props() {
            set_object_props(obj_ptr, ctx.empty_props());
        }
        return Ok(());
    }
    let hash_mask = list.hash_mask();
    let hash_size_log2 = get_prop_hash_size_log2(prop_count);
    let new_hash_mask = hash_mask.min((1 << hash_size_log2) - 1);
    let new_size = 2 + new_hash_mask + 1 + 3 * prop_count;
    if new_size >= list.array.size() {
        return Ok(());
    }
    list.set_hash_mask(new_hash_mask);
    let mut i = 0usize;
    let mut j = 0usize;
    while j < prop_count {
        let src = list.property_ref(2 + (hash_mask + 1) + 3 * i);
        if src.key() != JS_UNINITIALIZED {
            let dst = list.property_ref(2 + (new_hash_mask + 1) + 3 * j);
            let value = Property::new(src.key(), src.value(), src.meta());
            dst.set_key(value.key());
            dst.set_value(value.value());
            dst.set_meta(value.meta());
            j += 1;
        }
        i += 1;
    }
    shrink_value_array(ctx, &mut props, new_size)?;
    set_object_props(obj_ptr, props);
    rehash_props(ctx, obj_ptr, false)?;
    Ok(())
}

fn get_first_free(list: &PropertyList) -> usize {
    let last = list.property_ref(list.last_prop_index());
    if last.key() == JS_UNINITIALIZED {
        let raw = last.meta().hash_next();
        raw as usize >> 1
    } else {
        list.array.size()
    }
}

fn get_special_prop(ctx: &JSContext, val: JSValue) -> Result<JSValue, PropertyError> {
    let idx = value_get_int(val);
    if idx >= 0 {
        let idx = idx as usize;
        ctx.class_proto()
            .get(idx)
            .copied()
            .ok_or(PropertyError::InvalidValueArray)
    } else {
        let idx = (-idx - 1) as usize;
        ctx.class_obj()
            .get(idx)
            .copied()
            .ok_or(PropertyError::InvalidValueArray)
    }
}

fn update_props(ctx: &mut JSContext, obj: JSValue) -> Result<(), PropertyError> {
    let obj_ptr = object_ptr(obj)?;
    let props = object_props(obj_ptr);
    let list = PropertyList::from_value(props)?;
    if !ctx.is_rom_ptr(list.array.base) {
        return Ok(());
    }
    let size = list.array.size();
    let new_ptr = alloc_value_array(ctx, size)?;
    let new_list = PropertyList::from_value(value_from_ptr(new_ptr))?;
    unsafe {
        // SAFETY: both arrays are valid and non-overlapping.
        ptr::copy_nonoverlapping(list.array.arr.as_ptr(), new_list.array.arr.as_ptr(), size);
    }
    let prop_count = new_list.prop_count();
    let hash_mask = new_list.hash_mask();
    debug_assert_eq!(new_list.array.size(), 2 + (hash_mask + 1) + 3 * prop_count);
    let mut i = 0usize;
    let mut j = 0usize;
    let mut rehash_needed = false;
    while j < prop_count {
        let index = 2 + (hash_mask + 1) + 3 * i;
        let prop = new_list.property_ref(index);
        if prop.key() != JS_UNINITIALIZED {
            if prop.meta().prop_type() == JSPropType::Special {
                let resolved = get_special_prop(ctx, prop.value())?;
                prop.set_value(resolved);
                prop.set_meta(prop.meta().with_prop_type(JSPropType::Normal));
            }
            let mapped = ctx
                .resolve_rom_atom_value(prop.key())
                .map_err(|_| PropertyError::InvalidValueArray)?;
            if mapped != prop.key() {
                prop.set_key(mapped);
                rehash_needed = true;
            }
            j += 1;
        }
        i += 1;
    }
    set_object_props(obj_ptr, value_from_ptr(new_ptr));
    if rehash_needed {
        rehash_props(ctx, obj_ptr, false)?;
    }
    Ok(())
}

fn create_property(ctx: &mut JSContext, obj: JSValue, prop: JSValue) -> Result<PropertyRef, PropertyError> {
    let obj_ptr = object_ptr(obj)?;
    let mut props = object_props(obj_ptr);
    let list = PropertyList::from_value(props)?;
    let mut prop_count = list.prop_count();
    let mut hash_mask = list.hash_mask();
    let mut first_free;
    let last_prop = list.property_ref(list.array.size() - 3);
    if last_prop.key() != JS_UNINITIALIZED {
        if props == ctx.empty_props() {
            let new_props = alloc_props(ctx, 1)?;
            set_object_props(obj_ptr, new_props);
            props = new_props;
            first_free = 3;
        } else {
            first_free = list.array.size();
            let mut new_size = first_free + 3;
            let mut new_hash_mask = hash_mask;
            if (prop_count + 1) > 2 * (hash_mask + 1) {
                new_hash_mask = 2 * (hash_mask + 1) - 1;
                new_size += new_hash_mask - hash_mask;
            }
            let new_props = resize_value_array(
                ctx,
                props,
                new_size,
                Some(2 + new_hash_mask + 1),
            )?;
            set_object_props(obj_ptr, new_props);
            props = new_props;
            if new_hash_mask != hash_mask {
                let new_list = PropertyList::from_value(props)?;
                let old_base = 2 + hash_mask + 1;
                let new_base = 2 + new_hash_mask + 1;
                let count = first_free - old_base;
                unsafe {
                    // SAFETY: ranges are within the value array and may overlap.
                    ptr::copy(
                        new_list.array.arr.as_ptr().add(old_base),
                        new_list.array.arr.as_ptr().add(new_base),
                        count,
                    );
                }
                first_free += new_hash_mask - hash_mask;
                hash_mask = new_hash_mask;
                new_list.set_hash_mask(hash_mask);
                rehash_props(ctx, obj_ptr, false)?;
            }
        }
        let list = PropertyList::from_value(props)?;
        let last_prop = list.property_ref(list.array.size() - 3);
        last_prop.set_key(JS_UNINITIALIZED);
    } else {
        first_free = last_prop.meta().hash_next() as usize >> 1;
    }

    let list = PropertyList::from_value(props)?;
    let entry = list.property_ref(first_free);
    entry.set_key(prop);
    entry.set_value(JS_UNDEFINED);
    let h = (hash_prop(prop) as usize) & hash_mask;
    let head = list.hash_head_raw(h);
    entry.set_meta(PropertyMeta::new(head, JSPropType::Normal));
    list.set_hash_head_raw(h, encode_prop_index(first_free));
    prop_count += 1;
    list.set_prop_count(prop_count);

    first_free += 3;
    if first_free < list.array.size() {
        let last_prop = list.property_ref(list.array.size() - 3);
        last_prop.set_meta(last_prop.meta().with_hash_next(encode_prop_index(first_free)));
    }
    Ok(entry)
}

const DEF_PROP_LOOKUP: u8 = 1 << 0;
const DEF_PROP_RET_VAL: u8 = 1 << 1;

fn alloc_getset_array(
    ctx: &mut JSContext,
    getter: JSValue,
    setter: JSValue,
) -> Result<JSValue, PropertyError> {
    let ptr = alloc_value_array(ctx, 2)?;
    let array = unsafe { ValueArrayRaw::from_ptr(ptr)? };
    array.write(0, getter);
    array.write(1, setter);
    Ok(value_from_ptr(ptr))
}

fn alloc_var_ref(ctx: &mut JSContext, value: JSValue) -> Result<JSValue, PropertyError> {
    let size = size_of::<JSWord>() + size_of::<JSValue>();
    let ptr = ctx
        .heap_mut()
        .malloc(size, MTag::VarRef, |_| {})
        .ok_or(PropertyError::OutOfMemory)?;
    let header = VarRefHeader::new(true, false);
    unsafe {
        // SAFETY: ptr points to a writable var ref allocation.
        ptr::write_unaligned(ptr.as_ptr().cast::<JSWord>(), MbHeader::from(header).word());
        let val_ptr = ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue;
        ptr::write_unaligned(val_ptr, value);
    }
    Ok(value_from_ptr(ptr))
}

fn read_var_ref_value(val: JSValue) -> Result<JSValue, PropertyError> {
    let ptr = value_to_ptr::<u8>(val).ok_or(PropertyError::InvalidValueArray)?;
    let header_word = unsafe {
        // SAFETY: ptr points to a readable memblock header.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    if MbHeader::from_word(header_word).tag() != MTag::VarRef {
        return Err(PropertyError::InvalidValueArray);
    }
    let value_ptr = unsafe { ptr.as_ptr().add(size_of::<JSWord>()) as *const JSValue };
    let value = unsafe {
        // SAFETY: value_ptr lies within the var ref allocation.
        ptr::read_unaligned(value_ptr)
    };
    Ok(value)
}

fn write_var_ref_value(val: JSValue, new_value: JSValue) -> Result<(), PropertyError> {
    let ptr = value_to_ptr::<u8>(val).ok_or(PropertyError::InvalidValueArray)?;
    let header_word = unsafe {
        // SAFETY: ptr points to a readable memblock header.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    if MbHeader::from_word(header_word).tag() != MTag::VarRef {
        return Err(PropertyError::InvalidValueArray);
    }
    let value_ptr = unsafe { ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue };
    unsafe {
        // SAFETY: value_ptr lies within the var ref allocation.
        ptr::write_unaligned(value_ptr, new_value);
    }
    Ok(())
}

fn find_own_property(list: &PropertyList, prop: JSValue) -> Option<PropertyRef> {
    let hash_mask = list.hash_mask();
    let h = (hash_prop(prop) as usize) & hash_mask;
    let mut raw = list.hash_head_raw(h);
    while let Some(idx) = decode_prop_index(raw) {
        let entry = list.property_ref(idx);
        if entry.key() == prop {
            return Some(entry);
        }
        raw = entry.meta().hash_next();
    }
    None
}

fn find_own_property_linear_rom(
    ctx: &JSContext,
    list: &PropertyList,
    prop: JSValue,
) -> Option<(JSValue, PropertyMeta)> {
    let prop_count = list.prop_count();
    let prop_base = list.prop_base();
    let mut seen = 0usize;
    let mut idx = 0usize;
    while seen < prop_count {
        let base = prop_base + 3 * idx;
        let key_word = raw_bits(list.array.read(base));
        let key = ctx.rom_value_from_word(key_word);
        if key != JS_UNINITIALIZED {
            if prop_key_matches(prop, key) {
                let value_word = raw_bits(list.array.read(base + 1));
                let meta_word = raw_bits(list.array.read(base + 2));
                let value = ctx.rom_value_from_word(value_word);
                let meta = PropertyMeta::from_raw(meta_word as u32);
                return Some((value, meta));
            }
            seen += 1;
        }
        idx += 1;
    }
    None
}

fn prop_key_matches(prop: JSValue, key: JSValue) -> bool {
    if prop == key {
        return true;
    }
    let mut scratch_prop = [0u8; 5];
    let mut scratch_key = [0u8; 5];
    let Some(prop_view) = string_view(prop, &mut scratch_prop) else {
        return false;
    };
    let Some(key_view) = string_view(key, &mut scratch_key) else {
        return false;
    };
    prop_view.bytes() == key_view.bytes()
}

fn define_property_internal(
    ctx: &mut JSContext,
    obj: JSValue,
    prop: JSValue,
    mut val: JSValue,
    setter: JSValue,
    prop_type: JSPropType,
    flags: u8,
) -> Result<JSValue, PropertyError> {
    update_props(ctx, obj)?;
    let obj_ptr = object_ptr(obj)?;
    let list = PropertyList::from_value(object_props(obj_ptr))?;
    if (flags & DEF_PROP_LOOKUP) != 0 && let Some(entry) = find_own_property(&list, prop) {
        if entry.meta().prop_type() != prop_type {
            return Err(PropertyError::Unsupported("property kind mismatch"));
        }
        match prop_type {
            JSPropType::Normal => {
                entry.set_value(val);
            }
            JSPropType::GetSet => {
                let arr = unsafe { ValueArrayRaw::from_value(entry.value())? };
                if val != JS_UNDEFINED {
                    arr.write(0, val);
                }
                if setter != JS_UNDEFINED {
                    arr.write(1, setter);
                }
            }
            _ => {
                return Err(PropertyError::Unsupported("property kind update"));
            }
        }
        return Ok(entry.value());
    }

    if prop_type == JSPropType::GetSet {
        val = alloc_getset_array(ctx, val, setter)?;
    } else if prop_type == JSPropType::VarRef {
        val = alloc_var_ref(ctx, val)?;
    }

    let entry = create_property(ctx, obj, prop)?;
    entry.set_meta(entry.meta().with_prop_type(prop_type));
    entry.set_value(val);
    if (flags & DEF_PROP_RET_VAL) != 0 {
        Ok(val)
    } else {
        Ok(JS_UNDEFINED)
    }
}

pub fn define_property_value(
    ctx: &mut JSContext,
    obj: JSValue,
    prop: JSValue,
    val: JSValue,
) -> Result<JSValue, PropertyError> {
    define_property_internal(
        ctx,
        obj,
        prop,
        val,
        JS_NULL,
        JSPropType::Normal,
        DEF_PROP_LOOKUP,
    )
}

pub fn define_property_getset(
    ctx: &mut JSContext,
    obj: JSValue,
    prop: JSValue,
    getter: JSValue,
    setter: JSValue,
) -> Result<JSValue, PropertyError> {
    define_property_internal(
        ctx,
        obj,
        prop,
        getter,
        setter,
        JSPropType::GetSet,
        DEF_PROP_LOOKUP,
    )
}

pub fn define_property_varref(
    ctx: &mut JSContext,
    obj: JSValue,
    prop: JSValue,
    val: JSValue,
) -> Result<JSValue, PropertyError> {
    define_property_internal(
        ctx,
        obj,
        prop,
        val,
        JS_NULL,
        JSPropType::VarRef,
        DEF_PROP_RET_VAL,
    )
}

pub(crate) fn get_own_property_raw(
    ctx: &JSContext,
    obj: JSValue,
    prop: JSValue,
) -> Result<Option<(JSValue, JSPropType)>, PropertyError> {
    let obj_ptr = object_ptr(obj)?;
    let list = PropertyList::from_value(object_props(obj_ptr))?;
    if ctx.is_rom_ptr(list.array.base) {
        if let Some((value, meta)) = find_own_property_linear_rom(ctx, &list, prop) {
            return Ok(Some((value, meta.prop_type())));
        }
    } else if let Some(entry) = find_own_property(&list, prop) {
        return Ok(Some((entry.value(), entry.meta().prop_type())));
    }
    Ok(None)
}

pub fn get_property(ctx: &JSContext, obj: JSValue, prop: JSValue) -> Result<JSValue, PropertyError> {
    let mut current = obj;
    loop {
        let obj_ptr = object_ptr(current)?;
        let header = object_header(obj_ptr);
        let class_id = header.class_id();
        if class_id == JSObjectClass::Array as u8 {
            if let Some(idx) = prop_index_from_value(prop) {
                let data = unsafe {
                    // SAFETY: array_data_ptr returns a valid ArrayData pointer.
                    ptr::read_unaligned(array_data_ptr(obj_ptr))
                };
                if idx < data.len() {
                    let tab = data.tab();
                    if tab != JS_NULL {
                        let array = unsafe { ValueArrayRaw::from_value(tab)? };
                        return Ok(array.read(idx as usize));
                    }
                }
            } else if is_numeric_property(prop) {
                return Ok(JS_UNDEFINED);
            }
        } else if (JSObjectClass::Uint8CArray as u8..=JSObjectClass::Float64Array as u8).contains(&class_id) {
            if let Some(idx) = prop_index_from_value(prop) {
                let ta = unsafe {
                    // SAFETY: typed_array_ptr returns a valid TypedArray pointer.
                    ptr::read_unaligned(typed_array_ptr(obj_ptr))
                };
                return typed_array_get_element_ctx(ctx, ta, idx, class_id);
            } else if is_numeric_property(prop) {
                return Ok(JS_UNDEFINED);
            }
        }
        let list = PropertyList::from_value(object_props(obj_ptr))?;
        if ctx.is_rom_ptr(list.array.base) {
            if let Some((value, meta)) = find_own_property_linear_rom(ctx, &list, prop) {
                return match meta.prop_type() {
                    JSPropType::Normal => Ok(value),
                    JSPropType::VarRef => read_var_ref_value(value),
                    JSPropType::Special => get_special_prop(ctx, value),
                    JSPropType::GetSet => Err(PropertyError::Unsupported("getter/setter")),
                };
            }
        } else if let Some(entry) = find_own_property(&list, prop) {
            return match entry.meta().prop_type() {
                JSPropType::Normal => Ok(entry.value()),
                JSPropType::VarRef => read_var_ref_value(entry.value()),
                JSPropType::Special => get_special_prop(ctx, entry.value()),
                JSPropType::GetSet => Err(PropertyError::Unsupported("getter/setter")),
            };
        }
        let proto = object_proto(obj_ptr);
        if proto == JS_NULL {
            break;
        }
        current = proto;
    }
    Ok(JS_UNDEFINED)
}

pub fn object_keys(ctx: &mut JSContext, obj: JSValue) -> Result<JSValue, PropertyError> {
    let obj_ptr = object_ptr(obj)?;
    let header = object_header(obj_ptr);
    let class_id = header.class_id();
    let mut array_len = 0u32;
    if class_id == JSObjectClass::Array as u8 {
        let data = unsafe {
            // SAFETY: array_data_ptr returns a valid ArrayData pointer.
            ptr::read_unaligned(array_data_ptr(obj_ptr))
        };
        array_len = data.len();
    } else if (JSObjectClass::Uint8CArray as u8..=JSObjectClass::Float64Array as u8)
        .contains(&class_id)
    {
        let data = unsafe {
            // SAFETY: typed_array_ptr returns a valid TypedArray pointer.
            ptr::read_unaligned(typed_array_ptr(obj_ptr))
        };
        array_len = data.len();
    }

    let list = PropertyList::from_value(object_props(obj_ptr))?;
    let prop_count = list.prop_count();
    let alloc_size = array_len as usize + prop_count;
    let ret = ctx.alloc_array(alloc_size).map_err(|_| PropertyError::OutOfMemory)?;

    let mut pos = 0usize;
    for idx in 0..array_len {
        let key = key_to_string(ctx, new_short_int(idx as i32))?;
        set_property(ctx, ret, new_short_int(pos as i32), key)?;
        pos += 1;
    }

    let prop_base = list.prop_base();
    let mut seen = 0usize;
    let mut idx = 0usize;
    while seen < prop_count {
        let base = prop_base + 3 * idx;
        let mut key = list.array.read(base);
        if ctx.is_rom_ptr(list.array.base) {
            key = ctx.rom_value_from_word(raw_bits(key));
        }
        if key != JS_UNINITIALIZED {
            let key = key_to_string(ctx, key)?;
            set_property(ctx, ret, new_short_int(pos as i32), key)?;
            pos += 1;
            seen += 1;
        }
        idx += 1;
    }

    if pos != alloc_size {
        let ret_ptr = object_ptr(ret)?;
        let data_ptr = array_data_ptr(ret_ptr);
        let data = unsafe {
            // SAFETY: data_ptr points to a readable ArrayData.
            ptr::read_unaligned(data_ptr)
        };
        let new_len = pos as u32;
        if data.len() != new_len {
            unsafe {
                // SAFETY: data_ptr points to a writable ArrayData.
                ptr::write_unaligned(data_ptr, ArrayData::new(data.tab(), new_len));
            }
        }
    }

    Ok(ret)
}

pub fn has_property(ctx: &JSContext, obj: JSValue, prop: JSValue) -> Result<bool, PropertyError> {
    let mut current = obj;
    loop {
        let obj_ptr = object_ptr(current)?;
        let list = PropertyList::from_value(object_props(obj_ptr))?;
        let found = if ctx.is_rom_ptr(list.array.base) {
            find_own_property_linear_rom(ctx, &list, prop).is_some()
        } else {
            find_own_property(&list, prop).is_some()
        };
        if found {
            return Ok(true);
        }
        let proto = object_proto(obj_ptr);
        if proto == JS_NULL {
            break;
        }
        current = proto;
    }
    Ok(false)
}

/// Checks if an object has its own property (not inherited from prototype chain).
pub fn find_own_property_exposed(
    ctx: &JSContext,
    obj: JSValue,
    prop: JSValue,
) -> Result<bool, PropertyError> {
    let obj_ptr = object_ptr(obj)?;
    let header = object_header(obj_ptr);
    let class_id = header.class_id();

    // Check array indices for arrays and typed arrays.
    if class_id == JSObjectClass::Array as u8 {
        if let Some(idx) = prop_index_from_value(prop) {
            let data = unsafe {
                // SAFETY: array_data_ptr returns a valid ArrayData pointer.
                ptr::read_unaligned(array_data_ptr(obj_ptr))
            };
            return Ok(idx < data.len());
        }
    } else if (JSObjectClass::Uint8CArray as u8..=JSObjectClass::Float64Array as u8)
        .contains(&class_id)
    {
        if let Some(idx) = prop_index_from_value(prop) {
            let data = unsafe {
                // SAFETY: typed_array_ptr returns a valid TypedArray pointer.
                ptr::read_unaligned(typed_array_ptr(obj_ptr))
            };
            return Ok(idx < data.len());
        }
    }

    let list = PropertyList::from_value(object_props(obj_ptr))?;
    if ctx.is_rom_ptr(list.array.base) {
        Ok(find_own_property_linear_rom(ctx, &list, prop).is_some())
    } else {
        Ok(find_own_property(&list, prop).is_some())
    }
}

pub fn set_property(
    ctx: &mut JSContext,
    obj: JSValue,
    prop: JSValue,
    val: JSValue,
) -> Result<(), PropertyError> {
    let obj_ptr = object_ptr(obj)?;
    let header = object_header(obj_ptr);
    let class_id = header.class_id();
    if class_id == JSObjectClass::Array as u8 {
        if let Some(idx) = prop_index_from_value(prop) {
            let data_ptr = array_data_ptr(obj_ptr);
            let mut data = unsafe {
                // SAFETY: data_ptr points to a valid ArrayData.
                ptr::read_unaligned(data_ptr)
            };
            if idx < data.len() {
                let tab = data.tab();
                let array = unsafe { ValueArrayRaw::from_value(tab)? };
                array.write(idx as usize, val);
                return Ok(());
            } else if idx == data.len() {
                let new_size = idx as usize + 1;
                let new_tab = resize_value_array(ctx, data.tab(), new_size, None)?;
                let array = unsafe { ValueArrayRaw::from_value(new_tab)? };
                array.write(idx as usize, val);
                data = ArrayData::new(new_tab, idx + 1);
                unsafe {
                    // SAFETY: data_ptr points to a writable ArrayData.
                    ptr::write_unaligned(data_ptr, data);
                }
                return Ok(());
            }
            return Err(PropertyError::Unsupported("invalid array subscript"));
        } else if is_numeric_property(prop) {
            return Err(PropertyError::Unsupported("invalid array subscript"));
        }
    } else if (JSObjectClass::Uint8CArray as u8..=JSObjectClass::Float64Array as u8).contains(&class_id) {
        if let Some(idx) = prop_index_from_value(prop) {
            let ta = unsafe {
                // SAFETY: typed_array_ptr returns a valid TypedArray pointer.
                ptr::read_unaligned(typed_array_ptr(obj_ptr))
            };
            return typed_array_set_element(ctx, ta, idx, class_id, val);
        } else if is_numeric_property(prop) {
            return Err(PropertyError::Unsupported("invalid typed array subscript"));
        }
    }

    update_props(ctx, obj)?;
    let list = PropertyList::from_value(object_props(obj_ptr))?;
    if let Some(entry) = find_own_property(&list, prop) {
        match entry.meta().prop_type() {
            JSPropType::Normal => {
                entry.set_value(val);
                return Ok(());
            }
            JSPropType::VarRef => {
                write_var_ref_value(entry.value(), val)?;
                return Ok(());
            }
            JSPropType::Special => {
                update_props(ctx, obj)?;
                return set_property(ctx, obj, prop, val);
            }
            JSPropType::GetSet => {
                return Err(PropertyError::Unsupported("setter"));
            }
        }
    }
    define_property_internal(
        ctx,
        obj,
        prop,
        val,
        JS_UNDEFINED,
        JSPropType::Normal,
        0,
    )?;
    Ok(())
}

pub fn delete_property(ctx: &mut JSContext, obj: JSValue, prop: JSValue) -> Result<bool, PropertyError> {
    let obj_ptr = object_ptr(obj)?;
    let props = object_props(obj_ptr);
    let list = PropertyList::from_value(props)?;
    let hash_mask = list.hash_mask();
    let h = (hash_prop(prop) as usize) & hash_mask;
    let mut idx_raw = list.hash_head_raw(h);
    let mut last_idx: Option<usize> = None;
    while let Some(idx) = decode_prop_index(idx_raw) {
        let entry = list.property_ref(idx);
        if entry.key() == prop {
            if ctx.is_rom_ptr(list.array.base) {
                update_props(ctx, obj)?;
                return delete_property(ctx, obj, prop);
            }
            if let Some(last) = last_idx {
                let prev = list.property_ref(last);
                prev.set_meta(prev.meta().with_hash_next(entry.meta().hash_next()));
            } else {
                list.set_hash_head_raw(h, entry.meta().hash_next());
            }
            let mut first_free = get_first_free(&list);
            let prop_count = list.prop_count();
            list.set_prop_count(prop_count - 1);
            entry.set_meta(entry.meta().with_prop_type(JSPropType::Normal));
            entry.set_key(JS_UNINITIALIZED);
            entry.set_value(JS_UNDEFINED);
            while first_free > list.prop_base() {
                let prev = list.property_ref(first_free - 3);
                if prev.key() != JS_UNINITIALIZED {
                    break;
                }
                first_free -= 3;
            }
            if first_free < list.array.size() {
                let last_prop = list.property_ref(list.array.size() - 3);
                last_prop.set_meta(last_prop.meta().with_hash_next(encode_prop_index(first_free)));
            }
            if (2 + hash_mask + 1 + 3 * (prop_count - 1)) < list.array.size() / 2 {
                compact_props(ctx, obj_ptr)?;
            }
            return Ok(true);
        }
        last_idx = Some(idx);
        idx_raw = entry.meta().hash_next();
    }
    Ok(true)
}

#[cfg(test)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum DebugProperty {
    Normal(JSValue),
    GetSet { getter: JSValue, setter: JSValue },
    VarRef(JSValue),
    Special(JSValue),
}

#[cfg(test)]
fn debug_property_value(
    meta: PropertyMeta,
    value: JSValue,
) -> Result<DebugProperty, PropertyError> {
    match meta.prop_type() {
        JSPropType::Normal => Ok(DebugProperty::Normal(value)),
        JSPropType::GetSet => {
            let arr = unsafe {
                // SAFETY: get/set properties store a JSValueArray with getter/setter slots.
                ValueArrayRaw::from_value(value)?
            };
            Ok(DebugProperty::GetSet {
                getter: arr.read(0),
                setter: arr.read(1),
            })
        }
        JSPropType::VarRef => Ok(DebugProperty::VarRef(read_var_ref_value(value)?)),
        JSPropType::Special => Ok(DebugProperty::Special(value)),
    }
}

#[cfg(test)]
pub(crate) fn debug_property(
    ctx: &JSContext,
    obj: JSValue,
    prop: JSValue,
) -> Result<Option<DebugProperty>, PropertyError> {
    let obj_ptr = object_ptr(obj)?;
    let list = PropertyList::from_value(object_props(obj_ptr))?;
    if ctx.is_rom_ptr(list.array.base) {
        if let Some((value, meta)) = find_own_property_linear_rom(ctx, &list, prop) {
            return Ok(Some(debug_property_value(meta, value)?));
        }
    } else if let Some(entry) = find_own_property(&list, prop) {
        return Ok(Some(debug_property_value(entry.meta(), entry.value())?));
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;
    use core::mem::{align_of, size_of};

    fn new_ctx() -> JSContext {
        JSContext::new(crate::context::ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
        })
        .expect("context init")
    }

    #[test]
    fn property_meta_roundtrip() {
        let meta = PropertyMeta::new(123, JSPropType::VarRef);
        assert_eq!(meta.hash_next(), 123);
        assert_eq!(meta.prop_type(), JSPropType::VarRef);
    }

    #[test]
    fn property_meta_masks_hash() {
        let meta = PropertyMeta::new(PropertyMeta::HASH_NEXT_MASK, JSPropType::Normal);
        assert_eq!(meta.hash_next(), PropertyMeta::HASH_NEXT_MASK);
    }

    #[test]
    fn property_roundtrip() {
        let meta = PropertyMeta::new(7, JSPropType::GetSet);
        let prop = Property::new(crate::jsvalue::JS_NULL, crate::jsvalue::JS_UNDEFINED, meta);
        assert_eq!(prop.key(), crate::jsvalue::JS_NULL);
        assert_eq!(prop.value(), crate::jsvalue::JS_UNDEFINED);
        assert_eq!(prop.meta(), meta);
    }

    #[test]
    fn property_layout_matches_jsvalue_slots() {
        assert_eq!(size_of::<PropertyMeta>(), size_of::<u32>());
        assert_eq!(size_of::<Property>(), 3 * size_of::<JSValue>());
        assert_eq!(align_of::<Property>(), align_of::<JSValue>());
    }

    #[test]
    fn alloc_props_sets_first_free() {
        let mut ctx = new_ctx();
        let props = alloc_props(&mut ctx, 2).expect("alloc props");
        let list = PropertyList::from_value(props).expect("props list");
        let prop_base = list.prop_base();
        let last = list.property_ref(list.last_prop_index());
        assert_eq!(last.key(), JS_UNINITIALIZED);
        assert_eq!(last.meta().hash_next() as usize >> 1, prop_base);
        assert_eq!(list.prop_count(), 0);
    }

    #[test]
    fn define_and_lookup_property() {
        let mut ctx = new_ctx();
        let obj = ctx
            .alloc_object(JSObjectClass::Object, JS_NULL, 0)
            .expect("object");
        let key = new_short_int(1);
        let value = new_short_int(123);
        define_property_value(&mut ctx, obj, key, value).expect("define");
        let got = get_property(&ctx, obj, key).expect("get");
        assert_eq!(got, value);
        assert!(has_property(&ctx, obj, key).expect("has"));
    }

    #[test]
    fn define_getset_stores_value_array() {
        let mut ctx = new_ctx();
        let obj = ctx
            .alloc_object(JSObjectClass::Object, JS_NULL, 0)
            .expect("object");
        let key = new_short_int(7);
        let getter = new_short_int(1);
        let setter = new_short_int(2);
        define_property_getset(&mut ctx, obj, key, getter, setter).expect("define getset");
        let obj_ptr = object_ptr(obj).expect("object ptr");
        let list = PropertyList::from_value(object_props(obj_ptr)).expect("props");
        let entry = find_own_property(&list, key).expect("property");
        assert_eq!(entry.meta().prop_type(), JSPropType::GetSet);
        let array = unsafe { ValueArrayRaw::from_value(entry.value()).expect("value array") };
        assert_eq!(array.read(0), getter);
        assert_eq!(array.read(1), setter);
    }

    #[test]
    fn define_varref_allocates_detached_ref() {
        let mut ctx = new_ctx();
        let obj = ctx
            .alloc_object(JSObjectClass::Object, JS_NULL, 0)
            .expect("object");
        let key = new_short_int(5);
        let value = new_short_int(9);
        let varref = define_property_varref(&mut ctx, obj, key, value).expect("varref");
        assert_eq!(read_var_ref_value(varref).expect("varref value"), value);
    }

    #[test]
    fn delete_property_removes_entry() {
        let mut ctx = new_ctx();
        let obj = ctx
            .alloc_object(JSObjectClass::Object, JS_NULL, 0)
            .expect("object");
        let key1 = new_short_int(1);
        let key2 = new_short_int(2);
        define_property_value(&mut ctx, obj, key1, new_short_int(10)).expect("define");
        define_property_value(&mut ctx, obj, key2, new_short_int(20)).expect("define");
        delete_property(&mut ctx, obj, key1).expect("delete");
        assert!(!has_property(&ctx, obj, key1).expect("has"));
        assert!(has_property(&ctx, obj, key2).expect("has"));
    }

    #[test]
    fn update_props_copies_rom_and_converts_special() {
        let mut ctx = new_ctx();
        let obj = ctx
            .alloc_object(JSObjectClass::Object, JS_NULL, 0)
            .expect("object");
        let key = new_short_int(3);
        let class_index = new_short_int(JSObjectClass::Object as i32);

        let hash_mask = 0usize;
        let size = 2 + hash_mask + 1 + 3;
        let mut words = vec![0 as JSWord; 1 + size].into_boxed_slice();
        words[0] = MbHeader::from(ValueArrayHeader::new(size as JSWord, false)).word();
        let arr = &mut words[1..];
        arr[0] = raw_bits(new_short_int(1));
        arr[1] = raw_bits(new_short_int(hash_mask as i32));
        arr[2] = raw_bits(new_short_int(3));
        arr[3] = raw_bits(key);
        arr[4] = raw_bits(class_index);
        let meta = PropertyMeta::new(0, JSPropType::Special);
        arr[5] = meta.raw() as JSWord;

        let rom_ptr = NonNull::new(words.as_mut_ptr() as *mut u8).expect("rom ptr");
        let rom_props = value_from_ptr(rom_ptr);
        let obj_ptr = object_ptr(obj).expect("object ptr");
        set_object_props(obj_ptr, rom_props);

        update_props(&mut ctx, obj).expect("update props");
        let props = object_props(obj_ptr);
        let props_ptr = value_to_ptr::<u8>(props).expect("props ptr");
        assert!(!ctx.is_rom_ptr(props_ptr));

        let list = PropertyList::from_value(props).expect("props list");
        let entry = find_own_property(&list, key).expect("property");
        assert_eq!(entry.meta().prop_type(), JSPropType::Normal);
        let expected = ctx.class_proto()[JSObjectClass::Object as usize];
        assert_eq!(entry.value(), expected);
    }
}

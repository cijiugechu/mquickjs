use crate::context::{ContextConfig, ContextError, JSContext};

/// JSRuntime wrapper owning a single JSContext (mquickjs embeds runtime into the context).
pub struct JSRuntime {
    ctx: JSContext,
}

impl JSRuntime {
    pub fn new(config: ContextConfig<'_>) -> Result<Self, ContextError> {
        Ok(Self {
            ctx: JSContext::new(config)?,
        })
    }

    pub fn context(&self) -> &JSContext {
        &self.ctx
    }

    pub fn context_mut(&mut self) -> &mut JSContext {
        &mut self.ctx
    }

    pub fn into_context(self) -> JSContext {
        self.ctx
    }
}

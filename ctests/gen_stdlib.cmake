if(NOT DEFINED MQJS_STDLIB_TOOL)
  message(FATAL_ERROR "MQJS_STDLIB_TOOL is required")
endif()

if(NOT DEFINED OUTPUT)
  message(FATAL_ERROR "OUTPUT is required")
endif()

if(DEFINED ARGS)
  separate_arguments(ARGS)
endif()

execute_process(
  COMMAND "${MQJS_STDLIB_TOOL}" ${ARGS}
  OUTPUT_FILE "${OUTPUT}"
  RESULT_VARIABLE result
)

if(NOT result EQUAL 0)
  message(FATAL_ERROR "mqjs_stdlib failed with exit code ${result}")
endif()

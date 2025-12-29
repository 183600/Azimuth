test "logger_convenience_methods" {
  let provider = NoopLoggerProvider::{}
  let logger = provider.get_logger("test-logger", "1.0.0", "http://example.com/schema")
  
  // 测试各种便捷方法（确保不会崩溃）
  logger.debug("Debug message", [("level", AttributeValue::string("debug"))])
  logger.info("Info message", [("level", AttributeValue::string("info"))])
  logger.warn("Warning message", [("level", AttributeValue::string("warn"))])
  logger.error("Error message", [("level", AttributeValue::string("error"))])
  logger.fatal("Fatal message", [("level", AttributeValue::string("fatal"))])
  
  // 测试不带属性的便捷方法
  logger.debug("Debug message without attributes")
  logger.info("Info message without attributes")
  logger.warn("Warning message without attributes")
  logger.error("Error message without attributes")
  logger.fatal("Fatal message without attributes")
  
  // 测试带复杂属性的便捷方法
  let complex_attributes = [
    ("service.name", AttributeValue::string("test-service")),
    ("service.version", AttributeValue::string("1.0.0")),
    ("request.id", AttributeValue::string("req-123")),
    ("user.id", AttributeValue::int(456L)),
    ("processing.time", AttributeValue::float(123.45)),
    ("cache.hit", AttributeValue::bool(true)),
    ("tags", AttributeValue::array_string(["tag1", "tag2", "tag3"])),
    ("ports", AttributeValue::array_int([8080L, 8443L])),
    ("thresholds", AttributeValue::array_float([0.5, 0.9, 0.99])),
    ("features", AttributeValue::array_bool([true, false, true]))
  ]
  
  logger.info("Complex message", complex_attributes)
  
  // 测试特殊字符和Unicode消息
  logger.debug("Debug with special chars: !@#$%^&*()")
  logger.info("Info with Unicode: 测试消息🚀")
  logger.warn("Warning with newlines\nLine 2\nLine 3")
  logger.error("Error with tabs\tTabbed\tcontent")
  logger.fatal("Fatal with quotes \"quoted text\" and 'single quotes'")
  
  assert_eq(true, true) // 如果没有崩溃，测试通过
}
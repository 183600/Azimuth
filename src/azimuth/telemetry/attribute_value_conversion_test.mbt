// 属性值类型转换测试 - 验证AttributeValue的类型转换和操作

test "attribute_value_creation_and_type_checking" {
  // 测试各种属性值的创建
  let string_val = AttributeValue::string("test-string")
  let int_val = AttributeValue::int(12345L)
  let float_val = AttributeValue::float(67.89)
  let bool_val = AttributeValue::bool(true)
  let array_string_val = AttributeValue::array_string(["a", "b", "c"])
  let array_int_val = AttributeValue::array_int([1L, 2L, 3L, 4L])
  let array_float_val = AttributeValue::array_float([1.1, 2.2, 3.3])
  let array_bool_val = AttributeValue::array_bool([true, false, true, false])
  
  // 验证类型和值
  match string_val {
    StringValue(v) => @assert.eq(v, "test-string")
    _ => @assert.fail("Expected StringValue")
  }
  
  match int_val {
    IntValue(v) => @assert.eq(v, 12345L)
    _ => @assert.fail("Expected IntValue")
  }
  
  match float_val {
    FloatValue(v) => @assert.eq(v, 67.89)
    _ => @assert.fail("Expected FloatValue")
  }
  
  match bool_val {
    BoolValue(v) => @assert.eq(v, true)
    _ => @assert.fail("Expected BoolValue")
  }
  
  match array_string_val {
    ArrayStringValue(v) => {
      @assert.eq(v.length(), 3)
      @assert.eq(v[0], "a")
      @assert.eq(v[1], "b")
      @assert.eq(v[2], "c")
    }
    _ => @assert.fail("Expected ArrayStringValue")
  }
  
  match array_int_val {
    ArrayIntValue(v) => {
      @assert.eq(v.length(), 4)
      @assert.eq(v[0], 1L)
      @assert.eq(v[1], 2L)
      @assert.eq(v[2], 3L)
      @assert.eq(v[3], 4L)
    }
    _ => @assert.fail("Expected ArrayIntValue")
  }
  
  match array_float_val {
    ArrayFloatValue(v) => {
      @assert.eq(v.length(), 3)
      @assert.eq(v[0], 1.1)
      @assert.eq(v[1], 2.2)
      @assert.eq(v[2], 3.3)
    }
    _ => @assert.fail("Expected ArrayFloatValue")
  }
  
  match array_bool_val {
    ArrayBoolValue(v) => {
      @assert.eq(v.length(), 4)
      @assert.eq(v[0], true)
      @assert.eq(v[1], false)
      @assert.eq(v[2], true)
      @assert.eq(v[3], false)
    }
    _ => @assert.fail("Expected ArrayBoolValue")
  }
}

test "attribute_value_edge_cases" {
  // 测试边界情况
  
  // 空字符串
  let empty_string = AttributeValue::string("")
  match empty_string {
    StringValue(v) => @assert.eq(v, "")
    _ => @assert.fail("Expected empty StringValue")
  }
  
  // 零值
  let zero_int = AttributeValue::int(0L)
  let zero_float = AttributeValue::float(0.0)
  let false_bool = AttributeValue::bool(false)
  
  match zero_int {
    IntValue(v) => @assert.eq(v, 0L)
    _ => @assert.fail("Expected zero IntValue")
  }
  
  match zero_float {
    FloatValue(v) => @assert.eq(v, 0.0)
    _ => @assert.fail("Expected zero FloatValue")
  }
  
  match false_bool {
    BoolValue(v) => @assert.eq(v, false)
    _ => @assert.fail("Expected false BoolValue")
  }
  
  // 空数组
  let empty_array_string = AttributeValue::array_string([])
  let empty_array_int = AttributeValue::array_int([])
  let empty_array_float = AttributeValue::array_float([])
  let empty_array_bool = AttributeValue::array_bool([])
  
  match empty_array_string {
    ArrayStringValue(v) => @assert.eq(v.length(), 0)
    _ => @assert.fail("Expected empty ArrayStringValue")
  }
  
  match empty_array_int {
    ArrayIntValue(v) => @assert.eq(v.length(), 0)
    _ => @assert.fail("Expected empty ArrayIntValue")
  }
  
  match empty_array_float {
    ArrayFloatValue(v) => @assert.eq(v.length(), 0)
    _ => @assert.fail("Expected empty ArrayFloatValue")
  }
  
  match empty_array_bool {
    ArrayBoolValue(v) => @assert.eq(v.length(), 0)
    _ => @assert.fail("Expected empty ArrayBoolValue")
  }
  
  // 极值
  let max_int = AttributeValue::int(9223372036854775807L)  // Int64最大值
  let min_int = AttributeValue::int(-9223372036854775808L) // Int64最小值
  let max_float = AttributeValue::float(1.7976931348623157e+308) // Double最大值
  let min_float = AttributeValue::float(-1.7976931348623157e+308) // Double最小值
  
  match max_int {
    IntValue(v) => @assert.eq(v, 9223372036854775807L)
    _ => @assert.fail("Expected max IntValue")
  }
  
  match min_int {
    IntValue(v) => @assert.eq(v, -9223372036854775808L)
    _ => @assert.fail("Expected min IntValue")
  }
  
  match max_float {
    FloatValue(v) => @assert.eq(v, 1.7976931348623157e+308)
    _ => @assert.fail("Expected max FloatValue")
  }
  
  match min_float {
    FloatValue(v) => @assert.eq(v, -1.7976931348623157e+308)
    _ => @assert.fail("Expected min FloatValue")
  }
}

test "attribute_value_in_telemetry_contexts" {
  // 测试属性值在各种遥测上下文中的使用
  
  // 创建复杂属性值集合
  let complex_attributes = [
    ("string.key", AttributeValue::string("complex.value")),
    ("int.key", AttributeValue::int(999999L)),
    ("float.key", AttributeValue::float(3.14159265359)),
    ("bool.key", AttributeValue::bool(false)),
    ("array.string.key", AttributeValue::array_string(["alpha", "beta", "gamma", "delta"])),
    ("array.int.key", AttributeValue::array_int([10L, 20L, 30L, 40L, 50L])),
    ("array.float.key", AttributeValue::array_float([0.1, 0.2, 0.3])),
    ("array.bool.key", AttributeValue::array_bool([true, true, false, false]))
  ]
  
  // 在Trace中使用复杂属性
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("complex-attr-tracer")
  let (_, span) = tracer.start_span(
    context::Context::empty(),
    "complex-attr-span",
    trace::Server,
    complex_attributes
  )
  
  // 验证Span属性
  @assert.eq(span.attributes.length(), 8)
  
  // 在Metrics中使用复杂属性
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("complex-attr-meter")
  let histogram = meter.create_histogram("complex-attr-histogram", "ms", "Histogram with complex attributes")
  histogram.record(123.456, complex_attributes)
  
  // 在Logs中使用复杂属性
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("complex-attr-logger")
  
  let log_record = logs::LogRecord::builder()
    .timestamp(5000000L)
    .severity(logs::Warn)
    .body("Log with complex attributes")
    .with_attribute("log.string", AttributeValue::string("log.value"))
    .with_attribute("log.int", AttributeValue::int(777))
    .with_attribute("log.float", AttributeValue::float(2.71828))
    .with_attribute("log.bool", AttributeValue::bool(true))
    .build()
  
  logger.emit(log_record)
  
  // 验证日志属性
  @assert.eq(log_record.attributes.length(), 4)
}

test "attribute_value_serialization_compatibility" {
  // 测试属性值的序列化兼容性（模拟场景）
  
  // 创建包含各种类型的属性值
  let attributes = [
    ("string.attr", AttributeValue::string("serialization-test")),
    ("int.attr", AttributeValue::int(42L)),
    ("float.attr", AttributeValue::float(3.14159)),
    ("bool.attr", AttributeValue::bool(true)),
    ("array.string.attr", AttributeValue::array_string(["x", "y", "z"])),
    ("array.int.attr", AttributeValue::array_int([1L, 2L, 3L])),
    ("array.float.attr", AttributeValue::array_float([1.0, 2.0, 3.0])),
    ("array.bool.attr", AttributeValue::array_bool([true, false, true]))
  ]
  
  // 模拟序列化过程（转换为字符串表示）
  let mut serialized_strings = []
  let mut i = 0
  while i < attributes.length() {
    let (key, value) = attributes[i]
    let serialized = match value {
      StringValue(v) => "string:" + v
      IntValue(v) => "int:" + v.to_string()
      FloatValue(v) => "float:" + v.to_string()
      BoolValue(v) => "bool:" + (if v { "true" } else { "false" })
      ArrayStringValue(v) => {
        let mut result = "array.string:["
        let mut j = 0
        while j < v.length() {
          result = result + v[j]
          if j < v.length() - 1 {
            result = result + ","
          }
          j = j + 1
        }
        result = result + "]"
        result
      }
      ArrayIntValue(v) => {
        let mut result = "array.int:["
        let mut j = 0
        while j < v.length() {
          result = result + v[j].to_string()
          if j < v.length() - 1 {
            result = result + ","
          }
          j = j + 1
        }
        result = result + "]"
        result
      }
      ArrayFloatValue(v) => {
        let mut result = "array.float:["
        let mut j = 0
        while j < v.length() {
          result = result + v[j].to_string()
          if j < v.length() - 1 {
            result = result + ","
          }
          j = j + 1
        }
        result = result + "]"
        result
      }
      ArrayBoolValue(v) => {
        let mut result = "array.bool:["
        let mut j = 0
        while j < v.length() {
          result = result + (if v[j] { "true" } else { "false" })
          if j < v.length() - 1 {
            result = result + ","
          }
          j = j + 1
        }
        result = result + "]"
        result
      }
    }
    serialized_strings.push((key, serialized))
    i = i + 1
  }
  
  // 验证序列化结果
  @assert.eq(serialized_strings.length(), 8)
  
  // 验证特定序列化字符串
  let mut found_string_serialized = false
  let mut found_int_serialized = false
  let mut found_bool_serialized = false
  let mut found_array_string_serialized = false
  
  let mut j = 0
  while j < serialized_strings.length() {
    let (key, serialized) = serialized_strings[j]
    if key == "string.attr" {
      @assert.eq(serialized, "string:serialization-test")
      found_string_serialized = true
    } else if key == "int.attr" {
      @assert.eq(serialized, "int:42")
      found_int_serialized = true
    } else if key == "bool.attr" {
      @assert.eq(serialized, "bool:true")
      found_bool_serialized = true
    } else if key == "array.string.attr" {
      @assert.eq(serialized, "array.string:[x,y,z]")
      found_array_string_serialized = true
    }
    j = j + 1
  }
  
  @assert.assert(found_string_serialized, "string.attr not properly serialized")
  @assert.assert(found_int_serialized, "int.attr not properly serialized")
  @assert.assert(found_bool_serialized, "bool.attr not properly serialized")
  @assert.assert(found_array_string_serialized, "array.string.attr not properly serialized")
}

test "attribute_value_type_safety_and_validation" {
  // 测试属性值的类型安全和验证
  
  // 测试类型匹配函数（模拟）
  let is_string = fn(attr : common::AttributeValue) -> Bool {
    match attr {
      StringValue(_) => true
      _ => false
    }
  }
  
  let is_int = fn(attr : common::AttributeValue) -> Bool {
    match attr {
      IntValue(_) => true
      _ => false
    }
  }
  
  let is_float = fn(attr : common::AttributeValue) -> Bool {
    match attr {
      FloatValue(_) => true
      _ => false
    }
  }
  
  let is_bool = fn(attr : common::AttributeValue) -> Bool {
    match attr {
      BoolValue(_) => true
      _ => false
    }
  }
  
  let is_array_string = fn(attr : common::AttributeValue) -> Bool {
    match attr {
      ArrayStringValue(_) => true
      _ => false
    }
  }
  
  // 测试各种属性值
  let string_attr = AttributeValue::string("type-test")
  let int_attr = AttributeValue::int(100L)
  let float_attr = AttributeValue::float(50.5)
  let bool_attr = AttributeValue::bool(false)
  let array_string_attr = AttributeValue::array_string(["type", "test"])
  
  // 验证类型检查
  @assert.assert(is_string(string_attr), "string_attr should be identified as string")
  @assert.assert(not(is_string(int_attr)), "int_attr should not be identified as string")
  
  @assert.assert(is_int(int_attr), "int_attr should be identified as int")
  @assert.assert(not(is_int(float_attr)), "float_attr should not be identified as int")
  
  @assert.assert(is_float(float_attr), "float_attr should be identified as float")
  @assert.assert(not(is_float(bool_attr)), "bool_attr should not be identified as float")
  
  @assert.assert(is_bool(bool_attr), "bool_attr should be identified as bool")
  @assert.assert(not(is_bool(array_string_attr)), "array_string_attr should not be identified as bool")
  
  @assert.assert(is_array_string(array_string_attr), "array_string_attr should be identified as array string")
  @assert.assert(not(is_array_string(string_attr)), "string_attr should not be identified as array string")
}
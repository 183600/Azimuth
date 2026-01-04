// Azimuth Advanced Data Serialization and Deserialization Test Suite
// 测试高级数据序列化和反序列化功能，确保遥测数据在不同格式间的正确转换

test "属性值JSON序列化测试" {
  // 测试字符串属性值序列化
  let string_attr = StringValue("test_value")
  let string_serialized = Serialization::to_json(string_attr)
  assert_eq(string_serialized, "{\"type\":\"string\",\"value\":\"test_value\"}")
  
  // 测试整数属性值序列化
  let int_attr = IntValue(42)
  let int_serialized = Serialization::to_json(int_attr)
  assert_eq(int_serialized, "{\"type\":\"int\",\"value\":42}")
  
  // 测试浮点属性值序列化
  let float_attr = FloatValue(3.14)
  let float_serialized = Serialization::to_json(float_attr)
  assert_eq(float_serialized, "{\"type\":\"float\",\"value\":3.14}")
  
  // 测试布尔属性值序列化
  let bool_attr = BoolValue(true)
  let bool_serialized = Serialization::to_json(bool_attr)
  assert_eq(bool_serialized, "{\"type\":\"bool\",\"value\":true}")
  
  // 测试数组字符串属性值序列化
  let array_string_attr = ArrayStringValue(["a", "b", "c"])
  let array_string_serialized = Serialization::to_json(array_string_attr)
  assert_eq(array_string_serialized, "{\"type\":\"array_string\",\"value\":[\"a\",\"b\",\"c\"]}")
  
  // 测试数组整数属性值序列化
  let array_int_attr = ArrayIntValue([1, 2, 3])
  let array_int_serialized = Serialization::to_json(array_int_attr)
  assert_eq(array_int_serialized, "{\"type\":\"array_int\",\"value\":[1,2,3]}")
}

test "属性值JSON反序列化测试" {
  // 测试字符串属性值反序列化
  let string_json = "{\"type\":\"string\",\"value\":\"test_value\"}"
  let string_deserialized = Serialization::from_json(string_json)
  match string_deserialized {
    StringValue(v) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  // 测试整数属性值反序列化
  let int_json = "{\"type\":\"int\",\"value\":42}"
  let int_deserialized = Serialization::from_json(int_json)
  match int_deserialized {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // 测试浮点属性值反序列化
  let float_json = "{\"type\":\"float\",\"value\":3.14}"
  let float_deserialized = Serialization::from_json(float_json)
  match float_deserialized {
    FloatValue(v) => assert_true(v > 3.13 && v < 3.15)
    _ => assert_true(false)
  }
  
  // 测试布尔属性值反序列化
  let bool_json = "{\"type\":\"bool\",\"value\":true}"
  let bool_deserialized = Serialization::from_json(bool_json)
  match bool_deserialized {
    BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
  
  // 测试数组字符串属性值反序列化
  let array_string_json = "{\"type\":\"array_string\",\"value\":[\"a\",\"b\",\"c\"]}"
  let array_string_deserialized = Serialization::from_json(array_string_json)
  match array_string_deserialized {
    ArrayStringValue(v) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], "a")
      assert_eq(v[1], "b")
      assert_eq(v[2], "c")
    }
    _ => assert_true(false)
  }
  
  // 测试数组整数属性值反序列化
  let array_int_json = "{\"type\":\"array_int\",\"value\":[1,2,3]}"
  let array_int_deserialized = Serialization::from_json(array_int_json)
  match array_int_deserialized {
    ArrayIntValue(v) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], 1)
      assert_eq(v[1], 2)
      assert_eq(v[2], 3)
    }
    _ => assert_true(false)
  }
}

test "跨度上下文序列化测试" {
  // 创建跨度上下文
  let span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "rojo=00f067aa0ba902b7")
  
  // 序列化跨度上下文
  let serialized = SpanContext::serialize(span_ctx)
  
  // 验证序列化结果包含所有必要字段
  assert_true(serialized.contains("trace-id"))
  assert_true(serialized.contains("span-id"))
  assert_true(serialized.contains("sampled"))
  assert_true(serialized.contains("trace-state"))
  
  // 验证具体值
  assert_true(serialized.contains("0af7651916cd43dd8448eb211c80319c"))
  assert_true(serialized.contains("b7ad6b7169203331"))
  assert_true(serialized.contains("true"))
  assert_true(serialized.contains("rojo=00f067aa0ba902b7"))
}

test "跨度上下文反序列化测试" {
  // 序列化的跨度上下文数据
  let serialized = "{\"trace-id\":\"0af7651916cd43dd8448eb211c80319c\",\"span-id\":\"b7ad6b7169203331\",\"sampled\":true,\"trace-state\":\"rojo=00f067aa0ba902b7\"}"
  
  // 反序列化跨度上下文
  let deserialized = SpanContext::deserialize(serialized)
  
  // 验证反序列化结果
  assert_eq(SpanContext::trace_id(deserialized), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(SpanContext::span_id(deserialized), "b7ad6b7169203331")
  assert_true(SpanContext::is_sampled(deserialized))
  assert_eq(SpanContext::trace_state(deserialized), "rojo=00f067aa0ba902b7")
}

test "属性集合序列化测试" {
  // 创建属性集合
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("payment-service"))
  Attributes::set(attrs, "service.version", StringValue("2.1.0"))
  Attributes::set(attrs, "service.port", IntValue(8080))
  Attributes::set(attrs, "service.tags", ArrayStringValue(["api", "payment", "critical"]))
  Attributes::set(attrs, "service.enabled", BoolValue(true))
  
  // 序列化属性集合
  let serialized = Attributes::serialize(attrs)
  
  // 验证序列化结果包含所有属性
  assert_true(serialized.contains("service.name"))
  assert_true(serialized.contains("service.version"))
  assert_true(serialized.contains("service.port"))
  assert_true(serialized.contains("service.tags"))
  assert_true(serialized.contains("service.enabled"))
  
  // 验证具体值
  assert_true(serialized.contains("payment-service"))
  assert_true(serialized.contains("2.1.0"))
  assert_true(serialized.contains("8080"))
  assert_true(serialized.contains("api"))
  assert_true(serialized.contains("payment"))
  assert_true(serialized.contains("critical"))
  assert_true(serialized.contains("true"))
}

test "属性集合反序列化测试" {
  // 序列化的属性集合数据
  let serialized = "{\"service.name\":\"payment-service\",\"service.version\":\"2.1.0\",\"service.port\":8080,\"service.tags\":[\"api\",\"payment\",\"critical\"],\"service.enabled\":true}"
  
  // 反序列化属性集合
  let deserialized = Attributes::deserialize(serialized)
  
  // 验证反序列化结果
  let service_name = Attributes::get(deserialized, "service.name")
  match service_name {
    Some(StringValue(v)) => assert_eq(v, "payment-service")
    _ => assert_true(false)
  }
  
  let service_version = Attributes::get(deserialized, "service.version")
  match service_version {
    Some(StringValue(v)) => assert_eq(v, "2.1.0")
    _ => assert_true(false)
  }
  
  let service_port = Attributes::get(deserialized, "service.port")
  match service_port {
    Some(IntValue(v)) => assert_eq(v, 8080)
    _ => assert_true(false)
  }
  
  let service_tags = Attributes::get(deserialized, "service.tags")
  match service_tags {
    Some(ArrayStringValue(v)) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], "api")
      assert_eq(v[1], "payment")
      assert_eq(v[2], "critical")
    }
    _ => assert_true(false)
  }
  
  let service_enabled = Attributes::get(deserialized, "service.enabled")
  match service_enabled {
    Some(BoolValue(v)) => assert_true(v)
    _ => assert_true(false)
  }
}

test "日志记录序列化测试" {
  // 创建日志记录
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(Attributes::new()),
    Some(1234567890L),
    Some(1234567891L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    Some(Context::root())
  )
  
  // 序列化日志记录
  let serialized = LogRecord::serialize(log_record)
  
  // 验证序列化结果包含所有必要字段
  assert_true(serialized.contains("severity"))
  assert_true(serialized.contains("body"))
  assert_true(serialized.contains("timestamp"))
  assert_true(serialized.contains("observed_timestamp"))
  assert_true(serialized.contains("trace_id"))
  assert_true(serialized.contains("span_id"))
  
  // 验证具体值
  assert_true(serialized.contains("ERROR"))
  assert_true(serialized.contains("Database connection failed"))
  assert_true(serialized.contains("1234567890"))
  assert_true(serialized.contains("1234567891"))
  assert_true(serialized.contains("0af7651916cd43dd8448eb211c80319c"))
  assert_true(serialized.contains("b7ad6b7169203331"))
}

test "日志记录反序列化测试" {
  // 序列化的日志记录数据
  let serialized = "{\"severity\":\"ERROR\",\"body\":\"Database connection failed\",\"timestamp\":1234567890,\"observed_timestamp\":1234567891,\"trace_id\":\"0af7651916cd43dd8448eb211c80319c\",\"span_id\":\"b7ad6b7169203331\",\"attributes\":{}}"
  
  // 反序列化日志记录
  let deserialized = LogRecord::deserialize(serialized)
  
  // 验证反序列化结果
  assert_eq(LogRecord::severity_number(deserialized), Error)
  match LogRecord::body(deserialized) {
    Some(body) => assert_eq(body, "Database connection failed")
    None => assert_true(false)
  }
  assert_eq(LogRecord::timestamp(deserialized), Some(1234567890L))
  assert_eq(LogRecord::observed_timestamp(deserialized), Some(1234567891L))
  assert_eq(LogRecord::trace_id(deserialized), Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(LogRecord::span_id(deserialized), Some("b7ad6b7169203331"))
}

test "资源序列化测试" {
  // 创建资源
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("order-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("service.port", IntValue(8080)),
    ("service.enabled", BoolValue(true))
  ])
  
  // 序列化资源
  let serialized = Resource::serialize(resource)
  
  // 验证序列化结果包含所有属性
  assert_true(serialized.contains("service.name"))
  assert_true(serialized.contains("service.version"))
  assert_true(serialized.contains("service.instance.id"))
  assert_true(serialized.contains("service.port"))
  assert_true(serialized.contains("service.enabled"))
  
  // 验证具体值
  assert_true(serialized.contains("order-service"))
  assert_true(serialized.contains("1.0.0"))
  assert_true(serialized.contains("instance-123"))
  assert_true(serialized.contains("8080"))
  assert_true(serialized.contains("true"))
}

test "资源反序列化测试" {
  // 序列化的资源数据
  let serialized = "{\"service.name\":\"order-service\",\"service.version\":\"1.0.0\",\"service.instance.id\":\"instance-123\",\"service.port\":8080,\"service.enabled\":true}"
  
  // 反序列化资源
  let deserialized = Resource::deserialize(serialized)
  
  // 验证反序列化结果
  let service_name = Resource::get_attribute(deserialized, "service.name")
  match service_name {
    Some(StringValue(v)) => assert_eq(v, "order-service")
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(deserialized, "service.version")
  match service_version {
    Some(StringValue(v)) => assert_eq(v, "1.0.0")
    _ => assert_true(false)
  }
  
  let service_instance_id = Resource::get_attribute(deserialized, "service.instance.id")
  match service_instance_id {
    Some(StringValue(v)) => assert_eq(v, "instance-123")
    _ => assert_true(false)
  }
  
  let service_port = Resource::get_attribute(deserialized, "service.port")
  match service_port {
    Some(IntValue(v)) => assert_eq(v, 8080)
    _ => assert_true(false)
  }
  
  let service_enabled = Resource::get_attribute(deserialized, "service.enabled")
  match service_enabled {
    Some(BoolValue(v)) => assert_true(v)
    _ => assert_true(false)
  }
}

test "序列化错误处理测试" {
  // 测试无效JSON的反序列化
  let invalid_json = "{invalid json}"
  let result = Serialization::from_json(invalid_json)
  match result {
    ErrorValue => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试缺少类型的JSON反序列化
  let missing_type_json = "{\"value\":\"test_value\"}"
  let missing_type_result = Serialization::from_json(missing_type_json)
  match missing_type_result {
    ErrorValue => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试未知类型的JSON反序列化
  let unknown_type_json = "{\"type\":\"unknown\",\"value\":\"test_value\"}"
  let unknown_type_result = Serialization::from_json(unknown_type_json)
  match unknown_type_result {
    ErrorValue => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试空字符串的反序列化
  let empty_string = ""
  let empty_result = Serialization::from_json(empty_string)
  match empty_result {
    ErrorValue => assert_true(true)
    _ => assert_true(false)
  }
}

test "序列化性能测试" {
  // 创建大型属性集合
  let large_attrs = Attributes::new()
  for i in 0..=100 {
    let key = "attr." + i.to_string()
    let value = StringValue("value." + i.to_string())
    Attributes::set(large_attrs, key, value)
  }
  
  // 测量序列化时间
  let start_time = Time::now()
  let serialized = Attributes::serialize(large_attrs)
  let end_time = Time::now()
  let serialization_time = end_time - start_time
  
  // 验证序列化时间在合理范围内（小于100ms）
  assert_true(serialization_time < 100)
  
  // 验证序列化结果包含所有属性
  for i in 0..=100 {
    let key = "attr." + i.to_string()
    let value = "value." + i.to_string()
    assert_true(serialized.contains(key))
    assert_true(serialized.contains(value))
  }
  
  // 测量反序列化时间
  let start_time = Time::now()
  let deserialized = Attributes::deserialize(serialized)
  let end_time = Time::now()
  let deserialization_time = end_time - start_time
  
  // 验证反序列化时间在合理范围内（小于100ms）
  assert_true(deserialization_time < 100)
  
  // 验证反序列化结果正确
  for i in 0..=100 {
    let key = "attr." + i.to_string()
    let value = "value." + i.to_string()
    let attr = Attributes::get(deserialized, key)
    match attr {
      Some(StringValue(v)) => assert_eq(v, value)
      _ => assert_true(false)
    }
  }
}
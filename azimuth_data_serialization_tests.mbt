// 阿兹米克数据序列化和反序列化测试用例
// 专注于遥测数据的序列化、反序列化和格式转换功能

// 测试1: 基本数据类型序列化
test "基本数据类型序列化测试" {
  let serializer = JsonSerializer::new()
  
  // 测试字符串序列化
  let string_value = StringValue("test_string")
  let string_serialized = JsonSerializer::serialize(serializer, string_value)
  assert_true(string_serialized.contains("\"test_string\""))
  
  // 测试整数序列化
  let int_value = IntValue(42)
  let int_serialized = JsonSerializer::serialize(serializer, int_value)
  assert_true(int_serialized.contains("42"))
  
  // 测试浮点数序列化
  let float_value = FloatValue(3.14159)
  let float_serialized = JsonSerializer::serialize(serializer, float_value)
  assert_true(float_serialized.contains("3.14159"))
  
  // 测试布尔值序列化
  let bool_value = BoolValue(true)
  let bool_serialized = JsonSerializer::serialize(serializer, bool_value)
  assert_true(bool_serialized.contains("true"))
  
  // 测试数组序列化
  let array_value = ArrayStringValue(["a", "b", "c"])
  let array_serialized = JsonSerializer::serialize(serializer, array_value)
  assert_true(array_serialized.contains("[\"a\",\"b\",\"c\"]"))
}

// 测试2: 复杂数据结构序列化
test "复杂数据结构序列化测试" {
  let serializer = JsonSerializer::new()
  
  // 创建复杂属性集合
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("azimuth_service"))
  Attributes::set(attrs, "service.version", StringValue("1.0.0"))
  Attributes::set(attrs, "service.instance.id", StringValue("instance-123"))
  Attributes::set(attrs, "service.port", IntValue(8080))
  Attributes::set(attrs, "service.enabled", BoolValue(true))
  Attributes::set(attrs, "service.tags", ArrayStringValue(["web", "api", "telemetry"]))
  
  // 序列化属性集合
  let attrs_serialized = JsonSerializer::serialize(serializer, attrs)
  assert_true(attrs_serialized.contains("azimuth_service"))
  assert_true(attrs_serialized.contains("1.0.0"))
  assert_true(attrs_serialized.contains("instance-123"))
  assert_true(attrs_serialized.contains("8080"))
  assert_true(attrs_serialized.contains("true"))
  assert_true(attrs_serialized.contains("[\"web\",\"api\",\"telemetry\"]"))
  
  // 创建Span上下文
  let span_ctx = SpanContext::new("trace_id_123", "span_id_456", true, "sampled_state")
  let span_ctx_serialized = JsonSerializer::serialize(serializer, span_ctx)
  assert_true(span_ctx_serialized.contains("trace_id_123"))
  assert_true(span_ctx_serialized.contains("span_id_456"))
  assert_true(span_ctx_serialized.contains("true"))
  assert_true(span_ctx_serialized.contains("sampled_state"))
}

// 测试3: 基本数据类型反序列化
test "基本数据类型反序列化测试" {
  let deserializer = JsonSerializer::new()
  
  // 测试字符串反序列化
  let string_json = "{\"type\":\"string\",\"value\":\"test_string\"}"
  let string_deserialized = JsonSerializer::deserialize(deserializer, string_json)
  match string_deserialized {
    StringValue(v) => assert_eq(v, "test_string")
    _ => assert_true(false)
  }
  
  // 测试整数反序列化
  let int_json = "{\"type\":\"int\",\"value\":42}"
  let int_deserialized = JsonSerializer::deserialize(deserializer, int_json)
  match int_deserialized {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // 测试浮点数反序列化
  let float_json = "{\"type\":\"float\",\"value\":3.14159}"
  let float_deserialized = JsonSerializer::deserialize(deserializer, float_json)
  match float_deserialized {
    FloatValue(v) => assert_eq(v, 3.14159)
    _ => assert_true(false)
  }
  
  // 测试布尔值反序列化
  let bool_json = "{\"type\":\"bool\",\"value\":true}"
  let bool_deserialized = JsonSerializer::deserialize(deserializer, bool_json)
  match bool_deserialized {
    BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
  
  // 测试数组反序列化
  let array_json = "{\"type\":\"array_string\",\"value\":[\"a\",\"b\",\"c\"]}"
  let array_deserialized = JsonSerializer::deserialize(deserializer, array_json)
  match array_deserialized {
    ArrayStringValue(v) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], "a")
      assert_eq(v[1], "b")
      assert_eq(v[2], "c")
    }
    _ => assert_true(false)
  }
}

// 测试4: 复杂数据结构反序列化
test "复杂数据结构反序列化测试" {
  let deserializer = JsonSerializer::new()
  
  // 测试属性集合反序列化
  let attrs_json = "{\"attributes\":{\"service.name\":\"azimuth_service\",\"service.version\":\"1.0.0\",\"service.port\":8080,\"service.enabled\":true}}"
  let attrs_deserialized = JsonSerializer::deserialize(deserializer, attrs_json)
  
  // 验证反序列化结果
  match attrs_deserialized {
    Attributes(attrs_map) => {
      let service_name = Attributes::get(attrs_map, "service.name")
      match service_name {
        Some(StringValue(name)) => assert_eq(name, "azimuth_service")
        _ => assert_true(false)
      }
      
      let service_version = Attributes::get(attrs_map, "service.version")
      match service_version {
        Some(StringValue(version)) => assert_eq(version, "1.0.0")
        _ => assert_true(false)
      }
      
      let service_port = Attributes::get(attrs_map, "service.port")
      match service_port {
        Some(IntValue(port)) => assert_eq(port, 8080)
        _ => assert_true(false)
      }
      
      let service_enabled = Attributes::get(attrs_map, "service.enabled")
      match service_enabled {
        Some(BoolValue(enabled)) => assert_true(enabled)
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 测试Span上下文反序列化
  let span_ctx_json = "{\"trace_id\":\"trace_id_123\",\"span_id\":\"span_id_456\",\"sampled\":true,\"trace_state\":\"sampled_state\"}"
  let span_ctx_deserialized = JsonSerializer::deserialize(deserializer, span_ctx_json)
  
  match span_ctx_deserialized {
    SpanContext(ctx) => {
      assert_eq(SpanContext::trace_id(ctx), "trace_id_123")
      assert_eq(SpanContext::span_id(ctx), "span_id_456")
      assert_true(SpanContext::is_sampled(ctx))
      assert_eq(SpanContext::trace_state(ctx), "sampled_state")
    }
    _ => assert_true(false)
  }
}

// 测试5: 序列化格式兼容性
test "序列化格式兼容性测试" {
  let json_serializer = JsonSerializer::new()
  let xml_serializer = XmlSerializer::new()
  let protobuf_serializer = ProtobufSerializer::new()
  
  // 创建测试数据
  let test_attrs = Attributes::new()
  Attributes::set(test_attrs, "test.key", StringValue("test.value"))
  Attributes::set(test_attrs, "test.number", IntValue(123))
  
  // JSON格式序列化
  let json_result = JsonSerializer::serialize(json_serializer, test_attrs)
  assert_true(json_result.starts_with("{"))
  assert_true(json_result.ends_with("}"))
  
  // XML格式序列化
  let xml_result = XmlSerializer::serialize(xml_serializer, test_attrs)
  assert_true(xml_result.starts_with("<"))
  assert_true(xml_result.ends_with(">"))
  
  // Protobuf格式序列化
  let protobuf_result = ProtobufSerializer::serialize(protobuf_serializer, test_attrs)
  assert_true(protobuf_result.length() > 0)
  
  // 验证不同格式的反序列化结果一致性
  let json_deserializer = JsonSerializer::new()
  let xml_deserializer = XmlSerializer::new()
  let protobuf_deserializer = ProtobufSerializer::new()
  
  let json_restored = JsonSerializer::deserialize(json_deserializer, json_result)
  let xml_restored = XmlSerializer::deserialize(xml_deserializer, xml_result)
  let protobuf_restored = ProtobufSerializer::deserialize(protobuf_deserializer, protobuf_result)
  
  // 验证反序列化结果的一致性
  match (json_restored, xml_restored, protobuf_restored) {
    (Attributes(json_attrs), Attributes(xml_attrs), Attributes(protobuf_attrs)) => {
      let json_value = Attributes::get(json_attrs, "test.key")
      let xml_value = Attributes::get(xml_attrs, "test.key")
      let protobuf_value = Attributes::get(protobuf_attrs, "test.key")
      
      match (json_value, xml_value, protobuf_value) {
        (Some(StringValue(json_v)), Some(StringValue(xml_v)), Some(StringValue(protobuf_v))) => {
          assert_eq(json_v, xml_v)
          assert_eq(xml_v, protobuf_v)
          assert_eq(json_v, "test.value")
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

// 测试6: 序列化性能和大小优化
test "序列化性能和大小优化测试" {
  let serializer = JsonSerializer::new()
  let compressed_serializer = CompressedJsonSerializer::new()
  
  // 创建大量测试数据
  let large_attrs = Attributes::new()
  for i in 1..=1000 {
    let key = "large.key." + i.to_string()
    let value = "large.value." + i.to_string()
    Attributes::set(large_attrs, key, StringValue(value))
  }
  
  // 普通序列化
  let start_time = Timestamp::now()
  let normal_serialized = JsonSerializer::serialize(serializer, large_attrs)
  let normal_time = Timestamp::now() - start_time
  let normal_size = normal_serialized.length()
  
  // 压缩序列化
  let start_time = Timestamp::now()
  let compressed_serialized = CompressedJsonSerializer::serialize(compressed_serializer, large_attrs)
  let compressed_time = Timestamp::now() - start_time
  let compressed_size = compressed_serialized.length()
  
  // 验证压缩效果
  assert_true(compressed_size < normal_size)
  let compression_ratio = compressed_size.to_float() / normal_size.to_float()
  assert_true(compression_ratio < 0.8) // 压缩率应该小于80%
  
  // 验证压缩序列化的正确性
  let compressed_deserializer = CompressedJsonSerializer::new()
  let restored_attrs = CompressedJsonSerializer::deserialize(compressed_deserializer, compressed_serialized)
  
  match restored_attrs {
    Attributes(attrs) => {
      let test_value = Attributes::get(attrs, "large.key.500")
      match test_value {
        Some(StringValue(v)) => assert_eq(v, "large.value.500")
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

// 测试7: 序列化错误处理和边界情况
test "序列化错误处理和边界情况测试" {
  let serializer = JsonSerializer::new()
  let deserializer = JsonSerializer::new()
  
  // 测试空数据序列化
  let empty_attrs = Attributes::new()
  let empty_serialized = JsonSerializer::serialize(serializer, empty_attrs)
  assert_true(empty_serialized.contains("{}"))
  
  // 测试空数据反序列化
  let empty_json = "{}"
  let empty_deserialized = JsonSerializer::deserialize(deserializer, empty_json)
  match empty_deserialized {
    Attributes(attrs) => {
      // 验证空属性集合
      let non_existent = Attributes::get(attrs, "non.existent.key")
      match non_existent {
        Some(_) => assert_true(false)
        None => assert_true(true)
      }
    }
    _ => assert_true(false)
  }
  
  // 测试无效JSON反序列化
  let invalid_json = "{\"invalid\": json}"
  let invalid_deserialized = JsonSerializer::deserialize(deserializer, invalid_json)
  match invalid_deserialized {
    Error(_) => assert_true(true) // 应该返回错误
    _ => assert_true(false)
  }
  
  // 测试类型不匹配反序列化
  let type_mismatch_json = "{\"type\":\"string\",\"value\":123}" // 声明为string但值为数字
  let type_mismatch_deserialized = JsonSerializer::deserialize(deserializer, type_mismatch_json)
  match type_mismatch_deserialized {
    Error(_) => assert_true(true) // 应该返回错误
    _ => assert_true(false)
  }
  
  // 测试循环引用处理
  let cyclic_attrs = Attributes::new()
  Attributes::set(cyclic_attrs, "self.reference", StringValue("self.reference")) // 模拟自引用
  let cyclic_serialized = JsonSerializer::serialize(serializer, cyclic_attrs)
  assert_true(cyclic_serialized.length() > 0) // 应该能序列化，但可能不包含循环引用
  
  // 测试超大数据序列化
  let huge_attrs = Attributes::new()
  let huge_string = "x".repeat(1000000) // 1MB字符串
  Attributes::set(huge_attrs, "huge.string", StringValue(huge_string))
  
  let huge_serialized = JsonSerializer::serialize(serializer, huge_attrs)
  assert_true(huge_serialized.length() > 1000000) // 序列化后应该大于1MB
}

// 测试8: 序列化版本兼容性
test "序列化版本兼容性测试" {
  let v1_serializer = JsonSerializer::with_version("1.0.0")
  let v2_serializer = JsonSerializer::with_version("2.0.0")
  
  // 创建V1格式数据
  let v1_attrs = Attributes::new()
  Attributes::set(v1_attrs, "service.name", StringValue("azimuth"))
  Attributes::set(v1_attrs, "service.version", StringValue("1.0.0"))
  
  let v1_serialized = JsonSerializer::serialize(v1_serializer, v1_attrs)
  
  // 使用V2反序列化器处理V1数据
  let v2_deserializer = JsonSerializer::with_version("2.0.0")
  let v1_to_v2_deserialized = JsonSerializer::deserialize(v2_deserializer, v1_serialized)
  
  match v1_to_v2_deserialized {
    Attributes(attrs) => {
      // V2应该能读取V1的基本字段
      let service_name = Attributes::get(attrs, "service.name")
      match service_name {
        Some(StringValue(name)) => assert_eq(name, "azimuth")
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 创建V2格式数据（包含V1没有的字段）
  let v2_attrs = Attributes::new()
  Attributes::set(v2_attrs, "service.name", StringValue("azimuth"))
  Attributes::set(v2_attrs, "service.version", StringValue("2.0.0"))
  Attributes::set(v2_attrs, "service.new_field", StringValue("new_value")) // V2新增字段
  
  let v2_serialized = JsonSerializer::serialize(v2_serializer, v2_attrs)
  
  // 使用V1反序列化器处理V2数据
  let v1_deserializer = JsonSerializer::with_version("1.0.0")
  let v2_to_v1_deserialized = JsonSerializer::deserialize(v1_deserializer, v2_serialized)
  
  match v2_to_v1_deserialized {
    Attributes(attrs) => {
      // V1应该能读取V1的字段，但忽略新字段
      let service_name = Attributes::get(attrs, "service.name")
      match service_name {
        Some(StringValue(name)) => assert_eq(name, "azimuth")
        _ => assert_true(false)
      }
      
      // V1不应该包含V2的新字段
      let new_field = Attributes::get(attrs, "service.new_field")
      match new_field {
        Some(_) => assert_true(false) // V1不应该有这个字段
        None => assert_true(true)
      }
    }
    _ => assert_true(false)
  }
}
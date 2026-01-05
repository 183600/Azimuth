// Azimuth Serialization and Deserialization Test Suite
// 测试遥测系统的序列化和反序列化功能，确保数据在不同系统和格式间的正确转换

// 测试1: JSON序列化和反序列化
test "JSON序列化和反序列化测试" {
  let json_serializer = JsonSerializer::new()
  
  // 创建复杂的遥测数据
  let telemetry_data = TelemetryData::new()
  TelemetryData::set_trace_id(telemetry_data, "4bf92f3577b34da6a3ce929d0e0e4736")
  TelemetryData::set_span_id(telemetry_data, "00f067aa0ba902b7")
  TelemetryData::add_attribute(telemetry_data, "service.name", StringValue("auth-service"))
  TelemetryData::add_attribute(telemetry_data, "operation.name", StringValue("user.login"))
  TelemetryData::add_attribute(telemetry_data, "user.id", StringValue("user123"))
  TelemetryData::add_attribute(telemetry_data, "http.method", StringValue("POST"))
  TelemetryData::add_attribute(telemetry_data, "http.status_code", IntValue(200))
  TelemetryData::add_attribute(telemetry_data, "duration", FloatValue(123.45))
  TelemetryData::add_attribute(telemetry_data, "success", BoolValue(true))
  TelemetryData::add_event(telemetry_data, "user.authenticated", [
    ("auth.method", StringValue("password")),
    ("auth.provider", StringValue("local"))
  ])
  
  // 序列化为JSON
  let json_string = JsonSerializer::serialize(json_serializer, telemetry_data)
  
  // 验证JSON内容
  assert_true(json_string.contains("trace_id"))
  assert_true(json_string.contains("4bf92f3577b34da6a3ce929d0e0e4736"))
  assert_true(json_string.contains("service.name"))
  assert_true(json_string.contains("auth-service"))
  
  // 反序列化
  let deserialized_data = JsonSerializer::deserialize(json_serializer, json_string)
  
  // 验证反序列化结果
  assert_eq(TelemetryData::trace_id(deserialized_data), "4bf92f3577b34da6a3ce929d0e0e4736")
  assert_eq(TelemetryData::span_id(deserialized_data), "00f067aa0ba902b7")
  
  let service_name = TelemetryData::get_attribute(deserialized_data, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "auth-service")
    _ => assert_true(false)
  }
  
  let http_status = TelemetryData::get_attribute(deserialized_data, "http.status_code")
  match http_status {
    Some(IntValue(status)) => assert_eq(status, 200)
    _ => assert_true(false)
  }
  
  let duration = TelemetryData::get_attribute(deserialized_data, "duration")
  match duration {
    Some(FloatValue(d)) => assert_true(abs_float(d - 123.45) < 0.001)
    _ => assert_true(false)
  }
  
  let success = TelemetryData::get_attribute(deserialized_data, "success")
  match success {
    Some(BoolValue(s)) => assert_true(s)
    _ => assert_true(false)
  }
  
  // 验证事件
  let events = TelemetryData::get_events(deserialized_data)
  assert_eq(events.length(), 1)
  assert_eq(events[0].name, "user.authenticated")
  
  // 测试部分缺失字段的反序列化
  let partial_json = "{\"trace_id\":\"12345\",\"span_id\":\"67890\"}"
  let partial_data = JsonSerializer::deserialize(json_serializer, partial_json)
  
  assert_eq(TelemetryData::trace_id(partial_data), "12345")
  assert_eq(TelemetryData::span_id(partial_data), "67890")
  
  let missing_attr = TelemetryData::get_attribute(partial_data, "service.name")
  match missing_attr {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// 测试2: 二进制序列化和反序列化
test "二进制序列化和反序列化测试" {
  let binary_serializer = BinarySerializer::new()
  
  // 创建包含各种数据类型的遥测数据
  let telemetry_data = TelemetryData::new()
  TelemetryData::set_trace_id(telemetry_data, "abc123def456")
  TelemetryData::set_span_id(telemetry_data, "789012345678")
  TelemetryData::add_attribute(telemetry_data, "string.attr", StringValue("test value"))
  TelemetryData::add_attribute(telemetry_data, "int.attr", IntValue(42))
  TelemetryData::add_attribute(telemetry_data, "float.attr", FloatValue(3.14159))
  TelemetryData::add_attribute(telemetry_data, "bool.attr", BoolValue(true))
  TelemetryData::add_attribute(telemetry_data, "array.attr", ArrayStringValue(["a", "b", "c"]))
  TelemetryData::add_attribute(telemetry_data, "bytes.attr", BytesValue([0x01, 0x02, 0x03, 0x04]))
  
  // 序列化为二进制
  let binary_data = BinarySerializer::serialize(binary_serializer, telemetry_data)
  
  // 验证二进制数据不为空
  assert_true(binary_data.length() > 0)
  
  // 反序列化
  let deserialized_data = BinarySerializer::deserialize(binary_serializer, binary_data)
  
  // 验证反序列化结果
  assert_eq(TelemetryData::trace_id(deserialized_data), "abc123def456")
  assert_eq(TelemetryData::span_id(deserialized_data), "789012345678")
  
  let string_attr = TelemetryData::get_attribute(deserialized_data, "string.attr")
  match string_attr {
    Some(StringValue(value)) => assert_eq(value, "test value")
    _ => assert_true(false)
  }
  
  let int_attr = TelemetryData::get_attribute(deserialized_data, "int.attr")
  match int_attr {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  let float_attr = TelemetryData::get_attribute(deserialized_data, "float.attr")
  match float_attr {
    Some(FloatValue(value)) => assert_true(abs_float(value - 3.14159) < 0.0001)
    _ => assert_true(false)
  }
  
  let bool_attr = TelemetryData::get_attribute(deserialized_data, "bool.attr")
  match bool_attr {
    Some(BoolValue(value)) => assert_true(value)
    _ => assert_true(false)
  }
  
  let array_attr = TelemetryData::get_attribute(deserialized_data, "array.attr")
  match array_attr {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "a")
      assert_eq(values[1], "b")
      assert_eq(values[2], "c")
    }
    _ => assert_true(false)
  }
  
  let bytes_attr = TelemetryData::get_attribute(deserialized_data, "bytes.attr")
  match bytes_attr {
    Some(BytesValue(bytes)) => {
      assert_eq(bytes.length(), 4)
      assert_eq(bytes[0], 0x01)
      assert_eq(bytes[1], 0x02)
      assert_eq(bytes[2], 0x03)
      assert_eq(bytes[3], 0x04)
    }
    _ => assert_true(false)
  }
  
  // 测试二进制数据大小
  let json_size = JsonSerializer::serialize(JsonSerializer::new(), telemetry_data).length()
  let binary_size = binary_data.length()
  
  // 二进制序列化应该更紧凑
  assert_true(binary_size < json_size)
}

// 测试3: 协议缓冲区序列化
test "协议缓冲区序列化测试" {
  let protobuf_serializer = ProtobufSerializer::new()
  
  // 创建嵌套的遥测数据结构
  let telemetry_data = TelemetryData::new()
  TelemetryData::set_trace_id(telemetry_data, "trace123456789")
  TelemetryData::set_span_id(telemetry_data, "span123456789")
  
  // 添加嵌套属性
  let nested_attrs = [
    ("user.info", [
      ("user.id", StringValue("user123")),
      ("user.name", StringValue("张三")),
      ("user.email", StringValue("zhang.san@example.com")),
      ("user.age", IntValue(30))
    ]),
    ("request.info", [
      ("request.id", StringValue("req-12345")),
      ("request.method", StringValue("POST")),
      ("request.path", StringValue("/api/login")),
      ("request.size", IntValue(1024))
    ]),
    ("response.info", [
      ("response.status", IntValue(200)),
      ("response.size", IntValue(512)),
      ("response.duration", FloatValue(150.5))
    ])
  ]
  
  for (prefix, attrs) in nested_attrs {
    for (key, value) in attrs {
      let full_key = prefix + "." + key
      TelemetryData::add_attribute(telemetry_data, full_key, value)
    }
  }
  
  // 序列化为协议缓冲区
  let protobuf_data = ProtobufSerializer::serialize(protobuf_serializer, telemetry_data)
  
  // 验证协议缓冲区数据
  assert_true(protobuf_data.length() > 0)
  
  // 反序列化
  let deserialized_data = ProtobufSerializer::deserialize(protobuf_serializer, protobuf_data)
  
  // 验证反序列化结果
  assert_eq(TelemetryData::trace_id(deserialized_data), "trace123456789")
  assert_eq(TelemetryData::span_id(deserialized_data), "span123456789")
  
  let user_id = TelemetryData::get_attribute(deserialized_data, "user.info.user.id")
  match user_id {
    Some(StringValue(id)) => assert_eq(id, "user123")
    _ => assert_true(false)
  }
  
  let user_name = TelemetryData::get_attribute(deserialized_data, "user.info.user.name")
  match user_name {
    Some(StringValue(name)) => assert_eq(name, "张三")
    _ => assert_true(false)
  }
  
  let request_method = TelemetryData::get_attribute(deserialized_data, "request.info.request.method")
  match request_method {
    Some(StringValue(method)) => assert_eq(method, "POST")
    _ => assert_true(false)
  }
  
  let response_status = TelemetryData::get_attribute(deserialized_data, "response.info.response.status")
  match response_status {
    Some(IntValue(status)) => assert_eq(status, 200)
    _ => assert_true(false)
  }
  
  // 测试协议缓冲区的向前兼容性
  let v1_data = ProtobufSerializer::serialize_v1(protobuf_serializer, telemetry_data)
  let v2_deserialized = ProtobufSerializer::deserialize_v2(protobuf_serializer, v1_data)
  
  // 即使版本不同，基本字段应该仍然可读
  assert_eq(TelemetryData::trace_id(v2_deserialized), "trace123456789")
  assert_eq(TelemetryData::span_id(v2_deserialized), "span123456789")
}

// 测试4: 自定义序列化格式
test "自定义序列化格式测试" {
  let custom_serializer = CustomSerializer::new()
  
  // 创建包含特殊字符和Unicode的数据
  let telemetry_data = TelemetryData::new()
  TelemetryData::add_attribute(telemetry_data, "unicode.text", StringValue("你好世界！Hello World! 🌍"))
  TelemetryData::add_attribute(telemetry_data, "special.chars", StringValue("特殊字符：\n\t\r\"'\\"))
  TelemetryData::add_attribute(telemetry_data, "emoji", StringValue("😀😃😄😁😆😅😂🤣"))
  TelemetryData::add_attribute(telemetry_data, "math.symbols", StringValue("∑∏∫∆∇∂"))
  
  // 序列化为自定义格式
  let custom_data = CustomSerializer::serialize(custom_serializer, telemetry_data)
  
  // 验证自定义格式数据
  assert_true(custom_data.length() > 0)
  
  // 反序列化
  let deserialized_data = CustomSerializer::deserialize(custom_serializer, custom_data)
  
  // 验证特殊字符和Unicode正确处理
  let unicode_text = TelemetryData::get_attribute(deserialized_data, "unicode.text")
  match unicode_text {
    Some(StringValue(text)) => assert_eq(text, "你好世界！Hello World! 🌍")
    _ => assert_true(false)
  }
  
  let special_chars = TelemetryData::get_attribute(deserialized_data, "special.chars")
  match special_chars {
    Some(StringValue(chars)) => assert_eq(chars, "特殊字符：\n\t\r\"'\\")
    _ => assert_true(false)
  }
  
  let emoji = TelemetryData::get_attribute(deserialized_data, "emoji")
  match emoji {
    Some(StringValue(e)) => assert_eq(e, "😀😃😄😁😆😅😂🤣")
    _ => assert_true(false)
  }
  
  let math_symbols = TelemetryData::get_attribute(deserialized_data, "math.symbols")
  match math_symbols {
    Some(StringValue(symbols)) => assert_eq(symbols, "∑∏∫∆∇∂")
    _ => assert_true(false)
  }
  
  // 测试自定义压缩
  let compressed_data = CustomSerializer::serialize_compressed(custom_serializer, telemetry_data)
  let decompressed_data = CustomSerializer::deserialize_compressed(custom_serializer, compressed_data)
  
  // 验证压缩后的数据一致性
  let original_unicode = TelemetryData::get_attribute(telemetry_data, "unicode.text")
  let decompressed_unicode = TelemetryData::get_attribute(decompressed_data, "unicode.text")
  
  match (original_unicode, decompressed_unicode) {
    (Some(StringValue(orig)), Some(StringValue(decomp))) => assert_eq(orig, decomp)
    _ => assert_true(false)
  }
  
  // 压缩数据应该更小
  assert_true(compressed_data.length() < custom_data.length())
}

// 测试5: 序列化错误处理
test "序列化错误处理测试" {
  let serializer = JsonSerializer::new()
  
  // 测试循环引用处理
  let circular_obj = CircularObject::new("parent")
  let child_obj = CircularObject::new("child")
  
  // 建立循环引用
  CircularObject::set_child(circular_obj, child_obj)
  CircularObject::set_child(child_obj, circular_obj)
  
  // 尝试序列化循环引用对象
  let circular_result = JsonSerializer::try_serialize(serializer, circular_obj)
  match circular_result {
    Error(CircularReferenceError) => assert_true(true) // 应该检测到循环引用
    Ok(_) => assert_true(false) // 不应该成功
    Error(_) => assert_true(false) // 其他错误类型
  }
  
  // 测试过大对象处理
  let large_obj = LargeObject::new(100 * 1024 * 1024) // 100MB对象
  let large_result = JsonSerializer::try_serialize(serializer, large_obj)
  match large_result {
    Error(ObjectTooLargeError) => assert_true(true) // 应该检测到对象过大
    Ok(_) => assert_true(false) // 不应该成功
    Error(_) => assert_true(false) // 其他错误类型
  }
  
  // 测试不支持的类型处理
  let unsupported_obj = UnsupportedTypeObject::new()
  let unsupported_result = JsonSerializer::try_serialize(serializer, unsupported_obj)
  match unsupported_result {
    Error(UnsupportedTypeError) => assert_true(true) // 应该检测到不支持的类型
    Ok(_) => assert_true(false) // 不应该成功
    Error(_) => assert_true(false) // 其他错误类型
  }
  
  // 测试损坏数据的反序列化
  let corrupted_json = "{\"trace_id\":\"123\",\"invalid_json"
  let corrupted_result = JsonSerializer::try_deserialize(serializer, corrupted_json)
  match corrupted_result {
    Error(InvalidFormatError) => assert_true(true) // 应该检测到格式错误
    Ok(_) => assert_true(false) // 不应该成功
    Error(_) => assert_true(false) // 其他错误类型
  }
  
  // 测试类型不匹配的反序列化
  let type_mismatch_json = "{\"trace_id\":123}" // 应该是字符串
  let type_mismatch_result = JsonSerializer::try_deserialize(serializer, type_mismatch_json)
  match type_mismatch_result {
    Error(TypeMismatchError) => assert_true(true) // 应该检测到类型不匹配
    Ok(_) => assert_true(false) // 不应该成功
    Error(_) => assert_true(false) // 其他错误类型
  }
}

// 测试6: 序列化性能测试
test "序列化性能测试" {
  let json_serializer = JsonSerializer::new()
  let binary_serializer = BinarySerializer::new()
  
  // 创建大量测试数据
  let mut telemetry_data_list = []
  for i in 0..1000 {
    let telemetry_data = TelemetryData::new()
    TelemetryData::set_trace_id(telemetry_data, "trace_" + i.to_string())
    TelemetryData::set_span_id(telemetry_data, "span_" + i.to_string())
    TelemetryData::add_attribute(telemetry_data, "service.name", StringValue("service_" + (i % 10).to_string()))
    TelemetryData::add_attribute(telemetry_data, "operation.name", StringValue("operation_" + (i % 5).to_string()))
    TelemetryData::add_attribute(telemetry_data, "index", IntValue(i))
    TelemetryData::add_attribute(telemetry_data, "value", FloatValue(i * 1.5))
    telemetry_data_list = telemetry_data_list + [telemetry_data]
  }
  
  // 测试JSON序列化性能
  let json_start_time = get_current_time_millis()
  let mut json_results = []
  for data in telemetry_data_list {
    let json_string = JsonSerializer::serialize(json_serializer, data)
    json_results = json_results + [json_string]
  }
  let json_serialize_time = get_current_time_millis() - json_start_time
  
  // 测试JSON反序列化性能
  let json_deserialize_start = get_current_time_millis()
  for json_string in json_results {
    JsonSerializer::deserialize(json_serializer, json_string)
  }
  let json_deserialize_time = get_current_time_millis() - json_deserialize_start
  
  // 测试二进制序列化性能
  let binary_start_time = get_current_time_millis()
  let mut binary_results = []
  for data in telemetry_data_list {
    let binary_data = BinarySerializer::serialize(binary_serializer, data)
    binary_results = binary_results + [binary_data]
  }
  let binary_serialize_time = get_current_time_millis() - binary_start_time
  
  // 测试二进制反序列化性能
  let binary_deserialize_start = get_current_time_millis()
  for binary_data in binary_results {
    BinarySerializer::deserialize(binary_serializer, binary_data)
  }
  let binary_deserialize_time = get_current_time_millis() - binary_deserialize_start
  
  // 性能断言
  assert_true(json_serialize_time < 5000) // JSON序列化应该小于5秒
  assert_true(json_deserialize_time < 5000) // JSON反序列化应该小于5秒
  assert_true(binary_serialize_time < 3000) // 二进制序列化应该小于3秒
  assert_true(binary_deserialize_time < 3000) // 二进制反序列化应该小于3秒
  
  // 二进制序列化应该比JSON更快
  assert_true(binary_serialize_time < json_serialize_time)
  assert_true(binary_deserialize_time < json_deserialize_time)
  
  // 计算数据大小
  let mut json_total_size = 0
  for json_string in json_results {
    json_total_size = json_total_size + json_string.length()
  }
  
  let mut binary_total_size = 0
  for binary_data in binary_results {
    binary_total_size = binary_total_size + binary_data.length()
  }
  
  // 二进制数据应该更紧凑
  let size_ratio = (binary_total_size as Float) / (json_total_size as Float)
  assert_true(size_ratio < 0.8) // 二进制应该比JSON小至少20%
}

// 测试7: 序列化版本兼容性
test "序列化版本兼容性测试" {
  // 创建版本1的数据
  let v1_data = TelemetryData::v1()
  TelemetryData::v1_set_trace_id(v1_data, "trace123")
  TelemetryData::v1_set_span_id(v1_data, "span456")
  TelemetryData::v1_add_attribute(v1_data, "service", StringValue("test-service"))
  
  // 使用版本1序列化器序列化
  let v1_serializer = V1Serializer::new()
  let v1_serialized = V1Serializer::serialize(v1_serializer, v1_data)
  
  // 使用版本2反序列化器反序列化
  let v2_deserializer = V2Deserializer::new()
  let v2_data = V2Deserializer::deserialize(v2_deserializer, v1_serialized)
  
  // 验证基本字段仍然可读
  assert_eq(TelemetryData::trace_id(v2_data), "trace123")
  assert_eq(TelemetryData::span_id(v2_data), "span456")
  
  let service_attr = TelemetryData::get_attribute(v2_data, "service")
  match service_attr {
    Some(StringValue(service)) => assert_eq(service, "test-service")
    _ => assert_true(false)
  }
  
  // 创建版本2的数据（包含新字段）
  let v2_new_data = TelemetryData::new()
  TelemetryData::set_trace_id(v2_new_data, "trace789")
  TelemetryData::set_span_id(v2_new_data, "span012")
  TelemetryData::add_attribute(v2_new_data, "service", StringValue("new-service"))
  TelemetryData::add_attribute(v2_new_data, "version", StringValue("2.0")) // 新字段
  TelemetryData::add_attribute(v2_new_data, "metadata", MapValue([("key", "value")])) // 新类型
  
  // 使用版本2序列化器序列化
  let v2_serializer = V2Serializer::new()
  let v2_serialized = V2Serializer::serialize(v2_serializer, v2_new_data)
  
  // 使用版本1反序列化器反序列化
  let v1_deserializer = V1Deserializer::new()
  let v1_compatible_data = V1Deserializer::deserialize(v1_deserializer, v2_serialized)
  
  // 验证旧版本可以读取新版本的基本字段
  assert_eq(TelemetryData::trace_id(v1_compatible_data), "trace789")
  assert_eq(TelemetryData::span_id(v1_compatible_data), "span012")
  
  let service_attr_compat = TelemetryData::get_attribute(v1_compatible_data, "service")
  match service_attr_compat {
    Some(StringValue(service)) => assert_eq(service, "new-service")
    _ => assert_true(false)
  }
  
  // 新字段应该被忽略
  let version_attr = TelemetryData::get_attribute(v1_compatible_data, "version")
  match version_attr {
    Some(_) => assert_true(false) // 不应该存在
    None => assert_true(true)
  }
  
  let metadata_attr = TelemetryData::get_attribute(v1_compatible_data, "metadata")
  match metadata_attr {
    Some(_) => assert_true(false) // 不应该存在
    None => assert_true(true)
  }
}

// 测试8: 流式序列化
test "流式序列化测试" {
  let stream_serializer = StreamSerializer::new()
  
  // 创建大量数据
  let mut telemetry_data_list = []
  for i in 0..10000 {
    let telemetry_data = TelemetryData::new()
    TelemetryData::set_trace_id(telemetry_data, "trace_" + i.to_string())
    TelemetryData::set_span_id(telemetry_data, "span_" + i.to_string())
    TelemetryData::add_attribute(telemetry_data, "index", IntValue(i))
    telemetry_data_list = telemetry_data_list + [telemetry_data]
  }
  
  // 流式序列化
  let stream = StreamSerializer::create_stream(stream_serializer)
  for data in telemetry_data_list {
    StreamSerializer::serialize_to_stream(stream_serializer, stream, data)
  }
  StreamSerializer::finalize_stream(stream_serializer, stream)
  
  // 获取流数据
  let stream_data = StreamSerializer::get_stream_data(stream)
  
  // 验证流数据
  assert_true(stream_data.length() > 0)
  
  // 流式反序列化
  let read_stream = StreamSerializer::open_read_stream(stream_serializer, stream_data)
  let mut deserialized_count = 0
  
  while StreamSerializer::has_more(read_stream) {
    let data = StreamSerializer::deserialize_from_stream(stream_serializer, read_stream)
    
    // 验证反序列化的数据
    let expected_trace_id = "trace_" + deserialized_count.to_string()
    assert_eq(TelemetryData::trace_id(data), expected_trace_id)
    
    let expected_span_id = "span_" + deserialized_count.to_string()
    assert_eq(TelemetryData::span_id(data), expected_span_id)
    
    let index_attr = TelemetryData::get_attribute(data, "index")
    match index_attr {
      Some(IntValue(index)) => assert_eq(index, deserialized_count)
      _ => assert_true(false)
    }
    
    deserialized_count = deserialized_count + 1
  }
  
  // 验证所有数据都被反序列化
  assert_eq(deserialized_count, 10000)
  
  // 测试流式压缩
  let compressed_stream = StreamSerializer::create_compressed_stream(stream_serializer)
  for data in telemetry_data_list {
    StreamSerializer::serialize_to_compressed_stream(stream_serializer, compressed_stream, data)
  }
  StreamSerializer::finalize_compressed_stream(stream_serializer, compressed_stream)
  
  let compressed_data = StreamSerializer::get_stream_data(compressed_stream)
  
  // 压缩数据应该更小
  assert_true(compressed_data.length() < stream_data.length())
  
  // 流式反压缩反序列化
  let compressed_read_stream = StreamSerializer::open_compressed_read_stream(stream_serializer, compressed_data)
  let mut compressed_deserialized_count = 0
  
  while StreamSerializer::has_more(compressed_read_stream) {
    let data = StreamSerializer::deserialize_from_compressed_stream(stream_serializer, compressed_read_stream)
    compressed_deserialized_count = compressed_deserialized_count + 1
  }
  
  // 验证压缩流中的数据完整性
  assert_eq(compressed_deserialized_count, 10000)
}

// 辅助函数和类型定义（模拟实现）
type JsonSerializer
type BinarySerializer
type ProtobufSerializer
type CustomSerializer
type V1Serializer
type V2Serializer
type V1Deserializer
type V2Deserializer
type StreamSerializer
type TelemetryData
type CircularObject
type LargeObject
type UnsupportedTypeObject

// 辅助函数：获取当前时间（毫秒）
fn get_current_time_millis() -> Int {
  // 模拟时间戳，实际应该使用系统时间API
  1609459200000 // 2021-01-01 00:00:00 UTC in milliseconds
}

// 辅助函数：浮点数绝对值
fn abs_float(x : Float) -> Float {
  if x < 0.0 { -x } else { x }
}
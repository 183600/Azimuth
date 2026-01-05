// Azimuth Data Conversion and Validation Test Suite
// 测试遥测系统的数据转换和验证功能，确保数据的准确性和一致性

// 测试1: 属性值类型转换
test "属性值类型转换测试" {
  // 字符串到整数的转换
  let string_int = StringValue("42")
  let converted_int = AttributeValue::to_int(string_int)
  match converted_int {
    Some(i) => assert_eq(i, 42)
    None => assert_true(false)
  }
  
  // 字符串到浮点数的转换
  let string_float = StringValue("3.14159")
  let converted_float = AttributeValue::to_float(string_float)
  match converted_float {
    Some(f) => assert_true(abs_float(f - 3.14159) < 0.0001)
    None => assert_true(false)
  }
  
  // 字符串到布尔值的转换
  let string_bool_true = StringValue("true")
  let converted_bool_true = AttributeValue::to_bool(string_bool_true)
  match converted_bool_true {
    Some(b) => assert_true(b)
    None => assert_true(false)
  }
  
  let string_bool_false = StringValue("false")
  let converted_bool_false = AttributeValue::to_bool(string_bool_false)
  match converted_bool_false {
    Some(b) => assert_false(b)
    None => assert_true(false)
  }
  
  // 整数到字符串的转换
  let int_value = IntValue(123)
  let converted_string = AttributeValue::to_string(int_value)
  match converted_string {
    Some(s) => assert_eq(s, "123")
    None => assert_true(false)
  }
  
  // 布尔值到字符串的转换
  let bool_value = BoolValue(true)
  let converted_bool_string = AttributeValue::to_string(bool_value)
  match converted_bool_string {
    Some(s) => assert_eq(s, "true")
    None => assert_true(false)
  }
  
  // 测试无效转换
  let invalid_int = StringValue("not_a_number")
  let invalid_int_result = AttributeValue::to_int(invalid_int)
  match invalid_int_result {
    Some(_) => assert_true(false) // 不应该成功转换
    None => assert_true(true) // 应该返回None
  }
}

// 测试2: 追踪ID和跨度ID验证
test "追踪ID和跨度ID验证测试" {
  // 有效的追踪ID验证（32个十六进制字符）
  let valid_trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  assert_true(TraceId::is_valid(valid_trace_id))
  
  // 有效的跨度ID验证（16个十六进制字符）
  let valid_span_id = "00f067aa0ba902b7"
  assert_true(SpanId::is_valid(valid_span_id))
  
  // 无效的追踪ID验证（长度不正确）
  let invalid_trace_id_short = "4bf92f3577b34da6"
  assert_false(TraceId::is_valid(invalid_trace_id_short))
  
  let invalid_trace_id_long = "4bf92f3577b34da6a3ce929d0e0e4736abc123"
  assert_false(TraceId::is_valid(invalid_trace_id_long))
  
  // 无效的跨度ID验证（长度不正确）
  let invalid_span_id_short = "00f067aa"
  assert_false(SpanId::is_valid(invalid_span_id_short))
  
  let invalid_span_id_long = "00f067aa0ba902b7abc123"
  assert_false(SpanId::is_valid(invalid_span_id_long))
  
  // 无效的十六进制字符
  let invalid_hex_trace = "4bf92f3577b34da6a3ce929d0e0e47g6" // 包含'g'
  assert_false(TraceId::is_valid(invalid_hex_trace))
  
  // 空字符串验证
  assert_false(TraceId::is_valid(""))
  assert_false(SpanId::is_valid(""))
  
  // 全零ID验证（特殊情况下可能有效）
  let all_zero_trace = "00000000000000000000000000000000"
  assert_true(TraceId::is_valid(all_zero_trace)) // 格式上有效，但语义上可能无效
  
  let all_zero_span = "0000000000000000"
  assert_true(SpanId::is_valid(all_zero_span)) // 格式上有效，但语义上可能无效
}

// 测试3: 时间戳转换和验证
test "时间戳转换和验证测试" {
  // Unix时间戳转换测试
  let unix_timestamp = 1609459200L // 2021-01-01 00:00:00 UTC
  let iso_string = Timestamp::to_iso_string(unix_timestamp)
  assert_eq(iso_string, "2021-01-01T00:00:00Z")
  
  // ISO字符串到时间戳的转换
  let parsed_timestamp = Timestamp::from_iso_string("2021-01-01T00:00:00Z")
  match parsed_timestamp {
    Some(ts) => assert_eq(ts, 1609459200L)
    None => assert_true(false)
  }
  
  // 带毫秒的ISO字符串转换
  let iso_with_ms = "2021-01-01T12:34:56.789Z"
  let parsed_with_ms = Timestamp::from_iso_string(iso_with_ms)
  match parsed_with_ms {
    Some(ts) => assert_eq(ts, 1609459200L + 12 * 3600 + 34 * 60 + 56) // 简化验证
    None => assert_true(false)
  }
  
  // 无效时间戳格式
  let invalid_iso = "not-a-timestamp"
  let invalid_result = Timestamp::from_iso_string(invalid_iso)
  match invalid_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 时间戳范围验证
  let min_timestamp = 0L
  let max_timestamp = 253402300799L // 9999-12-31 23:59:59 UTC
  
  assert_true(Timestamp::is_valid_range(min_timestamp))
  assert_true(Timestamp::is_valid_range(max_timestamp))
  
  let negative_timestamp = -1L
  assert_false(Timestamp::is_valid_range(negative_timestamp))
  
  let too_large_timestamp = 253402300800L // 超过最大值
  assert_false(Timestamp::is_valid_range(too_large_timestamp))
}

// 测试4: 采样标志验证和转换
test "采样标志验证和转换测试" {
  // 布尔值到采样标志的转换
  let sampled_true = SamplingDecision::from_bool(true)
  assert_eq(sampled_true, RecordAndSample)
  
  let sampled_false = SamplingDecision::from_bool(false)
  assert_eq(sampled_false, Drop)
  
  // 字符串到采样标志的转换
  let sampled_string_true = SamplingDecision::from_string("true")
  assert_eq(sampled_string_true, RecordAndSample)
  
  let sampled_string_false = SamplingDecision::from_string("false")
  assert_eq(sampled_string_false, Drop)
  
  let sampled_string_one = SamplingDecision::from_string("1")
  assert_eq(sampled_string_one, RecordAndSample)
  
  let sampled_string_zero = SamplingDecision::from_string("0")
  assert_eq(sampled_string_zero, Drop)
  
  // 无效采样字符串
  let invalid_sampling = SamplingDecision::from_string("invalid")
  assert_eq(invalid_sampling, Drop) // 默认为不采样
  
  // 采样标志到字符串的转换
  let sampling_to_string_true = SamplingDecision::to_string(RecordAndSample)
  assert_eq(sampling_to_string_true, "true")
  
  let sampling_to_string_false = SamplingDecision::to_string(Drop)
  assert_eq(sampling_to_string_false, "false")
  
  // 采样标志到布尔值的转换
  let sampling_to_bool_true = SamplingDecision::to_bool(RecordAndSample)
  assert_true(sampling_to_bool_true)
  
  let sampling_to_bool_false = SamplingDecision::to_bool(Drop)
  assert_false(sampling_to_bool_false)
}

// 测试5: 属性键名验证
test "属性键名验证测试" {
  // 有效的属性键名
  assert_true(AttributeKey::is_valid("service.name"))
  assert_true(AttributeKey::is_valid("http.method"))
  assert_true(AttributeKey::is_valid("db.system"))
  assert_true(AttributeKey::is_valid("user.id"))
  assert_true(AttributeKey::is_valid("custom.attribute"))
  
  // 无效的属性键名（空字符串）
  assert_false(AttributeKey::is_valid(""))
  
  // 无效的属性键名（包含非法字符）
  assert_false(AttributeKey::is_valid("invalid key with spaces"))
  assert_false(AttributeKey::is_valid("invalid.key.with.dots..double.dots"))
  assert_false(AttributeKey::is_valid(".starts.with.dot"))
  assert_false(AttributeKey::is_valid("ends.with.dot."))
  assert_false(AttributeKey::is_valid("contains@special#chars"))
  
  // 键名长度验证
  let very_long_key = "a".repeat(300) // 超长键名
  assert_false(AttributeKey::is_valid(very_long_key))
  
  let max_length_key = "a".repeat(255) // 最大长度键名
  assert_true(AttributeKey::is_valid(max_length_key))
  
  // 键名规范化
  let normalized_key = AttributeKey::normalize("Service.Name") // 大写转小写
  assert_eq(normalized_key, "service.name")
  
  let normalized_with_spaces = AttributeKey::normalize(" service name ") // 去除空格
  assert_eq(normalized_with_spaces, "service.name")
}

// 测试6: 度量单位转换和验证
test "度量单位转换和验证测试" {
  // 时间单位转换
  let seconds_to_ms = MetricUnit::convert(1.0, "s", "ms")
  assert_true(abs_float(seconds_to_ms - 1000.0) < 0.001)
  
  let ms_to_seconds = MetricUnit::convert(1000.0, "ms", "s")
  assert_true(abs_float(ms_to_seconds - 1.0) < 0.001)
  
  let microseconds_to_ms = MetricUnit::convert(1000.0, "us", "ms")
  assert_true(abs_float(microseconds_to_ms - 1.0) < 0.001)
  
  // 字节单位转换
  let bytes_to_kb = MetricUnit::convert(1024.0, "By", "kB")
  assert_true(abs_float(bytes_to_kb - 1.0) < 0.001)
  
  let kb_to_mb = MetricUnit::convert(1024.0, "kB", "MB")
  assert_true(abs_float(kb_to_mb - 1.0) < 0.001)
  
  // 无效单位转换
  let invalid_conversion = MetricUnit::convert(100.0, "invalid", "unit")
  match invalid_conversion {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 不兼容的单位转换
  let incompatible_conversion = MetricUnit::convert(100.0, "s", "By")
  match incompatible_conversion {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 单位验证
  assert_true(MetricUnit::is_valid("s")) // 秒
  assert_true(MetricUnit::is_valid("ms")) // 毫秒
  assert_true(MetricUnit::is_valid("us")) // 微秒
  assert_true(MetricUnit::is_valid("ns")) // 纳秒
  assert_true(MetricUnit::is_valid("By")) // 字节
  assert_true(MetricUnit::is_valid("kB")) // 千字节
  assert_true(MetricUnit::is_valid("MB")) // 兆字节
  
  assert_false(MetricUnit::is_valid("invalid"))
  assert_false(MetricUnit::is_valid(""))
  assert_false(MetricUnit::is_valid("second")) // 不是标准缩写
}

// 测试7: 状态码转换和验证
test "状态码转换和验证测试" {
  // HTTP状态码转换
  let http_status_200 = StatusCode::from_http(200)
  assert_eq(http_status_200, Ok)
  
  let http_status_400 = StatusCode::from_http(400)
  assert_eq(http_status_400, Error)
  
  let http_status_500 = StatusCode::from_http(500)
  assert_eq(http_status_500, Error)
  
  let http_status_301 = StatusCode::from_http(301)
  assert_eq(http_status_301, Unset)
  
  // gRPC状态码转换
  let grpc_status_ok = StatusCode::from_grpc(0) // gRPC OK
  assert_eq(grpc_status_ok, Ok)
  
  let grpc_status_cancelled = StatusCode::from_grpc(1) // gRPC CANCELLED
  assert_eq(grpc_status_cancelled, Error)
  
  let grpc_status_unknown = StatusCode::from_grpc(2) // gRPC UNKNOWN
  assert_eq(grpc_status_unknown, Error)
  
  // 状态码到HTTP的转换
  let status_to_http_ok = StatusCode::to_http(Ok)
  assert_eq(status_to_http_ok, 200)
  
  let status_to_http_error = StatusCode::to_http(Error)
  assert_eq(status_to_http_error, 500)
  
  let status_to_http_unset = StatusCode::to_http(Unset)
  assert_eq(status_to_http_unset, 200) // 默认为200
  
  // 状态码描述
  let ok_description = StatusCode::description(Ok)
  assert_eq(ok_description, "OK")
  
  let error_description = StatusCode::description(Error)
  assert_eq(error_description, "ERROR")
  
  let unset_description = StatusCode::description(Unset)
  assert_eq(unset_description, "UNSET")
}

// 辅助函数：浮点数绝对值
fn abs_float(x : Float) -> Float {
  if x < 0.0 { -x } else { x }
}

// 辅助函数：字符串重复
fn string_repeat(s : String, count : Int) -> String {
  let mut result = ""
  for i in 0..count {
    result = result + s
  }
  result
}
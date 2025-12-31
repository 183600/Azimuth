// 数据格式转换和兼容性测试
// 测试不同数据格式间的转换和向后兼容性

test "data_format_attribute_value_conversion" {
  // 测试属性值类型的转换
  
  // 原始属性值
  let original_attributes = [
    ("string_attr", common::AttributeValue::string("test_value")),
    ("int_attr", common::AttributeValue::int(42L)),
    ("float_attr", common::AttributeValue::float(3.14159)),
    ("bool_attr", common::AttributeValue::bool(true)),
    ("string_array_attr", common::AttributeValue::array_string(["a", "b", "c"])),
    ("int_array_attr", common::AttributeValue::array_int([1L, 2L, 3L])),
    ("float_array_attr", common::AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool_array_attr", common::AttributeValue::array_bool([true, false, true]))
  ]
  
  // 模拟类型转换
  let converted_attributes = []
  let mut i = 0
  
  while i < original_attributes.length() {
    let (key, value) = original_attributes[i]
    
    let converted_value = match value {
      common::StringValue(s) => {
        // 字符串转其他类型
        if s == "true" {
          common::AttributeValue::bool(true)
        } else if s == "false" {
          common::AttributeValue::bool(false)
        } else if s.has_prefix("int:") {
          let num_str = s.substring(4, s.length() - 4)
          common::AttributeValue::int(num_str.to_int64())
        } else if s.has_prefix("float:") {
          let num_str = s.substring(6, s.length() - 6)
          common::AttributeValue::float(num_str.to_double())
        } else {
          value  // 保持原类型
        }
      }
      common::IntValue(i_val) => {
        // 整数转字符串
        common::AttributeValue::string(i_val.to_string())
      }
      common::FloatValue(f_val) => {
        // 浮点数转字符串
        common::AttributeValue::string(f_val.to_string())
      }
      common::BoolValue(b_val) => {
        // 布尔值转字符串
        common::AttributeValue::string(if b_val { "true" } else { "false" })
      }
      common::ArrayStringValue(arr) => {
        // 字符串数组转逗号分隔的字符串
        let mut result = ""
        let mut j = 0
        while j < arr.length() {
          result = result + arr[j]
          if j < arr.length() - 1 {
            result = result + ","
          }
          j = j + 1
        }
        common::AttributeValue::string(result)
      }
      _ => value  // 其他类型保持不变
    }
    
    converted_attributes.push((key, converted_value))
    i = i + 1
  }
  
  // 验证转换结果
  assert_eq(converted_attributes.length(), original_attributes.length())
  
  // 验证特定转换
  match converted_attributes[1].1 {  // int_attr
    common::StringValue(s) => assert_eq(s, "42")
    _ => @test.fail("Int should be converted to string")
  }
  
  match converted_attributes[2].1 {  // float_attr
    common::StringValue(s) => assert_eq(s, "3.14159")
    _ => @test.fail("Float should be converted to string")
  }
  
  match converted_attributes[3].1 {  // bool_attr
    common::StringValue(s) => assert_eq(s, "true")
    _ => @test.fail("Bool should be converted to string")
  }
  
  match converted_attributes[4].1 {  // string_array_attr
    common::StringValue(s) => assert_eq(s, "a,b,c")
    _ => @test.fail("String array should be converted to comma-separated string")
  }
  
  // 验证未转换的类型
  match converted_attributes[0].1 {  // string_attr
    common::StringValue(s) => assert_eq(s, "test_value")
    _ => @test.fail("String should remain unchanged")
  }
}

test "data_format_serialization_compatibility" {
  // 测试序列化格式的兼容性
  
  // 模拟不同版本的数据格式
  let v1_format = {
    span_name: "operation_v1",
    timestamp: 1640995200L,  // 秒级时间戳
    duration_ms: 500L,
    attributes: [("status", "success")]
  }
  
  let v2_format = {
    span_name: "operation_v2",
    timestamp_unix_nanos: 1640995200000000000L,  // 纳秒级时间戳
    end_time_unix_nanos: Some(1640995200500000000L),
    duration_nanos: 500000000L,
    attributes: [("status", common::AttributeValue::string("success"))],
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331"
  }
  
  // V1到V2的转换
  let v1_to_v2_converted = {
    span_name: v1_format.span_name,
    timestamp_unix_nanos: v1_format.timestamp * 1000000000L,
    end_time_unix_nanos: Some((v1_format.timestamp + v1_format.duration_ms / 1000L) * 1000000000L),
    duration_nanos: v1_format.duration_ms * 1000000L,
    attributes: [],
    trace_id: "00000000000000000000000000000000",  // V1格式没有trace_id
    span_id: "0000000000000000"  // V1格式没有span_id
  }
  
  // 转换V1的属性到V2格式
  let v2_attributes = []
  let mut i = 0
  while i < v1_format.attributes.length() {
    let (key, value) = v1_format.attributes[i]
    v2_attributes.push((key, common::AttributeValue::string(value)))
    i = i + 1
  }
  
  let final_v2_format = {
    ..v1_to_v2_converted,
    attributes: v2_attributes
  }
  
  // 验证转换结果
  assert_eq(final_v2_format.span_name, v1_format.span_name)
  assert_eq(final_v2_format.timestamp_unix_nanos, v1_format.timestamp * 1000000000L)
  assert_eq(final_v2_format.duration_nanos, v1_format.duration_ms * 1000000L)
  assert_eq(final_v2_format.attributes.length(), v1_format.attributes.length())
  
  // 验证属性转换
  match final_v2_format.attributes[0].1 {
    common::StringValue(s) => assert_eq(s, "success")
    _ => @test.fail("Attribute should be converted to string value")
  }
  
  // V2到V1的转换（向下兼容）
  let v2_to_v1_converted = {
    span_name: v2_format.span_name,
    timestamp: v2_format.timestamp_unix_nanos / 1000000000L,
    duration_ms: v2_format.duration_nanos / 1000000L,
    attributes: []
  }
  
  // 转换V2的属性到V1格式
  let v1_attributes = []
  let mut j = 0
  while j < v2_format.attributes.length() {
    let (key, value) = v2_format.attributes[j]
    match value {
      common::StringValue(s) => v1_attributes.push((key, s))
      _ => v1_attributes.push((key, "unknown_type"))
    }
    j = j + 1
  }
  
  let final_v1_format = {
    ..v2_to_v1_converted,
    attributes: v1_attributes
  }
  
  // 验证向下兼容转换
  assert_eq(final_v1_format.span_name, v2_format.span_name)
  assert_eq(final_v1_format.timestamp, v2_format.timestamp_unix_nanos / 1000000000L)
  assert_eq(final_v1_format.duration_ms, v2_format.duration_nanos / 1000000L)
  assert_eq(final_v1_format.attributes[0].1, "success")
}

test "data_format_cross_protocol_conversion" {
  // 测试跨协议数据格式转换
  
  // OpenTelemetry格式
  let otel_format = {
    trace_id: [10_byte, 15_byte, 20_byte, 25_byte, 30_byte, 35_byte, 40_byte, 45_byte,
                50_byte, 55_byte, 60_byte, 65_byte, 70_byte, 75_byte, 80_byte, 85_byte],
    span_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte],
    name: "otel_operation",
    kind: "SERVER",
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200300000000L),
    attributes: [
      ("service.name", "otel-service"),
      ("operation.type", "http_request")
    ],
    status: "OK"
  }
  
  // Zipkin格式
  let zipkin_format = {
    traceId: "0af7651916cd43dd8448eb211c80319c",
    id: "0102030405060708",
    name: "otel_operation",
    timestamp: 1640995200000L,  // 微秒时间戳
    duration: 30000,  // 微秒持续时间
    localEndpoint: { serviceName: "otel-service" },
    tags: [
      ("operation.type", "http_request"),
      ("otel.status", "OK")
    ]
  }
  
  // OpenTelemetry到Zipkin的转换
  let trace_id_hex = []
  let mut i = 0
  while i < otel_format.trace_id.length() {
    let byte_val = otel_format.trace_id[i]
    let hex_str = if byte_val < 16_byte { "0" } else { "" } + byte_val.to_int().to_string(16)
    trace_id_hex.push(hex_str)
    i = i + 1
  }
  
  let span_id_hex = []
  let mut j = 0
  while j < otel_format.span_id.length() {
    let byte_val = otel_format.span_id[j]
    let hex_str = if byte_val < 16_byte { "0" } else { "" } + byte_val.to_int().to_string(16)
    span_id_hex.push(hex_str)
    j = j + 1
  }
  
  let converted_trace_id = ""
  let mut k = 0
  while k < trace_id_hex.length() {
    converted_trace_id = converted_trace_id + trace_id_hex[k]
    k = k + 1
  }
  
  let converted_span_id = ""
  let mut l = 0
  while l < span_id_hex.length() {
    converted_span_id = converted_span_id + span_id_hex[l]
    l = l + 1
  }
  
  let otel_to_zipkin = {
    traceId: converted_trace_id,
    id: converted_span_id,
    name: otel_format.name,
    timestamp: otel_format.start_time_unix_nanos / 1000000L,  // 纳秒转微秒
    duration: (otel_format.end_time_unix_nanos? - otel_format.start_time_unix_nanos) / 1000000L,
    localEndpoint: { 
      serviceName: match otel_format.attributes[0].1 {
        common::StringValue(s) => s
        _ => "unknown"
      }
    },
    tags: [
      (otel_format.attributes[1].0, match otel_format.attributes[1].1 {
        common::StringValue(s) => s
        _ => "unknown"
      }),
      ("otel.status", otel_format.status)
    ]
  }
  
  // 验证转换结果
  assert_eq(otel_to_zipkin.traceId.length(), 32)  // 32个十六进制字符
  assert_eq(otel_to_zipkin.id.length(), 16)       // 16个十六进制字符
  assert_eq(otel_to_zipkin.name, otel_format.name)
  assert_eq(otel_to_zipkin.duration, 30000L)
  
  // 验证时间戳转换
  assert_eq(otel_to_zipkin.timestamp, 1640995200000L)
  assert_eq(otel_to_zipkin.duration, 30000L)
  
  // 验证属性转换
  assert_eq(otel_to_zipkin.localEndpoint.serviceName, "otel-service")
  assert_eq(otel_to_zipkin.tags.length(), 2)
  assert_eq(otel_to_zipkin.tags[0].1, "http_request")
  assert_eq(otel_to_zipkin.tags[1].1, "OK")
}

test "data_format_backward_compatibility" {
  // 测试向后兼容性
  
  // 模拟旧版本客户端发送的数据
  let legacy_data = [
    {
      version: "0.9.0",
      span_name: "legacy_operation",
      timestamp: 1640995200L,  // 秒级时间戳
      duration: 500,  // 毫秒
      metadata: {
        service: "legacy-service",
        version: "1.0.0"
      }
    },
    {
      version: "1.0.0", 
      span_name: "modern_operation",
      timestamp_unix_nanos: 1640995200000000000L,
      duration_nanos: 500000000L,
      attributes: [
        ("service.name", "modern-service"),
        ("service.version", "2.0.0")
      ]
    }
  ]
  
  // 统一处理不同版本的数据
  let normalized_data = []
  let mut i = 0
  
  while i < legacy_data.length() {
    let data = legacy_data[i]
    
    let normalized = match data.version {
      "0.9.0" => {
        // 升级旧版本格式
        {
          span_name: data.span_name,
          timestamp_unix_nanos: data.timestamp * 1000000000L,
          duration_nanos: data.duration * 1000000L,
          attributes: [
            ("service.name", common::AttributeValue::string(data.metadata.service)),
            ("service.version", common::AttributeValue::string(data.metadata.version))
          ],
          original_version: data.version
        }
      }
      "1.0.0" => {
        // 现代格式直接使用
        {
          span_name: data.span_name,
          timestamp_unix_nanos: data.timestamp_unix_nanos,
          duration_nanos: data.duration_nanos,
          attributes: data.attributes,
          original_version: data.version
        }
      }
      _ => {
        // 未知版本，使用默认值
        {
          span_name: "unknown_operation",
          timestamp_unix_nanos: 0L,
          duration_nanos: 0L,
          attributes: [],
          original_version: "unknown"
        }
      }
    }
    
    normalized_data.push(normalized)
    i = i + 1
  }
  
  // 验证标准化结果
  assert_eq(normalized_data.length(), legacy_data.length())
  
  // 验证旧版本数据的升级
  let legacy_normalized = normalized_data[0]
  assert_eq(legacy_normalized.span_name, "legacy_operation")
  assert_eq(legacy_normalized.timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(legacy_normalized.duration_nanos, 500000000L)
  assert_eq(legacy_normalized.original_version, "0.9.0")
  
  match legacy_normalized.attributes[0].1 {
    common::StringValue(s) => assert_eq(s, "legacy-service")
    _ => @test.fail("Service name should be string")
  }
  
  // 验证现代版本数据
  let modern_normalized = normalized_data[1]
  assert_eq(modern_normalized.span_name, "modern_operation")
  assert_eq(modern_normalized.timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(modern_normalized.duration_nanos, 500000000L)
  assert_eq(modern_normalized.original_version, "1.0.0")
  
  match modern_normalized.attributes[0].1 {
    common::StringValue(s) => assert_eq(s, "modern-service")
    _ => @test.fail("Service name should be string")
  }
  
  // 验证数据格式一致性
  let mut j = 0
  while j < normalized_data.length() {
    let data = normalized_data[j]
    assert_eq(data.timestamp_unix_nanos > 0L, true)
    assert_eq(data.duration_nanos >= 0L, true)
    assert_eq(data.span_name.length() > 0, true)
    assert_eq(data.original_version.length() > 0, true)
    j = j + 1
  }
}

test "data_format_attribute_name_standardization" {
  // 测试属性名称标准化
  
  // 不同风格的属性名称
  let non_standard_attributes = [
    ("ServiceName", "web-api"),           // 驼峰命名
    ("service-name", "worker-service"),   // 连字符命名
    ("service_name", "data-processor"),   // 下划线命名
    ("SERVICE.NAME", "cache-service"),    // 大写和点号
    ("service.id", "instance-123"),       // 标准命名
    ("http.method", "GET"),               // 标准命名
    ("db_system", "postgresql"),          // 下划线命名
    ("customMetric", "value123")          // 驼峰命名
  ]
  
  // 标准化属性名称
  let standardized_attributes = []
  let mut i = 0
  
  while i < non_standard_attributes.length() {
    let (original_name, value) = non_standard_attributes[i]
    
    // 标准化规则：转换为小写，用点号分隔单词
    let standardized_name = 
      if original_name.has("_") {
        original_name.replace("_", ".")
      } else if original_name.has("-") {
        original_name.replace("-", ".")
      } else {
        // 驼峰转点号分隔（简化实现）
        let mut result = ""
        let mut j = 0
        while j < original_name.length() {
          let char = original_name[j]
          if char.is_uppercase() && j > 0 {
            result = result + "." + char.to_lowercase()
          } else {
            result = result + char.to_lowercase()
          }
          j = j + 1
        }
        result
      }
    
    standardized_attributes.push((standardized_name, common::AttributeValue::string(value)))
    i = i + 1
  }
  
  // 验证标准化结果
  assert_eq(standardized_attributes.length(), non_standard_attributes.length())
  
  // 验证特定标准化
  let expected_names = [
    "service.name",
    "service.name", 
    "service.name",
    "service.name",
    "service.id",
    "http.method",
    "db.system",
    "custom.metric"
  ]
  
  let mut j = 0
  while j < standardized_attributes.length() {
    let (actual_name, _) = standardized_attributes[j]
    let expected_name = expected_names[j]
    assert_eq(actual_name, expected_name)
    j = j + 1
  }
  
  // 验证属性值保持不变
  let expected_values = ["web-api", "worker-service", "data-processor", "cache-service", 
                         "instance-123", "GET", "postgresql", "value123"]
  
  let mut k = 0
  while k < standardized_attributes.length() {
    let (_, value) = standardized_attributes[k]
    match value {
      common::StringValue(s) => assert_eq(s, expected_values[k])
      _ => @test.fail("Value should be string")
    }
    k = k + 1
  }
}
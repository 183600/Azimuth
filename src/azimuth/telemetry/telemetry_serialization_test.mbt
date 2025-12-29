// 遥测数据序列化测试 - 验证不同格式的数据序列化和反序列化

test "json_span_serialization" {
  // 测试Span数据的JSON序列化
  
  let span_name = "http_request"
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let start_time = 1640995200000000000L
  let duration = 150000000L // 150ms
  
  // 创建JSON格式的Span数据
  let json_span = "{"
  json_span = json_span + "\"name\":\"" + span_name + "\","
  json_span = json_span + "\"traceId\":\"" + trace_id + "\","
  json_span = json_span + "\"spanId\":\"" + span_id + "\","
  json_span = json_span + "\"startTimeUnixNano\":\"" + start_time.to_string() + "\","
  json_span = json_span + "\"durationNano\":\"" + duration.to_string() + "\","
  json_span = json_span + "\"status\":{\"code\":\"OK\"}"
  json_span = json_span + "}"
  
  // 验证JSON格式
  assert_eq(json_span.has_prefix("{"), true)
  assert_eq(json_span.has_suffix("}"), true)
  assert_eq(json_span.contains("\"name\":\"http_request\""), true)
  assert_eq(json_span.contains("\"traceId\":\"0af7651916cd43dd8448eb211c80319c\""), true)
  assert_eq(json_span.contains("\"spanId\":\"b7ad6b7169203331\""), true)
  assert_eq(json_span.contains("\"status\":{\"code\":\"OK\"}"), true)
  
  // 验证JSON结构完整性
  assert_eq(json_span.contains("\"name\":"), true)
  assert_eq(json_span.contains("\"traceId\":"), true)
  assert_eq(json_span.contains("\"spanId\":"), true)
  assert_eq(json_span.contains("\"startTimeUnixNano\":"), true)
  assert_eq(json_span.contains("\"durationNano\":"), true)
  assert_eq(json_span.contains("\"status\":"), true)
}

test "json_metric_serialization" {
  // 测试Metric数据的JSON序列化
  
  let metric_name = "cpu_usage"
  let metric_value = 75.5
  let metric_unit = "percent"
  let metric_timestamp = 1640995200L
  let metric_tags = "service:api,env:production"
  
  // 创建JSON格式的Metric数据
  let json_metric = "{"
  json_metric = json_metric + "\"name\":\"" + metric_name + "\","
  json_metric = json_metric + "\"value\":" + metric_value.to_string() + ","
  json_metric = json_metric + "\"unit\":\"" + metric_unit + "\","
  json_metric = json_metric + "\"timestamp\":\"" + metric_timestamp.to_string() + "\","
  json_metric = json_metric + "\"attributes\":{"
  json_metric = json_metric + "\"service\":\"api\","
  json_metric = json_metric + "\"environment\":\"production\""
  json_metric = json_metric + "}"
  json_metric = json_metric + "}"
  
  // 验证JSON格式
  assert_eq(json_metric.has_prefix("{"), true)
  assert_eq(json_metric.has_suffix("}"), true)
  assert_eq(json_metric.contains("\"name\":\"cpu_usage\""), true)
  assert_eq(json_metric.contains("\"value\":75.5"), true)
  assert_eq(json_metric.contains("\"unit\":\"percent\""), true)
  assert_eq(json_metric.contains("\"attributes\":"), true)
  
  // 验证嵌套属性
  assert_eq(json_metric.contains("\"service\":\"api\""), true)
  assert_eq(json_metric.contains("\"environment\":\"production\""), true)
}

test "json_log_serialization" {
  // 测试Log数据的JSON序列化
  
  let log_message = "User login successful"
  let severity = "INFO"
  let log_timestamp = 1640995300L
  let user_id = "user_12345"
  
  // 创建JSON格式的Log数据
  let json_log = "{"
  json_log = json_log + "\"timestamp\":\"" + log_timestamp.to_string() + "\","
  json_log = json_log + "\"severity\":\"" + severity + "\","
  json_log = json_log + "\"message\":\"" + log_message + "\","
  json_log = json_log + "\"attributes\":{"
  json_log = json_log + "\"user_id\":\"" + user_id + "\","
  json_log = json_log + "\"action\":\"login\","
  json_log = json_log + "\"result\":\"success\""
  json_log = json_log + "}"
  json_log = json_log + "}"
  
  // 验证JSON格式
  assert_eq(json_log.has_prefix("{"), true)
  assert_eq(json_log.has_suffix("}"), true)
  assert_eq(json_log.contains("\"timestamp\":\"1640995300\""), true)
  assert_eq(json_log.contains("\"severity\":\"INFO\""), true)
  assert_eq(json_log.contains("\"message\":\"User login successful\""), true)
  
  // 验证日志属性
  assert_eq(json_log.contains("\"user_id\":\"user_12345\""), true)
  assert_eq(json_log.contains("\"action\":\"login\""), true)
  assert_eq(json_log.contains("\"result\":\"success\""), true)
}

test "prometheus_metric_format" {
  // 测试Prometheus格式的Metric数据
  
  let metric_name = "http_requests_total"
  let metric_value = 1234
  let metric_labels = "method=\"GET\",status=\"200\",service=\"api\""
  let metric_timestamp = 1640995200000L
  
  // 创建Prometheus格式的Metric数据
  let prometheus_metric = metric_name + "{" + metric_labels + "} " + metric_value.to_string()
  prometheus_metric = prometheus_metric + " " + metric_timestamp.to_string()
  
  // 验证Prometheus格式
  assert_eq(prometheus_metric.has_prefix(metric_name + "{"), true)
  assert_eq(prometheus_metric.contains("method=\"GET\""), true)
  assert_eq(prometheus_metric.contains("status=\"200\""), true)
  assert_eq(prometheus_metric.contains("service=\"api\""), true)
  assert_eq(prometheus_metric.contains("} 1234"), true)
  assert_eq(prometheus_metric.has_suffix(" " + metric_timestamp.to_string()), true)
  
  // 验证标签格式
  assert_eq(prometheus_metric.contains("{method=\"GET\",status=\"200\",service=\"api\"}"), true)
}

test "otel_collector_json_format" {
  // 测试OpenTelemetry Collector的JSON格式
  
  let resource_attributes = "service.name:payment-service,service.version:1.2.3"
  let span_attributes = "http.method:POST,http.status_code:200"
  
  // 创建OTLP JSON格式的数据
  let otlp_json = "{"
  otlp_json = otlp_json + "\"resourceSpans\":[{"
  otlp_json = otlp_json + "\"resource\":{"
  otlp_json = otlp_json + "\"attributes\":["
  otlp_json = otlp_json + "{\"key\":\"service.name\",\"value\":{\"stringValue\":\"payment-service\"}},"
  otlp_json = otlp_json + "{\"key\":\"service.version\",\"value\":{\"stringValue\":\"1.2.3\"}}"
  otlp_json = otlp_json + "]"
  otlp_json = otlp_json + "},"
  otlp_json = otlp_json + "\"scopeSpans\":[{"
  otlp_json = otlp_json + "\"spans\":[{"
  otlp_json = otlp_json + "\"name\":\"http_request\","
  otlp_json = otlp_json + "\"kind\":2,"
  otlp_json = otlp_json + "\"attributes\":["
  otlp_json = otlp_json + "{\"key\":\"http.method\",\"value\":{\"stringValue\":\"POST\"}},"
  otlp_json = otlp_json + "{\"key\":\"http.status_code\",\"value\":{\"intValue\":\"200\"}}"
  otlp_json = otlp_json + "]"
  otlp_json = otlp_json + "}"
  otlp_json = otlp_json + "]"
  otlp_json = otlp_json + "}"
  otlp_json = otlp_json + "}]"
  otlp_json = otlp_json + "}"
  
  // 验证OTLP JSON格式
  assert_eq(otlp_json.contains("\"resourceSpans\":"), true)
  assert_eq(otlp_json.contains("\"resource\":"), true)
  assert_eq(otlp_json.contains("\"scopeSpans\":"), true)
  assert_eq(otlp_json.contains("\"spans\":"), true)
  assert_eq(otlp_json.contains("\"attributes\":"), true)
  
  // 验证特定的OTLP字段
  assert_eq(otlp_json.contains("\"service.name\""), true)
  assert_eq(otlp_json.contains("\"service.version\""), true)
  assert_eq(otlp_json.contains("\"http.method\""), true)
  assert_eq(otlp_json.contains("\"http.status_code\""), true)
  assert_eq(otlp_json.contains("\"stringValue\""), true)
  assert_eq(otlp_json.contains("\"intValue\""), true)
}

test "csv_log_serialization" {
  // 测试CSV格式的Log数据序列化
  
  let log_entries = [
    "1640995200,INFO,User login,user_12345,success",
    "1640995210,WARN,Password retry,user_67890,failure",
    "1640995220,ERROR,Database error,,connection_timeout"
  ]
  
  // 验证CSV格式
  assert_eq(log_entries.length(), 3)
  
  // 验证第一个日志条目
  let entry1 = log_entries[0]
  assert_eq(entry1.contains("1640995200"), true)
  assert_eq(entry1.contains("INFO"), true)
  assert_eq(entry1.contains("User login"), true)
  assert_eq(entry1.contains("user_12345"), true)
  assert_eq(entry1.contains("success"), true)
  
  // 验证第二个日志条目
  let entry2 = log_entries[1]
  assert_eq(entry2.contains("1640995210"), true)
  assert_eq(entry2.contains("WARN"), true)
  assert_eq(entry2.contains("Password retry"), true)
  assert_eq(entry2.contains("user_67890"), true)
  assert_eq(entry2.contains("failure"), true)
  
  // 验证第三个日志条目（包含空字段）
  let entry3 = log_entries[2]
  assert_eq(entry3.contains("1640995220"), true)
  assert_eq(entry3.contains("ERROR"), true)
  assert_eq(entry3.contains("Database error"), true)
  assert_eq(entry3.contains(",,connection_timeout"), true) // 空用户ID字段
  
  // 创建CSV头部
  let csv_header = "timestamp,severity,message,user_id,result"
  assert_eq(csv_header.contains("timestamp"), true)
  assert_eq(csv_header.contains("severity"), true)
  assert_eq(csv_header.contains("message"), true)
  assert_eq(csv_header.contains("user_id"), true)
  assert_eq(csv_header.contains("result"), true)
}

test "binary_serialization_format" {
  // 测试二进制序列化格式（模拟）
  
  let trace_id_bytes = [0x0a, 0xf7, 0x65, 0x19, 0x16, 0xcd, 0x43, 0xdd]
  let span_id_bytes = [0xb7, 0xad, 0x6b, 0x71, 0x69, 0x20, 0x33, 0x31]
  
  // 验证字节数组
  assert_eq(trace_id_bytes.length(), 8)
  assert_eq(span_id_bytes.length(), 8)
  assert_eq(trace_id_bytes[0], 0x0a)
  assert_eq(span_id_bytes[7], 0x31)
  
  // 模拟二进制序列化（转换为十六进制字符串表示）
  let mut binary_data = ""
  let mut i = 0
  while i < trace_id_bytes.length() {
    let hex_byte = trace_id_bytes[i].to_string()
    binary_data = binary_data + hex_byte + ":"
    i = i + 1
  }
  
  i = 0
  while i < span_id_bytes.length() {
    let hex_byte = span_id_bytes[i].to_string()
    binary_data = binary_data + hex_byte + ":"
    i = i + 1
  }
  
  // 验证二进制数据格式
  assert_eq(binary_data.has_suffix(":"), true)
  assert_eq(binary_data.contains("10:"), true) // 0x0a
  assert_eq(binary_data.contains("247:"), true) // 0xf7
  assert_eq(binary_data.contains("183:"), true) // 0xb7
  assert_eq(binary_data.contains("173:"), true) // 0xad
  assert_eq(binary_data.contains("49:"), true) // 0x31
}

test "protobuf_serialization_simulation" {
  // 测试Protocol Buffers序列化模拟
  
  let field_numbers = [1, 2, 3, 4, 5] // 字段编号
  let field_values = ["span_name", "trace_id", "start_time", "duration", "status"]
  let wire_types = [2, 2, 0, 0, 0] // 线路类型：2=长度限定，0=变长整数
  
  // 验证Protobuf字段定义
  assert_eq(field_numbers.length(), 5)
  assert_eq(field_values.length(), 5)
  assert_eq(wire_types.length(), 5)
  
  // 模拟Protobuf序列化过程
  let mut protobuf_data = ""
  let mut i = 0
  while i < field_numbers.length() {
    let field_key = (field_numbers[i] << 3) | wire_types[i] // 字段键 = (字段编号 << 3) | 线路类型
    protobuf_data = protobuf_data + "key=" + field_key.to_string() + ":"
    protobuf_data = protobuf_data + "value=" + field_values[i] + ";"
    i = i + 1
  }
  
  // 验证Protobuf数据格式
  assert_eq(protobuf_data.contains("key=10:value=span_name;"), true) // (1 << 3) | 2 = 10
  assert_eq(protobuf_data.contains("key=18:value=trace_id;"), true) // (2 << 3) | 2 = 18
  assert_eq(protobuf_data.contains("key=24:value=start_time;"), true) // (3 << 3) | 0 = 24
  assert_eq(protobuf_data.contains("key=32:value=duration;"), true) // (4 << 3) | 0 = 32
  assert_eq(protobuf_data.contains("key=40:value=status;"), true) // (5 << 3) | 0 = 40
  
  // 验证字段完整性
  let fields = protobuf_data.split(";")
  assert_eq(fields.length(), 6) // 5个字段 + 最后一个空字符串
  assert_eq(fields[0], "key=10:value=span_name")
  assert_eq(fields[4], "key=40:value=status")
}
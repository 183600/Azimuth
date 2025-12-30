// 遥测数据导出测试
// 测试遥测数据到各种后端和格式的导出功能

test "otlp_export_format_test" {
  // 测试OTLP导出格式
  
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "11112222"
  let span_name = "http_request"
  let span_kind = "SERVER"
  let start_time = 1640995200000000000L // 纳秒时间戳
  let end_time = 1640995201000000000L
  
  // 创建OTLP格式的追踪数据
  let otlp_trace_data = "{"
  otlp_trace_data = otlp_trace_data + "\"trace_id\":\"" + trace_id + "\"," 
  otlp_trace_data = otlp_trace_data + "\"span_id\":\"" + span_id + "\"," 
  otlp_trace_data = otlp_trace_data + "\"name\":\"" + span_name + "\"," 
  otlp_trace_data = otlp_trace_data + "\"kind\":\"" + span_kind + "\"," 
  otlp_trace_data = otlp_trace_data + "\"start_time_unix_nano\":\"" + start_time.to_string() + "\"," 
  otlp_trace_data = otlp_trace_data + "\"end_time_unix_nano\":\"" + end_time.to_string() + "\""
  otlp_trace_data = otlp_trace_data + "}"
  
  // 验证OTLP格式
  assert_eq(otlp_trace_data.has_prefix("{"), true)
  assert_eq(otlp_trace_data.has_suffix("}"), true)
  assert_eq(otlp_trace_data.contains("\"trace_id\":\"" + trace_id + "\""), true)
  assert_eq(otlp_trace_data.contains("\"span_id\":\"" + span_id + "\""), true)
  assert_eq(otlp_trace_data.contains("\"name\":\"" + span_name + "\""), true)
  assert_eq(otlp_trace_data.contains("\"kind\":\"" + span_kind + "\""), true)
  assert_eq(otlp_trace_data.contains("\"start_time_unix_nano\":\"" + start_time.to_string() + "\""), true)
  assert_eq(otlp_trace_data.contains("\"end_time_unix_nano\":\"" + end_time.to_string() + "\""), true)
  
  // 创建OTLP格式的指标数据
  let metric_name = "http_requests_total"
  let metric_value = 1234.0
  let metric_unit = "count"
  
  let otlp_metric_data = "{"
  otlp_metric_data = otlp_metric_data + "\"name\":\"" + metric_name + "\"," 
  otlp_metric_data = otlp_metric_data + "\"unit\":\"" + metric_unit + "\"," 
  otlp_metric_data = otlp_metric_data + "\"sum\":{\"value\":" + metric_value.to_string() + "}"
  otlp_metric_data = otlp_metric_data + "}"
  
  // 验证OTLP指标格式
  assert_eq(otlp_metric_data.contains("\"name\":\"" + metric_name + "\""), true)
  assert_eq(otlp_metric_data.contains("\"unit\":\"" + metric_unit + "\""), true)
  assert_eq(otlp_metric_data.contains("\"sum\":{\"value\":" + metric_value.to_string() + "}"), true)
}

test "prometheus_export_format_test" {
  // 测试Prometheus导出格式
  
  let metric_name = "http_request_duration_seconds"
  let metric_value = 0.123
  let metric_labels = "method=\"GET\",endpoint=\"/api/users\""
  let metric_timestamp = 1640995200L
  
  // 创建Prometheus格式的指标数据
  let prometheus_metric = metric_name + "{" + metric_labels + "} " + metric_value.to_string() + " " + metric_timestamp.to_string()
  
  // 验证Prometheus格式
  assert_eq(prometheus_metric.has_prefix(metric_name + "{"), true)
  assert_eq(prometheus_metric.contains(metric_labels), true)
  assert_eq(prometheus_metric.contains(metric_value.to_string()), true)
  assert_eq(prometheus_metric.has_suffix(" " + metric_timestamp.to_string()), true)
  
  // 创建Prometheus格式的直方图数据
  let histogram_name = "http_request_duration_seconds_bucket"
  let buckets = [
    ("le=\"0.1\"", 10),
    ("le=\"0.5\"", 25),
    ("le=\"1.0\"", 40),
    ("le=\"+Inf\"", 50)
  ]
  
  let mut prometheus_histogram = ""
  for bucket in buckets {
    prometheus_histogram = prometheus_histogram + histogram_name + "{" + bucket.0 + ",method=\"GET\"} " + bucket.1.to_string() + " " + metric_timestamp.to_string() + "\n"
  }
  
  // 验证Prometheus直方图格式
  assert_eq(prometheus_histogram.contains(histogram_name + "{le=\"0.1\",method=\"GET\"} 10"), true)
  assert_eq(prometheus_histogram.contains(histogram_name + "{le=\"+Inf\",method=\"GET\"} 50"), true)
  assert_eq(prometheus_histogram.contains("\n"), true)
  
  // 创建计数器总和
  let counter_sum = histogram_name.replace("_bucket", "_sum") + "{method=\"GET\"} 6.15 " + metric_timestamp.to_string()
  let counter_count = histogram_name.replace("_bucket", "_count") + "{method=\"GET\"} 50 " + metric_timestamp.to_string()
  
  // 验证计数器格式
  assert_eq(counter_sum.contains("http_request_duration_seconds_sum"), true)
  assert_eq(counter_count.contains("http_request_duration_seconds_count"), true)
}

test "jaeger_export_format_test" {
  // 测试Jaeger导出格式
  
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "11112222"
  let parent_span_id = "00000000"
  let operation_name = "GET /api/users"
  let service_name = "user-service"
  let start_time = 1640995200000L // 微秒时间戳
  let duration = 150000 // 微秒
  
  // 创建Jaeger格式的span数据
  let jaeger_span = "{"
  jaeger_span = jaeger_span + "\"traceID\":\"" + trace_id + "\"," 
  jaeger_span = jaeger_span + "\"spanID\":\"" + span_id + "\"," 
  jaeger_span = jaeger_span + "\"parentSpanID\":\"" + parent_span_id + "\"," 
  jaeger_span = jaeger_span + "\"operationName\":\"" + operation_name + "\"," 
  jaeger_span = jaeger_span + "\"startTime\":" + start_time.to_string() + "," 
  jaeger_span = jaeger_span + "\"duration\":" + duration.to_string() + "," 
  jaeger_span = jaeger_span + "\"tags\":[{\"key\":\"service.name\",\"value\":\"" + service_name + "\"}]"
  jaeger_span = jaeger_span + "}"
  
  // 验证Jaeger格式
  assert_eq(jaeger_span.contains("\"traceID\":\"" + trace_id + "\""), true)
  assert_eq(jaeger_span.contains("\"spanID\":\"" + span_id + "\""), true)
  assert_eq(jaeger_span.contains("\"operationName\":\"" + operation_name + "\""), true)
  assert_eq(jaeger_span.contains("\"startTime\":" + start_time.to_string()), true)
  assert_eq(jaeger_span.contains("\"duration\":" + duration.to_string()), true)
  assert_eq(jaeger_span.contains("\"service.name\",\"value\":\"" + service_name + "\""), true)
  
  // 创建Jaeger批处理格式
  let jaeger_batch = "{"
  jaeger_batch = jaeger_batch + "\"process\":{\"serviceName\":\"" + service_name + "\",\"tags\":[]}," 
  jaeger_batch = jaeger_batch + "\"spans\":[" + jaeger_span + "]"
  jaeger_batch = jaeger_batch + "}"
  
  // 验证Jaeger批处理格式
  assert_eq(jaeger_batch.contains("\"process\":{\"serviceName\":\"" + service_name + "\""), true)
  assert_eq(jaeger_batch.contains("\"spans\":["), true)
  assert_eq(jaeger_batch.contains("]"), true)
}

test "zipkin_export_format_test" {
  // 测试Zipkin导出格式
  
  let trace_id = "1234567890abcdef"
  let span_id = "11112222"
  let parent_id = "00000000"
  let name = "get /api/users"
  let timestamp = 1640995200000L // 微秒时间戳
  let duration = 150000 // 微秒
  
  // 创建Zipkin格式的span数据
  let zipkin_span = "{"
  zipkin_span = zipkin_span + "\"traceId\":\"" + trace_id + "\"," 
  zipkin_span = zipkin_span + "\"id\":\"" + span_id + "\"," 
  zipkin_span = zipkin_span + "\"parentId\":\"" + parent_id + "\"," 
  zipkin_span = zipkin_span + "\"name\":\"" + name + "\"," 
  zipkin_span = zipkin_span + "\"timestamp\":" + timestamp.to_string() + "," 
  zipkin_span = zipkin_span + "\"duration\":" + duration.to_string() + "," 
  zipkin_span = zipkin_span + "\"localEndpoint\":{\"serviceName\":\"user-service\"}"
  zipkin_span = zipkin_span + "}"
  
  // 验证Zipkin格式
  assert_eq(zipkin_span.contains("\"traceId\":\"" + trace_id + "\""), true)
  assert_eq(zipkin_span.contains("\"id\":\"" + span_id + "\""), true)
  assert_eq(zipkin_span.contains("\"name\":\"" + name + "\""), true)
  assert_eq(zipkin_span.contains("\"timestamp\":" + timestamp.to_string()), true)
  assert_eq(zipkin_span.contains("\"duration\":" + duration.to_string()), true)
  assert_eq(zipkin_span.contains("\"serviceName\":\"user-service\""), true)
  
  // 创建Zipkin批处理格式
  let zipkin_batch = "[" + zipkin_span + "]"
  
  // 验证Zipkin批处理格式
  assert_eq(zipkin_batch.has_prefix("["), true)
  assert_eq(zipkin_batch.has_suffix("]"), true)
  assert_eq(zipkin_batch.contains(zipkin_span), true)
}

test "elasticsearch_export_format_test" {
  // 测试Elasticsearch导出格式
  
  let index_name = "telemetry-2025.12.30"
  let doc_id = "trace_1234567890"
  let timestamp = "2025-12-30T10:00:00.000Z"
  
  // 创建Elasticsearch格式的文档
  let es_document = "{"
  es_document = es_document + "\"_index\":\"" + index_name + "\"," 
  es_document = es_document + "\"_id\":\"" + doc_id + "\"," 
  es_document = es_document + "\"_source\":{" 
  es_document = es_document + "\"@timestamp\":\"" + timestamp + "\"," 
  es_document = es_document + "\"trace_id\":\"1234567890abcdef1234567890abcdef\"," 
  es_document = es_document + "\"span_id\":\"11112222\"," 
  es_document = es_document + "\"service_name\":\"user-service\"," 
  es_document = es_document + "\"operation_name\":\"GET /api/users\"," 
  es_document = es_document + "\"duration_ms\":150," 
  es_document = es_document + "\"status\":\"success\""
  es_document = es_document + "}"
  es_document = es_document + "}"
  
  // 验证Elasticsearch格式
  assert_eq(es_document.contains("\"_index\":\"" + index_name + "\""), true)
  assert_eq(es_document.contains("\"_id\":\"" + doc_id + "\""), true)
  assert_eq(es_document.contains("\"@timestamp\":\"" + timestamp + "\""), true)
  assert_eq(es_document.contains("\"trace_id\":\"1234567890abcdef1234567890abcdef\""), true)
  assert_eq(es_document.contains("\"duration_ms\":150"), true)
  assert_eq(es_document.contains("\"status\":\"success\""), true)
  
  // 创建批量操作格式
  let bulk_operation = "{ \"index\" : { \"_index\" : \"" + index_name + "\" } }\n"
  bulk_operation = bulk_operation + "{ \"@timestamp\" : \"" + timestamp + "\", \"service_name\" : \"user-service\" }\n"
  bulk_operation = bulk_operation + "{ \"index\" : { \"_index\" : \"" + index_name + "\" } }\n"
  bulk_operation = bulk_operation + "{ \"@timestamp\" : \"" + timestamp + "\", \"service_name\" : \"order-service\" }\n"
  
  // 验证批量操作格式
  assert_eq(bulk_operation.contains("{ \"index\" : { \"_index\" : \"" + index_name + "\" } }"), true)
  assert_eq(bulk_operation.contains("\"service_name\" : \"user-service\""), true)
  assert_eq(bulk_operation.contains("\"service_name\" : \"order-service\""), true)
  assert_eq(bulk_operation.contains("\n"), true)
}

test "influxdb_export_format_test" {
  // 测试InfluxDB导出格式
  
  let measurement = "http_requests"
  let tags = "method=GET,endpoint=/api/users,service=user-service"
  let fields = "duration=150i,status_code=200"
  let timestamp = 1640995200000000000L // 纳秒时间戳
  
  // 创建InfluxDB line protocol格式
  let influxdb_line = measurement + "," + tags + " " + fields + " " + timestamp.to_string()
  
  // 验证InfluxDB line protocol格式
  assert_eq(influxdb_line.has_prefix(measurement + ","), true)
  assert_eq(influxdb_line.contains(tags), true)
  assert_eq(influxdb_line.contains(fields), true)
  assert_eq(influxdb_line.has_suffix(" " + timestamp.to_string()), true)
  
  // 创建多个数据点
  let influxdb_batch = ""
  influxdb_batch = influxdb_batch + measurement + ",method=GET,endpoint=/api/users duration=150i,status_code=200 " + timestamp.to_string() + "\n"
  influxdb_batch = influxdb_batch + measurement + ",method=POST,endpoint=/api/orders duration=300i,status_code=201 " + (timestamp + 1000000000L).to_string() + "\n"
  influxdb_batch = influxdb_batch + measurement + ",method=GET,endpoint=/api/products duration=100i,status_code=200 " + (timestamp + 2000000000L).to_string()
  
  // 验证InfluxDB批处理格式
  assert_eq(influxdb_batch.contains("method=GET,endpoint=/api/users"), true)
  assert_eq(influxdb_batch.contains("method=POST,endpoint=/api/orders"), true)
  assert_eq(influxdb_batch.contains("method=GET,endpoint=/api/products"), true)
  assert_eq(influxdb_batch.contains("duration=150i"), true)
  assert_eq(influxdb_batch.contains("duration=300i"), true)
  assert_eq(influxdb_batch.contains("duration=100i"), true)
  assert_eq(influxdb_batch.contains("\n"), true)
}

test "csv_export_format_test" {
  // 测试CSV导出格式
  
  // 创建CSV头部
  let csv_header = "timestamp,trace_id,span_id,service_name,operation_name,duration_ms,status"
  
  // 创建CSV数据行
  let csv_data = []
  csv_data.push("2025-12-30T10:00:00Z,1234567890abcdef1234567890abcdef,11112222,user-service,GET /api/users,150,success")
  csv_data.push("2025-12-30T10:00:01Z,1234567890abcdef1234567890abcdef,33334444,user-service,POST /api/orders,300,success")
  csv_data.push("2025-12-30T10:00:02Z,1234567890abcdef1234567890abcdef,55556666,user-service,GET /api/products,100,error")
  
  // 验证CSV头部
  assert_eq(csv_header.contains("timestamp"), true)
  assert_eq(csv_header.contains("trace_id"), true)
  assert_eq(csv_header.contains("duration_ms"), true)
  assert_eq(csv_header.contains(","), true)
  
  // 验证CSV数据
  assert_eq(csv_data.length(), 3)
  for row in csv_data {
    assert_eq(row.contains(","), true)
    assert_eq(row.split(",").length(), 7) // 7列
  }
  
  // 验证特定数据行
  assert_eq(csv_data[0].contains("user-service"), true)
  assert_eq(csv_data[0].contains("150"), true)
  assert_eq(csv_data[0].contains("success"), true)
  
  assert_eq(csv_data[2].contains("error"), true)
  assert_eq(csv_data[2].contains("100"), true)
  
  // 创建完整CSV内容
  let mut csv_content = csv_header + "\n"
  for row in csv_data {
    csv_content = csv_content + row + "\n"
  }
  
  // 验证CSV内容
  assert_eq(csv_content.has_prefix(csv_header), true)
  assert_eq(csv_content.contains("\n"), true)
  assert_eq(csv_content.split("\n").length(), 5) // 头部 + 3行数据 + 空行
}

test "json_export_format_test" {
  // 测试JSON导出格式
  
  // 创建JSON数组格式的遥测数据
  let json_array = "["
  json_array = json_array + "{"
  json_array = json_array + "\"timestamp\":\"2025-12-30T10:00:00Z\"," 
  json_array = json_array + "\"trace_id\":\"1234567890abcdef1234567890abcdef\"," 
  json_array = json_array + "\"span_id\":\"11112222\"," 
  json_array = json_array + "\"service_name\":\"user-service\"," 
  json_array = json_array + "\"operation_name\":\"GET /api/users\"," 
  json_array = json_array + "\"duration_ms\":150," 
  json_array = json_array + "\"attributes\":{\"http.method\":\"GET\",\"http.status_code\":200}"
  json_array = json_array + "},"
  json_array = json_array + "{"
  json_array = json_array + "\"timestamp\":\"2025-12-30T10:00:01Z\"," 
  json_array = json_array + "\"trace_id\":\"1234567890abcdef1234567890abcdef\"," 
  json_array = json_array + "\"span_id\":\"33334444\"," 
  json_array = json_array + "\"service_name\":\"order-service\"," 
  json_array = json_array + "\"operation_name\":\"POST /api/orders\"," 
  json_array = json_array + "\"duration_ms\":300," 
  json_array = json_array + "\"attributes\":{\"http.method\":\"POST\",\"http.status_code\":201}"
  json_array = json_array + "}"
  json_array = json_array + "]"
  
  // 验证JSON数组格式
  assert_eq(json_array.has_prefix("["), true)
  assert_eq(json_array.has_suffix("]"), true)
  assert_eq(json_array.contains("\"service_name\":\"user-service\""), true)
  assert_eq(json_array.contains("\"service_name\":\"order-service\""), true)
  assert_eq(json_array.contains("\"duration_ms\":150"), true)
  assert_eq(json_array.contains("\"duration_ms\":300"), true)
  assert_eq(json_array.contains("\"attributes\":"), true)
  
  // 创建JSON行格式（每行一个JSON对象）
  let json_lines = ""
  json_lines = json_lines + "{\"timestamp\":\"2025-12-30T10:00:00Z\",\"service\":\"user-service\",\"metric\":\"request_count\",\"value\":42}\n"
  json_lines = json_lines + "{\"timestamp\":\"2025-12-30T10:00:01Z\",\"service\":\"order-service\",\"metric\":\"request_count\",\"value\":38}\n"
  json_lines = json_lines + "{\"timestamp\":\"2025-12-30T10:00:02Z\",\"service\":\"payment-service\",\"metric\":\"request_count\",\"value\":25}"
  
  // 验证JSON行格式
  assert_eq(json_lines.contains("{\"timestamp\""), true)
  assert_eq(json_lines.contains("\"service\":\"user-service\""), true)
  assert_eq(json_lines.contains("\"metric\":\"request_count\""), true)
  assert_eq(json_lines.contains("\"value\":42"), true)
  assert_eq(json_lines.contains("\n"), true)
  assert_eq(json_lines.split("\n").length(), 3)
}
// Azimuth 遥测数据序列化增强测试用例
// 专注于遥测数据的序列化、反序列化和格式转换功能

// 测试1: JSON序列化基础功能
test "JSON序列化基础功能测试" {
  // 创建测试span
  let span = Span::new("test.operation", Server, TraceContext::new("trace-123", "span-456", true, ""))
  Span::set_attribute(span, "service.name", StringValue("test.service"))
  Span::set_attribute(span, "http.method", StringValue("GET"))
  Span::set_attribute(span, "http.status_code", IntValue(200))
  Span::set_attribute(span, "duration", FloatValue(123.45))
  Span::set_attribute(span, "success", BoolValue(true))
  
  // 序列化为JSON
  let json_data = JsonSerializer::serialize_span(span)
  
  // 验证JSON内容
  assert_true(json_data.contains("\"name\":\"test.operation\""))
  assert_true(json_data.contains("\"kind\":\"Server\""))
  assert_true(json_data.contains("\"trace_id\":\"trace-123\""))
  assert_true(json_data.contains("\"span_id\":\"span-456\""))
  assert_true(json_data.contains("\"service.name\":\"test.service\""))
  assert_true(json_data.contains("\"http.method\":\"GET\""))
  assert_true(json_data.contains("\"http.status_code\":200"))
  assert_true(json_data.contains("\"duration\":123.45"))
  assert_true(json_data.contains("\"success\":true"))
  
  // 反序列化JSON
  let deserialized_span = JsonSerializer::deserialize_span(json_data)
  
  // 验证反序列化结果
  assert_eq(deserialized_span.name, span.name)
  assert_eq(deserialized_span.kind, span.kind)
  assert_eq(deserialized_span.context.trace_id, span.context.trace_id)
  assert_eq(deserialized_span.context.span_id, span.context.span_id)
  
  let deserialized_service = Span::get_attribute(deserialized_span, "service.name")
  assert_eq(deserialized_service, Some(StringValue("test.service")))
  
  let deserialized_status = Span::get_attribute(deserialized_span, "http.status_code")
  assert_eq(deserialized_status, Some(IntValue(200)))
}

// 测试2: 批量数据序列化
test "批量数据序列化测试" {
  // 创建多个span
  let spans = []
  for i in 0..=10 {
    let span = Span::new("operation-" + i.to_string(), Server, 
                        TraceContext::new("trace-123", "span-" + i.to_string(), true, ""))
    Span::set_attribute(span, "service.name", StringValue("service-" + (i % 3).to_string()))
    Span::set_attribute(span, "iteration", IntValue(i))
    spans = spans.push(span)
  }
  
  // 批量序列化
  let batch_json = JsonSerializer::serialize_spans(spans)
  
  // 验证批量序列化结果
  assert_true(batch_json.contains("\"spans\":["))
  
  // 计算JSON中的span数量
  let span_count = batch_json.split("\"name\"").length() - 1
  assert_eq(span_count, 11)  // 11个span
  
  // 批量反序列化
  let deserialized_spans = JsonSerializer::deserialize_spans(batch_json)
  assert_eq(deserialized_spans.length(), spans.length())
  
  // 验证每个span
  for i in 0..=10 {
    assert_eq(deserialized_spans[i].name, spans[i].name)
    assert_eq(deserialized_spans[i].context.span_id, spans[i].context.span_id)
  }
}

// 测试3: 度量数据序列化
test "度量数据序列化测试" {
  // 创建各种类型的度量
  let counter = Metric::counter("requests.total", "总请求数", [])
  Metric::add(counter, 100.0, [("service", StringValue("api"))])
  
  let gauge = Metric::gauge("memory.usage", "内存使用量", [])
  Metric::set(gauge, 512.0, [("service", StringValue("api"))])
  
  let histogram = Metric::histogram("request.duration", "请求延迟", [10.0, 50.0, 100.0, 500.0])
  Metric::observe(histogram, 25.0, [("service", StringValue("api"))])
  Metric::observe(histogram, 75.0, [("service", StringValue("api"))])
  Metric::observe(histogram, 150.0, [("service", StringValue("api"))])
  
  let metrics = [counter, gauge, histogram]
  
  // 序列化度量数据
  let metrics_json = JsonSerializer::serialize_metrics(metrics)
  
  // 验证度量JSON
  assert_true(metrics_json.contains("\"name\":\"requests.total\""))
  assert_true(metrics_json.contains("\"type\":\"counter\""))
  assert_true(metrics_json.contains("\"value\":100.0"))
  
  assert_true(metrics_json.contains("\"name\":\"memory.usage\""))
  assert_true(metrics_json.contains("\"type\":\"gauge\""))
  assert_true(metrics_json.contains("\"value\":512.0"))
  
  assert_true(metrics_json.contains("\"name\":\"request.duration\""))
  assert_true(metrics_json.contains("\"type\":\"histogram\""))
  assert_true(metrics_json.contains("\"buckets\":["))
  
  // 反序列化度量数据
  let deserialized_metrics = JsonSerializer::deserialize_metrics(metrics_json)
  assert_eq(deserialized_metrics.length(), 3)
  
  // 验证反序列化的度量
  let deserialized_counter = deserialized_metrics.find(fn(m) { m.name == "requests.total" })
  assert_true(deserialized_counter != None)
  
  match deserialized_counter {
    Some(metric) => {
      assert_eq(metric.metric_type, Counter)
      assert_eq(metric.value, 100.0)
    }
    None => assert_true(false)
  }
}

// 测试4: 日志数据序列化
test "日志数据序列化测试" {
  // 创建日志记录器
  let logger = Logger::new("test.logger")
  
  // 添加不同级别的日志
  Logger::info(logger, "服务启动", [
    ("service.name", StringValue("api.service")),
    ("version", StringValue("1.0.0")),
    ("port", IntValue(8080))
  ])
  
  Logger::warn(logger, "连接池接近满载", [
    ("service.name", StringValue("api.service")),
    ("pool.usage", FloatValue(0.85)),
    ("pool.size", IntValue(100))
  ])
  
  Logger::error(logger, "数据库连接失败", [
    ("service.name", StringValue("api.service")),
    ("error.code", StringValue("DB_CONN_ERROR")),
    ("retry.count", IntValue(3))
  ])
  
  // 获取日志记录
  let logs = Logger::get_logs(logger)
  
  // 序列化日志数据
  let logs_json = JsonSerializer::serialize_logs(logs)
  
  // 验证日志JSON
  assert_true(logs_json.contains("\"logger_name\":\"test.logger\""))
  assert_true(logs_json.contains("\"message\":\"服务启动\""))
  assert_true(logs_json.contains("\"level\":\"Info\""))
  assert_true(logs_json.contains("\"service.name\":\"api.service\""))
  assert_true(logs_json.contains("\"version\":\"1.0.0\""))
  assert_true(logs_json.contains("\"port\":8080"))
  
  assert_true(logs_json.contains("\"message\":\"连接池接近满载\""))
  assert_true(logs_json.contains("\"level\":\"Warn\""))
  assert_true(logs_json.contains("\"pool.usage\":0.85"))
  
  assert_true(logs_json.contains("\"message\":\"数据库连接失败\""))
  assert_true(logs_json.contains("\"level\":\"Error\""))
  assert_true(logs_json.contains("\"error.code\":\"DB_CONN_ERROR\""))
  
  // 反序列化日志数据
  let deserialized_logs = JsonSerializer::deserialize_logs(logs_json)
  assert_eq(deserialized_logs.length(), logs.length())
  
  // 验证反序列化的日志
  let error_log = deserialized_logs.find(fn(l) { l.level == Error })
  assert_true(error_log != None)
  
  match error_log {
    Some(log) => {
      assert_eq(log.message, "数据库连接失败")
      assert_eq(log.logger_name, "test.logger")
      assert_true(log.attributes.contains(("error.code", StringValue("DB_CONN_ERROR"))))
    }
    None => assert_true(false)
  }
}

// 测试5: 资源数据序列化
test "资源数据序列化测试" {
  // 创建资源
  let resource = Resource::new([
    ("service.name", StringValue("payment.service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("inst-abc123")),
    ("host.name", StringValue("prod-web-01")),
    ("host.ip", StringValue("10.0.1.100")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("20.04")),
    ("process.pid", IntValue(1234)),
    ("process.executable.name", StringValue("payment-service")),
    ("process.command_args", ["--config", "/etc/payment/config.yaml"]),
    ("cloud.provider", StringValue("aws")),
    ("cloud.region", StringValue("us-west-2")),
    ("cloud.availability_zone", StringValue("us-west-2a"))
  ])
  
  // 序列化资源数据
  let resource_json = JsonSerializer::serialize_resource(resource)
  
  // 验证资源JSON
  assert_true(resource_json.contains("\"service.name\":\"payment.service\""))
  assert_true(resource_json.contains("\"service.version\":\"2.1.0\""))
  assert_true(resource_json.contains("\"service.instance.id\":\"inst-abc123\""))
  assert_true(resource_json.contains("\"host.name\":\"prod-web-01\""))
  assert_true(resource_json.contains("\"host.ip\":\"10.0.1.100\""))
  assert_true(resource_json.contains("\"os.type\":\"linux\""))
  assert_true(resource_json.contains("\"process.pid\":1234"))
  assert_true(resource_json.contains("\"cloud.provider\":\"aws\""))
  assert_true(resource_json.contains("\"cloud.region\":\"us-west-2\""))
  
  // 反序列化资源数据
  let deserialized_resource = JsonSerializer::deserialize_resource(resource_json)
  
  // 验证反序列化的资源
  let service_name = Resource::get_attribute(deserialized_resource, "service.name")
  assert_eq(service_name, Some(StringValue("payment.service")))
  
  let cloud_region = Resource::get_attribute(deserialized_resource, "cloud.region")
  assert_eq(cloud_region, Some(StringValue("us-west-2")))
  
  let process_pid = Resource::get_attribute(deserialized_resource, "process.pid")
  assert_eq(process_pid, Some(IntValue(1234)))
}

// 测试6: 二进制序列化
test "二进制序列化测试" {
  // 创建测试数据
  let span = Span::new("binary.test", Server, TraceContext::new("trace-bin", "span-bin", true, ""))
  Span::set_attribute(span, "binary.data", StringValue("binary test data"))
  Span::set_attribute(span, "numeric.value", IntValue(42))
  
  // 二进制序列化
  let binary_data = BinarySerializer::serialize_span(span)
  assert_true(binary_data.length() > 0)
  
  // 二进制反序列化
  let deserialized_span = BinarySerializer::deserialize_span(binary_data)
  
  // 验证反序列化结果
  assert_eq(deserialized_span.name, span.name)
  assert_eq(deserialized_span.context.trace_id, span.context.trace_id)
  
  let binary_attr = Span::get_attribute(deserialized_span, "binary.data")
  assert_eq(binary_attr, Some(StringValue("binary test data")))
  
  // 测试二进制序列化性能
  let start_time = Time::now()
  for i in 0..=100 {
    let test_span = Span::new("perf-test-" + i.to_string(), Server, 
                             TraceContext::new("trace-" + i.to_string(), "span-" + i.to_string(), true, ""))
    let _binary = BinarySerializer::serialize_span(test_span)
  }
  let end_time = Time::now()
  let duration = end_time - start_time
  
  // 验证性能（应该在合理时间内完成）
  assert_true(duration < 5000)  // 5秒内完成100个span的序列化
}

// 测试7: 压缩序列化
test "压缩序列化测试" {
  // 创建大量测试数据
  let spans = []
  for i in 0..=1000 {
    let span = Span::new("large-data-" + i.to_string(), Server, 
                        TraceContext::new("trace-large", "span-" + i.to_string(), true, ""))
    Span::set_attribute(span, "service.name", StringValue("large.data.service"))
    Span::set_attribute(span, "payload", StringValue("large payload data " + i.to_string()))
    Span::set_attribute(span, "metadata", StringValue("metadata with lots of repeated text to test compression efficiency"))
    spans = spans.push(span)
  }
  
  // 普通序列化
  let normal_json = JsonSerializer::serialize_spans(spans)
  let normal_size = normal_json.length()
  
  // 压缩序列化
  let compressed_data = CompressedSerializer::serialize_spans(spans, "gzip")
  let compressed_size = compressed_data.length()
  
  // 验证压缩效果
  assert_true(compressed_size < normal_size)
  let compression_ratio = 1.0 - (compressed_size.to_float() / normal_size.to_float())
  assert_true(compression_ratio > 0.5)  // 至少50%压缩率
  
  // 解压缩并反序列化
  let decompressed_json = CompressedSerializer::decompress(compressed_data, "gzip")
  let decompressed_spans = JsonSerializer::deserialize_spans(decompressed_json)
  
  // 验证解压缩结果
  assert_eq(decompressed_spans.length(), spans.length())
  assert_eq(decompressed_json.length(), normal_json.length())
  
  // 验证数据完整性
  for i in 0..=1000 {
    assert_eq(decompressed_spans[i].name, spans[i].name)
    assert_eq(decompressed_spans[i].context.span_id, spans[i].context.span_id)
  }
}

// 测试8: 跨格式序列化转换
test "跨格式序列化转换测试" {
  // 创建测试数据
  let span = Span::new("format.conversion", Client, TraceContext::new("trace-fmt", "span-fmt", true, ""))
  Span::set_attribute(span, "format.test", StringValue("format conversion test"))
  Span::set_attribute(span, "numeric.value", FloatValue(3.14159))
  
  // JSON序列化
  let json_data = JsonSerializer::serialize_span(span)
  
  // XML序列化
  let xml_data = XmlSerializer::serialize_span(span)
  
  // 验证XML格式
  assert_true(xml_data.contains("<span>"))
  assert_true(xml_data.contains("</span>"))
  assert_true(xml_data.contains("<name>format.conversion</name>"))
  assert_true(xml_data.contains("<kind>Client</kind>"))
  assert_true(xml_data.contains("<format.test>format conversion test</format.test>"))
  assert_true(xml_data.contains("<numeric.value>3.14159</numeric.value>"))
  
  // YAML序列化
  let yaml_data = YamlSerializer::serialize_span(span)
  
  // 验证YAML格式
  assert_true(yaml_data.contains("name: format.conversion"))
  assert_true(yaml_data.contains("kind: Client"))
  assert_true(yaml_data.contains("format.test: format conversion test"))
  assert_true(yaml_data.contains("numeric.value: 3.14159"))
  
  // 跨格式转换：JSON -> XML
  let json_to_xml = FormatConverter::convert(json_data, "json", "xml")
  assert_true(json_to_xml.contains("<span>"))
  assert_true(json_to_xml.contains("<name>format.conversion</name>"))
  
  // 跨格式转换：XML -> YAML
  let xml_to_yaml = FormatConverter::convert(xml_data, "xml", "yaml")
  assert_true(xml_to_yaml.contains("name: format.conversion"))
  assert_true(xml_to_yaml.contains("kind: Client"))
  
  // 跨格式转换：YAML -> JSON
  let yaml_to_json = FormatConverter::convert(yaml_data, "yaml", "json")
  assert_true(yaml_to_json.contains("\"name\":\"format.conversion\""))
  assert_true(yaml_to_json.contains("\"kind\":\"Client\""))
  
  // 验证转换后的数据一致性
  let json_span = JsonSerializer::deserialize_span(yaml_to_json)
  assert_eq(json_span.name, span.name)
  assert_eq(json_span.kind, span.kind)
  
  let format_attr = Span::get_attribute(json_span, "format.test")
  assert_eq(format_attr, Some(StringValue("format conversion test")))
}

// 测试9: 序列化错误处理
test "序列化错误处理测试" {
  // 测试无效JSON反序列化
  let invalid_json = "{ invalid json structure"
  let result = JsonSerializer::deserialize_span(invalid_json)
  assert_eq(result.name, "")  // 返回默认空span
  
  // 测试空数据反序列化
  let empty_json = ""
  let empty_result = JsonSerializer::deserialize_span(empty_json)
  assert_eq(empty_result.name, "")
  
  // 测试部分缺失字段的JSON
  let partial_json = "{\"name\":\"test\"}"  // 缺少必要字段
  let partial_result = JsonSerializer::deserialize_span(partial_json)
  assert_eq(partial_result.name, "test")
  assert_eq(partial_result.context.trace_id, "")  // 默认空值
  
  // 测试循环引用处理
  let circular_ref = CircularRef::new()
  CircularRef::set_self_reference(circular_ref, circular_ref)
  
  // 序列化应该能处理循环引用而不崩溃
  let circular_result = JsonSerializer::serialize_circular(circular_ref)
  assert_true(circular_result.contains("\"circular\":true") or 
             circular_result.contains("\"ref\":\"self\""))
  
  // 测试过大数据序列化
  let large_data = String::repeat("x", 10000000)  // 10MB数据
  let large_span = Span::new("large.data", Server, TraceContext::new("trace", "span", true, ""))
  Span::set_attribute(large_span, "large.data", StringValue(large_data))
  
  // 应该能处理大数据或返回适当错误
  let large_result = JsonSerializer::serialize_span(large_span)
  assert_true(large_result.length() > 0 or large_result.contains("\"error\":\"size_limit_exceeded\""))
}

// 测试10: 序列化性能基准
test "序列化性能基准测试" {
  // 创建性能测试数据集
  let test_spans = []
  for i in 0..=10000 {
    let span = Span::new("perf.test-" + i.to_string(), Server, 
                        TraceContext::new("trace-perf", "span-" + i.to_string(), true, ""))
    Span::set_attribute(span, "service.name", StringValue("performance.test.service"))
    Span::set_attribute(span, "iteration", IntValue(i))
    Span::set_attribute(span, "timestamp", IntValue(Time::now()))
    Span::set_attribute(span, "payload", StringValue("performance test payload " + i.to_string()))
    test_spans = test_spans.push(span)
  }
  
  // JSON序列化性能测试
  let json_start = Time::now()
  let json_result = JsonSerializer::serialize_spans(test_spans)
  let json_end = Time::now()
  let json_duration = json_end - json_start
  let json_size = json_result.length()
  
  // 二进制序列化性能测试
  let binary_start = Time::now()
  let binary_result = BinarySerializer::serialize_spans(test_spans)
  let binary_end = Time::now()
  let binary_duration = binary_end - binary_start
  let binary_size = binary_result.length()
  
  // 压缩序列化性能测试
  let compressed_start = Time::now()
  let compressed_result = CompressedSerializer::serialize_spans(test_spans, "gzip")
  let compressed_end = Time::now()
  let compressed_duration = compressed_end - compressed_start
  let compressed_size = compressed_result.length()
  
  // 验证性能基准
  assert_true(json_duration < 10000)  // JSON序列化应在10秒内完成
  assert_true(binary_duration < 5000)  // 二进制序列化应在5秒内完成
  assert_true(compressed_duration < 15000)  // 压缩序列化应在15秒内完成
  
  // 验证大小比较
  assert_true(binary_size < json_size)  // 二进制应该更小
  assert_true(compressed_size < json_size)  // 压缩应该更小
  
  // 计算性能指标
  let json_throughput = test_spans.length().to_float() / json_duration.to_float() * 1000.0  // spans/second
  let binary_throughput = test_spans.length().to_float() / binary_duration.to_float() * 1000.0
  let compressed_throughput = test_spans.length().to_float() / compressed_duration.to_float() * 1000.0
  
  // 验证吞吐量
  assert_true(json_throughput > 100)  // 至少100 spans/second
  assert_true(binary_throughput > 200)  // 二进制应该更快
  assert_true(compressed_throughput > 50)  // 压缩可能较慢但仍应有合理性能
  
  // 计算压缩比
  let compression_ratio = 1.0 - (compressed_size.to_float() / json_size.to_float())
  assert_true(compression_ratio > 0.3)  // 至少30%压缩率
}
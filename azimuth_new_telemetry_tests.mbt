// New MoonBit Test Cases for Azimuth Telemetry System
// This file contains 10 comprehensive test cases for telemetry functionality

// Test 1: Resource Management and Attribute Operations
test "资源管理和属性操作测试" {
  let resource = Resource::new()
  let attrs = Resource::with_attributes(resource, [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ])
  
  let service_name = Resource::get_attribute(attrs, "service.name")
  let service_version = Resource::get_attribute(attrs, "service.version")
  let missing_attr = Resource::get_attribute(attrs, "missing.attribute")
  
  match (service_name, service_version, missing_attr) {
    (Some(StringValue(name)), Some(StringValue(version)), None) => name + version
    _ => "default"
  }
}

// Test 2: Span Lifecycle Management
test "Span生命周期管理测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle.test")
  
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "key1=value1")
  let span = Span::new("test.span", Server, span_ctx)
  
  assert_eq(Span::name(span), "test.span")
  assert_eq(Span::kind(span), Server)
  assert_true(Span::is_recording(span))
  
  Span::add_event(span, "operation.started", None)
  Span::set_status(span, Ok)
  Span::end(span)
  
  let span_name = Span::name(span)
  span_name
}

// Test 3: Metrics Collection and Analysis
test "指标收集和分析测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "metrics.test")
  
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let response_histogram = Meter::create_histogram(meter, "http.response.duration")
  let active_connections = Meter::create_updown_counter(meter, "http.connections.active")
  let memory_usage = Meter::create_gauge(meter, "process.memory.usage")
  
  Counter::add(request_counter, 100.0)
  Histogram::record(response_histogram, 0.125)
  UpDownCounter::add(active_connections, 25.0)
  
  let counter_name = request_counter.name
  let histogram_name = response_histogram.name
  
  counter_name + histogram_name
}

// Test 4: Context Propagation Across Services
test "跨服务上下文传播测试" {
  let ctx = Context::root()
  let trace_key = ContextKey::new("trace.id")
  let user_key = ContextKey::new("user.id")
  
  let ctx_with_trace = Context::with_value(ctx, trace_key, "trace-789")
  let ctx_with_user = Context::with_value(ctx_with_trace, user_key, "user-456")
  
  let propagated_trace = Context::get(ctx_with_user, trace_key)
  let propagated_user = Context::get(ctx_with_user, user_key)
  
  match (propagated_trace, propagated_user) {
    (Some(trace), Some(user)) => trace + user
    _ => "default"
  }
}

// Test 5: Baggage Operations and Management
test "Baggage操作和管理测试" {
  let baggage = Baggage::new()
  
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let with_tenant = Baggage::set_entry(updated_baggage, "tenant.id", "tenant-abc")
  let with_region = Baggage::set_entry(with_tenant, "region", "us-west-2")
  
  let user_id = Baggage::get_entry(with_region, "user.id")
  let tenant_id = Baggage::get_entry(with_region, "tenant.id")
  let region = Baggage::get_entry(with_region, "region")
  let missing = Baggage::get_entry(with_region, "missing.key")
  
  match (user_id, tenant_id, region, missing) {
    (Some(user), Some(tenant), Some(reg), None) => user + tenant + reg
    _ => "default"
  }
}

// Test 6: Logging and Log Record Management
test "日志记录管理测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test.logger")
  
  let info_record = LogRecord::new_with_context(
    Info,
    Some("Informational log message"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace-123"),
    Some("span-456"),
    None
  )
  
  let error_record = LogRecord::new_with_context(
    Error,
    Some("Error occurred during operation"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace-123"),
    Some("span-789"),
    None
  )
  
  Logger::emit(logger, info_record)
  Logger::emit(logger, error_record)
  
  let info_severity = LogRecord::severity_number(info_record)
  let error_severity = LogRecord::severity_number(error_record)
  
  match (info_severity, error_severity) {
    (Info, Error) => "log_levels_correct"
    _ => "log_levels_incorrect"
  }
}

// Test 7: HTTP Client Operations with Telemetry
test "HTTP客户端遥测操作测试" {
  let client = HttpClient::new()
  
  let request_headers = [
    ("Content-Type", "application/json"),
    ("Accept", "application/json"),
    ("X-Request-ID", "req-123")
  ]
  
  let request = HttpRequest::new(
    "POST",
    "https://api.example.com/telemetry",
    request_headers,
    Some("{\"operation\":\"test\"}")
  )
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/telemetry")
  assert_eq(HttpRequest::body(request), Some("{\"operation\":\"test\"}"))
  
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Response-Time", "125ms")
  ]
  
  let response = HttpResponse::new(
    200,
    response_headers,
    Some("{\"status\":\"success\"}")
  )
  
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("{\"status\":\"success\"}"))
  
  let status_code = HttpResponse::status_code(response)
  status_code.to_string()
}

// Test 8: Composite Propagator Operations
test "复合传播器操作测试" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  let composite = CompositePropagator::new([trace_propagator])
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  CompositePropagator::inject(composite, ctx, carrier)
  
  TextMapCarrier::set(carrier, "baggage", "user=123,tenant=abc")
  TextMapCarrier::set(carrier, "x-custom-header", "custom-value")
  
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  let baggage = TextMapCarrier::get(carrier, "baggage")
  let custom = TextMapCarrier::get(carrier, "x-custom-header")
  let missing = TextMapCarrier::get(carrier, "missing-header")
  
  match (traceparent, baggage, custom, missing) {
    (Some(trace), Some(bag), Some(custom), None) => trace + bag + custom
    _ => "default"
  }
}

// Test 9: Time Series Operations and Timestamps
test "时间序列操作和时间戳测试" {
  let clock = Clock::system()
  let timestamp1 = Clock::now_unix_nanos(clock)
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "timeseries.test")
  
  let latency_histogram = Meter::create_histogram(meter, "operation.latency")
  let throughput_counter = Meter::create_counter(meter, "operation.throughput")
  
  Histogram::record(latency_histogram, 0.05)
  Histogram::record(latency_histogram, 0.1)
  Histogram::record(latency_histogram, 0.025)
  
  Counter::add(throughput_counter, 1.0)
  Counter::add(throughput_counter, 1.0)
  Counter::add(throughput_counter, 1.0)
  
  let timestamp2 = Clock::now_unix_nanos(clock)
  
  assert_true(timestamp2 >= timestamp1)
  
  let time_diff = timestamp2 - timestamp1
  time_diff.to_string()
}

// Test 10: Advanced Attribute Value Conversions
test "高级属性值转换测试" {
  let attrs = Attributes::new()
  
  Attributes::set(attrs, "string.value", StringValue("test.string"))
  Attributes::set(attrs, "int.value", IntValue(42))
  Attributes::set(attrs, "float.value", FloatValue(3.14159))
  Attributes::set(attrs, "bool.value", BoolValue(true))
  Attributes::set(attrs, "array.string", ArrayStringValue(["item1", "item2", "item3"]))
  Attributes::set(attrs, "array.int", ArrayIntValue([1, 2, 3, 4, 5]))
  
  let string_val = Attributes::get(attrs, "string.value")
  let int_val = Attributes::get(attrs, "int.value")
  let float_val = Attributes::get(attrs, "float.value")
  let bool_val = Attributes::get(attrs, "bool.value")
  let array_str = Attributes::get(attrs, "array.string")
  let array_int = Attributes::get(attrs, "array.int")
  let missing = Attributes::get(attrs, "missing.value")
  
  match (string_val, int_val, float_val, bool_val, array_str, array_int, missing) {
    (Some(StringValue(s)), Some(IntValue(i)), Some(FloatValue(f)), Some(BoolValue(b)), 
     Some(ArrayStringValue(arr_s)), Some(ArrayIntValue(arr_i)), None) => {
      s + i.to_string() + f.to_string() + b.to_string() + arr_s.length.to_string() + arr_i.length.to_string()
    }
    _ => "default"
  }
}
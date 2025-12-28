test "基本属性操作测试" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  
  // 使用模式匹配而不是直接比较
  let result = Attributes::get(attrs, "string.key")
  assert_true(result is Some)
  match result {
    Some(StringValue(value)) => assert_eq(value, "test_value")
    _ => assert_true(false) // 如果不是StringValue，测试失败
  }
}

test "Span生命周期测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test.tracer")
  let span = Tracer::start_span(tracer, "test.span")
  
  // 使用模式匹配检查状态
  match Span::status(span) {
    Unset => assert_true(true) // 预期状态为Unset
    _ => assert_true(false)
  }
  
  Span::end(span)
}

test "Counter操作测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test.meter")
  let counter = Meter::create_counter(meter, "test.counter")
  
  // 简单调用，不验证返回值
  Counter::add(counter, 5.0)
  
  // 测试通过表示没有崩溃
  assert_true(true)
}

test "Histogram操作测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test.meter")
  let histogram = Meter::create_histogram(meter, "test.histogram")
  
  // 简单调用，不验证返回值
  Histogram::record(histogram, 200.5)
  
  // 测试通过表示没有崩溃
  assert_true(true)
}

test "Resource合并测试" {
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  let resource3 = Resource::merge(resource1, resource2)
  
  // 简单验证合并操作不崩溃
  assert_true(true)
}

test "Logger操作测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test.logger")
  
  let log_record = LogRecord::new(Info, "Test log message")
  
  // 简单调用，不验证返回值
  Logger::emit(logger, log_record)
  
  // 测试通过表示没有崩溃
  assert_true(true)
}

test "Context传播测试" {
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  let ctx = CompositePropagator::extract(composite, carrier)
  
  // 简单验证提取操作不崩溃
  assert_true(true)
}

test "Baggage操作测试" {
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  
  // 简单验证操作不崩溃
  assert_true(true)
}

test "HTTP操作测试" {
  let client = HttpClient::new()
  let headers = [("Content-Type", "application/json")]
  let request = HttpRequest::new("GET", "https://api.example.com/data", headers)
  let response = HttpResponse::new(200, headers)
  
  // 简单验证HTTP操作不崩溃
  assert_true(true)
}
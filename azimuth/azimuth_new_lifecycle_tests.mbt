// 遥测数据生命周期管理测试
test "遥测数据生命周期管理测试" {
  // 测试数据创建阶段
  let creation_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建Span
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "lifecycle-tracer")
  let span = azimuth::Tracer::start_span(tracer, "lifecycle-operation")
  
  // 创建度量
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "lifecycle-meter")
  let counter = azimuth::Meter::create_counter(meter, "lifecycle.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "lifecycle.histogram")
  
  // 创建日志记录
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "lifecycle-logger")
  let log_record = azimuth::LogRecord::new(azimuth::Info, "Lifecycle test log")
  
  let creation_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let creation_duration = creation_end_time - creation_start_time
  
  // 验证创建阶段
  assert_true(creation_duration > 0L)
  assert_eq(azimuth::Span::name(span), "lifecycle-operation")
  assert_eq(counter.name, "lifecycle.counter")
  assert_eq(histogram.name, "lifecycle.histogram")
  assert_eq(logger.scope.name, "lifecycle-logger")
  
  azimuth::Span::end(span)
}
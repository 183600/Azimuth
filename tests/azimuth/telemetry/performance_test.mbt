// 性能相关测试
// Performance-related tests for Azimuth telemetry system

test "大量Span创建性能测试" {
  // 测试创建大量Span的性能
  let tracer_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(tracer_provider, "performance-test")
  
  let start_time = @azimuth.telemetry.api.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.api.platform.time.Clock::system())
  
  // 创建100个Span（减少数量以适应测试环境）
  for i = 0; i < 100; i = i + 1 {
    let span_name = "perf-span-" + i.to_string()
    let ctx = @azimuth.telemetry.api.context.Context::root()
    let (_, span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, ctx, span_name)
    @azimuth.telemetry.api.trace.Span::end(span)
  }
  
  let end_time = @azimuth.telemetry.api.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.api.platform.time.Clock::system())
  let duration = end_time - start_time
  
  // 验证在合理时间内完成
  assert_true(duration < 10000000000L) // 10秒
}

test "指标记录性能测试" {
  // 测试大量指标记录的性能
  let meter_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(meter_provider, "metrics-perf-test")
  
  let counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "perf.counter")
  let histogram = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(meter, "perf.histogram")
  
  let start_time = @azimuth.telemetry.api.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.api.platform.time.Clock::system())
  
  // 记录1000次指标（减少数量以适应测试环境）
  for i = 0; i < 1000; i = i + 1 {
    @azimuth.telemetry.sdk.metrics.Counter::add(counter, 1.0)
    @azimuth.telemetry.sdk.metrics.Histogram::record(histogram, i.to_double())
  }
  
  let end_time = @azimuth.telemetry.api.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.api.platform.time.Clock::system())
  let duration = end_time - start_time
  
  // 验证在合理时间内完成
  assert_true(duration < 5000000000L) // 5秒
}

test "日志记录性能测试" {
  // 测试大量日志记录的性能
  let logger_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(logger_provider, "log-perf-test")
  
  let start_time = @azimuth.telemetry.api.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.api.platform.time.Clock::system())
  
  // 记录500条日志（减少数量以适应测试环境）
  for i = 0; i < 500; i = i + 1 {
    let log_message = "Performance test log message " + i.to_string()
    let log_record = @azimuth.telemetry.api.logs.LogRecord::new(@azimuth.telemetry.api.logs.Info, log_message)
    @azimuth.telemetry.sdk.logs.Logger::emit(logger, log_record)
  }
  
  let end_time = @azimuth.telemetry.api.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.api.platform.time.Clock::system())
  let duration = end_time - start_time
  
  // 验证在合理时间内完成
  assert_true(duration < 3000000000L) // 3秒
}

test "属性操作性能测试" {
  // 测试大量属性操作的性能
  let start_time = @azimuth.telemetry.api.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.api.platform.time.Clock::system())
  
  // 创建和操作大量属性
  for i = 0; i < 100; i = i + 1 {
    let attrs = @azimuth.telemetry.api.common.Attributes::new()
    
    // 设置10个属性
    for j = 0; j < 10; j = j + 1 {
      let key = "attr." + j.to_string()
      let value = @azimuth.telemetry.api.common.StringValue("value." + j.to_string())
      @azimuth.telemetry.api.common.Attributes::set(attrs, key, value)
    }
    
    // 获取一些属性
    for j = 0; j < 5; j = j + 1 {
      let key = "attr." + j.to_string()
      let _ = @azimuth.telemetry.api.common.Attributes::get(attrs, key)
    }
  }
  
  let end_time = @azimuth.telemetry.api.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.api.platform.time.Clock::system())
  let duration = end_time - start_time
  
  // 验证在合理时间内完成
  assert_true(duration < 8000000000L) // 8秒
}
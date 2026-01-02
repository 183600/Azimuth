// 性能压力测试用例
// 测试Azimuth遥测系统在高负载下的性能表现

test "高并发span创建和结束性能" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "performance.stress.test")
  
  // 测量高并发span创建性能
  let start_time = Timestamp::now()
  let concurrent_spans = []
  
  // 创建1000个并发span
  for i = 0; i < 1000; i = i + 1 {
    let span = Tracer::start_span(tracer, "concurrent.span." + i.to_string())
    concurrent_spans = concurrent_spans + [span]
    
    // 为每个span添加属性
    Span::set_attribute(span, "iteration", i.to_string())
    Span::set_attribute(span, "concurrency.test", "true")
    
    // 每100个span添加一个事件
    if i % 100 == 0 {
      Span::add_event(span, "milestone.reached", [
        ("milestone", (i / 100).to_string()),
        ("timestamp", Timestamp::now().to_string())
      ])
    }
  }
  
  let creation_time = Timestamp::now()
  let creation_duration = Timestamp::difference(creation_time, start_time)
  
  // 快速结束所有span
  for span in concurrent_spans {
    Span::end(span)
  }
  
  let end_time = Timestamp::now()
  let total_duration = Timestamp::difference(end_time, start_time)
  
  // 验证性能指标
  assert_true(creation_duration.to_millis() < 5000)  // 创建1000个span应在5秒内完成
  assert_true(total_duration.to_millis() < 10000)    // 总操作应在10秒内完成
  
  // 验证所有span都被正确处理
  let exported_spans = TracerProvider::get_exported_spans(tracer_provider)
  assert_true(exported_spans.length() >= 900)  // 允许一些采样损失
  
  assert_true(true)
}

test "大批量度量数据处理性能" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance.metrics.test")
  
  // 创建多种类型的度量
  let counter = Meter::create_counter(meter, "stress.test.counter")
  let gauge = Meter::create_gauge(meter, "stress.test.gauge")
  let histogram = Meter::create_histogram(meter, "stress.test.histogram")
  
  let start_time = Timestamp::now()
  
  // 批量添加度量数据
  for batch = 0; batch < 10; batch = batch + 1 {
    // 每批1000个数据点
    for i = 0; i < 1000; i = i + 1 {
      let batch_id = batch.to_string()
      let iteration_id = i.to_string()
      
      // 添加计数器数据
      Counter::add_with_attributes(counter, 1.0, [
        ("batch", batch_id),
        ("iteration", iteration_id),
        ("type", "counter")
      ])
      
      // 设置仪表值
      Gauge::set_with_attributes(gauge, 100.0 + i.to_int().to_float(), [
        ("batch", batch_id),
        ("iteration", iteration_id),
        ("type", "gauge")
      ])
      
      // 记录直方图数据
      Histogram::record_with_attributes(histogram, 50.0 + (i % 50).to_int().to_float(), [
        ("batch", batch_id),
        ("iteration", iteration_id),
        ("type", "histogram")
      ])
    }
  }
  
  let data_generation_time = Timestamp::now()
  let generation_duration = Timestamp::difference(data_generation_time, start_time)
  
  // 强制刷新所有度量数据
  MeterProvider::flush(meter_provider)
  
  let flush_time = Timestamp::now()
  let total_duration = Timestamp::difference(flush_time, start_time)
  
  // 验证性能指标
  assert_true(generation_duration.to_millis() < 8000)  // 生成30000个数据点应在8秒内完成
  assert_true(total_duration.to_millis() < 15000)      // 总操作应在15秒内完成
  
  // 验证数据完整性
  let metrics = Meter::collect_metrics(meter)
  assert_true(metrics.length() > 0)
  
  assert_true(true)
}

test "高频日志记录性能测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "performance.logging.test")
  
  let start_time = Timestamp::now()
  
  // 高频日志记录
  for i = 0; i < 5000; i = i + 1 {
    let log_level = match i % 4 {
      0 => LogLevel::DEBUG
      1 => LogLevel::INFO
      2 => LogLevel::WARN
      _ => LogLevel::ERROR
    }
    
    let message = "High frequency log message " + i.to_string()
    
    Logger::emit_log(logger, log_level, message, [
      ("iteration", i.to_string()),
      ("batch", (i / 100).to_string()),
      ("thread", "main"),
      ("performance.test", "true")
    ])
  }
  
  let logging_time = Timestamp::now()
  let logging_duration = Timestamp::difference(logging_time, start_time)
  
  // 强制刷新所有日志
  LoggerProvider::flush(logger_provider)
  
  let flush_time = Timestamp::now()
  let total_duration = Timestamp::difference(flush_time, start_time)
  
  // 验证性能指标
  assert_true(logging_duration.to_millis() < 10000)  // 记录5000条日志应在10秒内完成
  assert_true(total_duration.to_millis() < 20000)    // 总操作应在20秒内完成
  
  // 验证日志记录
  let recorded_logs = LoggerProvider::get_recorded_logs(logger_provider)
  assert_true(recorded_logs.length() > 0)
  
  assert_true(true)
}

test "内存使用压力测试" {
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "memory.stress.test")
  let meter = MeterProvider::get_meter(meter_provider, "memory.stress.test")
  let logger = LoggerProvider::get_logger(logger_provider, "memory.stress.test")
  
  // 记录初始内存使用
  let initial_memory = MemoryMonitor::get_current_usage()
  
  // 创建大量对象但不立即释放
  let large_spans = []
  let large_metrics = []
  
  // 创建大量span
  for i = 0; i < 2000; i = i + 1 {
    let span = Tracer::start_span(tracer, "memory.stress.span." + i.to_string())
    
    // 为每个span添加大量属性
    for j = 0; j < 10; j = j + 1 {
      Span::set_attribute(span, "attr." + j.to_string(), "value." + i.to_string() + "." + j.to_string())
    }
    
    large_spans = large_spans + [span]
  }
  
  // 创建大量度量数据
  let counter = Meter::create_counter(meter, "memory.stress.counter")
  for i = 0; i < 10000; i = i + 1 {
    Counter::add_with_attributes(counter, 1.0, [
      ("large.attribute.name.that.consumes.memory." + i.to_string(), 
       "large.attribute.value.that.also.consumes.memory." + i.to_string())
    ])
  }
  
  // 记录峰值内存使用
  let peak_memory = MemoryMonitor::get_current_usage()
  
  // 释放所有span
  for span in large_spans {
    Span::end(span)
  }
  
  // 强制垃圾回收（如果可用）
  MemoryMonitor::force_gc()
  
  // 记录最终内存使用
  let final_memory = MemoryMonitor::get_current_usage()
  
  // 验证内存使用模式
  assert_true(peak_memory > initial_memory)  // 内存使用应该增加
  assert_true(final_memory < peak_memory)    // 释放后内存应该减少
  
  // 验证内存泄漏不严重（允许合理的内存增长）
  let memory_growth = final_memory - initial_memory
  assert_true(memory_growth < initial_memory * 2)  // 内存增长不应超过初始使用的200%
  
  assert_true(true)
}

test "长时间运行稳定性测试" {
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "stability.test")
  let meter = MeterProvider::get_meter(meter_provider, "stability.test")
  let logger = LoggerProvider::get_logger(logger_provider, "stability.test")
  
  let start_time = Timestamp::now()
  let test_duration_ms = 30000  // 30秒测试
  let iteration_count = 0
  
  // 长时间运行循环
  while Timestamp::difference(Timestamp::now(), start_time).to_millis() < test_duration_ms {
    iteration_count = iteration_count + 1
    
    // 创建span
    let span = Tracer::start_span(tracer, "stability.iteration." + iteration_count.to_string())
    Span::set_attribute(span, "iteration", iteration_count.to_string())
    Span::set_attribute(span, "timestamp", Timestamp::now().to_string())
    
    // 添加度量
    let counter = Meter::create_counter(meter, "stability.counter")
    Counter::add_with_attributes(counter, 1.0, [
      ("iteration", iteration_count.to_string()),
      ("timestamp", Timestamp::now().to_string())
    ])
    
    // 记录日志
    Logger::emit_log(logger, LogLevel::INFO, "Stability test iteration " + iteration_count.to_string(), [
      ("iteration", iteration_count.to_string()),
      ("timestamp", Timestamp::now().to_string())
    ])
    
    // 结束span
    Span::end(span)
    
    // 短暂休眠以模拟真实工作负载
    if iteration_count % 100 == 0 {
      // 每100次迭代刷新一次
      TracerProvider::flush(tracer_provider)
      MeterProvider::flush(meter_provider)
      LoggerProvider::flush(logger_provider)
    }
  }
  
  let end_time = Timestamp::now()
  let total_duration = Timestamp::difference(end_time, start_time)
  
  // 验证测试持续时间
  assert_true(total_duration.to_millis() >= test_duration_ms)
  
  // 验证系统在长时间运行后仍然稳定
  let final_span = Tracer::start_span(tracer, "stability.final.check")
  Span::set_attribute(final_span, "total.iterations", iteration_count.to_string())
  Span::set_attribute(final_span, "system.stable", "true")
  Span::end(final_span)
  
  // 验证数据收集仍然正常
  let exported_spans = TracerProvider::get_exported_spans(tracer_provider)
  let metrics = Meter::collect_metrics(meter)
  let logs = LoggerProvider::get_recorded_logs(logger_provider)
  
  assert_true(exported_spans.length() > 0)
  assert_true(metrics.length() > 0)
  assert_true(logs.length() > 0)
  
  assert_true(true)
}

test "资源限制下的性能表现" {
  // 创建带有资源限制的配置
  let limited_config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_max_spans(limited_config, 100)      // 限制最大span数
  TelemetryConfiguration::set_max_metrics(limited_config, 500)    // 限制最大度量数
  TelemetryConfiguration::set_max_logs(limited_config, 1000)      // 限制最大日志数
  TelemetryConfiguration::set_batch_size(limited_config, 10)      // 小批处理大小
  
  let tracer_provider = TracerProvider::builder()
    .with_config(limited_config)
    .build()
  
  let meter_provider = MeterProvider::builder()
    .with_config(limited_config)
    .build()
  
  let logger_provider = LoggerProvider::builder()
    .with_config(limited_config)
    .build()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "resource.limit.test")
  let meter = MeterProvider::get_meter(meter_provider, "resource.limit.test")
  let logger = LoggerProvider::get_logger(logger_provider, "resource.limit.test")
  
  // 尝试创建超过限制的span
  let spans_over_limit = []
  for i = 0; i < 200; i = i + 1 {  // 超过100的限制
    let span = Tracer::start_span(tracer, "over.limit.span." + i.to_string())
    spans_over_limit = spans_over_limit + [span]
  }
  
  // 结束所有span
  for span in spans_over_limit {
    Span::end(span)
  }
  
  // 创建超过限制的度量
  let counter = Meter::create_counter(meter, "over.limit.counter")
  for i = 0; i < 600; i = i + 1 {  // 超过500的限制
    Counter::add_with_attributes(counter, 1.0, [
      ("iteration", i.to_string()),
      ("over.limit", "true")
    ])
  }
  
  // 创建超过限制的日志
  for i = 0; i < 1200; i = i + 1 {  // 超过1000的限制
    Logger::emit_log(logger, LogLevel::INFO, "Over limit log " + i.to_string(), [
      ("iteration", i.to_string())
    ])
  }
  
  // 强制刷新
  TracerProvider::flush(tracer_provider)
  MeterProvider::flush(meter_provider)
  LoggerProvider::flush(logger_provider)
  
  // 验证资源限制被尊重
  let exported_spans = TracerProvider::get_exported_spans(tracer_provider)
  let metrics = Meter::collect_metrics(meter)
  let logs = LoggerProvider::get_recorded_logs(logger_provider)
  
  // 数量应该接近但不应该超过限制（允许一些误差）
  assert_true(exported_spans.length() <= 110)  // 允许10%误差
  assert_true(metrics.length() <= 550)         // 允许10%误差
  assert_true(logs.length() <= 1100)           // 允许10%误差
  
  // 验证系统在资源限制下仍然可用
  let final_span = Tracer::start_span(tracer, "resource.limit.final.check")
  Span::end(final_span)
  
  assert_true(true)
}
// Memory Management Tests
// 验证遥测系统的内存管理能力

test "span lifecycle memory management" {
  // 记录初始内存使用
  let initial_memory = azimuth::Memory::allocated()
  
  // 创建大量span并测试内存回收
  let spans = []
  let span_count = 1000
  
  // 创建span
  for i in 0..span_count {
    let trace_id = "memory-test-trace-" + i.to_string()
    let span_id = "memory-test-span-" + i.to_string()
    let span_ctx = azimuth::SpanContext::new(trace_id, span_id, true, "")
    let span = azimuth::Span::new("memory-test-operation", azimuth::Internal, span_ctx)
    
    // 设置属性
    azimuth::Span::set_attribute(span, "index", azimuth::IntValue(i))
    azimuth::Span::set_attribute(span, "name", azimuth::StringValue("test-" + i.to_string()))
    
    spans.push(span)
  }
  
  // 记录峰值内存
  let peak_memory = azimuth::Memory::allocated()
  
  // 清理span引用
  spans = []
  
  // 强制垃圾回收
  azimuth::Memory::collect()
  
  // 记录清理后内存
  let final_memory = azimuth::Memory::allocated()
  
  // 验证内存回收
  let memory_used = peak_memory - initial_memory
  let memory_recovered = peak_memory - final_memory
  let recovery_rate = memory_recovered.to_double() / memory_used.to_double()
  
  assert_true(recovery_rate > 0.7, "At least 70% of span memory should be recovered")
  
  // 验证每个span的内存使用合理
  let memory_per_span = memory_used.to_double() / span_count.to_double()
  assert_true(memory_per_span < 2048.0, "Each span should use less than 2KB memory")
}

test "metrics buffer memory management" {
  // 测试指标缓冲区的内存管理
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "memory-test-meter")
  let counter = azimuth::Meter::create_counter(meter, "memory.test.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "memory.test.histogram")
  
  let initial_memory = azimuth::Memory::allocated()
  
  // 生成大量指标数据
  let data_points = 10000
  for i in 0..data_points {
    azimuth::Counter::add(counter, 1, [{"index", azimuth::IntValue(i)}])
    azimuth::Histogram::record(histogram, i.to_double(), [{"type", azimuth::StringValue("memory-test")}])
  }
  
  let buffer_memory = azimuth::Memory::allocated()
  
  // 收集并清理指标数据
  let counter_metrics = azimuth::Counter::collect(counter)
  let histogram_metrics = azimuth::Histogram::collect(histogram)
  
  // 清理指标缓冲区
  azimuth::Meter::clear_buffer(counter)
  azimuth::Meter::clear_buffer(histogram)
  
  // 强制垃圾回收
  azimuth::Memory::collect()
  
  let final_memory = azimuth::Memory::allocated()
  
  // 验证指标缓冲区内存管理
  let buffer_memory_used = buffer_memory - initial_memory
  let buffer_memory_recovered = buffer_memory - final_memory
  let buffer_recovery_rate = buffer_memory_recovered.to_double() / buffer_memory_used.to_double()
  
  assert_true(buffer_recovery_rate > 0.8, "At least 80% of metrics buffer memory should be recovered")
  
  // 验证每个数据点的内存使用合理
  let memory_per_data_point = buffer_memory_used.to_double() / data_points.to_double()
  assert_true(memory_per_data_point < 100.0, "Each metrics data point should use less than 100 bytes")
}

test "log record memory pooling" {
  // 测试日志记录的内存池管理
  let provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(provider, "memory-pool-test-logger")
  
  let initial_memory = azimuth::Memory::allocated()
  
  // 创建大量日志记录
  let log_records = []
  let log_count = 5000
  
  for i in 0..log_count {
    let log_record = azimuth::Logger::create_log_record(logger)
    azimuth::LogRecord::set_body(log_record, "Memory pool test log message " + i.to_string())
    azimuth::LogRecord::set_severity(log_record, azimuth::INFO)
    azimuth::LogRecord::set_attribute(log_record, "index", azimuth::IntValue(i))
    azimuth::LogRecord::set_attribute(log_record, "pool.test", azimuth::BoolValue(true))
    
    log_records.push(log_record)
  }
  
  let pool_memory = azimuth::Memory::allocated()
  
  // 释放日志记录回内存池
  for log_record in log_records {
    azimuth::Logger::release_log_record(logger, log_record)
  }
  
  log_records = []
  
  // 强制垃圾回收
  azimuth::Memory::collect()
  
  let final_memory = azimuth::Memory::allocated()
  
  // 验证内存池效率
  let pool_memory_used = pool_memory - initial_memory
  let pool_memory_recovered = pool_memory - final_memory
  let pool_recovery_rate = pool_memory_recovered.to_double() / pool_memory_used.to_double()
  
  assert_true(pool_recovery_rate > 0.9, "At least 90% of log record memory should be recovered by pooling")
  
  // 测试内存池重用
  let reuse_start_memory = azimuth::Memory::allocated()
  
  // 重新创建相同数量的日志记录
  for i in 0..log_count {
    let log_record = azimuth::Logger::create_log_record(logger)
    azimuth::LogRecord::set_body(log_record, "Reuse test log message " + i.to_string())
    azimuth::LogRecord::set_severity(log_record, azimuth::INFO)
    
    // 释放回池
    azimuth::Logger::release_log_record(logger, log_record)
  }
  
  let reuse_end_memory = azimuth::Memory::allocated()
  
  // 验证内存池重用效率
  let reuse_memory_used = reuse_end_memory - reuse_start_memory
  let reuse_efficiency = 1.0 - (reuse_memory_used.to_double() / pool_memory_used.to_double())
  
  assert_true(reuse_efficiency > 0.5, "Memory pool reuse should save at least 50% of memory allocation")
}

test "attribute cache memory management" {
  // 测试属性缓存的内存管理
  let attributes = azimuth::Attributes::new()
  let initial_memory = azimuth::Memory::allocated()
  
  // 添加大量属性
  let attribute_count = 2000
  for i in 0..attribute_count {
    let key = "cache.test.attribute." + i.to_string()
    let value = azimuth::StringValue("cache test value " + i.to_string())
    azimuth::Attributes::set(attributes, key, value)
  }
  
  let cache_memory = azimuth::Memory::allocated()
  
  // 随机访问属性以测试缓存
  for i in 0..1000 {
    let random_index = i % attribute_count
    let key = "cache.test.attribute." + random_index.to_string()
    let value = azimuth::Attributes::get(attributes, key)
    assert_true(value.length > 0)
  }
  
  // 清理部分属性
  for i in 0..attribute_count / 2 {
    let key = "cache.test.attribute." + i.to_string()
    azimuth::Attributes::remove(attributes, key)
  }
  
  // 强制垃圾回收
  azimuth::Memory::collect()
  
  let final_memory = azimuth::Memory::allocated()
  
  // 验证属性缓存内存管理
  let cache_memory_used = cache_memory - initial_memory
  let cache_memory_recovered = cache_memory - final_memory
  let cache_recovery_rate = cache_memory_recovered.to_double() / cache_memory_used.to_double()
  
  assert_true(cache_recovery_rate > 0.6, "At least 60% of attribute cache memory should be recovered")
  
  // 验证剩余属性数量
  let remaining_keys = azimuth::Attributes::keys(attributes)
  assert_eq(remaining_keys.length, attribute_count / 2)
}

test "memory leak detection" {
  // 测试内存泄漏检测
  let leak_detector = azimuth::MemoryLeakDetector::new()
  
  // 开始泄漏检测
  azimuth::MemoryLeakDetector::start_tracking(leak_detector)
  
  // 创建各种遥测对象
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "leak-test-meter")
  let counter = azimuth::Meter::create_counter(meter, "leak.test.counter")
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "leak-test-logger")
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "leak-test-tracer")
  
  // 创建一些对象但不显式释放
  let spans = []
  let log_records = []
  let attributes_list = []
  
  for i in 0..100 {
    // 创建span
    let span_ctx = azimuth::SpanContext::new("leak-test-trace", "leak-test-span-" + i.to_string(), true, "")
    let span = azimuth::Tracer::start_span(tracer, "leak-test-operation", span_ctx)
    spans.push(span)
    
    // 创建日志记录
    let log_record = azimuth::Logger::create_log_record(logger)
    azimuth::LogRecord::set_body(log_record, "Leak test log " + i.to_string())
    log_records.push(log_record)
    
    // 创建属性
    let attributes = azimuth::Attributes::new()
    azimuth::Attributes::set(attributes, "leak.test", azimuth::IntValue(i))
    attributes_list.push(attributes)
  }
  
  // 记录当前对象数量
  let object_counts = azimuth::MemoryLeakDetector::get_object_counts(leak_detector)
  
  // 清理对象
  spans = []
  log_records = []
  attributes_list = []
  
  // 强制垃圾回收
  azimuth::Memory::collect()
  
  // 检查内存泄漏
  let leak_report = azimuth::MemoryLeakDetector::detect_leaks(leak_detector)
  
  // 验证没有严重的内存泄漏
  let leaked_objects = azimuth::LeakReport::leaked_objects(leak_report)
  let total_leaked = azimuth::LeakReport::total_leaked_bytes(leak_report)
  
  // 允许少量泄漏（由于垃圾回收的不确定性）
  assert_true(leaked_objects.length < 20, "Should have minimal object leaks")
  assert_true(total_leaked < 10240, "Should have less than 10KB of leaked memory")
  
  // 停止泄漏检测
  azimuth::MemoryLeakDetector::stop_tracking(leak_detector)
}

test "memory pressure handling" {
  // 测试内存压力处理
  let initial_memory = azimuth::Memory::allocated()
  let memory_limit = initial_memory + 50 * 1024 * 1024 // 50MB限制
  
  // 创建内存压力监控器
  let pressure_monitor = azimuth::MemoryPressureMonitor::new(memory_limit)
  
  // 在内存压力下创建对象
  let objects = []
  let under_pressure = false
  
  while (azimuth::Memory::allocated() < memory_limit) {
    // 创建span
    let span_ctx = azimuth::SpanContext::new("pressure-test", "pressure-span", true, "")
    let span = azimuth::Span::new("pressure-operation", azimuth::Internal, span_ctx)
    
    // 设置大量属性
    for i in 0..10 {
      let key = "pressure.attr." + i.to_string()
      let value = azimuth::StringValue("x" * 100) // 100字符的字符串
      azimuth::Span::set_attribute(span, key, value)
    }
    
    objects.push(span)
    
    // 检查内存压力
    if (azimuth::MemoryPressureMonitor::is_under_pressure(pressure_monitor)) {
      under_pressure = true
      break
    }
  }
  
  // 验证内存压力检测
  assert_true(under_pressure, "Should detect memory pressure")
  
  // 触发内存压力释放
  azimuth::MemoryPressureMonitor::trigger_release(pressure_monitor)
  
  // 清理对象
  objects = []
  
  // 强制垃圾回收
  azimuth::Memory::collect()
  
  let final_memory = azimuth::Memory::allocated()
  
  // 验证内存释放
  let memory_released = azimuth::MemoryPressureMonitor::memory_released(pressure_monitor)
  assert_true(memory_released > 0, "Should release memory under pressure")
  
  // 验证内存使用回到合理范围
  assert_true(final_memory < initial_memory + 10 * 1024 * 1024, "Memory usage should return to reasonable range")
}
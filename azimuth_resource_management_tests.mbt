// 遥测资源管理和优化测试用例
// 测试Azimuth遥测系统的资源管理和优化功能

test "资源池管理" {
  // 测试资源池管理功能
  let resource_pool = ResourcePool::new(100) // 最大100个资源
  
  // 创建资源池
  let span_pool = ResourcePool::create_span_pool(resource_pool, 50) // 最多50个span
  let metric_pool = ResourcePool::create_metric_pool(resource_pool, 30) // 最多30个度量
  let log_pool = ResourcePool::create_log_pool(resource_pool, 20) // 最多20个日志
  
  // 测试span资源池
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "resource.pool.test")
  
  let spans = []
  for i in 1..=60 { // 尝试创建60个span，超过池限制
    let span = ResourcePool::acquire_span(span_pool, tracer, "pooled.span." + i.to_string())
    match span {
      Some(s) => {
        Span::set_attribute(s, "pool.index", i)
        spans = spans.push(s)
      }
      None => assert_true(i > 50) // 超过池限制应该返回None
    }
  }
  
  // 验证只创建了50个span
  assert_eq(spans.length(), 50)
  
  // 释放span资源
  for i in 0..=24 {
    ResourcePool::release_span(span_pool, spans[i])
  }
  
  // 验证资源池中有可用资源
  assert_eq(ResourcePool::available_span_count(span_pool), 25)
  
  // 重新获取span资源
  let new_spans = []
  for i in 1..=25 {
    let span = ResourcePool::acquire_span(span_pool, tracer, "reused.span." + i.to_string())
    match span {
      Some(s) => new_spans = new_spans.push(s)
      None => assert_true(false)
    }
  }
  
  // 验证重新获取了25个span
  assert_eq(new_spans.length(), 25)
  
  // 测试度量资源池
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "resource.pool.test")
  
  let metrics = []
  for i in 1..=35 { // 尝试创建35个度量，超过池限制
    let metric = ResourcePool::acquire_counter(metric_pool, meter, "pooled.counter." + i.to_string())
    match metric {
      Some(m) => metrics = metrics.push(m)
      None => assert_true(i > 30)
    }
  }
  
  // 验证只创建了30个度量
  assert_eq(metrics.length(), 30)
  
  // 测试日志资源池
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "resource.pool.test")
  
  let logs = []
  for i in 1..=25 { // 尝试创建25个日志，超过池限制
    let log = ResourcePool::acquire_log(log_pool, logger, Info, "Pooled log " + i.to_string())
    match log {
      Some(l) => logs = logs.push(l)
      None => assert_true(i > 20)
    }
  }
  
  // 验证只创建了20个日志
  assert_eq(logs.length(), 20)
  
  // 释放所有资源
  for span in spans {
    ResourcePool::release_span(span_pool, span)
  }
  for span in new_spans {
    ResourcePool::release_span(span_pool, span)
  }
  for metric in metrics {
    ResourcePool::release_counter(metric_pool, metric)
  }
  for log in logs {
    ResourcePool::release_log(log_pool, log)
  }
  
  // 验证所有资源都已释放
  assert_eq(ResourcePool::available_span_count(span_pool), 50)
  assert_eq(ResourcePool::available_metric_count(metric_pool), 30)
  assert_eq(ResourcePool::available_log_count(log_pool), 20)
  
  assert_true(true)
}

test "内存优化策略" {
  // 测试内存优化策略
  let memory_optimizer = MemoryOptimizer::new()
  
  // 设置内存优化配置
  MemoryOptimizer::set_max_memory_usage(memory_optimizer, 100 * 1024 * 1024) // 100MB
  MemoryOptimizer::set_gc_threshold(memory_optimizer, 0.8) // 80%内存使用时触发GC
  MemoryOptimizer::set_buffer_size(memory_optimizer, 1024 * 1024) // 1MB缓冲区
  
  // 创建大量遥测数据
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "memory.optimization.test")
  
  let spans = []
  for i in 1..=10000 {
    let span = Tracer::start_span(tracer, "memory.test.span." + i.to_string())
    Span::set_attribute(span, "index", i)
    Span::set_attribute(span, "large.data", "x".repeat(1000)) // 每个span包含1KB数据
    spans = spans.push(span)
    
    // 检查内存使用
    let current_memory = Memory::get_usage()
    let max_memory = MemoryOptimizer::get_max_memory_usage(memory_optimizer)
    
    // 如果内存使用超过阈值，触发优化
    if current_memory > max_memory * MemoryOptimizer::get_gc_threshold(memory_optimizer) {
      MemoryOptimizer::optimize(memory_optimizer)
    }
  }
  
  // 验证内存优化效果
  let optimized_memory = Memory::get_usage()
  let max_memory = MemoryOptimizer::get_max_memory_usage(memory_optimizer)
  assert_true(optimized_memory < max_memory) // 内存使用应该在限制内
  
  // 测试缓冲区优化
  let buffer_manager = BufferManager::new()
  BufferManager::set_buffer_size(buffer_manager, 512 * 1024) // 512KB缓冲区
  
  // 创建大量缓冲区数据
  let buffers = []
  for i in 1..=100 {
    let buffer = BufferManager::acquire_buffer(buffer_manager)
    BufferManager::fill_buffer(buffer, "buffer data " + i.to_string())
    buffers = buffers.push(buffer)
  }
  
  // 验证缓冲区管理
  assert_eq(buffers.length(), 100)
  assert_true(BufferManager::get_memory_usage(buffer_manager) <= 512 * 1024)
  
  // 释放缓冲区
  for buffer in buffers {
    BufferManager::release_buffer(buffer_manager, buffer)
  }
  
  // 验证缓冲区已释放
  assert_eq(BufferManager::get_active_buffer_count(buffer_manager), 0)
  
  // 结束所有span
  for span in spans {
    Span::end(span)
  }
  
  assert_true(true)
}

test "资源限制和节流" {
  // 测试资源限制和节流
  let resource_limiter = ResourceLimiter::new()
  
  // 设置资源限制
  ResourceLimiter::set_max_spans_per_second(resource_limiter, 1000)
  ResourceLimiter::set_max_metrics_per_second(resource_limiter, 5000)
  ResourceLimiter::set_max_logs_per_second(resource_limiter, 2000)
  ResourceLimiter::set_max_memory_usage(resource_limiter, 50 * 1024 * 1024) // 50MB
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "resource.limiting.test")
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "resource.limiting.test")
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "resource.limiting.test")
  
  // 测试span创建限制
  let span_count = 0
  let span_rejected = 0
  let span_start = Time::now()
  
  for i in 1..=1500 { // 尝试创建1500个span，超过限制
    let span = ResourceLimiter::try_create_span(resource_limiter, tracer, "limited.span." + i.to_string())
    match span {
      Some(s) => {
        Span::set_attribute(s, "index", i)
        Span::end(s)
        span_count = span_count + 1
      }
      None => span_rejected = span_rejected + 1
    }
  }
  
  let span_end = Time::now()
  let span_duration = span_end - span_start
  
  // 验证span限制
  assert_true(span_count <= 1000)
  assert_true(span_rejected > 0)
  
  // 计算span创建速率
  let span_rate = span_count.to_float() / (span_duration.to_float() / 1000000000.0)
  assert_true(span_rate <= 1000.0) // 不应该超过1000 span/秒
  
  // 测试度量创建限制
  let metric_count = 0
  let metric_rejected = 0
  let metric_start = Time::now()
  
  let counter = Meter::create_counter(meter, "limited.counter", Some("Limited counter"), Some("count"))
  for i in 1..=6000 { // 尝试创建6000个度量，超过限制
    let success = ResourceLimiter::try_record_metric(resource_limiter, counter, 1.0, [("index", i)])
    if success {
      metric_count = metric_count + 1
    } else {
      metric_rejected = metric_rejected + 1
    }
  }
  
  let metric_end = Time::now()
  let metric_duration = metric_end - metric_start
  
  // 验证度量限制
  assert_true(metric_count <= 5000)
  assert_true(metric_rejected > 0)
  
  // 计算度量记录速率
  let metric_rate = metric_count.to_float() / (metric_duration.to_float() / 1000000000.0)
  assert_true(metric_rate <= 5000.0) // 不应该超过5000 metric/秒
  
  // 测试日志创建限制
  let log_count = 0
  let log_rejected = 0
  let log_start = Time::now()
  
  for i in 1..=2500 { // 尝试创建2500个日志，超过限制
    let log = LogRecord::new(Info, "Limited log " + i.to_string())
    let success = ResourceLimiter::try_emit_log(resource_limiter, logger, log)
    if success {
      log_count = log_count + 1
    } else {
      log_rejected = log_rejected + 1
    }
  }
  
  let log_end = Time::now()
  let log_duration = log_end - log_start
  
  // 验证日志限制
  assert_true(log_count <= 2000)
  assert_true(log_rejected > 0)
  
  // 计算日志记录速率
  let log_rate = log_count.to_float() / (log_duration.to_float() / 1000000000.0)
  assert_true(log_rate <= 2000.0) // 不应该超过2000 log/秒
  
  // 测试动态限制调整
  ResourceLimiter::adjust_limits(resource_limiter, [
    ("max_spans_per_second", 1500),
    ("max_metrics_per_second", 7500),
    ("max_logs_per_second", 3000)
  ])
  
  // 验证限制已调整
  assert_eq(ResourceLimiter::get_max_spans_per_second(resource_limiter), 1500)
  assert_eq(ResourceLimiter::get_max_metrics_per_second(resource_limiter), 7500)
  assert_eq(ResourceLimiter::get_max_logs_per_second(resource_limiter), 3000)
  
  assert_true(true)
}

test "资源回收和清理" {
  // 测试资源回收和清理
  let resource_cleaner = ResourceCleaner::new()
  
  // 设置清理策略
  ResourceCleaner::set_cleanup_interval(resource_cleaner, 5000) // 5秒清理一次
  ResourceCleaner::set_max_idle_time(resource_cleaner, 10000) // 10秒空闲时间后清理
  ResourceCleaner::set_cleanup_batch_size(resource_cleaner, 100) // 每次清理100个资源
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "resource.cleanup.test")
  
  // 创建大量资源但不显式释放
  let spans = []
  for i in 1..=500 {
    let span = Tracer::start_span(tracer, "cleanup.test.span." + i.to_string())
    Span::set_attribute(span, "index", i)
    spans = spans.push(span)
  }
  
  // 等待一段时间使资源变为空闲
  Time::sleep(100) // 等待100ms
  
  // 手动触发清理
  let cleaned_resources = ResourceCleaner::cleanup_idle_resources(resource_cleaner)
  assert_true(cleaned_resources > 0)
  
  // 验证资源已清理
  let remaining_resources = ResourceCleaner::get_active_resource_count(resource_cleaner)
  assert_true(remaining_resources < 500)
  
  // 测试自动清理
  let auto_cleanup_spans = []
  for i in 1..=200 {
    let span = Tracer::start_span(tracer, "auto.cleanup.span." + i.to_string())
    Span::set_attribute(span, "index", i)
    auto_cleanup_spans = auto_cleanup_spans.push(span)
  }
  
  // 启动自动清理
  ResourceCleaner::start_auto_cleanup(resource_cleaner)
  
  // 等待自动清理触发
  Time::sleep(200) // 等待200ms
  
  // 停止自动清理
  ResourceCleaner::stop_auto_cleanup(resource_cleaner)
  
  // 验证自动清理效果
  let auto_cleaned_resources = ResourceCleaner::get_cleaned_resource_count(resource_cleaner)
  assert_true(auto_cleaned_resources > 0)
  
  // 测试内存泄漏检测
  let memory_leak_detector = MemoryLeakDetector::new()
  MemoryLeakDetector::start_tracking(memory_leak_detector)
  
  // 创建和释放资源
  for i in 1..=100 {
    let span = Tracer::start_span(tracer, "leak.test.span." + i.to_string())
    Span::set_attribute(span, "index", i)
    Span::end(span)
  }
  
  // 检测内存泄漏
  let leak_report = MemoryLeakDetector::detect_leaks(memory_leak_detector)
  assert_true(leak_report.leaked_objects < 10) // 允许少量泄漏
  
  // 停止跟踪
  MemoryLeakDetector::stop_tracking(memory_leak_detector)
  
  assert_true(true)
}

test "资源监控和指标" {
  // 测试资源监控和指标
  let resource_monitor = ResourceMonitor::new()
  
  // 启动资源监控
  ResourceMonitor::start_monitoring(resource_monitor)
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "resource.monitoring.test")
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "resource.monitoring.test")
  
  // 创建资源并监控
  let spans = []
  for i in 1..=1000 {
    let span = Tracer::start_span(tracer, "monitored.span." + i.to_string())
    Span::set_attribute(span, "index", i)
    spans = spans.push(span)
    
    // 每创建100个span检查一次资源使用情况
    if i % 100 == 0 {
      let resource_usage = ResourceMonitor::get_current_usage(resource_monitor)
      assert_true(resource_usage.span_count == i)
      assert_true(resource_usage.memory_usage > 0)
    }
  }
  
  // 获取资源使用报告
  let usage_report = ResourceMonitor::get_usage_report(resource_monitor)
  assert_true(usage_report.span_count == 1000)
  assert_true(usage_report.memory_usage > 0)
  assert_true(usage_report.cpu_usage >= 0.0)
  assert_true(usage_report.cpu_usage <= 100.0)
  
  // 创建度量并监控
  let counter = Meter::create_counter(meter, "monitored.counter", Some("Monitored counter"), Some("count"))
  for i in 1..=5000 {
    Counter::add_with_attributes(counter, 1.0, [("index", i)])
    
    // 每记录1000个度量检查一次资源使用情况
    if i % 1000 == 0 {
      let resource_usage = ResourceMonitor::get_current_usage(resource_monitor)
      assert_true(resource_usage.metric_count >= i / 1000)
    }
  }
  
  // 获取度量使用报告
  let metric_report = ResourceMonitor::get_metric_report(resource_monitor)
  assert_true(metric_report.metric_count > 0)
  assert_true(metric_report.total_operations >= 5000)
  
  // 结束所有span
  for span in spans {
    Span::end(span)
  }
  
  // 等待资源释放
  Time::sleep(100)
  
  // 获取最终资源使用报告
  let final_report = ResourceMonitor::get_usage_report(resource_monitor)
  assert_true(final_report.span_count < 1000) // span应该已经释放
  
  // 测试资源告警
  let resource_alerter = ResourceAlerter::new()
  
  // 设置告警阈值
  ResourceAlerter::set_memory_threshold(resource_alerter, 100 * 1024 * 1024) // 100MB
  ResourceAlerter::set_cpu_threshold(resource_alerter, 80.0) // 80% CPU
  ResourceAlerter::set_span_threshold(resource_alerter, 2000) // 2000个span
  
  // 模拟资源使用超过阈值
  let alert_spans = []
  for i in 1..=2500 { // 超过span阈值
    let span = Tracer::start_span(tracer, "alert.span." + i.to_string())
    alert_spans = alert_spans.push(span)
  }
  
  // 检查告警
  let alerts = ResourceAlerter::check_alerts(resource_alerter)
  assert_true(alerts.length() > 0)
  
  // 验证span告警
  let span_alert = alerts.find(fn(alert) { alert.alert_type == "span_threshold_exceeded" })
  assert_true(span_alert != None)
  
  // 结束告警span
  for span in alert_spans {
    Span::end(span)
  }
  
  // 停止资源监控
  ResourceMonitor::stop_monitoring(resource_monitor)
  
  assert_true(true)
}

test "资源优化策略" {
  // 测试资源优化策略
  let resource_optimizer = ResourceOptimizer::new()
  
  // 设置优化策略
  ResourceOptimizer::enable_batch_processing(resource_optimizer, true)
  ResourceOptimizer::set_batch_size(resource_optimizer, 100)
  ResourceOptimizer::enable_compression(resource_optimizer, true)
  ResourceOptimizer::enable_caching(resource_optimizer, true)
  ResourceOptimizer::set_cache_size(resource_optimizer, 1000)
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "resource.optimization.test")
  
  // 测试批处理优化
  let batch_spans = []
  for i in 1..=500 {
    let span = Tracer::start_span(tracer, "batch.span." + i.to_string())
    Span::set_attribute(span, "index", i)
    batch_spans = batch_spans.push(span)
  }
  
  // 批量处理span
  let batch_start = Time::now()
  ResourceOptimizer::batch_process_spans(resource_optimizer, batch_spans)
  let batch_end = Time::now()
  let batch_duration = batch_end - batch_start
  
  // 单独处理span
  let individual_spans = []
  for i in 1..=500 {
    let span = Tracer::start_span(tracer, "individual.span." + i.to_string())
    Span::set_attribute(span, "index", i)
    individual_spans = individual_spans.push(span)
  }
  
  let individual_start = Time::now()
  for span in individual_spans {
    Span::end(span)
  }
  let individual_end = Time::now()
  let individual_duration = individual_end - individual_start
  
  // 验证批处理更高效
  assert_true(batch_duration < individual_duration)
  
  // 测试压缩优化
  let large_data_spans = []
  for i in 1..=100 {
    let span = Tracer::start_span(tracer, "large.data.span." + i.to_string())
    Span::set_attribute(span, "index", i)
    Span::set_attribute(span, "large.data", "x".repeat(10000)) // 10KB数据
    large_data_spans = large_data_spans.push(span)
  }
  
  // 压缩处理
  let compressed_size = ResourceOptimizer::compress_spans(resource_optimizer, large_data_spans)
  let uncompressed_size = large_data_spans.length() * 10000 // 估算未压缩大小
  
  // 验证压缩效果
  assert_true(compressed_size < uncompressed_size)
  let compression_ratio = compressed_size.to_float() / uncompressed_size.to_float()
  assert_true(compression_ratio < 0.8) // 至少20%的压缩率
  
  // 测试缓存优化
  let cached_spans = []
  for i in 1..=200 {
    let span = Tracer::start_span(tracer, "cached.span." + i.to_string())
    Span::set_attribute(span, "index", i)
    
    // 第一次处理（应该缓存）
    let first_process_time = ResourceOptimizer::process_span_with_cache(resource_optimizer, span)
    
    // 第二次处理相同span（应该使用缓存）
    let second_process_time = ResourceOptimizer::process_span_with_cache(resource_optimizer, span)
    
    // 验证缓存加速
    assert_true(second_process_time <= first_process_time)
    
    cached_spans = cached_spans.push(span)
  }
  
  // 获取缓存统计
  let cache_stats = ResourceOptimizer::get_cache_stats(resource_optimizer)
  assert_true(cache_stats.hits > 0)
  assert_true(cache_stats.hit_rate > 0.5) // 至少50%的缓存命中率
  
  // 测试自适应优化
  ResourceOptimizer::enable_adaptive_optimization(resource_optimizer, true)
  
  // 创建不同模式的负载
  let adaptive_spans = []
  for i in 1..=1000 {
    let span = Tracer::start_span(tracer, "adaptive.span." + i.to_string())
    Span::set_attribute(span, "index", i)
    
    // 模拟不同负载模式
    if i % 3 == 0 {
      Span::set_attribute(span, "large.data", "x".repeat(5000)) // 大数据
    } else if i % 3 == 1 {
      Span::set_attribute(span, "many.attributes", "y".repeat(100)) // 多属性
    } else {
      Span::set_attribute(span, "normal.data", "z".repeat(100)) // 正常数据
    }
    
    adaptive_spans = adaptive_spans.push(span)
  }
  
  // 自适应优化处理
  let adaptive_start = Time::now()
  ResourceOptimizer::adaptive_process_spans(resource_optimizer, adaptive_spans)
  let adaptive_end = Time::now()
  let adaptive_duration = adaptive_end - adaptive_start
  
  // 获取自适应优化统计
  let adaptive_stats = ResourceOptimizer::get_adaptive_stats(resource_optimizer)
  assert_true(adaptive_stats.optimization_cycles > 0)
  assert_true(adaptive_stats.performance_improvement > 0.0)
  
  // 结束所有span
  for span in batch_spans {
    Span::end(span)
  }
  for span in large_data_spans {
    Span::end(span)
  }
  for span in cached_spans {
    Span::end(span)
  }
  for span in adaptive_spans {
    Span::end(span)
  }
  
  assert_true(true)
}
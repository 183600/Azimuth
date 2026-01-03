// 内存泄漏防护和资源清理测试
// 测试系统在各种场景下的内存管理和资源清理能力

test "Span生命周期内存管理" {
  // 测试Span生命周期的内存管理
  let leak_detector = MemoryLeakDetector::new()
  leak_detector.start_tracking()
  
  let initial_memory = leak_detector.get_allocated_memory()
  
  // 创建大量Span并测试内存回收
  let spans = []
  let span_count = 5000
  
  for i in 0..span_count {
    let trace_id = "memory-leak-test-trace-" + i.to_string()
    let span_id = "memory-leak-test-span-" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("memory-leak-test-operation", Internal, span_ctx)
    
    // 设置属性
    Span::set_attribute(span, "index", IntValue(i))
    Span::set_attribute(span, "name", StringValue("test-" + i.to_string()))
    Span::set_attribute(span, "large.data", StringValue("x" * 100)) // 添加一些大属性
    
    // 添加事件
    Span::add_event(span, "test-event-" + i.to_string(), [
      ("event.index", IntValue(i)),
      ("event.data", StringValue("event-data-" + i.to_string()))
    ])
    
    spans.push(span)
  }
  
  let peak_memory = leak_detector.get_allocated_memory()
  
  // 结束所有Span
  for span in spans {
    Span::end(span)
  }
  
  // 清理引用
  spans = []
  
  // 强制垃圾回收
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_allocated_memory()
  
  // 验证内存回收
  let memory_used = peak_memory - initial_memory
  let memory_recovered = peak_memory - final_memory
  let recovery_rate = memory_recovered.to_double() / memory_used.to_double()
  
  assert_true(recovery_rate > 0.8, "Should recover at least 80% of span memory")
  
  // 检查内存泄漏
  let leak_report = leak_detector.detect_leaks()
  assert_true(leak_report.leaked_objects < 100, "Should have minimal span object leaks")
  assert_true(leak_report.leaked_bytes < 1024 * 1024, "Should have less than 1MB of leaked memory")
  
  leak_detector.stop_tracking()
}

test "指标缓冲区内存管理" {
  // 测试指标缓冲区的内存管理
  let leak_detector = MemoryLeakDetector::new()
  leak_detector.start_tracking()
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "memory-leak-test-meter")
  
  let counter = Meter::create_counter(meter, "memory.leak.test.counter")
  let histogram = Meter::create_histogram(meter, "memory.leak.test.histogram")
  let gauge = Meter::create_gauge(meter, "memory.leak.test.gauge")
  
  let initial_memory = leak_detector.get_allocated_memory()
  
  // 生成大量指标数据
  let data_points = 50000
  
  for i in 0..data_points {
    let attributes = [
      ("index", IntValue(i)),
      ("name", StringValue("test-" + i.to_string())),
      ("large.attribute", StringValue("x" * 50)) // 添加大属性
    ]
    
    Counter::add(counter, 1.0, attributes)
    Histogram::record(histogram, i.to_double(), attributes)
    Gauge::record(gauge, (i % 1000).to_double(), attributes)
  }
  
  let buffer_memory = leak_detector.get_allocated_memory()
  
  // 收集并清理指标数据
  let counter_metrics = Counter::collect(counter)
  let histogram_metrics = Histogram::collect(histogram)
  let gauge_metrics = Gauge::collect(gauge)
  
  // 清理指标缓冲区
  Meter::clear_buffer(counter)
  Meter::clear_buffer(histogram)
  Meter::clear_buffer(gauge)
  
  // 强制垃圾回收
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_allocated_memory()
  
  // 验证指标缓冲区内存管理
  let buffer_memory_used = buffer_memory - initial_memory
  let buffer_memory_recovered = buffer_memory - final_memory
  let buffer_recovery_rate = buffer_memory_recovered.to_double() / buffer_memory_used.to_double()
  
  assert_true(buffer_recovery_rate > 0.85, "Should recover at least 85% of metrics buffer memory")
  
  // 验证每个数据点的内存使用合理
  let memory_per_data_point = buffer_memory_used.to_double() / (data_points * 3).to_double()
  assert_true(memory_per_data_point < 200.0, "Each metrics data point should use less than 200 bytes")
  
  // 检查指标内存泄漏
  let leak_report = leak_detector.detect_leaks()
  assert_true(leak_report.leaked_objects < 50, "Should have minimal metrics object leaks")
  
  leak_detector.stop_tracking()
}

test "日志记录内存池管理" {
  // 测试日志记录的内存池管理
  let leak_detector = MemoryLeakDetector::new()
  leak_detector.start_tracking()
  
  let provider = LoggerProvider::with_pool(1000) // 1000个日志记录的内存池
  let logger = LoggerProvider::get_logger(provider, "memory-pool-test-logger")
  
  let initial_memory = leak_detector.get_allocated_memory()
  
  // 创建大量日志记录
  let log_records = []
  let log_count = 10000
  
  for i in 0..log_count {
    let log_record = LogRecord::new(Info, "Memory pool test log message " + i.to_string())
    
    // 设置属性
    LogRecord::set_attribute(log_record, "index", IntValue(i))
    LogRecord::set_attribute(log_record, "pool.test", BoolValue(true))
    LogRecord::set_attribute(log_record, "large.data", StringValue("x" * 200))
    
    // 设置上下文
    let trace_id = "pool-test-trace-" + (i % 100).to_string()
    let span_id = "pool-test-span-" + i.to_string()
    LogRecord::set_trace_context(log_record, trace_id, span_id)
    
    log_records.push(log_record)
  }
  
  let pool_memory = leak_detector.get_allocated_memory()
  
  // 释放日志记录回内存池
  for log_record in log_records {
    Logger::release_log_record(logger, log_record)
  }
  
  log_records = []
  
  // 强制垃圾回收
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_allocated_memory()
  
  // 验证内存池效率
  let pool_memory_used = pool_memory - initial_memory
  let pool_memory_recovered = pool_memory - final_memory
  let pool_recovery_rate = pool_memory_recovered.to_double() / pool_memory_used.to_double()
  
  assert_true(pool_recovery_rate > 0.9, "Should recover at least 90% of log record memory by pooling")
  
  // 测试内存池重用效率
  let reuse_start_memory = leak_detector.get_allocated_memory()
  
  // 重新创建相同数量的日志记录
  for i in 0..log_count {
    let log_record = Logger::create_log_record(logger)
    LogRecord::set_body(log_record, "Reuse test log message " + i.to_string())
    LogRecord::set_severity(log_record, Info)
    
    // 释放回池
    Logger::release_log_record(logger, log_record)
  }
  
  let reuse_end_memory = leak_detector.get_allocated_memory()
  
  // 验证内存池重用效率
  let reuse_memory_used = reuse_end_memory - reuse_start_memory
  let reuse_efficiency = 1.0 - (reuse_memory_used.to_double() / pool_memory_used.to_double())
  
  assert_true(reuse_efficiency > 0.7, "Memory pool reuse should save at least 70% of memory allocation")
  
  leak_detector.stop_tracking()
}

test "属性缓存内存管理" {
  // 测试属性缓存的内存管理
  let leak_detector = MemoryLeakDetector::new()
  leak_detector.start_tracking()
  
  let attributes = Attributes::with_cache(5000) // 5000个条目的缓存
  let initial_memory = leak_detector.get_allocated_memory()
  
  // 添加大量属性
  let attribute_count = 10000
  
  for i in 0..attribute_count {
    let key = "cache.test.attribute." + i.to_string()
    let value = StringValue("cache test value " + i.to_string() + " " + "x" * 100)
    Attributes::set(attributes, key, value)
  }
  
  let cache_memory = leak_detector.get_allocated_memory()
  
  // 随机访问属性以测试缓存
  for i in 0..5000 {
    let random_index = Random::next_int() % attribute_count
    let key = "cache.test.attribute." + random_index.to_string()
    let value = Attributes::get(attributes, key)
    assert_true(value.length > 0)
  }
  
  // 清理部分属性
  for i in 0..attribute_count / 2 {
    let key = "cache.test.attribute." + i.to_string()
    Attributes::remove(attributes, key)
  }
  
  // 强制垃圾回收
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_allocated_memory()
  
  // 验证属性缓存内存管理
  let cache_memory_used = cache_memory - initial_memory
  let cache_memory_recovered = cache_memory - final_memory
  let cache_recovery_rate = cache_memory_recovered.to_double() / cache_memory_used.to_double()
  
  assert_true(cache_recovery_rate > 0.6, "Should recover at least 60% of attribute cache memory")
  
  // 验证剩余属性数量
  let remaining_keys = Attributes::keys(attributes)
  assert_eq(remaining_keys.length, attribute_count / 2)
  
  // 测试缓存命中率
  let cache_stats = Attributes::cache_stats(attributes)
  assert_true(cache_stats.hit_rate > 0.8, "Cache hit rate should be above 80%")
  
  leak_detector.stop_tracking()
}

test "传播器载体内存管理" {
  // 测试传播器载体的内存管理
  let leak_detector = MemoryLeakDetector::new()
  leak_detector.start_tracking()
  
  let propagator = W3CTraceContextPropagator::new()
  let initial_memory = leak_detector.get_allocated_memory()
  
  let carriers = []
  let carrier_count = 5000
  
  // 创建大量传播器载体
  for i in 0..carrier_count {
    let carrier = TextMapCarrier::new()
    
    // 注入大量头部
    TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    TextMapCarrier::set(carrier, "baggage", "user.id=12345,request.id=req-67890,tenant.id=tenant-001")
    TextMapCarrier::set(carrier, "x-custom-header-" + i.to_string(), "custom-value-" + i.to_string())
    
    // 添加一些大头部
    TextMapCarrier::set(carrier, "x-large-header", "x" * 1000)
    
    carriers.push(carrier)
  }
  
  let carrier_memory = leak_detector.get_allocated_memory()
  
  // 提取上下文
  for carrier in carriers {
    let extracted_ctx = W3CTraceContextPropagator::extract(propagator, carrier)
    assert_true(SpanContext::is_valid(extracted_ctx))
  }
  
  // 清理载体
  carriers = []
  
  // 强制垃圾回收
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_allocated_memory()
  
  // 验证载体内存管理
  let carrier_memory_used = carrier_memory - initial_memory
  let carrier_memory_recovered = carrier_memory - final_memory
  let carrier_recovery_rate = carrier_memory_recovered.to_double() / carrier_memory_used.to_double()
  
  assert_true(carrier_recovery_rate > 0.85, "Should recover at least 85% of carrier memory")
  
  leak_detector.stop_tracking()
}

test "资源生命周期自动管理" {
  // 测试资源生命周期的自动管理
  let leak_detector = MemoryLeakDetector::new()
  leak_detector.start_tracking()
  
  let resource_manager = ResourceManager::auto()
  let initial_memory = leak_detector.get_allocated_memory()
  
  let resources = []
  let resource_count = 3000
  
  // 创建大量资源并注册自动管理
  for i in 0..resource_count {
    let resource = Resource::with_attributes(Resource::new(), [
      ("resource.id", IntValue(i)),
      ("resource.name", StringValue("auto-managed-resource-" + i.to_string())),
      ("resource.type", StringValue("test-resource")),
      ("large.data", StringValue("x" * 500))
    ])
    
    // 注册自动管理
    ResourceManager::register(resource_manager, resource, {
      // 设置自动清理策略
      cleanup_policy: "on_gc",
      priority: i % 10,
      timeout_ms: 5000
    })
    
    resources.push(resource)
  }
  
  let resource_memory = leak_detector.get_allocated_memory()
  
  // 模拟资源使用
  for i in 0..resource_count {
    let resource = resources[i]
    
    // 访问资源
    let resource_id = Resource::get_attribute(resource, "resource.id")
    assert_eq(resource_id, Some(IntValue(i)))
    
    // 修改资源
    Resource::set_attribute(resource, "last.accessed", StringValue(Clock::now_unix_nanos(Clock::system()).to_string()))
  }
  
  // 清理部分资源引用
  for i in 0..resource_count / 2 {
    resources[i] = None
  }
  
  // 强制垃圾回收和自动清理
  leak_detector.force_gc()
  ResourceManager::cleanup_expired(resource_manager)
  
  let final_memory = leak_detector.get_allocated_memory()
  
  // 验证自动资源管理
  let resource_memory_used = resource_memory - initial_memory
  let resource_memory_recovered = resource_memory - final_memory
  let resource_recovery_rate = resource_memory_recovered.to_double() / resource_memory_used.to_double()
  
  assert_true(resource_recovery_rate > 0.7, "Should recover at least 70% of resource memory through auto-management")
  
  // 验证资源管理器统计
  let manager_stats = ResourceManager::stats(resource_manager)
  assert_true(manager_stats.registered_resources == resource_count, "Should track all registered resources")
  assert_true(manager_stats.cleaned_resources > 0, "Should have cleaned up some resources")
  
  leak_detector.stop_tracking()
}

test "内存压力下的资源管理" {
  // 测试内存压力下的资源管理
  let leak_detector = MemoryLeakDetector::new()
  leak_detector.start_tracking()
  
  let initial_memory = leak_detector.get_allocated_memory()
  let memory_limit = initial_memory + 100 * 1024 * 1024 // 100MB限制
  
  // 创建内存压力监控器
  let pressure_monitor = MemoryPressureMonitor::new(memory_limit)
  
  let objects = []
  let under_pressure = false
  let pressure_triggered = false
  
  // 在内存压力下创建对象
  while (leak_detector.get_allocated_memory() < memory_limit) {
    // 创建span
    let span_ctx = SpanContext::new("pressure-test", "pressure-span", true, "")
    let span = Span::new("pressure-operation", Internal, span_ctx)
    
    // 设置大量属性
    for i in 0..20 {
      let key = "pressure.attr." + i.to_string()
      let value = StringValue("x" * 200) // 200字符的字符串
      Span::set_attribute(span, key, value)
    }
    
    // 添加事件
    Span::add_event(span, "pressure-event", [
      ("event.data", StringValue("x" * 500))
    ])
    
    objects.push(span)
    
    // 检查内存压力
    if MemoryPressureMonitor::is_under_pressure(pressure_monitor) {
      under_pressure = true
      break
    }
  }
  
  // 验证内存压力检测
  assert_true(under_pressure, "Should detect memory pressure")
  
  // 触发内存压力释放
  if MemoryPressureMonitor::trigger_release(pressure_monitor) {
    pressure_triggered = true
  }
  
  // 清理对象
  objects = []
  
  // 强制垃圾回收
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_allocated_memory()
  
  // 验证内存释放
  assert_true(pressure_triggered, "Should trigger memory pressure release")
  
  let memory_released = MemoryPressureMonitor::memory_released(pressure_monitor)
  assert_true(memory_released > 0, "Should release memory under pressure")
  
  // 验证内存使用回到合理范围
  assert_true(final_memory < initial_memory + 50 * 1024 * 1024, "Memory usage should return to reasonable range")
  
  leak_detector.stop_tracking()
}

test "循环引用检测和清理" {
  // 测试循环引用检测和清理
  let leak_detector = MemoryLeakDetector::new()
  leak_detector.start_tracking()
  
  let initial_memory = leak_detector.get_allocated_memory()
  
  // 创建具有循环引用的对象图
  let objects = []
  
  for i in 0..1000 {
    let span_a = Span::new("span-a-" + i.to_string(), Internal, 
      SpanContext::new("trace-" + i.to_string(), "span-a-" + i.to_string(), true, ""))
    let span_b = Span::new("span-b-" + i.to_string(), Internal, 
      SpanContext::new("trace-" + i.to_string(), "span-b-" + i.to_string(), true, ""))
    
    // 设置相互引用（模拟循环引用）
    Span::set_attribute(span_a, "linked.span", StringValue("span-b-" + i.to_string()))
    Span::set_attribute(span_b, "linked.span", StringValue("span-a-" + i.to_string()))
    
    // 添加事件引用
    Span::add_event(span_a, "references-b", [("span.id", StringValue("span-b-" + i.to_string()))])
    Span::add_event(span_b, "references-a", [("span.id", StringValue("span-a-" + i.to_string()))])
    
    objects.push((span_a, span_b))
  }
  
  let circular_memory = leak_detector.get_allocated_memory()
  
  // 检测循环引用
  let cycle_detector = CircularRefDetector::new()
  let detected_cycles = []
  
  for (span_a, span_b) in objects {
    if CircularRefDetector::detect_cycle(cycle_detector, span_a, span_b) {
      detected_cycles.push((span_a, span_b))
    }
  }
  
  // 验证循环引用检测
  assert_true(detected_cycles.length > 0, "Should detect circular references")
  
  // 清理循环引用
  for (span_a, span_b) in detected_cycles {
    // 断开引用
    Span::remove_attribute(span_a, "linked.span")
    Span::remove_attribute(span_b, "linked.span")
  }
  
  // 清理对象
  objects = []
  
  // 强制垃圾回收
  leak_detector.force_gc()
  
  let final_memory = leak_detector.get_allocated_memory()
  
  // 验证循环引用清理效果
  let circular_memory_used = circular_memory - initial_memory
  let circular_memory_recovered = circular_memory - final_memory
  let circular_recovery_rate = circular_memory_recovered.to_double() / circular_memory_used.to_double()
  
  assert_true(circular_recovery_rate > 0.75, "Should recover at least 75% of circular reference memory")
  
  leak_detector.stop_tracking()
}
// 性能边界测试
// 测试Azimuth遥测系统在各种极端条件下的性能表现

test "高并发span创建和结束性能" {
  // 测试在高并发情况下创建和结束大量span的性能
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrency.test")
  
  let num_spans = 10000
  let num_threads = 50
  let spans_per_thread = num_spans / num_threads
  
  let performance_monitor = PerformanceMonitor::new()
  
  // 开始性能监控
  performance_monitor.start_monitoring()
  
  let mut threads = []
  
  // 创建多个线程并发创建span
  for i in 0..num_threads {
    let thread = Thread::spawn(fn() {
      let thread_spans = []
      
      for j in 0..spans_per_thread {
        let span_name = "concurrent.operation." + j.to_string()
        let span = Tracer::start_span(tracer, span_name)
        
        // 设置属性
        Span::set_attribute(span, "thread.id", i.to_string())
        Span::set_attribute(span, "operation.index", j.to_string())
        Span::set_attribute(span, "payload.size", (1024 + j % 1024).to_string())
        
        // 添加事件
        if j % 10 == 0 {
          Span::add_event(span, "milestone.reached", [
            ("milestone.index", (j / 10).to_string()),
            ("thread.id", i.to_string())
          ])
        }
        
        thread_spans = thread_spans.push(span)
      }
      
      // 结束所有span
      for span in thread_spans {
        Span::end(span)
      }
      
      return thread_spans.length()
    })
    
    threads = threads.push(thread)
  }
  
  // 等待所有线程完成
  let total_spans_created = 0
  for thread in threads {
    let spans_in_thread = thread.join()
    total_spans_created = total_spans_created + spans_in_thread
  }
  
  // 停止性能监控
  let performance_metrics = performance_monitor.stop_monitoring()
  
  // 验证结果
  assert_eq(total_spans_created, num_spans)
  
  // 验证性能指标
  let execution_time = performance_metrics.get_execution_time_ms()
  let memory_usage = performance_metrics.get_peak_memory_usage_mb()
  let cpu_usage = performance_metrics.get_average_cpu_usage_percent()
  
  assert_true(execution_time < 5000)  // 应该在5秒内完成
  assert_true(memory_usage < 500)    // 内存使用应该少于500MB
  assert_true(cpu_usage < 95.0)      // CPU使用率应该合理
  
  // 验证吞吐量
  let spans_per_second = (num_spans.to_float() / execution_time.to_float()) * 1000.0
  assert_true(spans_per_second > 1000.0)  // 应该至少每秒处理1000个span
  
  // 验证span创建和结束的时间分布
  let time_distribution = performance_metrics.get_operation_time_distribution()
  let p95 = time_distribution.get_percentile(95.0)
  let p99 = time_distribution.get_percentile(99.0)
  
  assert_true(p95 < 10.0)   // 95%的操作应该在10ms内完成
  assert_true(p99 < 50.0)   // 99%的操作应该在50ms内完成
}

test "大量度量化性能测试" {
  // 测试处理大量度量数据的性能
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "metrics.performance.test")
  
  let performance_monitor = PerformanceMonitor::new()
  performance_monitor.start_monitoring()
  
  // 创建多种类型的度量
  let counters = []
  let histograms = []
  let gauges = []
  
  // 创建100个计数器
  for i in 0..100 {
    let counter = Meter::create_counter(
      meter, 
      "counter." + i.to_string(), 
      Some("Test counter " + i.to_string()), 
      Some("count")
    )
    counters = counters.push(counter)
  }
  
  // 创建100个直方图
  for i in 0..100 {
    let histogram = Meter::create_histogram(
      meter, 
      "histogram." + i.to_string(), 
      Some("Test histogram " + i.to_string()), 
      Some("ms")
    )
    histograms = histograms.push(histogram)
  }
  
  // 创建100个仪表
  for i in 0..100 {
    let gauge = Meter::create_gauge(
      meter, 
      "gauge." + i.to_string(), 
      Some("Test gauge " + i.to_string()), 
      Some("value")
    )
    gauges = gauges.push(gauge)
  }
  
  // 并发更新所有度量
  let mut threads = []
  
  // 计数器更新线程
  let counter_thread = Thread::spawn(fn() {
    for round in 0..1000 {
      for i in 0..counters.length() {
        let counter = counters.get(i)
        Counter::add_with_attributes(counter, 1.0, [
          ("round", round.to_string()),
          ("counter.id", i.to_string()),
          ("update.type", "incremental")
        ])
      }
    }
  })
  
  // 直方图更新线程
  let histogram_thread = Thread::spawn(fn() {
    for round in 0..1000 {
      for i in 0..histograms.length() {
        let histogram = histograms.get(i)
        let value = 50.0 + (Math::random() * 100.0)
        Histogram::record_with_attributes(histogram, value, [
          ("round", round.to_string()),
          ("histogram.id", i.to_string()),
          ("value.range", if value < 75.0 { "low" } else { "high" })
        ])
      }
    }
  })
  
  // 仪表更新线程
  let gauge_thread = Thread::spawn(fn() {
    for round in 0..1000 {
      for i in 0..gauges.length() {
        let gauge = gauges.get(i)
        let value = 100.0 + Math::sin(round.to_float() * 0.1) * 50.0
        Gauge::set_with_attributes(gauge, value, [
          ("round", round.to_string()),
          ("gauge.id", i.to_string()),
          ("value.category", "performance")
        ])
      }
    }
  })
  
  threads = threads.push(counter_thread)
  threads = threads.push(histogram_thread)
  threads = threads.push(gauge_thread)
  
  // 等待所有线程完成
  for thread in threads {
    thread.join()
  }
  
  let performance_metrics = performance_monitor.stop_monitoring()
  
  // 验证性能指标
  let execution_time = performance_metrics.get_execution_time_ms()
  let memory_usage = performance_metrics.get_peak_memory_usage_mb()
  let throughput = performance_metrics.get_metrics_throughput_per_second()
  
  assert_true(execution_time < 10000)  // 应该在10秒内完成
  assert_true(memory_usage < 1000)    // 内存使用应该少于1GB
  assert_true(throughput > 50000.0)   // 每秒应该处理超过50000个度量更新
  
  // 验证度量聚合性能
  let aggregation_start = Time::now()
  
  for counter in counters {
    let aggregated = Counter::aggregate(counter)
    assert_true(aggregated.get_total() > 0.0)
  }
  
  for histogram in histograms {
    let aggregated = Histogram::aggregate(histogram)
    assert_true(aggregated.get_count() > 0)
  }
  
  let aggregation_time = Time::since(aggregation_start).as_millis()
  assert_true(aggregation_time < 5000)  // 聚合应该在5秒内完成
}

test "内存压力和垃圾回收测试" {
  // 测试在内存压力下的遥测系统性能
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "memory.stress.test")
  
  let memory_monitor = MemoryMonitor::new()
  memory_monitor.start_monitoring()
  
  // 创建大量span但不立即结束，模拟内存压力
  let active_spans = []
  let span_count = 50000
  
  for i in 0..span_count {
    let span = Tracer::start_span(tracer, "memory.stress.span." + i.to_string())
    
    // 添加大量属性和事件增加内存使用
    Span::set_attribute(span, "span.index", i.to_string())
    Span::set_attribute(span, "large.payload", "x" * 1000)  // 1KB payload
    
    for j in 0..10 {
      Span::add_event(span, "stress.event." + j.to_string(), [
        ("event.index", j.to_string()),
        ("payload.data", "y" * 500)  // 500 bytes per event
      ])
    }
    
    active_spans = active_spans.push(span)
    
    // 每10000个span检查一次内存使用
    if i % 10000 == 0 {
      let current_memory = memory_monitor.get_current_memory_usage_mb()
      assert_true(current_memory < 2000)  // 内存使用应该在合理范围内
    }
  }
  
  // 检查峰值内存使用
  let peak_memory = memory_monitor.get_peak_memory_usage_mb()
  assert_true(peak_memory < 3000)  // 峰值内存使用应该少于3GB
  
  // 逐步结束span，测试内存释放
  let batches_to_end = 10
  let spans_per_batch = span_count / batches_to_end
  
  for batch in 0..batches_to_end {
    let start_index = batch * spans_per_batch
    let end_index = start_index + spans_per_batch
    
    for i in start_index..end_index {
      Span::end(active_spans.get(i))
    }
    
    // 每批结束后检查内存释放
    let memory_after_batch = memory_monitor.get_current_memory_usage_mb()
    let expected_memory_reduction = (spans_per_batch.to_float() / span_count.to_float()) * peak_memory
    
    // 内存应该有明显释放（允许一些延迟）
    assert_true(memory_after_batch < peak_memory - (expected_memory_reduction * 0.5))
    
    // 触发垃圾回收
    System::gc()
    
    // 再次检查内存
    let memory_after_gc = memory_monitor.get_current_memory_usage_mb()
    assert_true(memory_after_gc <= memory_after_batch)
  }
  
  memory_monitor.stop_monitoring()
  
  // 验证内存泄漏
  let final_memory = memory_monitor.get_current_memory_usage_mb()
  let baseline_memory = memory_monitor.get_baseline_memory_usage_mb()
  
  assert_true(final_memory - baseline_memory < 100)  // 最终内存增长应该少于100MB
}

test "长时间运行稳定性测试" {
  // 测试遥测系统长时间运行的稳定性
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "stability.test")
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "stability.test")
  
  let stability_monitor = StabilityMonitor::new()
  stability_monitor.start_monitoring()
  
  let test_duration_minutes = 10  // 运行10分钟
  let end_time = Time::now() + Duration::minutes(test_duration_minutes)
  let operation_counter = AtomicCounter::new(0)
  
  // 创建长期运行的度量
  let long_term_counter = Meter::create_counter(meter, "long_term.operations", Some("Long term operations"), Some("count"))
  let long_term_histogram = Meter::create_histogram(meter, "operation.duration", Some("Operation duration"), Some("ms"))
  let memory_gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("MB"))
  
  // 模拟长期运行的工作负载
  let worker_thread = Thread::spawn(fn() {
    while Time::now() < end_time {
      let operation_start = Time::now()
      
      // 创建span
      let span = Tracer::start_span(tracer, "long.term.operation")
      Span::set_attribute(span, "operation.id", operation_counter.increment().to_string())
      
      // 模拟工作
      Thread::sleep(Duration::milliseconds(10 + (Math::random() * 40).to_int()))  // 10-50ms工作
      
      // 更新度量
      Counter::add(long_term_counter, 1.0)
      
      let operation_duration = Time::since(operation_start).as_millis().to_float()
      Histogram::record(long_term_histogram, operation_duration)
      
      let current_memory = Process::get_memory_usage_mb()
      Gauge::set(memory_gauge, current_memory.to_float())
      
      Span::end(span)
      
      // 短暂休息
      Thread::sleep(Duration::milliseconds(5))
    }
  })
  
  // 监控系统健康状况
  let monitor_thread = Thread::spawn(fn() {
    while Time::now() < end_time {
      let current_memory = Process::get_memory_usage_mb()
      let cpu_usage = Process::get_cpu_usage_percent()
      let thread_count = Process::get_thread_count()
      
      stability_monitor.record_system_metrics(current_memory, cpu_usage, thread_count)
      
      // 检查系统健康状况
      if current_memory > 1000 {
        stability_monitor.record_health_issue("high_memory_usage", current_memory.to_string())
      }
      
      if cpu_usage > 90.0 {
        stability_monitor.record_health_issue("high_cpu_usage", cpu_usage.to_string())
      }
      
      if thread_count > 100 {
        stability_monitor.record_health_issue("high_thread_count", thread_count.to_string())
      }
      
      Thread::sleep(Duration::seconds(1))
    }
  })
  
  // 等待测试完成
  worker_thread.join()
  monitor_thread.join()
  
  stability_monitor.stop_monitoring()
  
  // 分析稳定性结果
  let stability_report = stability_monitor.generate_report()
  
  // 验证系统稳定性
  assert_true(stability_report.get_uptime_minutes() >= test_duration_minutes * 0.95)  // 至少运行95%的时间
  assert_true(stability_report.get_health_issues().length() < 5)  // 健康问题应该很少
  assert_true(stability_report.get_average_memory_usage() < 500)  // 平均内存使用应该合理
  assert_true(stability_report.get_average_cpu_usage() < 80.0)   // 平均CPU使用应该合理
  
  // 验证度量数据完整性
  let final_counter_value = Counter::get_value(long_term_counter)
  let histogram_data = Histogram::get_data(long_term_histogram)
  
  assert_true(final_counter_value > 0.0)
  assert_true(histogram_data.get_count() > 0)
  assert_eq(histogram_data.get_count(), final_counter_value.to_int())  // 计数和直方图应该一致
  
  // 验证性能没有显著退化
  let performance_degradation = stability_report.get_performance_degradation_percent()
  assert_true(performance_degradation < 20.0)  // 性能退化应该少于20%
}

test "极端数据量边界测试" {
  // 测试处理极端数据量的边界情况
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "boundary.test")
  
  // 测试极长span名称
  let very_long_name = "a" * 10000
  let long_name_span = Tracer::start_span(tracer, very_long_name)
  assert_true(Span::is_valid(long_name_span))
  Span::end(long_name_span)
  
  // 测试极多属性
  let many_attributes_span = Tracer::start_span(tracer, "many.attributes.span")
  
  let max_attributes = 1000
  for i in 0..max_attributes {
    Span::set_attribute(many_attributes_span, "attr." + i.to_string(), "value." + i.to_string())
  }
  
  assert_true(Span::is_valid(many_attributes_span))
  Span::end(many_attributes_span)
  
  // 测试极多事件
  let many_events_span = Tracer::start_span(tracer, "many.events.span")
  
  let max_events = 5000
  for i in 0..max_events {
    Span::add_event(many_events_span, "event." + i.to_string(), [
      ("event.index", i.to_string()),
      ("event.data", "x" * 100)  // 每个事件100字节
    ])
  }
  
  assert_true(Span::is_valid(many_events_span))
  Span::end(many_events_span)
  
  // 测试极深层嵌套span
  let nested_spans = []
  let max_depth = 1000
  
  let current_span = Tracer::start_span(tracer, "root.span")
  nested_spans = nested_spans.push(current_span)
  
  for i in 0..max_depth {
    let parent_span = nested_spans.last()
    let child_span = Tracer::start_span_with_context(tracer, "nested.span." + i.to_string(), Span::span_context(parent_span))
    nested_spans = nested_spans.push(child_span)
  }
  
  // 验证最深层span仍然有效
  let deepest_span = nested_spans.last()
  assert_true(Span::is_valid(deepest_span))
  
  // 从最深层开始结束span
  for i in (0..nested_spans.length()).reverse() {
    Span::end(nested_spans.get(i))
  }
  
  // 测试极大量度量
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "boundary.metrics.test")
  
  // 创建大量度量
  let metrics_count = 10000
  let metrics = []
  
  for i in 0..metrics_count {
    let counter = Meter::create_counter(
      meter,
      "metric." + i.to_string(),
      Some("Metric " + i.to_string()),
      Some("count")
    )
    metrics = metrics.push(counter)
    
    // 立即更新
    Counter::add(counter, i.to_float())
  }
  
  // 验证所有度量都有效
  for metric in metrics {
    let value = Counter::get_value(metric)
    assert_true(value >= 0.0)
  }
  
  // 测试极值度量
  let extreme_counter = Meter::create_counter(meter, "extreme.values", Some("Extreme values"), Some("count"))
  
  // 测试极大值
  Counter::add(extreme_counter, Float::MAX / 2.0)
  Counter::add(extreme_counter, Float::MIN / 2.0)
  
  let extreme_value = Counter::get_value(extreme_counter)
  assert_true(extreme_value.is_finite())
  
  // 测试极小值
  let tiny_counter = Meter::create_counter(meter, "tiny.values", Some("Tiny values"), Some("count"))
  
  Counter::add(tiny_counter, Float::MIN_POSITIVE)
  Counter::add(tiny_counter, Float::EPSILON)
  
  let tiny_value = Counter::get_value(tiny_counter)
  assert_true(tiny_value > 0.0)
  
  // 测试直方图极值
  let extreme_histogram = Meter::create_histogram(meter, "extreme.histogram", Some("Extreme histogram"), Some("units"))
  
  Histogram::record(extreme_histogram, Float::MAX / 1000.0)
  Histogram::record(extreme_histogram, Float::MIN_POSITIVE * 1000.0)
  Histogram::record(extreme_histogram, Float::INFINITY)
  Histogram::record(extreme_histogram, Float::NEG_INFINITY)
  Histogram::record(extreme_histogram, Float::NAN)
  
  let extreme_histogram_data = Histogram::get_data(extreme_histogram)
  assert_true(extreme_histogram_data.get_count() >= 5)
  
  // 验证系统能够处理极端情况而不崩溃
  assert true  // 如果能执行到这里说明系统没有崩溃
}

test "资源耗尽恢复测试" {
  // 测试在资源耗尽情况下的恢复能力
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "resource.exhaustion.test")
  
  let resource_monitor = ResourceMonitor::new()
  resource_monitor.start_monitoring()
  
  // 模拟内存耗尽
  let memory_exhaustion_detected = AtomicBool::new(false)
  let recovery_triggered = AtomicBool::new(false)
  
  let memory_stress_thread = Thread::spawn(fn() {
    let large_objects = []
    
    // 分配大量内存直到接近限制
    while !memory_exhaustion_detected.get() {
      let large_object = String::repeat("x", 1024 * 1024)  // 1MB对象
      large_objects = large_objects.push(large_object)
      
      let current_memory = Process::get_memory_usage_mb()
      if current_memory > Process::get_available_memory_mb() * 0.9 {
        memory_exhaustion_detected.set(true)
      }
    }
    
    // 释放部分内存
    for i in 0..large_objects.length() / 2 {
      // 让对象超出作用域
    }
    
    recovery_triggered.set(true)
  })
  
  // 在内存压力下继续使用遥测系统
  let telemetry_thread = Thread::spawn(fn() {
    let operations_completed = 0
    
    while !recovery_triggered.get() {
      let span = Tracer::start_span(tracer, "resource.stress.operation")
      
      Span::set_attribute(span, "operation.index", operations_completed.to_string())
      Span::set_attribute(span, "memory.pressure", "true")
      
      // 尝试正常操作
      try {
        Span::add_event(span, "operation.started", [])
        Thread::sleep(Duration::milliseconds(1))
        Span::add_event(span, "operation.completed", [])
      } catch {
        // 在资源压力下可能会失败，这是可以接受的
      }
      
      Span::end(span)
      operations_completed = operations_completed + 1
      
      Thread::sleep(Duration::milliseconds(10))
    }
    
    return operations_completed
  })
  
  // 等待内存压力测试完成
  memory_stress_thread.join()
  let operations_under_stress = telemetry_thread.join()
  
  // 验证系统恢复能力
  assert_true(recovery_triggered.get())
  assert_true(operations_under_stress > 0)  // 即使在压力下也应该完成一些操作
  
  // 等待系统稳定
  Thread::sleep(Duration::seconds(5))
  
  // 测试恢复后的正常操作
  let recovery_test_start = Time::now()
  let recovery_operations = 1000
  
  for i in 0..recovery_operations {
    let span = Tracer::start_span(tracer, "recovery.test.operation")
    Span::set_attribute(span, "operation.index", i.to_string())
    Span::end(span)
  }
  
  let recovery_test_time = Time::since(recovery_test_start).as_millis()
  
  // 验证恢复后性能
  assert_true(recovery_test_time < 10000)  // 1000个操作应该在10秒内完成
  assert_true(recovery_test_time / recovery_operations < 10)  // 平均每个操作少于10ms
  
  resource_monitor.stop_monitoring()
  
  // 分析资源使用情况
  let resource_report = resource_monitor.generate_report()
  
  assert_true(resource_report.get_peak_memory_usage() > 0)
  assert_true(resource_report.get_memory_recovery_time_ms() > 0)
  assert_true(resource_report.get_operations_under_pressure() > 0)
  assert_true(resource_report.get_recovery_success_rate() > 0.8)  // 恢复成功率应该高于80%
}
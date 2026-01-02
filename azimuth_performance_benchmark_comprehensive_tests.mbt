// Azimuth性能基准测试用例
// 测试Azimuth遥测系统在各种负载下的性能表现

test "遥测数据收集性能基准" {
  // 设置性能基准测试参数
  let test_iterations = 10000
  let concurrent_threads = 10
  let batch_size = 100
  
  // 创建性能监控器
  let performance_monitor = PerformanceMonitor::new()
  PerformanceMonitor::start_monitoring(performance_monitor)
  
  // 测试span创建和结束的性能
  let span_creation_times = []
  
  for i in 0..=test_iterations-1 {
    let start_time = PerformanceMonitor::get_high_resolution_time()
    
    // 创建span
    let tracer = TracerProvider::get_tracer(TracerProvider::default(), "performance.test")
    let span = Tracer::start_span(tracer, "performance.operation." + i.to_string())
    
    // 设置属性
    Span::set_attribute(span, "operation.id", i.to_string())
    Span::set_attribute(span, "operation.type", "benchmark")
    
    // 添加事件
    Span::add_event(span, "operation.started", [("timestamp", PerformanceMonitor::get_current_timestamp().to_string())])
    
    // 结束span
    Span::end(span)
    
    let end_time = PerformanceMonitor::get_high_resolution_time()
    let duration = end_time - start_time
    span_creation_times = span_creation_times.push(duration)
  }
  
  // 计算span创建性能指标
  let avg_span_creation_time = span_creation_times.reduce(fn(acc, time) { acc + time }, 0L) / span_creation_times.length()
  let max_span_creation_time = span_creation_times.reduce(fn(acc, time) { if time > acc { time } else { acc } }, 0L)
  let min_span_creation_time = span_creation_times.reduce(fn(acc, time) { if time < acc { time } else { acc } }, 9223372036854775807L)
  
  // 验证span创建性能在合理范围内
  assert_true(avg_span_creation_time < 100000L)  // 平均创建时间应小于100微秒
  assert_true(max_span_creation_time < 1000000L)  // 最大创建时间应小于1毫秒
  
  // 测试度量记录的性能
  let metric_recording_times = []
  let meter = MeterProvider::get_meter(MeterProvider::default(), "performance.test")
  let counter = Meter::create_counter(meter, "performance.operations", Some("Performance operations"), Some("count"))
  
  for i in 0..=test_iterations-1 {
    let start_time = PerformanceMonitor::get_high_resolution_time()
    
    // 记录度量
    Counter::add_with_attributes(counter, 1.0, [
      ("operation.id", i.to_string()),
      ("operation.type", "benchmark"),
      ("thread.id", "0")
    ])
    
    let end_time = PerformanceMonitor::get_high_resolution_time()
    let duration = end_time - start_time
    metric_recording_times = metric_recording_times.push(duration)
  }
  
  // 计算度量记录性能指标
  let avg_metric_recording_time = metric_recording_times.reduce(fn(acc, time) { acc + time }, 0L) / metric_recording_times.length()
  let max_metric_recording_time = metric_recording_times.reduce(fn(acc, time) { if time > acc { time } else { acc } }, 0L)
  
  // 验证度量记录性能在合理范围内
  assert_true(avg_metric_recording_time < 50000L)  // 平均记录时间应小于50微秒
  assert_true(max_metric_recording_time < 500000L)  // 最大记录时间应小于500微秒
  
  // 测试日志记录的性能
  let log_recording_times = []
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "performance.test")
  
  for i in 0..=test_iterations-1 {
    let start_time = PerformanceMonitor::get_high_resolution_time()
    
    // 创建并记录日志
    let log = LogRecord::new(Info, "Performance test operation " + i.to_string())
    LogRecord::add_attribute(log, "operation.id", i.to_string())
    LogRecord::add_attribute(log, "operation.type", "benchmark")
    
    Logger::emit(logger, log)
    
    let end_time = PerformanceMonitor::get_high_resolution_time()
    let duration = end_time - start_time
    log_recording_times = log_recording_times.push(duration)
  }
  
  // 计算日志记录性能指标
  let avg_log_recording_time = log_recording_times.reduce(fn(acc, time) { acc + time }, 0L) / log_recording_times.length()
  let max_log_recording_time = log_recording_times.reduce(fn(acc, time) { if time > acc { time } else { acc } }, 0L)
  
  // 验证日志记录性能在合理范围内
  assert_true(avg_log_recording_time < 75000L)  // 平均记录时间应小于75微秒
  assert_true(max_log_recording_time < 750000L)  // 最大记录时间应小于750微秒
  
  // 停止性能监控
  PerformanceMonitor::stop_monitoring(performance_monitor)
  
  // 获取系统资源使用情况
  let resource_usage = PerformanceMonitor::get_resource_usage(performance_monitor)
  
  // 验证资源使用在合理范围内
  assert_true(resource_usage.memory_usage < 100 * 1024 * 1024)  // 内存使用应小于100MB
  assert_true(resource_usage.cpu_usage < 80.0)  // CPU使用率应小于80%
}

test "并发遥测操作性能测试" {
  // 设置并发测试参数
  let concurrent_operations = 1000
  let thread_count = 10
  let operations_per_thread = concurrent_operations / thread_count
  
  // 创建并发性能监控器
  let concurrent_monitor = ConcurrentPerformanceMonitor::new()
  ConcurrentPerformanceMonitor::start_monitoring(concurrent_monitor)
  
  // 创建线程同步机制
  let start_barrier = ThreadBarrier::new(thread_count)
  let completion_latch = CountDownLatch::new(thread_count)
  let results = ThreadSafeArray::new()
  
  // 启动多个线程执行并发操作
  for thread_id in 0..=thread_count-1 {
    Thread::spawn(fn() {
      // 等待所有线程准备就绪
      ThreadBarrier::wait(start_barrier)
      
      let thread_start_time = ConcurrentPerformanceMonitor::get_high_resolution_time()
      let thread_operation_times = []
      
      // 执行分配给该线程的操作
      for i in 0..=operations_per_thread-1 {
        let operation_id = thread_id * operations_per_thread + i
        let operation_start_time = ConcurrentPerformanceMonitor::get_high_resolution_time()
        
        // 创建span
        let tracer = TracerProvider::get_tracer(TracerProvider::default(), "concurrent.test")
        let span = Tracer::start_span(tracer, "concurrent.operation." + operation_id.to_string())
        
        // 设置属性
        Span::set_attribute(span, "operation.id", operation_id.to_string())
        Span::set_attribute(span, "thread.id", thread_id.to_string())
        
        // 记录度量
        let meter = MeterProvider::get_meter(MeterProvider::default(), "concurrent.test")
        let counter = Meter::create_counter(meter, "concurrent.operations", Some("Concurrent operations"), Some("count"))
        
        Counter::add_with_attributes(counter, 1.0, [
          ("operation.id", operation_id.to_string()),
          ("thread.id", thread_id.to_string())
        ])
        
        // 记录日志
        let logger = LoggerProvider::get_logger(LoggerProvider::default(), "concurrent.test")
        let log = LogRecord::new(Info, "Concurrent operation " + operation_id.to_string())
        LogRecord::add_attribute(log, "operation.id", operation_id.to_string())
        LogRecord::add_attribute(log, "thread.id", thread_id.to_string())
        
        Logger::emit(logger, log)
        
        // 结束span
        Span::end(span)
        
        let operation_end_time = ConcurrentPerformanceMonitor::get_high_resolution_time()
        let operation_duration = operation_end_time - operation_start_time
        thread_operation_times = thread_operation_times.push(operation_duration)
      }
      
      let thread_end_time = ConcurrentPerformanceMonitor::get_high_resolution_time()
      let thread_total_duration = thread_end_time - thread_start_time
      
      // 计算线程性能指标
      let thread_avg_operation_time = thread_operation_times.reduce(fn(acc, time) { acc + time }, 0L) / thread_operation_times.length()
      let thread_max_operation_time = thread_operation_times.reduce(fn(acc, time) { if time > acc { time } else { acc } }, 0L)
      
      // 存储线程结果
      let thread_result = {
        "thread_id": thread_id,
        "total_duration": thread_total_duration,
        "avg_operation_time": thread_avg_operation_time,
        "max_operation_time": thread_max_operation_time,
        "operation_count": thread_operation_times.length()
      }
      
      ThreadSafeArray::push(results, thread_result)
      CountDownLatch::count_down(completion_latch)
    })
  }
  
  // 等待所有线程完成
  CountDownLatch::await(completion_latch, 30000)  // 最多等待30秒
  
  // 收集并分析结果
  let all_results = ThreadSafeArray::to_array(results)
  assert_eq(all_results.length(), thread_count)
  
  // 计算总体并发性能指标
  let total_thread_durations = all_results.map(fn(r) { r.total_duration })
  let max_thread_duration = total_thread_durations.reduce(fn(acc, duration) { if duration > acc { duration } else { acc } }, 0L)
  let total_operations = all_results.reduce(fn(acc, r) { acc + r.operation_count }, 0)
  
  let avg_thread_operation_times = all_results.map(fn(r) { r.avg_operation_time })
  let overall_avg_operation_time = avg_thread_operation_times.reduce(fn(acc, time) { acc + time }, 0L) / avg_thread_operation_times.length()
  
  // 计算吞吐量（操作/秒）
  let throughput_seconds = max_thread_duration.to_float() / 1000000000.0
  let operations_per_second = total_operations.to_float() / throughput_seconds
  
  // 验证并发性能指标
  assert_true(overall_avg_operation_time < 200000L)  // 平均操作时间应小于200微秒
  assert_true(operations_per_second > 10000.0)  // 每秒应能处理超过10000个操作
  
  // 停止并发性能监控
  ConcurrentPerformanceMonitor::stop_monitoring(concurrent_monitor)
  
  // 获取并发资源使用情况
  let concurrent_resource_usage = ConcurrentPerformanceMonitor::get_resource_usage(concurrent_monitor)
  
  // 验证并发资源使用在合理范围内
  assert_true(concurrent_resource_usage.peak_memory_usage < 200 * 1024 * 1024)  // 峰值内存使用应小于200MB
  assert_true(concurrent_resource_usage.max_cpu_usage < 90.0)  // 最大CPU使用率应小于90%
  assert_true(concurrent_resource_usage.thread_count == thread_count)  // 线程数应正确
}

test "内存使用效率和垃圾回收影响测试" {
  // 设置内存测试参数
  let memory_test_iterations = 50000
  let memory_sample_interval = 5000  // 每5000次操作采样一次内存使用情况
  
  // 创建内存监控器
  let memory_monitor = MemoryMonitor::new()
  MemoryMonitor::start_monitoring(memory_monitor)
  
  let memory_samples = []
  
  // 执行大量遥测操作并监控内存使用
  for i in 0..=memory_test_iterations-1 {
    // 创建span
    let tracer = TracerProvider::get_tracer(TracerProvider::default(), "memory.test")
    let span = Tracer::start_span(tracer, "memory.operation." + i.to_string())
    
    // 设置大量属性（增加内存使用）
    for j in 0..=9 {
      Span::set_attribute(span, "attribute." + j.to_string(), "value-" + i.to_string() + "-" + j.to_string())
    }
    
    // 记录度量
    let meter = MeterProvider::get_meter(MeterProvider::default(), "memory.test")
    let histogram = Meter::create_histogram(meter, "memory.test.histogram", Some("Memory test histogram"), Some("bytes"))
    
    Histogram::record_with_attributes(histogram, i.to_float(), [
      ("operation.id", i.to_string()),
      ("attribute.large", "large-value-" + i.to_string().repeat(10))  // 创建较大的字符串值
    ])
    
    // 记录日志
    let logger = LoggerProvider::get_logger(LoggerProvider::default(), "memory.test")
    let log = LogRecord::new(Info, "Memory test operation " + i.to_string())
    LogRecord::add_attribute(log, "operation.id", i.to_string())
    LogRecord::add_attribute(log, "large.data", "large-data-" + i.to_string().repeat(20))
    
    Logger::emit(logger, log)
    
    // 结束span
    Span::end(span)
    
    // 定期采样内存使用情况
    if i % memory_sample_interval == 0 {
      let current_memory = MemoryMonitor::get_current_memory_usage(memory_monitor)
      memory_samples = memory_samples.push((i, current_memory))
    }
    
    // 定期触发垃圾回收（模拟真实环境）
    if i % 10000 == 0 && i > 0 {
      MemoryMonitor::trigger_garbage_collection()
    }
  }
  
  // 分析内存使用模式
  let memory_values = memory_samples.map(fn(sample) { sample.1 })
  let initial_memory = memory_values[0]
  let final_memory = memory_values[memory_values.length() - 1]
  let peak_memory = memory_values.reduce(fn(acc, memory) { if memory > acc { memory } else { acc } }, 0L)
  let avg_memory = memory_values.reduce(fn(acc, memory) { acc + memory }, 0L) / memory_values.length()
  
  // 计算内存增长率
  let memory_growth_rate = (final_memory - initial_memory).to_float() / memory_test_iterations.to_float()
  
  // 验证内存使用效率
  assert_true(memory_growth_rate < 1024.0)  // 每次操作的内存增长应小于1KB
  assert_true(peak_memory < 500 * 1024 * 1024)  // 峰值内存使用应小于500MB
  assert_true(avg_memory < 200 * 1024 * 1024)  // 平均内存使用应小于200MB
  
  // 验证垃圾回收效果
  let gc_samples = memory_samples.filter(fn(sample) { sample.0 % 10000 == 0 && sample.0 > 0 })
  if gc_samples.length() >= 2 {
    let before_gc = gc_samples[0].1
    let after_gc = gc_samples[1].1
    let gc_efficiency = (before_gc - after_gc).to_float() / before_gc.to_float()
    
    assert_true(gc_efficiency > 0.1)  // 垃圾回收应能回收至少10%的内存
  }
  
  // 停止内存监控
  MemoryMonitor::stop_monitoring(memory_monitor)
  
  // 获取详细的内存使用报告
  let memory_report = MemoryMonitor::get_memory_report(memory_monitor)
  
  // 验证内存报告包含必要的信息
  assert_true(memory_report.contains_key("initial_memory"))
  assert_true(memory_report.contains_key("final_memory"))
  assert_true(memory_report.contains_key("peak_memory"))
  assert_true(memory_report.contains_key("avg_memory"))
  assert_true(memory_report.contains_key("gc_count"))
  assert_true(memory_report.contains_key("gc_total_time"))
}

test "大规模遥测数据吞吐量测试" {
  // 设置吞吐量测试参数
  let throughput_test_duration = 30  // 30秒测试
  let target_throughput = 50000  // 目标每秒50000个操作
  let reporting_interval = 5  // 每5秒报告一次吞吐量
  
  // 创建吞吐量监控器
  let throughput_monitor = ThroughputMonitor::new()
  ThroughputMonitor::start_monitoring(throughput_monitor)
  
  // 创建吞吐量统计
  let throughput_stats = ThreadSafeCounter::new()
  let start_time = ThroughputMonitor::get_current_time()
  let test_end_time = start_time + (throughput_test_duration * 1000)
  
  // 启动多个生产者线程生成遥测数据
  let producer_threads = 5
  let completion_signal = AtomicBool::new(false)
  
  for thread_id in 0..=producer_threads-1 {
    Thread::spawn(fn() {
      let operations_per_second = target_throughput / producer_threads
      let operation_interval = 1000000000L / operations_per_second  // 纳秒
      
      let mut next_operation_time = ThroughputMonitor::get_high_resolution_time()
      
      while ThroughputMonitor::get_current_time() < test_end_time {
        let current_time = ThroughputMonitor::get_high_resolution_time()
        
        if current_time >= next_operation_time {
          // 执行遥测操作
          let operation_start = ThroughputMonitor::get_high_resolution_time()
          
          // 创建span
          let tracer = TracerProvider::get_tracer(TracerProvider::default(), "throughput.test")
          let span = Tracer::start_span(tracer, "throughput.operation")
          
          // 设置基本属性
          Span::set_attribute(span, "thread.id", thread_id.to_string())
          Span::set_attribute(span, "operation.time", operation_start.to_string())
          
          // 记录度量
          let meter = MeterProvider::get_meter(MeterProvider::default(), "throughput.test")
          let counter = Meter::create_counter(meter, "throughput.operations", Some("Throughput operations"), Some("count"))
          
          Counter::add_with_attributes(counter, 1.0, [
            ("thread.id", thread_id.to_string()),
            ("operation.time", operation_start.to_string())
          ])
          
          // 记录日志
          let logger = LoggerProvider::get_logger(LoggerProvider::default(), "throughput.test")
          let log = LogRecord::new(Info, "Throughput test operation")
          LogRecord::add_attribute(log, "thread.id", thread_id.to_string())
          
          Logger::emit(logger, log)
          
          // 结束span
          Span::end(span)
          
          // 增加操作计数
          ThreadSafeCounter::increment(throughput_stats)
          
          // 计算下一个操作的时间
          next_operation_time = next_operation_time + operation_interval
        }
        
        // 短暂休眠以避免过度占用CPU
        Thread::sleep(1000)  // 1微秒
      }
    })
  }
  
  // 启动吞吐量报告线程
  let throughput_reports = ThreadSafeArray::new()
  let report_thread = Thread::spawn(fn() {
    let mut last_report_time = start_time
    let mut last_operation_count = 0
    
    while ThroughputMonitor::get_current_time() < test_end_time {
      Thread::sleep(reporting_interval * 1000)  // 休眠报告间隔
      
      let current_time = ThroughputMonitor::get_current_time()
      let current_operation_count = ThreadSafeCounter::get(throughput_stats)
      
      let time_diff = current_time - last_report_time
      let operation_diff = current_operation_count - last_operation_count
      let current_throughput = operation_diff.to_float() / (time_diff.to_float() / 1000.0)
      
      let report = {
        "timestamp": current_time,
        "interval": time_diff,
        "operations": operation_diff,
        "throughput": current_throughput
      }
      
      ThreadSafeArray::push(throughput_reports, report)
      
      last_report_time = current_time
      last_operation_count = current_operation_count
    }
  })
  
  // 等待测试完成
  Thread::sleep(throughput_test_duration * 1000)
  completion_signal := true
  
  // 等待报告线程完成
  Thread::join(report_thread)
  
  // 停止吞吐量监控
  ThroughputMonitor::stop_monitoring(throughput_monitor)
  
  // 分析吞吐量结果
  let reports = ThreadSafeArray::to_array(throughput_reports)
  let total_operations = ThreadSafeCounter::get(throughput_stats)
  let actual_test_duration = ThroughputMonitor::get_current_time() - start_time
  let average_throughput = total_operations.to_float() / (actual_test_duration.to_float() / 1000.0)
  
  // 计算吞吐量统计
  let throughput_values = reports.map(fn(r) { r.throughput })
  let max_throughput = throughput_values.reduce(fn(acc, throughput) { if throughput > acc { throughput } else { acc } }, 0.0)
  let min_throughput = throughput_values.reduce(fn(acc, throughput) { if throughput < acc { throughput } else { acc } }, 999999.0)
  let avg_reported_throughput = throughput_values.reduce(fn(acc, throughput) { acc + throughput }, 0.0) / throughput_values.length()
  
  // 验证吞吐量指标
  assert_true(average_throughput > target_throughput * 0.8)  // 平均吞吐量应达到目标的80%
  assert_true(max_throughput > target_throughput)  // 最大吞吐量应超过目标
  assert_true(min_throughput > target_throughput * 0.5)  // 最小吞吐量应至少达到目标的50%
  assert_true(total_operations > target_throughput * throughput_test_duration * 0.8)  // 总操作数应达到期望的80%
  
  // 获取详细的吞吐量报告
  let throughput_report = ThroughputMonitor::get_throughput_report(throughput_monitor)
  
  // 验证吞吐量报告包含必要的信息
  assert_true(throughput_report.contains_key("total_operations"))
  assert_true(throughput_report.contains_key("test_duration"))
  assert_true(throughput_report.contains_key("average_throughput"))
  assert_true(throughput_report.contains_key("max_throughput"))
  assert_true(throughput_report.contains_key("min_throughput"))
  assert_true(throughput_report.contains_key("producer_threads"))
}
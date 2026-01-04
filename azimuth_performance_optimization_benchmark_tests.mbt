// Azimuth Performance Optimization and Benchmark Test Suite
// 测试性能优化和基准测试，确保系统在高负载下的高效性能

test "属性操作性能基准测试" {
  // 创建性能测试器
  let benchmark = PerformanceBenchmark::new()
  
  // 测试属性创建性能
  let start_time = Time::now()
  for i in 0..=10000 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "key." + i.to_string(), StringValue("value." + i.to_string()))
  }
  let end_time = Time::now()
  let creation_time = end_time - start_time
  
  // 验证创建时间在合理范围内（小于100ms）
  assert_true(creation_time < 100)
  
  // 测试属性访问性能
  let attrs = Attributes::new()
  for i in 0..=10000 {
    Attributes::set(attrs, "key." + i.to_string(), StringValue("value." + i.to_string()))
  }
  
  let start_time = Time::now()
  for i in 0..=10000 {
    Attributes::get(attrs, "key." + i.to_string())
  }
  let end_time = Time::now()
  let access_time = end_time - start_time
  
  // 验证访问时间在合理范围内（小于50ms）
  assert_true(access_time < 50)
  
  // 记录基准测试结果
  Benchmark::record_result(benchmark, "attribute_creation", creation_time)
  Benchmark::record_result(benchmark, "attribute_access", access_time)
}

test "跨度操作性能基准测试" {
  // 创建性能测试器
  let benchmark = PerformanceBenchmark::new()
  
  // 测试跨度创建性能
  let start_time = Time::now()
  for i in 0..=10000 {
    let span_ctx = SpanContext::new("trace_id_" + i.to_string(), "span_id_" + i.to_string(), true, "")
    let span = Span::new("span_" + i.to_string(), Internal, span_ctx)
  }
  let end_time = Time::now()
  let creation_time = end_time - start_time
  
  // 验证创建时间在合理范围内（小于100ms）
  assert_true(creation_time < 100)
  
  // 测试跨度操作性能
  let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
  let span = Span::new("test_span", Internal, span_ctx)
  
  let start_time = Time::now()
  for i in 0..=10000 {
    Span::add_event(span, "event_" + i.to_string(), None)
    Span::set_status(span, Ok, Some("Operation " + i.to_string()))
  }
  let end_time = Time::now()
  let operation_time = end_time - start_time
  
  // 验证操作时间在合理范围内（小于50ms）
  assert_true(operation_time < 50)
  
  // 记录基准测试结果
  Benchmark::record_result(benchmark, "span_creation", creation_time)
  Benchmark::record_result(benchmark, "span_operations", operation_time)
}

test "指标操作性能基准测试" {
  // 创建性能测试器
  let benchmark = PerformanceBenchmark::new()
  
  // 创建指标提供器和计量器
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "benchmark_meter")
  
  // 测试计数器操作性能
  let counter = Meter::create_counter(meter, "benchmark_counter", None, None)
  
  let start_time = Time::now()
  for i in 0..=10000 {
    Counter::add(counter, i.to_float())
  }
  let end_time = Time::now()
  let counter_time = end_time - start_time
  
  // 验证计数器操作时间在合理范围内（小于50ms）
  assert_true(counter_time < 50)
  
  // 测试直方图操作性能
  let histogram = Meter::create_histogram(meter, "benchmark_histogram", None, None)
  
  let start_time = Time::now()
  for i in 0..=10000 {
    Histogram::record(histogram, i.to_float())
  }
  let end_time = Time::now()
  let histogram_time = end_time - start_time
  
  // 验证直方图操作时间在合理范围内（小于50ms）
  assert_true(histogram_time < 50)
  
  // 记录基准测试结果
  Benchmark::record_result(benchmark, "counter_operations", counter_time)
  Benchmark::record_result(benchmark, "histogram_operations", histogram_time)
}

test "日志记录性能基准测试" {
  // 创建性能测试器
  let benchmark = PerformanceBenchmark::new()
  
  // 创建日志提供器和记录器
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "benchmark_logger")
  
  // 测试日志记录性能
  let start_time = Time::now()
  for i in 0..=10000 {
    let log_record = LogRecord::new(Info, "Log message " + i.to_string())
    Logger::emit(logger, log_record)
  }
  let end_time = Time::now()
  let logging_time = end_time - start_time
  
  // 验证日志记录时间在合理范围内（小于100ms）
  assert_true(logging_time < 100)
  
  // 测试带属性的日志记录性能
  let start_time = Time::now()
  for i in 0..=10000 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "log_id", StringValue("log_" + i.to_string()))
    let log_record = LogRecord::new_with_context(
      Info,
      Some("Log message " + i.to_string()),
      Some(attrs),
      Some(Time::now().to_int()),
      None,
      None,
      None,
      None
    )
    Logger::emit(logger, log_record)
  }
  let end_time = Time::now()
  let logging_with_attrs_time = end_time - start_time
  
  // 验证带属性的日志记录时间在合理范围内（小于150ms）
  assert_true(logging_with_attrs_time < 150)
  
  // 记录基准测试结果
  Benchmark::record_result(benchmark, "simple_logging", logging_time)
  Benchmark::record_result(benchmark, "logging_with_attributes", logging_with_attrs_time)
}

test "序列化性能基准测试" {
  // 创建性能测试器
  let benchmark = PerformanceBenchmark::new()
  
  // 创建大型属性集合
  let large_attrs = Attributes::new()
  for i in 0..=1000 {
    Attributes::set(large_attrs, "key." + i.to_string(), StringValue("value." + i.to_string()))
  }
  
  // 测试序列化性能
  let start_time = Time::now()
  for i in 0..=1000 {
    Attributes::serialize(large_attrs)
  }
  let end_time = Time::now()
  let serialization_time = end_time - start_time
  
  // 验证序列化时间在合理范围内（小于100ms）
  assert_true(serialization_time < 100)
  
  // 测试反序列化性能
  let serialized = Attributes::serialize(large_attrs)
  
  let start_time = Time::now()
  for i in 0..=1000 {
    Attributes::deserialize(serialized)
  }
  let end_time = Time::now()
  let deserialization_time = end_time - start_time
  
  // 验证反序列化时间在合理范围内（小于100ms）
  assert_true(deserialization_time < 100)
  
  // 记录基准测试结果
  Benchmark::record_result(benchmark, "serialization", serialization_time)
  Benchmark::record_result(benchmark, "deserialization", deserialization_time)
}

test "内存使用优化测试" {
  // 创建内存监控器
  let memory_monitor = MemoryMonitor::new()
  
  // 记录初始内存使用
  let initial_memory = MemoryMonitor::get_usage(memory_monitor)
  
  // 创建大量对象
  let objects = []
  for i in 0..=10000 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "key", StringValue("value_" + i.to_string()))
    objects = objects + [attrs]
  }
  
  // 记录峰值内存使用
  let peak_memory = MemoryMonitor::get_usage(memory_monitor)
  
  // 验证内存使用在合理范围内（小于100MB）
  assert_true(peak_memory - initial_memory < 100 * 1024 * 1024)
  
  // 清理对象
  let objects = []
  
  // 强制垃圾回收
  MemoryMonitor::force_gc(memory_monitor)
  
  // 记录清理后内存使用
  let cleanup_memory = MemoryMonitor::get_usage(memory_monitor)
  
  // 验证内存已释放（至少释放了80%）
  let memory_freed = peak_memory - cleanup_memory
  let memory_allocated = peak_memory - initial_memory
  assert_true(memory_freed > memory_allocated * 0.8)
}

test "并发性能测试" {
  // 创建并发性能测试器
  let concurrent_benchmark = ConcurrentBenchmark::new()
  
  // 测试并发属性操作
  let start_time = Time::now()
  
  // 模拟并发操作
  let tasks = []
  for i in 0..=10 {
    let task = || {
      let attrs = Attributes::new()
      for j in 0..=1000 {
        Attributes::set(attrs, "key." + j.to_string(), StringValue("value." + j.to_string()))
        Attributes::get(attrs, "key." + j.to_string())
      }
    }
    tasks = tasks + [task]
  }
  
  // 执行并发任务
  ConcurrentBenchmark::execute_tasks(concurrent_benchmark, tasks)
  
  let end_time = Time::now()
  let concurrent_time = end_time - start_time
  
  // 验证并发时间在合理范围内（小于100ms）
  assert_true(concurrent_time < 100)
  
  // 测试并发跨度操作
  let start_time = Time::now()
  
  // 模拟并发跨度操作
  let span_tasks = []
  for i in 0..=10 {
    let task = || {
      let span_ctx = SpanContext::new("trace_id", "span_id_" + i.to_string(), true, "")
      let span = Span::new("span_" + i.to_string(), Internal, span_ctx)
      for j in 0..=100 {
        Span::add_event(span, "event_" + j.to_string(), None)
      }
    }
    span_tasks = span_tasks + [task]
  }
  
  // 执行并发跨度任务
  ConcurrentBenchmark::execute_tasks(concurrent_benchmark, span_tasks)
  
  let end_time = Time::now()
  let concurrent_span_time = end_time - start_time
  
  // 验证并发跨度操作时间在合理范围内（小于100ms）
  assert_true(concurrent_span_time < 100)
}

test "缓存性能优化测试" {
  // 创建缓存
  let cache = Cache::new(1000) // 最大1000个条目
  
  // 测试缓存写入性能
  let start_time = Time::now()
  for i in 0..=1000 {
    Cache::set(cache, "key_" + i.to_string(), "value_" + i.to_string())
  }
  let end_time = Time::now()
  let write_time = end_time - start_time
  
  // 验证缓存写入时间在合理范围内（小于50ms）
  assert_true(write_time < 50)
  
  // 测试缓存读取性能
  let start_time = Time::now()
  for i in 0..=1000 {
    Cache::get(cache, "key_" + i.to_string())
  }
  let end_time = Time::now()
  let read_time = end_time - start_time
  
  // 验证缓存读取时间在合理范围内（小于10ms）
  assert_true(read_time < 10)
  
  // 测试缓存命中率
  let hits = 0
  let misses = 0
  for i in 0..=1000 {
    match Cache::get(cache, "key_" + i.to_string()) {
      Some(_) => hits = hits + 1
      None => misses = misses + 1
    }
  }
  
  // 验证缓存命中率（至少90%）
  let hit_rate = hits.to_float() / (hits + misses).to_float()
  assert_true(hit_rate > 0.9)
}

test "批处理性能优化测试" {
  // 创建批处理器
  let batch_processor = BatchProcessor::new(100) // 批量大小100
  
  // 创建测试数据
  let data = []
  for i in 0..=10000 {
    data = data + ["item_" + i.to_string()]
  }
  
  // 测试批处理性能
  let start_time = Time::now()
  BatchProcessor::process(batch_processor, data, |batch| {
    for item in batch {
      // 模拟处理操作
      let processed = item + "_processed"
    }
  })
  let end_time = Time::now()
  let batch_time = end_time - start_time
  
  // 验证批处理时间在合理范围内（小于100ms）
  assert_true(batch_time < 100)
  
  // 测试非批处理性能
  let start_time = Time::now()
  for item in data {
    // 模拟处理操作
    let processed = item + "_processed"
  }
  let end_time = Time::now()
  let non_batch_time = end_time - start_time
  
  // 验证批处理比非批处理快（至少快20%）
  assert_true(batch_time < non_batch_time * 0.8)
}

test "性能回归测试" {
  // 创建性能基准
  let performance_baseline = PerformanceBaseline::new()
  
  // 设置基准值
  PerformanceBaseline::set_baseline(performance_baseline, "attribute_creation", 50) // 50ms
  PerformanceBaseline::set_baseline(performance_baseline, "attribute_access", 25) // 25ms
  PerformanceBaseline::set_baseline(performance_baseline, "span_creation", 50) // 50ms
  PerformanceBaseline::set_baseline(performance_baseline, "span_operations", 25) // 25ms
  PerformanceBaseline::set_baseline(performance_baseline, "counter_operations", 25) // 25ms
  PerformanceBaseline::set_baseline(performance_baseline, "histogram_operations", 25) // 25ms
  PerformanceBaseline::set_baseline(performance_baseline, "simple_logging", 50) // 50ms
  PerformanceBaseline::set_baseline(performance_baseline, "logging_with_attributes", 75) // 75ms
  PerformanceBaseline::set_baseline(performance_baseline, "serialization", 50) // 50ms
  PerformanceBaseline::set_baseline(performance_baseline, "deserialization", 50) // 50ms
  
  // 测量当前性能
  let current_performance = PerformanceBenchmark::new()
  
  // 测试属性创建性能
  let start_time = Time::now()
  for i in 0..=10000 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "key." + i.to_string(), StringValue("value." + i.to_string()))
  }
  let end_time = Time::now()
  let creation_time = end_time - start_time
  
  // 验证性能没有回归（不超过基准值的20%）
  let creation_baseline = PerformanceBaseline::get_baseline(performance_baseline, "attribute_creation")
  assert_true(creation_time < creation_baseline * 1.2)
  
  // 测试属性访问性能
  let attrs = Attributes::new()
  for i in 0..=10000 {
    Attributes::set(attrs, "key." + i.to_string(), StringValue("value." + i.to_string()))
  }
  
  let start_time = Time::now()
  for i in 0..=10000 {
    Attributes::get(attrs, "key." + i.to_string())
  }
  let end_time = Time::now()
  let access_time = end_time - start_time
  
  // 验证性能没有回归（不超过基准值的20%）
  let access_baseline = PerformanceBaseline::get_baseline(performance_baseline, "attribute_access")
  assert_true(access_time < access_baseline * 1.2)
  
  // 生成性能报告
  let performance_report = PerformanceBaseline::generate_report(performance_baseline, current_performance)
  
  // 验证性能报告包含必要信息
  assert_true(performance_report.contains("attribute_creation"))
  assert_true(performance_report.contains("attribute_access"))
  assert_true(performance_report.contains("baseline"))
  assert_true(performance_report.contains("current"))
  assert_true(performance_report.contains("regression"))
}
// Azimuth 性能基准综合测试用例
// 测试遥测系统的性能基准，包括吞吐量、延迟和资源使用

// 测试1: 追踪系统性能基准
test "追踪系统性能基准测试" {
  // 创建性能基准测试器
  let benchmark = PerformanceBenchmark::new("tracing.performance")
  
  // 创建追踪器
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "benchmark.test")
  
  // 基准测试：span创建性能
  let span_creation_time = Benchmark::measure(benchmark, fn() {
    for i = 0; i < 1000; i = i + 1 {
      let span = Tracer::start_span(tracer, "benchmark.operation." + i.to_string())
      Span::end(span)
    }
  })
  
  // 验证span创建性能（每个span创建应该在1ms内完成）
  let avg_span_creation_time = span_creation_time / 1000.0
  assert_true(avg_span_creation_time < 1.0, "Span creation should be under 1ms")
  
  // 基准测试：带属性的span创建性能
  let attributed_span_creation_time = Benchmark::measure(benchmark, fn() {
    for i = 0; i < 1000; i = i + 1 {
      let span = Tracer::start_span(tracer, "attributed.operation." + i.to_string())
      Span::set_attribute(span, "operation.id", i.to_string())
      Span::set_attribute(span, "operation.type", "benchmark")
      Span::set_attribute(span, "service.name", "test.service")
      Span::add_event(span, "operation.started", [("timestamp", i.to_string())])
      Span::end(span)
    }
  })
  
  let avg_attributed_span_creation_time = attributed_span_creation_time / 1000.0
  assert_true(avg_attributed_span_creation_time < 2.0, "Attributed span creation should be under 2ms")
  
  // 基准测试：嵌套span创建性能
  let nested_span_creation_time = Benchmark::measure(benchmark, fn() {
    for i = 0; i < 100; i = i + 1 {
      let root_span = Tracer::start_span(tracer, "root.operation." + i.to_string())
      
      for j = 0; j < 10; j = j + 1 {
        let child_span = Tracer::start_span_with_parent(tracer, "child.operation." + j.to_string(), root_span)
        Span::set_attribute(child_span, "child.id", j.to_string())
        Span::end(child_span)
      }
      
      Span::end(root_span)
    }
  })
  
  let total_nested_spans = 100 * 11 // 100个root span + 1000个child span
  let avg_nested_span_creation_time = nested_span_creation_time / total_nested_spans.to_float()
  assert_true(avg_nested_span_creation_time < 1.5, "Nested span creation should be under 1.5ms")
  
  // 验证内存使用
  let memory_usage = Benchmark::measure_memory_usage(benchmark, fn() {
    let spans = []
    for i = 0; i < 1000; i = i + 1 {
      let span = Tracer::start_span(tracer, "memory.test.operation." + i.to_string())
      spans.push(span)
    }
    spans
  })
  
  // 每个span应该占用不超过1KB内存
  let memory_per_span = memory_usage / 1000
  assert_true(memory_per_span < 1024, "Each span should use less than 1KB memory")
}

// 测试2: 度量系统性能基准
test "度量系统性能基准测试" {
  // 创建性能基准测试器
  let benchmark = PerformanceBenchmark::new("metrics.performance")
  
  // 创建度量提供者
  let meter_provider = MeterProvider::new()
  let meter = MeterProvider::get_meter(meter_provider, "benchmark.test")
  
  // 基准测试：计数器操作性能
  let counter_creation_time = Benchmark::measure(benchmark, fn() {
    for i = 0; i < 1000; i = i + 1 {
      let counter = Meter::create_counter(meter, "counter." + i.to_string())
      Counter::add(counter, 1.0)
    }
  })
  
  let avg_counter_creation_time = counter_creation_time / 1000.0
  assert_true(avg_counter_creation_time < 0.5, "Counter creation and increment should be under 0.5ms")
  
  // 基准测试：带属性的计数器操作性能
  let attributed_counter_time = Benchmark::measure(benchmark, fn() {
    let counter = Meter::create_counter(meter, "attributed.counter")
    for i = 0; i < 10000; i = i + 1 {
      Counter::add_with_attributes(counter, 1.0, [
        ("operation.type", "benchmark"),
        ("operation.id", i.to_string()),
        ("service.name", "test.service")
      ])
    }
  })
  
  let avg_attributed_counter_time = attributed_counter_time / 10000.0
  assert_true(avg_attributed_counter_time < 0.1, "Attributed counter increment should be under 0.1ms")
  
  // 基准测试：仪表操作性能
  let gauge_creation_time = Benchmark::measure(benchmark, fn() {
    for i = 0; i < 1000; i = i + 1 {
      let gauge = Meter::create_gauge(meter, "gauge." + i.to_string())
      Gauge::set(gauge, i.to_float())
    }
  })
  
  let avg_gauge_creation_time = gauge_creation_time / 1000.0
  assert_true(avg_gauge_creation_time < 0.5, "Gauge creation and set should be under 0.5ms")
  
  // 基准测试：直方图操作性能
  let histogram_time = Benchmark::measure(benchmark, fn() {
    let histogram = Meter::create_histogram(meter, "benchmark.histogram")
    for i = 0; i < 10000; i = i + 1 {
      Histogram::record(histogram, (i % 1000).to_float())
    }
  })
  
  let avg_histogram_time = histogram_time / 10000.0
  assert_true(avg_histogram_time < 0.2, "Histogram record should be under 0.2ms")
  
  // 验证度量数据查询性能
  let query_time = Benchmark::measure(benchmark, fn() {
    for i = 0; i < 1000; i = i + 1 {
      let metrics = MeterProvider::get_all_metrics(meter_provider)
      let filtered_metrics = metrics.filter(fn(m) { Metric::name(m).contains("benchmark") })
      assert_true(filtered_metrics.length() > 0)
    }
  })
  
  let avg_query_time = query_time / 1000.0
  assert_true(avg_query_time < 5.0, "Metrics query should be under 5ms")
}

// 测试3: 日志系统性能基准
test "日志系统性能基准测试" {
  // 创建性能基准测试器
  let benchmark = PerformanceBenchmark::new("logging.performance")
  
  // 创建日志记录器
  let logger = Logger::new("benchmark.test")
  
  // 基准测试：简单日志记录性能
  let simple_logging_time = Benchmark::measure(benchmark, fn() {
    for i = 0; i < 10000; i = i + 1 {
      Logger::info(logger, "Simple log message " + i.to_string(), [])
    }
  })
  
  let avg_simple_logging_time = simple_logging_time / 10000.0
  assert_true(avg_simple_logging_time < 0.1, "Simple logging should be under 0.1ms")
  
  // 基准测试：带属性的日志记录性能
  let attributed_logging_time = Benchmark::measure(benchmark, fn() {
    for i = 0; i < 10000; i = i + 1 {
      Logger::info(logger, "Attributed log message " + i.to_string(), [
        ("log.id", i.to_string()),
        ("operation.type", "benchmark"),
        ("service.name", "test.service"),
        ("timestamp", i.to_string())
      ])
    }
  })
  
  let avg_attributed_logging_time = attributed_logging_time / 10000.0
  assert_true(avg_attributed_logging_time < 0.2, "Attributed logging should be under 0.2ms")
  
  // 基准测试：结构化日志记录性能
  let structured_logging_time = Benchmark::measure(benchmark, fn() {
    for i = 0; i < 5000; i = i + 1 {
      let structured_data = StructuredData::new()
      StructuredData::set_field(structured_data, "operation.id", i.to_string())
      StructuredData::set_field(structured_data, "operation.type", "benchmark")
      StructuredData::set_field(structured_data, "duration", (i % 1000).to_string())
      StructuredData::set_field(structured_data, "success", (i % 2 == 0).to_string())
      
      Logger::info_structured(logger, "Structured log message", structured_data)
    }
  })
  
  let avg_structured_logging_time = structured_logging_time / 5000.0
  assert_true(avg_structured_logging_time < 0.5, "Structured logging should be under 0.5ms")
  
  // 基准测试：日志查询性能
  let log_query_time = Benchmark::measure(benchmark, fn() {
    for i = 0; i < 1000; i = i + 1 {
      let logs = Logger::get_logs_by_level(logger, LogLevel::Info)
      let filtered_logs = logs.filter(fn(l) { LogRecord::message(l).contains("benchmark") })
      assert_true(filtered_logs.length() > 0)
    }
  })
  
  let avg_log_query_time = log_query_time / 1000.0
  assert_true(avg_log_query_time < 10.0, "Log query should be under 10ms")
}

// 测试4: 序列化性能基准
test "序列化性能基准测试" {
  // 创建性能基准测试器
  let benchmark = PerformanceBenchmark::new("serialization.performance")
  
  // 创建测试数据
  let test_spans = []
  for i = 0; i < 1000; i = i + 1 {
    let span = SpanData::new("test.operation." + i.to_string(), 1000 + i, 1100 + i, [
      ("operation.id", i.to_string()),
      ("service.name", "test.service"),
      ("operation.type", "benchmark")
    ])
    test_spans.push(span)
  }
  
  // 基准测试：JSON序列化性能
  let json_serialization_time = Benchmark::measure(benchmark, fn() {
    for span in test_spans {
      let json = JsonSerializer::serialize(span)
      assert_true(json.length() > 0)
    }
  })
  
  let avg_json_serialization_time = json_serialization_time / 1000.0
  assert_true(avg_json_serialization_time < 1.0, "JSON serialization should be under 1ms")
  
  // 基准测试：JSON反序列化性能
  let json_deserialization_time = Benchmark::measure(benchmark, fn() {
    for span in test_spans {
      let json = JsonSerializer::serialize(span)
      let deserialized_span = JsonSerializer::deserialize_span(json)
      assert_eq(SpanData::name(deserialized_span), SpanData::name(span))
    }
  })
  
  let avg_json_deserialization_time = json_deserialization_time / 1000.0
  assert_true(avg_json_deserialization_time < 1.0, "JSON deserialization should be under 1ms")
  
  // 基准测试：二进制序列化性能
  let binary_serialization_time = Benchmark::measure(benchmark, fn() {
    for span in test_spans {
      let binary = BinarySerializer::serialize(span)
      assert_true(binary.length() > 0)
    }
  })
  
  let avg_binary_serialization_time = binary_serialization_time / 1000.0
  assert_true(avg_binary_serialization_time < 0.5, "Binary serialization should be under 0.5ms")
  
  // 基准测试：二进制反序列化性能
  let binary_deserialization_time = Benchmark::measure(benchmark, fn() {
    for span in test_spans {
      let binary = BinarySerializer::serialize(span)
      let deserialized_span = BinarySerializer::deserialize_span(binary)
      assert_eq(SpanData::name(deserialized_span), SpanData::name(span))
    }
  })
  
  let avg_binary_deserialization_time = binary_deserialization_time / 1000.0
  assert_true(avg_binary_deserialization_time < 0.5, "Binary deserialization should be under 0.5ms")
  
  // 比较序列化大小
  let json_size = JsonSerializer::serialize(test_spans[0]).length()
  let binary_size = BinarySerializer::serialize(test_spans[0]).length()
  
  // 二进制序列化应该更紧凑
  assert_true(binary_size < json_size, "Binary serialization should be more compact than JSON")
}

// 测试5: 并发性能基准
test "并发性能基准测试" {
  // 创建性能基准测试器
  let benchmark = PerformanceBenchmark::new("concurrency.performance")
  
  // 创建并发追踪器
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrency.test")
  
  // 基准测试：并发span创建性能
  let concurrent_span_time = Benchmark::measure_concurrent(benchmark, 10, fn() {
    for i = 0; i < 1000; i = i + 1 {
      let span = Tracer::start_span(tracer, "concurrent.operation." + i.to_string())
      Span::set_attribute(span, "thread.id", Thread::current_id().to_string())
      Span::end(span)
    }
  })
  
  let total_concurrent_spans = 10 * 1000
  let avg_concurrent_span_time = concurrent_span_time / total_concurrent_spans.to_float()
  assert_true(avg_concurrent_span_time < 2.0, "Concurrent span creation should be under 2ms")
  
  // 创建并发度量器
  let meter_provider = MeterProvider::new()
  let meter = MeterProvider::get_meter(meter_provider, "concurrency.test")
  
  // 基准测试：并发度量操作性能
  let concurrent_metrics_time = Benchmark::measure_concurrent(benchmark, 10, fn() {
    let counter = Meter::create_counter(meter, "concurrent.counter")
    for i = 0; i < 1000; i = i + 1 {
      Counter::add_with_attributes(counter, 1.0, [
        ("thread.id", Thread::current_id().to_string()),
        ("operation.id", i.to_string())
      ])
    }
  })
  
  let avg_concurrent_metrics_time = concurrent_metrics_time / total_concurrent_spans.to_float()
  assert_true(avg_concurrent_metrics_time < 0.2, "Concurrent metrics operation should be under 0.2ms")
  
  // 创建并发日志记录器
  let logger = Logger::new("concurrency.test")
  
  // 基准测试：并发日志记录性能
  let concurrent_logging_time = Benchmark::measure_concurrent(benchmark, 10, fn() {
    for i = 0; i < 1000; i = i + 1 {
      Logger::info(logger, "Concurrent log message " + i.to_string(), [
        ("thread.id", Thread::current_id().to_string()),
        ("operation.id", i.to_string())
      ])
    }
  })
  
  let avg_concurrent_logging_time = concurrent_logging_time / total_concurrent_spans.to_float()
  assert_true(avg_concurrent_logging_time < 0.3, "Concurrent logging should be under 0.3ms")
  
  // 验证并发安全性
  let all_spans = TracerProvider::get_all_spans(tracer_provider)
  let all_metrics = MeterProvider::get_all_metrics(meter_provider)
  let all_logs = Logger::get_all_logs(logger)
  
  assert_eq(all_spans.length(), total_concurrent_spans)
  assert_true(all_metrics.length() > 0)
  assert_eq(all_logs.length(), total_concurrent_spans)
}

// 测试6: 资源使用基准
test "资源使用基准测试" {
  // 创建资源监控器
  let resource_monitor = ResourceMonitor::new()
  
  // 基准测试：内存使用模式
  let initial_memory = ResourceMonitor::get_memory_usage(resource_monitor)
  
  // 创建大量spans
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "memory.test")
  
  let spans = []
  for i = 0; i < 10000; i = i + 1 {
    let span = Tracer::start_span(tracer, "memory.test.operation." + i.to_string())
    Span::set_attribute(span, "operation.id", i.to_string())
    Span::set_attribute(span, "large.data", "x".repeat(100)) // 添加一些数据增加内存使用
    spans.push(span)
  }
  
  let peak_memory = ResourceMonitor::get_memory_usage(resource_monitor)
  let memory_per_span = (peak_memory - initial_memory) / 10000
  
  // 每个span应该占用不超过2KB内存
  assert_true(memory_per_span < 2048, "Each span should use less than 2KB memory")
  
  // 清理spans
  for span in spans {
    Span::end(span)
  }
  
  // 强制垃圾回收
  ResourceMonitor::force_gc(resource_monitor)
  
  let final_memory = ResourceMonitor::get_memory_usage(resource_monitor)
  let memory_recovered = peak_memory - final_memory
  
  // 至少应该回收80%的内存
  let memory_recovery_rate = memory_recovered.to_float() / (peak_memory - initial_memory).to_float()
  assert_true(memory_recovery_rate > 0.8, "Should recover at least 80% of memory")
  
  // 基准测试：CPU使用模式
  let initial_cpu_time = ResourceMonitor::get_cpu_time(resource_monitor)
  
  // 执行CPU密集型操作
  for i = 0; i < 100000; i = i + 1 {
    let span = Tracer::start_span(tracer, "cpu.intensive.operation." + i.to_string())
    Span::set_attribute(span, "operation.id", i.to_string())
    
    // 模拟一些计算
    let result = 0
    for j = 0; j < 10; j = j + 1 {
      result = result + j
    }
    
    Span::set_attribute(span, "computed.result", result.to_string())
    Span::end(span)
  }
  
  let final_cpu_time = ResourceMonitor::get_cpu_time(resource_monitor)
  let cpu_time_per_operation = (final_cpu_time - initial_cpu_time) / 100000
  
  // 每个操作应该使用不超过0.1ms的CPU时间
  assert_true(cpu_time_per_operation < 0.1, "Each operation should use less than 0.1ms CPU time")
  
  // 基准测试：文件句柄使用
  let initial_handles = ResourceMonitor::get_file_handle_count(resource_monitor)
  
  // 创建多个日志记录器
  let loggers = []
  for i = 0; i < 100; i = i + 1 {
    let logger = Logger::new("handle.test." + i.to_string())
    loggers.push(logger)
    
    // 记录一些日志
    for j = 0; j < 10; j = j + 1 {
      Logger::info(logger, "Test log message " + j.to_string(), [])
    }
  }
  
  let peak_handles = ResourceMonitor::get_file_handle_count(resource_monitor)
  let handles_per_logger = (peak_handles - initial_handles) / 100
  
  // 每个日志记录器应该使用不超过5个文件句柄
  assert_true(handles_per_logger < 5, "Each logger should use less than 5 file handles")
  
  // 清理日志记录器
  for logger in loggers {
    Logger::close(logger)
  }
  
  let final_handles = ResourceMonitor::get_file_handle_count(resource_monitor)
  
  // 文件句柄应该被正确释放
  assert_true(final_handles <= initial_handles + 10, "File handles should be properly released")
}
// 性能基准测试用例

test "span_creation_performance_benchmark" {
  // Span创建性能基准测试
  
  let tracer = azimuth::telemetry::api::trace::TracerProvider::global().get_tracer("benchmark-tracer")
  let ctx = azimuth::telemetry::api::context::Context::empty()
  
  // 基准测试：创建1000个Span
  let start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  for i = 0; i < 1000; i = i + 1 {
    let (_, span) = tracer.start_span(
      ctx,
      "benchmark.span." + i.to_string(),
      azimuth::telemetry::api::trace::SpanKind::Server
    )
    span.end()
  }
  
  let end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let total_duration = end_time - start_time
  let avg_duration_per_span = total_duration.to_float() / 1000.0
  
  // 性能断言：每个Span创建和结束应该在合理时间内完成
  assert_eq(avg_duration_per_span < 100_000.0, true)  // 小于100微秒每个Span
  
  // 总时间应该在合理范围内
  assert_eq(total_duration < 100_000_000, true)  // 小于100毫秒总共
}

test "span_attribute_performance_benchmark" {
  // Span属性设置性能基准测试
  
  let tracer = azimuth::telemetry::api::trace::TracerProvider::global().get_tracer("attribute-benchmark-tracer")
  let ctx = azimuth::telemetry::api::context::Context::empty()
  
  let (_, span) = tracer.start_span(
    ctx,
    "attribute.benchmark.span",
    azimuth::telemetry::api::trace::SpanKind::Server
  )
  
  // 基准测试：设置100个属性
  let start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  for i = 0; i < 100; i = i + 1 {
    span.set_attribute(
      "attr." + i.to_string(),
      azimuth::telemetry::api::common::AttributeValue::String("value." + i.to_string())
    )
  }
  
  let end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let total_duration = end_time - start_time
  let avg_duration_per_attribute = total_duration.to_float() / 100.0
  
  // 性能断言：每个属性设置应该在合理时间内完成
  assert_eq(avg_duration_per_attribute < 10_000.0, true)  // 小于10微秒每个属性
  
  span.end()
}

test "context_propagation_performance_benchmark" {
  // 上下文传播性能基准测试
  
  let propagator = azimuth::telemetry::api::propagation::W3CTraceContextPropagator::new()
  
  // 创建包含丰富上下文的Context
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_id = "00f067aa0ba902b7"
  let span_context = azimuth::telemetry::api::trace::SpanContext::new(trace_id, span_id, true, false)
  
  let ctx = azimuth::telemetry::api::context::Context::empty()
    .with_value(azimuth::telemetry::api::context::create_key("span_context"), span_context)
    .with_value(azimuth::telemetry::api::context::create_key("user_id"), "user123")
    .with_value(azimuth::telemetry::api::context::create_key("tenant_id"), "tenant456")
    .with_value(azimuth::telemetry::api::context::create_key("request_id"), "req789")
  
  // 基准测试：1000次注入操作
  let start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  for i = 0; i < 1000; i = i + 1 {
    let carrier = azimuth::telemetry::api::propagation::TextMapCarrier::new()
    propagator.inject(ctx, carrier)
  }
  
  let end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let total_duration = end_time - start_time
  let avg_duration_per_inject = total_duration.to_float() / 1000.0
  
  // 性能断言：每次注入应该在合理时间内完成
  assert_eq(avg_duration_per_inject < 50_000.0, true)  // 小于50微秒每次注入
  
  // 基准测试：1000次提取操作
  let carrier = azimuth::telemetry::api::propagation::TextMapCarrier::new()
  propagator.inject(ctx, carrier)
  
  let extract_start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  for i = 0; i < 1000; i = i + 1 {
    propagator.extract(carrier)
  }
  
  let extract_end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let extract_total_duration = extract_end_time - extract_start_time
  let avg_duration_per_extract = extract_total_duration.to_float() / 1000.0
  
  // 性能断言：每次提取应该在合理时间内完成
  assert_eq(avg_duration_per_extract < 50_000.0, true)  // 小于50微秒每次提取
}

test "metric_recording_performance_benchmark" {
  // 指标记录性能基准测试
  
  let meter = azimuth::telemetry::api::metrics::MeterProvider::global().get_meter("benchmark-meter")
  
  let counter = meter.create_counter("benchmark.counter", "Benchmark counter", "operations")
  let histogram = meter.create_histogram("benchmark.histogram", "Benchmark histogram", "ms")
  let gauge = meter.create_gauge("benchmark.gauge", "Benchmark gauge", "bytes")
  
  let attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("operation", azimuth::telemetry::api::common::AttributeValue::String("benchmark"))
    .with("service", azimuth::telemetry::api::common::AttributeValue::String("test-service"))
  
  // 基准测试：计数器记录
  let counter_start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  for i = 0; i < 10000; i = i + 1 {
    counter.add(1, attrs)
  }
  
  let counter_end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let counter_duration = counter_end_time - counter_start_time
  let avg_counter_duration = counter_duration.to_float() / 10000.0
  
  // 性能断言：每次计数器记录应该很快
  assert_eq(avg_counter_duration < 5_000.0, true)  // 小于5微秒每次记录
  
  // 基准测试：直方图记录
  let histogram_start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  for i = 0; i < 10000; i = i + 1 {
    histogram.record(i.to_float(), attrs)
  }
  
  let histogram_end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let histogram_duration = histogram_end_time - histogram_start_time
  let avg_histogram_duration = histogram_duration.to_float() / 10000.0
  
  // 性能断言：每次直方图记录应该合理
  assert_eq(avg_histogram_duration < 10_000.0, true)  // 小于10微秒每次记录
  
  // 基准测试：仪表记录
  let gauge_start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  for i = 0; i < 10000; i = i + 1 {
    gauge.record(i.to_int64(), attrs)
  }
  
  let gauge_end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let gauge_duration = gauge_end_time - gauge_start_time
  let avg_gauge_duration = gauge_duration.to_float() / 10000.0
  
  // 性能断言：每次仪表记录应该很快
  assert_eq(avg_gauge_duration < 5_000.0, true)  // 小于5微秒每次记录
}

test "log_emission_performance_benchmark" {
  // 日志发射性能基准测试
  
  let logger = azimuth::telemetry::api::logs::LoggerProvider::global().get_logger("benchmark-logger")
  
  let attrs = azimuth::telemetry::api::common::Attributes::empty()
    .with("component", azimuth::telemetry::api::common::AttributeValue::String("benchmark"))
    .with("operation", azimuth::telemetry::api::common::AttributeValue::String("test"))
  
  // 基准测试：单个日志发射
  let single_start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  for i = 0; i < 10000; i = i + 1 {
    let log_record = azimuth::telemetry::api::logs::LogRecord::new(
      timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
      observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
      severity_number=azimuth::telemetry::api::logs::SeverityNumber::Info,
      severity_text="INFO",
      body="Benchmark log message " + i.to_string(),
      attributes=attrs,
      trace_id=None,
      span_id=None,
      trace_flags=None
    )
    
    logger.emit(log_record)
  }
  
  let single_end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let single_duration = single_end_time - single_start_time
  let avg_single_duration = single_duration.to_float() / 10000.0
  
  // 性能断言：每次日志发射应该合理
  assert_eq(avg_single_duration < 20_000.0, true)  // 小于20微秒每次日志
  
  // 基准测试：批量日志发射
  let batch_start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  let mut batch_logs = []
  for i = 0; i < 1000; i = i + 1 {
    let log_record = azimuth::telemetry::api::logs::LogRecord::new(
      timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
      observed_timestamp=azimuth::telemetry::sdk::platform::time::current_unix_nanos(),
      severity_number=azimuth::telemetry::api::logs::SeverityNumber::Info,
      severity_text="INFO",
      body="Batch benchmark log " + i.to_string(),
      attributes=attrs,
      trace_id=None,
      span_id=None,
      trace_flags=None
    )
    
    batch_logs.push(log_record)
  }
  
  logger.emit_batch(batch_logs)
  
  let batch_end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let batch_duration = batch_end_time - batch_start_time
  let avg_batch_duration = batch_duration.to_float() / 1000.0
  
  // 性能断言：批量操作应该更高效
  assert_eq(avg_batch_duration < avg_single_duration, true)
}

test "attribute_operations_performance_benchmark" {
  // 属性操作性能基准测试
  
  // 基准测试：属性创建
  let create_start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  let mut attrs_list = []
  for i = 0; i < 1000; i = i + 1 {
    let attrs = azimuth::telemetry::api::common::Attributes::empty()
      .with("attr1", azimuth::telemetry::api::common::AttributeValue::String("value1"))
      .with("attr2", azimuth::telemetry::api::common::AttributeValue::Int64(i))
      .with("attr3", azimuth::telemetry::api::common::AttributeValue::Bool(i % 2 == 0))
      .with("attr4", azimuth::telemetry::api::common::AttributeValue::Float64(i.to_float() * 1.5))
    
    attrs_list.push(attrs)
  }
  
  let create_end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let create_duration = create_end_time - create_start_time
  let avg_create_duration = create_duration.to_float() / 1000.0
  
  // 性能断言：属性创建应该合理
  assert_eq(avg_create_duration < 50_000.0, true)  // 小于50微秒每次创建
  
  // 基准测试：属性查找
  let lookup_start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  for i = 0; i < 1000; i = i + 1 {
    let attrs = attrs_list[i]
    attrs.get("attr1")
    attrs.get("attr2")
    attrs.get("attr3")
    attrs.get("attr4")
    attrs.get("nonexistent")  // 不存在的键
  }
  
  let lookup_end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let lookup_duration = lookup_end_time - lookup_start_time
  let avg_lookup_duration = lookup_duration.to_float() / 5000.0  // 5次查找每个属性集合
  
  // 性能断言：属性查找应该很快
  assert_eq(avg_lookup_duration < 5_000.0, true)  // 小于5微秒每次查找
  
  // 基准测试：属性合并
  let merge_start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  for i = 0; i < 500; i = i + 1 {
    let attrs1 = attrs_list[i * 2]
    let attrs2 = attrs_list[i * 2 + 1]
    attrs1.merge(attrs2)
  }
  
  let merge_end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let merge_duration = merge_end_time - merge_start_time
  let avg_merge_duration = merge_duration.to_float() / 500.0
  
  // 性能断言：属性合并应该合理
  assert_eq(avg_merge_duration < 100_000.0, true)  // 小于100微秒每次合并
}

test "sampling_performance_benchmark" {
  // 采样性能基准测试
  
  let samplers = [
    azimuth::telemetry::sdk::trace::AlwaysOnSampler::new(),
    azimuth::telemetry::sdk::trace::AlwaysOffSampler::new(),
    azimuth::telemetry::sdk::trace::TraceIdRatioBasedSampler::new(0.1),
    azimuth::telemetry::sdk::trace::TraceIdRatioBasedSampler::new(0.5),
    azimuth::telemetry::sdk::trace::TraceIdRatioBasedSampler::new(0.9)
  ]
  
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_name = "benchmark.span"
  let span_kind = azimuth::telemetry::api::trace::SpanKind::Server
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
  let parent_context = None
  
  for sampler in samplers {
    let sampler_name = match sampler {
      AlwaysOnSampler(_) => "AlwaysOn",
      AlwaysOffSampler(_) => "AlwaysOff",
      TraceIdRatioBasedSampler(_) => "TraceIdRatio",
      _ => "Unknown"
    }
    
    // 基准测试：10000次采样决策
    let start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
    
    for i = 0; i < 10000; i = i + 1 {
      sampler.should_sample(
        parent_context,
        trace_id + i.to_string(),
        span_name,
        span_kind,
        attributes
      )
    }
    
    let end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
    let duration = end_time - start_time
    let avg_duration = duration.to_float() / 10000.0
    
    // 性能断言：采样决策应该很快
    assert_eq(avg_duration < 10_000.0, true)  // 小于10微秒每次决策
  }
}

test "serialization_performance_benchmark" {
  // 序列化性能基准测试
  
  // 创建复杂的数据结构
  let complex_resource = azimuth::telemetry::api::common::Resource::builder()
    .with("service.name", azimuth::telemetry::api::common::AttributeValue::String("complex-service"))
    .with("service.version", azimuth::telemetry::api::common::AttributeValue::String("1.2.3"))
    .with("deployment.environment", azimuth::telemetry::api::common::AttributeValue::String("production"))
    .with("host.name", azimuth::telemetry::api::common::AttributeValue::String("prod-server-01"))
    .with("cloud.provider", azimuth::telemetry::api::common::AttributeValue::String("aws"))
    .with("cloud.region", azimuth::telemetry::api::common::AttributeValue::String("us-west-2"))
    .build()
  
  // 基准测试：资源序列化
  let serialize_start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  for i = 0; i < 1000; i = i + 1 {
    let serialized = complex_resource.to_json()
    // 防止编译器优化掉序列化操作
    assert_eq(serialized.length() > 0, true)
  }
  
  let serialize_end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let serialize_duration = serialize_end_time - serialize_start_time
  let avg_serialize_duration = serialize_duration.to_float() / 1000.0
  
  // 性能断言：序列化应该合理
  assert_eq(avg_serialize_duration < 100_000.0, true)  // 小于100微秒每次序列化
  
  // 基准测试：资源反序列化
  let serialized = complex_resource.to_json()
  
  let deserialize_start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  for i = 0; i < 1000; i = i + 1 {
    let deserialized = azimuth::telemetry::api::common::Resource::from_json(serialized)
    // 防止编译器优化掉反序列化操作
    assert_eq(deserialized.attributes.size() > 0, true)
  }
  
  let deserialize_end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let deserialize_duration = deserialize_end_time - deserialize_start_time
  let avg_deserialize_duration = deserialize_duration.to_float() / 1000.0
  
  // 性能断言：反序列化应该合理
  assert_eq(avg_deserialize_duration < 100_000.0, true)  // 小于100微秒每次反序列化
}

test "memory_allocation_benchmark" {
  // 内存分配基准测试
  
  // 测试大量Span创建的内存使用
  let initial_memory = azimuth::telemetry::sdk::platform::memory::get_allocated_bytes()
  
  let tracer = azimuth::telemetry::api::trace::TracerProvider::global().get_tracer("memory-benchmark-tracer")
  let ctx = azimuth::telemetry::api::context::Context::empty()
  
  let spans = []
  
  // 创建10000个Span
  for i = 0; i < 10000; i = i + 1 {
    let (_, span) = tracer.start_span(
      ctx,
      "memory.benchmark.span." + i.to_string(),
      azimuth::telemetry::api::trace::SpanKind::Server
    )
    
    // 为每个Span添加一些属性
    span.set_attribute("iteration", azimuth::telemetry::api::common::AttributeValue::Int64(i))
    span.set_attribute("operation", azimuth::telemetry::api::common::AttributeValue::String("memory_test"))
    
    spans.push(span)
  }
  
  let after_creation_memory = azimuth::telemetry::sdk::platform::memory::get_allocated_bytes()
  let memory_per_span = (after_creation_memory - initial_memory).to_float() / 10000.0
  
  // 内存断言：每个Span的内存使用应该合理
  assert_eq(memory_per_span < 10_000.0, true)  // 小于10KB每个Span
  
  // 结束所有Span并检查内存回收
  for span in spans {
    span.end()
  }
  
  // 强制垃圾回收（如果支持）
  azimuth::telemetry::sdk::platform::memory::force_gc()
  
  let after_cleanup_memory = azimuth::telemetry::sdk::platform::memory::get_allocated_bytes()
  let memory_recovered = after_creation_memory - after_cleanup_memory
  
  // 内存回收断言：应该回收大部分内存
  assert_eq(memory_recovered > (after_creation_memory - initial_memory) * 0.7, true)  // 至少回收70%
}

test "concurrent_performance_benchmark" {
  // 并发性能基准测试
  
  let tracer = azimuth::telemetry::api::trace::TracerProvider::global().get_tracer("concurrent-benchmark-tracer")
  let ctx = azimuth::telemetry::api::context::Context::empty()
  
  // 模拟10个并发线程，每个创建1000个Span
  let num_threads = 10
  let spans_per_thread = 1000
  
  let start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  // 使用任务组进行并发操作
  let task_group = azimuth::telemetry::sdk::platform::async::TaskGroup::new()
  
  for thread_id = 0; thread_id < num_threads; thread_id = thread_id + 1 {
    task_group.spawn(async {
      for i = 0; i < spans_per_thread; i = i + 1 {
        let (_, span) = tracer.start_span(
          ctx,
          "concurrent.span." + thread_id.to_string() + "." + i.to_string(),
          azimuth::telemetry::api::trace::SpanKind::Server
        )
        
        span.set_attribute("thread_id", azimuth::telemetry::api::common::AttributeValue::Int64(thread_id))
        span.set_attribute("iteration", azimuth::telemetry::api::common::AttributeValue::Int64(i))
        
        span.end()
      }
    })
  }
  
  task_group.wait_all()
  
  let end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let total_duration = end_time - start_time
  let total_spans = num_threads * spans_per_thread
  let avg_duration_per_span = total_duration.to_float() / total_spans.to_float()
  
  // 并发性能断言：并发情况下每个Span的处理时间应该仍然合理
  assert_eq(avg_duration_per_span < 200_000.0, true)  // 小于200微秒每个Span（并发情况下会慢一些）
  
  // 并发效率断言：总时间应该显著少于串行执行时间
  let serial_estimate = total_spans.to_float() * 100_000.0  // 假设串行每个Span需要100微秒
  assert_eq(total_duration.to_float() < serial_estimate * 0.8, true)  // 并发应该至少快20%
}
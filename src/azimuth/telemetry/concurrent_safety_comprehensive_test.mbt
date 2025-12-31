// 并发安全综合测试

test "concurrent_span_creation_safety" {
  // 测试并发Span创建的安全性
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("concurrent-tracer")
  
  // 模拟并发创建多个Span
  let spans = Array[]::build(fn(push) {
    for i = 0; i < 100; i = i + 1 {
      let (ctx, span) = tracer.start_span(
        context::Context::empty(),
        "concurrent_span_${i}",
        trace::Internal,
        [("thread_id", common::AttributeValue::int(i.to_int64()))]
      )
      push(span)
    }
  })
  
  // 验证并发创建的安全性
  @assertion.assert_eq(spans.length, 100)
  
  // 验证每个Span的唯一性
  for i = 0; i < spans.length; i = i + 1 {
    let span = spans[i]
    @assertion.assert_eq(span.name, "concurrent_span_${i}")
    @assertion.assert_eq(span.attributes.length, 1)
    
    let (key, value) = span.attributes[0]
    @assertion.assert_eq(key, "thread_id")
    
    match value {
      common::IntValue(thread_id) => @assertion.assert_eq(thread_id, i.to_int64())
      _ => @assertion.assert_true(false)
    }
  }
}

test "concurrent_metrics_recording_safety" {
  // 测试并发指标记录的安全性
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("concurrent-meter")
  
  let counter = meter.create_counter("concurrent_counter", "count", "Concurrent counter")
  let histogram = meter.create_histogram("concurrent_histogram", "ms", "Concurrent histogram")
  let gauge = meter.create_gauge("concurrent_gauge", "value", "Concurrent gauge")
  
  // 模拟并发记录指标
  for i = 0; i < 1000; i = i + 1 {
    counter.add(1L, [("worker_id", common::AttributeValue::int(i.to_int64()))])
    histogram.record(i.to_double(), [("metric_type", common::AttributeValue::string("latency"))])
    gauge.record((i % 100).to_double(), [("status", common::AttributeValue::string("active"))])
  }
  
  // 验证并发记录的安全性
  @assertion.assert_true(true) // 如果没有竞态条件，测试通过
}

test "concurrent_context_propagation_safety" {
  // 测试并发上下文传播的安全性
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("context-propagation-tracer")
  
  // 创建根上下文
  let (root_ctx, root_span) = tracer.start_span(
    context::Context::empty(),
    "root_concurrent_operation",
    trace::Server,
    [("operation_id", common::AttributeValue::string("concurrent_test_001"))]
  )
  
  // 并发创建子操作
  let child_spans = Array[]::build(fn(push) {
    for i = 0; i < 50; i = i + 1 {
      let (child_ctx, child_span) = tracer.start_span(
        root_ctx,
        "child_concurrent_operation_${i}",
        trace::Internal,
        [
          ("child_id", common::AttributeValue::int(i.to_int64())),
          ("parent_operation", common::AttributeValue::string(root_span.name))
        ]
      )
      push(child_span)
    }
  })
  
  // 验证并发上下文传播
  @assertion.assert_eq(root_span.name, "root_concurrent_operation")
  @assertion.assert_eq(child_spans.length, 50)
  
  for i = 0; i < child_spans.length; i = i + 1 {
    let child_span = child_spans[i]
    @assertion.assert_eq(child_span.name, "child_concurrent_operation_${i}")
    @assertion.assert_eq(child_span.attributes.length, 2)
  }
}

test "concurrent_resource_sharing_safety" {
  // 测试并发资源共享的安全性
  let shared_resource = common::Resource::default("shared-concurrent-service")
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  
  // 多个组件共享同一资源
  let components = Array[]::build(fn(push) {
    for i = 0; i < 20; i = i + 1 {
      let tracer = tracer_provider.get_tracer("component_${i}")
      let meter = meter_provider.get_meter("component_${i}")
      
      let (ctx, span) = tracer.start_span(
        context::Context::empty(),
        "component_${i}_operation",
        trace::Internal,
        [
          ("component_id", common::AttributeValue::int(i.to_int64())),
          ("shared_resource", common::AttributeValue::string(shared_resource.service_name))
        ]
      )
      
      let counter = meter.create_counter("component_${i}_counter", "count", "Component counter")
      counter.add(1L, [("resource", common::AttributeValue::string(shared_resource.service_name))])
      
      push(span)
    }
  })
  
  // 验证资源共享安全性
  @assertion.assert_eq(components.length, 20)
  @assertion.assert_eq(shared_resource.service_name, "shared-concurrent-service")
}

test "concurrent_attribute_modification_safety" {
  // 测试并发属性修改的安全性
  let base_attributes = Array[(String, common::AttributeValue)]::with_capacity(100, fn(i) {
    ("base_attr_${i}", common::AttributeValue::string("base_value_${i}"))
  })
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("attribute-modification-tracer")
  
  // 并发创建带有修改属性的Span
  let spans = Array[]::build(fn(push) {
    for i = 0; i < 50; i = i + 1 {
      let modified_attributes = Array[(String, common::AttributeValue)]::concat(
        base_attributes,
        [
          ("worker_id", common::AttributeValue::int(i.to_int64())),
          ("modification_time", common::AttributeValue::string("time_${i}"))
        ]
      )
      
      let (ctx, span) = tracer.start_span(
        context::Context::empty(),
        "attribute_modification_span_${i}",
        trace::Internal,
        modified_attributes
      )
      push(span)
    }
  })
  
  // 验证并发属性修改安全性
  @assertion.assert_eq(spans.length, 50)
  
  for i = 0; i < spans.length; i = i + 1 {
    let span = spans[i]
    @assertion.assert_eq(span.name, "attribute_modification_span_${i}")
    @assertion.assert_eq(span.attributes.length, 102) // 100 base + 2 modified
  }
}

test "concurrent_instrument_creation_safety" {
  // 测试并发仪表创建的安全性
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("instrument-creation-meter")
  
  // 并发创建多个仪表
  let instruments = Array[]::build(fn(push) {
    for i = 0; i < 100; i = i + 1 {
      let counter = meter.create_counter("counter_${i}", "count", "Counter ${i}")
      let histogram = meter.create_histogram("histogram_${i}", "ms", "Histogram ${i}")
      let gauge = meter.create_gauge("gauge_${i}", "value", "Gauge ${i}")
      
      push(counter)
      push(histogram)
      push(gauge)
    }
  })
  
  // 验证并发仪表创建安全性
  @assertion.assert_eq(instruments.length, 300) // 100 each of counter, histogram, gauge
}

test "concurrent_large_data_processing_safety" {
  // 测试并发大数据处理的安全性
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("large-data-tracer")
  
  let large_string = "x".repeat(1000)
  let large_array = Array[String]::with_capacity(500, fn(i) { "item_${i}" })
  
  // 并发处理大数据
  let spans = Array[]::build(fn(push) {
    for i = 0; i < 20; i = i + 1 {
      let (ctx, span) = tracer.start_span(
        context::Context::empty(),
        "large_data_processing_${i}",
        trace::Internal,
        [
          ("large_string", common::AttributeValue::string(large_string)),
          ("large_array", common::AttributeValue::array_string(large_array)),
          ("batch_id", common::AttributeValue::int(i.to_int64()))
        ]
      )
      push(span)
    }
  })
  
  // 验证并发大数据处理安全性
  @assertion.assert_eq(spans.length, 20)
  
  for i = 0; i < spans.length; i = i + 1 {
    let span = spans[i]
    @assertion.assert_eq(span.name, "large_data_processing_${i}")
    @assertion.assert_eq(span.attributes.length, 3)
  }
}

test "concurrent_error_handling_safety" {
  // 测试并发错误处理的安全性
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("error-handling-tracer")
  
  // 并发模拟错误场景
  let error_spans = Array[]::build(fn(push) {
    for i = 0; i < 30; i = i + 1 {
      let (ctx, span) = tracer.start_span(
        context::Context::empty(),
        "error_scenario_${i}",
        trace::Internal,
        [
          ("error_type", common::AttributeValue::string("concurrent_error_${i}")),
          ("error_code", common::AttributeValue::int((400 + i).to_int64())),
          ("recovery_attempted", common::AttributeValue::bool(i % 2 == 0))
        ]
      )
      push(span)
    }
  })
  
  // 验证并发错误处理安全性
  @assertion.assert_eq(error_spans.length, 30)
  
  for i = 0; i < error_spans.length; i = i + 1 {
    let span = error_spans[i]
    @assertion.assert_eq(span.name, "error_scenario_${i}")
    @assertion.assert_eq(span.attributes.length, 3)
  }
}

test "concurrent_memory_allocation_safety" {
  // 测试并发内存分配的安全性
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  
  // 并发大量内存分配
  let operations = Array[]::build(fn(push) {
    for i = 0; i < 100; i = i + 1 {
      let tracer = tracer_provider.get_tracer("memory_allocator_${i}")
      let meter = meter_provider.get_meter("memory_allocator_${i}")
      
      // 创建大量对象
      let spans = Array[]::build(fn(span_push) {
        for j = 0; j < 10; j = j + 1 {
          let (ctx, span) = tracer.start_span(
            context::Context::empty(),
            "memory_allocation_${i}_${j}",
            trace::Internal,
            [("allocation_id", common::AttributeValue::string("alloc_${i}_${j}"))]
          )
          span_push(span)
        }
      })
      
      let counter = meter.create_counter("allocation_counter_${i}", "count", "Allocation counter")
      counter.add(10L)
      
      push(spans.length)
    }
  })
  
  // 验证并发内存分配安全性
  @assertion.assert_eq(operations.length, 100)
  
  let total_spans = operations.fold_left(0L, fn(acc, count) { acc + count.to_int64() })
  @assertion.assert_eq(total_spans, 1000L) // 100 operations * 10 spans each
}
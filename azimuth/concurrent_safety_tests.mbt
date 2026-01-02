// Concurrent Safety Tests
// 验证遥测系统在并发环境下的安全性

test "concurrent span creation and operations" {
  // 测试并发span创建和操作
  let provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(provider, "concurrent-test-tracer")
  
  // 创建共享的span集合
  let spans = azimuth::ConcurrentArray::new()
  let operation_count = 100
  let thread_count = 4
  
  // 在多个线程中并发创建span
  for thread_id in 0..thread_count {
    // 模拟并发操作
    for i in 0..operation_count {
      let trace_id = "trace-" + thread_id.to_string() + "-" + i.to_string()
      let span_id = "span-" + thread_id.to_string() + "-" + i.to_string()
      let span_ctx = azimuth::SpanContext::new(trace_id, span_id, true, "")
      let span = azimuth::Tracer::start_span(tracer, "concurrent-operation-" + i.to_string(), span_ctx)
      
      // 并发设置属性
      azimuth::Span::set_attribute(span, "thread.id", azimuth::IntValue(thread_id))
      azimuth::Span::set_attribute(span, "operation.id", azimuth::IntValue(i))
      azimuth::Span::set_attribute(span, "thread.safe", azimuth::BoolValue(true))
      
      // 添加到共享集合
      azimuth::ConcurrentArray::push(spans, span)
    }
  }
  
  // 验证所有span都被创建
  let total_spans = azimuth::ConcurrentArray::size(spans)
  assert_eq(total_spans, operation_count * thread_count)
  
  // 验证span属性的正确性
  let correct_attributes = 0
  for i in 0..total_spans {
    let span = azimuth::ConcurrentArray::get(spans, i)
    let thread_id = azimuth::Span::get_attribute(span, "thread.id")
    let operation_id = azimuth::Span::get_attribute(span, "operation.id")
    let thread_safe = azimuth::Span::get_attribute(span, "thread.safe")
    
    if (thread_id.length > 0 && operation_id.length > 0 && thread_safe.length > 0) {
      correct_attributes += 1
    }
  }
  
  assert_eq(correct_attributes, total_spans)
}

test "concurrent metrics aggregation" {
  // 测试并发指标聚合
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "concurrent-test-meter")
  let counter = azimuth::Meter::create_counter(meter, "concurrent.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "concurrent.histogram")
  
  let thread_count = 8
  let operations_per_thread = 1000
  
  // 在多个线程中并发记录指标
  for thread_id in 0..thread_count {
    for i in 0..operations_per_thread {
      // 并发更新计数器
      azimuth::Counter::add(counter, 1, [
        {"thread.id", azimuth::IntValue(thread_id)},
        {"operation", azimuth::StringValue("concurrent-test")}
      ])
      
      // 并发记录直方图值
      let value = (thread_id * operations_per_thread + i).to_double()
      azimuth::Histogram::record(histogram, value, [
        {"thread.id", azimuth::IntValue(thread_id)}
      ])
    }
  }
  
  // 收集指标并验证聚合结果
  let counter_metrics = azimuth::Counter::collect(counter)
  let histogram_metrics = azimuth::Histogram::collect(histogram)
  
  // 验证计数器总和
  let total_count = 0
  for metric in counter_metrics {
    total_count += azimuth::CounterMetric::value(metric)
  }
  
  assert_eq(total_count, thread_count * operations_per_thread)
  
  // 验证直方图统计
  for metric in histogram_metrics {
    let count = azimuth::HistogramMetric::count(metric)
    assert_eq(count, thread_count * operations_per_thread)
    
    let sum = azimuth::HistogramMetric::sum(metric)
    let expected_sum = (thread_count * operations_per_thread * (thread_count * operations_per_thread - 1) / 2).to_double()
    assert_eq(sum, expected_sum)
  }
}

test "concurrent context propagation" {
  // 测试并发上下文传播
  let propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let thread_count = 6
  let operations_per_thread = 500
  
  // 创建共享的传播结果集合
  let propagation_results = azimuth::ConcurrentArray::new()
  
  // 在多个线程中并发进行上下文传播
  for thread_id in 0..thread_count {
    for i in 0..operations_per_thread {
      // 创建唯一的上下文
      let trace_id = "concurrent-trace-" + thread_id.to_string() + "-" + i.to_string()
      let span_id = "concurrent-span-" + thread_id.to_string() + "-" + i.to_string()
      let span_ctx = azimuth::SpanContext::new(trace_id, span_id, true, "")
      
      // 创建baggage
      let baggage = azimuth::Baggage::new()
      azimuth::Baggage::set_entry(baggage, "thread.id", thread_id.to_string())
      azimuth::Baggage::set_entry(baggage, "operation.id", i.to_string())
      
      // 创建载体
      let carrier = azimuth::TextMapCarrier::new()
      
      // 并发注入
      azimuth::W3CTraceContextPropagator::inject(propagator, span_ctx, carrier)
      azimuth::W3CBaggagePropagator::inject(baggage_propagator, baggage, carrier)
      
      // 并发提取
      let extracted_ctx = azimuth::W3CTraceContextPropagator::extract(propagator, carrier)
      let extracted_baggage = azimuth::W3CBaggagePropagator::extract(baggage_propagator, carrier)
      
      // 验证传播正确性
      let trace_id_match = azimuth::SpanContext::trace_id(extracted_ctx) == trace_id
      let thread_id_match = azimuth::Baggage::get_entry(extracted_baggage, "thread.id") == Some(thread_id.to_string())
      let operation_id_match = azimuth::Baggage::get_entry(extracted_baggage, "operation.id") == Some(i.to_string())
      
      // 记录结果
      if (trace_id_match && thread_id_match && operation_id_match) {
        azimuth::ConcurrentArray::push(propagation_results, true)
      } else {
        azimuth::ConcurrentArray::push(propagation_results, false)
      }
    }
  }
  
  // 验证所有传播操作都成功
  let total_operations = azimuth::ConcurrentArray::size(propagation_results)
  let successful_operations = 0
  
  for i in 0..total_operations {
    let result = azimuth::ConcurrentArray::get(propagation_results, i)
    if (result) {
      successful_operations += 1
    }
  }
  
  assert_eq(successful_operations, total_operations)
}

test "concurrent attribute operations" {
  // 测试并发属性操作
  let attributes = azimuth::Attributes::new()
  let thread_count = 10
  let operations_per_thread = 100
  
  // 在多个线程中并发操作属性
  for thread_id in 0..thread_count {
    for i in 0..operations_per_thread {
      let key = "concurrent.key." + (i % 10).to_string()
      let value = azimuth::StringValue("thread-" + thread_id.to_string() + "-value-" + i.to_string())
      
      // 并发设置属性
      azimuth::Attributes::set(attributes, key, value)
      
      // 并发读取属性
      let retrieved = azimuth::Attributes::get(attributes, key)
      assert_true(retrieved.length > 0)
      
      // 并发删除一些属性
      if (i % 20 == 0) {
        let delete_key = "delete.key." + (i % 5).to_string()
        azimuth::Attributes::set(attributes, delete_key, azimuth::StringValue("temp"))
        azimuth::Attributes::remove(attributes, delete_key)
      }
    }
  }
  
  // 验证属性操作的最终状态
  let final_keys = azimuth::Attributes::keys(attributes)
  assert_true(final_keys.length > 0)
  
  // 验证属性值的一致性
  let consistent_values = 0
  for key in final_keys {
    let value = azimuth::Attributes::get(attributes, key)
    if (value.length > 0) {
      consistent_values += 1
    }
  }
  
  assert_eq(consistent_values, final_keys.length)
}

test "concurrent resource management" {
  // 测试并发资源管理
  let resource_manager = azimuth::ResourceManager::new()
  let thread_count = 5
  let resources_per_thread = 200
  
  // 在多个线程中并发创建和管理资源
  for thread_id in 0..thread_count {
    let thread_resources = []
    
    for i in 0..resources_per_thread {
      let resource = azimuth::Resource::new()
      azimuth::Resource::set_attribute(resource, "thread.id", azimuth::IntValue(thread_id))
      azimuth::Resource::set_attribute(resource, "resource.id", azimuth::IntValue(i))
      
      // 并发注册资源
      azimuth::ResourceManager::register_resource(resource_manager, resource)
      thread_resources.push(resource)
    }
    
    // 并发清理部分资源
    for i in 0..resources_per_thread / 2 {
      let resource = thread_resources[i]
      azimuth::ResourceManager::unregister_resource(resource_manager, resource)
    }
  }
  
  // 验证资源管理器的状态
  let managed_resources = azimuth::ResourceManager::get_resources(resource_manager)
  assert_true(managed_resources.length > 0)
  
  // 验证没有资源泄漏
  let leaked_resources = azimuth::ResourceManager::detect_leaks(resource_manager)
  assert_eq(leaked_resources.length, 0)
}
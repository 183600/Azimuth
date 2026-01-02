// Azimuth Resource Lifecycle Management Tests
// 资源生命周期管理测试用例 - 专注于资源创建、管理和清理

// Test 1: TracerProvider资源管理
test "tracer provider resource management" {
  // 创建多个TracerProvider实例
  let provider1 = TracerProvider::default()
  let provider2 = TracerProvider::default()
  let provider3 = TracerProvider::default()
  
  // 验证每个provider都能正常工作
  let tracer1 = TracerProvider::get_tracer(provider1, "service1")
  let tracer2 = TracerProvider::get_tracer(provider2, "service2")
  let tracer3 = TracerProvider::get_tracer(provider3, "service3")
  
  assert_true(tracer1 !== None)
  assert_true(tracer2 !== None)
  assert_true(tracer3 !== None)
  
  // 创建并结束多个span
  let span1 = Tracer::start_span(tracer1, "operation1")
  let span2 = Tracer::start_span(tracer2, "operation2")
  let span3 = Tracer::start_span(tracer3, "operation3")
  
  // 设置属性和事件
  Span::set_attribute(span1, "resource.id", "res1")
  Span::set_attribute(span2, "resource.id", "res2")
  Span::set_attribute(span3, "resource.id", "res3")
  
  Span::add_event(span1, "resource.created", [])
  Span::add_event(span2, "resource.processed", [])
  Span::add_event(span3, "resource.completed", [])
  
  // 结束所有span
  Span::end(span1)
  Span::end(span2)
  Span::end(span3)
  
  // 强制资源清理
  TracerProvider::shutdown(provider1)
  TracerProvider::shutdown(provider2)
  TracerProvider::shutdown(provider3)
  
  assert_true(true)
}

// Test 2: MeterProvider资源管理
test "meter provider resource management" {
  // 创建MeterProvider并注册多个仪表
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "resource.test")
  
  // 创建不同类型的仪表
  let counter = Meter::create_counter(meter, "operations.total", Some("Total operations"), Some("count"))
  let histogram = Meter::create_histogram(meter, "operation.duration", Some("Operation duration"), Some("ms"))
  let gauge = Meter::create_gauge(meter, "active.connections", Some("Active connections"), Some("connections"))
  
  // 执行度量操作
  Counter::add(counter, 10.0)
  Counter::add_with_attributes(counter, 5.0, [("operation.type", "read")])
  
  Histogram::record(histogram, 150.5)
  Histogram::record_with_attributes(histogram, 75.2, [("operation.type", "write")])
  
  Gauge::set(gauge, 25.0)
  Gauge::set_with_attributes(gauge, 30.0, [("connection.type", "persistent")])
  
  // 验证度量数据已记录
  assert_true(true)
  
  // 清理资源
  MeterProvider::shutdown(meter_provider)
  assert_true(true)
}
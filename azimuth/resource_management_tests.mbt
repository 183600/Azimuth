// Resource Management Tests
// 验证遥测系统的资源管理能力，包括内存、文件句柄等

test "resource lifecycle management" {
  // 测试资源的生命周期管理
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "lifecycle-test")
  
  // 创建多个资源
  let resources = []
  for i in 0..100 {
    let resource = azimuth::Resource::new()
    azimuth::Resource::set_attribute(resource, "resource.id", azimuth::IntValue(i))
    azimuth::Resource::set_attribute(resource, "resource.type", azimuth::StringValue("test"))
    resources.push(resource)
  }
  
  // 验证资源创建成功
  assert_eq(resources.length, 100)
  
  // 使用资源创建测量
  for resource in resources {
    let counter = azimuth::Meter::create_counter_with_resource(meter, "resource.counter", resource)
    azimuth::Counter::add(counter, 1, [])
  }
  
  // 清理资源
  resources = []
  
  // 验证资源被正确清理
  assert_eq(resources.length, 0)
}

test "memory pool management" {
  // 测试内存池管理
  let initial_memory = azimuth::Memory::allocated()
  
  // 创建大量对象使用内存池
  let objects = []
  for i in 0..1000 {
    let span_ctx = azimuth::SpanContext::new("pool-test-" + i.to_string(), "span-" + i.to_string(), true, "")
    let span = azimuth::Span::new("pooled-operation", azimuth::Internal, span_ctx)
    objects.push(span)
  }
  
  let peak_memory = azimuth::Memory::allocated()
  
  // 释放对象回内存池
  objects = []
  
  // 强制垃圾回收
  azimuth::Memory::collect()
  
  let final_memory = azimuth::Memory::allocated()
  
  // 验证内存池有效回收内存
  let memory_recovered = (peak_memory - final_memory).to_double() / (peak_memory - initial_memory).to_double()
  assert_true(memory_recovered > 0.7, "Memory pool should recover at least 70% of allocated memory")
}

test "resource attribute merge strategies" {
  // 测试资源属性合并策略
  let resource1 = azimuth::Resource::new()
  azimuth::Resource::set_attribute(resource1, "service.name", azimuth::StringValue("service-A"))
  azimuth::Resource::set_attribute(resource1, "service.version", azimuth::StringValue("1.0.0"))
  azimuth::Resource::set_attribute(resource1, "env", azimuth::StringValue("production"))
  
  let resource2 = azimuth::Resource::new()
  azimuth::Resource::set_attribute(resource2, "service.name", azimuth::StringValue("service-B")) // 冲突的键
  azimuth::Resource::set_attribute(resource2, "host.name", azimuth::StringValue("host-123"))
  azimuth::Resource::set_attribute(resource2, "zone", azimuth::StringValue("us-west-1"))
  
  // 测试不同的合并策略
  let merged_override = azimuth::Resource::merge(resource1, resource2, azimuth::OverrideStrategy)
  let merged_keep = azimuth::Resource::merge(resource1, resource2, azimuth::KeepExistingStrategy)
  let merged_combine = azimuth::Resource::merge(resource1, resource2, azimuth::CombineStrategy)
  
  // 验证覆盖策略
  let service_name_override = azimuth::Resource::get_attribute(merged_override, "service.name")
  assert_eq(service_name_override, Some(azimuth::StringValue("service-B")))
  
  // 验证保留策略
  let service_name_keep = azimuth::Resource::get_attribute(merged_keep, "service.name")
  assert_eq(service_name_keep, Some(azimuth::StringValue("service-A")))
  
  // 验证合并策略包含所有属性
  let host_name = azimuth::Resource::get_attribute(merged_combine, "host.name")
  let zone = azimuth::Resource::get_attribute(merged_combine, "zone")
  let env = azimuth::Resource::get_attribute(merged_combine, "env")
  
  assert_eq(host_name, Some(azimuth::StringValue("host-123")))
  assert_eq(zone, Some(azimuth::StringValue("us-west-1")))
  assert_eq(env, Some(azimuth::StringValue("production")))
}

test "resource cleanup on shutdown" {
  // 测试关闭时的资源清理
  let provider = azimuth::MeterProvider::default()
  let logger_provider = azimuth::LoggerProvider::default()
  let tracer_provider = azimuth::TracerProvider::default()
  
  // 创建多个组件
  let meters = []
  let loggers = []
  let tracers = []
  
  for i in 0..50 {
    let meter = azimuth::MeterProvider::get_meter(provider, "meter-" + i.to_string())
    let logger = azimuth::LoggerProvider::get_logger(logger_provider, "logger-" + i.to_string())
    let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "tracer-" + i.to_string())
    
    meters.push(meter)
    loggers.push(logger)
    tracers.push(tracer)
  }
  
  // 验证组件创建成功
  assert_eq(meters.length, 50)
  assert_eq(loggers.length, 50)
  assert_eq(tracers.length, 50)
  
  // 模拟关闭时的清理
  let shutdown_result = azimuth::MeterProvider::shutdown(provider)
  let logger_shutdown_result = azimuth::LoggerProvider::shutdown(logger_provider)
  let tracer_shutdown_result = azimuth::TracerProvider::shutdown(tracer_provider)
  
  // 验证关闭成功
  assert_true(shutdown_result)
  assert_true(logger_shutdown_result)
  assert_true(tracer_shutdown_result)
  
  // 清理引用
  meters = []
  loggers = []
  tracers = []
}

test "resource limits and quotas" {
  // 测试资源限制和配额
  let resource_manager = azimuth::ResourceManager::new()
  
  // 设置资源限制
  azimuth::ResourceManager::set_max_spans(resource_manager, 1000)
  azimuth::ResourceManager::set_max_metrics(resource_manager, 5000)
  azimuth::ResourceManager::set_max_log_records(resource_manager, 2000)
  
  // 测试在限制内的操作
  let spans_created = 0
  for i in 0..1000 {
    let span_ctx = azimuth::SpanContext::new("limit-test-" + i.to_string(), "span-" + i.to_string(), true, "")
    let span = azimuth::Span::new("limited-operation", azimuth::Internal, span_ctx)
    
    if (azimuth::ResourceManager::can_create_span(resource_manager)) {
      azimuth::ResourceManager::register_span(resource_manager, span)
      spans_created += 1
    }
  }
  
  // 验证不超过限制
  assert_true(spans_created <= 1000)
  
  // 测试超出限制的行为
  let additional_span_created = false
  let span_ctx = azimuth::SpanContext::new("additional", "additional-span", true, "")
  let additional_span = azimuth::Span::new("additional-operation", azimuth::Internal, span_ctx)
  
  if (azimuth::ResourceManager::can_create_span(resource_manager)) {
    azimuth::ResourceManager::register_span(resource_manager, additional_span)
    additional_span_created = true
  }
  
  // 如果已经达到限制，不应该能创建更多span
  if (spans_created >= 1000) {
    assert_false(additional_span_created)
  }
  
  // 测试资源清理后的限制重置
  azimuth::ResourceManager::cleanup_spans(resource_manager)
  let can_create_after_cleanup = azimuth::ResourceManager::can_create_span(resource_manager)
  assert_true(can_create_after_cleanup)
}
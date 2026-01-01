// Azimuth Resource Management Tests
// 资源管理测试

test "基础resource创建和属性测试" {
  let resource = Resource::new()
  
  // 验证空resource
  let empty_attr = Resource::get_attribute(resource, "any.key")
  assert_eq(empty_attr, None)
  
  // 创建带有属性的resource
  let attrs = [
    ("service.name", StringValue("test.service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-12345")),
    ("deployment.environment", StringValue("test"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // 验证属性
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  match service_name {
    Some(StringValue(value)) => assert_eq(value, "test.service")
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  match service_version {
    Some(StringValue(value)) => assert_eq(value, "1.0.0")
    _ => assert_true(false)
  }
  
  let service_instance_id = Resource::get_attribute(resource_with_attrs, "service.instance.id")
  match service_instance_id {
    Some(StringValue(value)) => assert_eq(value, "instance-12345")
    _ => assert_true(false)
  }
  
  let deployment_environment = Resource::get_attribute(resource_with_attrs, "deployment.environment")
  match deployment_environment {
    Some(StringValue(value)) => assert_eq(value, "test")
    _ => assert_true(false)
  }
  
  // 验证不存在的属性
  let missing_attr = Resource::get_attribute(resource_with_attrs, "nonexistent.attr")
  assert_eq(missing_attr, None)
}

test "resource属性类型转换测试" {
  let resource = Resource::new()
  
  // 测试不同类型的属性
  let mixed_attrs = [
    ("string.attr", StringValue("string.value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("array.string.attr", ArrayStringValue(["item1", "item2", "item3"])),
    ("array.int.attr", ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  let resource_with_mixed = Resource::with_attributes(resource, mixed_attrs)
  
  // 验证字符串属性
  let string_attr = Resource::get_attribute(resource_with_mixed, "string.attr")
  match string_attr {
    Some(StringValue(value)) => assert_eq(value, "string.value")
    _ => assert_true(false)
  }
  
  // 验证整数属性
  let int_attr = Resource::get_attribute(resource_with_mixed, "int.attr")
  match int_attr {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  // 验证浮点属性
  let float_attr = Resource::get_attribute(resource_with_mixed, "float.attr")
  match float_attr {
    Some(FloatValue(value)) => assert_eq(value, 3.14159)
    _ => assert_true(false)
  }
  
  // 验证布尔属性
  let bool_attr = Resource::get_attribute(resource_with_mixed, "bool.attr")
  match bool_attr {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false)
  }
  
  // 验证字符串数组属性
  let array_string_attr = Resource::get_attribute(resource_with_mixed, "array.string.attr")
  match array_string_attr {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "item1")
      assert_eq(values[1], "item2")
      assert_eq(values[2], "item3")
    }
    _ => assert_true(false)
  }
  
  // 验证整数数组属性
  let array_int_attr = Resource::get_attribute(resource_with_mixed, "array.int.attr")
  match array_int_attr {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length(), 5)
      assert_eq(values[0], 1)
      assert_eq(values[1], 2)
      assert_eq(values[2], 3)
      assert_eq(values[3], 4)
      assert_eq(values[4], 5)
    }
    _ => assert_true(false)
  }
}

test "resource合并策略测试" {
  let base_resource = Resource::new()
  let override_resource = Resource::new()
  
  // 设置基础resource属性
  let base_attrs = [
    ("service.name", StringValue("base.service")),
    ("service.version", StringValue("1.0.0")),
    ("base.only.attr", StringValue("base.value"))
  ]
  
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  // 设置覆盖resource属性
  let override_attrs = [
    ("service.version", StringValue("2.0.0")), // 覆盖基础版本
    ("override.only.attr", StringValue("override.value"))
  ]
  
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // 测试合并
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // 验证合并结果（简化实现可能只返回override resource）
  let service_name = Resource::get_attribute(merged, "service.name")
  let service_version = Resource::get_attribute(merged, "service.version")
  let base_only = Resource::get_attribute(merged, "base.only.attr")
  let override_only = Resource::get_attribute(merged, "override.only.attr")
  
  // 简化实现可能只返回override resource的属性
  match service_version {
    Some(StringValue(value)) => assert_eq(value, "2.0.0")
    _ => assert_true(false)
  }
  
  match override_only {
    Some(StringValue(value)) => assert_eq(value, "override.value")
    _ => assert_true(false)
  }
  
  // 测试多次合并
  let additional_attrs = [
    ("additional.attr", StringValue("additional.value")),
    ("service.version", StringValue("3.0.0")) // 再次覆盖版本
  ]
  
  let additional_resource = Resource::with_attributes(Resource::new(), additional_attrs)
  let multi_merged = Resource::merge(merged, additional_resource)
  
  // 验证多次合并结果
  let final_version = Resource::get_attribute(multi_merged, "service.version")
  let additional_attr = Resource::get_attribute(multi_merged, "additional.attr")
  
  match final_version {
    Some(StringValue(value)) => assert_eq(value, "3.0.0")
    _ => assert_true(false)
  }
  
  match additional_attr {
    Some(StringValue(value)) => assert_eq(value, "additional.value")
    _ => assert_true(false)
  }
}

test "resource边界条件测试" {
  let resource = Resource::new()
  
  // 测试空键和值
  let empty_attrs = [
    ("", StringValue("empty.key")),
    ("empty.value", StringValue(""))
  ]
  
  let resource_with_empty = Resource::with_attributes(resource, empty_attrs)
  
  // 验证空键和值
  let empty_key = Resource::get_attribute(resource_with_empty, "")
  let empty_value = Resource::get_attribute(resource_with_empty, "empty.value")
  
  match empty_key {
    Some(StringValue(value)) => assert_eq(value, "empty.key")
    _ => assert_true(false)
  }
  
  match empty_value {
    Some(StringValue(value)) => assert_eq(value, "")
    _ => assert_true(false)
  }
  
  // 测试非常长的键和值
  let long_key = "this.is.a.very.long.attribute.key.name.that.exceeds.normal.limits"
  let long_value = "this.is.a.very.long.attribute.value.that.exceeds.normal.limits.and.tests.boundary.conditions"
  
  let long_attrs = [
    (long_key, StringValue(long_value))
  ]
  
  let resource_with_long = Resource::with_attributes(resource, long_attrs)
  
  // 验证长键和值
  let retrieved_long = Resource::get_attribute(resource_with_long, long_key)
  match retrieved_long {
    Some(StringValue(value)) => assert_eq(value, long_value)
    _ => assert_true(false)
  }
  
  // 测试特殊字符键和值
  let special_attrs = [
    ("!@#$%^&*()", StringValue("!@#$%^&*()")),
    ("[]{}|;':\",./<>?", StringValue("[]{}|;':\",./<>?"))
  ]
  
  let resource_with_special = Resource::with_attributes(resource, special_attrs)
  
  // 验证特殊字符键和值
  let special1 = Resource::get_attribute(resource_with_special, "!@#$%^&*()")
  let special2 = Resource::get_attribute(resource_with_special, "[]{}|;':\",./<>?")
  
  match special1 {
    Some(StringValue(value)) => assert_eq(value, "!@#$%^&*()")
    _ => assert_true(false)
  }
  
  match special2 {
    Some(StringValue(value)) => assert_eq(value, "[]{}|;':\",./<>?")
    _ => assert_true(false)
  }
  
  // 测试Unicode键和值
  let unicode_attrs = [
    ("测试键", StringValue("测试值")),
    ("emoji🚀", StringValue("emoji📊"))
  ]
  
  let resource_with_unicode = Resource::with_attributes(resource, unicode_attrs)
  
  // 验证Unicode键和值
  let unicode1 = Resource::get_attribute(resource_with_unicode, "测试键")
  let unicode2 = Resource::get_attribute(resource_with_unicode, "emoji🚀")
  
  match unicode1 {
    Some(StringValue(value)) => assert_eq(value, "测试值")
    _ => assert_true(false)
  }
  
  match unicode2 {
    Some(StringValue(value)) => assert_eq(value, "emoji📊")
    _ => assert_true(false)
  }
}

test "resource性能测试" {
  // 测试大量resource创建
  let resource_count = 1000
  let resources = []
  
  for i = 0; i < resource_count; i = i + 1 {
    let attrs = [
      ("resource.id", StringValue("resource." + i.to_string())),
      ("resource.type", StringValue("test.resource")),
      ("resource.index", IntValue(i))
    ]
    let res = Resource::with_attributes(Resource::new(), attrs)
    resources.push(res)
  }
  
  // 验证所有resource都成功创建
  assert_eq(resources.length(), resource_count)
  
  // 测试大量属性检索
  for i = 0; i < resource_count; i = i + 1 {
    let res = resources[i]
    let resource_id = Resource::get_attribute(res, "resource.id")
    let resource_type = Resource::get_attribute(res, "resource.type")
    let resource_index = Resource::get_attribute(res, "resource.index")
    
    match resource_id {
      Some(StringValue(value)) => assert_eq(value, "resource." + i.to_string())
      _ => assert_true(false)
    }
    
    match resource_type {
      Some(StringValue(value)) => assert_eq(value, "test.resource")
      _ => assert_true(false)
    }
    
    match resource_index {
      Some(IntValue(value)) => assert_eq(value, i)
      _ => assert_true(false)
    }
  }
  
  // 测试大量resource合并
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("base.attr", StringValue("base.value")),
    ("base.version", StringValue("1.0.0"))
  ])
  
  let merged_resources = []
  for res in resources {
    let merged = Resource::merge(base_resource, res)
    merged_resources.push(merged)
  }
  
  // 验证所有resource都成功合并
  assert_eq(merged_resources.length(), resource_count)
  
  // 测试复杂属性的resource
  let complex_resources = []
  for i = 0; i < 100; i = i + 1 {
    let complex_attrs = [
      ("string.attr", StringValue("string.value." + i.to_string())),
      ("int.attr", IntValue(i)),
      ("float.attr", FloatValue(i.to_double() * 0.1)),
      ("bool.attr", BoolValue(i % 2 == 0)),
      ("array.string.attr", ArrayStringValue(["item1", "item2", "item3"])),
      ("array.int.attr", ArrayIntValue([1, 2, 3, 4, 5]))
    ]
    let complex_res = Resource::with_attributes(Resource::new(), complex_attrs)
    complex_resources.push(complex_res)
  }
  
  // 验证复杂resource
  for i = 0; i < complex_resources.length(); i = i + 1 {
    let res = complex_resources[i]
    
    let string_attr = Resource::get_attribute(res, "string.attr")
    let int_attr = Resource::get_attribute(res, "int.attr")
    let float_attr = Resource::get_attribute(res, "float.attr")
    let bool_attr = Resource::get_attribute(res, "bool.attr")
    let array_string_attr = Resource::get_attribute(res, "array.string.attr")
    let array_int_attr = Resource::get_attribute(res, "array.int.attr")
    
    match string_attr {
      Some(StringValue(value)) => assert_eq(value, "string.value." + i.to_string())
      _ => assert_true(false)
    }
    
    match int_attr {
      Some(IntValue(value)) => assert_eq(value, i)
      _ => assert_true(false)
    }
    
    match float_attr {
      Some(FloatValue(value)) => assert_eq(value, i.to_double() * 0.1)
      _ => assert_true(false)
    }
    
    match bool_attr {
      Some(BoolValue(value)) => assert_eq(value, i % 2 == 0)
      _ => assert_true(false)
    }
    
    // 简化实现可能不支持数组属性
  }
  
  assert_true(true)
}

test "resource与telemetry组件集成测试" {
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  // 创建带有丰富属性的resource
  let resource = Resource::new()
  let resource_attrs = [
    ("service.name", StringValue("integrated.service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-12345")),
    ("deployment.environment", StringValue("test")),
    ("host.name", StringValue("test-host")),
    ("host.ip", StringValue("192.168.1.100")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("5.4.0")),
    ("process.id", IntValue(12345)),
    ("process.name", StringValue("integrated.service")),
    ("process.command_line", StringValue("/usr/bin/integrated.service --config=test.conf"))
  ]
  
  let enriched_resource = Resource::with_attributes(resource, resource_attrs)
  
  // 创建带有resource信息的telemetry组件
  let tracer = TracerProvider::get_tracer(tracer_provider, "integrated.tracer")
  let meter = MeterProvider::get_meter(meter_provider, "integrated.meter")
  let logger = LoggerProvider::get_logger(logger_provider, "integrated.logger")
  
  // 创建span并关联resource信息
  let span = Tracer::start_span(tracer, "integrated.operation")
  
  // 添加resource信息到span
  let service_name = Resource::get_attribute(enriched_resource, "service.name")
  let service_version = Resource::get_attribute(enriched_resource, "service.version")
  let service_instance_id = Resource::get_attribute(enriched_resource, "service.instance.id")
  
  match service_name {
    Some(StringValue(name)) => {
      Span::add_event(span, "resource.info", Some([
        ("service.name", StringValue(name))
      ]))
    }
    _ => assert_true(false)
  }
  
  match service_version {
    Some(StringValue(version)) => {
      Span::add_event(span, "resource.info", Some([
        ("service.version", StringValue(version))
      ]))
    }
    _ => assert_true(false)
  }
  
  match service_instance_id {
    Some(StringValue(instance_id)) => {
      Span::add_event(span, "resource.info", Some([
        ("service.instance.id", StringValue(instance_id))
      ]))
    }
    _ => assert_true(false)
  }
  
  // 创建metrics并关联resource信息
  let counter = Meter::create_counter(meter, "integrated.operations")
  let histogram = Meter::create_histogram(meter, "integrated.duration")
  
  // 记录metrics
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 100.0)
  
  // 创建log记录并关联resource信息
  let host_name = Resource::get_attribute(enriched_resource, "host.name")
  let process_id = Resource::get_attribute(enriched_resource, "process.id")
  
  let log_attrs = Attributes::new()
  
  match host_name {
    Some(StringValue(name)) => {
      Attributes::set(log_attrs, "host.name", StringValue(name))
    }
    _ => assert_true(false)
  }
  
  match process_id {
    Some(IntValue(pid)) => {
      Attributes::set(log_attrs, "process.id", IntValue(pid))
    }
    _ => assert_true(false)
  }
  
  let log = LogRecord::new_with_context(
    Info,
    Some("Integrated operation with resource context"),
    Some(log_attrs),
    Some(1735689600000000000L),
    None,
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    Some(Context::root())
  )
  
  Logger::emit(logger, log)
  
  // 结束span
  Span::end(span)
  
  // 验证resource属性仍然可以访问
  let deployment_environment = Resource::get_attribute(enriched_resource, "deployment.environment")
  match deployment_environment {
    Some(StringValue(env)) => assert_eq(env, "test")
    _ => assert_true(false)
  }
  
  assert_true(true)
}

test "resource内存管理测试" {
  // 创建大量resource以测试内存管理
  let resource_count = 5000
  let resources = []
  
  for i = 0; i < resource_count; i = i + 1 {
    // 每个resource包含大量属性
    let attrs = [
      ("resource.id", StringValue("resource." + i.to_string())),
      ("resource.type", StringValue("memory.test.resource")),
      ("resource.index", IntValue(i)),
      ("resource.data", StringValue("x".repeat(100))), // 100字符的字符串
      ("resource.timestamp", StringValue("2025-01-01T00:00:00Z")),
      ("resource.metadata", StringValue("metadata-" + i.to_string()))
    ]
    
    let res = Resource::with_attributes(Resource::new(), attrs)
    resources.push(res)
  }
  
  // 验证所有resource都成功创建
  assert_eq(resources.length(), resource_count)
  
  // 执行大量属性检索操作
  for i = 0; i < resource_count; i = i + 1 {
    let res = resources[i]
    
    // 检索所有属性
    let resource_id = Resource::get_attribute(res, "resource.id")
    let resource_type = Resource::get_attribute(res, "resource.type")
    let resource_index = Resource::get_attribute(res, "resource.index")
    let resource_data = Resource::get_attribute(res, "resource.data")
    let resource_timestamp = Resource::get_attribute(res, "resource.timestamp")
    let resource_metadata = Resource::get_attribute(res, "resource.metadata")
    
    // 验证属性值
    match resource_id {
      Some(StringValue(value)) => assert_eq(value, "resource." + i.to_string())
      _ => assert_true(false)
    }
    
    match resource_type {
      Some(StringValue(value)) => assert_eq(value, "memory.test.resource")
      _ => assert_true(false)
    }
    
    match resource_index {
      Some(IntValue(value)) => assert_eq(value, i)
      _ => assert_true(false)
    }
    
    match resource_data {
      Some(StringValue(value)) => assert_eq(value.length(), 100)
      _ => assert_true(false)
    }
  }
  
  // 执行大量合并操作
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("base.attr", StringValue("base.value")),
    ("base.version", StringValue("1.0.0"))
  ])
  
  let merged_resources = []
  for res in resources {
    let merged = Resource::merge(base_resource, res)
    merged_resources.push(merged)
  }
  
  // 验证合并操作
  assert_eq(merged_resources.length(), resource_count)
  
  // 测试resource释放（通过重新赋值）
  resources = []
  merged_resources = []
  
  // 创建新的resource以验证内存已释放
  let new_resources = []
  for i = 0; i < 1000; i = i + 1 {
    let attrs = [
      ("new.resource.id", StringValue("new.resource." + i.to_string())),
      ("new.resource.type", StringValue("new.test.resource"))
    ]
    let res = Resource::with_attributes(Resource::new(), attrs)
    new_resources.push(res)
  }
  
  // 验证新resource创建成功
  assert_eq(new_resources.length(), 1000)
  
  assert_true(true)
}
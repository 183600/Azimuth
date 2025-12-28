// Resource and InstrumentationScope functionality tests

test "Resource基本创建测试" {
  let resource = Resource::new()
  assert_true(resource.attributes.length == 0)
}

test "Resource属性设置测试" {
  let resource = Resource::new()
  let attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let with_attrs = Resource::with_attributes(resource, attrs)
  
  let name_result = Resource::get_attribute(with_attrs, "service.name")
  assert_true(name_result is Some)
  
  match name_result {
    Some(StringValue(value)) => assert_eq(value, "test-service")
    _ => assert_true(false)
  }
}

test "Resource多种属性类型测试" {
  let resource = Resource::new()
  let attrs = [
    ("string.attr", StringValue("test")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14)),
    ("bool.attr", BoolValue(true)),
    ("array.string.attr", ArrayStringValue(["a", "b", "c"])),
    ("array.int.attr", ArrayIntValue([1, 2, 3]))
  ]
  let with_attrs = Resource::with_attributes(resource, attrs)
  
  // 测试字符串属性
  let string_result = Resource::get_attribute(with_attrs, "string.attr")
  match string_result {
    Some(StringValue(value)) => assert_eq(value, "test")
    _ => assert_true(false)
  }
  
  // 测试整数属性
  let int_result = Resource::get_attribute(with_attrs, "int.attr")
  match int_result {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  // 测试浮点属性
  let float_result = Resource::get_attribute(with_attrs, "float.attr")
  match float_result {
    Some(FloatValue(value)) => assert_eq(value, 3.14)
    _ => assert_true(false)
  }
  
  // 测试布尔属性
  let bool_result = Resource::get_attribute(with_attrs, "bool.attr")
  match bool_result {
    Some(BoolValue(value)) => assert_true(value)
    _ => assert_true(false)
  }
}

test "Resource合并测试" {
  let base = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("environment", StringValue("production"))
  ]
  let base_with_attrs = Resource::with_attributes(base, base_attrs)
  
  let override_ = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("service.version", StringValue("2.0.0"))
  ]
  let override_with_attrs = Resource::with_attributes(override_, override_attrs)
  
  let merged = Resource::merge(base_with_attrs, override_with_attrs)
  
  // 检查合并后的属性
  let name_result = Resource::get_attribute(merged, "service.name")
  match name_result {
    Some(StringValue(value)) => assert_eq(value, "override-service")
    _ => assert_true(false)
  }
  
  let version_result = Resource::get_attribute(merged, "service.version")
  match version_result {
    Some(StringValue(value)) => assert_eq(value, "2.0.0")
    _ => assert_true(false)
  }
}

test "InstrumentationScope基本创建测试" {
  let scope = InstrumentationScope::{ 
    name: "test-scope", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/schema") 
  }
  
  assert_eq(scope.name, "test-scope")
  assert_true(scope.version is Some)
  assert_true(scope.schema_url is Some)
  
  match scope.version {
    Some(version) => assert_eq(version, "1.0.0")
    None => assert_true(false)
  }
  
  match scope.schema_url {
    Some(url) => assert_eq(url, "https://example.com/schema")
    None => assert_true(false)
  }
}

test "InstrumentationScope可选字段测试" {
  let minimal_scope = InstrumentationScope::{ 
    name: "minimal-scope", 
    version: None, 
    schema_url: None 
  }
  
  assert_eq(minimal_scope.name, "minimal-scope")
  assert_true(minimal_scope.version is None)
  assert_true(minimal_scope.schema_url is None)
}

test "InstrumentationScope与Tracer集成测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test.tracer", Some("1.0.0"))
  let scope = Tracer::instrumentation_scope(tracer)
  
  assert_eq(scope.name, "test.tracer")
  assert_true(scope.version is Some)
  
  match scope.version {
    Some(version) => assert_eq(version, "1.0.0")
    None => assert_true(false)
  }
}

test "InstrumentationScope与Meter集成测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test.meter", Some("2.0.0"))
  
  assert_eq(meter.scope.name, "test.meter")
  assert_true(meter.scope.version is Some)
  
  match meter.scope.version {
    Some(version) => assert_eq(version, "2.0.0")
    None => assert_true(false)
  }
}

test "InstrumentationScope与Logger集成测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test.logger", Some("3.0.0"))
  
  assert_eq(logger.scope.name, "test.logger")
  assert_true(logger.scope.version is Some)
  
  match logger.scope.version {
    Some(version) => assert_eq(version, "3.0.0")
    None => assert_true(false)
  }
}

test "Resource空属性访问测试" {
  let empty_resource = Resource::new()
  let result = Resource::get_attribute(empty_resource, "nonexistent.key")
  assert_true(result is None)
}
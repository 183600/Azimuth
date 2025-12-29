// 跨模块集成测试 - 测试common、context和metrics模块之间的交互
test "cross module integration: resource with context" {
  // 创建Resource
  let resource = Resource::default("integration-test-service")
  
  // 创建Context并添加resource信息
  let context = Context::empty()
  let resource_key = create_key("resource")
  let context_with_resource = context.with_value(resource_key, resource.service_name)
  
  // 验证context中包含了resource信息
  assert match (context_with_resource.get(resource_key)) {
    Some(value) => value == "integration-test-service"
    None => false
  }
  
  // 创建MeterProvider和Meter
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("integration-meter", Some("1.0.0"), None)
  
  // 使用meter创建counter
  let counter = meter.create_counter("integration_counter", Some("count"), Some("Integration test counter"))
  
  // 添加带有resource属性的measurement
  let attributes = [
    ("service.name", AttributeValue::string(resource.service_name)),
    ("service.version", match (resource.service_version) {
      Some(v) => AttributeValue::string(v)
      None => AttributeValue::string("unknown")
    }),
    ("sdk.name", AttributeValue::string(resource.telemetry_sdk_name))
  ]
  
  // 执行measurement
  counter.add(1L, Some(attributes))
  
  // 验证Resource和Context的集成
  assert resource.telemetry_sdk_name == "azimuth"
  assert resource.attributes.length == 0
}

test "cross module integration: baggage with metrics attributes" {
  // 创建Baggage
  let baggage = Baggage::empty()
  let baggage_with_entries = baggage
    .with_entry("user.id", "12345")
    .with_entry("request.id", "req-67890")
    .with_entry("environment", "production")
  
  // 从Baggage创建metrics attributes
  let mut attributes = []
  
  // 模拟从baggage中提取值并转换为attributes
  match (baggage_with_entries.get("user.id")) {
    Some(user_id) => attributes.push(("user.id", AttributeValue::string(user_id)))
    None => ()
  }
  
  match (baggage_with_entries.get("request.id")) {
    Some(request_id) => attributes.push(("request.id", AttributeValue::string(request_id)))
    None => ()
  }
  
  match (baggage_with_entries.get("environment")) {
    Some(env) => attributes.push(("environment", AttributeValue::string(env)))
    None => ()
  }
  
  // 创建meter和instrument
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("baggage-meter", None, None)
  let histogram = meter.create_histogram("request_duration", Some("ms"), Some("Request duration histogram"))
  
  // 使用从baggage提取的attributes进行measurement
  histogram.record(100.5, Some(attributes))
  
  // 验证baggage entries
  assert match (baggage_with_entries.get("user.id")) {
    Some(id) => id == "12345"
    None => false
  }
  
  assert match (baggage_with_entries.get("request.id")) {
    Some(id) => id == "req-67890"
    None => false
  }
  
  assert match (baggage_with_entries.get("environment")) {
    Some(env) => env == "production"
    None => false
  }
  
  // 验证attributes数量
  assert attributes.length == 3
}

test "cross module integration: instrumentation scope with context" {
  // 创建InstrumentationScope
  let scope = InstrumentationScope::{
    name: "integration-scope",
    version: Some("2.0.0"),
    schema_url: Some("https://example.com/integration-schema")
  }
  
  // 创建Context并添加scope信息
  let context = Context::empty()
  let scope_key = create_key("instrumentation_scope")
  let context_with_scope = context.with_value(scope_key, scope.name)
  
  // 验证context中包含了scope信息
  assert match (context_with_scope.get(scope_key)) {
    Some(value) => value == "integration-scope"
    None => false
  }
  
  // 创建带有scope属性的Resource
  let resource = Resource::default("scope-test-service")
  let scope_attributes = [
    ("scope.name", AttributeValue::string(scope.name)),
    ("scope.version", match (scope.version) {
      Some(v) => AttributeValue::string(v)
      None => AttributeValue::string("unknown")
    }),
    ("scope.schema", match (scope.schema_url) {
      Some(url) => AttributeValue::string(url)
      None => AttributeValue::string("")
    })
  ]
  
  // 将scope attributes添加到resource
  let resource_with_scope = Resource::{
    service_name: resource.service_name,
    service_version: resource.service_version,
    telemetry_sdk_name: resource.telemetry_sdk_name,
    telemetry_sdk_version: resource.telemetry_sdk_version,
    attributes: scope_attributes
  }
  
  // 验证resource包含了scope信息
  assert resource_with_scope.attributes.length == 3
  assert match (resource_with_scope.attributes[0].1) {
    StringValue(name) => name == "integration-scope"
    _ => false
  }
}

test "cross module integration: complex attribute conversion" {
  // 创建包含各种类型的AttributeValue
  let complex_attributes = [
    ("string_attr", AttributeValue::string("test_string")),
    ("int_attr", AttributeValue::int(42L)),
    ("float_attr", AttributeValue::float(3.14159)),
    ("bool_attr", AttributeValue::bool(true)),
    ("string_array_attr", AttributeValue::array_string(["a", "b", "c"])),
    ("int_array_attr", AttributeValue::array_int([1L, 2L, 3L])),
    ("float_array_attr", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool_array_attr", AttributeValue::array_bool([true, false, true]))
  ]
  
  // 创建带有复杂属性的Resource
  let resource = Resource::{
    service_name: "complex-attr-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: complex_attributes
  }
  
  // 创建Context并添加属性计数信息
  let context = Context::empty()
  let attr_count_key = create_key("attribute_count")
  let context_with_count = context.with_value(attr_count_key, resource.attributes.length.to_string())
  
  // 验证属性数量
  assert match (context_with_count.get(attr_count_key)) {
    Some(count) => count == "8"
    None => false
  }
  
  // 验证各种属性类型
  assert match (resource.attributes[0].1) {
    StringValue(s) => s == "test_string"
    _ => false
  }
  
  assert match (resource.attributes[1].1) {
    IntValue(i) => i == 42L
    _ => false
  }
  
  assert match (resource.attributes[2].1) {
    FloatValue(f) => f == 3.14159
    _ => false
  }
  
  assert match (resource.attributes[3].1) {
    BoolValue(b) => b == true
    _ => false
  }
  
  assert match (resource.attributes[4].1) {
    ArrayStringValue(arr) => arr.length == 3
    _ => false
  }
  
  assert match (resource.attributes[5].1) {
    ArrayIntValue(arr) => arr.length == 3
    _ => false
  }
  
  assert match (resource.attributes[6].1) {
    ArrayFloatValue(arr) => arr.length == 3
    _ => false
  }
  
  assert match (resource.attributes[7].1) {
    ArrayBoolValue(arr) => arr.length == 3
    _ => false
  }
}
// 资源生命周期测试 - 测试资源的创建、使用和清理

test "resource lifecycle: basic creation and usage" {
  // 测试Resource的基本生命周期
  let resource = Resource::default("lifecycle_test_service")
  
  // 验证初始状态
  assert resource.service_name == "lifecycle_test_service"
  assert resource.service_version == None
  assert resource.telemetry_sdk_name == "azimuth"
  assert resource.telemetry_sdk_version == "0.1.0"
  assert resource.attributes.length == 0
  
  // 使用阶段 - 添加属性
  let resource_with_attrs = Resource::{
    service_name: resource.service_name,
    service_version: Some("1.0.0"),
    telemetry_sdk_name: resource.telemetry_sdk_name,
    telemetry_sdk_version: resource.telemetry_sdk_version,
    attributes: [
      ("lifecycle.phase", AttributeValue::string("active")),
      ("created_at", AttributeValue::string("2023-01-01T00:00:00Z")),
      ("instance_id", AttributeValue::string("instance-123"))
    ]
  }
  
  // 验证使用阶段的状态
  assert resource_with_attrs.service_version == Some("1.0.0")
  assert resource_with_attrs.attributes.length == 3
  
  // 更新阶段 - 修改属性
  let updated_resource = Resource::{
    service_name: resource_with_attrs.service_name,
    service_version: resource_with_attrs.service_version,
    telemetry_sdk_name: resource_with_attrs.telemetry_sdk_name,
    telemetry_sdk_version: resource_with_attrs.telemetry_sdk_version,
    attributes: [
      ("lifecycle.phase", AttributeValue::string("updated")),
      ("created_at", AttributeValue::string("2023-01-01T00:00:00Z")),
      ("instance_id", AttributeValue::string("instance-123")),
      ("updated_at", AttributeValue::string("2023-01-02T00:00:00Z"))
    ]
  }
  
  // 验证更新后的状态
  assert match (updated_resource.attributes[0].1) {
    StringValue(phase) => phase == "updated"
    _ => false
  }
  assert updated_resource.attributes.length == 4
}

test "resource lifecycle: attribute lifecycle management" {
  // 测试属性的生命周期管理
  let resource = Resource::default("attr_lifecycle_service")
  
  // 初始属性
  let initial_attrs = [
    ("static.attr", AttributeValue::string("never_changes")),
    ("dynamic.attr", AttributeValue::string("initial_value")),
    ("timestamp.attr", AttributeValue::string("2023-01-01T00:00:00Z"))
  ]
  
  let resource_with_initial = Resource::{
    service_name: resource.service_name,
    service_version: resource.service_version,
    telemetry_sdk_name: resource.telemetry_sdk_name,
    telemetry_sdk_version: resource.telemetry_sdk_version,
    attributes: initial_attrs
  }
  
  // 属性更新阶段
  let updated_attrs = [
    ("static.attr", AttributeValue::string("never_changes")),  // 保持不变
    ("dynamic.attr", AttributeValue::string("updated_value")),  // 更新值
    ("timestamp.attr", AttributeValue::string("2023-01-02T00:00:00Z")),  // 更新时间戳
    ("new.attr", AttributeValue::string("newly_added"))  // 新增属性
  ]
  
  let resource_with_updated = Resource::{
    service_name: resource_with_initial.service_name,
    service_version: resource_with_initial.service_version,
    telemetry_sdk_name: resource_with_initial.telemetry_sdk_name,
    telemetry_sdk_version: resource_with_initial.telemetry_sdk_version,
    attributes: updated_attrs
  }
  
  // 验证属性生命周期
  assert resource_with_updated.attributes.length == 4
  
  // 验证静态属性保持不变
  let mut found_static = false
  let mut i = 0
  while i < resource_with_updated.attributes.length {
    let (key, value) = resource_with_updated.attributes[i]
    if key == "static.attr" {
      assert match (value) {
        StringValue(v) => v == "never_changes"
        _ => false
      }
      found_static = true
    }
    i = i + 1
  }
  assert found_static
  
  // 验证动态属性已更新
  let mut found_dynamic = false
  i = 0
  while i < resource_with_updated.attributes.length {
    let (key, value) = resource_with_updated.attributes[i]
    if key == "dynamic.attr" {
      assert match (value) {
        StringValue(v) => v == "updated_value"
        _ => false
      }
      found_dynamic = true
    }
    i = i + 1
  }
  assert found_dynamic
}

test "instrumentation scope lifecycle" {
  // 测试InstrumentationScope的生命周期
  let scope = InstrumentationScope::{
    name: "lifecycle_scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/initial-schema")
  }
  
  // 验证初始状态
  assert scope.name == "lifecycle_scope"
  assert match (scope.version) {
    Some(v) => v == "1.0.0"
    None => false
  }
  assert match (scope.schema_url) {
    Some(url) => url == "https://example.com/initial-schema"
    None => false
  }
  
  // 版本升级阶段
  let upgraded_scope = InstrumentationScope::{
    name: scope.name,
    version: Some("2.0.0"),
    schema_url: Some("https://example.com/upgraded-schema")
  }
  
  // 验证升级后的状态
  assert upgraded_scope.name == "lifecycle_scope"
  assert match (upgraded_scope.version) {
    Some(v) => v == "2.0.0"
    None => false
  }
  assert match (upgraded_scope.schema_url) {
    Some(url) => url == "https://example.com/upgraded-schema"
    None => false
  }
  
  // 简化阶段 - 移除可选字段
  let simplified_scope = InstrumentationScope::{
    name: scope.name,
    version: None,
    schema_url: None
  }
  
  // 验证简化后的状态
  assert simplified_scope.name == "lifecycle_scope"
  assert simplified_scope.version == None
  assert simplified_scope.schema_url == None
}

test "context lifecycle management" {
  // 测试Context的生命周期管理
  let context = Context::empty()
  
  // 构建阶段
  let key1 = create_key("phase1")
  let key2 = create_key("phase2")
  let key3 = create_key("phase3")
  
  let context_phase1 = context.with_value(key1, "value1")
  let context_phase2 = context_phase1.with_value(key2, "value2")
  let context_phase3 = context_phase2.with_value(key3, "value3")
  
  // 验证构建阶段的结果
  assert match (context_phase3.get(key1)) {
    Some(value) => value == "value1"
    None => false
  }
  assert match (context_phase3.get(key2)) {
    Some(value) => value == "value2"
    None => false
  }
  assert match (context_phase3.get(key3)) {
    Some(value) => value == "value3"
    None => false
  }
  
  // 重置阶段 - 创建新的context
  let reset_context = Context::empty()
  let reset_key = create_key("reset")
  let context_after_reset = reset_context.with_value(reset_key, "reset_value")
  
  // 验证重置后的状态
  assert match (context_after_reset.get(reset_key)) {
    Some(value) => value == "reset_value"
    None => false
  }
  assert match (context_after_reset.get(key1)) {
    Some(_) => false  // 应该不存在
    None => true
  }
}

test "baggage lifecycle management" {
  // 测试Baggage的生命周期管理
  let baggage = Baggage::empty()
  
  // 积累阶段
  let baggage_phase1 = baggage.with_entry("user.id", "12345")
  let baggage_phase2 = baggage_phase1.with_entry("request.id", "req-67890")
  let baggage_phase3 = baggage_phase2.with_entry("session.id", "session-abc")
  
  // 验证积累阶段的结果
  assert match (baggage_phase3.get("user.id")) {
    Some(value) => value == "12345"
    None => false
  }
  assert match (baggage_phase3.get("request.id")) {
    Some(value) => value == "req-67890"
    None => false
  }
  assert match (baggage_phase3.get("session.id")) {
    Some(value) => value == "session-abc"
    None => false
  }
  
  // 传播阶段 - 模拟baggage在系统中的传播
  let propagated_baggage = baggage_phase3
    .with_entry("propagation.timestamp", "2023-01-01T12:00:00Z")
    .with_entry("propagation.service", "service-A")
  
  // 验证传播后的状态
  assert match (propagated_baggage.get("user.id")) {
    Some(value) => value == "12345"  // 原有条目保持不变
    None => false
  }
  assert match (propagated_baggage.get("propagation.timestamp")) {
    Some(value) => value == "2023-01-01T12:00:00Z"  // 新增传播条目
    None => false
  }
  
  // 清理阶段 - 创建新的baggage（模拟清理）
  let cleaned_baggage = Baggage::empty()
    .with_entry("user.id", "12345")  // 只保留必要的条目
  
  // 验证清理后的状态
  assert match (cleaned_baggage.get("user.id")) {
    Some(value) => value == "12345"
    None => false
  }
  assert match (cleaned_baggage.get("request.id")) {
    Some(_) => false  // 应该被清理
    None => true
  }
}

test "metrics instrument lifecycle" {
  // 测试Metrics Instrument的生命周期
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("lifecycle_meter", Some("1.0.0"), None)
  
  // 创建阶段
  let counter = meter.create_counter("lifecycle_counter", Some("count"), Some("Lifecycle counter"))
  let histogram = meter.create_histogram("lifecycle_histogram", Some("ms"), Some("Lifecycle histogram"))
  let up_down_counter = meter.create_up_down_counter("lifecycle_up_down", None, None)
  let gauge = meter.create_gauge("lifecycle_gauge", Some("value"), Some("Lifecycle gauge"))
  
  // 使用阶段 - 执行measurements
  let attributes = [
    ("lifecycle.phase", AttributeValue::string("active")),
    ("instrument.type", AttributeValue::string("counter"))
  ]
  
  counter.add(1L, Some(attributes))
  counter.add(5L, Some(attributes))
  counter.add(10L, Some(attributes))
  
  histogram.record(100.0, Some(attributes))
  histogram.record(200.0, Some(attributes))
  histogram.record(150.0, Some(attributes))
  
  up_down_counter.add(5L, Some(attributes))
  up_down_counter.add(-2L, Some(attributes))
  up_down_counter.add(10L, Some(attributes))
  
  gauge.record(75.5, Some(attributes))
  gauge.record(80.0, Some(attributes))
  gauge.record(78.2, Some(attributes))
  
  // 更新阶段 - 使用不同的属性
  let updated_attributes = [
    ("lifecycle.phase", AttributeValue::string("updated")),
    ("instrument.type", AttributeValue::string("counter")),
    ("update.reason", AttributeValue::string("configuration_change"))
  ]
  
  counter.add(1L, Some(updated_attributes))
  histogram.record(120.0, Some(updated_attributes))
  up_down_counter.add(3L, Some(updated_attributes))
  gauge.record(82.1, Some(updated_attributes))
  
  // 验证instrument仍然可用
  // 在No-op实现中，这些操作不会失败，但我们需要验证它们可以正常调用
  counter.add(100L, None)
  histogram.record(300.0, None)
  up_down_counter.add(-50L, None)
  gauge.record(90.0, None)
}

test "complex resource lifecycle scenario" {
  // 测试复杂的资源生命周期场景
  let resource = Resource::default("complex_lifecycle_service")
  
  // 初始化阶段
  let init_resource = Resource::{
    service_name: resource.service_name,
    service_version: Some("0.1.0"),
    telemetry_sdk_name: resource.telemetry_sdk_name,
    telemetry_sdk_version: resource.telemetry_sdk_version,
    attributes: [
      ("lifecycle.stage", AttributeValue::string("initializing")),
      ("start_time", AttributeValue::string("2023-01-01T00:00:00Z")),
      ("init.version", AttributeValue::string("0.1.0"))
    ]
  }
  
  // 运行阶段
  let running_resource = Resource::{
    service_name: init_resource.service_name,
    service_version: Some("1.0.0"),
    telemetry_sdk_name: init_resource.telemetry_sdk_name,
    telemetry_sdk_version: init_resource.telemetry_sdk_version,
    attributes: [
      ("lifecycle.stage", AttributeValue::string("running")),
      ("start_time", AttributeValue::string("2023-01-01T00:00:00Z")),
      ("run_time", AttributeValue::string("2023-01-01T01:00:00Z")),
      ("version", AttributeValue::string("1.0.0")),
      ("instance.id", AttributeValue::string("instance-456"))
    ]
  }
  
  // 扩展阶段
  let scaled_resource = Resource::{
    service_name: running_resource.service_name,
    service_version: running_resource.service_version,
    telemetry_sdk_name: running_resource.telemetry_sdk_name,
    telemetry_sdk_version: running_resource.telemetry_sdk_version,
    attributes: [
      ("lifecycle.stage", AttributeValue::string("scaled")),
      ("start_time", AttributeValue::string("2023-01-01T00:00:00Z")),
      ("run_time", AttributeValue::string("2023-01-01T01:00:00Z")),
      ("scale_time", AttributeValue::string("2023-01-01T02:00:00Z")),
      ("version", AttributeValue::string("1.0.0")),
      ("instance.id", AttributeValue::string("instance-456")),
      ("scale.factor", AttributeValue::int(3L)),
      ("node.count", AttributeValue::int(3L))
    ]
  }
  
  // 验证各个阶段的状态
  assert match (init_resource.attributes[0].1) {
    StringValue(stage) => stage == "initializing"
    _ => false
  }
  assert init_resource.attributes.length == 3
  
  assert match (running_resource.attributes[0].1) {
    StringValue(stage) => stage == "running"
    _ => false
  }
  assert running_resource.attributes.length == 5
  
  assert match (scaled_resource.attributes[0].1) {
    StringValue(stage) => stage == "scaled"
    _ => false
  }
  assert scaled_resource.attributes.length == 8
  
  // 验证属性值的演变
  let mut found_version = false
  let mut found_scale_factor = false
  let mut i = 0
  while i < scaled_resource.attributes.length {
    let (key, value) = scaled_resource.attributes[i]
    if key == "version" {
      assert match (value) {
        StringValue(v) => v == "1.0.0"
        _ => false
      }
      found_version = true
    }
    if key == "scale.factor" {
      assert match (value) {
        IntValue(factor) => factor == 3L
        _ => false
      }
      found_scale_factor = true
    }
    i = i + 1
  }
  assert found_version
  assert found_scale_factor
}
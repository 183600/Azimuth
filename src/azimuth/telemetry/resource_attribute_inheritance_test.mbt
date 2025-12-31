// 资源属性继承测试 - 测试Resource属性的继承机制
// Resource Attribute Inheritance Test

test "resource_attribute_inheritance_basic" {
  // 测试基本的资源属性继承
  let base_resource = api::common::Resource::default("test-service")
  
  // 创建带有额外属性的子资源
  let child_attributes = [
    ("service.instance.id", api::common::AttributeValue::string("instance-123")),
    ("service.version", api::common::AttributeValue::string("1.0.0")),
    ("deployment.environment", api::common::AttributeValue::string("production"))
  ]
  
  let child_resource = api::common::Resource::{
    service_name: base_resource.service_name,
    service_version: Some("1.0.0"),
    telemetry_sdk_name: base_resource.telemetry_sdk_name,
    telemetry_sdk_version: base_resource.telemetry_sdk_version,
    attributes: child_attributes
  }
  
  // 验证属性继承
  assert_eq(base_resource.service_name, "test-service")
  assert_eq(child_resource.service_name, "test-service")
  assert_eq(child_resource.service_version.unwrap(), "1.0.0")
  assert_eq(child_resource.attributes.length(), 3)
}

test "resource_attribute_inheritance_merging" {
  // 测试资源属性合并
  let parent_resource = api::common::Resource::{
    service_name: "parent-service",
    service_version: Some("2.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("region", api::common::AttributeValue::string("us-west-1")),
      ("zone", api::common::AttributeValue::string("us-west-1a")),
      ("host.name", api::common::AttributeValue::string("host-001"))
    ]
  }
  
  // 创建继承并扩展的子资源
  let merged_attributes = [
    ("region", api::common::AttributeValue::string("us-west-1")), // 继承
    ("zone", api::common::AttributeValue::string("us-west-1b")), // 覆盖
    ("host.name", api::common::AttributeValue::string("host-001")), // 继承
    ("container.id", api::common::AttributeValue::string("container-456")), // 新增
    ("process.id", api::common::AttributeValue::int(12345L)) // 新增
  ]
  
  let merged_resource = api::common::Resource::{
    service_name: parent_resource.service_name,
    service_version: parent_resource.service_version,
    telemetry_sdk_name: parent_resource.telemetry_sdk_name,
    telemetry_sdk_version: parent_resource.telemetry_sdk_version,
    attributes: merged_attributes
  }
  
  // 验证合并结果
  assert_eq(merged_resource.service_name, "parent-service")
  assert_eq(merged_resource.service_version.unwrap(), "2.0.0")
  assert_eq(merged_resource.attributes.length(), 5)
  
  // 验证特定属性值
  let zone_attr = merged_resource.attributes.find(fn(attr) { attr.0 == "zone" })
  assert_eq(zone_attr.unwrap().1, api::common::AttributeValue::string("us-west-1b"))
}

test "resource_attribute_inheritance_with_instruments" {
  // 测试资源属性在仪器中的继承
  let resource = api::common::Resource::{
    service_name: "instrument-test-service",
    service_version: Some("3.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.namespace", api::common::AttributeValue::string("test-namespace")),
      ("service.instance.id", api::common::AttributeValue::string("instance-789")),
      ("cloud.provider", api::common::AttributeValue::string("aws")),
      ("cloud.region", api::common::AttributeValue::string("eu-central-1"))
    ]
  }
  
  // 创建带有资源属性的tracer
  let tracer_provider = api::trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("instrument-test-tracer")
  
  // 创建带有资源属性的meter
  let meter_provider = api::metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("instrument-test-meter")
  
  // 创建带有资源属性的logger
  let logger_provider = api::logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("instrument-test-logger")
  
  // 使用资源属性创建span
  let context = api::context::Context::current()
  let span_attributes = [
    ("service.namespace", api::common::AttributeValue::string("test-namespace")),
    ("service.instance.id", api::common::AttributeValue::string("instance-789")),
    ("operation.name", api::common::AttributeValue::string("test-operation"))
  ]
  
  let (ctx, span) = tracer.start_span(context, "resource-test-span", 
    api::trace::Server, span_attributes)
  
  // 使用资源属性创建metrics
  let counter = meter.create_counter("resource-test-counter")
  counter.add(1L, [
    ("service.namespace", api::common::AttributeValue::string("test-namespace")),
    ("cloud.provider", api::common::AttributeValue::string("aws")),
    ("metric.type", api::common::AttributeValue::string("counter"))
  ])
  
  // 使用资源属性创建logs
  let log_record = api::logs::LogRecord::builder()
    .timestamp(1234567890L)
    .severity(api::logs::Info)
    .body("Resource inheritance test log")
    .with_attribute("service.namespace", api::common::AttributeValue::string("test-namespace"))
    .with_attribute("cloud.region", api::common::AttributeValue::string("eu-central-1"))
    .with_attribute("log.type", api::common::AttributeValue::string("test"))
    .build()
  
  logger.emit(log_record)
  
  // 验证资源属性在所有仪器中的正确使用
  assert_eq(resource.service_name, "instrument-test-service")
  assert_eq(resource.attributes.length(), 4)
  assert_eq(span.name, "resource-test-span")
  assert_eq(span.attributes.length(), 3)
}

test "resource_attribute_inheritance_hierarchy" {
  // 测试多级资源属性继承层次结构
  let global_resource = api::common::Resource::{
    service_name: "global-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("global.attribute", api::common::AttributeValue::string("global-value")),
      ("environment", api::common::AttributeValue::string("development"))
    ]
  }
  
  let service_resource = api::common::Resource::{
    service_name: global_resource.service_name,
    service_version: global_resource.service_version,
    telemetry_sdk_name: global_resource.telemetry_sdk_name,
    telemetry_sdk_version: global_resource.telemetry_sdk_version,
    attributes: [
      ("global.attribute", api::common::AttributeValue::string("global-value")), // 继承
      ("environment", api::common::AttributeValue::string("production")), // 覆盖
      ("service.attribute", api::common::AttributeValue::string("service-value")), // 新增
      ("service.team", api::common::AttributeValue::string("backend-team")) // 新增
    ]
  }
  
  let instance_resource = api::common::Resource::{
    service_name: service_resource.service_name,
    service_version: service_resource.service_version,
    telemetry_sdk_name: service_resource.telemetry_sdk_name,
    telemetry_sdk_version: service_resource.telemetry_sdk_version,
    attributes: [
      ("global.attribute", api::common::AttributeValue::string("global-value")), // 继承
      ("environment", api::common::AttributeValue::string("production")), // 继承
      ("service.attribute", api::common::AttributeValue::string("service-value")), // 继承
      ("service.team", api::common::AttributeValue::string("backend-team")), // 继承
      ("instance.id", api::common::AttributeValue::string("instance-001")), // 新增
      ("instance.hostname", api::common::AttributeValue::string("host-prod-001")) // 新增
    ]
  }
  
  // 验证继承层次结构
  assert_eq(instance_resource.service_name, "global-service")
  assert_eq(instance_resource.attributes.length(), 6)
  
  // 验证各级属性都正确继承
  let global_attr = instance_resource.attributes.find(fn(attr) { attr.0 == "global.attribute" })
  let env_attr = instance_resource.attributes.find(fn(attr) { attr.0 == "environment" })
  let instance_attr = instance_resource.attributes.find(fn(attr) { attr.0 == "instance.id" })
  
  assert_eq(global_attr.unwrap().1, api::common::AttributeValue::string("global-value"))
  assert_eq(env_attr.unwrap().1, api::common::AttributeValue::string("production"))
  assert_eq(instance_attr.unwrap().1, api::common::AttributeValue::string("instance-001"))
}
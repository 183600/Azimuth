// 资源管理的生命周期综合测试
// 测试资源创建、使用、更新和销毁的完整生命周期

test "resource_creation_and_initialization" {
  // 测试资源的创建和初始化
  let basic_resource = common::Resource::default("test_service")
  
  @assertion.assert_eq(basic_resource.service_name, "test_service")
  @assertion.assert_eq(basic_resource.service_version, None)
  @assertion.assert_eq(basic_resource.telemetry_sdk_name, "azimuth")
  @assertion.assert_eq(basic_resource.telemetry_sdk_version, "0.1.0")
  @assertion.assert_eq(basic_resource.attributes.length(), 0)
  
  // 测试带有版本的资源创建
  let versioned_resource = common::Resource::default("versioned_service")
  let versioned_with_version = versioned_resource.{ service_version: Some("1.0.0") }
  
  @assertion.assert_eq(versioned_with_version.service_name, "versioned_service")
  @assertion.assert_eq(versioned_with_version.service_version, Some("1.0.0"))
  
  // 测试带有属性的完整资源创建
  let full_resource = common::Resource::{
    service_name: "full_service",
    service_version: Some("2.0.0"),
    telemetry_sdk_name: "custom_sdk",
    telemetry_sdk_version: "1.5.0",
    attributes: [
      ("environment", AttributeValue::string("production")),
      ("region", AttributeValue::string("us-west-2")),
      ("instance_id", AttributeValue::string("i-1234567890abcdef0"))
    ]
  }
  
  @assertion.assert_eq(full_resource.service_name, "full_service")
  @assertion.assert_eq(full_resource.service_version, Some("2.0.0"))
  @assertion.assert_eq(full_resource.telemetry_sdk_name, "custom_sdk")
  @assertion.assert_eq(full_resource.telemetry_sdk_version, "1.5.0")
  @assertion.assert_eq(full_resource.attributes.length(), 3)
}

test "resource_attribute_lifecycle" {
  // 测试资源属性的生命周期管理
  let mut resource = common::Resource::default("attr_test_service")
  
  // 初始状态：无属性
  @assertion.assert_eq(resource.attributes.length(), 0)
  
  // 添加单个属性
  resource = resource.{ attributes: [("initial_attr", AttributeValue::string("initial_value"))] }
  @assertion.assert_eq(resource.attributes.length(), 1)
  
  // 添加多个属性
  resource = resource.{ attributes: resource.attributes + [
    ("environment", AttributeValue::string("development")),
    ("version", AttributeValue::string("1.0.0")),
    ("debug", AttributeValue::bool(true))
  ]}
  @assertion.assert_eq(resource.attributes.length(), 4)
  
  // 更新现有属性（通过替换整个属性数组）
  let updated_attributes = resource.attributes.map(fn(attr) {
    if attr[0] == "environment" {
      ("environment", AttributeValue::string("production"))
    } else {
      attr
    }
  })
  resource = resource.{ attributes: updated_attributes }
  
  // 验证属性更新
  let env_attr = resource.attributes.find(fn(attr) { attr[0] == "environment" })
  match env_attr {
    Some((_, AttributeValue::string("production"))) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  // 移除属性（通过过滤）
  let filtered_attributes = resource.attributes.filter(fn(attr) { attr[0] != "debug" })
  resource = resource.{ attributes: filtered_attributes }
  @assertion.assert_eq(resource.attributes.length(), 3)
}

test "resource_merging_and_inheritance" {
  // 测试资源合并和继承
  let base_resource = common::Resource::{
    service_name: "base_service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", AttributeValue::string("base")),
      ("region", AttributeValue::string("us-east-1")),
      ("team", AttributeValue::string("platform"))
    ]
  }
  
  let override_resource = common::Resource::{
    service_name: "override_service",
    service_version: Some("2.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", AttributeValue::string("override")),
      ("datacenter", AttributeValue::string("dc-1")),
      ("feature", AttributeValue::bool(true))
    ]
  }
  
  // 模拟资源合并逻辑：基础资源 + 覆盖资源的属性
  let merged_attributes = base_resource.attributes + override_resource.attributes
  let merged_resource = common::Resource::{
    service_name: override_resource.service_name,
    service_version: override_resource.service_version,
    telemetry_sdk_name: base_resource.telemetry_sdk_name,
    telemetry_sdk_version: base_resource.telemetry_sdk_version,
    attributes: merged_attributes
  }
  
  // 验证合并结果
  @assertion.assert_eq(merged_resource.service_name, "override_service")
  @assertion.assert_eq(merged_resource.service_version, Some("2.0.0"))
  @assertion.assert_eq(merged_resource.attributes.length(), 6)  // 3 + 3
  
  // 验证合并后的属性
  let environment_attr = merged_resource.attributes.find(fn(attr) { attr[0] == "environment" })
  let region_attr = merged_resource.attributes.find(fn(attr) { attr[0] == "region" })
  let datacenter_attr = merged_resource.attributes.find(fn(attr) { attr[0] == "datacenter" })
  
  match environment_attr {
    Some((_, AttributeValue::string("override"))) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match region_attr {
    Some((_, AttributeValue::string("us-east-1"))) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match datacenter_attr {
    Some((_, AttributeValue::string("dc-1"))) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
}

test "resource_validation_and_constraints" {
  // 测试资源验证和约束
  // 测试空服务名称
  let empty_name_resource = common::Resource::default("")
  @assertion.assert_eq(empty_name_resource.service_name, "")
  
  // 测试非常长的服务名称
  let long_name = "a" * 1000
  let long_name_resource = common::Resource::default(long_name)
  @assertion.assert_eq(long_name_resource.service_name.length(), 1000)
  
  // 测试特殊字符的服务名称
  let special_chars_resource = common::Resource::default("service-name_123.test")
  @assertion.assert_eq(special_chars_resource.service_name, "service-name_123.test")
  
  // 测试Unicode服务名称
  let unicode_resource = common::Resource::default("遥测服务")
  @assertion.assert_eq(unicode_resource.service_name, "遥测服务")
  
  // 测试大量属性
  let large_attributes = Array::range_with(0, 1000, 1).map(fn(i) {
    ("attr_" + i.to_string(), AttributeValue::int(i.to_int64()))
  })
  let large_attr_resource = common::Resource::{
    service_name: "large_attr_service",
    service_version: None,
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: large_attributes
  }
  @assertion.assert_eq(large_attr_resource.attributes.length(), 1000)
  
  // 测试非常长的属性值
  let long_value = "b" * 10000
  let long_value_resource = common::Resource::{
    service_name: "long_value_service",
    service_version: None,
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [("long_attr", AttributeValue::string(long_value))]
  }
  
  match long_value_resource.attributes[0][1] {
    AttributeValue::StringValue(s) => @assertion.assert_eq(s.length(), 10000)
    _ => @assertion.assert_true(false)
  }
}

test "resource_lifecycle_with_instrumentation_scope" {
  // 测试资源与仪表化范围的交互
  let resource = common::Resource::default("instrumented_service")
  
  // 创建多个仪表化范围
  let scope1 = common::InstrumentationScope::{
    name: "scope1",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema1")
  }
  
  let scope2 = common::InstrumentationScope::{
    name: "scope2",
    version: Some("2.0.0"),
    schema_url: Some("https://example.com/schema2")
  }
  
  let scope3 = common::InstrumentationScope::{
    name: "scope3",
    version: None,
    schema_url: None
  }
  
  // 验证仪表化范围的创建
  @assertion.assert_eq(scope1.name, "scope1")
  @assertion.assert_eq(scope1.version, Some("1.0.0"))
  @assertion.assert_eq(scope1.schema_url, Some("https://example.com/schema1"))
  
  @assertion.assert_eq(scope2.name, "scope2")
  @assertion.assert_eq(scope2.version, Some("2.0.0"))
  @assertion.assert_eq(scope2.schema_url, Some("https://example.com/schema2"))
  
  @assertion.assert_eq(scope3.name, "scope3")
  @assertion.assert_eq(scope3.version, None)
  @assertion.assert_eq(scope3.schema_url, None)
  
  // 模拟资源与范围的关联
  let resource_scopes = [scope1, scope2, scope3]
  @assertion.assert_eq(resource_scopes.length(), 3)
}

test "resource_cleanup_and_disposal" {
  // 测试资源清理和处置
  let mut resources = []
  
  // 创建多个资源
  for i in 0..10 {
    let resource = common::Resource::{
      service_name: "service_" + i.to_string(),
      service_version: Some("1.0." + i.to_string()),
      telemetry_sdk_name: "azimuth",
      telemetry_sdk_version: "0.1.0",
      attributes: [
        ("index", AttributeValue::int(i.to_int64())),
        ("created_at", AttributeValue::string("2023-01-01"))
      ]
    }
    resources = resources + [resource]
  }
  
  // 验证资源创建
  @assertion.assert_eq(resources.length(), 10)
  
  // 模拟资源清理（通过移除引用）
  resources = []
  @assertion.assert_eq(resources.length(), 0)
  
  // 验证资源可以重新创建
  let new_resource = common::Resource::default("new_service_after_cleanup")
  @assertion.assert_eq(new_resource.service_name, "new_service_after_cleanup")
}

test "resource_state_transitions" {
  // 测试资源状态转换
  let mut resource = common::Resource::default("stateful_service")
  
  // 初始状态
  @assertion.assert_eq(resource.service_name, "stateful_service")
  @assertion.assert_eq(resource.attributes.length(), 0)
  
  // 状态转换：初始化
  resource = resource.{ 
    attributes: [
      ("state", AttributeValue::string("initialized")),
      ("start_time", AttributeValue::string("2023-01-01T00:00:00Z"))
    ]
  }
  
  // 状态转换：配置
  resource = resource.{ 
    attributes: resource.attributes + [
      ("state", AttributeValue::string("configured")),
      ("config_version", AttributeValue::string("1.0.0"))
    ]
  }
  
  // 状态转换：活跃
  resource = resource.{ 
    attributes: resource.attributes.map(fn(attr) {
      if attr[0] == "state" {
        ("state", AttributeValue::string("active"))
      } else {
        attr
      }
    }) + [
      ("active_since", AttributeValue::string("2023-01-01T01:00:00Z"))
    ]
  }
  
  // 状态转换：停用
  resource = resource.{ 
    attributes: resource.attributes.map(fn(attr) {
      if attr[0] == "state" {
        ("state", AttributeValue::string("inactive"))
      } else {
        attr
      }
    }) + [
      ("inactive_since", AttributeValue::string("2023-01-01T02:00:00Z"))
    ]
  }
  
  // 验证最终状态
  @assertion.assert_eq(resource.attributes.length(), 5)
  
  let current_state = resource.attributes.find(fn(attr) { attr[0] == "state" })
  match current_state {
    Some((_, AttributeValue::string("inactive"))) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
}

test "resource_concurrent_access" {
  // 测试资源的并发访问
  let base_resource = common::Resource::default("concurrent_service")
  
  // 模拟并发资源操作
  let num_operations = 100
  let operations = Array::range_with(0, num_operations, 1).map(fn(i) {
    // 创建资源变体（模拟并发操作）
    let variant_resource = base_resource.{ 
      attributes: [
        ("operation_id", AttributeValue::int(i.to_int64())),
        ("thread_id", AttributeValue::string("thread_" + (i % 10).to_string()))
      ]
    }
    variant_resource
  })
  
  // 验证所有操作都产生了有效的资源
  @assertion.assert_eq(operations.length(), num_operations)
  
  // 验证每个资源的唯一性
  let operation_ids = operations.map(fn(resource) {
    match resource.attributes.find(fn(attr) { attr[0] == "operation_id" }) {
      Some((_, AttributeValue::IntValue(id))) => id
      _ => -1L
    }
  })
  
  let unique_ids = operation_ids.to_set()
  @assertion.assert_eq(unique_ids.size(), num_operations)
  
  // 验证线程ID分布
  let thread_ids = operations.map(fn(resource) {
    match resource.attributes.find(fn(attr) { attr[0] == "thread_id" }) {
      Some((_, AttributeValue::StringValue(id))) => id
      _ => ""
    }
  })
  
  let unique_thread_ids = thread_ids.to_set()
  @assertion.assert_eq(unique_thread_ids.size(), 10)  // 10个不同的线程
}
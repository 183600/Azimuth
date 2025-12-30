// 资源管理测试
// 测试遥测资源的创建、管理和生命周期

use azimuth.telemetry.api.trace
use azimuth.telemetry.api.context
use azimuth.telemetry.api.common

test "resource_creation_and_validation" {
  // 测试资源创建和验证
  let basic_resource = common::Resource::default("test-service")
  assert_eq(basic_resource.service_name, "test-service")
  assert_eq(basic_resource.service_version, None)
  assert_eq(basic_resource.telemetry_sdk_name, "azimuth")
  assert_eq(basic_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(basic_resource.attributes.length(), 0)
  
  // 测试完整资源创建
  let full_resource = common::Resource::{
    service_name: "full-service",
    service_version: Some("2.0.0"),
    telemetry_sdk_name: "custom-sdk",
    telemetry_sdk_version: "1.5.0",
    attributes: [
      ("env", common::AttributeValue::string("production")),
      ("region", common::AttributeValue::string("us-west")),
      ("instance_id", common::AttributeValue::string("i-1234567890"))
    ]
  }
  
  assert_eq(full_resource.service_name, "full-service")
  assert_eq(full_resource.service_version, Some("2.0.0"))
  assert_eq(full_resource.telemetry_sdk_name, "custom-sdk")
  assert_eq(full_resource.telemetry_sdk_version, "1.5.0")
  assert_eq(full_resource.attributes.length(), 3)
}

test "resource_attribute_management" {
  // 测试资源属性管理
  let mut resource = common::Resource::default("attr-service")
  
  // 添加属性
  resource = { resource |
    attributes: resource.attributes.push([
      ("version", common::AttributeValue::string("1.0.0")),
      ("port", common::AttributeValue::int(8080L)),
      ("debug", common::AttributeValue::bool(false))
    ])
  }
  
  assert_eq(resource.attributes.length(), 3)
  
  // 验证属性值
  let version_attr = resource.attributes.find(fn(attr) { attr.0 == "version" })
  let port_attr = resource.attributes.find(fn(attr) { attr.0 == "port" })
  let debug_attr = resource.attributes.find(fn(attr) { attr.0 == "debug" })
  
  match version_attr {
    Some((_, common::StringValue(v))) => assert_eq(v, "1.0.0")
    _ => assert_eq(false, true, "Version attribute not found or wrong type")
  }
  
  match port_attr {
    Some((_, common::IntValue(p))) => assert_eq(p, 8080L)
    _ => assert_eq(false, true, "Port attribute not found or wrong type")
  }
  
  match debug_attr {
    Some((_, common::BoolValue(d))) => assert_eq(d, false)
    _ => assert_eq(false, true, "Debug attribute not found or wrong type")
  }
}

test "instrumentation_scope_lifecycle" {
  // 测试仪表化范围生命周期
  let basic_scope = common::InstrumentationScope::{
    name: "basic-instrumentation",
    version: None,
    schema_url: None
  }
  
  assert_eq(basic_scope.name, "basic-instrumentation")
  assert_eq(basic_scope.version, None)
  assert_eq(basic_scope.schema_url, None)
  
  // 测试完整范围
  let full_scope = common::InstrumentationScope::{
    name: "full-instrumentation",
    version: Some("1.2.3"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  assert_eq(full_scope.name, "full-instrumentation")
  assert_eq(full_scope.version, Some("1.2.3"))
  assert_eq(full_scope.schema_url, Some("https://example.com/schema/v1"))
}

test "resource_inheritance" {
  // 测试资源继承
  let parent_resource = common::Resource::{
    service_name: "parent-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("global_attr", common::AttributeValue::string("global_value")),
      ("env", common::AttributeValue::string("development"))
    ]
  }
  
  // 创建子资源，继承父资源属性
  let child_resource = { parent_resource |
    service_name: "child-service",
    service_version: Some("1.1.0"),
    attributes: parent_resource.attributes.push([
      ("child_attr", common::AttributeValue::string("child_value")),
      ("env", common::AttributeValue::string("testing"))  // 覆盖父属性
    ])
  }
  
  // 验证继承
  assert_eq(child_resource.service_name, "child-service")
  assert_eq(child_resource.service_version, Some("1.1.0"))
  assert_eq(child_resource.telemetry_sdk_name, "azimuth")  // 继承
  assert_eq(child_resource.telemetry_sdk_version, "0.1.0")  // 继承
  
  // 验证属性（包括覆盖）
  assert_eq(child_resource.attributes.length(), 3)
  
  let global_attr = child_resource.attributes.find(fn(attr) { attr.0 == "global_attr" })
  let child_attr = child_resource.attributes.find(fn(attr) { attr.0 == "child_attr" })
  let env_attr = child_resource.attributes.filter(fn(attr) { attr.0 == "env" })
  
  assert_eq(global_attr.is_some(), true)
  assert_eq(child_attr.is_some(), true)
  assert_eq(env_attr.length(), 2)  // 父和子的env属性都存在
}

test "resource_cleanup" {
  // 测试资源清理
  let mut resources = []
  
  // 创建多个资源
  for i = 0; i < 10; i = i + 1 {
    let resource = common::Resource::{
      service_name: "service-" + i.to_string(),
      service_version: Some(i.to_string() + ".0.0"),
      telemetry_sdk_name: "azimuth",
      telemetry_sdk_version: "0.1.0",
      attributes: [("id", common::AttributeValue::int(i.to_int64()))]
    }
    resources = resources.push(resource)
  }
  
  assert_eq(resources.length(), 10)
  
  // 模拟资源清理
  resources = resources.filter(fn(resource) {
    match resource.service_version {
      Some(v) => v != "5.0.0"  // 保留版本5.0.0
      None => true
    }
  })
  
  assert_eq(resources.length(), 9)  // 移除了一个
  
  // 验证剩余资源
  for resource in resources {
    assert_eq(resource.service_name.starts_with("service-"), true)
    assert_eq(resource.attributes.length(), 1)
  }
}

test "resource_memory_efficiency" {
  // 测试资源内存效率
  let initial_memory = @sys.get_memory_usage()
  
  // 创建大量资源
  let mut resources = []
  for i = 0; i < 100; i = i + 1 {
    let resource = common::Resource::{
      service_name: "memory-test-service-" + i.to_string(),
      service_version: Some("1.0.0"),
      telemetry_sdk_name: "azimuth",
      telemetry_sdk_version: "0.1.0",
      attributes: [
        ("index", common::AttributeValue::int(i.to_int64())),
        ("batch", common::AttributeValue::string("memory-test"))
      ]
    }
    resources = resources.push(resource)
  }
  
  let after_creation_memory = @sys.get_memory_usage()
  
  // 清理资源引用
  resources = []
  @sys.gc_collect()
  
  let after_cleanup_memory = @sys.get_memory_usage()
  
  // 验证内存使用
  let memory_used = after_creation_memory - initial_memory
  let memory_freed = after_creation_memory - after_cleanup_memory
  
  assert_eq(resources.length(), 0)
  assert_eq(memory_used > 0L, true)
  assert_eq(memory_freed > 0L, true)
}
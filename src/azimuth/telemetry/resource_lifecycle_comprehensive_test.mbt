// 资源管理的生命周期综合测试
// 测试资源创建、使用、更新和销毁的完整生命周期

test "resource_creation_and_initialization" {
  // 测试资源的创建和初始化
  let basic_resource = @azimuth.telemetry.api.common.Resource::default("test_service")
  
  assert_eq(basic_resource.service_name, "test_service")
  assert_eq(basic_resource.telemetry_sdk_name, "azimuth")
  assert_eq(basic_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(basic_resource.attributes.length(), 0)
  
  // 测试带有属性的完整资源创建
  let full_resource = @azimuth.telemetry.api.common.Resource::{
    service_name: "full_service",
    service_version: Some("2.0.0"),
    telemetry_sdk_name: "custom_sdk",
    telemetry_sdk_version: "1.5.0",
    attributes: [
      ("environment", @azimuth.telemetry.api.common.AttributeValue::string("production")),
      ("region", @azimuth.telemetry.api.common.AttributeValue::string("us-west-2"))
    ]
  }
  
  assert_eq(full_resource.service_name, "full_service")
  assert_eq(full_resource.service_version, Some("2.0.0"))
  assert_eq(full_resource.telemetry_sdk_name, "custom_sdk")
  assert_eq(full_resource.attributes.length(), 2)
}
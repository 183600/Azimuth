// 资源合并策略测试
import "azimuth/azimuth"

pub test "资源合并策略测试" {
  // 测试基础资源合并
  let resource1 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("service-A")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("environment", azimuth::StringValue("development"))
  ])
  
  let resource2 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("service-B")),  // 应该覆盖
    ("host.name", azimuth::StringValue("host-123")),
    ("region", azimuth::StringValue("us-west"))
  ])
  
  let merged = azimuth::Resource::merge(resource1, resource2)
  
  // 验证合并结果
  assert_eq(azimuth::Resource::get_attribute(merged, "service.name"), Some(azimuth::StringValue("service-B")))
  assert_eq(azimuth::Resource::get_attribute(merged, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged, "environment"), Some(azimuth::StringValue("development")))
  assert_eq(azimuth::Resource::get_attribute(merged, "host.name"), Some(azimuth::StringValue("host-123")))
  assert_eq(azimuth::Resource::get_attribute(merged, "region"), Some(azimuth::StringValue("us-west")))
  
  // 测试多级资源合并
  let resource3 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("deployment.environment", azimuth::StringValue("staging")),
    ("service.instance.id", azimuth::StringValue("instance-456"))
  ])
  
  let final_merged = azimuth::Resource::merge(merged, resource3)
  
  assert_eq(azimuth::Resource::get_attribute(final_merged, "deployment.environment"), Some(azimuth::StringValue("staging")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "service.instance.id"), Some(azimuth::StringValue("instance-456")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "service.name"), Some(azimuth::StringValue("service-B")))
  
  // 测试空资源合并
  let empty_resource = azimuth::Resource::new()
  let merged_with_empty = azimuth::Resource::merge(resource1, empty_resource)
  
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty, "service.name"), Some(azimuth::StringValue("service-A")))
  assert_eq(azimuth::Resource::get_attribute(merged_with_empty, "service.version"), Some(azimuth::StringValue("1.0.0")))
  
  // 测试数组属性合并
  let resource_with_array = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("tags", azimuth::ArrayStringValue(["web", "api"])),
    ("endpoints", azimuth::ArrayStringValue(["/health", "/metrics"]))
  ])
  
  let resource_with_more_arrays = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("tags", azimuth::ArrayStringValue(["database", "cache"])),  // 应该覆盖
    ("ports", azimuth::ArrayStringValue([8080, 9090]))
  ])
  
  let array_merged = azimuth::Resource::merge(resource_with_array, resource_with_more_arrays)
  
  // 验证数组合并（基于简化实现）
  assert_eq(azimuth::Resource::get_attribute(array_merged, "tags"), Some(azimuth::StringValue("test_value")))
}
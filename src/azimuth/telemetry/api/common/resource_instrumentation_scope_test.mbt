// 资源结构体测试用例

test "resource_default_creation" {
  // 测试资源默认创建功能
  
  let service_name = "payment-service"
  let resource = Resource::default(service_name)
  
  // 验证基本属性
  assert_eq(resource.service_name, "payment-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  
  // 验证可选属性
  match resource.service_version {
    Some(version) => assert_eq(false, true) // 默认应该是None
    None => assert_eq(true, true)
  }
  
  // 验证属性数组
  assert_eq(resource.attributes.length(), 0)
}

test "resource_custom_creation" {
  // 测试资源自定义创建功能
  
  let service_name = "order-processing"
  let service_version = Some("2.1.0")
  let telemetry_sdk_name = "azimuth"
  let telemetry_sdk_version = "0.1.0"
  
  let attributes = [
    ("service.namespace", AttributeValue::string("ecommerce")),
    ("service.instance.id", AttributeValue::string("instance-12345")),
    ("deployment.environment", AttributeValue::string("production"))
  ]
  
  let resource = Resource::{
    service_name,
    service_version,
    telemetry_sdk_name,
    telemetry_sdk_version,
    attributes
  }
  
  // 验证基本属性
  assert_eq(resource.service_name, "order-processing")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  
  // 验证可选属性
  match resource.service_version {
    Some(version) => assert_eq(version, "2.1.0")
    None => assert_eq(false, true)
  }
  
  // 验证属性数组
  assert_eq(resource.attributes.length(), 3)
  assert_eq(resource.attributes[0].0, "service.namespace")
  assert_eq(resource.attributes[2].0, "deployment.environment")
}

test "resource_attribute_operations" {
  // 测试资源属性操作
  
  let service_name = "api-gateway"
  let mut resource = Resource::default(service_name)
  
  // 添加属性
  let new_attributes = [
    ("http.port", AttributeValue::int(8080L)),
    ("http.scheme", AttributeValue::string("https")),
    ("service.version", AttributeValue::string("1.0.0")),
    ("service.tier", AttributeValue::string("critical"))
  ]
  
  // 更新资源属性
  resource.attributes = new_attributes
  
  // 验证属性数量
  assert_eq(resource.attributes.length(), 4)
  
  // 验证具体属性
  assert_eq(resource.attributes[0].0, "http.port")
  match resource.attributes[0].1 {
    IntValue(value) => assert_eq(value, 8080L)
    _ => assert_eq(false, true)
  }
  
  assert_eq(resource.attributes[1].0, "http.scheme")
  match resource.attributes[1].1 {
    StringValue(value) => assert_eq(value, "https")
    _ => assert_eq(false, true)
  }
  
  assert_eq(resource.attributes[2].0, "service.version")
  match resource.attributes[2].1 {
    StringValue(value) => assert_eq(value, "1.0.0")
    _ => assert_eq(false, true)
  }
}

test "resource_complex_attributes" {
  // 测试资源复杂属性
  
  let service_name = "data-processor"
  let mut resource = Resource::default(service_name)
  
  // 添加复杂属性
  let complex_attributes = [
    ("service.tags", AttributeValue::array_string(["batch", "scheduled", "high-priority"])),
    ("service.capabilities", AttributeValue::array_string["validation", "transformation", "export"]),
    ("worker.count", AttributeValue::int(10L)),
    ("memory.limit.mb", AttributeValue::float(2048.0)),
    ("auto.scaling", AttributeValue::bool(true)),
    ("retry.policy.max_attempts", AttributeValue::int(3L))
  ]
  
  resource.attributes = complex_attributes
  
  // 验证复杂属性
  assert_eq(resource.attributes.length(), 6)
  
  // 验证字符串数组属性
  assert_eq(resource.attributes[0].0, "service.tags")
  match resource.attributes[0].1 {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "batch")
      assert_eq(values[2], "high-priority")
    }
    _ => assert_eq(false, true)
  }
  
  // 验证整数属性
  assert_eq(resource.attributes[2].0, "worker.count")
  match resource.attributes[2].1 {
    IntValue(value) => assert_eq(value, 10L)
    _ => assert_eq(false, true)
  }
  
  // 验证浮点数属性
  assert_eq(resource.attributes[3].0, "memory.limit.mb")
  match resource.attributes[3].1 {
    FloatValue(value) => assert_eq(value, 2048.0)
    _ => assert_eq(false, true)
  }
  
  // 验证布尔属性
  assert_eq(resource.attributes[4].0, "auto.scaling")
  match resource.attributes[4].1 {
    BoolValue(value) => assert_eq(value, true)
    _ => assert_eq(false, true)
  }
}

test "resource_metadata_validation" {
  // 测试资源元数据验证
  
  let service_name = "auth-service"
  let mut resource = Resource::default(service_name)
  
  // 添加验证相关的属性
  let validation_attributes = [
    ("service.name", AttributeValue::string("auth-service")),
    ("service.version", AttributeValue::string("3.2.1")),
    ("service.namespace", AttributeValue::string("security")),
    ("service.instance.id", AttributeValue::string("auth-7f4a2b1c")),
    ("deployment.environment", AttributeValue::string("production")),
    ("host.name", AttributeValue::string("auth-prod-01.example.com")),
    ("host.ip", AttributeValue::string("10.0.1.100")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.executable.name", AttributeValue::string("auth-service")),
    ("process.executable.path", AttributeValue::string("/opt/services/auth-service/bin/auth-service"))
  ]
  
  resource.attributes = validation_attributes
  
  // 验证元数据完整性
  assert_eq(resource.attributes.length(), 10)
  
  // 验证服务名称一致性
  assert_eq(resource.service_name, "auth-service")
  assert_eq(resource.attributes[0].0, "service.name")
  match resource.attributes[0].1 {
    StringValue(value) => assert_eq(value, "auth-service")
    _ => assert_eq(false, true)
  }
  
  // 验证版本格式
  assert_eq(resource.attributes[1].0, "service.version")
  match resource.attributes[1].1 {
    StringValue(value) => {
      assert_eq(value.has_prefix("3."), true)
      assert_eq(value.contains("."), true)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证实例ID格式
  assert_eq(resource.attributes[3].0, "service.instance.id")
  match resource.attributes[3].1 {
    StringValue(value) => {
      assert_eq(value.has_prefix("auth-"), true)
      assert_eq(value.length(), 15)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证进程ID
  assert_eq(resource.attributes[7].0, "process.pid")
  match resource.attributes[7].1 {
    IntValue(value) => {
      assert_eq(value > 0L, true)
      assert_eq(value < 100000L, true)
    }
    _ => assert_eq(false, true)
  }
}

test "resource_edge_cases" {
  // 测试资源边界情况
  
  // 空服务名称
  let empty_service_name = ""
  let empty_resource = Resource::default(empty_service_name)
  assert_eq(empty_resource.service_name, "")
  assert_eq(empty_resource.attributes.length(), 0)
  
  // 长服务名称
  let long_service_name = "very-long-service-name-with-many-components-and-dashes"
  let long_resource = Resource::default(long_service_name)
  assert_eq(long_resource.service_name.length(), 58)
  
  // 包含特殊字符的服务名称
  let special_service_name = "service_with_underscores_and.dots"
  let special_resource = Resource::default(special_service_name)
  assert_eq(special_resource.service_name.contains("_"), true)
  assert_eq(special_resource.service_name.contains("."), true)
  
  // 带有版本号的资源
  let service_with_version = "database-service"
  let versioned_resource = Resource::{
    service_name: service_with_version,
    service_version: Some("5.7.23"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: []
  }
  
  match versioned_resource.service_version {
    Some(version) => assert_eq(version, "5.7.23")
    None => assert_eq(false, true)
  }
}
// 遥测资源属性测试用例

test "resource_attributes_creation" {
  // 测试资源属性创建
  
  let service_name = "user-service"
  let service_version = "2.1.0"
  let environment = "production"
  let region = "us-west-2"
  
  // 创建资源属性
  let resource_attributes = [
    ("service.name", AttributeValue::string(service_name)),
    ("service.version", AttributeValue::string(service_version)),
    ("deployment.environment", AttributeValue::string(environment)),
    ("cloud.region", AttributeValue::string(region)),
    ("service.instance.id", AttributeValue::string("instance-abc123")),
    ("process.pid", AttributeValue::int(12345)),
    ("process.cpu.count", AttributeValue::int(4)),
    ("process.memory.rss", AttributeValue::int(52428800)),
    ("service.startup.time", AttributeValue::float(1640995200.123)),
    ("service.healthy", AttributeValue::bool(true))
  ]
  
  // 验证属性数量
  assert_eq(resource_attributes.length(), 10)
  
  // 验证字符串属性
  assert_eq(resource_attributes[0].0, "service.name")
  assert_eq(resource_attributes[0].1, AttributeValue::string("user-service"))
  
  // 验证整数属性
  assert_eq(resource_attributes[5].0, "process.pid")
  assert_eq(resource_attributes[5].1, AttributeValue::int(12345))
  
  // 验证浮点数属性
  assert_eq(resource_attributes[8].0, "service.startup.time")
  assert_eq(resource_attributes[8].1, AttributeValue::float(1640995200.123))
  
  // 验证布尔属性
  assert_eq(resource_attributes[9].0, "service.healthy")
  assert_eq(resource_attributes[9].1, AttributeValue::bool(true))
}

test "resource_attributes_array_types" {
  // 测试数组类型的资源属性
  
  let service_tags = ["web", "api", "microservice"]
  let endpoint_paths = ["/users", "/orders", "/products"]
  let response_times = [125.5, 200.3, 89.7]
  let endpoint_enabled = [true, true, false]
  
  // 创建数组类型属性
  let array_attributes = [
    ("service.tags", AttributeValue::array_string(service_tags)),
    ("service.endpoints", AttributeValue::array_string(endpoint_paths)),
    ("performance.response_times", AttributeValue::array_float(response_times)),
    ("service.endpoints.enabled", AttributeValue::array_bool(endpoint_enabled))
  ]
  
  // 验证数组属性
  assert_eq(array_attributes.length(), 4)
  assert_eq(array_attributes[0].0, "service.tags")
  assert_eq(array_attributes[0].1, AttributeValue::array_string(["web", "api", "microservice"]))
  
  assert_eq(array_attributes[1].0, "service.endpoints")
  assert_eq(array_attributes[1].1, AttributeValue::array_string(["/users", "/orders", "/products"]))
  
  assert_eq(array_attributes[2].0, "performance.response_times")
  assert_eq(array_attributes[2].1, AttributeValue::array_float([125.5, 200.3, 89.7]))
  
  assert_eq(array_attributes[3].0, "service.endpoints.enabled")
  assert_eq(array_attributes[3].1, AttributeValue::array_bool([true, true, false]))
}

test "resource_attributes_filtering" {
  // 测试资源属性过滤
  
  let all_attributes = [
    ("service.name", AttributeValue::string("order-service")),
    ("service.version", AttributeValue::string("1.5.2")),
    ("deployment.environment", AttributeValue::string("staging")),
    ("cloud.region", AttributeValue::string("eu-central-1")),
    ("service.instance.id", AttributeValue::string("instance-def456")),
    ("process.pid", AttributeValue::int(54321)),
    ("process.cpu.count", AttributeValue::int(8)),
    ("process.memory.rss", AttributeValue::int(104857600)),
    ("service.startup.time", AttributeValue::float(1640995300.456)),
    ("service.healthy", AttributeValue::bool(true)),
    ("custom.attribute", AttributeValue::string("custom-value")),
    ("debug.mode", AttributeValue::bool(false))
  ]
  
  // 过滤出服务相关属性
  let service_attributes = []
  let mut i = 0
  while i < all_attributes.length() {
    if all_attributes[i].0.has_prefix("service") {
      service_attributes.push(all_attributes[i])
    }
    i = i + 1
  }
  
  // 验证过滤结果
  assert_eq(service_attributes.length(), 5)
  assert_eq(service_attributes[0].0, "service.name")
  assert_eq(service_attributes[4].0, "service.healthy")
  
  // 过滤出进程相关属性
  let process_attributes = []
  i = 0
  while i < all_attributes.length() {
    if all_attributes[i].0.has_prefix("process") {
      process_attributes.push(all_attributes[i])
    }
    i = i + 1
  }
  
  // 验证过滤结果
  assert_eq(process_attributes.length(), 3)
  assert_eq(process_attributes[0].0, "process.pid")
  assert_eq(process_attributes[2].0, "process.memory.rss")
}

test "resource_attributes_serialization" {
  // 测试资源属性序列化
  
  let resource_attrs = [
    ("service.name", AttributeValue::string("payment-service")),
    ("service.version", AttributeValue::string("3.0.1")),
    ("deployment.environment", AttributeValue::string("production")),
    ("service.instance.id", AttributeValue::string("instance-xyz789")),
    ("service.replica.count", AttributeValue::int(3)),
    ("service.healthy", AttributeValue::bool(true))
  ]
  
  // 简单序列化为键值对字符串
  let mut serialized = "{"
  let mut i = 0
  while i < resource_attrs.length() {
    let (key, value) = resource_attrs[i]
    serialized = serialized + "\"" + key + "\":"
    
    match value {
      AttributeValue::StringValue(s) => serialized = serialized + "\"" + s + "\""
      AttributeValue::IntValue(n) => serialized = serialized + n.to_string()
      AttributeValue::BoolValue(b) => serialized = serialized + b.to_string()
      _ => serialized = serialized + "\"unsupported\""
    }
    
    if i < resource_attrs.length() - 1 {
      serialized = serialized + ","
    }
    i = i + 1
  }
  serialized = serialized + "}"
  
  // 验证序列化结果
  assert_eq(serialized.has_prefix("{"), true)
  assert_eq(serialized.has_suffix("}"), true)
  assert_eq(serialized.contains("\"service.name\":\"payment-service\""), true)
  assert_eq(serialized.contains("\"service.replica.count\":3"), true)
  assert_eq(serialized.contains("\"service.healthy\":true"), true)
}

test "resource_attributes_merging" {
  // 测试资源属性合并
  
  let base_attributes = [
    ("service.name", AttributeValue::string("base-service")),
    ("service.version", AttributeValue::string("1.0.0")),
    ("deployment.environment", AttributeValue::string("development")),
    ("cloud.region", AttributeValue::string("us-east-1"))
  ]
  
  let override_attributes = [
    ("service.version", AttributeValue::string("2.0.0")),
    ("deployment.environment", AttributeValue::string("production")),
    ("service.instance.id", AttributeValue::string("instance-override123")),
    ("feature.flag", AttributeValue::bool(true))
  ]
  
  // 合并属性（后者覆盖前者）
  let merged_attributes = base_attributes
  let mut i = 0
  while i < override_attributes.length() {
    let (key, value) = override_attributes[i]
    
    // 查找是否已存在相同key
    let mut found = false
    let mut j = 0
    while j < merged_attributes.length() {
      if merged_attributes[j].0 == key {
        merged_attributes[j] = (key, value)
        found = true
        break
      }
      j = j + 1
    }
    
    // 如果不存在，添加新属性
    if not found {
      merged_attributes.push((key, value))
    }
    i = i + 1
  }
  
  // 验证合并结果
  assert_eq(merged_attributes.length(), 6)
  
  // 验证覆盖的属性
  let mut found_version = false
  let mut found_env = false
  let mut found_instance = false
  let mut found_flag = false
  
  i = 0
  while i < merged_attributes.length() {
    match merged_attributes[i].0 {
      "service.version" => {
        assert_eq(merged_attributes[i].1, AttributeValue::string("2.0.0"))
        found_version = true
      }
      "deployment.environment" => {
        assert_eq(merged_attributes[i].1, AttributeValue::string("production"))
        found_env = true
      }
      "service.instance.id" => {
        assert_eq(merged_attributes[i].1, AttributeValue::string("instance-override123"))
        found_instance = true
      }
      "feature.flag" => {
        assert_eq(merged_attributes[i].1, AttributeValue::bool(true))
        found_flag = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_version, true)
  assert_eq(found_env, true)
  assert_eq(found_instance, true)
  assert_eq(found_flag, true)
}
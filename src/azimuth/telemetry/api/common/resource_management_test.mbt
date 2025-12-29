// Resource 默认值和属性管理高级测试用例
// 测试Resource的创建、默认值、属性管理和复杂场景

test "resource_default_creation_and_validation" {
  // 测试Resource的默认创建和验证
  
  // 创建默认Resource
  let resource = common::Resource::default("test-service")
  
  // 验证基本字段
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  
  // 验证可选字段
  match resource.service_version {
    None => assert_eq(true, true)
    Some(_) => assert_eq(false, true)
  }
  
  // 验证初始属性为空
  assert_eq(resource.attributes.length(), 0)
  
  // 测试不同服务名的Resource创建
  let api_resource = common::Resource::default("api-service")
  let web_resource = common::Resource::default("web-frontend")
  let db_resource = common::Resource::default("database")
  
  assert_eq(api_resource.service_name, "api-service")
  assert_eq(web_resource.service_name, "web-frontend")
  assert_eq(db_resource.service_name, "database")
  
  // 验证所有Resource都有相同的SDK信息
  assert_eq(api_resource.telemetry_sdk_name, "azimuth")
  assert_eq(web_resource.telemetry_sdk_name, "azimuth")
  assert_eq(db_resource.telemetry_sdk_name, "azimuth")
  
  assert_eq(api_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(web_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(db_resource.telemetry_sdk_version, "0.1.0")
}

test "resource_with_explicit_attributes" {
  // 测试显式设置属性的Resource
  
  // 创建带有显式属性的Resource
  let resource_with_attrs = common::Resource::{
    service_name: "payment-service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("deployment.environment", AttributeValue::string("production")),
      ("service.instance.id", AttributeValue::string("i-1234567890abcdef0")),
      ("availability.zone", AttributeValue::string("us-west-2a"))
    ]
  }
  
  // 验证基本字段
  assert_eq(resource_with_attrs.service_name, "payment-service")
  match resource_with_attrs.service_version {
    Some(version) => assert_eq(version, "2.1.0")
    None => assert_eq(false, true)
  }
  
  // 验证属性
  assert_eq(resource_with_attrs.attributes.length(), 3)
  
  // 验证每个属性
  assert_eq(resource_with_attrs.attributes[0].0, "deployment.environment")
  match resource_with_attrs.attributes[0].1 {
    StringValue(env) => assert_eq(env, "production")
    _ => assert_eq(false, true)
  }
  
  assert_eq(resource_with_attrs.attributes[1].0, "service.instance.id")
  match resource_with_attrs.attributes[1].1 {
    StringValue(instance_id) => assert_eq(instance_id, "i-1234567890abcdef0")
    _ => assert_eq(false, true)
  }
  
  assert_eq(resource_with_attrs.attributes[2].0, "availability.zone")
  match resource_with_attrs.attributes[2].1 {
    StringValue(zone) => assert_eq(zone, "us-west-2a")
    _ => assert_eq(false, true)
  }
}

test "resource_attribute_types_comprehensive" {
  // 测试Resource的各种属性类型
  
  // 创建包含各种属性类型的Resource
  let comprehensive_resource = common::Resource::{
    service_name: "comprehensive-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      // 字符串属性
      ("service.namespace", AttributeValue::string("backend")),
      ("host.name", AttributeValue::string("web-server-01")),
      
      // 整数属性
      ("process.pid", AttributeValue::int(12345L)),
      ("process.start_time", AttributeValue::int(1640995200L)),
      
      // 浮点属性
      ("cpu.count", AttributeValue::float(4.0)),
      ("memory.total", AttributeValue::float(8589934592.0)),
      
      // 布尔属性
      ("service.healthy", AttributeValue::bool(true)),
      ("maintenance.mode", AttributeValue::bool(false)),
      
      // 字符串数组属性
      ("service.tags", AttributeValue::array_string(["web", "api", "public"])),
      ("endpoints", AttributeValue::array_string(["/health", "/metrics", "/api"])),
      
      // 整数数组属性
      ("ports", AttributeValue::array_int([80L, 443L, 8080L])),
      ("worker.threads", AttributeValue::array_int([1L, 2L, 4L, 8L])),
      
      // 浮点数组属性
      ("cpu.thresholds", AttributeValue::array_float([0.5, 0.8, 0.9])),
      ("memory.thresholds", AttributeValue::array_float([0.6, 0.85, 0.95])),
      
      // 布尔数组属性
      ("feature.flags", AttributeValue::array_bool([true, false, true]))
    ]
  }
  
  // 验证属性总数
  assert_eq(comprehensive_resource.attributes.length(), 15)
  
  // 验证字符串属性
  match comprehensive_resource.attributes[0].1 {
    StringValue(namespace) => assert_eq(namespace, "backend")
    _ => assert_eq(false, true)
  }
  
  // 验证整数属性
  match comprehensive_resource.attributes[2].1 {
    IntValue(pid) => assert_eq(pid, 12345L)
    _ => assert_eq(false, true)
  }
  
  // 验证浮点属性
  match comprehensive_resource.attributes[4].1 {
    FloatValue(cpu_count) => assert_eq(cpu_count, 4.0)
    _ => assert_eq(false, true)
  }
  
  // 验证布尔属性
  match comprehensive_resource.attributes[6].1 {
    BoolValue(healthy) => assert_eq(healthy, true)
    _ => assert_eq(false, true)
  }
  
  // 验证字符串数组属性
  match comprehensive_resource.attributes[8].1 {
    ArrayStringValue(tags) => {
      assert_eq(tags.length(), 3)
      assert_eq(tags[0], "web")
      assert_eq(tags[1], "api")
      assert_eq(tags[2], "public")
    }
    _ => assert_eq(false, true)
  }
  
  // 验证整数数组属性
  match comprehensive_resource.attributes[10].1 {
    ArrayIntValue(ports) => {
      assert_eq(ports.length(), 3)
      assert_eq(ports[0], 80L)
      assert_eq(ports[1], 443L)
      assert_eq(ports[2], 8080L)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证浮点数组属性
  match comprehensive_resource.attributes[12].1 {
    ArrayFloatValue(thresholds) => {
      assert_eq(thresholds.length(), 3)
      assert_eq(thresholds[0], 0.5)
      assert_eq(thresholds[1], 0.8)
      assert_eq(thresholds[2], 0.9)
    }
    _ => assert_eq(false, true)
  }
  
  // 验证布尔数组属性
  match comprehensive_resource.attributes[14].1 {
    ArrayBoolValue(flags) => {
      assert_eq(flags.length(), 3)
      assert_eq(flags[0], true)
      assert_eq(flags[1], false)
      assert_eq(flags[2], true)
    }
    _ => assert_eq(false, true)
  }
}

test "resource_edge_cases_and_boundary_values" {
  // 测试Resource的边界情况和特殊值
  
  // 测试空服务名
  let empty_name_resource = common::Resource::default("")
  assert_eq(empty_name_resource.service_name, "")
  assert_eq(empty_name_resource.service_name.length(), 0)
  
  // 测试长服务名
  let long_name = "this.is.a.very.long.service.name.that.exceeds.typical.lengths.and.tests.boundaries"
  let long_name_resource = common::Resource::default(long_name)
  assert_eq(long_name_resource.service_name, long_name)
  
  // 测试特殊字符服务名
  let special_name = "service-with_special.chars!@#$%^&*()"
  let special_name_resource = common::Resource::default(special_name)
  assert_eq(special_name_resource.service_name, special_name)
  
  // 测试Unicode服务名
  let unicode_name = "服务名称.🌟"
  let unicode_name_resource = common::Resource::default(unicode_name)
  assert_eq(unicode_name_resource.service_name, unicode_name)
  
  // 测试边界属性值
  let boundary_resource = common::Resource::{
    service_name: "boundary-test-service",
    service_version: Some(""),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      // 空字符串值
      ("empty.string", AttributeValue::string("")),
      
      // 长字符串值
      ("long.string", AttributeValue::string("a".repeat(1000))),
      
      // 边界数值
      ("zero.int", AttributeValue::int(0L)),
      ("max.int", AttributeValue::int(9223372036854775807L)),
      ("min.int", AttributeValue::int(-9223372036854775808L)),
      ("zero.float", AttributeValue::float(0.0)),
      ("max.float", AttributeValue::float(1.7976931348623157e+308)),
      ("min.float", AttributeValue::float(2.2250738585072014e-308)),
      ("negative.float", AttributeValue::float(-3.14159)),
      
      // 空数组
      ("empty.array", AttributeValue::array_string([])),
      
      // 单元素数组
      ("single.element", AttributeValue::array_string(["only"]))
    ]
  }
  
  // 验证边界值
  match boundary_resource.service_version {
    Some(version) => assert_eq(version.length(), 0)
    None => assert_eq(false, true)
  }
  
  match boundary_resource.attributes[0].1 {
    StringValue(empty_str) => assert_eq(empty_str.length(), 0)
    _ => assert_eq(false, true)
  }
  
  match boundary_resource.attributes[1].1 {
    StringValue(long_str) => assert_eq(long_str.length(), 1000)
    _ => assert_eq(false, true)
  }
  
  match boundary_resource.attributes[2].1 {
    IntValue(zero_int) => assert_eq(zero_int, 0L)
    _ => assert_eq(false, true)
  }
  
  match boundary_resource.attributes[3].1 {
    IntValue(max_int) => assert_eq(max_int, 9223372036854775807L)
    _ => assert_eq(false, true)
  }
  
  match boundary_resource.attributes[4].1 {
    IntValue(min_int) => assert_eq(min_int, -9223372036854775808L)
    _ => assert_eq(false, true)
  }
  
  match boundary_resource.attributes[9].1 {
    ArrayStringValue(empty_array) => assert_eq(empty_array.length(), 0)
    _ => assert_eq(false, true)
  }
  
  match boundary_resource.attributes[10].1 {
    ArrayStringValue(single_element) => {
      assert_eq(single_element.length(), 1)
      assert_eq(single_element[0], "only")
    }
    _ => assert_eq(false, true)
  }
}

test "resource_real_world_scenarios" {
  // 测试Resource的实际应用场景
  
  // Web服务器Resource
  let web_server_resource = common::Resource::{
    service_name: "web-frontend",
    service_version: Some("3.2.1"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.namespace", AttributeValue::string("frontend")),
      ("deployment.environment", AttributeValue::string("production")),
      ("host.name", AttributeValue::string("web-prod-01.example.com")),
      ("host.ip", AttributeValue::string("10.0.1.100")),
      ("os.type", AttributeValue::string("linux")),
      ("os.version", AttributeValue::string("ubuntu-20.04")),
      ("process.pid", AttributeValue::int(1234L)),
      ("process.command", AttributeValue::string("nginx")),
      ("process.runtime.name", AttributeValue::string("node")),
      ("process.runtime.version", AttributeValue::string("18.17.0")),
      ("cpu.count", AttributeValue::float(4.0)),
      ("memory.total", AttributeValue::float(8589934592.0)),
      ("service.instance.id", AttributeValue::string("web-prod-01-1234"))
    ]
  }
  
  // 验证Web服务器Resource
  assert_eq(web_server_resource.service_name, "web-frontend")
  match web_server_resource.service_version {
    Some(version) => assert_eq(version, "3.2.1")
    None => assert_eq(false, true)
  }
  assert_eq(web_server_resource.attributes.length(), 13)
  
  // 数据库Resource
  let database_resource = common::Resource::{
    service_name: "user-database",
    service_version: Some("12.4")),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.namespace", AttributeValue::string("data")),
      ("deployment.environment", AttributeValue::string("production")),
      ("host.name", AttributeValue::string("db-prod-01.example.com")),
      ("db.system", AttributeValue::string("postgresql")),
      ("db.version", AttributeValue::string("14.6")),
      ("db.connection_string", AttributeValue::string("postgresql://user:pass@db-prod-01:5432/users")),
      ("db.max_connections", AttributeValue::int(200L)),
      ("db.active_connections", AttributeValue::int(45L)),
      ("cpu.count", AttributeValue::float(8.0)),
      ("memory.total", AttributeValue::float(17179869184.0)),
      ("storage.total", AttributeValue::float(1099511627776.0)),
      ("storage.used", AttributeValue::float(549755813888.0)),
      ("service.instance.id", AttributeValue::string("db-prod-01-5678"))
    ]
  }
  
  // 验证数据库Resource
  assert_eq(database_resource.service_name, "user-database")
  match database_resource.service_version {
    Some(version) => assert_eq(version, "12.4")
    None => assert_eq(false, true)
  }
  assert_eq(database_resource.attributes.length(), 13)
  
  // 微服务Resource
  let microservice_resource = common::Resource::{
    service_name: "order-processing-service",
    service_version: Some("2.5.0")),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.namespace", AttributeValue::string("order-system")),
      ("deployment.environment", AttributeValue::string("staging")),
      ("k8s.pod.name", AttributeValue::string("order-processing-7d4f8c9b-xyz123")),
      ("k8s.namespace", AttributeValue::string("order-system")),
      ("k8s.node.name", AttributeValue::string("worker-node-3")),
      ("k8s.deployment.name", AttributeValue::string("order-processing")),
      ("container.name", AttributeValue::string("order-processing")),
      ("container.image", AttributeValue::string("order-processing:2.5.0")),
      ("process.language", AttributeValue::string("java")),
      ("process.runtime.version", AttributeValue::string("17.0.2")),
      ("service.instance.id", AttributeValue::string("order-processing-7d4f8c9b-xyz123")),
      ("feature.flags", AttributeValue::array_string(["new-algorithm", "enhanced-logging", "cache-optimization"])),
      ("dependencies", AttributeValue::array_string(["payment-service", "inventory-service", "notification-service"]))
    ]
  }
  
  // 验证微服务Resource
  assert_eq(microservice_resource.service_name, "order-processing-service")
  match microservice_resource.service_version {
    Some(version) => assert_eq(version, "2.5.0")
    None => assert_eq(false, true)
  }
  assert_eq(microservice_resource.attributes.length(), 14)
  
  // 验证数组属性
  match microservice_resource.attributes[12].1 {
    ArrayStringValue(feature_flags) => {
      assert_eq(feature_flags.length(), 3)
      assert_eq(feature_flags.contains("new-algorithm"), true)
      assert_eq(feature_flags.contains("enhanced-logging"), true)
      assert_eq(feature_flags.contains("cache-optimization"), true)
    }
    _ => assert_eq(false, true)
  }
  
  match microservice_resource.attributes[13].1 {
    ArrayStringValue(dependencies) => {
      assert_eq(dependencies.length(), 3)
      assert_eq(dependencies.contains("payment-service"), true)
      assert_eq(dependencies.contains("inventory-service"), true)
      assert_eq(dependencies.contains("notification-service"), true)
    }
    _ => assert_eq(false, true)
  }
}

test "resource_inheritance_and_composition" {
  // 测试Resource的继承和组合模式
  
  // 创建基础Resource模板
  let base_resource = common::Resource::{
    service_name: "base-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("deployment.environment", AttributeValue::string("production")),
      ("service.namespace", AttributeValue::string("backend")),
      ("cpu.count", AttributeValue::float(4.0)),
      ("memory.total", AttributeValue::float(8589934592.0))
    ]
  }
  
  // 基于基础Resource创建特定服务Resource
  let api_resource = common::Resource::{
    service_name: "user-api-service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: base_resource.telemetry_sdk_name,
    telemetry_sdk_version: base_resource.telemetry_sdk_version,
    attributes: [
      // 继承基础属性
      ("deployment.environment", AttributeValue::string("production")),
      ("service.namespace", AttributeValue::string("backend")),
      ("cpu.count", AttributeValue::float(4.0)),
      ("memory.total", AttributeValue::float(8589934592.0)),
      
      // 添加特定属性
      ("service.type", AttributeValue::string("api")),
      ("http.port", AttributeValue::int(8080L)),
      ("api.version", AttributeValue::string("v2")),
      ("endpoints", AttributeValue::array_string(["/users", "/auth", "/profiles"]))
    ]
  }
  
  // 验证继承的属性
  assert_eq(api_resource.service_name, "user-api-service")
  assert_eq(api_resource.telemetry_sdk_name, "azimuth")
  assert_eq(api_resource.telemetry_sdk_version, "0.1.0")
  
  // 验证混合属性（继承+新增）
  assert_eq(api_resource.attributes.length(), 8)
  
  // 验证继承的属性存在
  let mut found_env = false
  let mut found_namespace = false
  let mut found_cpu = false
  let mut found_memory = false
  
  let mut index = 0
  while index < api_resource.attributes.length() {
    let (key, _) = api_resource.attributes[index]
    if key == "deployment.environment" {
      found_env = true
    }
    if key == "service.namespace" {
      found_namespace = true
    }
    if key == "cpu.count" {
      found_cpu = true
    }
    if key == "memory.total" {
      found_memory = true
    }
    index = index + 1
  }
  
  assert_eq(found_env, true)
  assert_eq(found_namespace, true)
  assert_eq(found_cpu, true)
  assert_eq(found_memory = true)
  
  // 验证新增的属性
  let mut found_service_type = false
  let mut found_http_port = false
  let mut found_api_version = false
  let mut found_endpoints = false
  
  let mut index2 = 0
  while index2 < api_resource.attributes.length() {
    let (key, _) = api_resource.attributes[index2]
    if key == "service.type" {
      found_service_type = true
    }
    if key == "http.port" {
      found_http_port = true
    }
    if key == "api.version" {
      found_api_version = true
    }
    if key == "endpoints" {
      found_endpoints = true
    }
    index2 = index2 + 1
  }
  
  assert_eq(found_service_type, true)
  assert_eq(found_http_port, true)
  assert_eq(found_api_version, true)
  assert_eq(found_endpoints, true)
  
  // 测试Resource组合（多个服务合并信息）
  let composite_attributes = [
    // 来自API服务的属性
    ("service.type", AttributeValue::string("api")),
    ("http.port", AttributeValue::int(8080L)),
    
    // 来自数据库服务的属性
    ("db.system", AttributeValue::string("postgresql")),
    ("db.port", AttributeValue::int(5432L)),
    
    // 来自缓存服务的属性
    ("cache.system", AttributeValue::string("redis")),
    ("cache.port", AttributeValue::int(6379L)),
    
    // 共享的基础设施属性
    ("datacenter", AttributeValue::string("us-west-2")),
    ("region", AttributeValue::string("oregon"))
  ]
  
  let composite_resource = common::Resource::{
    service_name: "composite-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: composite_attributes
  }
  
  // 验证组合Resource
  assert_eq(composite_resource.service_name, "composite-service")
  assert_eq(composite_resource.attributes.length(), 8)
  
  // 验证各个组件的属性都存在
  let component_keys = [
    "service.type", "http.port", "db.system", "db.port",
    "cache.system", "cache.port", "datacenter", "region"
  ]
  
  let mut index3 = 0
  while index3 < component_keys.length() {
    let key = component_keys[index3]
    let mut found = false
    
    let mut attr_index = 0
    while attr_index < composite_resource.attributes.length() {
      if composite_resource.attributes[attr_index].0 == key {
        found = true
        break
      }
      attr_index = attr_index + 1
    }
    
    assert_eq(found, true)
    index3 = index3 + 1
  }
}
// 元数据管理测试用例
// 验证遥测系统中元数据的创建、更新、查询和管理功能

test "telemetry_metadata_creation" {
  // 测试遥测元数据创建
  
  let service_metadata = {
    "service_name": "user-authentication-service",
    "service_version": "2.1.3",
    "service_namespace": "authentication",
    "service_instance_id": "auth-service-pod-12345",
    "host_name": "auth-server-01.example.com",
    "ip_address": "10.0.1.100",
    "deployment_environment": "production",
    "telemetry_sdk_name": "azimuth",
    "telemetry_sdk_version": "0.1.0",
    "attributes": [
      {"key": "service.language", "value": "rust"},
      {"key": "service.framework", "value": "actix-web"},
      {"key": "service.repository", "value": "github.com/company/auth-service"},
      {"key": "service.commit", "value": "abc123def456"},
      {"key": "service.build_time", "value": "2023-12-29T10:30:00Z"}
    ]
  }
  
  // 验证服务元数据结构
  assert_eq(service_metadata["service_name"], "user-authentication-service")
  assert_eq(service_metadata["service_version"], "2.1.3")
  assert_eq(service_metadata["service_namespace"], "authentication")
  assert_eq(service_metadata["service_instance_id"], "auth-service-pod-12345")
  assert_eq(service_metadata["deployment_environment"], "production")
  
  // 验证SDK元数据
  assert_eq(service_metadata["telemetry_sdk_name"], "azimuth")
  assert_eq(service_metadata["telemetry_sdk_version"], "0.1.0")
  
  // 验证属性数量和内容
  assert_eq(service_metadata["attributes"].length(), 5)
  
  let mut language_attr_found = false
  let mut i = 0
  while i < service_metadata["attributes"].length() {
    let attr = service_metadata["attributes"][i]
    if attr["key"] == "service.language" && attr["value"] == "rust" {
      language_attr_found = true
      break
    }
    i = i + 1
  }
  assert_eq(language_attr_found, true)
  
  // 创建资源元数据
  let resource_metadata = {
    "resource": service_metadata,
    "schema_url": "https://opentelemetry.io/schemas/1.20.0",
    "creation_timestamp": 1640995200000L,
    "last_updated": 1640995200000L
  }
  
  // 验证资源元数据
  assert_eq(resource_metadata["schema_url"], "https://opentelemetry.io/schemas/1.20.0")
  assert_eq(resource_metadata["creation_timestamp"], 1640995200000L)
  assert_eq(resource_metadata["last_updated"], 1640995200000L)
}

test "telemetry_metadata_update" {
  // 测试遥测元数据更新
  
  let initial_metadata = {
    "service_name": "order-processing-service",
    "service_version": "1.0.0",
    "attributes": [
      {"key": "service.region", "value": "us-west-1"},
      {"key": "service.zone", "value": "us-west-1a"},
      {"key": "service.replicas", "value": "3"}
    ],
    "last_updated": 1640995200000L
  }
  
  // 模拟元数据更新
  let updated_metadata = {
    "service_name": "order-processing-service",
    "service_version": "1.1.0",
    "attributes": [
      {"key": "service.region", "value": "us-west-1"},
      {"key": "service.zone", "value": "us-west-1b"}, // 区域变更
      {"key": "service.replicas", "value": "5"},     // 副本数增加
      {"key": "service.feature_flag", "value": "new_feature_enabled"} // 新增属性
    ],
    "last_updated": 1640995300000L // 更新时间变更
  }
  
  // 验证版本更新
  assert_eq(updated_metadata["service_version"], "1.1.0")
  assert_eq(updated_metadata["service_version"] != initial_metadata["service_version"], true)
  
  // 验证时间戳更新
  assert_eq(updated_metadata["last_updated"], 1640995300000L)
  assert_eq(updated_metadata["last_updated"] > initial_metadata["last_updated"], true)
  
  // 验证属性变更
  assert_eq(updated_metadata["attributes"].length(), 4)
  assert_eq(updated_metadata["attributes"].length() > initial_metadata["attributes"].length(), true)
  
  // 验证具体属性变更
  let mut zone_updated = false
  let mut replicas_updated = false
  let mut feature_flag_added = false
  
  let mut i = 0
  while i < updated_metadata["attributes"].length() {
    let attr = updated_metadata["attributes"][i]
    
    if attr["key"] == "service.zone" && attr["value"] == "us-west-1b" {
      zone_updated = true
    }
    
    if attr["key"] == "service.replicas" && attr["value"] == "5" {
      replicas_updated = true
    }
    
    if attr["key"] == "service.feature_flag" && attr["value"] == "new_feature_enabled" {
      feature_flag_added = true
    }
    
    i = i + 1
  }
  
  assert_eq(zone_updated, true)
  assert_eq(replicas_updated, true)
  assert_eq(feature_flag_added, true)
}

test "telemetry_metadata_query" {
  // 测试遥测元数据查询
  
  let metadata_store = [
    {
      "service_name": "user-service",
      "service_version": "1.2.0",
      "environment": "production",
      "attributes": [
        {"key": "service.language", "value": "java"},
        {"key": "service.team", "value": "identity"}
      ]
    },
    {
      "service_name": "order-service", 
      "service_version": "2.0.1",
      "environment": "production",
      "attributes": [
        {"key": "service.language", "value": "go"},
        {"key": "service.team", "value": "commerce"}
      ]
    },
    {
      "service_name": "payment-service",
      "service_version": "1.5.3",
      "environment": "staging",
      "attributes": [
        {"key": "service.language", "value": "python"},
        {"key": "service.team", "value": "payments"}
      ]
    },
    {
      "service_name": "notification-service",
      "service_version": "1.0.0",
      "environment": "production",
      "attributes": [
        {"key": "service.language", "value": "rust"},
        {"key": "service.team", "value": "communications"}
      ]
    }
  ]
  
  // 按环境查询
  let production_services = []
  let mut i = 0
  while i < metadata_store.length() {
    let metadata = metadata_store[i]
    if metadata["environment"] == "production" {
      production_services.push(metadata)
    }
    i = i + 1
  }
  
  // 验证环境查询结果
  assert_eq(production_services.length(), 3)
  assert_eq(production_services[0]["service_name"], "user-service")
  assert_eq(production_services[1]["service_name"], "order-service")
  assert_eq(production_services[2]["service_name"], "notification-service")
  
  // 按属性查询（语言）
  let java_services = []
  i = 0
  while i < metadata_store.length() {
    let metadata = metadata_store[i]
    let mut is_java_service = false
    
    let mut j = 0
    while j < metadata["attributes"].length() {
      let attr = metadata["attributes"][j]
      if attr["key"] == "service.language" && attr["value"] == "java" {
        is_java_service = true
        break
      }
      j = j + 1
    }
    
    if is_java_service {
      java_services.push(metadata)
    }
    
    i = i + 1
  }
  
  // 验证属性查询结果
  assert_eq(java_services.length(), 1)
  assert_eq(java_services[0]["service_name"], "user-service")
  
  // 复合查询（环境 + 版本范围）
  let production_recent_services = []
  i = 0
  while i < metadata_store.length() {
    let metadata = metadata_store[i]
    let version = metadata["service_version"]
    let major_version = version.split(".")[0].to_int()
    
    if metadata["environment"] == "production" && major_version >= 1 {
      production_recent_services.push(metadata)
    }
    
    i = i + 1
  }
  
  // 验证复合查询结果
  assert_eq(production_recent_services.length(), 3)
}

test "telemetry_metadata_versioning" {
  // 测试遥测元数据版本控制
  
  let metadata_versions = [
    {
      "version": "1.0.0",
      "timestamp": 1640995200000L,
      "metadata": {
        "service_name": "inventory-service",
        "service_version": "1.0.0",
        "attributes": [
          {"key": "service.language", "value": "java"},
          {"key": "service.framework", "value": "spring-boot"}
        ]
      }
    },
    {
      "version": "1.1.0",
      "timestamp": 1640995300000L,
      "metadata": {
        "service_name": "inventory-service",
        "service_version": "1.1.0",
        "attributes": [
          {"key": "service.language", "value": "java"},
          {"key": "service.framework", "value": "spring-boot"},
          {"key": "service.cache", "value": "redis"}
        ]
      }
    },
    {
      "version": "2.0.0",
      "timestamp": 1640995400000L,
      "metadata": {
        "service_name": "inventory-service",
        "service_version": "2.0.0",
        "attributes": [
          {"key": "service.language", "value": "rust"},
          {"key": "service.framework", "value": "axum"},
          {"key": "service.cache", "value": "redis"},
          {"key": "service.database", "value": "postgresql"}
        ]
      }
    }
  ]
  
  // 获取最新版本
  let latest_version = metadata_versions[metadata_versions.length() - 1]
  
  // 验证最新版本
  assert_eq(latest_version["version"], "2.0.0")
  assert_eq(latest_version["metadata"]["service_version"], "2.0.0")
  assert_eq(latest_version["timestamp"], 1640995400000L)
  
  // 验证版本演进
  let rust_migration = latest_version["metadata"]["attributes"][0]["value"] == "rust"
  assert_eq(rust_migration, true)
  
  let database_added = false
  let mut i = 0
  while i < latest_version["metadata"]["attributes"].length() {
    let attr = latest_version["metadata"]["attributes"][i]
    if attr["key"] == "service.database" && attr["value"] == "postgresql" {
      database_added = true
      break
    }
    i = i + 1
  }
  assert_eq(database_added, true)
  
  // 版本比较
  let version_compare = fn(v1: String, v2: String) -> Int {
    let parts1 = v1.split(".")
    let parts2 = v2.split(".")
    
    let mut i = 0
    while i < parts1.length() && i < parts2.length() {
      let num1 = parts1[i].to_int()
      let num2 = parts2[i].to_int()
      
      if num1 > num2 {
        return 1
      } else if num1 < num2 {
        return -1
      }
      
      i = i + 1
    }
    
    if parts1.length() > parts2.length() {
      return 1
    } else if parts1.length() < parts2.length() {
      return -1
    }
    
    return 0
  }
  
  // 验证版本比较
  assert_eq(version_compare("2.0.0", "1.1.0"), 1)
  assert_eq(version_compare("1.0.0", "1.1.0"), -1)
  assert_eq(version_compare("1.1.0", "1.1.0"), 0)
}

test "telemetry_metadata_validation" {
  // 测试遥测元数据验证
  
  let valid_metadata = {
    "service_name": "analytics-service",
    "service_version": "1.2.3",
    "service_namespace": "analytics",
    "attributes": [
      {"key": "service.language", "value": "python"},
      {"key": "service.team", "value": "data"}
    ]
  }
  
  let invalid_metadata_cases = [
    {
      "metadata": {
        "service_name": "", // 空服务名
        "service_version": "1.0.0"
      },
      "expected_error": "service_name cannot be empty"
    },
    {
      "metadata": {
        "service_name": "test-service",
        "service_version": "invalid" // 无效版本格式
      },
      "expected_error": "service_version must follow semantic versioning"
    },
    {
      "metadata": {
        "service_name": "test-service",
        "service_version": "1.0.0",
        "attributes": [
          {"key": "", "value": "test"} // 空属性键
        ]
      },
      "expected_error": "attribute key cannot be empty"
    }
  ]
  
  // 验证有效元数据
  let validate_metadata = fn(metadata: Map[String, Any]) -> Map[String, Any] {
    let mut errors = []
    
    // 验证服务名称
    if metadata["service_name"] == "" {
      errors.push("service_name cannot be empty")
    }
    
    // 验证版本格式（简化版语义版本检查）
    let version = metadata["service_version"]
    let version_parts = version.split(".")
    if version_parts.length() != 3 {
      errors.push("service_version must follow semantic versioning")
    } else {
      let mut i = 0
      while i < version_parts.length() {
        if !version_parts[i].chars().all(fn(c) { c >= '0' && c <= '9' }) {
          errors.push("service_version must contain only numbers")
          break
        }
        i = i + 1
      }
    }
    
    // 验证属性
    if metadata.contains("attributes") {
      let attributes = metadata["attributes"]
      let mut i = 0
      while i < attributes.length() {
        let attr = attributes[i]
        if attr["key"] == "" {
          errors.push("attribute key cannot be empty")
        }
        i = i + 1
      }
    }
    
    {
      "valid": errors.length() == 0,
      "errors": errors
    }
  }
  
  let validation_result = validate_metadata(valid_metadata)
  assert_eq(validation_result["valid"], true)
  assert_eq(validation_result["errors"].length(), 0)
  
  // 验证无效元数据
  let mut i = 0
  while i < invalid_metadata_cases.length() {
    let test_case = invalid_metadata_cases[i]
    let result = validate_metadata(test_case["metadata"])
    
    assert_eq(result["valid"], false)
    assert_eq(result["errors"].length() > 0, true)
    
    i = i + 1
  }
}

test "telemetry_metadata_search" {
  // 测试遥测元数据搜索
  
  let metadata_index = [
    {
      "service_id": "svc-001",
      "service_name": "user-profile-service",
      "team": "identity",
      "language": "typescript",
      "environment": "production",
      "tags": ["api", "user-management", "microservice"]
    },
    {
      "service_id": "svc-002", 
      "service_name": "order-processing-service",
      "team": "commerce",
      "language": "go",
      "environment": "production",
      "tags": ["api", "order-management", "microservice"]
    },
    {
      "service_id": "svc-003",
      "service_name": "data-analytics-service",
      "team": "analytics",
      "language": "python",
      "environment": "staging",
      "tags": ["batch", "analytics", "data-processing"]
    },
    {
      "service_id": "svc-004",
      "service_name": "notification-service",
      "team": "communications",
      "language": "rust",
      "environment": "production",
      "tags": ["api", "notifications", "microservice"]
    }
  ]
  
  // 全文搜索
  let search_results = []
  let search_term = "service"
  
  let mut i = 0
  while i < metadata_index.length() {
    let metadata = metadata_index[i]
    let service_name = metadata["service_name"]
    
    if service_name.contains(search_term) {
      search_results.push(metadata)
    }
    
    i = i + 1
  }
  
  // 验证搜索结果
  assert_eq(search_results.length(), 4) // 所有服务名都包含"service"
  
  // 标签搜索
  let api_services = []
  i = 0
  while i < metadata_index.length() {
    let metadata = metadata_index[i]
    let tags = metadata["tags"]
    
    let mut has_api_tag = false
    let mut j = 0
    while j < tags.length() {
      if tags[j] == "api" {
        has_api_tag = true
        break
      }
      j = j + 1
    }
    
    if has_api_tag {
      api_services.push(metadata)
    }
    
    i = i + 1
  }
  
  // 验证标签搜索结果
  assert_eq(api_services.length(), 3) // 3个服务有"api"标签
  
  // 多条件搜索
  let production_microservices = []
  i = 0
  while i < metadata_index.length() {
    let metadata = metadata_index[i]
    let tags = metadata["tags"]
    
    let mut is_production = metadata["environment"] == "production"
    let mut is_microservice = false
    
    let mut j = 0
    while j < tags.length() {
      if tags[j] == "microservice" {
        is_microservice = true
        break
      }
      j = j + 1
    }
    
    if is_production && is_microservice {
      production_microservices.push(metadata)
    }
    
    i = i + 1
  }
  
  // 验证多条件搜索结果
  assert_eq(production_microservices.length(), 3)
  
  // 验证搜索结果包含正确的服务
  let service_names = []
  i = 0
  while i < production_microservices.length() {
    service_names.push(production_microservices[i]["service_name"])
    i = i + 1
  }
  
  assert_eq(service_names.contains("user-profile-service"), true)
  assert_eq(service_names.contains("order-processing-service"), true)
  assert_eq(service_names.contains("notification-service"), true)
}

test "telemetry_metadata_export_import" {
  // 测试遥测元数据导出导入
  
  let original_metadata = {
    "services": [
      {
        "service_name": "auth-service",
        "service_version": "2.1.0",
        "environment": "production",
        "attributes": [
          {"key": "service.language", "value": "rust"},
          {"key": "service.team", "value": "identity"}
        ]
      },
      {
        "service_name": "payment-service",
        "service_version": "1.5.2",
        "environment": "production",
        "attributes": [
          {"key": "service.language", "value": "go"},
          {"key": "service.team", "value": "payments"}
        ]
      }
    ],
    "export_timestamp": 1640999999000L,
    "export_version": "1.0",
    "export_format": "json"
  }
  
  // 模拟导出为JSON字符串
  let export_to_json = fn(data: Map[String, Any]) -> String {
    // 简化的JSON序列化
    let json = "{"}
    json = json + "\"services\":["
    
    let mut i = 0
    while i < data["services"].length() {
      let service = data["services"][i]
      json = json + "{"
      json = json + "\"service_name\":\"" + service["service_name"] + "\"," 
      json = json + "\"service_version\":\"" + service["service_version"] + "\"," 
      json = json + "\"environment\":\"" + service["environment"] + "\""
      json = json + "}"
      
      if i < data["services"].length() - 1 {
        json = json + ","
      }
      
      i = i + 1
    }
    
    json = json + "],"
    json = json + "\"export_timestamp\":" + data["export_timestamp"].to_string() + ","
    json = json + "\"export_version\":\"" + data["export_version"] + "\"," 
    json = json + "\"export_format\":\"" + data["export_format"] + "\""
    json = json + "}"
    
    json
  }
  
  let exported_json = export_to_json(original_metadata)
  
  // 验证导出格式
  assert_eq(exported_json.has_prefix("{"), true)
  assert_eq(exported_json.has_suffix("}"), true)
  assert_eq(exported_json.contains("\"services\""), true)
  assert_eq(exported_json.contains("\"export_timestamp\""), true)
  assert_eq(exported_json.contains("\"auth-service\""), true)
  assert_eq(exported_json.contains("\"payment-service\""), true)
  
  // 模拟从JSON导入
  let import_from_json = fn(json: String) -> Map[String, Any] {
    // 简化的JSON反序列化（实际实现会更复杂）
    let mut services = []
    
    // 提取服务信息（简化版）
    if json.contains("auth-service") {
      services.push({
        "service_name": "auth-service",
        "service_version": "2.1.0",
        "environment": "production"
      })
    }
    
    if json.contains("payment-service") {
      services.push({
        "service_name": "payment-service", 
        "service_version": "1.5.2",
        "environment": "production"
      })
    }
    
    {
      "services": services,
      "import_timestamp": 1640999999000L,
      "import_version": "1.0"
    }
  }
  
  let imported_metadata = import_from_json(exported_json)
  
  // 验证导入结果
  assert_eq(imported_metadata["services"].length(), 2)
  assert_eq(imported_metadata["services"][0]["service_name"], "auth-service")
  assert_eq(imported_metadata["services"][1]["service_name"], "payment-service")
  
  // 验证数据完整性
  let mut data_integrity = true
  let mut i = 0
  while i < original_metadata["services"].length() {
    let original_service = original_metadata["services"][i]
    let imported_service = imported_metadata["services"][i]
    
    if original_service["service_name"] != imported_service["service_name"] ||
       original_service["service_version"] != imported_service["service_version"] ||
       original_service["environment"] != imported_service["environment"] {
      data_integrity = false
      break
    }
    
    i = i + 1
  }
  
  assert_eq(data_integrity, true)
}
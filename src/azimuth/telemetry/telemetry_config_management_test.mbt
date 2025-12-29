// 遥测配置管理测试用例

test "telemetry_configuration_creation" {
  // 测试遥测配置创建
  
  let service_config = {
    "service_name": "order-processing-service",
    "service_version": "2.1.0",
    "environment": "production",
    "enabled": true,
    "sampling_rate": 0.1,
    "batch_size": 100,
    "flush_interval_ms": 5000,
    "exporter_endpoint": "https://otel-collector.internal:4317",
    "resource_attributes": [
      ("deployment.region", AttributeValue::string("us-west-2")),
      ("kubernetes.namespace", AttributeValue::string("orders")),
      ("kubernetes.pod.name", AttributeValue::string("order-processing-xyz"))
    ]
  }
  
  // 验证基本配置
  assert_eq(service_config["service_name"], "order-processing-service")
  assert_eq(service_config["service_version"], "2.1.0")
  assert_eq(service_config["environment"], "production")
  assert_eq(service_config["enabled"], true)
  assert_eq(service_config["sampling_rate"], 0.1)
  assert_eq(service_config["batch_size"], 100)
  assert_eq(service_config["flush_interval_ms"], 5000)
  
  // 验证导出器配置
  assert_eq(service_config["exporter_endpoint"], "https://otel-collector.internal:4317")
  assert_eq(service_config["exporter_endpoint"].has_prefix("https://"), true)
  assert_eq(service_config["exporter_endpoint"].contains(":4317"), true)
  
  // 验证资源属性
  let resource_attrs = service_config["resource_attributes"]
  assert_eq(resource_attrs.length(), 3)
  assert_eq(resource_attrs[0].0, "deployment.region")
  assert_eq(resource_attrs[0].1, AttributeValue::string("us-west-2"))
  assert_eq(resource_attrs[1].0, "kubernetes.namespace")
  assert_eq(resource_attrs[1].1, AttributeValue::string("orders"))
  assert_eq(resource_attrs[2].0, "kubernetes.pod.name")
  assert_eq(resource_attrs[2].1, AttributeValue::string("order-processing-xyz"))
}

test "telemetry_configuration_validation" {
  // 测试遥测配置验证
  
  // 有效配置
  let valid_config = {
    "service_name": "valid-service",
    "service_version": "1.0.0",
    "environment": "staging",
    "enabled": true,
    "sampling_rate": 0.5,
    "batch_size": 50,
    "flush_interval_ms": 2000
  }
  
  // 验证有效配置
  let mut validation_errors = []
  
  // 验证服务名称
  if valid_config["service_name"].length() == 0 {
    validation_errors.push("Service name cannot be empty")
  }
  
  // 验证采样率
  let sampling_rate = valid_config["sampling_rate"]
  if sampling_rate < 0.0 || sampling_rate > 1.0 {
    validation_errors.push("Sampling rate must be between 0.0 and 1.0")
  }
  
  // 验证批次大小
  let batch_size = valid_config["batch_size"]
  if batch_size <= 0 || batch_size > 10000 {
    validation_errors.push("Batch size must be between 1 and 10000")
  }
  
  // 验证刷新间隔
  let flush_interval = valid_config["flush_interval_ms"]
  if flush_interval < 100 || flush_interval > 60000 {
    validation_errors.push("Flush interval must be between 100ms and 60000ms")
  }
  
  // 验证没有错误
  assert_eq(validation_errors.length(), 0)
  
  // 无效配置
  let invalid_config = {
    "service_name": "",
    "service_version": "1.0.0",
    "environment": "staging",
    "enabled": true,
    "sampling_rate": 1.5, // 无效：超过1.0
    "batch_size": -10,    // 无效：负数
    "flush_interval_ms": 50 // 无效：太小
  }
  
  // 验证无效配置
  validation_errors = []
  
  // 验证服务名称
  if invalid_config["service_name"].length() == 0 {
    validation_errors.push("Service name cannot be empty")
  }
  
  // 验证采样率
  let invalid_sampling_rate = invalid_config["sampling_rate"]
  if invalid_sampling_rate < 0.0 || invalid_sampling_rate > 1.0 {
    validation_errors.push("Sampling rate must be between 0.0 and 1.0")
  }
  
  // 验证批次大小
  let invalid_batch_size = invalid_config["batch_size"]
  if invalid_batch_size <= 0 || invalid_batch_size > 10000 {
    validation_errors.push("Batch size must be between 1 and 10000")
  }
  
  // 验证刷新间隔
  let invalid_flush_interval = invalid_config["flush_interval_ms"]
  if invalid_flush_interval < 100 || invalid_flush_interval > 60000 {
    validation_errors.push("Flush interval must be between 100ms and 60000ms")
  }
  
  // 验证有错误
  assert_eq(validation_errors.length(), 4)
  assert_eq(validation_errors[0], "Service name cannot be empty")
  assert_eq(validation_errors[1], "Sampling rate must be between 0.0 and 1.0")
  assert_eq(validation_errors[2], "Batch size must be between 1 and 10000")
  assert_eq(validation_errors[3], "Flush interval must be between 100ms and 60000ms")
}

test "telemetry_configuration_merge" {
  // 测试遥测配置合并
  
  let base_config = {
    "service_name": "base-service",
    "service_version": "1.0.0",
    "environment": "development",
    "enabled": true,
    "sampling_rate": 1.0, // 开发环境全采样
    "batch_size": 10,
    "flush_interval_ms": 1000,
    "exporter_endpoint": "http://localhost:4317",
    "resource_attributes": [
      ("environment", AttributeValue::string("dev"))
    ]
  }
  
  let override_config = {
    "environment": "production",
    "sampling_rate": 0.1, // 生产环境采样10%
    "batch_size": 100,
    "flush_interval_ms": 5000,
    "exporter_endpoint": "https://otel-collector.prod:4317",
    "resource_attributes": [
      ("deployment.region", AttributeValue::string("us-east-1")),
      ("kubernetes.cluster", AttributeValue::string("prod-cluster"))
    ]
  }
  
  // 合并配置
  let merged_config = base_config
  
  // 覆盖基本配置项
  merged_config["environment"] = override_config["environment"]
  merged_config["sampling_rate"] = override_config["sampling_rate"]
  merged_config["batch_size"] = override_config["batch_size"]
  merged_config["flush_interval_ms"] = override_config["flush_interval_ms"]
  merged_config["exporter_endpoint"] = override_config["exporter_endpoint"]
  
  // 合并资源属性
  let base_attrs = base_config["resource_attributes"]
  let override_attrs = override_config["resource_attributes"]
  let merged_attrs = base_attrs
  
  let mut i = 0
  while i < override_attrs.length() {
    merged_attrs.push(override_attrs[i])
    i = i + 1
  }
  
  merged_config["resource_attributes"] = merged_attrs
  
  // 验证合并结果
  assert_eq(merged_config["service_name"], "base-service") // 保留原值
  assert_eq(merged_config["environment"], "production") // 被覆盖
  assert_eq(merged_config["sampling_rate"], 0.1) // 被覆盖
  assert_eq(merged_config["batch_size"], 100) // 被覆盖
  assert_eq(merged_config["exporter_endpoint"], "https://otel-collector.prod:4317") // 被覆盖
  
  // 验证合并的资源属性
  let final_attrs = merged_config["resource_attributes"]
  assert_eq(final_attrs.length(), 3) // 1个原有 + 2个新增
  assert_eq(final_attrs[0].0, "environment")
  assert_eq(final_attrs[0].1, AttributeValue::string("dev"))
  assert_eq(final_attrs[1].0, "deployment.region")
  assert_eq(final_attrs[1].1, AttributeValue::string("us-east-1"))
  assert_eq(final_attrs[2].0, "kubernetes.cluster")
  assert_eq(final_attrs[2].1, AttributeValue::string("prod-cluster"))
}

test "telemetry_configuration_environment_override" {
  // 测试遥测配置环境覆盖
  
  let default_config = {
    "service_name": "my-service",
    "service_version": "1.0.0",
    "environment": "development",
    "enabled": true,
    "sampling_rate": 1.0,
    "batch_size": 10,
    "flush_interval_ms": 1000,
    "exporter_endpoint": "http://localhost:4317"
  }
  
  // 环境特定配置
  let environment_configs = [
    ("development", {
      "sampling_rate": 1.0,
      "batch_size": 10,
      "flush_interval_ms": 1000,
      "exporter_endpoint": "http://localhost:4317"
    }),
    ("staging", {
      "sampling_rate": 0.5,
      "batch_size": 50,
      "flush_interval_ms": 3000,
      "exporter_endpoint": "https://otel-collector.staging:4317"
    }),
    ("production", {
      "sampling_rate": 0.1,
      "batch_size": 100,
      "flush_interval_ms": 5000,
      "exporter_endpoint": "https://otel-collector.production:4317"
    })
  ]
  
  // 测试开发环境配置
  let dev_config = default_config
  let dev_overrides = environment_configs[0].1
  dev_config["sampling_rate"] = dev_overrides["sampling_rate"]
  dev_config["batch_size"] = dev_overrides["batch_size"]
  dev_config["flush_interval_ms"] = dev_overrides["flush_interval_ms"]
  dev_config["exporter_endpoint"] = dev_overrides["exporter_endpoint"]
  
  assert_eq(dev_config["sampling_rate"], 1.0)
  assert_eq(dev_config["batch_size"], 10)
  assert_eq(dev_config["exporter_endpoint"], "http://localhost:4317")
  
  // 测试预发布环境配置
  let staging_config = default_config
  let staging_overrides = environment_configs[1].1
  staging_config["environment"] = "staging"
  staging_config["sampling_rate"] = staging_overrides["sampling_rate"]
  staging_config["batch_size"] = staging_overrides["batch_size"]
  staging_config["flush_interval_ms"] = staging_overrides["flush_interval_ms"]
  staging_config["exporter_endpoint"] = staging_overrides["exporter_endpoint"]
  
  assert_eq(staging_config["environment"], "staging")
  assert_eq(staging_config["sampling_rate"], 0.5)
  assert_eq(staging_config["batch_size"], 50)
  assert_eq(staging_config["exporter_endpoint"], "https://otel-collector.staging:4317")
  
  // 测试生产环境配置
  let prod_config = default_config
  let prod_overrides = environment_configs[2].1
  prod_config["environment"] = "production"
  prod_config["sampling_rate"] = prod_overrides["sampling_rate"]
  prod_config["batch_size"] = prod_overrides["batch_size"]
  prod_config["flush_interval_ms"] = prod_overrides["flush_interval_ms"]
  prod_config["exporter_endpoint"] = prod_overrides["exporter_endpoint"]
  
  assert_eq(prod_config["environment"], "production")
  assert_eq(prod_config["sampling_rate"], 0.1)
  assert_eq(prod_config["batch_size"], 100)
  assert_eq(prod_config["exporter_endpoint"], "https://otel-collector.production:4317")
}

test "telemetry_configuration_serialization" {
  // 测试遥测配置序列化
  
  let config = {
    "service_name": "api-gateway",
    "service_version": "3.2.1",
    "environment": "production",
    "enabled": true,
    "sampling_rate": 0.05,
    "batch_size": 200,
    "flush_interval_ms": 10000,
    "exporter_endpoint": "https://otel-collector.prod:4317",
    "resource_attributes": [
      ("service.type", AttributeValue::string("gateway")),
      ("deployment.region", AttributeValue::string("eu-west-1")),
      ("kubernetes.namespace", AttributeValue::string("infrastructure"))
    ]
  }
  
  // 序列化为JSON字符串
  let json_config = "{"
  json_config = json_config + "\"service_name\":\"" + config["service_name"] + "\","
  json_config = json_config + "\"service_version\":\"" + config["service_version"] + "\","
  json_config = json_config + "\"environment\":\"" + config["environment"] + "\","
  json_config = json_config + "\"enabled\":" + config["enabled"].to_string() + ","
  json_config = json_config + "\"sampling_rate\":" + config["sampling_rate"].to_string() + ","
  json_config = json_config + "\"batch_size\":" + config["batch_size"].to_string() + ","
  json_config = json_config + "\"flush_interval_ms\":" + config["flush_interval_ms"].to_string() + ","
  json_config = json_config + "\"exporter_endpoint\":\"" + config["exporter_endpoint"] + "\","
  json_config = json_config + "\"resource_attributes\":{"
  
  let resource_attrs = config["resource_attributes"]
  let mut i = 0
  while i < resource_attrs.length() {
    let (key, value) = resource_attrs[i]
    json_config = json_config + "\"" + key + "\":"
    
    match value {
      AttributeValue::StringValue(s) => json_config = json_config + "\"" + s + "\""
      AttributeValue::IntValue(n) => json_config = json_config + n.to_string()
      AttributeValue::FloatValue(f) => json_config = json_config + f.to_string()
      AttributeValue::BoolValue(b) => json_config = json_config + b.to_string()
      _ => json_config = json_config + "\"unsupported\""
    }
    
    if i < resource_attrs.length() - 1 {
      json_config = json_config + ","
    }
    i = i + 1
  }
  
  json_config = json_config + "}}"
  
  // 验证JSON序列化
  assert_eq(json_config.has_prefix("{"), true)
  assert_eq(json_config.has_suffix("}"), true)
  assert_eq(json_config.contains("\"service_name\":\"api-gateway\""), true)
  assert_eq(json_config.contains("\"environment\":\"production\""), true)
  assert_eq(json_config.contains("\"sampling_rate\":0.05"), true)
  assert_eq(json_config.contains("\"batch_size\":200"), true)
  assert_eq(json_config.contains("\"resource_attributes\":{"), true)
  assert_eq(json_config.contains("\"service.type\":\"gateway\""), true)
  
  // 模拟反序列化验证
  let service_name_pos = json_config.index_of("\"service_name\":\"") + 15
  let service_name_end = json_config.index_of("\",\"service_version\"")
  let extracted_service_name = json_config.substring(service_name_pos, service_name_end)
  
  let sampling_rate_pos = json_config.index_of("\"sampling_rate\":") + 16
  let sampling_rate_end = json_config.index_of(",\"batch_size\"")
  let extracted_sampling_rate_str = json_config.substring(sampling_rate_pos, sampling_rate_end)
  
  // 验证反序列化结果
  assert_eq(extracted_service_name, "api-gateway")
  assert_eq(extracted_sampling_rate_str, "0.05")
}

test "telemetry_configuration_dynamic_update" {
  // 测试遥测配置动态更新
  
  let current_config = {
    "service_name": "dynamic-service",
    "service_version": "1.0.0",
    "environment": "development",
    "enabled": true,
    "sampling_rate": 1.0,
    "batch_size": 20,
    "flush_interval_ms": 1500,
    "exporter_endpoint": "http://localhost:4317"
  }
  
  // 配置更新历史
  let config_history = []
  config_history.push(current_config)
  
  // 第一次更新：增加资源属性
  let update1 = current_config
  update1["resource_attributes"] = [
    ("initial.attr", AttributeValue::string("initial-value"))
  ]
  config_history.push(update1)
  
  // 第二次更新：调整采样率
  let update2 = config_history[1]
  update2["sampling_rate"] = 0.8
  config_history.push(update2)
  
  // 第三次更新：增加更多资源属性
  let update3 = config_history[2]
  let existing_attrs = update3["resource_attributes"]
  existing_attrs.push(("new.attr", AttributeValue::string("new-value")))
  update3["resource_attributes"] = existing_attrs
  config_history.push(update3)
  
  // 第四次更新：环境切换
  let update4 = config_history[3]
  update4["environment"] = "staging"
  update4["sampling_rate"] = 0.5
  update4["batch_size"] = 50
  update4["flush_interval_ms"] = 3000
  update4["exporter_endpoint"] = "https://otel-collector.staging:4317"
  config_history.push(update4)
  
  // 验证配置历史
  assert_eq(config_history.length(), 5)
  
  // 验证初始配置
  assert_eq(config_history[0]["sampling_rate"], 1.0)
  assert_eq(config_history[0]["environment"], "development")
  
  // 验证第一次更新
  assert_eq(config_history[1]["resource_attributes"].length(), 1)
  assert_eq(config_history[1]["resource_attributes"][0].0, "initial.attr")
  
  // 验证第二次更新
  assert_eq(config_history[2]["sampling_rate"], 0.8)
  
  // 验证第三次更新
  assert_eq(config_history[3]["resource_attributes"].length(), 2)
  assert_eq(config_history[3]["resource_attributes"][1].0, "new.attr")
  
  // 验证第四次更新
  assert_eq(config_history[4]["environment"], "staging")
  assert_eq(config_history[4]["sampling_rate"], 0.5)
  assert_eq(config_history[4]["batch_size"], 50)
  assert_eq(config_history[4]["exporter_endpoint"], "https://otel-collector.staging:4317")
  
  // 验证配置变更统计
  let mut changes_count = 0
  let mut i = 1
  while i < config_history.length() {
    // 检查采样率变更
    if config_history[i]["sampling_rate"] != config_history[i-1]["sampling_rate"] {
      changes_count = changes_count + 1
    }
    i = i + 1
  }
  
  // 采样率变更了2次（1.0 -> 0.8 -> 0.5）
  assert_eq(changes_count, 2)
}
// SDK模块的配置管理测试

test "sdk_configuration_defaults" {
  // 测试SDK配置默认值
  
  struct SdkConfiguration {
    service_name: String
    service_version: String
    endpoint: String
    protocol: String
    headers: Array[(String, String)]
    batch_size: Int
    timeout: Int
    enable_compression: Bool
    log_level: String
    export_interval: Int
    sampling_rate: Double
    resource_attributes: Array[(String, String)]
  }
  
  // 创建默认配置
  let create_default_config = fn() -> SdkConfiguration {
    SdkConfiguration{
      service_name: "unknown_service",
      service_version: "1.0.0",
      endpoint: "http://localhost:4318",
      protocol: "http/protobuf",
      headers: [],
      batch_size: 512,
      timeout: 30000,
      enable_compression: false,
      log_level: "INFO",
      export_interval: 5000,
      sampling_rate: 1.0,
      resource_attributes: [
        ("telemetry.sdk.name", "azimuth"),
        ("telemetry.sdk.version", "0.1.0"),
        ("telemetry.sdk.language", "moonbit")
      ]
    }
  }
  
  // 验证默认配置
  let default_config = create_default_config()
  
  assert_eq(default_config.service_name, "unknown_service")
  assert_eq(default_config.service_version, "1.0.0")
  assert_eq(default_config.endpoint, "http://localhost:4318")
  assert_eq(default_config.protocol, "http/protobuf")
  assert_eq(default_config.headers.length(), 0)
  assert_eq(default_config.batch_size, 512)
  assert_eq(default_config.timeout, 30000)
  assert_eq(default_config.enable_compression, false)
  assert_eq(default_config.log_level, "INFO")
  assert_eq(default_config.export_interval, 5000)
  assert_eq(default_config.sampling_rate, 1.0)
  assert_eq(default_config.resource_attributes.length(), 3)
  
  // 验证默认资源属性
  assert_eq(default_config.resource_attributes[0], ("telemetry.sdk.name", "azimuth"))
  assert_eq(default_config.resource_attributes[1], ("telemetry.sdk.version", "0.1.0"))
  assert_eq(default_config.resource_attributes[2], ("telemetry.sdk.language", "moonbit"))
}

test "sdk_configuration_override" {
  // 测试SDK配置覆盖
  
  struct SdkConfiguration {
    service_name: String
    service_version: String
    endpoint: String
    protocol: String
    headers: Array[(String, String)]
    batch_size: Int
    timeout: Int
    enable_compression: Bool
    log_level: String
    export_interval: Int
    sampling_rate: Double
    resource_attributes: Array[(String, String)]
  }
  
  // 创建默认配置
  let create_default_config = fn() -> SdkConfiguration {
    SdkConfiguration{
      service_name: "unknown_service",
      service_version: "1.0.0",
      endpoint: "http://localhost:4318",
      protocol: "http/protobuf",
      headers: [],
      batch_size: 512,
      timeout: 30000,
      enable_compression: false,
      log_level: "INFO",
      export_interval: 5000,
      sampling_rate: 1.0,
      resource_attributes: [
        ("telemetry.sdk.name", "azimuth"),
        ("telemetry.sdk.version", "0.1.0"),
        ("telemetry.sdk.language", "moonbit")
      ]
    }
  }
  
  // 覆盖配置函数
  let override_config = fn(base_config: SdkConfiguration, overrides: Array[(String, String)]) -> SdkConfiguration {
    let mut config = base_config
    
    let mut i = 0
    while i < overrides.length() {
      let (key, value) = overrides[i]
      
      match key {
        "service_name" => config = { config | service_name: value }
        "service_version" => config = { config | service_version: value }
        "endpoint" => config = { config | endpoint: value }
        "protocol" => config = { config | protocol: value }
        "batch_size" => config = { config | batch_size: value.to_int() }
        "timeout" => config = { config | timeout: value.to_int() }
        "enable_compression" => config = { config | enable_compression: value == "true" }
        "log_level" => config = { config | log_level: value }
        "export_interval" => config = { config | export_interval: value.to_int() }
        "sampling_rate" => config = { config | sampling_rate: value.to_double() }
        _ => {} // 忽略未知键
      }
      
      i = i + 1
    }
    
    config
  }
  
  // 测试配置覆盖
  let default_config = create_default_config()
  let overrides = [
    ("service_name", "payment-service"),
    ("service_version", "2.1.0"),
    ("endpoint", "http://otel-collector:4318"),
    ("batch_size", "1024"),
    ("enable_compression", "true"),
    ("log_level", "DEBUG"),
    ("sampling_rate", "0.1")
  ]
  
  let overridden_config = override_config(default_config, overrides)
  
  // 验证覆盖的值
  assert_eq(overridden_config.service_name, "payment-service")
  assert_eq(overridden_config.service_version, "2.1.0")
  assert_eq(overridden_config.endpoint, "http://otel-collector:4318")
  assert_eq(overridden_config.protocol, "http/protobuf") // 未覆盖，保持默认值
  assert_eq(overridden_config.batch_size, 1024)
  assert_eq(overridden_config.timeout, 30000) // 未覆盖，保持默认值
  assert_eq(overridden_config.enable_compression, true)
  assert_eq(overridden_config.log_level, "DEBUG")
  assert_eq(overridden_config.export_interval, 5000) // 未覆盖，保持默认值
  assert_eq(overridden_config.sampling_rate, 0.1)
  
  // 验证资源属性保持不变
  assert_eq(overridden_config.resource_attributes.length(), 3)
}

test "sdk_configuration_merge" {
  // 测试SDK配置合并
  
  struct SdkConfiguration {
    service_name: String
    service_version: String
    endpoint: String
    protocol: String
    headers: Array[(String, String)]
    batch_size: Int
    timeout: Int
    enable_compression: Bool
    log_level: String
    export_interval: Int
    sampling_rate: Double
    resource_attributes: Array[(String, String)]
  }
  
  // 创建默认配置
  let create_default_config = fn() -> SdkConfiguration {
    SdkConfiguration{
      service_name: "unknown_service",
      service_version: "1.0.0",
      endpoint: "http://localhost:4318",
      protocol: "http/protobuf",
      headers: [],
      batch_size: 512,
      timeout: 30000,
      enable_compression: false,
      log_level: "INFO",
      export_interval: 5000,
      sampling_rate: 1.0,
      resource_attributes: [
        ("telemetry.sdk.name", "azimuth"),
        ("telemetry.sdk.version", "0.1.0"),
        ("telemetry.sdk.language", "moonbit")
      ]
    }
  }
  
  // 合并headers函数
  let merge_headers = fn(base_headers: Array[(String, String)], new_headers: Array[(String, String)]) -> Array[(String, String)] {
    let merged = []
    let mut i = 0
    
    // 添加基础headers
    while i < base_headers.length() {
      merged.push(base_headers[i])
      i = i + 1
    }
    
    // 添加新headers（覆盖重复的键）
    i = 0
    while i < new_headers.length() {
      let (key, value) = new_headers[i]
      let mut found = false
      let mut j = 0
      
      // 查找并替换已存在的键
      while j < merged.length() && !found {
        if merged[j].0 == key {
          merged[j] = (key, value)
          found = true
        }
        j = j + 1
      }
      
      // 如果键不存在，添加新的
      if !found {
        merged.push((key, value))
      }
      
      i = i + 1
    }
    
    merged
  }
  
  // 合并资源属性函数
  let merge_resource_attributes = fn(base_attrs: Array[(String, String)], new_attrs: Array[(String, String)]) -> Array[(String, String)] {
    let merged = []
    let mut i = 0
    
    // 添加基础属性
    while i < base_attrs.length() {
      merged.push(base_attrs[i])
      i = i + 1
    }
    
    // 添加新属性（覆盖重复的键）
    i = 0
    while i < new_attrs.length() {
      let (key, value) = new_attrs[i]
      let mut found = false
      let mut j = 0
      
      // 查找并替换已存在的键
      while j < merged.length() && !found {
        if merged[j].0 == key {
          merged[j] = (key, value)
          found = true
        }
        j = j + 1
      }
      
      // 如果键不存在，添加新的
      if !found {
        merged.push((key, value))
      }
      
      i = i + 1
    }
    
    merged
  }
  
  // 测试headers合并
  let default_config = create_default_config()
  let new_headers = [
    ("Authorization", "Bearer token123"),
    ("X-Service-Name", "payment-service"),
    ("X-Custom-Header", "custom-value")
  ]
  
  let merged_headers = merge_headers(default_config.headers, new_headers)
  assert_eq(merged_headers.length(), 3)
  assert_eq(merged_headers[0], ("Authorization", "Bearer token123"))
  assert_eq(merged_headers[1], ("X-Service-Name", "payment-service"))
  assert_eq(merged_headers[2], ("X-Custom-Header", "custom-value"))
  
  // 测试headers覆盖
  let override_headers = [
    ("Authorization", "Bearer newtoken456"),
    ("X-Another-Header", "another-value")
  ]
  
  let merged_override_headers = merge_headers(merged_headers, override_headers)
  assert_eq(merged_override_headers.length(), 4)
  assert_eq(merged_override_headers[0], ("Authorization", "Bearer newtoken456")) // 被覆盖
  assert_eq(merged_override_headers[1], ("X-Service-Name", "payment-service")) // 保持不变
  assert_eq(merged_override_headers[2], ("X-Custom-Header", "custom-value")) // 保持不变
  assert_eq(merged_override_headers[3], ("X-Another-Header", "another-value")) // 新增
  
  // 测试资源属性合并
  let new_resource_attrs = [
    ("service.name", "payment-service"),
    ("service.version", "2.1.0"),
    ("deployment.environment", "production")
  ]
  
  let merged_resource_attrs = merge_resource_attributes(default_config.resource_attributes, new_resource_attrs)
  assert_eq(merged_resource_attrs.length(), 6)
  assert_eq(merged_resource_attrs[0], ("telemetry.sdk.name", "azimuth")) // 保持不变
  assert_eq(merged_resource_attrs[1], ("telemetry.sdk.version", "0.1.0")) // 保持不变
  assert_eq(merged_resource_attrs[2], ("telemetry.sdk.language", "moonbit")) // 保持不变
  assert_eq(merged_resource_attrs[3], ("service.name", "payment-service")) // 新增
  assert_eq(merged_resource_attrs[4], ("service.version", "2.1.0")) // 新增
  assert_eq(merged_resource_attrs[5], ("deployment.environment", "production")) // 新增
  
  // 测试资源属性覆盖
  let override_resource_attrs = [
    ("telemetry.sdk.version", "0.2.0"), // 覆盖现有
    ("service.instance.id", "instance-123") // 新增
  ]
  
  let merged_override_resource_attrs = merge_resource_attributes(merged_resource_attrs, override_resource_attrs)
  assert_eq(merged_override_resource_attrs.length(), 7)
  assert_eq(merged_override_resource_attrs[0], ("telemetry.sdk.name", "azimuth")) // 保持不变
  assert_eq(merged_override_resource_attrs[1], ("telemetry.sdk.version", "0.2.0")) // 被覆盖
  assert_eq(merged_override_resource_attrs[2], ("telemetry.sdk.language", "moonbit")) // 保持不变
  assert_eq(merged_override_resource_attrs[3], ("service.name", "payment-service")) // 保持不变
  assert_eq(merged_override_resource_attrs[4], ("service.version", "2.1.0")) // 保持不变
  assert_eq(merged_override_resource_attrs[5], ("deployment.environment", "production")) // 保持不变
  assert_eq(merged_override_resource_attrs[6], ("service.instance.id", "instance-123")) // 新增
}

test "sdk_configuration_validation" {
  // 测试SDK配置验证
  
  struct ValidationResult {
    is_valid: Bool
    errors: Array[String]
    warnings: Array[String]
  }
  
  struct SdkConfiguration {
    service_name: String
    service_version: String
    endpoint: String
    protocol: String
    headers: Array[(String, String)]
    batch_size: Int
    timeout: Int
    enable_compression: Bool
    log_level: String
    export_interval: Int
    sampling_rate: Double
    resource_attributes: Array[(String, String)]
  }
  
  // 验证配置函数
  let validate_config = fn(config: SdkConfiguration) -> ValidationResult {
    let errors = []
    let warnings = []
    let mut is_valid = true
    
    // 验证service_name
    if config.service_name == "" {
      errors.push("Service name cannot be empty")
      is_valid = false
    } else if config.service_name.length() > 255 {
      errors.push("Service name cannot exceed 255 characters")
      is_valid = false
    }
    
    // 验证service_version
    if config.service_version == "" {
      errors.push("Service version cannot be empty")
      is_valid = false
    }
    
    // 验证endpoint
    if config.endpoint == "" {
      errors.push("Endpoint cannot be empty")
      is_valid = false
    } else if !(config.endpoint.has_prefix("http://") || config.endpoint.has_prefix("https://")) {
      errors.push("Endpoint must start with http:// or https://")
      is_valid = false
    }
    
    // 验证protocol
    let valid_protocols = ["http/protobuf", "http/json", "grpc"]
    let mut valid_protocol = false
    let mut i = 0
    while i < valid_protocols.length() && !valid_protocol {
      if valid_protocols[i] == config.protocol {
        valid_protocol = true
      }
      i = i + 1
    }
    if !valid_protocol {
      errors.push("Protocol must be one of: " + valid_protocols.join(", "))
      is_valid = false
    }
    
    // 验证batch_size
    if config.batch_size < 1 || config.batch_size > 10000 {
      errors.push("Batch size must be between 1 and 10000")
      is_valid = false
    }
    
    // 验证timeout
    if config.timeout < 1000 || config.timeout > 300000 {
      errors.push("Timeout must be between 1000 and 300000 milliseconds")
      is_valid = false
    }
    
    // 验证log_level
    let valid_log_levels = ["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"]
    let mut valid_log_level = false
    i = 0
    while i < valid_log_levels.length() && !valid_log_level {
      if valid_log_levels[i] == config.log_level {
        valid_log_level = true
      }
      i = i + 1
    }
    if !valid_log_level {
      errors.push("Log level must be one of: " + valid_log_levels.join(", "))
      is_valid = false
    }
    
    // 验证export_interval
    if config.export_interval < 100 || config.export_interval > 60000 {
      errors.push("Export interval must be between 100 and 60000 milliseconds")
      is_valid = false
    }
    
    // 验证sampling_rate
    if config.sampling_rate < 0.0 || config.sampling_rate > 1.0 {
      errors.push("Sampling rate must be between 0.0 and 1.0")
      is_valid = false
    }
    
    // 生成警告
    if config.sampling_rate < 0.1 {
      warnings.push("Very low sampling rate may result in insufficient data")
    }
    
    if config.batch_size > 2048 {
      warnings.push("Large batch size may increase memory usage")
    }
    
    if config.timeout < 5000 {
      warnings.push("Short timeout may cause request failures")
    }
    
    ValidationResult{
      is_valid: is_valid,
      errors: errors,
      warnings: warnings
    }
  }
  
  // 测试有效配置
  let valid_config = SdkConfiguration{
    service_name: "payment-service",
    service_version: "2.1.0",
    endpoint: "http://otel-collector:4318",
    protocol: "http/protobuf",
    headers: [],
    batch_size: 512,
    timeout: 30000,
    enable_compression: true,
    log_level: "INFO",
    export_interval: 5000,
    sampling_rate: 0.1,
    resource_attributes: []
  }
  
  let valid_result = validate_config(valid_config)
  assert_eq(valid_result.is_valid, true)
  assert_eq(valid_result.errors.length(), 0)
  
  // 测试无效配置 - 空service_name
  let invalid_service_name_config = SdkConfiguration{
    service_name: "",
    service_version: "2.1.0",
    endpoint: "http://otel-collector:4318",
    protocol: "http/protobuf",
    headers: [],
    batch_size: 512,
    timeout: 30000,
    enable_compression: true,
    log_level: "INFO",
    export_interval: 5000,
    sampling_rate: 0.1,
    resource_attributes: []
  }
  
  let invalid_service_name_result = validate_config(invalid_service_name_config)
  assert_eq(invalid_service_name_result.is_valid, false)
  assert_eq(invalid_service_name_result.errors.length() > 0, true)
  
  // 测试无效配置 - 无效的endpoint
  let invalid_endpoint_config = SdkConfiguration{
    service_name: "payment-service",
    service_version: "2.1.0",
    endpoint: "invalid-endpoint",
    protocol: "http/protobuf",
    headers: [],
    batch_size: 512,
    timeout: 30000,
    enable_compression: true,
    log_level: "INFO",
    export_interval: 5000,
    sampling_rate: 0.1,
    resource_attributes: []
  }
  
  let invalid_endpoint_result = validate_config(invalid_endpoint_config)
  assert_eq(invalid_endpoint_result.is_valid, false)
  assert_eq(invalid_endpoint_result.errors.length() > 0, true)
  
  // 测试无效配置 - 超出范围的batch_size
  let invalid_batch_size_config = SdkConfiguration{
    service_name: "payment-service",
    service_version: "2.1.0",
    endpoint: "http://otel-collector:4318",
    protocol: "http/protobuf",
    headers: [],
    batch_size: 20000, // 超出范围
    timeout: 30000,
    enable_compression: true,
    log_level: "INFO",
    export_interval: 5000,
    sampling_rate: 0.1,
    resource_attributes: []
  }
  
  let invalid_batch_size_result = validate_config(invalid_batch_size_config)
  assert_eq(invalid_batch_size_result.is_valid, false)
  assert_eq(invalid_batch_size_result.errors.length() > 0, true)
  
  // 测试有效配置但有警告
  let warning_config = SdkConfiguration{
    service_name: "payment-service",
    service_version: "2.1.0",
    endpoint: "http://otel-collector:4318",
    protocol: "http/protobuf",
    headers: [],
    batch_size: 3000, // 大批量，可能产生警告
    timeout: 3000, // 短超时，可能产生警告
    enable_compression: true,
    log_level: "INFO",
    export_interval: 5000,
    sampling_rate: 0.05, // 低采样率，可能产生警告
    resource_attributes: []
  }
  
  let warning_result = validate_config(warning_config)
  assert_eq(warning_result.is_valid, true)
  assert_eq(warning_result.errors.length(), 0)
  assert_eq(warning_result.warnings.length() > 0, true)
}

test "sdk_configuration_serialization" {
  // 测试SDK配置序列化
  
  struct SdkConfiguration {
    service_name: String
    service_version: String
    endpoint: String
    protocol: String
    headers: Array[(String, String)]
    batch_size: Int
    timeout: Int
    enable_compression: Bool
    log_level: String
    export_interval: Int
    sampling_rate: Double
    resource_attributes: Array[(String, String)]
  }
  
  // 序列化为JSON字符串
  let serialize_to_json = fn(config: SdkConfiguration) -> String {
    let json = "{"
    json = json + "\"service_name\":\"" + config.service_name + "\"," 
    json = json + "\"service_version\":\"" + config.service_version + "\"," 
    json = json + "\"endpoint\":\"" + config.endpoint + "\"," 
    json = json + "\"protocol\":\"" + config.protocol + "\"," 
    
    // 序列化headers
    json = json + "\"headers\":["
    let mut i = 0
    while i < config.headers.length() {
      let (key, value) = config.headers[i]
      json = json + "{\"key\":\"" + key + "\",\"value\":\"" + value + "\"}"
      if i < config.headers.length() - 1 {
        json = json + ","
      }
      i = i + 1
    }
    json = json + "],"
    
    json = json + "\"batch_size\":" + config.batch_size.to_string() + ","
    json = json + "\"timeout\":" + config.timeout.to_string() + ","
    json = json + "\"enable_compression\":" + (if config.enable_compression { "true" } else { "false" }) + ","
    json = json + "\"log_level\":\"" + config.log_level + "\"," 
    json = json + "\"export_interval\":" + config.export_interval.to_string() + ","
    json = json + "\"sampling_rate\":" + config.sampling_rate.to_string() + ","
    
    // 序列化resource_attributes
    json = json + "\"resource_attributes\":["
    i = 0
    while i < config.resource_attributes.length() {
      let (key, value) = config.resource_attributes[i]
      json = json + "{\"key\":\"" + key + "\",\"value\":\"" + value + "\"}"
      if i < config.resource_attributes.length() - 1 {
        json = json + ","
      }
      i = i + 1
    }
    json = json + "]"
    
    json = json + "}"
    json
  }
  
  // 创建测试配置
  let test_config = SdkConfiguration{
    service_name: "payment-service",
    service_version: "2.1.0",
    endpoint: "http://otel-collector:4318",
    protocol: "http/protobuf",
    headers: [
      ("Authorization", "Bearer token123"),
      ("X-Service-Name", "payment-service")
    ],
    batch_size: 1024,
    timeout: 30000,
    enable_compression: true,
    log_level: "INFO",
    export_interval: 5000,
    sampling_rate: 0.1,
    resource_attributes: [
      ("service.name", "payment-service"),
      ("service.version", "2.1.0"),
      ("deployment.environment", "production")
    ]
  }
  
  // 序列化配置
  let json_string = serialize_to_json(test_config)
  
  // 验证序列化结果包含关键信息
  assert_eq(json_string.contains("\"service_name\":\"payment-service\""), true)
  assert_eq(json_string.contains("\"service_version\":\"2.1.0\""), true)
  assert_eq(json_string.contains("\"endpoint\":\"http://otel-collector:4318\""), true)
  assert_eq(json_string.contains("\"protocol\":\"http/protobuf\""), true)
  assert_eq(json_string.contains("\"batch_size\":1024"), true)
  assert_eq(json_string.contains("\"timeout\":30000"), true)
  assert_eq(json_string.contains("\"enable_compression\":true"), true)
  assert_eq(json_string.contains("\"log_level\":\"INFO\""), true)
  assert_eq(json_string.contains("\"export_interval\":5000"), true)
  assert_eq(json_string.contains("\"sampling_rate\":0.1"), true)
  
  // 验证headers被正确序列化
  assert_eq(json_string.contains("\"key\":\"Authorization\",\"value\":\"Bearer token123\""), true)
  assert_eq(json_string.contains("\"key\":\"X-Service-Name\",\"value\":\"payment-service\""), true)
  
  // 验证resource_attributes被正确序列化
  assert_eq(json_string.contains("\"key\":\"service.name\",\"value\":\"payment-service\""), true)
  assert_eq(json_string.contains("\"key\":\"service.version\",\"value\":\"2.1.0\""), true)
  assert_eq(json_string.contains("\"key\":\"deployment.environment\",\"value\":\"production\""), true)
  
  // 验证JSON格式正确性
  assert_eq(json_string.has_prefix("{"), true)
  assert_eq(json_string.has_suffix("}"), true)
  
  // 简单的括号匹配验证
  let mut open_braces = 0
  let mut i = 0
  while i < json_string.length() {
    if json_string[i] == '{' {
      open_braces = open_braces + 1
    } else if json_string[i] == '}' {
      open_braces = open_braces - 1
    }
    i = i + 1
  }
  assert_eq(open_braces, 0) // 所有开括号都应该有对应的闭括号
}
// 配置验证测试 - 测试SDK配置参数验证

test "service_resource_config_validation" {
  // 测试服务资源配置的验证
  
  struct ServiceResource {
    service_name : String
    service_namespace : String
    service_instance_id : String
    service_version : String
    schema_url : String
    attributes : Array[(String, String)]
  }
  
  struct ValidationResult {
    is_valid : Bool
    error_messages : Array[String]
    warning_messages : Array[String]
  }
  
  func validate_service_resource(resource : ServiceResource) -> ValidationResult {
    let mut errors = []
    let mut warnings = []
    
    // 验证必需字段
    if resource.service_name.length() == 0 {
      errors.push("service_name is required")
    } else if resource.service_name.length() > 255 {
      errors.push("service_name must be 255 characters or less")
    }
    
    // 验证可选字段的长度限制
    if resource.service_namespace.length() > 255 {
      errors.push("service_namespace must be 255 characters or less")
    }
    
    if resource.service_instance_id.length() > 255 {
      errors.push("service_instance_id must be 255 characters or less")
    }
    
    if resource.service_version.length() > 255 {
      errors.push("service_version must be 255 characters or less")
    }
    
    // 验证schema URL格式
    if resource.schema_url.length() > 0 && !resource.schema_url.starts_with("https://") && !resource.schema_url.starts_with("http://") {
      warnings.push("schema_url should be a valid HTTP/HTTPS URL")
    }
    
    // 验证属性数量
    if resource.attributes.length() > 1000 {
      errors.push("too many attributes (maximum 1000 allowed)")
    }
    
    // 验证属性键值
    for attr in resource.attributes {
      if attr.0.length() == 0 {
        errors.push("attribute key cannot be empty")
      } else if attr.0.length() > 255 {
        errors.push("attribute key '" + attr.0 + "' exceeds 255 character limit")
      }
      
      if attr.1.length() > 4096 {
        errors.push("attribute value for key '" + attr.0 + "' exceeds 4096 character limit")
      }
    }
    
    ValidationResult{
      is_valid: errors.length() == 0,
      error_messages: errors,
      warning_messages: warnings
    }
  }
  
  // 测试有效配置
  let valid_resource = ServiceResource{
    service_name: "test-service",
    service_namespace: "production",
    service_instance_id: "instance-12345",
    service_version: "1.0.0",
    schema_url: "https://opentelemetry.io/schemas/1.20.0",
    attributes: [
      ("environment", "production"),
      ("region", "us-west-2"),
      ("team", "observability")
    ]
  }
  
  let valid_result = validate_service_resource(valid_resource)
  assert_eq(valid_result.is_valid, true)
  assert_eq(valid_result.error_messages.length(), 0)
  
  // 测试无效配置 - 缺少必需字段
  let invalid_resource_1 = ServiceResource{
    service_name: "",  // 空的服务名
    service_namespace: "production",
    service_instance_id: "instance-12345",
    service_version: "1.0.0",
    schema_url: "https://opentelemetry.io/schemas/1.20.0",
    attributes: []
  }
  
  let invalid_result_1 = validate_service_resource(invalid_resource_1)
  assert_eq(invalid_result_1.is_valid, false)
  assert_eq(invalid_result_1.error_messages.length(), 1)
  assert_eq(invalid_result_1.error_messages[0], "service_name is required")
  
  // 测试无效配置 - 字段过长
  let long_name = "a" * 256
  let invalid_resource_2 = ServiceResource{
    service_name: long_name,
    service_namespace: "production",
    service_instance_id: "instance-12345",
    service_version: "1.0.0",
    schema_url: "https://opentelemetry.io/schemas/1.20.0",
    attributes: []
  }
  
  let invalid_result_2 = validate_service_resource(invalid_resource_2)
  assert_eq(invalid_result_2.is_valid, false)
  assert_eq(invalid_result_2.error_messages.length(), 1)
  assert_eq(invalid_result_2.error_messages[0], "service_name must be 255 characters or less")
  
  // 测试警告情况
  let warning_resource = ServiceResource{
    service_name: "test-service",
    service_namespace: "production",
    service_instance_id: "instance-12345",
    service_version: "1.0.0",
    schema_url: "invalid-schema-url",  // 无效的schema URL
    attributes: []
  }
  
  let warning_result = validate_service_resource(warning_resource)
  assert_eq(warning_result.is_valid, true)
  assert_eq(warning_result.warning_messages.length(), 1)
  assert_eq(warning_result.warning_messages[0], "schema_url should be a valid HTTP/HTTPS URL")
}

test "exporter_config_validation" {
  // 测试导出器配置的验证
  
  enum ExporterType {
    OtlpHttp
    OtlpGrpc
    Console
    None
  }
  
  struct ExporterConfig {
    exporter_type : ExporterType
    endpoint : String
    headers : Array[(String, String)]
    timeout_ms : Int
    compression : String
    certificate_file : String
    retry_policy : RetryPolicy
  }
  
  struct RetryPolicy {
    max_attempts : Int
    initial_backoff_ms : Int
    max_backoff_ms : Int
    backoff_multiplier : Double
  }
  
  func validate_exporter_config(config : ExporterConfig) -> ValidationResult {
    let mut errors = []
    let mut warnings = []
    
    match config.exporter_type {
      OtlpHttp => {
        if config.endpoint.length() == 0 {
          errors.push("endpoint is required for OTLP HTTP exporter")
        } else if !config.endpoint.starts_with("http://") && !config.endpoint.starts_with("https://") {
          errors.push("endpoint must be a valid HTTP/HTTPS URL for OTLP HTTP exporter")
        }
        
        if config.certificate_file.length() > 0 && !config.endpoint.starts_with("https://") {
          warnings.push("certificate_file specified but endpoint is not HTTPS")
        }
      }
      
      OtlpGrpc => {
        if config.endpoint.length() == 0 {
          errors.push("endpoint is required for OTLP gRPC exporter")
        } else if !config.endpoint.starts_with("http://") && !config.endpoint.starts_with("https://") {
          errors.push("endpoint must be a valid HTTP/HTTPS URL for OTLP gRPC exporter")
        }
      }
      
      Console => {
        if config.endpoint.length() > 0 {
          warnings.push("endpoint is ignored for console exporter")
        }
      }
      
      None => {
        if config.endpoint.length() > 0 {
          warnings.push("endpoint is ignored for none exporter")
        }
      }
    }
    
    // 验证超时设置
    if config.timeout_ms <= 0 {
      errors.push("timeout_ms must be positive")
    } else if config.timeout_ms > 300000 {
      warnings.push("timeout_ms is very large (5+ minutes), consider reducing it")
    }
    
    // 验证压缩设置
    if config.compression.length() > 0 && config.compression != "gzip" && config.compression != "none" {
      errors.push("compression must be 'gzip' or 'none'")
    }
    
    // 验证重试策略
    if config.retry_policy.max_attempts < 1 {
      errors.push("max_attempts must be at least 1")
    } else if config.retry_policy.max_attempts > 10 {
      warnings.push("max_attempts is very large, may cause excessive retries")
    }
    
    if config.retry_policy.initial_backoff_ms <= 0 {
      errors.push("initial_backoff_ms must be positive")
    }
    
    if config.retry_policy.max_backoff_ms <= 0 {
      errors.push("max_backoff_ms must be positive")
    }
    
    if config.retry_policy.max_backoff_ms < config.retry_policy.initial_backoff_ms {
      errors.push("max_backoff_ms must be greater than or equal to initial_backoff_ms")
    }
    
    if config.retry_policy.backoff_multiplier <= 1.0 {
      errors.push("backoff_multiplier must be greater than 1.0")
    } else if config.retry_policy.backoff_multiplier > 10.0 {
      warnings.push("backoff_multiplier is very large, may cause long delays")
    }
    
    ValidationResult{
      is_valid: errors.length() == 0,
      error_messages: errors,
      warning_messages: warnings
    }
  }
  
  // 测试有效的OTLP HTTP配置
  let valid_retry_policy = RetryPolicy{
    max_attempts: 3,
    initial_backoff_ms: 1000,
    max_backoff_ms: 30000,
    backoff_multiplier: 2.0
  }
  
  let valid_http_config = ExporterConfig{
    exporter_type: OtlpHttp,
    endpoint: "https://otel-collector:4318/v1/traces",
    headers: [
      ("Authorization", "Bearer token123"),
      ("User-Agent", "azimuth-telemetry/1.0.0")
    ],
    timeout_ms: 30000,
    compression: "gzip",
    certificate_file: "/path/to/cert.pem",
    retry_policy: valid_retry_policy
  }
  
  let valid_http_result = validate_exporter_config(valid_http_config)
  assert_eq(valid_http_result.is_valid, true)
  assert_eq(valid_http_result.error_messages.length(), 0)
  
  // 测试无效配置 - 缺少endpoint
  let invalid_http_config = ExporterConfig{
    exporter_type: OtlpHttp,
    endpoint: "",  // 缺少endpoint
    headers: [],
    timeout_ms: 30000,
    compression: "gzip",
    certificate_file: "",
    retry_policy: valid_retry_policy
  }
  
  let invalid_http_result = validate_exporter_config(invalid_http_config)
  assert_eq(invalid_http_result.is_valid, false)
  assert_eq(invalid_http_result.error_messages.length(), 1)
  assert_eq(invalid_http_result.error_messages[0], "endpoint is required for OTLP HTTP exporter")
  
  // 测试有效的Console配置
  let valid_console_config = ExporterConfig{
    exporter_type: Console,
    endpoint: "",  // Console exporter忽略endpoint
    headers: [],
    timeout_ms: 5000,
    compression: "none",
    certificate_file: "",
    retry_policy: valid_retry_policy
  }
  
  let valid_console_result = validate_exporter_config(valid_console_config)
  assert_eq(valid_console_result.is_valid, true)
  assert_eq(valid_console_result.warning_messages.length(), 0)
  
  // 测试无效的重试策略
  let invalid_retry_policy = RetryPolicy{
    max_attempts: 0,  // 无效：必须至少1次
    initial_backoff_ms: 1000,
    max_backoff_ms: 500,  // 无效：必须大于等于初始退避
    backoff_multiplier: 0.5  // 无效：必须大于1.0
  }
  
  let invalid_retry_config = ExporterConfig{
    exporter_type: OtlpHttp,
    endpoint: "https://otel-collector:4318/v1/traces",
    headers: [],
    timeout_ms: 30000,
    compression: "gzip",
    certificate_file: "",
    retry_policy: invalid_retry_policy
  }
  
  let invalid_retry_result = validate_exporter_config(invalid_retry_config)
  assert_eq(invalid_retry_result.is_valid, false)
  assert_eq(invalid_retry_result.error_messages.length(), 3)
}

test "sampling_config_validation" {
  // 测试采样配置的验证
  
  enum SamplerType {
    AlwaysOn
    AlwaysOff
    TraceIdRatio
    ParentBased
  }
  
  struct SamplingConfig {
    sampler_type : SamplerType
    ratio : Double
    parent_based : ParentBasedConfig
  }
  
  struct ParentBasedConfig {
    remote_parent_sampled : SamplerType
    remote_parent_not_sampled : SamplerType
    local_parent_sampled : SamplerType
    local_parent_not_sampled : SamplerType
  }
  
  func validate_sampling_config(config : SamplingConfig) -> ValidationResult {
    let mut errors = []
    let mut warnings = []
    
    match config.sampler_type {
      TraceIdRatio => {
        if config.ratio < 0.0 || config.ratio > 1.0 {
          errors.push("ratio must be between 0.0 and 1.0 for TraceIdRatio sampler")
        } else if config.ratio == 0.0 {
          warnings.push("ratio of 0.0 is equivalent to AlwaysOff sampler")
        } else if config.ratio == 1.0 {
          warnings.push("ratio of 1.0 is equivalent to AlwaysOn sampler")
        } else if config.ratio < 0.0001 {
          warnings.push("very low sampling ratio may result in insufficient traces")
        }
      }
      
      ParentBased => {
        // 验证parent-based配置的递归采样器
        let parent_errors = validate_parent_based_config(config.parent_based)
        errors = errors.concat(parent_errors)
      }
      
      _ => {
        if config.ratio != 0.0 {
          warnings.push("ratio is ignored for AlwaysOn/AlwaysOff samplers")
        }
      }
    }
    
    ValidationResult{
      is_valid: errors.length() == 0,
      error_messages: errors,
      warning_messages: warnings
    }
  }
  
  func validate_parent_based_config(config : ParentBasedConfig) -> Array[String] {
    let mut errors = []
    
    // ParentBased配置中不允许递归使用ParentBased
    if config.remote_parent_sampled == ParentBased ||
       config.remote_parent_not_sampled == ParentBased ||
       config.local_parent_sampled == ParentBased ||
       config.local_parent_not_sampled == ParentBased {
      errors.push("ParentBased sampler cannot be nested within ParentBased configuration")
    }
    
    errors
  }
  
  // 测试有效的AlwaysOn配置
  let always_on_config = SamplingConfig{
    sampler_type: AlwaysOn,
    ratio: 1.0,
    parent_based: ParentBasedConfig{
      remote_parent_sampled: AlwaysOn,
      remote_parent_not_sampled: AlwaysOff,
      local_parent_sampled: AlwaysOn,
      local_parent_not_sampled: AlwaysOff
    }
  }
  
  let always_on_result = validate_sampling_config(always_on_config)
  assert_eq(always_on_result.is_valid, true)
  assert_eq(always_on_result.error_messages.length(), 0)
  
  // 测试有效的TraceIdRatio配置
  let ratio_config = SamplingConfig{
    sampler_type: TraceIdRatio,
    ratio: 0.1,  // 10%采样率
    parent_based: ParentBasedConfig{
      remote_parent_sampled: AlwaysOn,
      remote_parent_not_sampled: AlwaysOff,
      local_parent_sampled: AlwaysOn,
      local_parent_not_sampled: AlwaysOff
    }
  }
  
  let ratio_result = validate_sampling_config(ratio_config)
  assert_eq(ratio_result.is_valid, true)
  assert_eq(ratio_result.error_messages.length(), 0)
  
  // 测试无效的TraceIdRatio配置
  let invalid_ratio_config = SamplingConfig{
    sampler_type: TraceIdRatio,
    ratio: 1.5,  // 无效：超过1.0
    parent_based: ParentBasedConfig{
      remote_parent_sampled: AlwaysOn,
      remote_parent_not_sampled: AlwaysOff,
      local_parent_sampled: AlwaysOn,
      local_parent_not_sampled: AlwaysOff
    }
  }
  
  let invalid_ratio_result = validate_sampling_config(invalid_ratio_config)
  assert_eq(invalid_ratio_result.is_valid, false)
  assert_eq(invalid_ratio_result.error_messages.length(), 1)
  assert_eq(invalid_ratio_result.error_messages[0], "ratio must be between 0.0 and 1.0 for TraceIdRatio sampler")
  
  // 测试边界情况 - 0.0采样率
  let zero_ratio_config = SamplingConfig{
    sampler_type: TraceIdRatio,
    ratio: 0.0,
    parent_based: ParentBasedConfig{
      remote_parent_sampled: AlwaysOn,
      remote_parent_not_sampled: AlwaysOff,
      local_parent_sampled: AlwaysOn,
      local_parent_not_sampled: AlwaysOff
    }
  }
  
  let zero_ratio_result = validate_sampling_config(zero_ratio_config)
  assert_eq(zero_ratio_result.is_valid, true)
  assert_eq(zero_ratio_result.warning_messages.length(), 1)
  assert_eq(zero_ratio_result.warning_messages[0], "ratio of 0.0 is equivalent to AlwaysOff sampler")
}

test "batch_processor_config_validation" {
  // 测试批处理器配置的验证
  
  struct BatchProcessorConfig {
    max_batch_size : Int
    max_export_batch_size : Int
    max_export_timeout_ms : Int
    scheduled_delay_ms : Int
    max_queue_size : Int
    export_timeout_ms : Int
  }
  
  func validate_batch_processor_config(config : BatchProcessorConfig) -> ValidationResult {
    let mut errors = []
    let mut warnings = []
    
    // 验证批处理大小
    if config.max_batch_size <= 0 {
      errors.push("max_batch_size must be positive")
    } else if config.max_batch_size > 65536 {
      errors.push("max_batch_size is too large (maximum 65536)")
    } else if config.max_batch_size < 32 {
      warnings.push("max_batch_size is very small, may impact performance")
    }
    
    if config.max_export_batch_size <= 0 {
      errors.push("max_export_batch_size must be positive")
    } else if config.max_export_batch_size > config.max_batch_size {
      errors.push("max_export_batch_size cannot be larger than max_batch_size")
    }
    
    // 验证超时设置
    if config.max_export_timeout_ms <= 0 {
      errors.push("max_export_timeout_ms must be positive")
    } else if config.max_export_timeout_ms > 300000 {
      warnings.push("max_export_timeout_ms is very large (5+ minutes)")
    }
    
    if config.scheduled_delay_ms <= 0 {
      errors.push("scheduled_delay_ms must be positive")
    } else if config.scheduled_delay_ms > 60000 {
      warnings.push("scheduled_delay_ms is very large, may cause delays")
    } else if config.scheduled_delay_ms < 100 {
      warnings.push("scheduled_delay_ms is very small, may cause excessive exports")
    }
    
    if config.export_timeout_ms <= 0 {
      errors.push("export_timeout_ms must be positive")
    } else if config.export_timeout_ms > config.max_export_timeout_ms {
      errors.push("export_timeout_ms cannot be larger than max_export_timeout_ms")
    }
    
    // 验证队列大小
    if config.max_queue_size <= 0 {
      errors.push("max_queue_size must be positive")
    } else if config.max_queue_size < config.max_batch_size {
      errors.push("max_queue_size must be at least max_batch_size")
    } else if config.max_queue_size > 1000000 {
      warnings.push("max_queue_size is very large, may use significant memory")
    }
    
    // 验证配置关系
    let optimal_delay = config.max_export_timeout_ms / 10
    if config.scheduled_delay_ms > optimal_delay * 2 {
      warnings.push("scheduled_delay_ms is much larger than optimal delay")
    }
    
    ValidationResult{
      is_valid: errors.length() == 0,
      error_messages: errors,
      warning_messages: warnings
    }
  }
  
  // 测试有效的批处理配置
  let valid_batch_config = BatchProcessorConfig{
    max_batch_size: 512,
    max_export_batch_size: 512,
    max_export_timeout_ms: 30000,
    scheduled_delay_ms: 5000,
    max_queue_size: 2048,
    export_timeout_ms: 30000
  }
  
  let valid_batch_result = validate_batch_processor_config(valid_batch_config)
  assert_eq(valid_batch_result.is_valid, true)
  assert_eq(valid_batch_result.error_messages.length(), 0)
  
  // 测试无效配置 - 导出批处理大小超过最大批处理大小
  let invalid_batch_config_1 = BatchProcessorConfig{
    max_batch_size: 256,
    max_export_batch_size: 512,  // 超过max_batch_size
    max_export_timeout_ms: 30000,
    scheduled_delay_ms: 5000,
    max_queue_size: 2048,
    export_timeout_ms: 30000
  }
  
  let invalid_batch_result_1 = validate_batch_processor_config(invalid_batch_config_1)
  assert_eq(invalid_batch_result_1.is_valid, false)
  assert_eq(invalid_batch_result_1.error_messages.length(), 1)
  assert_eq(invalid_batch_result_1.error_messages[0], "max_export_batch_size cannot be larger than max_batch_size")
  
  // 测试无效配置 - 队列大小小于批处理大小
  let invalid_batch_config_2 = BatchProcessorConfig{
    max_batch_size: 1024,
    max_export_batch_size: 512,
    max_export_timeout_ms: 30000,
    scheduled_delay_ms: 5000,
    max_queue_size: 512,  // 小于max_batch_size
    export_timeout_ms: 30000
  }
  
  let invalid_batch_result_2 = validate_batch_processor_config(invalid_batch_config_2)
  assert_eq(invalid_batch_result_2.is_valid, false)
  assert_eq(invalid_batch_result_2.error_messages.length(), 1)
  assert_eq(invalid_batch_result_2.error_messages[0], "max_queue_size must be at least max_batch_size")
  
  // 测试警告情况 - 非常小的调度延迟
  let warning_batch_config = BatchProcessorConfig{
    max_batch_size: 512,
    max_export_batch_size: 512,
    max_export_timeout_ms: 30000,
    scheduled_delay_ms: 50,  // 非常小的延迟
    max_queue_size: 2048,
    export_timeout_ms: 30000
  }
  
  let warning_batch_result = validate_batch_processor_config(warning_batch_config)
  assert_eq(warning_batch_result.is_valid, true)
  assert_eq(warning_batch_result.warning_messages.length(), 1)
  assert_eq(warning_batch_result.warning_messages[0], "scheduled_delay_ms is very small, may cause excessive exports")
}

test "environment_variable_config_parsing" {
  // 测试环境变量配置的解析
  
  struct EnvConfig {
    service_name : String
    service_version : String
    otlp_endpoint : String
    otlp_headers : String
    sampling_ratio : String
    batch_size : String
    export_timeout : String
  }
  
  func parse_env_config(env_vars : Array[(String, String)]) -> EnvConfig {
    let mut config = EnvConfig{
      service_name: "",
      service_version: "",
      otlp_endpoint: "",
      otlp_headers: "",
      sampling_ratio: "",
      batch_size: "",
      export_timeout: ""
    }
    
    for (key, value) in env_vars {
      match key {
        "OTEL_SERVICE_NAME" => config.service_name = value
        "OTEL_SERVICE_VERSION" => config.service_version = value
        "OTEL_EXPORTER_OTLP_ENDPOINT" => config.otlp_endpoint = value
        "OTEL_EXPORTER_OTLP_HEADERS" => config.otlp_headers = value
        "OTEL_TRACES_SAMPLER_ARG" => config.sampling_ratio = value
        "OTEL_BSP_MAX_BATCH_SIZE" => config.batch_size = value
        "OTEL_BSP_EXPORT_TIMEOUT" => config.export_timeout = value
        _ => {}
      }
    }
    
    config
  }
  
  func validate_env_config(config : EnvConfig) -> ValidationResult {
    let mut errors = []
    let mut warnings = []
    
    // 验证必需的环境变量
    if config.service_name.length() == 0 {
      errors.push("OTEL_SERVICE_NAME environment variable is required")
    }
    
    // 验证数值类型的环境变量
    if config.sampling_ratio.length() > 0 {
      match config.sampling_ratio.parse_double() {
        Ok(ratio) => {
          if ratio < 0.0 || ratio > 1.0 {
            errors.push("OTEL_TRACES_SAMPLER_ARG must be between 0.0 and 1.0")
          }
        }
        Err(_) => errors.push("OTEL_TRACES_SAMPLER_ARG must be a valid number")
      }
    }
    
    if config.batch_size.length() > 0 {
      match config.batch_size.parse_int() {
        Ok(size) => {
          if size <= 0 {
            errors.push("OTEL_BSP_MAX_BATCH_SIZE must be positive")
          }
        }
        Err(_) => errors.push("OTEL_BSP_MAX_BATCH_SIZE must be a valid integer")
      }
    }
    
    if config.export_timeout.length() > 0 {
      match config.export_timeout.parse_int() {
        Ok(timeout) => {
          if timeout <= 0 {
            errors.push("OTEL_BSP_EXPORT_TIMEOUT must be positive")
          }
        }
        Err(_) => errors.push("OTEL_BSP_EXPORT_TIMEOUT must be a valid integer")
      }
    }
    
    // 验证URL格式
    if config.otlp_endpoint.length() > 0 && 
       !config.otlp_endpoint.starts_with("http://") && 
       !config.otlp_endpoint.starts_with("https://") {
      errors.push("OTEL_EXPORTER_OTLP_ENDPOINT must be a valid HTTP/HTTPS URL")
    }
    
    // 验证headers格式
    if config.otlp_headers.length() > 0 {
      let header_pairs = config.otlp_headers.split(",")
      for pair in header_pairs {
        let parts = pair.split("=")
        if parts.length() != 2 {
          errors.push("OTEL_EXPORTER_OTLP_HEADERS must be in format 'key1=value1,key2=value2'")
          break
        }
      }
    }
    
    ValidationResult{
      is_valid: errors.length() == 0,
      error_messages: errors,
      warning_messages: warnings
    }
  }
  
  // 测试有效的环境变量配置
  let valid_env_vars = [
    ("OTEL_SERVICE_NAME", "test-service"),
    ("OTEL_SERVICE_VERSION", "1.0.0"),
    ("OTEL_EXPORTER_OTLP_ENDPOINT", "https://otel-collector:4318"),
    ("OTEL_EXPORTER_OTLP_HEADERS", "Authorization=Bearer token123,User-Agent=azimuth"),
    ("OTEL_TRACES_SAMPLER_ARG", "0.1"),
    ("OTEL_BSP_MAX_BATCH_SIZE", "512"),
    ("OTEL_BSP_EXPORT_TIMEOUT", "30000")
  ]
  
  let valid_env_config = parse_env_config(valid_env_vars)
  let valid_env_result = validate_env_config(valid_env_config)
  assert_eq(valid_env_result.is_valid, true)
  assert_eq(valid_env_result.error_messages.length(), 0)
  assert_eq(valid_env_config.service_name, "test-service")
  assert_eq(valid_env_config.sampling_ratio, "0.1")
  
  // 测试缺少必需环境变量
  let missing_required_vars = [
    ("OTEL_SERVICE_VERSION", "1.0.0"),
    ("OTEL_EXPORTER_OTLP_ENDPOINT", "https://otel-collector:4318")
  ]
  
  let missing_config = parse_env_config(missing_required_vars)
  let missing_result = validate_env_config(missing_config)
  assert_eq(missing_result.is_valid, false)
  assert_eq(missing_result.error_messages.length(), 1)
  assert_eq(missing_result.error_messages[0], "OTEL_SERVICE_NAME environment variable is required")
  
  // 测试无效的数值格式
  let invalid_numeric_vars = [
    ("OTEL_SERVICE_NAME", "test-service"),
    ("OTEL_TRACES_SAMPLER_ARG", "invalid_ratio"),  // 无效的数值
    ("OTEL_BSP_MAX_BATCH_SIZE", "not_a_number")    // 无效的整数
  ]
  
  let invalid_numeric_config = parse_env_config(invalid_numeric_vars)
  let invalid_numeric_result = validate_env_config(invalid_numeric_config)
  assert_eq(invalid_numeric_result.is_valid, false)
  assert_eq(invalid_numeric_result.error_messages.length(), 2)
  
  // 测试无效的headers格式
  let invalid_headers_vars = [
    ("OTEL_SERVICE_NAME", "test-service"),
    ("OTEL_EXPORTER_OTLP_HEADERS", "invalid-header-format")  // 缺少等号
  ]
  
  let invalid_headers_config = parse_env_config(invalid_headers_vars)
  let invalid_headers_result = validate_env_config(invalid_headers_config)
  assert_eq(invalid_headers_result.is_valid, false)
  assert_eq(invalid_headers_result.error_messages.length(), 1)
  assert_eq(invalid_headers_result.error_messages[0], "OTEL_EXPORTER_OTLP_HEADERS must be in format 'key1=value1,key2=value2'")
}
// 阿兹米克数据验证测试用例
// 专注于遥测数据的质量检查、格式验证和完整性校验功能

// 测试1: 基本数据类型验证
test "基本数据类型验证测试" {
  let validator = DataValidator::new()
  
  // 测试字符串验证
  let string_rules = [
    ValidationRule::required(),
    ValidationRule::min_length(1),
    ValidationRule::max_length(100),
    ValidationRule::pattern("^[a-zA-Z0-9_-]+$") // 只允许字母、数字、下划线和连字符
  ]
  
  // 有效字符串
  let valid_string = "service-name-123"
  let string_result = Validator::validate_string(validator, valid_string, string_rules)
  assert_true(ValidationResult::is_valid(string_result))
  
  // 无效字符串（空字符串）
  let empty_string = ""
  let empty_result = Validator::validate_string(validator, empty_string, string_rules)
  assert_false(ValidationResult::is_valid(empty_result))
  
  // 无效字符串（超过长度限制）
  let long_string = "x".repeat(101)
  let long_result = Validator::validate_string(validator, long_string, string_rules)
  assert_false(ValidationResult::is_valid(long_result))
  
  // 无效字符串（包含特殊字符）
  let special_char_string = "service@name"
  let special_result = Validator::validate_string(validator, special_char_string, string_rules)
  assert_false(ValidationResult::is_valid(special_result))
  
  // 测试整数验证
  let int_rules = [
    ValidationRule::required(),
    ValidationRule::min_value(0),
    ValidationRule::max_value(10000)
  ]
  
  // 有效整数
  let valid_int = 5000
  let int_result = Validator::validate_int(validator, valid_int, int_rules)
  assert_true(ValidationResult::is_valid(int_result))
  
  // 无效整数（超出范围）
  let invalid_int = 15000
  let invalid_int_result = Validator::validate_int(validator, invalid_int, int_rules)
  assert_false(ValidationResult::is_valid(invalid_int_result))
  
  // 测试浮点数验证
  let float_rules = [
    ValidationRule::required(),
    ValidationRule::min_value(0.0),
    ValidationRule::max_value(100.0),
    ValidationRule::precision(2) // 最多2位小数
  ]
  
  // 有效浮点数
  let valid_float = 75.52
  let float_result = Validator::validate_float(validator, valid_float, float_rules)
  assert_true(ValidationResult::is_valid(float_result))
  
  // 无效浮点数（超出精度）
  let invalid_float = 75.523
  let invalid_float_result = Validator::validate_float(validator, invalid_float, float_rules)
  assert_false(ValidationResult::is_valid(invalid_float_result))
  
  // 测试布尔值验证
  let bool_rules = [
    ValidationRule::required()
  ]
  
  // 有效布尔值
  let valid_bool = true
  let bool_result = Validator::validate_bool(validator, valid_bool, bool_rules)
  assert_true(ValidationResult::is_valid(bool_result))
  
  // 测试数组验证
  let array_rules = [
    ValidationRule::required(),
    ValidationRule::min_items(1),
    ValidationRule::max_items(10),
    ValidationRule::unique_items() // 数组元素必须唯一
  ]
  
  // 有效数组
  let valid_array = ["item1", "item2", "item3"]
  let array_result = Validator::validate_string_array(validator, valid_array, array_rules)
  assert_true(ValidationResult::is_valid(array_result))
  
  // 无效数组（重复元素）
  let duplicate_array = ["item1", "item2", "item1"]
  let duplicate_result = Validator::validate_string_array(validator, duplicate_array, array_rules)
  assert_false(ValidationResult::is_valid(duplicate_result))
}

// 测试2: 遥测数据结构验证
test "遥测数据结构验证测试" {
  let telemetry_validator = TelemetryDataValidator::new()
  
  // 定义遥测数据模式
  let telemetry_schema = TelemetrySchema::new()
  TelemetrySchema::add_required_field(telemetry_schema, "timestamp", "datetime")
  TelemetrySchema::add_required_field(telemetry_schema, "service_name", "string")
  TelemetrySchema::add_optional_field(telemetry_schema, "version", "string")
  TelemetrySchema::add_required_field(telemetry_schema, "metrics", "object")
  TelemetrySchema::add_optional_field(telemetry_schema, "attributes", "object")
  
  // 定义指标模式
  let metrics_schema = ObjectSchema::new()
  ObjectSchema::add_field(metrics_schema, "cpu_usage", "float", [
    ValidationRule::min_value(0.0),
    ValidationRule::max_value(100.0)
  ])
  ObjectSchema::add_field(metrics_schema, "memory_usage", "float", [
    ValidationRule::min_value(0.0),
    ValidationRule::max_value(100.0)
  ])
  ObjectSchema::add_field(metrics_schema, "response_time", "float", [
    ValidationRule::min_value(0.0)
  ])
  
  TelemetrySchema::add_nested_schema(telemetry_schema, "metrics", metrics_schema)
  
  // 创建有效的遥测数据
  let valid_telemetry_data = TelemetryData::new()
  TelemetryData::set_timestamp(valid_telemetry_data, Timestamp::now())
  TelemetryData::set_service_name(valid_telemetry_data, "web-service")
  TelemetryData::set_version(valid_telemetry_data, "1.0.0")
  
  TelemetryData::add_metric(valid_telemetry_data, "cpu_usage", 75.5)
  TelemetryData::add_metric(valid_telemetry_data, "memory_usage", 62.3)
  TelemetryData::add_metric(valid_telemetry_data, "response_time", 125.5)
  
  TelemetryData::add_attribute(valid_telemetry_data, "environment", "production")
  TelemetryData::add_attribute(valid_telemetry_data, "region", "us-west-2")
  
  // 验证有效数据
  let valid_result = TelemetryDataValidator::validate(telemetry_validator, valid_telemetry_data, telemetry_schema)
  assert_true(ValidationResult::is_valid(valid_result))
  
  // 创建无效的遥测数据（缺少必需字段）
  let invalid_telemetry_data = TelemetryData::new()
  TelemetryData::set_service_name(invalid_telemetry_data, "web-service")
  // 缺少timestamp和metrics
  
  // 验证无效数据
  let invalid_result = TelemetryDataValidator::validate(telemetry_validator, invalid_telemetry_data, telemetry_schema)
  assert_false(ValidationResult::is_valid(invalid_result))
  
  // 验证具体错误
  let errors = ValidationResult::get_errors(invalid_result)
  assert_true(errors.any(fn(error) {
    ValidationError::field(error) == "timestamp" && ValidationError::type(error) == "required"
  }))
  assert_true(errors.any(fn(error) {
    ValidationError::field(error) == "metrics" && ValidationError::type(error) == "required"
  }))
  
  // 创建超出范围的指标数据
  let out_of_range_data = TelemetryData::new()
  TelemetryData::set_timestamp(out_of_range_data, Timestamp::now())
  TelemetryData::set_service_name(out_of_range_data, "web-service")
  
  TelemetryData::add_metric(out_of_range_data, "cpu_usage", 150.0) // 超出100%限制
  TelemetryData::add_metric(out_of_range_data, "memory_usage", 62.3)
  TelemetryData::add_metric(out_of_range_data, "response_time", -10.0) // 负值
  
  // 验证超出范围的数据
  let out_of_range_result = TelemetryDataValidator::validate(telemetry_validator, out_of_range_data, telemetry_schema)
  assert_false(ValidationResult::is_valid(out_of_range_result))
  
  // 验证范围错误
  let range_errors = ValidationResult::get_errors(out_of_range_result)
  assert_true(range_errors.any(fn(error) {
    ValidationError::field(error) == "metrics.cpu_usage" && ValidationError::type(error) == "max_value"
  }))
  assert_true(range_errors.any(fn(error) {
    ValidationError::field(error) == "metrics.response_time" && ValidationError::type(error) == "min_value"
  }))
}

// 测试3: 时间序列数据验证
test "时间序列数据验证测试" {
  let time_series_validator = TimeSeriesValidator::new()
  
  // 创建时间序列数据
  let time_series_data = TimeSeriesData::new("cpu.usage")
  
  // 添加有效数据点
  let base_time = Timestamp::now()
  TimeSeriesData::add_point(time_series_data, TimeSeriesPoint::new(base_time, 45.2))
  TimeSeriesData::add_point(time_series_data, TimeSeriesPoint::new(base_time + 60000, 52.8)) // 1分钟后
  TimeSeriesData::add_point(time_series_data, TimeSeriesPoint::new(base_time + 120000, 48.1)) // 2分钟后
  TimeSeriesData::add_point(time_series_data, TimeSeriesPoint::new(base_time + 180000, 61.3)) // 3分钟后
  TimeSeriesData::add_point(time_series_data, TimeSeriesPoint::new(base_time + 240000, 55.7)) // 4分钟后
  
  // 配置时间序列验证规则
  let ts_rules = TimeSeriesValidationRules::new()
  TimeSeriesValidationRules::set_min_points(ts_rules, 3) // 至少3个数据点
  TimeSeriesValidationRules::set_max_points(ts_rules, 1000) // 最多1000个数据点
  TimeSeriesValidationRules::set_time_interval(ts_rules, 60000, 10) // 60秒间隔，允许10%误差
  TimeSeriesValidationRules::set_value_range(ts_rules, 0.0, 100.0) // 值范围0-100
  
  // 验证有效时间序列
  let valid_ts_result = TimeSeriesValidator::validate(time_series_validator, time_series_data, ts_rules)
  assert_true(ValidationResult::is_valid(valid_ts_result))
  
  // 创建数据点不足的时间序列
  let insufficient_data = TimeSeriesData::new("memory.usage")
  TimeSeriesData::add_point(insufficient_data, TimeSeriesPoint::new(base_time, 1024))
  TimeSeriesData::add_point(insufficient_data, TimeSeriesPoint::new(base_time + 60000, 1128))
  
  // 验证数据点不足的时间序列
  let insufficient_result = TimeSeriesValidator::validate(time_series_validator, insufficient_data, ts_rules)
  assert_false(ValidationResult::is_valid(insufficient_result))
  
  // 创建时间间隔不规则的时间序列
  let irregular_data = TimeSeriesData::new("disk.io")
  TimeSeriesData::add_point(irregular_data, TimeSeriesPoint::new(base_time, 100.0))
  TimeSeriesData::add_point(irregular_data, TimeSeriesPoint::new(base_time + 30000, 120.0)) // 30秒间隔
  TimeSeriesData::add_point(irregular_data, TimeSeriesPoint::new(base_time + 90000, 110.0)) // 60秒间隔
  TimeSeriesData::add_point(irregular_data, TimeSeriesPoint::new(base_time + 150000, 115.0)) // 60秒间隔
  
  // 验证时间间隔不规则的时间序列
  let irregular_result = TimeSeriesValidator::validate(time_series_validator, irregular_data, ts_rules)
  assert_false(ValidationResult::is_valid(irregular_result))
  
  // 创建包含异常值的时间序列
  let outlier_data = TimeSeriesData::new("network.latency")
  TimeSeriesData::add_point(outlier_data, TimeSeriesPoint::new(base_time, 25.5))
  TimeSeriesData::add_point(outlier_data, TimeSeriesPoint::new(base_time + 60000, 28.2))
  TimeSeriesData::add_point(outlier_data, TimeSeriesPoint::new(base_time + 120000, 150.0)) // 异常高值
  TimeSeriesData::add_point(outlier_data, TimeSeriesPoint::new(base_time + 180000, 26.8))
  TimeSeriesData::add_point(outlier_data, TimeSeriesPoint::new(base_time + 240000, 24.1))
  
  // 配置异常检测规则
  TimeSeriesValidationRules::enable_outlier_detection(ts_rules, true)
  TimeSeriesValidationRules::set_outlier_threshold(ts_rules, 2.0) // 2倍标准差
  
  // 验证包含异常值的时间序列
  let outlier_result = TimeSeriesValidator::validate(time_series_validator, outlier_data, ts_rules)
  assert_false(ValidationResult::is_valid(outlier_result))
  
  // 验证异常值检测
  let outlier_errors = ValidationResult::get_errors(outlier_result)
  assert_true(outlier_errors.any(fn(error) {
    ValidationError::type(error) == "outlier"
  }))
  
  // 测试时间序列连续性验证
  let continuity_rules = TimeSeriesValidationRules::new()
  TimeSeriesValidationRules::require_continuity(continuity_rules, true) // 要求数据连续
  TimeSeriesValidationRules::set_max_gap(continuity_rules, 120000) // 最大间隔2分钟
  
  // 创建有间隔的时间序列
  let gapped_data = TimeSeriesData::new("temperature")
  TimeSeriesData::add_point(gapped_data, TimeSeriesPoint::new(base_time, 22.5))
  TimeSeriesData::add_point(gapped_data, TimeSeriesPoint::new(base_time + 60000, 23.1))
  TimeSeriesData::add_point(gapped_data, TimeSeriesPoint::new(base_time + 300000, 23.8)) // 4分钟间隔
  TimeSeriesData::add_point(gapped_data, TimeSeriesPoint::new(base_time + 360000, 24.2))
  
  // 验证有间隔的时间序列
  let gapped_result = TimeSeriesValidator::validate(time_series_validator, gapped_data, continuity_rules)
  assert_false(ValidationResult::is_valid(gapped_result))
  
  // 验证间隔错误
  let gap_errors = ValidationResult::get_errors(gapped_result)
  assert_true(gap_errors.any(fn(error) {
    ValidationError::type(error) == "gap"
  }))
}

// 测试4: 复杂数据结构验证
test "复杂数据结构验证测试" {
  let complex_validator = ComplexDataValidator::new()
  
  // 定义复杂嵌套结构
  let complex_schema = ComplexSchema::new()
  
  // 定义服务结构
  let service_schema = ObjectSchema::new()
  ObjectSchema::add_required_field(service_schema, "name", "string")
  ObjectSchema::add_required_field(service_schema, "version", "string")
  ObjectSchema::add_optional_field(service_schema, "environment", "string", [
    ValidationRule::one_of(["development", "staging", "production"])
  ])
  ObjectSchema::add_optional_field(service_schema, "tags", "array")
  
  // 定义资源结构
  let resource_schema = ObjectSchema::new()
  ObjectSchema::add_required_field(resource_schema, "cpu_cores", "int", [
    ValidationRule::min_value(1),
    ValidationRule::max_value(128)
  ])
  ObjectSchema::add_required_field(resource_schema, "memory_mb", "int", [
    ValidationRule::min_value(512),
    ValidationRule::max_value(1048576) // 1TB
  ])
  ObjectSchema::add_optional_field(resource_schema, "disk_gb", "int", [
    ValidationRule::min_value(10)
  ])
  
  // 定义依赖结构
  let dependency_schema = ArraySchema::new()
  let dependency_item_schema = ObjectSchema::new()
  ObjectSchema::add_required_field(dependency_item_schema, "name", "string")
  ObjectSchema::add_required_field(dependency_item_schema, "endpoint", "string", [
    ValidationRule::url()
  ])
  ObjectSchema::add_optional_field(dependency_item_schema, "timeout_ms", "int", [
    ValidationRule::min_value(100),
    ValidationRule::max_value(300000) // 5分钟
  ])
  
  ArraySchema::set_item_schema(dependency_schema, dependency_item_schema)
  
  // 组装复杂结构
  ComplexSchema::add_field(complex_schema, "service", service_schema)
  ComplexSchema::add_field(complex_schema, "resources", resource_schema)
  ComplexSchema::add_field(complex_schema, "dependencies", dependency_schema)
  ComplexSchema::add_optional_field(complex_schema, "metadata", "object")
  
  // 创建有效的复杂数据
  let valid_complex_data = ComplexData::new()
  
  // 设置服务信息
  let service_data = ObjectData::new()
  ObjectData::set_field(service_data, "name", StringValue("user-service"))
  ObjectData::set_field(service_data, "version", StringValue("2.1.0"))
  ObjectData::set_field(service_data, "environment", StringValue("production"))
  
  let tags_data = ArrayData::new()
  ArrayData::add_item(tags_data, StringValue("web"))
  ArrayData::add_item(tags_data, StringValue("api"))
  ArrayData::add_item(tags_data, StringValue("microservice"))
  ObjectData::set_field(service_data, "tags", tags_data)
  
  ComplexData::set_field(valid_complex_data, "service", service_data)
  
  // 设置资源信息
  let resource_data = ObjectData::new()
  ObjectData::set_field(resource_data, "cpu_cores", IntValue(4))
  ObjectData::set_field(resource_data, "memory_mb", IntValue(8192))
  ObjectData::set_field(resource_data, "disk_gb", IntValue(100))
  
  ComplexData::set_field(valid_complex_data, "resources", resource_data)
  
  // 设置依赖信息
  let dependencies_data = ArrayData::new()
  
  let dependency1 = ObjectData::new()
  ObjectData::set_field(dependency1, "name", StringValue("user-database"))
  ObjectData::set_field(dependency1, "endpoint", StringValue("https://db.example.com:5432"))
  ObjectData::set_field(dependency1, "timeout_ms", IntValue(5000))
  
  let dependency2 = ObjectData::new()
  ObjectData::set_field(dependency2, "name", StringValue("auth-service"))
  ObjectData::set_field(dependency2, "endpoint", StringValue("https://auth.example.com"))
  ObjectData::set_field(dependency2, "timeout_ms", IntValue(3000))
  
  ArrayData::add_item(dependencies_data, dependency1)
  ArrayData::add_item(dependencies_data, dependency2)
  
  ComplexData::set_field(valid_complex_data, "dependencies", dependencies_data)
  
  // 设置元数据
  let metadata_data = ObjectData::new()
  ObjectData::set_field(metadata_data, "created_at", StringValue("2023-01-15T10:30:00Z"))
  ObjectData::set_field(metadata_data, "created_by", StringValue("team-platform"))
  
  ComplexData::set_field(valid_complex_data, "metadata", metadata_data)
  
  // 验证有效的复杂数据
  let valid_complex_result = ComplexDataValidator::validate(complex_validator, valid_complex_data, complex_schema)
  assert_true(ValidationResult::is_valid(valid_complex_result))
  
  // 创建无效的复杂数据（缺少必需字段）
  let invalid_complex_data = ComplexData::new()
  
  // 设置不完整的服务信息
  let incomplete_service_data = ObjectData::new()
  ObjectData::set_field(incomplete_service_data, "name", StringValue("user-service"))
  // 缺少version字段
  
  ComplexData::set_field(invalid_complex_data, "service", incomplete_service_data)
  
  // 验证无效的复杂数据
  let invalid_complex_result = ComplexDataValidator::validate(complex_validator, invalid_complex_data, complex_schema)
  assert_false(ValidationResult::is_valid(invalid_complex_result))
  
  // 验证具体错误
  let complex_errors = ValidationResult::get_errors(invalid_complex_result)
  assert_true(complex_errors.any(fn(error) {
    ValidationError::field(error) == "service.version" && ValidationError::type(error) == "required"
  }))
  
  // 创建包含无效值的复杂数据
  let invalid_values_data = ComplexData::new()
  
  // 设置无效的环境值
  let invalid_service_data = ObjectData::new()
  ObjectData::set_field(invalid_service_data, "name", StringValue("user-service"))
  ObjectData::set_field(invalid_service_data, "version", StringValue("2.1.0"))
  ObjectData::set_field(invalid_service_data, "environment", StringValue("testing")) // 不在允许列表中
  
  ComplexData::set_field(invalid_values_data, "service", invalid_service_data)
  
  // 设置无效的资源值
  let invalid_resource_data = ObjectData::new()
  ObjectData::set_field(invalid_resource_data, "cpu_cores", IntValue(0)) // 小于最小值
  ObjectData::set_field(invalid_resource_data, "memory_mb", IntValue(8192))
  
  ComplexData::set_field(invalid_values_data, "resources", invalid_resource_data)
  
  // 验证包含无效值的复杂数据
  let invalid_values_result = ComplexDataValidator::validate(complex_validator, invalid_values_data, complex_schema)
  assert_false(ValidationResult::is_valid(invalid_values_result))
  
  // 验证具体错误
  let value_errors = ValidationResult::get_errors(invalid_values_result)
  assert_true(value_errors.any(fn(error) {
    ValidationError::field(error) == "service.environment" && ValidationError::type(error) == "one_of"
  }))
  assert_true(value_errors.any(fn(error) {
    ValidationError::field(error) == "resources.cpu_cores" && ValidationError::type(error) == "min_value"
  }))
}

// 测试5: 自定义验证规则
test "自定义验证规则测试" {
  let custom_validator = CustomValidator::new()
  
  // 创建自定义验证规则：服务名称必须符合命名约定
  let service_name_rule = CustomValidationRule::new("service_name_format", fn(value) {
    match value {
      StringValue(name) => {
        // 必须以小写字母开头，只能包含小写字母、数字和连字符
        name.matches("^[a-z][a-z0-9-]*$")
      }
      _ => false
    }
  }, "Service name must start with lowercase letter and contain only lowercase letters, numbers, and hyphens")
  
  // 创建自定义验证规则：版本号必须符合语义化版本规范
  let version_rule = CustomValidationRule::new("semantic_version", fn(value) {
    match value {
      StringValue(version) => {
        // 匹配 x.y.z 格式
        version.matches("^\\d+\\.\\d+\\.\\d+$")
      }
      _ => false
    }
  }, "Version must follow semantic versioning (x.y.z)")
  
  // 创建自定义验证规则：端口号必须在非特权范围内
  let non_privileged_port_rule = CustomValidationRule::new("non_privileged_port", fn(value) {
    match value {
      IntValue(port) => port >= 1024 && port <= 65535
      _ => false
    }
  }, "Port must be in non-privileged range (1024-65535)")
  
  // 创建自定义验证规则：URL必须是HTTPS
  let https_url_rule = CustomValidationRule::new("https_url", fn(value) {
    match value {
      StringValue(url) => url.starts_with("https://")
      _ => false
    }
  }, "URL must use HTTPS protocol")
  
  // 注册自定义规则
  CustomValidator::add_rule(custom_validator, service_name_rule)
  CustomValidator::add_rule(custom_validator, version_rule)
  CustomValidator::add_rule(custom_validator, non_privileged_port_rule)
  CustomValidator::add_rule(custom_validator, https_url_rule)
  
  // 测试有效数据
  let valid_service_name = StringValue("user-service")
  let valid_service_result = CustomValidator::validate(custom_validator, valid_service_name, "service_name_format")
  assert_true(ValidationResult::is_valid(valid_service_result))
  
  let valid_version = StringValue("2.1.0")
  let valid_version_result = CustomValidator::validate(custom_validator, valid_version, "semantic_version")
  assert_true(ValidationResult::is_valid(valid_version_result))
  
  let valid_port = IntValue(8080)
  let valid_port_result = CustomValidator::validate(custom_validator, valid_port, "non_privileged_port")
  assert_true(ValidationResult::is_valid(valid_port_result))
  
  let valid_url = StringValue("https://api.example.com")
  let valid_url_result = CustomValidator::validate(custom_validator, valid_url, "https_url")
  assert_true(ValidationResult::is_valid(valid_url_result))
  
  // 测试无效数据
  let invalid_service_name = StringValue("User-Service") // 包含大写字母
  let invalid_service_result = CustomValidator::validate(custom_validator, invalid_service_name, "service_name_format")
  assert_false(ValidationResult::is_valid(invalid_service_result))
  
  let invalid_version = StringValue("2.1") // 不完整版本号
  let invalid_version_result = CustomValidator::validate(custom_validator, invalid_version, "semantic_version")
  assert_false(ValidationResult::is_valid(invalid_version_result))
  
  let invalid_port = IntValue(80) // 特权端口
  let invalid_port_result = CustomValidator::validate(custom_validator, invalid_port, "non_privileged_port")
  assert_false(ValidationResult::is_valid(invalid_port_result))
  
  let invalid_url = StringValue("http://api.example.com") // HTTP而非HTTPS
  let invalid_url_result = CustomValidator::validate(custom_validator, invalid_url, "https_url")
  assert_false(ValidationResult::is_valid(invalid_url_result))
  
  // 创建复合自定义验证规则：服务配置必须一致
  let service_consistency_rule = CustomValidationRule::new("service_consistency", fn(value) {
    match value {
      ObjectValue(service_config) => {
        // 获取服务名称和环境
        let name_option = ObjectValue::get_field(service_config, "name")
        let env_option = ObjectValue::get_field(service_config, "environment")
        
        match (name_option, env_option) {
          (Some(StringValue(name)), Some(StringValue(env))) => {
            // 生产环境的服务名称必须包含-prod后缀
            if env == "production" && !name.contains("-prod") {
              false
            } else if env == "development" && name.contains("-prod") {
              false
            } else {
              true
            }
          }
          _ => false
        }
      }
      _ => false
    }
  }, "Service name must be consistent with environment (production services must end with -prod)")
  
  CustomValidator::add_rule(custom_validator, service_consistency_rule)
  
  // 测试服务一致性
  let consistent_prod_service = ObjectValue::new()
  ObjectValue::set_field(consistent_prod_service, "name", StringValue("user-service-prod"))
  ObjectValue::set_field(consistent_prod_service, "environment", StringValue("production"))
  
  let consistent_result = CustomValidator::validate(custom_validator, consistent_prod_service, "service_consistency")
  assert_true(ValidationResult::is_valid(consistent_result))
  
  let inconsistent_prod_service = ObjectValue::new()
  ObjectValue::set_field(inconsistent_prod_service, "name", StringValue("user-service"))
  ObjectValue::set_field(inconsistent_prod_service, "environment", StringValue("production"))
  
  let inconsistent_result = CustomValidator::validate(custom_validator, inconsistent_prod_service, "service_consistency")
  assert_false(ValidationResult::is_valid(inconsistent_result))
}

// 测试6: 数据完整性验证
test "数据完整性验证测试" {
  let integrity_validator = IntegrityValidator::new()
  
  // 创建带校验和的数据
  let telemetry_data = TelemetryData::new()
  TelemetryData::add_metric(telemetry_data, "cpu_usage", 75.5)
  TelemetryData::add_metric(telemetry_data, "memory_usage", 62.3)
  TelemetryData::add_attribute(telemetry_data, "service_name", "web-service")
  TelemetryData::add_attribute(telemetry_data, "timestamp", "2023-01-15T10:30:00Z")
  
  // 计算校验和
  let checksum = IntegrityValidator::calculate_checksum(integrity_validator, telemetry_data)
  TelemetryData::add_attribute(telemetry_data, "checksum", checksum)
  
  // 验证数据完整性
  let integrity_result = IntegrityValidator::verify(integrity_validator, telemetry_data)
  assert_true(IntegrityResult::is_valid(integrity_result))
  
  // 修改数据（破坏完整性）
  TelemetryData::add_metric(telemetry_data, "network_usage", 1024.5)
  
  // 验证被修改的数据
  let modified_integrity_result = IntegrityValidator::verify(integrity_validator, telemetry_data)
  assert_false(IntegrityResult::is_valid(modified_integrity_result))
  
  // 测试数字签名验证
  let signature_validator = SignatureValidator::new()
  
  // 生成密钥对
  let key_pair = SignatureValidator::generate_key_pair(signature_validator)
  
  // 创建数据并签名
  let signed_data = TelemetryData::new()
  TelemetryData::add_metric(signed_data, "request_count", 1000)
  TelemetryData::add_attribute(signed_data, "service_name", "api-service")
  
  let signature = SignatureValidator::sign(signature_validator, signed_data, key_pair.private_key)
  TelemetryData::add_attribute(signed_data, "signature", signature)
  
  // 验证签名
  let signature_result = SignatureValidator::verify(signature_validator, signed_data, key_pair.public_key)
  assert_true(SignatureResult::is_valid(signature_result))
  
  // 修改数据（破坏签名）
  TelemetryData::add_metric(signed_data, "error_count", 5)
  
  // 验证被修改的数据签名
  let modified_signature_result = SignatureValidator::verify(signature_validator, signed_data, key_pair.public_key)
  assert_false(SignatureResult::is_valid(modified_signature_result))
  
  // 测试数据序列化完整性
  let serialization_validator = SerializationIntegrityValidator::new()
  
  // 序列化数据
  let serialized_data = SerializationIntegrityValidator::serialize(serialization_validator, telemetry_data)
  
  // 计算序列化数据的哈希
  let serialized_hash = SerializationIntegrityValidator::calculate_hash(serialization_validator, serialized_data)
  
  // 反序列化数据
  let deserialized_data = SerializationIntegrityValidator::deserialize(serialization_validator, serialized_data)
  
  // 计算反序列化数据的哈希
  let deserialized_hash = SerializationIntegrityValidator::calculate_hash(serialization_validator, deserialized_data)
  
  // 验证序列化/反序列化完整性
  assert_eq(serialized_hash, deserialized_hash)
  
  // 测试部分损坏数据的检测
  let corrupted_data = SerializationIntegrityValidator::corrupt_data(serialization_validator, serialized_data, 10) // 损坏第10个字节
  
  // 尝试反序列化损坏的数据
  let corrupted_deserialization_result = SerializationIntegrityValidator::try_deserialize(serialization_validator, corrupted_data)
  assert_false(SerializationResult::is_successful(corrupted_deserialization_result))
}

// 测试7: 验证性能和批量处理
test "验证性能和批量处理测试" {
  let batch_validator = BatchValidator::new()
  
  // 创建批量数据
  let batch_size = 1000
  let telemetry_batch = []
  
  for i in 1..=batch_size {
    let telemetry_data = TelemetryData::new()
    TelemetryData::add_metric(telemetry_data, "cpu_usage", 50.0 + (i % 50).to_float())
    TelemetryData::add_metric(telemetry_data, "memory_usage", 60.0 + (i % 40).to_float())
    TelemetryData::add_attribute(telemetry_data, "service_name", "service-" + (i % 10).to_string())
    TelemetryData::add_attribute(telemetry_data, "instance_id", "instance-" + i.to_string())
    
    telemetry_batch = telemetry_batch.push(telemetry_data)
  }
  
  // 配置批量验证规则
  let batch_rules = BatchValidationRules::new()
  BatchValidationRules::set_parallelism(batch_rules, 4) // 使用4个并行线程
  BatchValidationRules::set_batch_size(batch_rules, 100) // 每批处理100个
  BatchValidationRules::set_fail_fast(batch_rules, false) // 不在第一个错误时停止
  
  // 定义验证模式
  let telemetry_schema = TelemetrySchema::new()
  TelemetrySchema::add_required_field(telemetry_schema, "cpu_usage", "float")
  TelemetrySchema::add_required_field(telemetry_schema, "memory_usage", "float")
  TelemetrySchema::add_required_field(telemetry_schema, "service_name", "string")
  TelemetrySchema::add_required_field(telemetry_schema, "instance_id", "string")
  
  // 执行批量验证
  let start_time = Timestamp::now()
  let batch_result = BatchValidator::validate(batch_validator, telemetry_batch, telemetry_schema, batch_rules)
  let end_time = Timestamp::now()
  
  let validation_duration = end_time - start_time
  
  // 验证性能
  assert_true(validation_duration < 10000) // 应该在10秒内完成
  
  // 验证批量结果
  assert_eq(BatchValidationResult::total_count(batch_result), batch_size)
  assert_eq(BatchValidationResult::valid_count(batch_result), batch_size)
  assert_eq(BatchValidationResult::invalid_count(batch_result), 0)
  
  // 创建包含无效数据的批次
  let mixed_batch = []
  
  // 添加有效数据
  for i in 1..=800 {
    let valid_data = TelemetryData::new()
    TelemetryData::add_metric(valid_data, "cpu_usage", 50.0 + (i % 50).to_float())
    TelemetryData::add_metric(valid_data, "memory_usage", 60.0 + (i % 40).to_float())
    TelemetryData::add_attribute(valid_data, "service_name", "service-" + (i % 10).to_string())
    TelemetryData::add_attribute(valid_data, "instance_id", "instance-" + i.to_string())
    
    mixed_batch = mixed_batch.push(valid_data)
  }
  
  // 添加无效数据
  for i in 1..=200 {
    let invalid_data = TelemetryData::new()
    // 缺少必需字段
    TelemetryData::add_metric(invalid_data, "cpu_usage", 50.0 + (i % 50).to_float())
    // 缺少memory_usage, service_name, instance_id
    
    mixed_batch = mixed_batch.push(invalid_data)
  }
  
  // 验证混合批次
  let mixed_batch_result = BatchValidator::validate(batch_validator, mixed_batch, telemetry_schema, batch_rules)
  
  // 验证混合批次结果
  assert_eq(BatchValidationResult::total_count(mixed_batch_result), 1000)
  assert_eq(BatchValidationResult::valid_count(mixed_batch_result), 800)
  assert_eq(BatchValidationResult::invalid_count(mixed_batch_result), 200)
  
  // 获取详细错误
  let detailed_errors = BatchValidationResult::get_errors(mixed_batch_result)
  assert_eq(detailed_errors.length(), 200)
  
  // 验证错误类型
  let missing_field_errors = detailed_errors.filter(fn(error) {
    ValidationError::type(error) == "required"
  })
  assert_true(missing_field_errors.length() > 0)
  
  // 测试验证性能优化
  let optimized_validator = OptimizedValidator::new()
  
  // 启用缓存
  OptimizedValidator::enable_caching(optimized_validator, true)
  
  // 启用预编译验证规则
  OptimizedValidator::enable_rule_compilation(optimized_validator, true)
  
  // 预热验证器
  OptimizedValidator::warm_up(optimized_validator, telemetry_schema)
  
  // 执行优化验证
  let start_time = Timestamp::now()
  let optimized_result = OptimizedValidator::validate(optimized_validator, telemetry_batch, telemetry_schema)
  let end_time = Timestamp::now()
  
  let optimized_duration = end_time - start_time
  
  // 验证优化后的性能提升
  assert_true(optimized_duration < validation_duration) // 应该比未优化的版本更快
  
  // 验证结果一致性
  assert_eq(OptimizedValidationResult::valid_count(optimized_result), batch_size)
}

// 测试8: 验证报告和指标
test "验证报告和指标测试" {
  let reporting_validator = ReportingValidator::new()
  
  // 创建测试数据集
  let test_data_set = []
  
  // 添加有效数据
  for i in 1..=70 {
    let valid_data = TelemetryData::new()
    TelemetryData::add_metric(valid_data, "cpu_usage", 50.0 + (i % 50).to_float())
    TelemetryData::add_metric(valid_data, "memory_usage", 60.0 + (i % 40).to_float())
    TelemetryData::add_attribute(valid_data, "service_name", "service-" + (i % 10).to_string())
    TelemetryData::add_attribute(valid_data, "instance_id", "instance-" + i.to_string())
    
    test_data_set = test_data_set.push(valid_data)
  }
  
  // 添加无效数据
  for i in 1..=30 {
    let invalid_data = TelemetryData::new()
    
    // 不同类型的无效数据
    match i % 3 {
      0 => {
        // 缺少必需字段
        TelemetryData::add_metric(invalid_data, "cpu_usage", 50.0 + (i % 50).to_float())
      }
      1 => {
        // 值超出范围
        TelemetryData::add_metric(invalid_data, "cpu_usage", 150.0) // 超出100%
        TelemetryData::add_metric(invalid_data, "memory_usage", 60.0 + (i % 40).to_float())
        TelemetryData::add_attribute(invalid_data, "service_name", "service-" + (i % 10).to_string())
        TelemetryData::add_attribute(invalid_data, "instance_id", "instance-" + i.to_string())
      }
      2 => {
        // 错误的数据类型
        TelemetryData::add_metric(invalid_data, "cpu_usage", 50.0 + (i % 50).to_float())
        TelemetryData::add_metric(invalid_data, "memory_usage", 60.0 + (i % 40).to_float())
        TelemetryData::add_attribute(invalid_data, "service_name", "service-" + (i % 10).to_string())
        TelemetryData::add_attribute(invalid_data, "instance_id", IntValue(i)) // 应该是字符串
      }
      _ => assert_true(false)
    }
    
    test_data_set = test_data_set.push(invalid_data)
  }
  
  // 配置验证模式
  let telemetry_schema = TelemetrySchema::new()
  TelemetrySchema::add_required_field(telemetry_schema, "cpu_usage", "float", [
    ValidationRule::min_value(0.0),
    ValidationRule::max_value(100.0)
  ])
  TelemetrySchema::add_required_field(telemetry_schema, "memory_usage", "float")
  TelemetrySchema::add_required_field(telemetry_schema, "service_name", "string")
  TelemetrySchema::add_required_field(telemetry_schema, "instance_id", "string")
  
  // 执行验证并生成报告
  let validation_report = ReportingValidator::validate_and_report(reporting_validator, test_data_set, telemetry_schema)
  
  // 验证报告内容
  assert_eq(ValidationReport::total_count(validation_report), 100)
  assert_eq(ValidationReport::valid_count(validation_report), 70)
  assert_eq(ValidationReport::invalid_count(validation_report), 30)
  assert_eq(ValidationReport::success_rate(validation_report), 0.7)
  
  // 验证错误统计
  let error_stats = ValidationReport::get_error_statistics(validation_report)
  
  let missing_field_errors = ErrorStatistics::get_count_by_type(error_stats, "required")
  let out_of_range_errors = ErrorStatistics::get_count_by_type(error_stats, "max_value")
  let type_errors = ErrorStatistics::get_count_by_type(error_stats, "type")
  
  assert_eq(missing_field_errors, 10) // 大约10个缺少字段的错误
  assert_eq(out_of_range_errors, 10) // 大约10个超出范围的错误
  assert_eq(type_errors, 10) // 大约10个类型错误
  
  // 验证字段错误率
  let field_error_rates = ValidationReport::get_field_error_rates(validation_report)
  
  let cpu_usage_error_rate = FieldErrorRates::get_rate(field_error_rates, "cpu_usage")
  let memory_usage_error_rate = FieldErrorRates::get_rate(field_error_rates, "memory_usage")
  let service_name_error_rate = FieldErrorRates::get_rate(field_error_rates, "service_name")
  let instance_id_error_rate = FieldErrorRates::get_rate(field_error_rates, "instance_id")
  
  assert_eq(cpu_usage_error_rate, 0.1) // 10/100
  assert_eq(memory_usage_error_rate, 0.1) // 10/100
  assert_eq(service_name_error_rate, 0.1) // 10/100
  assert_eq(instance_id_error_rate, 0.2) // 20/100 (10个缺少字段 + 10个类型错误)
  
  // 生成详细报告
  let detailed_report = ValidationReport::generate_detailed_report(validation_report)
  
  // 验证报告格式
  assert_true(DetailedReport::contains_summary(detailed_report))
  assert_true(DetailedReport::contains_error_details(detailed_report))
  assert_true(DetailedReport::contains_recommendations(detailed_report))
  
  // 导出报告为JSON
  let json_report = ValidationReport::export_to_json(validation_report)
  assert_true(json_report.contains("\"total_count\":100"))
  assert_true(json_report.contains("\"valid_count\":70"))
  assert_true(json_report.contains("\"invalid_count\":30"))
  
  // 导出报告为CSV
  let csv_report = ValidationReport::export_to_csv(validation_report)
  assert_true(csv_report.contains("total_count,valid_count,invalid_count,success_rate"))
  assert_true(csv_report.contains("100,70,30,0.7"))
  
  // 生成趋势报告（比较多次验证）
  let historical_reports = []
  
  // 模拟历史报告
  for day in 1..=7 {
    let day_report = ValidationReport::new()
    ValidationReport::set_summary(day_report, 100, 65 + day, 35 - day, (65 + day).to_float() / 100.0)
    historical_reports = historical_reports.push(day_report)
  }
  
  let trend_report = ValidationReport::generate_trend_report(historical_reports)
  
  // 验证趋势报告
  assert_true(TrendReport::shows_improvement(trend_report))
  assert_true(TrendReport::contains_statistics(trend_report))
  assert_true(TrendReport::contains_visualization_data(trend_report))
  
  // 生成改进建议
  let recommendations = ValidationReport::generate_recommendations(validation_report)
  
  // 验证建议内容
  assert_true(Recommendations::has_field_recommendations(recommendations, "instance_id"))
  assert_true(Recommendations::has_rule_recommendations(recommendations, "max_value"))
  assert_true(Recommendations::has_data_quality_recommendations(recommendations))
}
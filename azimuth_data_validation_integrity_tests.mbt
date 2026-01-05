// Azimuth 数据验证和完整性检查测试用例
// 专注于数据验证规则和完整性检查功能的测试

// 测试1: 基本数据类型验证
test "基本数据类型验证测试" {
  // 创建数据验证器
  let data_validator = DataValidator::new()
  
  // 添加字符串验证规则
  DataValidator::add_string_rule(data_validator, "service.name", {
    min_length: 1,
    max_length: 50,
    pattern: "^[a-zA-Z][a-zA-Z0-9._-]*$",
    required: true
  })
  
  // 添加数字验证规则
  DataValidator::add_number_rule(data_validator, "metric.value", {
    min_value: 0.0,
    max_value: 1000000.0,
    integer_only: false,
    required: true
  })
  
  // 添加时间戳验证规则
  DataValidator::add_timestamp_rule(data_validator, "timestamp", {
    min_timestamp: 1609459200,  // 2021-01-01
    max_timestamp: 4102444800,  // 2100-01-01
    required: true
  })
  
  // 测试有效数据
  let valid_data = {
    "service.name": "payment.service",
    "metric.value": 99.5,
    "timestamp": 1640995200
  }
  
  let valid_result = DataValidator::validate(data_validator, valid_data)
  assert_true(valid_result.valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // 测试无效服务名（包含非法字符）
  let invalid_service_data = {
    "service.name": "invalid service name!",  // 包含空格和感叹号
    "metric.value": 99.5,
    "timestamp": 1640995200
  }
  
  let invalid_service_result = DataValidator::validate(data_validator, invalid_service_data)
  assert_false(invalid_service_result.valid)
  assert_true(invalid_service_result.errors.length() > 0)
  
  let service_error = invalid_service_result.errors.find(fn(e) { e.field == "service.name" })
  assert_true(service_error != None)
  
  // 测试无效数值（超出范围）
  let invalid_value_data = {
    "service.name": "valid.service",
    "metric.value": -10.0,  // 负值，超出最小值
    "timestamp": 1640995200
  }
  
  let invalid_value_result = DataValidator::validate(data_validator, invalid_value_data)
  assert_false(invalid_value_result.valid)
  
  let value_error = invalid_value_result.errors.find(fn(e) { e.field == "metric.value" })
  assert_true(value_error != None)
  
  // 测试无效时间戳
  let invalid_timestamp_data = {
    "service.name": "valid.service",
    "metric.value": 99.5,
    "timestamp": 1000000000  // 2001年，超出最小时间戳
  }
  
  let invalid_timestamp_result = DataValidator::validate(data_validator, invalid_timestamp_data)
  assert_false(invalid_timestamp_result.valid)
  
  let timestamp_error = invalid_timestamp_result.errors.find(fn(e) { e.field == "timestamp" })
  assert_true(timestamp_error != None)
  
  // 测试缺少必需字段
  let missing_field_data = {
    "service.name": "valid.service"
    // 缺少 metric.value 和 timestamp
  }
  
  let missing_field_result = DataValidator::validate(data_validator, missing_field_data)
  assert_false(missing_field_result.valid)
  assert_true(missing_field_result.errors.length() >= 2)  // 缺少两个必需字段
}

// 测试2: 复杂数据结构验证
test "复杂数据结构验证测试" {
  // 创建复杂数据验证器
  let complex_validator = DataValidator::new()
  
  // 添加数组验证规则
  DataValidator::add_array_rule(complex_validator, "telemetry.spans", {
    min_items: 1,
    max_items: 1000,
    item_type: "object",
    required: true,
    item_validation: {
      "span.id": { type: "string", pattern: "^[a-f0-9]{16}$", required: true },
      "trace.id": { type: "string", pattern: "^[a-f0-9]{32}$", required: true },
      "operation.name": { type: "string", min_length: 1, max_length: 100, required: true },
      "duration": { type: "number", min_value: 0, required: true },
      "status": { type: "string", enum: ["ok", "error", "timeout"], required: true }
    }
  })
  
  // 添加对象验证规则
  DataValidator::add_object_rule(complex_validator, "resource", {
    required: false,
    properties: {
      "service.name": { type: "string", required: true },
      "host.name": { type: "string", required: true },
      "attributes": {
        type: "object",
        required: false,
        additional_properties: true,
        property_validation: {
          type: "string",
          max_length: 255
        }
      }
    }
  })
  
  // 测试有效的复杂数据
  let valid_complex_data = {
    "telemetry.spans": [
      {
        "span.id": "1234567890abcdef",
        "trace.id": "1234567890abcdef1234567890abcdef",
        "operation.name": "database.query",
        "duration": 150,
        "status": "ok"
      },
      {
        "span.id": "fedcba0987654321",
        "trace.id": "1234567890abcdef1234567890abcdef",
        "operation.name": "cache.get",
        "duration": 5,
        "status": "ok"
      }
    ],
    "resource": {
      "service.name": "api.service",
      "host.name": "server-01.example.com",
      "attributes": {
        "version": "1.2.3",
        "environment": "production",
        "region": "us-west-2"
      }
    }
  }
  
  let valid_complex_result = DataValidator::validate(complex_validator, valid_complex_data)
  assert_true(valid_complex_result.valid)
  assert_eq(valid_complex_result.errors.length(), 0)
  
  // 测试无效的数组项
  let invalid_array_data = {
    "telemetry.spans": [
      {
        "span.id": "1234567890abcdef",
        "trace.id": "1234567890abcdef1234567890abcdef",
        "operation.name": "database.query",
        "duration": 150,
        "status": "ok"
      },
      {
        "span.id": "invalid-id",  // 无效的span ID格式
        "trace.id": "1234567890abcdef1234567890abcdef",
        "operation.name": "cache.get",
        "duration": 5,
        "status": "ok"
      }
    ],
    "resource": {
      "service.name": "api.service",
      "host.name": "server-01.example.com"
    }
  }
  
  let invalid_array_result = DataValidator::validate(complex_validator, invalid_array_data)
  assert_false(invalid_array_result.valid)
  
  let span_id_error = invalid_array_result.errors.find(fn(e) { 
    e.field == "telemetry.spans[1].span.id" 
  })
  assert_true(span_id_error != None)
  
  // 测试缺少必需的属性
  let missing_property_data = {
    "telemetry.spans": [
      {
        "span.id": "1234567890abcdef",
        "trace.id": "1234567890abcdef1234567890abcdef",
        "operation.name": "database.query",
        "duration": 150
        // 缺少 status
      }
    ],
    "resource": {
      "service.name": "api.service",
      // 缺少 host.name
      "attributes": {
        "version": "1.2.3"
      }
    }
  }
  
  let missing_property_result = DataValidator::validate(complex_validator, missing_property_data)
  assert_false(missing_property_result.valid)
  assert_true(missing_property_result.errors.length() >= 3)  // 缺少多个必需属性
}

// 测试3: 数据完整性检查
test "数据完整性检查测试" {
  // 创建完整性检查器
  let integrity_checker = IntegrityChecker::new()
  
  // 添加校验和验证规则
  IntegrityChecker::add_checksum_rule(integrity_checker, "telemetry.batch", {
    algorithm: "sha256",
    field: "data",
    checksum_field: "checksum"
  })
  
  // 添加数据一致性规则
  IntegrityChecker::add_consistency_rule(integrity_checker, "span.timing", {
    rules: [
      { check: "start_time <= end_time", message: "开始时间必须早于或等于结束时间" },
      { check: "duration = end_time - start_time", message: "持续时间必须等于结束时间减去开始时间" }
    ]
  })
  
  // 添加引用完整性规则
  IntegrityChecker::add_reference_rule(integrity_checker, "trace.consistency", {
    parent_field: "parent_span_id",
    reference_field: "span_id",
    allow_null: true,
    message: "父span ID必须引用有效的span"
  })
  
  // 测试有效的完整性数据
  let valid_integrity_data = {
    "telemetry.batch": {
      "data": "{\"metrics\": [{\"name\": \"cpu\", \"value\": 75.5}]}",
      "checksum": "a1b2c3d4e5f6..."  // 假设这是正确的校验和
    },
    "span.timing": {
      "start_time": 1640995200,
      "end_time": 1640995250,
      "duration": 50
    },
    "trace.consistency": {
      "spans": [
        { "span_id": "span-1", "parent_span_id": None },
        { "span_id": "span-2", "parent_span_id": Some("span-1") },
        { "span_id": "span-3", "parent_span_id": Some("span-2") }
      ]
    }
  }
  
  let valid_integrity_result = IntegrityChecker::check(integrity_checker, valid_integrity_data)
  assert_true(valid_integrity_result.valid)
  assert_eq(valid_integrity_result.violations.length(), 0)
  
  // 测试校验和不匹配
  let invalid_checksum_data = {
    "telemetry.batch": {
      "data": "{\"metrics\": [{\"name\": \"cpu\", \"value\": 75.5}]}",
      "checksum": "invalid_checksum"  // 无效的校验和
    }
  }
  
  let invalid_checksum_result = IntegrityChecker::check(integrity_checker, invalid_checksum_data)
  assert_false(invalid_checksum_result.valid)
  
  let checksum_violation = invalid_checksum_result.violations.find(fn(v) { 
    v.type == "checksum_mismatch" 
  })
  assert_true(checksum_violation != None)
  
  // 测试时间不一致
  let invalid_timing_data = {
    "span.timing": {
      "start_time": 1640995250,
      "end_time": 1640995200,  // 早于开始时间
      "duration": 50
    }
  }
  
  let invalid_timing_result = IntegrityChecker::check(integrity_checker, invalid_timing_data)
  assert_false(invalid_timing_result.valid)
  
  let timing_violation = invalid_timing_result.violations.find(fn(v) { 
    v.type == "consistency_violation" 
  })
  assert_true(timing_violation != None)
  
  // 测试引用完整性违反
  let invalid_reference_data = {
    "trace.consistency": {
      "spans": [
        { "span_id": "span-1", "parent_span_id": None },
        { "span_id": "span-2", "parent_span_id": Some("span-999") },  // 引用不存在的span
        { "span_id": "span-3", "parent_span_id": Some("span-2") }
      ]
    }
  }
  
  let invalid_reference_result = IntegrityChecker::check(integrity_checker, invalid_reference_data)
  assert_false(invalid_reference_result.valid)
  
  let reference_violation = invalid_reference_result.violations.find(fn(v) { 
    v.type == "reference_violation" 
  })
  assert_true(reference_violation != None)
}

// 测试4: 数据范围和边界检查
test "数据范围和边界检查测试" {
  // 创建范围检查器
  let range_checker = RangeChecker::new()
  
  // 添加数值范围检查
  RangeChecker::add_numeric_range(range_checker, "http.status_code", {
    min_value: 100,
    max_value: 599,
    valid_ranges: [
      (100, 199),  // 信息响应
      (200, 299),  // 成功响应
      (300, 399),  // 重定向
      (400, 499),  // 客户端错误
      (500, 599)   // 服务器错误
    ]
  })
  
  // 添加枚举值检查
  RangeChecker::add_enum_range(range_checker, "log.level", {
    allowed_values: ["debug", "info", "warn", "error", "fatal"]
  })
  
  // 添加时间范围检查
  RangeChecker::add_time_range(range_checker, "event.timestamp", {
    min_time: "2021-01-01T00:00:00Z",
    max_time: "2030-12-31T23:59:59Z",
    format: "iso8601"
  })
  
  // 测试有效范围数据
  let valid_range_data = {
    "http.status_code": 200,
    "log.level": "info",
    "event.timestamp": "2022-01-01T12:00:00Z"
  }
  
  let valid_range_result = RangeChecker::check(range_checker, valid_range_data)
  assert_true(valid_range_result.valid)
  
  // 测试无效HTTP状态码
  let invalid_status_data = {
    "http.status_code": 99  // 低于最小值
  }
  
  let invalid_status_result = RangeChecker::check(range_checker, invalid_status_data)
  assert_false(invalid_status_result.valid)
  
  // 测试无效日志级别
  let invalid_level_data = {
    "log.level": "trace"  // 不在允许的值列表中
  }
  
  let invalid_level_result = RangeChecker::check(range_checker, invalid_level_data)
  assert_false(invalid_level_result.valid)
  
  // 测试边界值
  let boundary_data = {
    "http.status_code": 100,  // 最小边界值
    "log.level": "debug",     // 第一个枚举值
    "event.timestamp": "2021-01-01T00:00:00Z"  // 最小时间
  }
  
  let boundary_result = RangeChecker::check(range_checker, boundary_data)
  assert_true(boundary_result.valid)
  
  // 测试超出边界值
  let out_of_bounds_data = {
    "http.status_code": 600,  // 超出最大值
    "log.level": "fatal",     // 最后一个枚举值
    "event.timestamp": "2031-01-01T00:00:00Z"  // 超出最大时间
  }
  
  let out_of_bounds_result = RangeChecker::check(range_checker, out_of_bounds_data)
  assert_false(out_of_bounds_result.valid)
  assert_true(out_of_bounds_result.violations.length() >= 2)  // 至少两个违反
}

// 测试5: 数据依赖关系验证
test "数据依赖关系验证测试" {
  // 创建依赖验证器
  let dependency_validator = DependencyValidator::new()
  
  // 添加条件依赖规则
  DependencyValidator::add_conditional_rule(dependency_validator, {
    condition: "error_code != null",
    required_fields: ["error_message", "error_stack"],
    message: "当存在错误代码时，必须提供错误消息和堆栈跟踪"
  })
  
  // 添加互斥字段规则
  DependencyValidator::add_mutual_exclusion_rule(dependency_validator, {
    fields: ["sync_mode", "async_mode"],
    message: "同步模式和异步模式不能同时启用"
  })
  
  // 添加值依赖规则
  DependencyValidator::add_value_dependency_rule(dependency_validator, {
    field: "protocol",
    dependencies: [
      { value: "http", required_fields: ["http_method", "http_url"] },
      { value: "grpc", required_fields: ["grpc_service", "grpc_method"] },
      { value: "tcp", required_fields: ["tcp_host", "tcp_port"] }
    ]
  })
  
  // 测试有效的依赖数据
  let valid_dependency_data = {
    "error_code": null,
    "sync_mode": true,
    "async_mode": false,
    "protocol": "http",
    "http_method": "POST",
    "http_url": "https://api.example.com/telemetry"
  }
  
  let valid_dependency_result = DependencyValidator::validate(dependency_validator, valid_dependency_data)
  assert_true(valid_dependency_result.valid)
  
  // 测试条件依赖违反
  let conditional_violation_data = {
    "error_code": "E001",  // 有错误代码
    // 缺少 error_message 和 error_stack
    "sync_mode": true,
    "async_mode": false,
    "protocol": "http",
    "http_method": "POST",
    "http_url": "https://api.example.com/telemetry"
  }
  
  let conditional_violation_result = DependencyValidator::validate(dependency_validator, conditional_violation_data)
  assert_false(conditional_violation_result.valid)
  
  // 测试互斥字段违反
  let mutual_exclusion_data = {
    "error_code": null,
    "sync_mode": true,   // 同时启用
    "async_mode": true,  // 同时启用
    "protocol": "http",
    "http_method": "POST",
    "http_url": "https://api.example.com/telemetry"
  }
  
  let mutual_exclusion_result = DependencyValidator::validate(dependency_validator, mutual_exclusion_data)
  assert_false(mutual_exclusion_result.valid)
  
  // 测试值依赖违反
  let value_dependency_data = {
    "error_code": null,
    "sync_mode": true,
    "async_mode": false,
    "protocol": "grpc",
    // 缺少 grpc_service 和 grpc_method
  }
  
  let value_dependency_result = DependencyValidator::validate(dependency_validator, value_dependency_data)
  assert_false(value_dependency_result.valid)
  
  // 测试复杂依赖链
  DependencyValidator::add_chain_rule(dependency_validator, {
    chain: [
      { field: "has_database", value: true, requires: ["db_type"] },
      { field: "db_type", value: "sql", requires: ["sql_dialect"] },
      { field: "sql_dialect", value: "postgresql", requires: ["pg_version"] }
    ]
  })
  
  let valid_chain_data = {
    "has_database": true,
    "db_type": "sql",
    "sql_dialect": "postgresql",
    "pg_version": "13.4"
  }
  
  let valid_chain_result = DependencyValidator::validate(dependency_validator, valid_chain_data)
  assert_true(valid_chain_result.valid)
  
  let invalid_chain_data = {
    "has_database": true,
    "db_type": "sql",
    "sql_dialect": "postgresql"
    // 缺少 pg_version
  }
  
  let invalid_chain_result = DependencyValidator::validate(dependency_validator, invalid_chain_data)
  assert_false(invalid_chain_result.valid)
}

// 测试6: 数据格式验证
test "数据格式验证测试" {
  // 创建格式验证器
  let format_validator = FormatValidator::new()
  
  // 添加字符串格式规则
  FormatValidator::add_string_format_rule(format_validator, "ip.address", {
    format: "ipv4",
    message: "必须是有效的IPv4地址"
  })
  
  FormatValidator::add_string_format_rule(format_validator, "email.address", {
    format: "email",
    message: "必须是有效的电子邮件地址"
  })
  
  FormatValidator::add_string_format_rule(format_validator, "uuid", {
    format: "uuid",
    message: "必须是有效的UUID"
  })
  
  // 添加日期时间格式规则
  FormatValidator::add_datetime_format_rule(format_validator, "iso.timestamp", {
    format: "iso8601",
    strict: true,
    message: "必须是有效的ISO8601时间戳"
  })
  
  // 添加URI格式规则
  FormatValidator::add_uri_format_rule(format_validator, "endpoint.url", {
    schemes: ["http", "https"],
    require_host: true,
    message: "必须是有效的HTTP或HTTPS URL"
  })
  
  // 测试有效格式数据
  let valid_format_data = {
    "ip.address": "192.168.1.100",
    "email.address": "user@example.com",
    "uuid": "550e8400-e29b-41d4-a716-446655440000",
    "iso.timestamp": "2022-01-01T12:00:00Z",
    "endpoint.url": "https://api.example.com/telemetry"
  }
  
  let valid_format_result = FormatValidator::validate(format_validator, valid_format_data)
  assert_true(valid_format_result.valid)
  
  // 测试无效IP地址
  let invalid_ip_data = {
    "ip.address": "256.256.256.256"  // 无效的IP地址
  }
  
  let invalid_ip_result = FormatValidator::validate(format_validator, invalid_ip_data)
  assert_false(invalid_ip_result.valid)
  
  // 测试无效电子邮件
  let invalid_email_data = {
    "email.address": "invalid.email"  // 缺少@和域名
  }
  
  let invalid_email_result = FormatValidator::validate(format_validator, invalid_email_data)
  assert_false(invalid_email_result.valid)
  
  // 测试无效UUID
  let invalid_uuid_data = {
    "uuid": "invalid-uuid"  // 不是有效的UUID格式
  }
  
  let invalid_uuid_result = FormatValidator::validate(format_validator, invalid_uuid_data)
  assert_false(invalid_uuid_result.valid)
  
  // 测试无效时间戳
  let invalid_timestamp_data = {
    "iso.timestamp": "2022-13-01T12:00:00Z"  // 无效的月份
  }
  
  let invalid_timestamp_result = FormatValidator::validate(format_validator, invalid_timestamp_data)
  assert_false(invalid_timestamp_result.valid)
  
  // 测试无效URL
  let invalid_url_data = {
    "endpoint.url": "ftp://example.com"  // 不允许的协议
  }
  
  let invalid_url_result = FormatValidator::validate(format_validator, invalid_url_data)
  assert_false(invalid_url_result.valid)
  
  // 测试自定义正则表达式格式
  FormatValidator::add_regex_format_rule(format_validator, "custom.id", {
    pattern: "^[A-Z]{2}-\\d{4}-[A-Z]{3}$",
    message: "ID格式必须是XX-0000-XXX"
  })
  
  let valid_custom_data = {
    "custom.id": "US-1234-ABC"
  }
  
  let valid_custom_result = FormatValidator::validate(format_validator, valid_custom_data)
  assert_true(valid_custom_result.valid)
  
  let invalid_custom_data = {
    "custom.id": "us-1234-abc"  // 小写字母
  }
  
  let invalid_custom_result = FormatValidator::validate(format_validator, invalid_custom_data)
  assert_false(invalid_custom_result.valid)
}

// 测试7: 数据质量评估
test "数据质量评估测试" {
  // 创建数据质量评估器
  let quality_assessor = DataQualityAssessor::new()
  
  // 添加完整性评分规则
  quality_assessor.add_completeness_rule({
    field: "required_fields",
    weight: 0.4,
    threshold: 0.9
  })
  
  // 添加准确性评分规则
  quality_assessor.add_accuracy_rule({
    field: "data_validation",
    weight: 0.3,
    threshold: 0.95
  })
  
  // 添加一致性评分规则
  quality_assessor.add_consistency_rule({
    field: "cross_reference_check",
    weight: 0.2,
    threshold: 0.85
  })
  
  // 添加时效性评分规则
  quality_assessor.add_timeliness_rule({
    field: "data_freshness",
    weight: 0.1,
    max_age_hours: 24,
    threshold: 0.8
  })
  
  // 测试高质量数据
  let high_quality_data = {
    "required_fields": {
      "total": 10,
      "present": 10,
      "missing": []
    },
    "data_validation": {
      "total": 100,
      "valid": 98,
      "invalid": 2
    },
    "cross_reference_check": {
      "total": 50,
      "consistent": 45,
      "inconsistent": 5
    },
    "data_freshness": {
      "max_age_hours": 2,
      "data_timestamp": "2022-01-01T12:00:00Z",
      "current_timestamp": "2022-01-01T13:30:00Z"
    }
  }
  
  let high_quality_result = quality_assessor.assess(high_quality_data)
  assert_true(high_quality_result.overall_score >= 0.9)
  assert_eq(high_quality_result.grade, "A")
  assert_true(high_quality_result.acceptable)
  
  // 测试中等质量数据
  let medium_quality_data = {
    "required_fields": {
      "total": 10,
      "present": 8,
      "missing": ["field3", "field7"]
    },
    "data_validation": {
      "total": 100,
      "valid": 90,
      "invalid": 10
    },
    "cross_reference_check": {
      "total": 50,
      "consistent": 40,
      "inconsistent": 10
    },
    "data_freshness": {
      "max_age_hours": 12,
      "data_timestamp": "2022-01-01T01:00:00Z",
      "current_timestamp": "2022-01-01T13:00:00Z"
    }
  }
  
  let medium_quality_result = quality_assessor.assess(medium_quality_data)
  assert_true(medium_quality_result.overall_score >= 0.7 and medium_quality_result.overall_score < 0.9)
  assert_eq(medium_quality_result.grade, "B")
  assert_true(medium_quality_result.acceptable)
  
  // 测试低质量数据
  let low_quality_data = {
    "required_fields": {
      "total": 10,
      "present": 5,
      "missing": ["field2", "field4", "field6", "field8", "field9"]
    },
    "data_validation": {
      "total": 100,
      "valid": 70,
      "invalid": 30
    },
    "cross_reference_check": {
      "total": 50,
      "consistent": 30,
      "inconsistent": 20
    },
    "data_freshness": {
      "max_age_hours": 48,
      "data_timestamp": "2021-12-30T13:00:00Z",
      "current_timestamp": "2022-01-01T13:00:00Z"
    }
  }
  
  let low_quality_result = quality_assessor.assess(low_quality_data)
  assert_true(low_quality_result.overall_score < 0.7)
  assert_true(["C", "D", "F"].contains(low_quality_result.grade))
  assert_false(low_quality_result.acceptable)
  
  // 测试质量改进建议
  let improvement_suggestions = quality_assessor.get_improvement_suggestions(low_quality_data)
  assert_true(improvement_suggestions.length() > 0)
  
  let completeness_suggestion = improvement_suggestions.find(fn(s) { s.category == "completeness" })
  assert_true(completeness_suggestion != None)
  
  match completeness_suggestion {
    Some(suggestion) => {
      assert_true(suggestion.priority == "high")
      assert_true(suggestion.description.contains("missing fields"))
    }
    None => assert_true(false)
  }
}

// 测试8: 数据验证性能测试
test "数据验证性能测试" {
  // 创建高性能验证器
  let performance_validator = PerformanceValidator::new({
    enable_caching: true,
    cache_size: 1000,
    parallel_validation: true,
    max_workers: 4
  })
  
  // 添加验证规则
  performance_validator.add_rule("field1", {
    type: "string",
    required: true,
    min_length: 1,
    max_length: 100
  })
  
  performance_validator.add_rule("field2", {
    type: "number",
    required: true,
    min_value: 0,
    max_value: 1000
  })
  
  performance_validator.add_rule("field3", {
    type: "array",
    required: false,
    max_items: 100
  })
  
  // 生成大量测试数据
  let large_dataset = []
  for i in 0..=1000 {
    large_dataset = large_dataset.push({
      "field1": "value" + i.to_string(),
      "field2": i % 1000,
      "field3": if i % 2 == 0 { [1, 2, 3, 4, 5] } else { [] }
    })
  }
  
  // 测试批量验证性能
  let start_time = Time::now()
  let batch_results = performance_validator.validate_batch(large_dataset)
  let end_time = Time::now()
  
  let validation_duration = end_time - start_time
  let throughput = large_dataset.length().to_float() / (validation_duration.to_float() / 1000.0)
  
  // 验证性能指标
  assert_true(throughput > 100)  // 至少每秒验证100条记录
  assert_eq(batch_results.length(), large_dataset.length())
  
  // 验证缓存效果
  let cached_data = large_dataset[0]
  
  // 第一次验证（无缓存）
  let first_validation_start = Time::now()
  performance_validator.validate_single(cached_data)
  let first_validation_duration = Time::now() - first_validation_start
  
  // 第二次验证（使用缓存）
  let second_validation_start = Time::now()
  performance_validator.validate_single(cached_data)
  let second_validation_duration = Time::now() - second_validation_start
  
  // 缓存应该提高性能
  assert_true(second_validation_duration <= first_validation_duration)
  
  // 测试并行验证
  let parallel_start_time = Time::now()
  let parallel_results = performance_validator.validate_parallel(large_dataset, 4)
  let parallel_end_time = Time::now()
  
  let parallel_duration = parallel_end_time - parallel_start_time
  let parallel_throughput = large_dataset.length().to_float() / (parallel_duration.to_float() / 1000.0)
  
  // 并行验证应该更快
  assert_true(parallel_throughput >= throughput)
  
  // 验证结果一致性
  let batch_valid_count = batch_results.filter(fn(r) { r.valid }).length()
  let parallel_valid_count = parallel_results.filter(fn(r) { r.valid }).length()
  
  assert_eq(batch_valid_count, parallel_valid_count)
  
  // 测试内存使用
  let memory_before = performance_validator.get_memory_usage()
  
  // 验证大量数据
  for data in large_dataset {
    performance_validator.validate_single(data)
  }
  
  let memory_after = performance_validator.get_memory_usage()
  let memory_increase = memory_after - memory_before
  
  // 内存增长应该是合理的
  assert_true(memory_increase < 100 * 1024 * 1024)  // 不应超过100MB
  
  // 清理缓存
  performance_validator.clear_cache()
  
  let memory_after_cleanup = performance_validator.get_memory_usage()
  assert_true(memory_after_cleanup < memory_after)
}

// 测试9: 自定义验证规则
test "自定义验证规则测试" {
  // 创建验证器
  let custom_validator = DataValidator::new()
  
  // 添加自定义验证函数
  let custom_business_rule = fn(data: Map[String, Any]) {
    match data.get("business.hours") {
      Some(hours) => {
        let hours_value = hours.to_int()
        hours_value >= 0 and hours_value <= 24 and hours_value % 0.5 == 0
      }
      None => false
    }
  }
  
  custom_validator.add_custom_rule("business.hours", custom_business_rule, {
    message: "营业时间必须是0到24之间的0.5的倍数"
  })
  
  // 添加跨字段自定义验证
  let cross_field_validation = fn(data: Map[String, Any]) {
    let start_time = data.get("start.time")
    let end_time = data.get("end.time")
    
    match (start_time, end_time) {
      (Some(start), Some(end)) => {
        let start_val = start.to_int()
        let end_val = end.to_int()
        
        // 检查时间顺序和合理的持续时间
        if start_val >= end_val {
          false
        } else {
          let duration = end_val - start_val
          duration >= 60 and duration <= 86400  // 1分钟到1天
        }
      }
      _ => false
    }
  }
  
  custom_validator.add_custom_rule("time.range", cross_field_validation, {
    message: "结束时间必须晚于开始时间，且持续时间在1分钟到1天之间"
  })
  
  // 添加条件自定义验证
  let conditional_validation = fn(data: Map[String, Any]) {
    let user_type = data.get("user.type")
    let permissions = data.get("permissions")
    
    match (user_type, permissions) {
      (Some(u_type), Some(perms)) => {
        if u_type.to_string() == "admin" {
          true  // 管理员有所有权限
        } else if u_type.to_string() == "user" {
          let perms_list = perms.to_array()
          // 普通用户不能有admin权限
          not(perms_list.contains("admin"))
        } else {
          false
        }
      }
      _ => false
    }
  }
  
  custom_validator.add_custom_rule("user.permissions", conditional_validation, {
    message: "用户权限必须与其类型匹配"
  })
  
  // 测试有效的自定义规则数据
  let valid_custom_data = {
    "business.hours": 9.5,
    "start.time": 1640995200,
    "end.time": 1640998800,
    "user.type": "user",
    "permissions": ["read", "write"]
  }
  
  let valid_custom_result = custom_validator.validate(valid_custom_data)
  assert_true(valid_custom_result.valid)
  
  // 测试无效的营业时间
  let invalid_hours_data = {
    "business.hours": 25.5  // 超出范围
  }
  
  let invalid_hours_result = custom_validator.validate(invalid_hours_data)
  assert_false(invalid_hours_result.valid)
  
  // 测试无效的时间范围
  let invalid_time_data = {
    "start.time": 1640998800,
    "end.time": 1640995200  // 早于开始时间
  }
  
  let invalid_time_result = custom_validator.validate(invalid_time_data)
  assert_false(invalid_time_result.valid)
  
  // 测试无效的用户权限
  let invalid_permissions_data = {
    "user.type": "user",
    "permissions": ["read", "write", "admin"]  // 普通用户不能有admin权限
  }
  
  let invalid_permissions_result = custom_validator.validate(invalid_permissions_data)
  assert_false(invalid_permissions_result.valid)
  
  // 测试管理员权限
  let admin_permissions_data = {
    "user.type": "admin",
    "permissions": ["read", "write", "admin", "delete"]
  }
  
  let admin_permissions_result = custom_validator.validate(admin_permissions_data)
  assert_true(admin_permissions_result.valid)
}

// 测试10: 验证规则组合和链式验证
test "验证规则组合和链式验证测试" {
  // 创建链式验证器
  let chain_validator = ChainValidator::new()
  
  // 添加验证阶段
  chain_validator.add_stage("syntax", {
    required: true,
    stop_on_failure: true
  })
  
  chain_validator.add_stage("semantic", {
    required: true,
    stop_on_failure: false
  })
  
  chain_validator.add_stage("business", {
    required: false,
    stop_on_failure: false
  })
  
  // 为每个阶段添加规则
  chain_validator.add_rule_to_stage("syntax", "field.format", {
    type: "format",
    field: "email",
    format: "email"
  })
  
  chain_validator.add_rule_to_stage("semantic", "field.consistency", {
    type: "consistency",
    rule: "age >= 0"
  })
  
  chain_validator.add_rule_to_stage("business", "field.policy", {
    type: "business",
    rule: "discount <= max_discount"
  })
  
  // 测试多阶段验证
  let multi_stage_data = {
    "email": "user@example.com",
    "age": 25,
    "discount": 10,
    "max_discount": 20
  }
  
  let multi_stage_result = chain_validator.validate(multi_stage_data)
  assert_true(multi_stage_result.overall_valid)
  
  // 检查各阶段结果
  assert_true(multi_stage_result.stage_results.get("syntax").valid)
  assert_true(multi_stage_result.stage_results.get("semantic").valid)
  assert_true(multi_stage_result.stage_results.get("business").valid)
  
  // 测试语法错误（应该停止后续验证）
  let syntax_error_data = {
    "email": "invalid-email",
    "age": 25,
    "discount": 10,
    "max_discount": 20
  }
  
  let syntax_error_result = chain_validator.validate(syntax_error_data)
  assert_false(syntax_error_result.overall_valid)
  assert_false(syntax_error_result.stage_results.get("syntax").valid)
  assert_eq(syntax_error_result.stage_results.get("semantic"), None)  // 未执行
  assert_eq(syntax_error_result.stage_results.get("business"), None)  // 未执行
  
  // 测试组合验证器
  let composite_validator = CompositeValidator::new()
  
  // 添加验证器组合
  composite_validator.add_validator("basic", {
    type: "basic",
    rules: [
      { field: "name", type: "string", required: true },
      { field: "age", type: "number", required: true }
    ]
  })
  
  composite_validator.add_validator("advanced", {
    type: "advanced",
    rules: [
      { field: "email", type: "email", required: false },
      { field: "phone", type: "phone", required: false }
    ]
  })
  
  // 设置组合策略
  composite_validator.set_strategy("all_required")  // 所有验证器都必须通过
  
  // 测试组合验证
  let composite_data = {
    "name": "John Doe",
    "age": 30,
    "email": "john@example.com",
    "phone": "+1234567890"
  }
  
  let composite_result = composite_validator.validate(composite_data)
  assert_true(composite_result.overall_valid)
  assert_true(composite_result.validator_results.get("basic").valid)
  assert_true(composite_result.validator_results.get("advanced").valid)
  
  // 测试部分验证失败
  let partial_failure_data = {
    "name": "Jane Doe",
    // 缺少 age
    "email": "jane@example.com"
  }
  
  let partial_failure_result = composite_validator.validate(partial_failure_data)
  assert_false(partial_failure_result.overall_valid)
  assert_false(partial_failure_result.validator_results.get("basic").valid)
  assert_true(partial_failure_result.validator_results.get("advanced").valid)
  
  // 更改策略为至少一个验证器通过
  composite_validator.set_strategy("any_valid")
  
  let any_valid_result = composite_validator.validate(partial_failure_data)
  assert_true(any_valid_result.overall_valid)  // 至少advanced验证器通过
}
// Azimuth 数据验证和转换测试用例
// 专注于数据验证规则和类型转换功能

// 测试1: 基础数据类型验证
test "基础数据类型验证" {
  // 创建验证器
  let validator = DataValidator::new()
  
  // 添加字符串验证规则
  Validator::add_string_rule(validator, "service_name", {
    min_length: 3,
    max_length: 50,
    pattern: "^[a-z][a-z0-9-]*$",
    required: true
  })
  
  // 添加数字验证规则
  Validator::add_number_rule(validator, "port", {
    min_value: 1,
    max_value: 65535,
    integer_only: true,
    required: true
  })
  
  // 添加布尔值验证规则
  Validator::add_boolean_rule(validator, "enabled", {
    required: false,
    default_value: true
  })
  
  // 测试有效数据
  let valid_data = {
    service_name: "payment-service",
    port: 8080,
    enabled: true
  }
  
  let valid_result = Validator::validate(validator, valid_data)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // 测试无效数据
  let invalid_data = {
    service_name: "Invalid Service!",  // 包含无效字符
    port: 70000,  // 超出范围
    enabled: "yes"  // 错误类型
  }
  
  let invalid_result = Validator::validate(validator, invalid_data)
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.errors.length() >= 2)
  
  // 检查具体错误
  let service_name_error = invalid_result.errors.find(fn(e) { e.field == "service_name" })
  assert_true(service_name_error != None)
  
  let port_error = invalid_result.errors.find(fn(e) { e.field == "port" })
  assert_true(port_error != None)
}

// 测试2: 复杂对象验证
test "复杂对象验证" {
  // 创建嵌套对象验证器
  let nested_validator = DataValidator::new()
  
  // 定义地址对象规则
  Validator::add_object_rule(nested_validator, "address", {
    required: true,
    fields: [
      ("street", { type: "string", min_length: 5, max_length: 100 }),
      ("city", { type: "string", min_length: 2, max_length: 50 }),
      ("state", { type: "string", length: 2 }),
      ("postal_code", { type: "string", pattern: "^\\d{5}(-\\d{4})?$" }),
      ("country", { type: "string", length: 2, default_value: "US" })
    ]
  })
  
  // 定义用户对象规则
  Validator::add_object_rule(nested_validator, "user", {
    required: true,
    fields: [
      ("id", { type: "number", integer_only: true, min_value: 1 }),
      ("name", { type: "string", min_length: 1, max_length: 100 }),
      ("email", { type: "string", pattern: "^[\\w.-]+@[\\w.-]+\\.[a-zA-Z]{2,}$" }),
      ("age", { type: "number", integer_only: true, min_value: 18, max_value: 120 }),
      ("address", { type: "object", validator: "address" }),
      ("preferences", { type: "object", required: false })
    ]
  })
  
  // 测试有效的嵌套对象
  let valid_user = {
    id: 123,
    name: "John Doe",
    email: "john.doe@example.com",
    age: 30,
    address: {
      street: "123 Main St",
      city: "Anytown",
      state: "CA",
      postal_code: "12345",
      country: "US"
    },
    preferences: {
      theme: "dark",
      notifications: true
    }
  }
  
  let valid_result = Validator::validate_nested(nested_validator, "user", valid_user)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // 测试无效的嵌套对象
  let invalid_user = {
    id: -1,  // 无效ID
    name: "",  // 空名称
    email: "invalid-email",  // 无效邮箱
    age: 15,  // 年龄过小
    address: {
      street: "123",  // 街道太短
      city: "A",  // 城市太短
      state: "California",  // 州太长
      postal_code: "123456"  // 无效邮编
    }
  }
  
  let invalid_result = Validator::validate_nested(nested_validator, "user", invalid_user)
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.errors.length() >= 6)
  
  // 检查嵌套错误路径
  let street_error = invalid_result.errors.find(fn(e) { e.field == "address.street" })
  assert_true(street_error != None)
  
  let email_error = invalid_result.errors.find(fn(e) { e.field == "email" })
  assert_true(email_error != None)
}

// 测试3: 数组验证
test "数组验证" {
  // 创建数组验证器
  let array_validator = DataValidator::new()
  
  // 添加简单数组验证规则
  Validator::add_array_rule(array_validator, "tags", {
    item_type: "string",
    min_items: 1,
    max_items: 10,
    unique_items: true,
    required: false
  })
  
  // 添加数字数组验证规则
  Validator::add_array_rule(array_validator, "metrics", {
    item_type: "number",
    min_items: 3,
    max_items: 100,
    item_constraints: {
      min_value: 0,
      max_value: 100
    }
  })
  
  // 添加对象数组验证规则
  Validator::add_array_rule(array_validator, "endpoints", {
    item_type: "object",
    min_items: 1,
    item_schema: {
      path: { type: "string", pattern: "^/api/.*" },
      method: { type: "string", enum: ["GET", "POST", "PUT", "DELETE"] },
      timeout: { type: "number", min_value: 100, max_value: 30000 }
    }
  })
  
  // 测试有效数组
  let valid_data = {
    tags: ["production", "api", "v1"],
    metrics: [10.5, 25.0, 87.3, 42.1],
    endpoints: [
      { path: "/api/users", method: "GET", timeout: 5000 },
      { path: "/api/orders", method: "POST", timeout: 10000 }
    ]
  }
  
  let valid_result = Validator::validate(array_validator, valid_data)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // 测试无效数组
  let invalid_data = {
    tags: [],  // 空数组，违反min_items
    metrics: [150.0, -10.0, 50.0],  // 包含超出范围的值
    endpoints: [
      { path: "invalid-path", method: "INVALID", timeout: 50 }  // 多个验证错误
    ]
  }
  
  let invalid_result = Validator::validate(array_validator, invalid_data)
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.errors.length() >= 4)
  
  // 检查数组特定错误
  let tags_error = invalid_result.errors.find(fn(e) { e.field == "tags" })
  assert_true(tags_error != None)
  
  let metrics_error = invalid_result.errors.find(fn(e) { e.field == "metrics" })
  assert_true(metrics_error != None)
}

// 测试4: 条件验证
test "条件验证" {
  // 创建条件验证器
  let conditional_validator = DataValidator::new()
  
  // 添加条件规则：如果type是"database"，则必须提供connection_string
  Validator::add_conditional_rule(conditional_validator, {
    condition: "type == 'database'",
    required_fields: ["connection_string", "max_connections"],
    field_rules: [
      ("connection_string", { type: "string", pattern: "^[\\w\\.-]+://.*" }),
      ("max_connections", { type: "number", min_value: 1, max_value: 1000 })
    ]
  })
  
  // 添加条件规则：如果type是"file"，则必须提供file_path
  Validator::add_conditional_rule(conditional_validator, {
    condition: "type == 'file'",
    required_fields: ["file_path"],
    field_rules: [
      ("file_path", { type: "string", pattern: "^/.*" }),
      ("rotation", { type: "boolean", default_value: false })
    ]
  })
  
  // 测试数据库配置
  let valid_db_config = {
    type: "database",
    connection_string: "postgresql://localhost:5432/mydb",
    max_connections: 100
  }
  
  let db_result = Validator::validate(conditional_validator, valid_db_config)
  assert_true(db_result.is_valid)
  
  // 测试无效数据库配置（缺少必需字段）
  let invalid_db_config = {
    type: "database",
    connection_string: "postgresql://localhost:5432/mydb"
    // 缺少max_connections
  }
  
  let invalid_db_result = Validator::validate(conditional_validator, invalid_db_config)
  assert_false(invalid_db_result.is_valid)
  
  // 测试文件配置
  let valid_file_config = {
    type: "file",
    file_path: "/var/log/app.log",
    rotation: true
  }
  
  let file_result = Validator::validate(conditional_validator, valid_file_config)
  assert_true(file_result.is_valid)
  
  // 测试无效文件配置（缺少必需字段）
  let invalid_file_config = {
    type: "file"
    // 缺少file_path
  }
  
  let invalid_file_result = Validator::validate(conditional_validator, invalid_file_config)
  assert_false(invalid_file_result.is_valid)
}

// 测试5: 自定义验证函数
test "自定义验证函数" {
  // 创建自定义验证器
  let custom_validator = DataValidator::new()
  
  // 添加自定义验证函数：检查密码强度
  Validator::add_custom_rule(custom_validator, "password", fn(value) {
    let password = value.to_string()
    let has_upper = password.any(fn(c) { c >= 'A' and c <= 'Z' })
    let has_lower = password.any(fn(c) { c >= 'a' and c <= 'z' })
    let has_digit = password.any(fn(c) { c >= '0' and c <= '9' })
    let has_special = password.any(fn(c) { not(c.is_alphanumeric()) })
    let is_long_enough = password.length() >= 8
    
    if has_upper and has_lower and has_digit and has_special and is_long_enough {
      ValidationSuccess()
    } else {
      ValidationError("Password must contain at least 8 characters including uppercase, lowercase, digit, and special character")
    }
  })
  
  // 添加自定义验证函数：检查信用卡校验和
  Validator::add_custom_rule(custom_validator, "credit_card", fn(value) {
    let card_number = value.to_string().replace("-", "").replace(" ", "")
    
    // Luhn算法验证
    let mut sum = 0
    let mut double = false
    
    for i in 0..card_number.length() {
      let digit = card_number[card_number.length() - 1 - i].to_int() - '0'.to_int()
      
      if double {
        let doubled = digit * 2
        sum = sum + if doubled > 9 { doubled - 9 } else { doubled }
      } else {
        sum = sum + digit
      }
      
      double = not(double)
    }
    
    if sum % 10 == 0 {
      ValidationSuccess()
    } else {
      ValidationError("Invalid credit card number")
    }
  })
  
  // 测试有效密码
  let valid_password_data = {
    password: "StrongP@ssw0rd"
  }
  
  let valid_password_result = Validator::validate(custom_validator, valid_password_data)
  assert_true(valid_password_result.is_valid)
  
  // 测试无效密码
  let invalid_password_data = {
    password: "weak"
  }
  
  let invalid_password_result = Validator::validate(custom_validator, invalid_password_data)
  assert_false(invalid_password_result.is_valid)
  
  // 测试有效信用卡号
  let valid_card_data = {
    credit_card: "4539-1488-0343-6467"  // 测试用的Visa卡号
  }
  
  let valid_card_result = Validator::validate(custom_validator, valid_card_data)
  assert_true(valid_card_result.is_valid)
  
  // 测试无效信用卡号
  let invalid_card_data = {
    credit_card: "1234-5678-9012-3456"
  }
  
  let invalid_card_result = Validator::validate(custom_validator, invalid_card_data)
  assert_false(invalid_card_result.is_valid)
}

// 测试6: 数据类型转换
test "数据类型转换" {
  // 创建类型转换器
  let converter = TypeConverter::new()
  
  // 添加字符串到数字的转换规则
  Converter::add_rule(converter, "string_to_int", {
    from_type: "string",
    to_type: "integer",
    convert_fn: fn(value) {
      let str_value = value.to_string()
      let mut result = 0
      let mut is_negative = false
      let mut i = 0
      
      if str_value.length() > 0 and str_value[0] == '-' {
        is_negative = true
        i = 1
      }
      
      while i < str_value.length() {
        let c = str_value[i]
        if c >= '0' and c <= '9' {
          result = result * 10 + (c.to_int() - '0'.to_int())
        } else {
          return ConversionError("Invalid integer format")
        }
        i = i + 1
      }
      
      if is_negative {
        ConversionSuccess(-result)
      } else {
        ConversionSuccess(result)
      }
    }
  })
  
  // 添加字符串到布尔值的转换规则
  Converter::add_rule(converter, "string_to_bool", {
    from_type: "string",
    to_type: "boolean",
    convert_fn: fn(value) {
      let str_value = value.to_string().to_lowercase()
      match str_value {
        "true" | "1" | "yes" | "on" => ConversionSuccess(true),
        "false" | "0" | "no" | "off" => ConversionSuccess(false),
        _ => ConversionError("Invalid boolean format")
      }
    }
  })
  
  // 添加JSON字符串到对象的转换规则
  Converter::add_rule(converter, "json_to_object", {
    from_type: "string",
    to_type: "object",
    convert_fn: fn(value) {
      let json_str = value.to_string()
      JSONParser::parse(json_str)
    }
  })
  
  // 测试字符串到整数转换
  let valid_int_result = Converter::convert(converter, "123", "string_to_int")
  match valid_int_result {
    ConversionSuccess(value) => assert_eq(value, 123),
    ConversionError(msg) => assert_true(false)
  }
  
  let negative_int_result = Converter::convert(converter, "-456", "string_to_int")
  match negative_int_result {
    ConversionSuccess(value) => assert_eq(value, -456),
    ConversionError(msg) => assert_true(false)
  }
  
  let invalid_int_result = Converter::convert(converter, "12a3", "string_to_int")
  match invalid_int_result {
    ConversionSuccess(value) => assert_true(false),
    ConversionError(msg) => assert_true(msg.contains("Invalid integer format"))
  }
  
  // 测试字符串到布尔值转换
  let true_variants = ["true", "1", "yes", "on"]
  for variant in true_variants {
    let result = Converter::convert(converter, variant, "string_to_bool")
    match result {
      ConversionSuccess(value) => assert_true(value),
      ConversionError(msg) => assert_true(false)
    }
  }
  
  let false_variants = ["false", "0", "no", "off"]
  for variant in false_variants {
    let result = Converter::convert(converter, variant, "string_to_bool")
    match result {
      ConversionSuccess(value) => assert_false(value),
      ConversionError(msg) => assert_true(false)
    }
  }
  
  let invalid_bool_result = Converter::convert(converter, "maybe", "string_to_bool")
  match invalid_bool_result {
    ConversionSuccess(value) => assert_true(false),
    ConversionError(msg) => assert_true(msg.contains("Invalid boolean format"))
  }
  
  // 测试JSON到对象转换
  let valid_json = "{\"name\": \"test\", \"value\": 42, \"active\": true}"
  let json_result = Converter::convert(converter, valid_json, "json_to_object")
  match json_result {
    ConversionSuccess(obj) => {
      assert_eq(obj.get("name"), "test")
      assert_eq(obj.get("value"), 42)
      assert_eq(obj.get("active"), true)
    }
    ConversionError(msg) => assert_true(false)
  }
  
  let invalid_json = "{\"name\": \"test\", \"value\": 42, \"active\":"
  let invalid_json_result = Converter::convert(converter, invalid_json, "json_to_object")
  match invalid_json_result {
    ConversionSuccess(value) => assert_true(false),
    ConversionError(msg) => assert_true(msg.contains("parse error"))
  }
}

// 测试7: 批量数据验证
test "批量数据验证" {
  // 创建批量验证器
  let batch_validator = BatchValidator::new()
  
  // 配置批量验证选项
  BatchValidator::set_options(batch_validator, {
    stop_on_first_error: false,
    max_errors: 100,
    include_valid_indices: true
  })
  
  // 添加验证规则
  BatchValidator::add_rule(batch_validator, "email", {
    type: "string",
    pattern: "^[\\w.-]+@[\\w.-]+\\.[a-zA-Z]{2,}$"
  })
  
  // 创建测试数据集
  let email_list = [
    "user1@example.com",
    "invalid-email",
    "user2@example.com",
    "user3@",
    "user4@example.org",
    "user5@sub.domain.com",
    "not-an-email",
    "user6@example.co.uk"
  ]
  
  // 执行批量验证
  let batch_result = BatchValidator::validate_batch(batch_validator, email_list, "email")
  
  // 验证批量结果
  assert_false(batch_result.all_valid)
  assert_eq(batch_result.valid_count, 4)  // 4个有效邮箱
  assert_eq(batch_result.invalid_count, 4)  // 4个无效邮箱
  assert_eq(batch_result.errors.length(), 4)
  assert_eq(batch_result.valid_indices.length(), 4)
  
  // 检查有效索引
  let expected_valid_indices = [0, 2, 4, 5]
  for i in expected_valid_indices {
    assert_true(batch_result.valid_indices.contains(i))
  }
  
  // 测试部分批量验证（只验证前5个）
  let partial_result = BatchValidator::validate_batch_partial(batch_validator, email_list, "email", 5)
  
  assert_eq(partial_result.total_processed, 5)
  assert_eq(partial_result.valid_count, 3)
  assert_eq(partial_result.invalid_count, 2)
  
  // 测试并行批量验证
  let parallel_result = BatchValidator::validate_batch_parallel(batch_validator, email_list, "email", 4)
  
  assert_false(parallel_result.all_valid)
  assert_eq(parallel_result.valid_count, 4)
  assert_eq(parallel_result.invalid_count, 4)
  
  // 测试大型数据集验证
  let large_dataset = []
  for i in 0..=1000 {
    if i % 3 == 0 {
      large_dataset = large_dataset.push("user" + i.to_string() + "@example.com")
    } else {
      large_dataset = large_dataset.push("invalid-email-" + i.to_string())
    }
  }
  
  let large_result = BatchValidator::validate_batch_parallel(batch_validator, large_dataset, "email", 8)
  
  assert_eq(large_result.total_processed, 1001)
  assert_eq(large_result.valid_count, 334)  // 约1/3有效
  assert_eq(large_result.invalid_count, 667)  // 约2/3无效
}

// 测试8: 验证结果格式化
test "验证结果格式化" {
  // 创建验证器
  let validator = DataValidator::new()
  
  // 添加验证规则
  Validator::add_string_rule(validator, "name", {
    min_length: 3,
    max_length: 50,
    required: true
  })
  
  Validator::add_number_rule(validator, "age", {
    min_value: 18,
    max_value: 120,
    required: true
  })
  
  Validator::add_email_rule(validator, "email", {
    required: true
  })
  
  // 测试数据
  let test_data = {
    name: "Jo",  // 太短
    age: 15,  // 太小
    email: "invalid-email"  // 无效格式
  }
  
  // 执行验证
  let validation_result = Validator::validate(validator, test_data)
  
  // 格式化为详细报告
  let detailed_report = ValidationFormatter::detailed(validation_result)
  assert_true(detailed_report.contains("Validation Failed"))
  assert_true(detailed_report.contains("name"))
  assert_true(detailed_report.contains("age"))
  assert_true(detailed_report.contains("email"))
  
  // 格式化为简洁报告
  let summary_report = ValidationFormatter::summary(validation_result)
  assert_true(summary_report.contains("3 errors"))
  assert_false(summary_report.contains("Jo"))
  
  // 格式化为JSON
  let json_report = ValidationFormatter::json(validation_result)
  assert_true(json_report.contains("\"is_valid\": false"))
  assert_true(json_report.contains("\"errors\""))
  
  // 解析JSON报告
  let parsed_json = JSONParser::parse(json_report)
  match parsed_json {
    ConversionSuccess(obj) => {
      assert_eq(obj.get("is_valid"), false)
      assert_eq(obj.get("error_count"), 3)
    }
    ConversionError(msg) => assert_true(false)
  }
  
  // 格式化为HTML报告
  let html_report = ValidationFormatter::html(validation_result)
  assert_true(html_report.contains("<div class=\"validation-report\">"))
  assert_true(html_report.contains("<table class=\"errors\">"))
  
  // 格式化为自定义模板
  let custom_template = "Validation {status}: {error_count} errors found\nFields: {error_fields}"
  let custom_report = ValidationFormatter::template(validation_result, custom_template)
  
  assert_true(custom_report.contains("Validation FAILED"))
  assert_true(custom_report.contains("3 errors found"))
  assert_true(custom_report.contains("Fields: name, age, email"))
}
// Azimuth Data Validation and Conversion Tests
// This file contains test cases for data validation and conversion

// Test 1: Schema Validation
test "模式验证" {
  // 定义验证规则
  enum ValidationRule {
    Required
    MinLength(Int)
    MaxLength(Int)
    MinValue(Int)
    MaxValue(Int)
    Pattern(String)  // 正则表达式模式
    Custom((String) -> Bool)
  }
  
  // 定义字段定义
  type FieldDefinition = {
    name: String,
    type: String,
    rules: Array[ValidationRule],
    required: Bool
  }
  
  // 定义模式
  type Schema = {
    name: String,
    fields: Array[FieldDefinition]
  }
  
  // 验证结果
  type ValidationResult = {
    is_valid: Bool,
    errors: Array[String]
  }
  
  // 验证单个值
  let validate_value = fn(value: String, rules: Array[ValidationRule]) {
    let mut errors = []
    
    for rule in rules {
      match rule {
        ValidationRule::Required => {
          if value == "" {
            errors = errors.push("Field is required")
          }
        }
        ValidationRule::MinLength(min_len) => {
          if value.length() < min_len {
            errors = errors.push("Value too short, minimum " + min_len.to_string() + " characters")
          }
        }
        ValidationRule::MaxLength(max_len) => {
          if value.length() > max_len {
            errors = errors.push("Value too long, maximum " + max_len.to_string() + " characters")
          }
        }
        ValidationRule::MinValue(min_val) => {
          let int_value = match value.parse_int() {
            Some(v) => v
            None => 0
          }
          if int_value < min_val {
            errors = errors.push("Value too small, minimum " + min_val.to_string())
          }
        }
        ValidationRule::MaxValue(max_val) => {
          let int_value = match value.parse_int() {
            Some(v) => v
            None => 0
          }
          if int_value > max_val {
            errors = errors.push("Value too large, maximum " + max_val.to_string())
          }
        }
        ValidationRule::Pattern(pattern) => {
          // 简化的模式匹配
          if pattern == "email" && not value.contains("@") {
            errors = errors.push("Invalid email format")
          } else if pattern == "phone" && value.length() != 10 {
            errors = errors.push("Invalid phone format")
          }
        }
        ValidationRule::Custom(validator_fn) => {
          if not validator_fn(value) {
            errors = errors.push("Custom validation failed")
          }
        }
      }
    }
    
    {
      is_valid: errors.length() == 0,
      errors
    }
  }
  
  // 验证数据对象
  let validate_data = fn(schema: Schema, data: Map[String, String]) {
    let mut all_errors = []
    
    for field in schema.fields {
      let field_value = match Map::get(data, field.name) {
        Some(value) => value
        None => ""
      }
      
      // 检查必填字段
      if field.required && field_value == "" {
        all_errors = all_errors.push(field.name + " is required")
      }
      
      // 如果有值，验证字段
      if field_value != "" || field.required {
        let result = validate_value(field_value, field.rules)
        all_errors = all_errors + result.errors.map(fn(error) { field.name + ": " + error })
      }
    }
    
    {
      is_valid: all_errors.length() == 0,
      errors: all_errors
    }
  }
  
  // 创建用户模式
  let user_schema = {
    name: "user",
    fields: [
      {
        name: "username",
        type: "string",
        rules: [ValidationRule::Required, ValidationRule::MinLength(3), ValidationRule::MaxLength(20)],
        required: true
      },
      {
        name: "email",
        type: "string",
        rules: [ValidationRule::Required, ValidationRule::Pattern("email")],
        required: true
      },
      {
        name: "age",
        type: "integer",
        rules: [ValidationRule::MinValue(18), ValidationRule::MaxValue(120)],
        required: false
      },
      {
        name: "phone",
        type: "string",
        rules: [ValidationRule::Pattern("phone")],
        required: false
      }
    ]
  }
  
  // 测试有效数据
  let valid_data = Map::from_array([
    ("username", "john_doe"),
    ("email", "john@example.com"),
    ("age", "25"),
    ("phone", "1234567890")
  ])
  
  let valid_result = validate_data(user_schema, valid_data)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // 测试无效数据
  let invalid_data = Map::from_array([
    ("username", "jo"),  // 太短
    ("email", "invalid-email"),  // 无效邮箱
    ("age", "15"),  // 太小
    ("phone", "123")  // 无效电话
  ])
  
  let invalid_result = validate_data(user_schema, invalid_data)
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.errors.length() >= 4)
  
  // 测试缺少必填字段
  let incomplete_data = Map::from_array([
    ("username", "john_doe")
    // 缺少email
  ])
  
  let incomplete_result = validate_data(user_schema, incomplete_data)
  assert_false(incomplete_result.is_valid)
  assert_true(incomplete_result.errors.any(fn(error) { error.contains("email") && error.contains("required") }))
}

// Test 2: Data Type Conversion
test "数据类型转换" {
  // 定义转换结果
  type ConversionResult[T] = {
    success: Bool,
    value: Option[T],
    error: Option[String]
  }
  
  // 字符串转整数
  let string_to_int = fn(str: String) {
    match str.parse_int() {
      Some(value) => {
        {
          success: true,
          value: Some(value),
          error: None
        }
      }
      None => {
        {
          success: false,
          value: None,
          error: Some("Invalid integer format")
        }
      }
    }
  }
  
  // 字符串转浮点数
  let string_to_float = fn(str: String) {
    match str.parse_float() {
      Some(value) => {
        {
          success: true,
          value: Some(value),
          error: None
        }
      }
      None => {
        {
          success: false,
          value: None,
          error: Some("Invalid float format")
        }
      }
    }
  }
  
  // 字符串转布尔值
  let string_to_bool = fn(str: String) {
    let lower_str = str.to_lowercase()
    if lower_str == "true" || lower_str == "1" || lower_str == "yes" {
      {
        success: true,
        value: Some(true),
        error: None
      }
    } else if lower_str == "false" || lower_str == "0" || lower_str == "no" {
      {
        success: true,
        value: Some(false),
        error: None
      }
    } else {
      {
        success: false,
        value: None,
        error: Some("Invalid boolean format")
      }
    }
  }
  
  // 整数转字符串
  let int_to_string = fn(value: Int) {
    {
      success: true,
      value: Some(value.to_string()),
      error: None
    }
  }
  
  // 浮点数转字符串
  let float_to_string = fn(value: Float) {
    {
      success: true,
      value: Some(value.to_string()),
      error: None
    }
  }
  
  // 布尔值转字符串
  let bool_to_string = fn(value: Bool) {
    {
      success: true,
      value: Some(if value { "true" } else { "false" }),
      error: None
    }
  }
  
  // 安全转换函数
  let safe_convert = fn(from: String, to_type: String) {
    match to_type {
      "int" => string_to_int(from),
      "float" => string_to_float(from),
      "bool" => string_to_bool(from),
      _ => {
        {
          success: false,
          value: None,
          error: Some("Unsupported target type")
        }
      }
    }
  }
  
  // 测试字符串到整数转换
  let valid_int = string_to_int("42")
  assert_true(valid_int.success)
  assert_eq(valid_int.value, Some(42))
  
  let invalid_int = string_to_int("not_a_number")
  assert_false(invalid_int.success)
  assert_eq(invalid_int.error, Some("Invalid integer format"))
  
  // 测试字符串到浮点数转换
  let valid_float = string_to_float("3.14")
  assert_true(valid_float.success)
  assert_eq(valid_float.value, Some(3.14))
  
  let invalid_float = string_to_float("not_a_float")
  assert_false(invalid_float.success)
  assert_eq(invalid_float.error, Some("Invalid float format"))
  
  // 测试字符串到布尔值转换
  let true_values = ["true", "1", "yes", "TRUE", "Yes"]
  for true_val in true_values {
    let result = string_to_bool(true_val)
    assert_true(result.success)
    assert_eq(result.value, Some(true))
  }
  
  let false_values = ["false", "0", "no", "FALSE", "No"]
  for false_val in false_values {
    let result = string_to_bool(false_val)
    assert_true(result.success)
    assert_eq(result.value, Some(false))
  }
  
  let invalid_bool = string_to_bool("maybe")
  assert_false(invalid_bool.success)
  assert_eq(invalid_bool.error, Some("Invalid boolean format"))
  
  // 测试其他类型到字符串转换
  let int_str = int_to_string(123)
  assert_true(int_str.success)
  assert_eq(int_str.value, Some("123"))
  
  let float_str = float_to_string(45.67)
  assert_true(float_str.success)
  assert_eq(float_str.value, Some("45.67"))
  
  let bool_str_true = bool_to_string(true)
  assert_true(bool_str_true.success)
  assert_eq(bool_str_true.value, Some("true"))
  
  let bool_str_false = bool_to_string(false)
  assert_true(bool_str_false.success)
  assert_eq(bool_str_false.value, Some("false"))
  
  // 测试安全转换函数
  let safe_int = safe_convert("123", "int")
  assert_true(safe_int.success)
  
  let safe_bool = safe_convert("yes", "bool")
  assert_true(safe_bool.success)
  
  let safe_unsupported = safe_convert("123", "date")
  assert_false(safe_unsupported.success)
  assert_eq(safe_unsupported.error, Some("Unsupported target type"))
}

// Test 3: Data Transformation Pipeline
test "数据转换管道" {
  // 定义转换步骤
  type TransformStep = {
    name: String,
    transform_fn: (Map[String, String]) -> Map[String, String]
  }
  
  // 定义转换管道
  type TransformPipeline = {
    name: String,
    steps: Array[TransformStep]
  }
  
  // 创建转换管道
  let create_pipeline = fn(name: String) {
    {
      name,
      steps: []
    }
  }
  
  // 添加转换步骤
  let add_step = fn(pipeline: TransformPipeline, step: TransformStep) {
    {
      ...pipeline,
      steps: pipeline.steps.push(step)
    }
  }
  
  // 执行转换管道
  let execute_pipeline = fn(pipeline: TransformPipeline, data: Map[String, String]) {
    let mut result = data
    
    for step in pipeline.steps {
      result = step.transform_fn(result)
    }
    
    result
  }
  
  // 常用转换函数
  let normalize_keys = fn(data: Map[String, String]) {
    let mut result = Map::empty()
    
    for (key, value) in data {
      let normalized_key = key.to_lowercase().replace(" ", "_")
      let _ = Map::insert(result, normalized_key, value)
    }
    
    result
  }
  
  let trim_values = fn(data: Map[String, String]) {
    let mut result = Map::empty()
    
    for (key, value) in data {
      let _ = Map::insert(result, key, value.trim())
    }
    
    result
  }
  
  let convert_types = fn(data: Map[String, String]) {
    let mut result = Map::empty()
    
    for (key, value) in data {
      let converted_value = if key.contains("count") || key.contains("age") {
        match value.parse_int() {
          Some(int_val) => int_val.to_string()
          None => value
        }
      } else if key.contains("price") || key.contains("rate") {
        match value.parse_float() {
          Some(float_val) => float_val.to_string()
          None => value
        }
      } else if key.contains("active") || key.contains("enabled") {
        let lower_val = value.to_lowercase()
        if lower_val == "true" || lower_val == "1" {
          "true"
        } else if lower_val == "false" || lower_val == "0" {
          "false"
        } else {
          value
        }
      } else {
        value
      }
      
      let _ = Map::insert(result, key, converted_value)
    }
    
    result
  }
  
  let add_derived_fields = fn(data: Map[String, String]) {
    let mut result = data
    
    // 添加全名字段
    let first_name = Map::get(data, "first_name")
    let last_name = Map::get(data, "last_name")
    
    match (first_name, last_name) {
      (Some(first), Some(last)) => {
        let full_name = first.trim() + " " + last.trim()
        let _ = Map::insert(result, "full_name", full_name)
      }
      _ => ()
    }
    
    // 添加年龄分类
    let age = Map::get(data, "age")
    match age {
      Some(age_str) => {
        match age_str.parse_int() {
          Some(age_int) => {
            let category = if age_int < 18 {
              "minor"
            } else if age_int < 65 {
              "adult"
            } else {
              "senior"
            }
            let _ = Map::insert(result, "age_category", category)
          }
          None => ()
        }
      }
      None => ()
    }
    
    result
  }
  
  // 创建测试管道
  let pipeline = create_pipeline("user_data_processing")
    |> add_step({ name: "normalize_keys", transform_fn: normalize_keys })
    |> add_step({ name: "trim_values", transform_fn: trim_values })
    |> add_step({ name: "convert_types", transform_fn: convert_types })
    |> add_step({ name: "add_derived_fields", transform_fn: add_derived_fields })
  
  // 测试数据
  let raw_data = Map::from_array([
    ("First Name", "  John  "),
    ("Last Name", "  Doe  "),
    ("Age", " 25"),
    ("Email", "john@example.com"),
    ("Active", "1"),
    ("Account Balance", "1000.50")
  ])
  
  // 执行管道
  let processed_data = execute_pipeline(pipeline, raw_data)
  
  // 验证转换结果
  assert_eq(Map::get(processed_data, "first_name"), Some("John"))
  assert_eq(Map::get(processed_data, "last_name"), Some("Doe"))
  assert_eq(Map::get(processed_data, "age"), Some("25"))
  assert_eq(Map::get(processed_data, "email"), Some("john@example.com"))
  assert_eq(Map::get(processed_data, "active"), Some("true"))
  assert_eq(Map::get(processed_data, "account_balance"), Some("1000.5"))
  
  // 验证派生字段
  assert_eq(Map::get(processed_data, "full_name"), Some("John Doe"))
  assert_eq(Map::get(processed_data, "age_category"), Some("adult"))
  
  // 测试边缘情况
  let edge_case_data = Map::from_array([
    ("First Name", "  Jane  "),
    ("Age", "17"),  // 未成年
    ("Active", "0")  // 非活跃
  ])
  
  let edge_case_result = execute_pipeline(pipeline, edge_case_data)
  assert_eq(Map::get(edge_case_result, "full_name"), Some("Jane"))
  assert_eq(Map::get(edge_case_result, "age_category"), Some("minor"))
  assert_eq(Map::get(edge_case_result, "active"), Some("false"))
}

// Test 4: Data Format Conversion
test "数据格式转换" {
  // 定义JSON对象
  type JsonObject = Map[String, JsonValue]
  
  enum JsonValue {
    JsonString(String)
    JsonNumber(Float)
    JsonBool(Bool)
    JsonNull
    JsonObject(JsonObject)
    JsonArray(Array[JsonValue])
  }
  
  // JSON到Map转换
  let json_to_map = fn(json: JsonObject) {
    let mut result = Map::empty()
    
    for (key, value) in json {
      let string_value = match value {
        JsonValue::JsonString(s) => s
        JsonValue::JsonNumber(n) => n.to_string()
        JsonValue::JsonBool(b) => if b { "true" } else { "false" }
        JsonValue::JsonNull => ""
        JsonValue::JsonObject(_) => "[object]"
        JsonValue::JsonArray(_) => "[array]"
      }
      
      let _ = Map::insert(result, key, string_value)
    }
    
    result
  }
  
  // Map到JSON转换
  let map_to_json = fn(data: Map[String, String], type_hints: Map[String, String]) {
    let mut result = Map::empty()
    
    for (key, value) in data {
      let json_value = match Map::get(type_hints, key) {
        Some("string") => JsonValue::JsonString(value)
        Some("number") => {
          match value.parse_float() {
            Some(num) => JsonValue::JsonNumber(num)
            None => JsonValue::JsonString(value)
          }
        }
        Some("bool") => {
          let lower_val = value.to_lowercase()
          if lower_val == "true" || lower_val == "1" {
            JsonValue::JsonBool(true)
          } else if lower_val == "false" || lower_val == "0" {
            JsonValue::JsonBool(false)
          } else {
            JsonValue::JsonString(value)
          }
        }
        Some("null") => {
          if value == "" || value == "null" {
            JsonValue::JsonNull
          } else {
            JsonValue::JsonString(value)
          }
        }
        _ => JsonValue::JsonString(value)
      }
      
      let _ = Map::insert(result, key, json_value)
    }
    
    result
  }
  
  // CSV到Map转换
  let csv_to_map = fn(csv_data: String, headers: Array[String]) {
    let lines = csv_data.split("\n")
    let mut result = []
    
    for line in lines {
      if line != "" {
        let values = line.split(",")
        if values.length() == headers.length() {
          let mut record = Map::empty()
          
          for i in 0..headers.length() {
            let _ = Map::insert(record, headers[i], values[i])
          }
          
          result = result.push(record)
        }
      }
    }
    
    result
  }
  
  // Map到CSV转换
  let map_to_csv = fn(records: Array[Map[String, String]], headers: Array[String]) {
    let mut csv_lines = [headers.join(",")]
    
    for record in records {
      let values = headers.map(fn(header) {
        match Map::get(record, header) {
          Some(value) => value
          None => ""
        }
      })
      csv_lines = csv_lines.push(values.join(","))
    }
    
    csv_lines.join("\n")
  }
  
  // 测试JSON转换
  let test_json = Map::from_array([
    ("name", JsonValue::JsonString("John")),
    ("age", JsonValue::JsonNumber(25.0)),
    ("active", JsonValue::JsonBool(true)),
    ("notes", JsonValue::JsonNull)
  ])
  
  let json_map = json_to_map(test_json)
  assert_eq(Map::get(json_map, "name"), Some("John"))
  assert_eq(Map::get(json_map, "age"), Some("25"))
  assert_eq(Map::get(json_map, "active"), Some("true"))
  assert_eq(Map::get(json_map, "notes"), Some(""))
  
  let type_hints = Map::from_array([
    ("name", "string"),
    ("age", "number"),
    ("active", "bool"),
    ("notes", "null")
  ])
  
  let back_to_json = map_to_json(json_map, type_hints)
  match Map::get(back_to_json, "name") {
    Some(JsonValue::JsonString(s)) => assert_eq(s, "John")
    _ => assert_true(false)
  }
  
  match Map::get(back_to_json, "age") {
    Some(JsonValue::JsonNumber(n)) => assert_eq(n, 25.0)
    _ => assert_true(false)
  }
  
  match Map::get(back_to_json, "active") {
    Some(JsonValue::JsonBool(b)) => assert_true(b)
    _ => assert_true(false)
  }
  
  match Map::get(back_to_json, "notes") {
    Some(JsonValue::JsonNull) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试CSV转换
  let csv_headers = ["name", "age", "city"]
  let csv_data = "John,25,New York\nJane,30,London\nBob,35,Paris"
  
  let csv_maps = csv_to_map(csv_data, csv_headers)
  assert_eq(csv_maps.length(), 3)
  
  match Map::get(csv_maps[0], "name") {
    Some(value) => assert_eq(value, "John")
    None => assert_true(false)
  }
  
  match Map::get(csv_maps[1], "age") {
    Some(value) => assert_eq(value, "30")
    None => assert_true(false)
  }
  
  match Map::get(csv_maps[2], "city") {
    Some(value) => assert_eq(value, "Paris")
    None => assert_true(false)
  }
  
  // 转换回CSV
  let back_to_csv = map_to_csv(csv_maps, csv_headers)
  assert_true(back_to_csv.contains("John,25,New York"))
  assert_true(back_to_csv.contains("Jane,30,London"))
  assert_true(back_to_csv.contains("Bob,35,Paris"))
}
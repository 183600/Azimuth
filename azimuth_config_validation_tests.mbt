// Azimuth Configuration Management and Data Validation Tests
// This file contains comprehensive test cases for configuration management and data validation

test "configuration file parsing validation" {
  // Simulate configuration file structure
  type ConfigValue {
    StringVal : String
    IntVal : Int
    FloatVal : Float
    BoolVal : Bool
    ArrayVal : Array[ConfigValue]
  }
  
  type ConfigEntry {
    key : String
    value : ConfigValue
    required : Bool
    validated : Bool
  }
  
  type Config {
    entries : Array[ConfigEntry]
    errors : Array[String]
  }
  
  let create_config = fn() -> Config {
    { entries: [], errors: [] }
  }
  
  let add_config_entry = fn(config : Config, key : String, value : ConfigValue, required : Bool) -> Config {
    let entry = { key: key, value: value, required: required, validated: false }
    { entries: config.entries.push(entry), errors: config.errors }
  }
  
  let validate_config = fn(config : Config) -> Config {
    let mut validated_entries = []
    let mut validation_errors = []
    
    for entry in config.entries {
      let mut is_valid = true
      let mut error_message = ""
      
      // Validate based on key and type
      match entry.key {
        "port" => {
          match entry.value {
            IntVal(port) => {
              if port < 1 or port > 65535 {
                is_valid = false
                error_message = "Port must be between 1 and 65535"
              }
            }
            _ => {
              is_valid = false
              error_message = "Port must be an integer"
            }
          }
        }
        "host" => {
          match entry.value {
            StringVal(host) => {
              if host.length() == 0 {
                is_valid = false
                error_message = "Host cannot be empty"
              }
            }
            _ => {
              is_valid = false
              error_message = "Host must be a string"
            }
          }
        }
        "debug_mode" => {
          match entry.value {
            BoolVal(_) => ()  // Always valid
            _ => {
              is_valid = false
              error_message = "Debug mode must be a boolean"
            }
          }
        }
        "timeout" => {
          match entry.value {
            FloatVal(timeout) => {
              if timeout <= 0.0 {
                is_valid = false
                error_message = "Timeout must be positive"
              }
            }
            _ => {
              is_valid = false
              error_message = "Timeout must be a float"
            }
          }
        }
        _ => {
          // Unknown key, but not necessarily an error
        }
      }
      
      // Check required fields
      if entry.required and not(is_valid) {
        validation_errors = validation_errors.push("Required field '" + entry.key + "': " + error_message)
      }
      
      validated_entries = validated_entries.push({ 
        key: entry.key, 
        value: entry.value, 
        required: entry.required, 
        validated: is_valid 
      })
    }
    
    { entries: validated_entries, errors: validation_errors }
  }
  
  // Test configuration validation
  let config = create_config()
  
  // Add valid configuration entries
  let config1 = add_config_entry(config, "port", IntVal(8080), true)
  let config2 = add_config_entry(config1, "host", StringVal("localhost"), true)
  let config3 = add_config_entry(config2, "debug_mode", BoolVal(true), false)
  let config4 = add_config_entry(config3, "timeout", FloatVal(30.0), true)
  
  // Validate configuration
  let validated_config = validate_config(config4)
  
  // Check validation results
  assert_eq(validated_config.errors.length(), 0)
  
  // Test invalid configuration
  let invalid_config = create_config()
  let invalid_config1 = add_config_entry(invalid_config, "port", IntVal(70000), true)  // Invalid port
  let invalid_config2 = add_config_entry(invalid_config1, "host", StringVal(""), true)  // Empty host
  let invalid_config3 = add_config_entry(invalid_config2, "debug_mode", StringVal("true"), false)  // Wrong type
  
  let invalid_validated_config = validate_config(invalid_config3)
  
  // Should have validation errors
  assert_true(invalid_validated_config.errors.length() > 0)
}

test "data type validation and conversion" {
  // Test string validation patterns
  let validate_email = fn(email : String) -> Result[String, String] {
    let has_at = email.contains("@")
    let has_dot = email.contains(".")
    let reasonable_length = email.length() >= 5 and email.length() <= 50
    let no_spaces = not(email.contains(" "))
    
    if has_at and has_dot and reasonable_length and no_spaces {
      Ok(email)
    } else {
      Err("Invalid email format")
    }
  }
  
  let validate_phone = fn(phone : String) -> Result[String, String] {
    let mut valid_chars = true
    let mut digit_count = 0
    
    for char in phone.to_char_array() {
      if char >= '0' and char <= '9' {
        digit_count = digit_count + 1
      } else if char == '-' or char == ' ' or char == '(' or char == ')' {
        // Allow formatting characters
      } else {
        valid_chars = false
        break
      }
    }
    
    if valid_chars and digit_count >= 10 {
      Ok(phone)
    } else {
      Err("Invalid phone number format")
    }
  }
  
  let validate_url = fn(url : String) -> Result[String, String] {
    let has_protocol = url.starts_with("http://") or url.starts_with("https://")
    let has_domain = url.contains(".") and not(url.ends_with("."))
    let reasonable_length = url.length() >= 10 and url.length() <= 200
    
    if has_protocol and has_domain and reasonable_length {
      Ok(url)
    } else {
      Err("Invalid URL format")
    }
  }
  
  // Test email validation
  let valid_email = validate_email("user@example.com")
  match valid_email {
    Ok(email) => assert_eq(email, "user@example.com")
    Err(_) => assert_true(false)
  }
  
  let invalid_email = validate_email("invalid-email")
  match invalid_email {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "Invalid email format")
  }
  
  // Test phone validation
  let valid_phone = validate_phone("123-456-7890")
  match valid_phone {
    Ok(phone) => assert_eq(phone, "123-456-7890")
    Err(_) => assert_true(false)
  }
  
  let invalid_phone = validate_phone("12-34")
  match invalid_phone {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "Invalid phone number format")
  }
  
  // Test URL validation
  let valid_url = validate_url("https://www.example.com")
  match valid_url {
    Ok(url) => assert_eq(url, "https://www.example.com")
    Err(_) => assert_true(false)
  }
  
  let invalid_url = validate_url("www.example.com")
  match invalid_url {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "Invalid URL format")
  }
}

test "range and constraint validation" {
  // Test numeric range validation
  let validate_int_range = fn(value : Int, min : Int, max : Int) -> Result[Int, String] {
    if value >= min and value <= max {
      Ok(value)
    } else {
      Err("Value " + value.to_string() + " is not in range [" + min.to_string() + ", " + max.to_string() + "]")
    }
  }
  
  let validate_float_range = fn(value : Float, min : Float, max : Float) -> Result[Float, String] {
    if value >= min and value <= max {
      Ok(value)
    } else {
      Err("Value is not in valid range")
    }
  }
  
  let validate_string_length = fn(value : String, min_length : Int, max_length : Int) -> Result[String, String] {
    let length = value.length()
    if length >= min_length and length <= max_length {
      Ok(value)
    } else {
      Err("String length must be between " + min_length.to_string() + " and " + max_length.to_string())
    }
  }
  
  let validate_array_size = fn(arr : Array[String], min_size : Int, max_size : Int) -> Result[Array[String], String] {
    let size = arr.length()
    if size >= min_size and size <= max_size {
      Ok(arr)
    } else {
      Err("Array size must be between " + min_size.to_string() + " and " + max_size.to_string())
    }
  }
  
  // Test integer range validation
  let valid_int = validate_int_range(50, 0, 100)
  match valid_int {
    Ok(value) => assert_eq(value, 50)
    Err(_) => assert_true(false)
  }
  
  let invalid_int = validate_int_range(150, 0, 100)
  match invalid_int {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "Value 150 is not in range [0, 100]")
  }
  
  // Test float range validation
  let valid_float = validate_float_range(0.5, 0.0, 1.0)
  match valid_float {
    Ok(value) => assert_eq(value, 0.5)
    Err(_) => assert_true(false)
  }
  
  let invalid_float = validate_float_range(1.5, 0.0, 1.0)
  match invalid_float {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "Value is not in valid range")
  }
  
  // Test string length validation
  let valid_string = validate_string_length("hello", 3, 10)
  match valid_string {
    Ok(value) => assert_eq(value, "hello")
    Err(_) => assert_true(false)
  }
  
  let invalid_string = validate_string_length("this is too long", 3, 10)
  match invalid_string {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "String length must be between 3 and 10")
  }
  
  // Test array size validation
  let valid_array = validate_array_size(["a", "b", "c"], 2, 5)
  match valid_array {
    Ok(arr) => assert_eq(arr.length(), 3)
    Err(_) => assert_true(false)
  }
  
  let invalid_array = validate_array_size(["a"], 2, 5)
  match invalid_array {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "Array size must be between 2 and 5")
  }
}

test "business rule validation" {
  // Test business rule validation
  type Product {
    id : String
    name : String
    price : Float
    stock : Int
    category : String
  }
  
  type Order {
    id : String
    customer_id : String
    items : Array[Product]
    total_amount : Float
    status : String
  }
  
  let validate_product = fn(product : Product) -> Result[Product, String] {
    let mut errors = []
    
    // Business rule: Product ID must start with "PRD"
    if not(product.id.starts_with("PRD")) {
      errors = errors.push("Product ID must start with 'PRD'")
    }
    
    // Business rule: Product name must be at least 3 characters
    if product.name.length() < 3 {
      errors = errors.push("Product name must be at least 3 characters")
    }
    
    // Business rule: Price must be positive
    if product.price <= 0.0 {
      errors = errors.push("Product price must be positive")
    }
    
    // Business rule: Stock cannot be negative
    if product.stock < 0 {
      errors = errors.push("Product stock cannot be negative")
    }
    
    // Business rule: Category must be one of the allowed categories
    let allowed_categories = ["Electronics", "Clothing", "Books", "Home"]
    let mut valid_category = false
    for category in allowed_categories {
      if product.category == category {
        valid_category = true
        break
      }
    }
    
    if not(valid_category) {
      errors = errors.push("Invalid product category")
    }
    
    if errors.length() > 0 {
      Err(errors[0])  // Return first error
    } else {
      Ok(product)
    }
  }
  
  let validate_order = fn(order : Order, products : Array[Product]) -> Result[Order, String] {
    let mut errors = []
    
    // Business rule: Order ID must start with "ORD"
    if not(order.id.starts_with("ORD")) {
      errors = errors.push("Order ID must start with 'ORD'")
    }
    
    // Business rule: Customer ID must start with "CUS"
    if not(order.customer_id.starts_with("CUS")) {
      errors = errors.push("Customer ID must start with 'CUS'")
    }
    
    // Business rule: Order must have at least one item
    if order.items.length() == 0 {
      errors = errors.push("Order must have at least one item")
    }
    
    // Business rule: Total amount must match sum of item prices
    let mut calculated_total = 0.0
    for item in order.items {
      calculated_total = calculated_total + item.price
    }
    
    if calculated_total != order.total_amount {
      errors = errors.push("Order total amount doesn't match sum of item prices")
    }
    
    // Business rule: Order status must be valid
    let valid_statuses = ["Pending", "Processing", "Shipped", "Delivered", "Cancelled"]
    let mut valid_status = false
    for status in valid_statuses {
      if order.status == status {
        valid_status = true
        break
      }
    }
    
    if not(valid_status) {
      errors = errors.push("Invalid order status")
    }
    
    if errors.length() > 0 {
      Err(errors[0])  // Return first error
    } else {
      Ok(order)
    }
  }
  
  // Test product validation
  let valid_product = {
    id: "PRD001",
    name: "Test Product",
    price: 19.99,
    stock: 100,
    category: "Electronics"
  }
  
  let product_validation = validate_product(valid_product)
  match product_validation {
    Ok(product) => assert_eq(product.id, "PRD001")
    Err(_) => assert_true(false)
  }
  
  let invalid_product = {
    id: "INVALID001",
    name: "X",
    price: -10.0,
    stock: -5,
    category: "Invalid"
  }
  
  let invalid_product_validation = validate_product(invalid_product)
  match invalid_product_validation {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "Product ID must start with 'PRD'")
  }
  
  // Test order validation
  let valid_order = {
    id: "ORD001",
    customer_id: "CUS001",
    items: [valid_product],
    total_amount: 19.99,
    status: "Pending"
  }
  
  let order_validation = validate_order(valid_order, [valid_product])
  match order_validation {
    Ok(order) => assert_eq(order.id, "ORD001")
    Err(_) => assert_true(false)
  }
  
  let invalid_order = {
    id: "INVALID001",
    customer_id: "INVALID001",
    items: [],
    total_amount: 0.0,
    status: "Invalid"
  }
  
  let invalid_order_validation = validate_order(invalid_order, [])
  match invalid_order_validation {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "Order ID must start with 'ORD'")
  }
}

test "configuration schema validation" {
  // Test schema-based validation
  type SchemaType {
    String : String
    Int : Int
    Float : Float
    Bool : Bool
    Array : SchemaType
    Object : Array[(String, SchemaType)]
  }
  
  type SchemaField {
    name : String
    type : SchemaType
    required : Bool
    default_value : Option[SchemaType]
    validation_rules : Array[String]
  }
  
  type Schema {
    name : String
    fields : Array[SchemaField]
  }
  
  let validate_against_schema = fn(data : Array[(String, SchemaType)], schema : Schema) -> Result[Array[(String, SchemaType)], String] {
    let mut errors = []
    
    // Check required fields
    for field in schema.fields {
      if field.required {
        let mut field_found = false
        for data_field in data {
          if data_field.0 == field.name {
            field_found = true
            
            // Check type compatibility
            let type_match = match (field.type, data_field.1) {
              (String, String) => true
              (Int, Int) => true
              (Float, Float) => true
              (Bool, Bool) => true
              _ => false
            }
            
            if not(type_match) {
              errors = errors.push("Field '" + field.name + "' has incorrect type")
            }
            
            break
          }
        }
        
        if not(field_found) {
          match field.default_value {
            Some(_) => ()  // Has default value
            None => errors = errors.push("Required field '" + field.name + "' is missing")
          }
        }
      }
    }
    
    if errors.length() > 0 {
      Err(errors[0])
    } else {
      Ok(data)
    }
  }
  
  // Define a simple schema
  let user_schema = {
    name: "User",
    fields: [
      { name: "username", type: String, required: true, default_value: None, validation_rules: [] },
      { name: "age", type: Int, required: true, default_value: None, validation_rules: [] },
      { name: "email", type: String, required: false, default_value: Some(String), validation_rules: [] },
      { name: "active", type: Bool, required: false, default_value: Some(Bool), validation_rules: [] }
    ]
  }
  
  // Test valid data
  let valid_user_data = [
    ("username", String("john_doe")),
    ("age", Int(25)),
    ("email", String("john@example.com")),
    ("active", Bool(true))
  ]
  
  let valid_validation = validate_against_schema(valid_user_data, user_schema)
  match valid_validation {
    Ok(data) => assert_eq(data.length(), 4)
    Err(_) => assert_true(false)
  }
  
  // Test missing required field
  let invalid_user_data = [
    ("username", String("john_doe")),
    ("email", String("john@example.com"))
  ]
  
  let invalid_validation = validate_against_schema(invalid_user_data, user_schema)
  match invalid_validation {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "Required field 'age' is missing")
  }
  
  // Test incorrect type
  let type_mismatch_data = [
    ("username", String("john_doe")),
    ("age", String("25")),  // Should be Int
    ("email", String("john@example.com"))
  ]
  
  let type_mismatch_validation = validate_against_schema(type_mismatch_data, user_schema)
  match type_mismatch_validation {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "Field 'age' has incorrect type")
  }
}

test "configuration dependency validation" {
  // Test configuration dependency validation
  type ConfigDependency {
    field : String
    depends_on : String
    condition : String  // "equals", "not_equals", "greater_than", etc.
    value : SchemaType
  }
  
  type ConfigWithDependencies {
    entries : Array[(String, SchemaType)]
    dependencies : Array[ConfigDependency]
  }
  
  let validate_dependencies = fn(config : ConfigWithDependencies) -> Result[ConfigWithDependencies, String] {
    let mut errors = []
    
    for dependency in config.dependencies {
      let mut field_value = None
      let mut dependency_value = None
      
      // Find field value
      for entry in config.entries {
        if entry.0 == dependency.field {
          field_value = Some(entry.1)
        } else if entry.0 == dependency.depends_on {
          dependency_value = Some(entry.1)
        }
      }
      
      match (field_value, dependency_value) {
        (Some(field), Some(dep)) => {
          let condition_met = match dependency.condition {
            "equals" => field == dep
            "not_equals" => field != dep
            "greater_than" => {
              match (field, dep) {
                (Int(f), Int(d)) => f > d
                (Float(f), Float(d)) => f > d
                _ => false
              }
            }
            _ => false
          }
          
          if not(condition_met) {
            errors = errors.push("Dependency condition not met for field '" + dependency.field + "'")
          }
        }
        _ => {
          errors = errors.push("Missing dependency fields for '" + dependency.field + "'")
        }
      }
    }
    
    if errors.length() > 0 {
      Err(errors[0])
    } else {
      Ok(config)
    }
  }
  
  // Test configuration with dependencies
  let config_with_deps = {
    entries: [
      ("mode", String("production")),
      ("port", Int(80)),
      ("debug", Bool(false))
    ],
    dependencies: [
      { field: "port", depends_on: "mode", condition: "equals", value: String("production") },
      { field: "debug", depends_on: "mode", condition: "equals", value: String("development") }
    ]
  }
  
  // This should fail because debug is false but mode is production (not development)
  let dependency_validation = validate_dependencies(config_with_deps)
  match dependency_validation {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "Dependency condition not met for field 'debug'")
  }
  
  // Test valid configuration
  let valid_config_with_deps = {
    entries: [
      ("mode", String("production")),
      ("port", Int(80)),
      ("debug", Bool(true))
    ],
    dependencies: [
      { field: "port", depends_on: "mode", condition: "equals", value: String("production") }
    ]
  }
  
  let valid_dependency_validation = validate_dependencies(valid_config_with_deps)
  match valid_dependency_validation {
    Ok(config) => assert_eq(config.entries.length(), 3)
    Err(_) => assert_true(false)
  }
}
// Azimuth Data Validation and Conversion Test Suite
// This file contains test cases for data validation and conversion functionality

// Test 1: Basic Type Validation
test "basic type validation" {
  // Test string validation
  let string_validator = Validator::string()
  assert_true(Validator::validate(string_validator, "test"))
  assert_false(Validator::validate(string_validator, 123))
  assert_false(Validator::validate(string_validator, true))
  
  // Test numeric validation
  let int_validator = Validator::int()
  assert_true(Validator::validate(int_validator, 42))
  assert_false(Validator::validate(int_validator, "42"))
  assert_false(Validator::validate(int_validator, 3.14))
  
  let float_validator = Validator::float()
  assert_true(Validator::validate(float_validator, 3.14))
  assert_true(Validator::validate(float_validator, 42)) // Int should be valid for float
  assert_false(Validator::validate(float_validator, "3.14"))
  
  // Test boolean validation
  let bool_validator = Validator::bool()
  assert_true(Validator::validate(bool_validator, true))
  assert_false(Validator::validate(bool_validator, "true"))
  assert_false(Validator::validate(bool_validator, 1))
}

// Test 2: String Validation Rules
test "string validation rules" {
  // Test length validation
  let length_validator = Validator::string().min_length(3).max_length(10)
  assert_true(Validator::validate(length_validator, "test"))
  assert_false(Validator::validate(length_validator, "ab")) // Too short
  assert_false(Validator::validate(length_validator, "very_long_string")) // Too long
  
  // Test pattern validation
  let email_validator = Validator::string().pattern(r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$")
  assert_true(Validator::validate(email_validator, "test@example.com"))
  assert_false(Validator::validate(email_validator, "invalid-email"))
  assert_false(Validator::validate(email_validator, "test@"))
  
  // Test whitelist validation
  let whitelist_validator = Validator::string().whitelist(["admin", "user", "guest"])
  assert_true(Validator::validate(whitelist_validator, "admin"))
  assert_false(Validator::validate(whitelist_validator, "superuser"))
  
  // Test blacklist validation
  let blacklist_validator = Validator::string().blacklist(["password", "123456", "admin"])
  assert_true(Validator::validate(blacklist_validator, "valid_password"))
  assert_false(Validator::validate(blacklist_validator, "password"))
}

// Test 3: Numeric Validation Rules
test "numeric validation rules" {
  // Test range validation
  let range_validator = Validator::int().range(1, 100)
  assert_true(Validator::validate(range_validator, 50))
  assert_false(Validator::validate(range_validator, 0)) // Below range
  assert_false(Validator::validate(range_validator, 101)) // Above range
  
  // Test positive/negative validation
  let positive_validator = Validator::float().positive()
  assert_true(Validator::validate(positive_validator, 3.14))
  assert_false(Validator::validate(positive_validator, -1.0))
  assert_false(Validator::validate(positive_validator, 0.0))
  
  let non_negative_validator = Validator::int().non_negative()
  assert_true(Validator::validate(non_negative_validator, 0))
  assert_true(Validator::validate(non_negative_validator, 42))
  assert_false(Validator::validate(non_negative_validator, -1))
  
  // Test precision validation
  let precision_validator = Validator::float().precision(2)
  assert_true(Validator::validate(precision_validator, 3.14))
  assert_false(Validator::validate(precision_validator, 3.141)) // Too many decimal places
}

// Test 4: Array and Collection Validation
test "array and collection validation" {
  // Test array validation
  let array_validator = Validator::array(Validator::string()).min_items(1).max_items(5)
  assert_true(Validator::validate(array_validator, ["a", "b", "c"]))
  assert_false(Validator::validate(array_validator, [])) // Too few items
  assert_false(Validator::validate(array_validator, ["a", "b", "c", "d", "e", "f"])) // Too many items
  assert_false(Validator::validate(array_validator, [1, 2, 3])) // Wrong item type
  
  // Test unique items validation
  let unique_validator = Validator::array(Validator::int()).unique()
  assert_true(Validator::validate(unique_validator, [1, 2, 3, 4]))
  assert_false(Validator::validate(unique_validator, [1, 2, 2, 3])) // Duplicate items
  
  // Test nested validation
  let nested_validator = Validator::array(
    Validator::object([
      ("name", Validator::string().required()),
      ("age", Validator::int().range(0, 150))
    ])
  )
  
  let valid_nested = [
    [("name", "Alice"), ("age", 25)],
    [("name", "Bob"), ("age", 30)]
  ]
  assert_true(Validator::validate(nested_validator, valid_nested))
  
  let invalid_nested = [
    [("name", "Charlie")], // Missing age
    [("name", "David"), ("age", 200)] // Age out of range
  ]
  assert_false(Validator::validate(nested_validator, invalid_nested))
}

// Test 5: Object Validation
test "object validation" {
  // Test object schema validation
  let user_validator = Validator::object([
    ("username", Validator::string().min_length(3).required()),
    ("email", Validator::string().pattern(r"^[^@]+@[^@]+\.[^@]+$").required()),
    ("age", Validator::int().range(13, 120)),
    ("is_active", Validator::bool().default(true))
  ])
  
  let valid_user = [
    ("username", "john_doe"),
    ("email", "john@example.com"),
    ("age", 25)
  ]
  assert_true(Validator::validate(user_validator, valid_user))
  
  // Test missing required field
  let missing_required = [
    ("email", "jane@example.com"),
    ("age", 30)
    // Missing username
  ]
  assert_false(Validator::validate(user_validator, missing_required))
  
  // Test invalid field values
  let invalid_values = [
    ("username", "ab"), // Too short
    ("email", "invalid-email"), // Invalid format
    ("age", 10) // Below range
  ]
  assert_false(Validator::validate(user_validator, invalid_values))
  
  // Test additional properties
  let strict_validator = user_validator.strict()
  let with_extra_props = [
    ("username", "test_user"),
    ("email", "test@example.com"),
    ("age", 25),
    ("extra_field", "should_not_be_allowed")
  ]
  assert_false(Validator::validate(strict_validator, with_extra_props))
  
  let lenient_validator = user_validator.lenient()
  assert_true(Validator::validate(lenient_validator, with_extra_props))
}

// Test 6: Custom Validation Rules
test "custom validation rules" {
  // Test custom function validator
  let even_validator = Validator::int().custom(fn(value) {
    match value {
      Int(n) => n % 2 == 0
      _ => false
    }
  }, "Value must be even")
  
  assert_true(Validator::validate(even_validator, 4))
  assert_false(Validator::validate(even_validator, 5))
  
  // Test conditional validation
  let conditional_validator = Validator::object([
    ("type", Validator::string().required()),
    ("value", Validator::string().when(
      fn(obj) { match obj.get("type") { Some(StringValue("email")) => true, _ => false } },
      Validator::string().pattern(r"^[^@]+@[^@]+\.[^@]+$")
    ).when(
      fn(obj) { match obj.get("type") { Some(StringValue("phone")) => true, _ => false } },
      Validator::string().pattern(r"^\+?[\d\s\-\(\)]+$")
    ))
  ])
  
  let email_obj = [
    ("type", "email"),
    ("value", "test@example.com")
  ]
  assert_true(Validator::validate(conditional_validator, email_obj))
  
  let phone_obj = [
    ("type", "phone"),
    ("value", "+1 (555) 123-4567")
  ]
  assert_true(Validator::validate(conditional_validator, phone_obj))
  
  let invalid_email = [
    ("type", "email"),
    ("value", "not-an-email")
  ]
  assert_false(Validator::validate(conditional_validator, invalid_email))
}

// Test 7: Data Type Conversion
test "data type conversion" {
  // Test string to numeric conversion
  match Converter::to_int("42") {
    Ok(42) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  match Converter::to_int("not_a_number") {
    Ok(_) => assert_true(false)
    Err(_) => assert_true(true)
  }
  
  match Converter::to_float("3.14") {
    Ok(3.14) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // Test numeric to string conversion
  assert_eq(Converter::to_string(42), "42")
  assert_eq(Converter::to_string(3.14), "3.14")
  assert_eq(Converter::to_string(true), "true")
  
  // Test boolean conversion
  match Converter::to_bool("true") {
    Ok(true) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  match Converter::to_bool("false") {
    Ok(false) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  match Converter::to_bool("1") {
    Ok(true) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  match Converter::to_bool("0") {
    Ok(false) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // Test array conversion
  match Converter::to_array("[1, 2, 3]") {
    Ok(arr) => assert_eq(arr, [1, 2, 3])
    Err(_) => assert_true(false)
  }
  
  match Converter::to_array("a,b,c") {
    Ok(arr) => assert_eq(arr, ["a", "b", "c"])
    Err(_) => assert_true(false)
  }
}

// Test 8: Data Sanitization
test "data sanitization" {
  // Test HTML sanitization
  let html_input = "<script>alert('xss')</script><p>Safe content</p>"
  let sanitized_html = Sanitizer::html(html_input)
  assert_false(sanitized_html.contains("<script>"))
  assert_true(sanitized_html.contains("<p>Safe content</p>"))
  
  // Test SQL injection sanitization
  let sql_input = "'; DROP TABLE users; --"
  let sanitized_sql = Sanitizer::sql(sql_input)
  assert_false(sanitized_sql.contains("DROP TABLE"))
  
  // Test string sanitization
  let dirty_string = "Hello\u0000World\u001fTest"
  let clean_string = Sanitizer::string(dirty_string)
  assert_false(clean_string.contains("\u0000"))
  assert_false(clean_string.contains("\u001f"))
  
  // Test phone number normalization
  let phone1 = "+1 (555) 123-4567"
  let phone2 = "555.123.4567"
  let phone3 = "5551234567"
  
  assert_eq(Sanitizer::phone(phone1), "15551234567")
  assert_eq(Sanitizer::phone(phone2), "5551234567")
  assert_eq(Sanitizer::phone(phone3), "5551234567")
  
  // Test email normalization
  let email1 = "Test.Example@DOMAIN.COM"
  let email2 = "test@example+tag.com"
  
  assert_eq(Sanitizer::email(email1), "test.example@domain.com")
  assert_eq(Sanitizer::email(email2), "test@example.com") // Removes tag
}

// Test 9: Data Transformation
test "data transformation" {
  // Test case transformation
  assert_eq(Transformer::to_upper_case("hello"), "HELLO")
  assert_eq(Transformer::to_lower_case("WORLD"), "world")
  assert_eq(Transformer::to_title_case("hello world"), "Hello World")
  assert_eq(Transformer::to_camel_case("hello_world_test"), "helloWorldTest")
  assert_eq(Transformer::to_snake_case("helloWorldTest"), "hello_world_test")
  
  // Test whitespace transformation
  assert_eq(Transformer::trim("  hello world  "), "hello world")
  assert_eq(Transformer::collapse_whitespace("hello    world"), "hello world")
  assert_eq(Transformer::remove_whitespace("helloworld"), "helloworld")
  
  // Test data masking
  assert_eq(Transformer::mask_email("test@example.com"), "t***@example.com")
  assert_eq(Transformer::mask_phone("1234567890"), "123***7890")
  assert_eq(Transformer::mask_credit_card("1234567890123456"), "1234********3456")
  
  // Test data formatting
  assert_eq(Transformer::format_currency(1234.56, "USD"), "$1,234.56")
  assert_eq(Transformer::format_percentage(0.1234, 2), "12.34%")
  assert_eq(Transformer::format_number(1234567.89, "en-US"), "1,234,567.89")
  
  // Test date/time transformation
  let timestamp = 1640995200000L // 2022-01-01 00:00:00 UTC
  assert_eq(Transformer::format_date(timestamp, "YYYY-MM-DD"), "2022-01-01")
  assert_eq(Transformer::format_time(timestamp, "HH:mm:ss"), "00:00:00")
  assert_eq(Transformer::format_datetime(timestamp, "YYYY-MM-DD HH:mm:ss"), "2022-01-01 00:00:00")
}

// Test 10: Validation and Conversion Pipeline
test "validation and conversion pipeline" {
  // Create a pipeline for user registration data
  let registration_pipeline = Pipeline::new()
    .add_step(PipelineStep::trim_strings())
    .add_step(PipelineStep::normalize_emails())
    .add_step(PipelineStep::validate(
      Validator::object([
        ("username", Validator::string().min_length(3).max_length(20).required()),
        ("email", Validator::string().pattern(r"^[^@]+@[^@]+\.[^@]+$").required()),
        ("age", Validator::int().range(13, 120).required()),
        ("password", Validator::string().min_length(8).required())
      ])
    ))
    .add_step(PipelineStep::sanitize_html())
    .add_step(PipelineStep::hash_passwords())
  
  // Test valid input
  let valid_input = [
    ("username", "  john_doe  "),
    ("email", "  JOHN.DOE@EXAMPLE.COM  "),
    ("age", "25"),
    ("password", "securepassword123")
  ]
  
  match Pipeline::process(registration_pipeline, valid_input) {
    Ok(result) => {
      assert_eq(result.get("username"), Some(StringValue("john_doe")))
      assert_eq(result.get("email"), Some(StringValue("john.doe@example.com")))
      assert_eq(result.get("age"), Some(IntValue(25)))
      // Password should be hashed
      match result.get("password") {
        Some(StringValue(hashed)) => assert_neq(hashed, "securepassword123")
        _ => assert_true(false)
      }
    }
    Err(errors) => assert_true(false, "Valid input should not fail: " + errors.join(", "))
  }
  
  // Test invalid input
  let invalid_input = [
    ("username", "ab"), // Too short
    ("email", "invalid-email"), // Invalid format
    ("age", "10"), // Below range
    ("password", "short") // Too short
  ]
  
  match Pipeline::process(registration_pipeline, invalid_input) {
    Ok(_) => assert_true(false, "Invalid input should fail")
    Err(errors) => {
      assert_true(errors.length() >= 4) // Should have multiple validation errors
    }
  }
  
  // Test conditional pipeline
  let conditional_pipeline = Pipeline::new()
    .add_step(PipelineStep::validate(
      Validator::object([
        ("user_type", Validator::string().whitelist(["admin", "user"]).required()),
        ("permissions", Validator::array(Validator::string()))
      ])
    ))
    .add_step(PipelineStep::when(
      fn(data) { match data.get("user_type") { Some(StringValue("admin")) => true, _ => false } },
      PipelineStep::validate(
        Validator::object([
          ("permissions", Validator::array(Validator::string()).min_items(1).required())
        ])
      )
    ))
  
  // Test admin user with permissions
  let admin_with_perms = [
    ("user_type", "admin"),
    ("permissions", ["read", "write", "delete"])
  ]
  match Pipeline::process(conditional_pipeline, admin_with_perms) {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // Test admin user without permissions (should fail)
  let admin_no_perms = [
    ("user_type", "admin"),
    ("permissions", [])
  ]
  match Pipeline::process(conditional_pipeline, admin_no_perms) {
    Ok(_) => assert_true(false)
    Err(_) => assert_true(true)
  }
  
  // Test regular user (permissions not required)
  let regular_user = [
    ("user_type", "user"),
    ("permissions", [])
  ]
  match Pipeline::process(conditional_pipeline, regular_user) {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
}
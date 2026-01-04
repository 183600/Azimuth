// Azimuth Telemetry System - Data Validation and Integrity Tests
// This file contains test cases for data validation and integrity functionality

// Test 1: Basic Data Type Validation
test "basic data type validation" {
  // Create a data validator
  let validator = DataValidator::new()
  
  // Test string validation
  let string_rule = ValidationRule::String(StringConstraints {
    min_length: Some(5),
    max_length: Some(20),
    pattern: Some("^[a-zA-Z0-9]+$"),
    required: true
  })
  
  validator.add_rule("username", string_rule)
  
  // Valid string
  let valid_string_result = validator.validate_string("username", "testuser123")
  assert_true(ValidationResult::is_valid(valid_string_result))
  
  // Invalid string (too short)
  let short_string_result = validator.validate_string("username", "test")
  assert_false(ValidationResult::is_valid(short_string_result))
  
  // Invalid string (too long)
  let long_string_result = validator.validate_string("username", "this_is_a_very_long_username")
  assert_false(ValidationResult::is_valid(long_string_result))
  
  // Invalid string (contains special characters)
  let special_char_result = validator.validate_string("username", "test@user")
  assert_false(ValidationResult::is_valid(special_char_result))
  
  // Test integer validation
  let int_rule = ValidationRule::Integer(IntegerConstraints {
    min_value: Some(1),
    max_value: Some(100),
    required: true
  })
  
  validator.add_rule("age", int_rule)
  
  // Valid integer
  let valid_int_result = validator.validate_int("age", 25)
  assert_true(ValidationResult::is_valid(valid_int_result))
  
  // Invalid integer (too small)
  let small_int_result = validator.validate_int("age", 0)
  assert_false(ValidationResult::is_valid(small_int_result))
  
  // Invalid integer (too large)
  let large_int_result = validator.validate_int("age", 150)
  assert_false(ValidationResult::is_valid(large_int_result))
  
  // Test float validation
  let float_rule = ValidationRule::Float(FloatConstraints {
    min_value: Some(0.0),
    max_value: Some(1.0),
    precision: Some(2),
    required: true
  })
  
  validator.add_rule("percentage", float_rule)
  
  // Valid float
  let valid_float_result = validator.validate_float("percentage", 0.75)
  assert_true(ValidationResult::is_valid(valid_float_result))
  
  // Invalid float (too small)
  let small_float_result = validator.validate_float("percentage", -0.1)
  assert_false(ValidationResult::is_valid(small_float_result))
  
  // Invalid float (too large)
  let large_float_result = validator.validate_float("percentage", 1.5)
  assert_false(ValidationResult::is_valid(large_float_result))
  
  // Test boolean validation
  let bool_rule = ValidationRule::Boolean(BooleanConstraints {
    required: true
  })
  
  validator.add_rule("enabled", bool_rule)
  
  // Valid boolean values
  let true_result = validator.validate_bool("enabled", true)
  assert_true(ValidationResult::is_valid(true_result))
  
  let false_result = validator.validate_bool("enabled", false)
  assert_true(ValidationResult::is_valid(false_result))
}

// Test 2: Complex Data Structure Validation
test "complex data structure validation" {
  // Create a data validator
  let validator = DataValidator::new()
  
  // Define nested object validation rules
  validator.add_rule("user.name", ValidationRule::String(StringConstraints {
    min_length: Some(2),
    max_length: Some(50),
    pattern: None,
    required: true
  }))
  
  validator.add_rule("user.email", ValidationRule::String(StringConstraints {
    min_length: Some(5),
    max_length: Some(100),
    pattern: Some("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"),
    required: true
  }))
  
  validator.add_rule("user.age", ValidationRule::Integer(IntegerConstraints {
    min_value: Some(18),
    max_value: Some(120),
    required: true
  }))
  
  validator.add_rule("user.address.street", ValidationRule::String(StringConstraints {
    min_length: Some(5),
    max_length: Some(100),
    pattern: None,
    required: true
  }))
  
  validator.add_rule("user.address.city", ValidationRule::String(StringConstraints {
    min_length: Some(2),
    max_length: Some(50),
    pattern: None,
    required: true
  }))
  
  validator.add_rule("user.address.zip", ValidationRule::String(StringConstraints {
    min_length: Some(5),
    max_length: Some(10),
    pattern: Some("^\\d{5}(-\\d{4})?$"),
    required: true
  }))
  
  validator.add_rule("user.hobbies", ValidationRule::Array(ArrayConstraints {
    min_items: Some(0),
    max_items: Some(10),
    item_type: ValidationRule::String(StringConstraints {
      min_length: Some(2),
      max_length: Some(30),
      pattern: None,
      required: true
    }),
    required: false
  }))
  
  // Create valid user data
  let valid_user = UserData::new()
  UserData::set_string(valid_user, "user.name", "John Doe")
  UserData::set_string(valid_user, "user.email", "john.doe@example.com")
  UserData::set_int(valid_user, "user.age", 30)
  UserData::set_string(valid_user, "user.address.street", "123 Main St")
  UserData::set_string(valid_user, "user.address.city", "New York")
  UserData::set_string(valid_user, "user.address.zip", "10001")
  UserData::set_string_array(valid_user, "user.hobbies", ["reading", "swimming", "coding"])
  
  // Validate valid user data
  let valid_result = validator.validate_object(valid_user)
  assert_true(ValidationResult::is_valid(valid_result))
  
  // Create invalid user data
  let invalid_user = UserData::new()
  UserData::set_string(invalid_user, "user.name", "J")  // Too short
  UserData::set_string(invalid_user, "user.email", "invalid-email")  // Invalid format
  UserData::set_int(invalid_user, "user.age", 15)  // Too young
  UserData::set_string(invalid_user, "user.address.street", "123")  // Too short
  UserData::set_string(invalid_user, "user.address.city", "")  // Empty
  UserData::set_string(invalid_user, "user.address.zip", "123")  // Invalid format
  UserData::set_string_array(invalid_user, "user.hobbies", ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"])  // Too many items
  
  // Validate invalid user data
  let invalid_result = validator.validate_object(invalid_user)
  assert_false(ValidationResult::is_valid(invalid_result))
  
  // Check specific validation errors
  let errors = ValidationResult::get_errors(invalid_result)
  assert_true(errors.length() >= 7)  // At least 7 validation errors
  
  let name_error = ValidationResult::get_error_for_field(invalid_result, "user.name")
  match name_error {
    Some(error) => assert_true(ValidationError::get_message(error).contains("minimum length"))
    None => assert_true(false)
  }
  
  let email_error = ValidationResult::get_error_for_field(invalid_result, "user.email")
  match email_error {
    Some(error) => assert_true(ValidationError::get_message(error).contains("pattern"))
    None => assert_true(false)
  }
  
  let age_error = ValidationResult::get_error_for_field(invalid_result, "user.age")
  match age_error {
    Some(error) => assert_true(ValidationError::get_message(error).contains("minimum value"))
    None => assert_true(false)
  }
}

// Test 3: Data Integrity Checks
test "data integrity checks" {
  // Create an integrity checker
  let integrity_checker = IntegrityChecker::new()
  
  // Test checksum calculation and verification
  let data = "This is test data for integrity checking"
  let checksum = IntegrityChecker::calculate_checksum(integrity_checker, data)
  
  // Verify integrity with correct checksum
  let verify_result = IntegrityChecker::verify_checksum(integrity_checker, data, checksum)
  assert_true(IntegrityResult::is_valid(verify_result))
  
  // Verify integrity with incorrect checksum
  let incorrect_checksum = "invalid_checksum"
  let incorrect_result = IntegrityChecker::verify_checksum(integrity_checker, data, incorrect_checksum)
  assert_false(IntegrityResult::is_valid(incorrect_result))
  
  // Test data tampering detection
  let original_data = "Original data that should not be modified"
  let original_checksum = IntegrityChecker::calculate_checksum(integrity_checker, original_data)
  
  let tampered_data = "Modified data that has been tampered with"
  let tampered_result = IntegrityChecker::verify_checksum(integrity_checker, tampered_data, original_checksum)
  assert_false(IntegrityResult::is_valid(tampered_result))
  
  // Test hash-based integrity
  let hash_value = IntegrityChecker::calculate_hash(integrity_checker, data, HashAlgorithm::SHA256)
  let hash_result = IntegrityChecker::verify_hash(integrity_checker, data, hash_value, HashAlgorithm::SHA256)
  assert_true(IntegrityResult::is_valid(hash_result))
  
  // Test with different hash algorithms
  let md5_hash = IntegrityChecker::calculate_hash(integrity_checker, data, HashAlgorithm::MD5)
  let md5_result = IntegrityChecker::verify_hash(integrity_checker, data, md5_hash, HashAlgorithm::MD5)
  assert_true(IntegrityResult::is_valid(md5_result))
  
  let sha1_hash = IntegrityChecker::calculate_hash(integrity_checker, data, HashAlgorithm::SHA1)
  let sha1_result = IntegrityChecker::verify_hash(integrity_checker, data, sha1_hash, HashAlgorithm::SHA1)
  assert_true(IntegrityResult::is_valid(sha1_result))
  
  // Test digital signature
  let key_pair = IntegrityChecker::generate_key_pair(integrity_checker, KeyAlgorithm::RSA)
  let private_key = KeyPair::get_private_key(key_pair)
  let public_key = KeyPair::get_public_key(key_pair)
  
  let signature = IntegrityChecker::sign_data(integrity_checker, data, private_key)
  let signature_result = IntegrityChecker::verify_signature(integrity_checker, data, signature, public_key)
  assert_true(IntegrityResult::is_valid(signature_result))
}

// Test 4: Referential Integrity Validation
test "referential integrity validation" {
  // Create a referential integrity validator
  let validator = ReferentialIntegrityValidator::new()
  
  // Define data model with relationships
  validator.define_entity("users", [
    ("id", DataType::Integer, true, None),  // Primary key
    ("name", DataType::String, true, None),
    ("email", DataType::String, true, None),
    ("department_id", DataType::Integer, false, Some("departments.id"))  // Foreign key
  ])
  
  validator.define_entity("departments", [
    ("id", DataType::Integer, true, None),  // Primary key
    ("name", DataType::String, true, None),
    ("location", DataType::String, false, None)
  ])
  
  validator.define_entity("projects", [
    ("id", DataType::Integer, true, None),  // Primary key
    ("name", DataType::String, true, None),
    ("manager_id", DataType::Integer, false, Some("users.id")),  // Foreign key
    ("department_id", DataType::Integer, false, Some("departments.id"))  // Foreign key
  ])
  
  // Create valid data with proper references
  let departments_data = [
    {"id": 1, "name": "Engineering", "location": "Building A"},
    {"id": 2, "name": "Marketing", "location": "Building B"}
  ]
  
  let users_data = [
    {"id": 101, "name": "John Doe", "email": "john@example.com", "department_id": 1},
    {"id": 102, "name": "Jane Smith", "email": "jane@example.com", "department_id": 1},
    {"id": 103, "name": "Bob Johnson", "email": "bob@example.com", "department_id": 2}
  ]
  
  let projects_data = [
    {"id": 1001, "name": "Project Alpha", "manager_id": 101, "department_id": 1},
    {"id": 1002, "name": "Project Beta", "manager_id": 102, "department_id": 1},
    {"id": 1003, "name": "Project Gamma", "manager_id": 103, "department_id": 2}
  ]
  
  // Load data into validator
  validator.load_entity_data("departments", departments_data)
  validator.load_entity_data("users", users_data)
  validator.load_entity_data("projects", projects_data)
  
  // Validate referential integrity
  let integrity_result = validator.validate_all_references()
  assert_true(ReferentialIntegrityResult::is_valid(integrity_result))
  
  // Test invalid reference
  let invalid_users_data = [
    {"id": 104, "name": "Invalid User", "email": "invalid@example.com", "department_id": 99}  // Non-existent department
  ]
  
  validator.load_entity_data("users", invalid_users_data)
  
  let invalid_integrity_result = validator.validate_all_references()
  assert_false(ReferentialIntegrityResult::is_valid(invalid_integrity_result))
  
  // Check specific reference errors
  let reference_errors = ReferentialIntegrityResult::get_reference_errors(invalid_integrity_result)
  assert_true(reference_errors.length() > 0)
  
  let user_error = ReferentialIntegrityResult::get_error_for_entity(invalid_integrity_result, "users")
  match user_error {
    Some(error) => {
      assert_eq(ReferenceError::get_field(error), "department_id")
      assert_eq(ReferenceError::get_invalid_value(error), 99)
      assert_eq(ReferenceError::get_referenced_entity(error), "departments")
    }
    None => assert_true(false)
  }
  
  // Test circular reference detection
  validator.define_entity("circular_a", [
    ("id", DataType::Integer, true, None),
    ("name", DataType::String, true, None),
    ("b_id", DataType::Integer, false, Some("circular_b.id"))
  ])
  
  validator.define_entity("circular_b", [
    ("id", DataType::Integer, true, None),
    ("name", DataType::String, true, None),
    ("a_id", DataType::Integer, false, Some("circular_a.id"))
  ])
  
  let circular_data_a = [{"id": 1, "name": "A1", "b_id": 1}]
  let circular_data_b = [{"id": 1, "name": "B1", "a_id": 1}]
  
  validator.load_entity_data("circular_a", circular_data_a)
  validator.load_entity_data("circular_b", circular_data_b)
  
  let circular_result = validator.validate_all_references()
  assert_false(ReferentialIntegrityResult::is_valid(circular_result))
  
  let circular_errors = ReferentialIntegrityResult::get_circular_reference_errors(circular_result)
  assert_true(circular_errors.length() > 0)
}

// Test 5: Business Rule Validation
test "business rule validation" {
  // Create a business rule validator
  let validator = BusinessRuleValidator::new()
  
  // Define business rules for an order system
  validator.add_rule("order.total", BusinessRule::new(
    "Order total must be greater than 0",
    |data| {
      let total = data.get_float("order.total")
      total > 0.0
    }
  ))
  
  validator.add_rule("order.items", BusinessRule::new(
    "Order must have at least one item",
    |data| {
      let items = data.get_array("order.items")
      items.length() > 0
    }
  ))
  
  validator.add_rule("order.payment_method", BusinessRule::new(
    "Payment method must be valid",
    |data| {
      let payment_method = data.get_string("order.payment_method")
      ["credit_card", "debit_card", "paypal", "bank_transfer"].contains(payment_method)
    }
  ))
  
  validator.add_rule("order.shipping_address", BusinessRule::new(
    "Shipping address is required for physical products",
    |data| {
      let has_physical_products = data.get_bool("order.has_physical_products")
      if has_physical_products {
        let shipping_address = data.get_object("order.shipping_address")
        shipping_address.has_key("street") && 
        shipping_address.has_key("city") && 
        shipping_address.has_key("zip_code")
      } else {
        true
      }
    }
  ))
  
  validator.add_rule("order.discount", BusinessRule::new(
    "Discount cannot exceed 20% of total",
    |data| {
      let total = data.get_float("order.total")
      let discount = data.get_float("order.discount")
      discount <= total * 0.2
    }
  ))
  
  // Create valid order data
  let valid_order = BusinessData::new()
  valid_order.set_float("order.total", 100.0)
  valid_order.set_array("order.items", [
    {"id": 1, "name": "Product A", "price": 50.0, "quantity": 2}
  ])
  valid_order.set_string("order.payment_method", "credit_card")
  valid_order.set_bool("order.has_physical_products", true)
  valid_order.set_object("order.shipping_address", {
    "street": "123 Main St",
    "city": "New York",
    "zip_code": "10001"
  })
  valid_order.set_float("order.discount", 10.0)
  
  // Validate valid order
  let valid_result = validator.validate(valid_order)
  assert_true(BusinessRuleResult::is_valid(valid_result))
  
  // Create invalid order data
  let invalid_order = BusinessData::new()
  invalid_order.set_float("order.total", 0.0)  // Violates: total > 0
  invalid_order.set_array("order.items", [])  // Violates: at least one item
  invalid_order.set_string("order.payment_method", "bitcoin")  // Violates: invalid payment method
  invalid_order.set_bool("order.has_physical_products", true)
  invalid_order.set_object("order.shipping_address", {
    "street": "123 Main St"
    // Missing city and zip_code
  })
  invalid_order.set_float("order.discount", 30.0)  // Violates: discount > 20% of total
  
  // Validate invalid order
  let invalid_result = validator.validate(invalid_order)
  assert_false(BusinessRuleResult::is_valid(invalid_result))
  
  // Check specific rule violations
  let violations = BusinessRuleResult::get_violations(invalid_result)
  assert_eq(violations.length(), 5)  // All 5 rules should be violated
  
  let total_violation = BusinessRuleResult::get_violation_for_field(invalid_result, "order.total")
  match total_violation {
    Some(violation) => assert_eq(BusinessRuleViolation::get_description(violation), "Order total must be greater than 0")
    None => assert_true(false)
  }
  
  // Test conditional business rules
  validator.add_conditional_rule(
    "order.express_shipping",
    |data| data.get_string("order.payment_method") == "credit_card",
    BusinessRule::new(
      "Express shipping requires credit card payment",
      |data| {
        let express_shipping = data.get_bool("order.express_shipping")
        let payment_method = data.get_string("order.payment_method")
        !express_shipping || payment_method == "credit_card"
      }
    )
  )
  
  // Test with express shipping and non-credit card payment
  let conditional_order = BusinessData::new()
  conditional_order.set_float("order.total", 50.0)
  conditional_order.set_array("order.items", [
    {"id": 1, "name": "Product A", "price": 50.0, "quantity": 1}
  ])
  conditional_order.set_string("order.payment_method", "paypal")
  conditional_order.set_bool("order.express_shipping", true)  // Violates conditional rule
  
  let conditional_result = validator.validate(conditional_order)
  assert_false(BusinessRuleResult::is_valid(conditional_result))
  
  let conditional_violations = BusinessRuleResult::get_violations(conditional_result)
  assert_true(conditional_violations.length() > 0)
}

// Test 6: Data Consistency Validation
test "data consistency validation" {
  // Create a consistency validator
  let validator = ConsistencyValidator::new()
  
  // Define consistency rules for a financial system
  validator.add_rule("account.balance", ConsistencyRule::new(
    "Account balance must equal sum of transactions",
    |data| {
      let balance = data.get_float("account.balance")
      let transactions = data.get_array("account.transactions")
      
      let mut transaction_sum = 0.0
      for transaction in transactions {
        transaction_sum = transaction_sum + transaction.get_float("amount")
      }
      
      // Allow small floating point differences
      (balance - transaction_sum).abs() < 0.01
    }
  ))
  
  validator.add_rule("portfolio.total_value", ConsistencyRule::new(
    "Portfolio total value must equal sum of holdings",
    |data| {
      let total_value = data.get_float("portfolio.total_value")
      let holdings = data.get_array("portfolio.holdings")
      
      let mut holdings_sum = 0.0
      for holding in holdings {
        holdings_sum = holdings_sum + (holding.get_float("quantity") * holding.get_float("price"))
      }
      
      // Allow small floating point differences
      (total_value - holdings_sum).abs() < 0.01
    }
  ))
  
  validator.add_rule("budget.remaining", ConsistencyRule::new(
    "Remaining budget must equal total budget minus spent amount",
    |data| {
      let total_budget = data.get_float("budget.total")
      let spent_amount = data.get_float("budget.spent")
      let remaining_budget = data.get_float("budget.remaining")
      
      // Allow small floating point differences
      (remaining_budget - (total_budget - spent_amount)).abs() < 0.01
    }
  ))
  
  // Create consistent financial data
  let consistent_data = ConsistencyData::new()
  
  // Account with consistent balance
  let transactions = [
    {"id": 1, "amount": 1000.0, "type": "deposit"},
    {"id": 2, "amount": -200.0, "type": "withdrawal"},
    {"id": 3, "amount": 50.0, "type": "deposit"}
  ]
  let transaction_sum = 1000.0 - 200.0 + 50.0
  
  consistent_data.set_float("account.balance", transaction_sum)
  consistent_data.set_array("account.transactions", transactions)
  
  // Portfolio with consistent total value
  let holdings = [
    {"symbol": "AAPL", "quantity": 10, "price": 150.0},
    {"symbol": "GOOGL", "quantity": 5, "price": 2500.0}
  ]
  let holdings_sum = (10 * 150.0) + (5 * 2500.0)
  
  consistent_data.set_float("portfolio.total_value", holdings_sum)
  consistent_data.set_array("portfolio.holdings", holdings)
  
  // Budget with consistent remaining amount
  let total_budget = 5000.0
  let spent_amount = 1200.0
  let remaining_budget = total_budget - spent_amount
  
  consistent_data.set_float("budget.total", total_budget)
  consistent_data.set_float("budget.spent", spent_amount)
  consistent_data.set_float("budget.remaining", remaining_budget)
  
  // Validate consistent data
  let consistent_result = validator.validate(consistent_data)
  assert_true(ConsistencyResult::is_valid(consistent_result))
  
  // Create inconsistent financial data
  let inconsistent_data = ConsistencyData::new()
  
  // Account with inconsistent balance
  inconsistent_data.set_float("account.balance", 500.0)  // Wrong balance
  inconsistent_data.set_array("account.transactions", transactions)
  
  // Portfolio with inconsistent total value
  inconsistent_data.set_float("portfolio.total_value", 10000.0)  // Wrong total
  inconsistent_data.set_array("portfolio.holdings", holdings)
  
  // Budget with inconsistent remaining amount
  inconsistent_data.set_float("budget.total", total_budget)
  inconsistent_data.set_float("budget.spent", spent_amount)
  inconsistent_data.set_float("budget.remaining", 5000.0)  // Wrong remaining
  
  // Validate inconsistent data
  let inconsistent_result = validator.validate(inconsistent_data)
  assert_false(ConsistencyResult::is_valid(inconsistent_result))
  
  // Check specific consistency violations
  let violations = ConsistencyResult::get_violations(inconsistent_result)
  assert_eq(violations.length(), 3)  // All 3 rules should be violated
  
  let account_violation = ConsistencyResult::get_violation_for_field(inconsistent_result, "account.balance")
  match account_violation {
    Some(violation) => assert_eq(ConsistencyViolation::get_description(violation), "Account balance must equal sum of transactions")
    None => assert_true(false)
  }
  
  // Test cross-entity consistency
  validator.add_cross_entity_rule(
    ["account.balance", "portfolio.total_value"],
    "Total assets must equal account balance plus portfolio value",
    |data| {
      let account_balance = data.get_float("account.balance")
      let portfolio_value = data.get_float("portfolio.total_value")
      let total_assets = data.get_float("total_assets")
      
      // Allow small floating point differences
      (total_assets - (account_balance + portfolio_value)).abs() < 0.01
    }
  )
  
  consistent_data.set_float("total_assets", transaction_sum + holdings_sum)
  
  let cross_entity_result = validator.validate(consistent_data)
  assert_true(ConsistencyResult::is_valid(cross_entity_result))
  
  inconsistent_data.set_float("total_assets", 1000.0)  // Wrong total
  
  let cross_entity_invalid_result = validator.validate(inconsistent_data)
  assert_false(ConsistencyResult::is_valid(cross_entity_invalid_result))
}

// Test 7: Data Format Validation
test "data format validation" {
  // Create a format validator
  let validator = FormatValidator::new()
  
  // Test email format validation
  validator.add_format("email", "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")
  
  let valid_emails = [
    "user@example.com",
    "user.name@domain.co.uk",
    "user+tag@example.org",
    "user123@test-domain.com"
  ]
  
  for email in valid_emails {
    let result = validator.validate_format("email", email)
    assert_true(FormatValidationResult::is_valid(result))
  }
  
  let invalid_emails = [
    "user@",
    "@example.com",
    "user.example.com",
    "user@.com",
    "user@domain.",
    "user name@example.com",
    "user@domain..com"
  ]
  
  for email in invalid_emails {
    let result = validator.validate_format("email", email)
    assert_false(FormatValidationResult::is_valid(result))
  }
  
  // Test phone number format validation
  validator.add_format("phone", "^\\+?[1-9]\\d{1,14}$")
  
  let valid_phones = [
    "+1234567890",
    "1234567890",
    "+11234567890",
    "+441234567890"
  ]
  
  for phone in valid_phones {
    let result = validator.validate_format("phone", phone)
    assert_true(FormatValidationResult::is_valid(result))
  }
  
  let invalid_phones = [
    "12345",           // Too short
    "+01234567890",    // Starts with 0 after +
    "123456789012345", // Too long
    "+1234567890a",    // Contains letter
    "(123)456-7890"    // Invalid characters
  ]
  
  for phone in invalid_phones {
    let result = validator.validate_format("phone", phone)
    assert_false(FormatValidationResult::is_valid(result))
  }
  
  // Test URL format validation
  validator.add_format("url", "^https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)$")
  
  let valid_urls = [
    "http://example.com",
    "https://example.com",
    "https://www.example.com",
    "https://example.com/path/to/resource",
    "https://example.com/path?query=value",
    "https://example.com/path#section"
  ]
  
  for url in valid_urls {
    let result = validator.validate_format("url", url)
    assert_true(FormatValidationResult::is_valid(result))
  }
  
  let invalid_urls = [
    "ftp://example.com",      // Wrong protocol
    "http://",                // Missing domain
    "example.com",            // Missing protocol
    "https://example .com",   // Space in domain
    "https://example.com:"    // Trailing colon
  ]
  
  for url in invalid_urls {
    let result = validator.validate_format("url", url)
    assert_false(FormatValidationResult::is_valid(result))
  }
  
  // Test date format validation
  validator.add_format("date", "^\\d{4}-\\d{2}-\\d{2}$")
  
  let valid_dates = [
    "2023-01-15",
    "1999-12-31",
    "2000-02-29"
  ]
  
  for date in valid_dates {
    let result = validator.validate_format("date", date)
    assert_true(FormatValidationResult::is_valid(result))
  }
  
  let invalid_dates = [
    "2023-1-15",        // Single digit month
    "2023-01-5",        // Single digit day
    "23-01-15",         // Two digit year
    "2023/01/15",       // Wrong separator
    "2023-13-15",       // Invalid month
    "2023-01-32",       // Invalid day
    "2023.01.15"        // Wrong separator
  ]
  
  for date in invalid_dates {
    let result = validator.validate_format("date", date)
    assert_false(FormatValidationResult::is_valid(result))
  }
  
  // Test custom format validation with callback
  validator.add_custom_format("custom_id", |value| {
    if value.length() != 10 {
      return false
    }
    
    // First 3 characters are letters, last 7 are digits
    let prefix = value.substring(0, 3)
    let suffix = value.substring(3, 10)
    
    prefix.is_alpha() && suffix.is_numeric()
  })
  
  let valid_custom_ids = [
    "ABC1234567",
    "XYZ9876543",
    "AAA1111111"
  ]
  
  for id in valid_custom_ids {
    let result = validator.validate_format("custom_id", id)
    assert_true(FormatValidationResult::is_valid(result))
  }
  
  let invalid_custom_ids = [
    "AB12345678",      // Only 2 letters
    "ABCD123456",      // 4 letters
    "ABC123456",       // Too short
    "ABC12345678",     // Too long
    "ABC12D4567",      // Letter in numeric part
    "123ABC4567"       // Numbers in letter part
  ]
  
  for id in invalid_custom_ids {
    let result = validator.validate_format("custom_id", id)
    assert_false(FormatValidationResult::is_valid(result))
  }
}

// Test 8: Data Range and Constraint Validation
test "data range and constraint validation" {
  // Create a constraint validator
  let validator = ConstraintValidator::new()
  
  // Test numeric range constraints
  validator.add_numeric_range("age", 18, 120)
  validator.add_numeric_range("temperature", -273.15, 1000.0)
  validator.add_numeric_range("percentage", 0.0, 100.0)
  
  // Test valid numeric ranges
  assert_true(validator.validate_numeric_range("age", 25))
  assert_true(validator.validate_numeric_range("age", 18))
  assert_true(validator.validate_numeric_range("age", 120))
  
  assert_true(validator.validate_numeric_range("temperature", 0.0))
  assert_true(validator.validate_numeric_range("temperature", -273.15))
  assert_true(validator.validate_numeric_range("temperature", 1000.0))
  
  assert_true(validator.validate_numeric_range("percentage", 0.0))
  assert_true(validator.validate_numeric_range("percentage", 50.0))
  assert_true(validator.validate_numeric_range("percentage", 100.0))
  
  // Test invalid numeric ranges
  assert_false(validator.validate_numeric_range("age", 17))
  assert_false(validator.validate_numeric_range("age", 121))
  
  assert_false(validator.validate_numeric_range("temperature", -274.0))
  assert_false(validator.validate_numeric_range("temperature", 1001.0))
  
  assert_false(validator.validate_numeric_range("percentage", -0.1))
  assert_false(validator.validate_numeric_range("percentage", 100.1))
  
  // Test string length constraints
  validator.add_string_length_range("username", 3, 20)
  validator.add_string_length_range("password", 8, 128)
  validator.add_string_length_range("title", 1, 100)
  
  // Test valid string lengths
  assert_true(validator.validate_string_length("username", "user"))
  assert_true(validator.validate_string_length("username", "verylongusername"))
  assert_true(validator.validate_string_length("password", "password123"))
  assert_true(validator.validate_string_length("title", "A"))
  
  // Test invalid string lengths
  assert_false(validator.validate_string_length("username", "us"))
  assert_false(validator.validate_string_length("username", "this_is_a_very_long_username_that_exceeds_limit"))
  assert_false(validator.validate_string_length("password", "short"))
  assert_false(validator.validate_string_length("title", ""))
  
  // Test array size constraints
  validator.add_array_size_range("tags", 0, 10)
  validator.add_array_size_range("items", 1, 100)
  
  // Test valid array sizes
  assert_true(validator.validate_array_size("tags", []))
  assert_true(validator.validate_array_size("tags", ["tag1", "tag2", "tag3"]))
  assert_true(validator.validate_array_size("tags", ["tag1", "tag2", "tag3", "tag4", "tag5", "tag6", "tag7", "tag8", "tag9", "tag10"]))
  
  assert_true(validator.validate_array_size("items", ["item1"]))
  assert_true(validator.validate_array_size("items", ["item1", "item2", "item3"]))
  
  // Test invalid array sizes
  assert_false(validator.validate_array_size("tags", ["tag1", "tag2", "tag3", "tag4", "tag5", "tag6", "tag7", "tag8", "tag9", "tag10", "tag11"]))
  
  assert_false(validator.validate_array_size("items", []))
  assert_false(validator.validate_array_size("items", Array::new(101, |_| "item")))
  
  // Test date range constraints
  let start_date = DateTime::new(2023, 1, 1, 0, 0, 0)
  let end_date = DateTime::new(2023, 12, 31, 23, 59, 59)
  
  validator.add_date_range("event_date", start_date, end_date)
  
  // Test valid dates
  assert_true(validator.validate_date_range("event_date", DateTime::new(2023, 6, 15, 12, 0, 0)))
  assert_true(validator.validate_date_range("event_date", start_date))
  assert_true(validator.validate_date_range("event_date", end_date))
  
  // Test invalid dates
  assert_false(validator.validate_date_range("event_date", DateTime::new(2022, 12, 31, 23, 59, 59)))
  assert_false(validator.validate_date_range("event_date", DateTime::new(2024, 1, 1, 0, 0, 0)))
  
  // Test custom constraints
  validator.add_custom_constraint("credit_card", |value| {
    // Simple Luhn algorithm validation
    if !value.is_numeric() {
      return false
    }
    
    let mut sum = 0
    let mut double_digit = false
    
    for i in range(value.length() - 1, -1, -1) {
      let digit = value.substring(i, i + 1).to_int()
      
      if double_digit {
        digit = digit * 2
        if digit > 9 {
          digit = digit - 9
        }
      }
      
      sum = sum + digit
      double_digit = !double_digit
    }
    
    sum % 10 == 0
  })
  
  // Test valid credit card numbers (using test numbers)
  assert_true(validator.validate_custom_constraint("credit_card", "4532015112830366"))  // Visa
  assert_true(validator.validate_custom_constraint("credit_card", "5555555555554444"))  // MasterCard
  
  // Test invalid credit card numbers
  assert_false(validator.validate_custom_constraint("credit_card", "1234567890123456"))
  assert_false(validator.validate_custom_constraint("credit_card", "4532015112830367"))  // Invalid checksum
}

// Test 9: Data Validation Pipeline
test "data validation pipeline" {
  // Create a validation pipeline
  let pipeline = ValidationPipeline::new()
  
  // Add validation stages
  pipeline.add_stage("format_validation", |data| {
    let validator = FormatValidator::new()
    validator.add_format("email", "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")
    validator.add_format("phone", "^\\+?[1-9]\\d{1,14}$")
    
    let email_result = validator.validate_format("email", data.get_string("email"))
    let phone_result = validator.validate_format("phone", data.get_string("phone"))
    
    if FormatValidationResult::is_valid(email_result) && FormatValidationResult::is_valid(phone_result) {
      PipelineResult::success(data)
    } else {
      PipelineResult::failure("Format validation failed")
    }
  })
  
  pipeline.add_stage("business_rules", |data| {
    let validator = BusinessRuleValidator::new()
    
    validator.add_rule("user.age", BusinessRule::new(
      "User must be at least 18 years old",
      |data| data.get_int("age") >= 18
    ))
    
    validator.add_rule("order.total", BusinessRule::new(
      "Order total must be positive",
      |data| data.get_float("order.total") > 0
    ))
    
    let result = validator.validate(data)
    
    if BusinessRuleResult::is_valid(result) {
      PipelineResult::success(data)
    } else {
      PipelineResult::failure("Business rule validation failed")
    }
  })
  
  pipeline.add_stage("consistency_check", |data| {
    let validator = ConsistencyValidator::new()
    
    validator.add_rule("order.total", ConsistencyRule::new(
      "Order total must equal sum of item prices",
      |data| {
        let total = data.get_float("order.total")
        let items = data.get_array("order.items")
        
        let mut sum = 0.0
        for item in items {
          sum = sum + (item.get_float("price") * item.get_int("quantity"))
        }
        
        (total - sum).abs() < 0.01
      }
    ))
    
    let result = validator.validate(data)
    
    if ConsistencyResult::is_valid(result) {
      PipelineResult::success(data)
    } else {
      PipelineResult::failure("Consistency check failed")
    }
  })
  
  // Create valid data
  let valid_data = PipelineData::new()
  valid_data.set_string("email", "user@example.com")
  valid_data.set_string("phone", "+1234567890")
  valid_data.set_int("age", 25)
  valid_data.set_float("order.total", 100.0)
  valid_data.set_array("order.items", [
    {"price": 50.0, "quantity": 2}
  ])
  
  // Run pipeline with valid data
  let valid_result = pipeline.execute(valid_data)
  assert_true(PipelineResult::is_success(valid_result))
  
  // Create invalid data (fails format validation)
  let invalid_format_data = PipelineData::new()
  invalid_format_data.set_string("email", "invalid-email")
  invalid_format_data.set_string("phone", "+1234567890")
  invalid_format_data.set_int("age", 25)
  invalid_format_data.set_float("order.total", 100.0)
  invalid_format_data.set_array("order.items", [
    {"price": 50.0, "quantity": 2}
  ])
  
  let format_result = pipeline.execute(invalid_format_data)
  assert_false(PipelineResult::is_success(format_result))
  assert_eq(PipelineResult::get_error(format_result), "Format validation failed")
  
  // Create invalid data (fails business rules)
  let invalid_rules_data = PipelineData::new()
  invalid_rules_data.set_string("email", "user@example.com")
  invalid_rules_data.set_string("phone", "+1234567890")
  invalid_rules_data.set_int("age", 16)  // Under 18
  invalid_rules_data.set_float("order.total", 100.0)
  invalid_rules_data.set_array("order.items", [
    {"price": 50.0, "quantity": 2}
  ])
  
  let rules_result = pipeline.execute(invalid_rules_data)
  assert_false(PipelineResult::is_success(rules_result))
  assert_eq(PipelineResult::get_error(rules_result), "Business rule validation failed")
  
  // Create invalid data (fails consistency check)
  let invalid_consistency_data = PipelineData::new()
  invalid_consistency_data.set_string("email", "user@example.com")
  invalid_consistency_data.set_string("phone", "+1234567890")
  invalid_consistency_data.set_int("age", 25)
  invalid_consistency_data.set_float("order.total", 150.0)  // Wrong total
  invalid_consistency_data.set_array("order.items", [
    {"price": 50.0, "quantity": 2}
  ])
  
  let consistency_result = pipeline.execute(invalid_consistency_data)
  assert_false(PipelineResult::is_success(consistency_result))
  assert_eq(PipelineResult::get_error(consistency_result), "Consistency check failed")
  
  // Test conditional pipeline stages
  pipeline.add_conditional_stage(
    "premium_validation",
    |data| data.get_bool("is_premium_user"),
    |data| {
      // Additional validation for premium users
      if data.has_key("premium_features") {
        PipelineResult::success(data)
      } else {
        PipelineResult::failure("Premium user missing features")
      }
    }
  )
  
  // Test with premium user
  let premium_data = PipelineData::new()
  premium_data.set_string("email", "premium@example.com")
  premium_data.set_string("phone", "+1234567890")
  premium_data.set_int("age", 25)
  premium_data.set_float("order.total", 100.0)
  premium_data.set_array("order.items", [
    {"price": 50.0, "quantity": 2}
  ])
  premium_data.set_bool("is_premium_user", true)
  premium_data.set_array("premium_features", ["feature1", "feature2"])
  
  let premium_result = pipeline.execute(premium_data)
  assert_true(PipelineResult::is_success(premium_result))
  
  // Test with premium user missing features
  let invalid_premium_data = PipelineData::new()
  invalid_premium_data.set_string("email", "premium@example.com")
  invalid_premium_data.set_string("phone", "+1234567890")
  invalid_premium_data.set_int("age", 25)
  invalid_premium_data.set_float("order.total", 100.0)
  invalid_premium_data.set_array("order.items", [
    {"price": 50.0, "quantity": 2}
  ])
  invalid_premium_data.set_bool("is_premium_user", true)
  // Missing premium_features
  
  let invalid_premium_result = pipeline.execute(invalid_premium_data)
  assert_false(PipelineResult::is_success(invalid_premium_result))
  assert_eq(PipelineResult::get_error(invalid_premium_result), "Premium user missing features")
}

// Test 10: Data Validation Metrics and Reporting
test "data validation metrics and reporting" {
  // Create a validation metrics collector
  let metrics_collector = ValidationMetricsCollector::new()
  
  // Create a validator with metrics enabled
  let validator = DataValidator::new()
  validator.enable_metrics(true)
  validator.set_metrics_collector(metrics_collector)
  
  // Add validation rules
  validator.add_rule("email", ValidationRule::String(StringConstraints {
    min_length: Some(5),
    max_length: Some(100),
    pattern: Some("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"),
    required: true
  }))
  
  validator.add_rule("age", ValidationRule::Integer(IntegerConstraints {
    min_value: Some(18),
    max_value: Some(120),
    required: true
  }))
  
  validator.add_rule("name", ValidationRule::String(StringConstraints {
    min_length: Some(2),
    max_length: Some(50),
    pattern: None,
    required: true
  }))
  
  // Run multiple validations
  for i in 0..100 {
    let data = ValidationData::new()
    
    // Mix of valid and invalid data
    if i % 3 == 0 {
      // Valid data
      data.set_string("email", "user" + i.to_string() + "@example.com")
      data.set_int("age", 25 + (i % 50))
      data.set_string("name", "User " + i.to_string())
    } else if i % 3 == 1 {
      // Invalid email
      data.set_string("email", "invalid-email")
      data.set_int("age", 25 + (i % 50))
      data.set_string("name", "User " + i.to_string())
    } else {
      // Invalid age
      data.set_string("email", "user" + i.to_string() + "@example.com")
      data.set_int("age", 10 + (i % 5))  // Under 18
      data.set_string("name", "User " + i.to_string())
    }
    
    validator.validate(data)
  }
  
  // Get validation metrics
  let metrics = validator.get_metrics()
  
  // Verify metrics
  assert_eq(ValidationMetrics::get_total_validations(metrics), 100)
  assert_eq(ValidationMetrics::get_successful_validations(metrics), 34)  // Approximately 1/3
  assert_eq(ValidationMetrics::get_failed_validations(metrics), 66)    // Approximately 2/3
  
  // Get field-specific metrics
  let email_metrics = ValidationMetrics::get_field_metrics(metrics, "email")
  assert_eq(FieldMetrics::get_validation_count(email_metrics), 100)
  assert_eq(FieldMetrics::get_success_count(email_metrics), 67)  // 2/3 valid
  assert_eq(FieldMetrics::get_failure_count(email_metrics), 33)  // 1/3 invalid
  
  let age_metrics = ValidationMetrics::get_field_metrics(metrics, "age")
  assert_eq(FieldMetrics::get_validation_count(age_metrics), 100)
  assert_eq(FieldMetrics::get_success_count(age_metrics), 67)  // 2/3 valid
  assert_eq(FieldMetrics::get_failure_count(age_metrics), 33)  // 1/3 invalid
  
  let name_metrics = ValidationMetrics::get_field_metrics(metrics, "name")
  assert_eq(FieldMetrics::get_validation_count(name_metrics), 100)
  assert_eq(FieldMetrics::get_success_count(name_metrics), 100)  // All valid
  assert_eq(FieldMetrics::get_failure_count(name_metrics), 0)    // None invalid
  
  // Get error distribution
  let error_distribution = ValidationMetrics::get_error_distribution(metrics)
  assert_true(error_distribution.size() > 0)
  
  // Test validation report generation
  let report = validator.generate_report()
  
  // Verify report content
  assert_true(ValidationReport::get_summary(report).contains("Total validations: 100"))
  assert_true(ValidationReport::get_summary(report).contains("Successful: 34"))
  assert_true(ValidationReport::get_summary(report).contains("Failed: 66"))
  
  let field_reports = ValidationReport::get_field_reports(report)
  assert_eq(field_reports.length(), 3)  // email, age, name
  
  // Test most common errors
  let common_errors = ValidationMetrics::get_most_common_errors(metrics, 5)
  assert_true(common_errors.length() > 0)
  
  // Test validation performance metrics
  let performance_metrics = ValidationMetrics::get_performance_metrics(metrics)
  assert_true(PerformanceMetrics::get_avg_validation_time(performance_metrics) >= 0)
  assert_true(PerformanceMetrics::get_max_validation_time(performance_metrics) >= 0)
  assert_true(PerformanceMetrics::get_min_validation_time(performance_metrics) >= 0)
  
  // Test time-based metrics
  let hourly_metrics = ValidationMetrics::get_hourly_metrics(metrics)
  assert_true(hourly_metrics.size() > 0)
  
  // Test validation trend analysis
  let trend_analysis = ValidationMetrics::get_trend_analysis(metrics, 10)  // Last 10 validations
  assert_true(TrendAnalysis::get_success_rate(trend_analysis) >= 0.0)
  assert_true(TrendAnalysis::get_success_rate(trend_analysis) <= 1.0)
  
  // Export metrics to different formats
  let json_metrics = ValidationMetrics::export_to_json(metrics)
  assert_true(json_metrics.length() > 0)
  
  let csv_metrics = ValidationMetrics::export_to_csv(metrics)
  assert_true(csv_metrics.length() > 0)
  
  // Test metrics reset
  validator.reset_metrics()
  let reset_metrics = validator.get_metrics()
  assert_eq(ValidationMetrics::get_total_validations(reset_metrics), 0)
}
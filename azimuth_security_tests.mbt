// Azimuth Security Tests
// This file contains comprehensive test cases for security features and vulnerability protection

// Test 1: Input Validation and Sanitization
test "input validation and sanitization" {
  type ValidationResult {
    Valid
    Invalid(String)
  }
  
  type ValidationRule {
    Required
    MinLength(Int)
    MaxLength(Int)
    Pattern(String)
    NoSpecialChars
    NoSQLInjection
    NoXSS
  }
  
  let validate_input = fn(input : String, rules : Array[ValidationRule]) : ValidationResult {
    for rule in rules {
      match rule {
        ValidationRule::Required => {
          if input.length() == 0 {
            return ValidationResult::Invalid("Input is required")
          }
        }
        ValidationRule::MinLength(min_len) => {
          if input.length() < min_len {
            return ValidationResult::Invalid("Input is too short")
          }
        }
        ValidationRule::MaxLength(max_len) => {
          if input.length() > max_len {
            return ValidationResult::Invalid("Input is too long")
          }
        }
        ValidationRule::Pattern(pattern) => {
          // Simplified pattern matching for testing
          if pattern == "email" && !input.contains("@") {
            return ValidationResult::Invalid("Invalid email format")
          } else if pattern == "phone" && input.length() != 10 {
            return ValidationResult::Invalid("Invalid phone format")
          }
        }
        ValidationRule::NoSpecialChars => {
          for i in 0..<input.length() {
            let char = input.substring(i, 1)
            if char == "<" || char == ">" || char == "\"" || char == "'" || char == "&" {
              return ValidationResult::Invalid("Special characters not allowed")
            }
          }
        }
        ValidationRule::NoSQLInjection => {
          let lower_input = input.to_lowercase()
          if lower_input.contains("drop") || lower_input.contains("delete") || lower_input.contains("insert") || lower_input.contains("update") || lower_input.contains("'") || lower_input.contains("\"") || lower_input.contains(";") {
            return ValidationResult::Invalid("Potential SQL injection detected")
          }
        }
        ValidationRule::NoXSS => {
          let lower_input = input.to_lowercase()
          if lower_input.contains("<script") || lower_input.contains("javascript:") || lower_input.contains("onerror") || lower_input.contains("onload") {
            return ValidationResult::Invalid("Potential XSS attack detected")
          }
        }
      }
    }
    ValidationResult::Valid
  }
  
  let sanitize_string = fn(input : String) : String {
    let mut result = ""
    for i in 0..<input.length() {
      let char = input.substring(i, 1)
      if char == "<" {
        result = result + "&lt;"
      } else if char == ">" {
        result = result + "&gt;"
      } else if char == "\"" {
        result = result + "&quot;"
      } else if char == "'" {
        result = result + "&#x27;"
      } else if char == "&" {
        result = result + "&amp;"
      } else {
        result = result + char
      }
    }
    result
  }
  
  // Test validation rules
  // Test required validation
  match validate_input("", [ValidationRule::Required]) {
    ValidationResult::Valid => assert_true(false)
    ValidationResult::Invalid(msg) => assert_eq(msg, "Input is required")
  }
  
  match validate_input("value", [ValidationRule::Required]) {
    ValidationResult::Valid => assert_true(true)
    ValidationResult::Invalid(_) => assert_true(false)
  }
  
  // Test length validation
  match validate_input("ab", [ValidationRule::MinLength(3)]) {
    ValidationResult::Valid => assert_true(false)
    ValidationResult::Invalid(msg) => assert_eq(msg, "Input is too short")
  }
  
  match validate_input("abcd", [ValidationRule::MinLength(3)]) {
    ValidationResult::Valid => assert_true(true)
    ValidationResult::Invalid(_) => assert_true(false)
  }
  
  match validate_input("abcdef", [ValidationRule::MaxLength(5)]) {
    ValidationResult::Valid => assert_true(false)
    ValidationResult::Invalid(msg) => assert_eq(msg, "Input is too long")
  }
  
  match validate_input("abcd", [ValidationRule::MaxLength(5)]) {
    ValidationResult::Valid => assert_true(true)
    ValidationResult::Invalid(_) => assert_true(false)
  }
  
  // Test pattern validation
  match validate_input("user@example.com", [ValidationRule::Pattern("email")]) {
    ValidationResult::Valid => assert_true(true)
    ValidationResult::Invalid(_) => assert_true(false)
  }
  
  match validate_input("invalid-email", [ValidationRule::Pattern("email")]) {
    ValidationResult::Valid => assert_true(false)
    ValidationResult::Invalid(msg) => assert_eq(msg, "Invalid email format")
  }
  
  // Test special characters validation
  match validate_input("normal", [ValidationRule::NoSpecialChars]) {
    ValidationResult::Valid => assert_true(true)
    ValidationResult::Invalid(_) => assert_true(false)
  }
  
  match validate_input("with<special", [ValidationRule::NoSpecialChars]) {
    ValidationResult::Valid => assert_true(false)
    ValidationResult::Invalid(msg) => assert_eq(msg, "Special characters not allowed")
  }
  
  // Test SQL injection validation
  match validate_input("normal query", [ValidationRule::NoSQLInjection]) {
    ValidationResult::Valid => assert_true(true)
    ValidationResult::Invalid(_) => assert_true(false)
  }
  
  match validate_input("DROP table", [ValidationRule::NoSQLInjection]) {
    ValidationResult::Valid => assert_true(false)
    ValidationResult::Invalid(msg) => assert_eq(msg, "Potential SQL injection detected")
  }
  
  match validate_input("'; DROP TABLE users; --", [ValidationRule::NoSQLInjection]) {
    ValidationResult::Valid => assert_true(false)
    ValidationResult::Invalid(msg) => assert_eq(msg, "Potential SQL injection detected")
  }
  
  // Test XSS validation
  match validate_input("normal text", [ValidationRule::NoXSS]) {
    ValidationResult::Valid => assert_true(true)
    ValidationResult::Invalid(_) => assert_true(false)
  }
  
  match validate_input("<script>alert('xss')</script>", [ValidationRule::NoXSS]) {
    ValidationResult::Valid => assert_true(false)
    ValidationResult::Invalid(msg) => assert_eq(msg, "Potential XSS attack detected")
  }
  
  match validate_input("javascript:alert('xss')", [ValidationRule::NoXSS]) {
    ValidationResult::Valid => assert_true(false)
    ValidationResult::Invalid(msg) => assert_eq(msg, "Potential XSS attack detected")
  }
  
  // Test string sanitization
  assert_eq(sanitize_string("normal"), "normal")
  assert_eq(sanitize_string("with<brackets"), "with&lt;brackets")
  assert_eq(sanitize_string("with>brackets"), "with&gt;brackets")
  assert_eq(sanitize_string("with\"quotes\""), "with&quot;quotes&quot;")
  assert_eq(sanitize_string("with'apostrophe'"), "with&#x27;apostrophe&#x27;")
  assert_eq(sanitize_string("with&ampersand"), "with&amp;ampersand")
  
  // Test combined validation
  let rules = [
    ValidationRule::Required,
    ValidationRule::MinLength(3),
    ValidationRule::MaxLength(20),
    ValidationRule::NoSpecialChars
  ]
  
  match validate_input("ab", rules) {
    ValidationResult::Valid => assert_true(false)
    ValidationResult::Invalid(msg) => assert_eq(msg, "Input is too short")
  }
  
  match validate_input("a very long string that exceeds the maximum length", rules) {
    ValidationResult::Valid => assert_true(false)
    ValidationResult::Invalid(msg) => assert_eq(msg, "Input is too long")
  }
  
  match validate_input("valid_input", rules) {
    ValidationResult::Valid => assert_true(true)
    ValidationResult::Invalid(_) => assert_true(false)
  }
  
  match validate_input("invalid<input", rules) {
    ValidationResult::Valid => assert_true(false)
    ValidationResult::Invalid(msg) => assert_eq(msg, "Special characters not allowed")
  }
}

// Test 2: Password Security
test "password security and hashing" {
  type PasswordStrength {
    Weak
    Fair
    Good
    Strong
  }
  
  type PasswordPolicy {
    min_length : Int
    require_uppercase : Bool
    require_lowercase : Bool
    require_numbers : Bool
    require_special_chars : Bool
  }
  
  let create_password_policy = fn() : PasswordPolicy {
    {
      min_length: 8,
      require_uppercase: true,
      require_lowercase: true,
      require_numbers: true,
      require_special_chars: true
    }
  }
  
  let has_uppercase = fn(password : String) : Bool {
    for i in 0..<password.length() {
      let char = password.substring(i, 1)
      if char >= "A" && char <= "Z" {
        return true
      }
    }
    false
  }
  
  let has_lowercase = fn(password : String) : Bool {
    for i in 0..<password.length() {
      let char = password.substring(i, 1)
      if char >= "a" && char <= "z" {
        return true
      }
    }
    false
  }
  
  let has_numbers = fn(password : String) : Bool {
    for i in 0..<password.length() {
      let char = password.substring(i, 1)
      if char >= "0" && char <= "9" {
        return true
      }
    }
    false
  }
  
  let has_special_chars = fn(password : String) : Bool {
    for i in 0..<password.length() {
      let char = password.substring(i, 1)
      if !((char >= "a" && char <= "z") || (char >= "A" && char <= "Z") || (char >= "0" && char <= "9")) {
        return true
      }
    }
    false
  }
  
  let check_password_policy = fn(password : String, policy : PasswordPolicy) : Bool {
    if password.length() < policy.min_length {
      return false
    }
    
    if policy.require_uppercase && !has_uppercase(password) {
      return false
    }
    
    if policy.require_lowercase && !has_lowercase(password) {
      return false
    }
    
    if policy.require_numbers && !has_numbers(password) {
      return false
    }
    
    if policy.require_special_chars && !has_special_chars(password) {
      return false
    }
    
    true
  }
  
  let calculate_password_strength = fn(password : String) : PasswordStrength {
    let mut score = 0
    
    // Length contribution
    if password.length() >= 8 {
      score = score + 1
    }
    if password.length() >= 12 {
      score = score + 1
    }
    
    // Character variety contribution
    if has_lowercase(password) {
      score = score + 1
    }
    if has_uppercase(password) {
      score = score + 1
    }
    if has_numbers(password) {
      score = score + 1
    }
    if has_special_chars(password) {
      score = score + 1
    }
    
    if score <= 2 {
      PasswordStrength::Weak
    } else if score <= 4 {
      PasswordStrength::Fair
    } else if score <= 5 {
      PasswordStrength::Good
    } else {
      PasswordStrength::Strong
    }
  }
  
  let simple_hash = fn(password : String) : String {
    // Simplified hash function for testing (not secure for production)
    let mut hash = 0
    for i in 0..<password.length() {
      hash = hash + password.char_code_at(i) * (i + 1)
    }
    hash.to_string()
  }
  
  let verify_password = fn(password : String, stored_hash : String) : Bool {
    simple_hash(password) == stored_hash
  }
  
  // Test password policy
  let policy = create_password_policy()
  
  // Test valid passwords
  assert_true(check_password_policy("StrongP@ssw0rd", policy))
  assert_true(check_password_policy("MySecure123!", policy))
  
  // Test invalid passwords
  assert_false(check_password_policy("short", policy))  // Too short
  assert_false(check_password_policy("nouppercase1!", policy))  // No uppercase
  assert_false(check_password_policy("NOLOWERCASE1!", policy))  // No lowercase
  assert_false(check_password_policy("NoNumbers!", policy))  // No numbers
  assert_false(check_password_policy("NoSpecialChars1", policy))  // No special chars
  
  // Test password strength
  assert_eq(calculate_password_strength("password"), PasswordStrength::Weak)
  assert_eq(calculate_password_strength("Password1"), PasswordStrength::Fair)
  assert_eq(calculate_password_strength("Password1!"), PasswordStrength::Good)
  assert_eq(calculate_password_strength("Str0ngP@ssw0rd!"), PasswordStrength::Strong)
  
  // Test password hashing and verification
  let password = "MySecurePassword123!"
  let hash = simple_hash(password)
  
  assert_true(verify_password("MySecurePassword123!", hash))
  assert_false(verify_password("WrongPassword", hash))
  assert_false(verify_password("MySecurePassword123", hash))  // Missing !
  assert_false(verify_password("mySecurePassword123!", hash))  // Wrong case
}

// Test 3: Authentication and Authorization
test "authentication and authorization mechanisms" {
  type User {
    id : Int
    username : String
    password_hash : String
    roles : Array[String]
    is_active : Bool
  }
  
  type Permission {
    ReadUsers
    WriteUsers
    ReadData
    WriteData
    Admin
  }
  
  type AuthResult {
    Success(User)
    Failure(String)
  }
  
  type AuthzResult {
    Allowed
    Denied(String)
  }
  
  let create_user = fn(id : Int, username : String, password : String, roles : Array[String]) : User {
    {
      id: id,
      username: username,
      password_hash: simple_hash(password),
      roles: roles,
      is_active: true
    }
  }
  
  let simple_hash = fn(password : String) : String {
    // Simplified hash function for testing
    let mut hash = 0
    for i in 0..<password.length() {
      hash = hash + password.char_code_at(i) * (i + 1)
    }
    hash.to_string()
  }
  
  let authenticate = fn(users : Array[User>, username : String, password : String) : AuthResult {
    for user in users {
      if user.username == username {
        if !user.is_active {
          return AuthResult::Failure("User account is inactive")
        }
        
        if user.password_hash == simple_hash(password) {
          return AuthResult::Success(user)
        } else {
          return AuthResult::Failure("Invalid password")
        }
      }
    }
    AuthResult::Failure("User not found")
  }
  
  let has_permission = fn(user : User, permission : Permission) : AuthzResult {
    match permission {
      Permission::ReadUsers => {
        if user.roles.contains("admin") || user.roles.contains("user_manager") {
          AuthzResult::Allowed
        } else {
          AuthzResult::Denied("Insufficient permissions to read users")
        }
      }
      Permission::WriteUsers => {
        if user.roles.contains("admin") {
          AuthzResult::Allowed
        } else {
          AuthzResult::Denied("Insufficient permissions to write users")
        }
      }
      Permission::ReadData => {
        if user.roles.contains("admin") || user.roles.contains("user") || user.roles.contains("viewer") {
          AuthzResult::Allowed
        } else {
          AuthzResult::Denied("Insufficient permissions to read data")
        }
      }
      Permission::WriteData => {
        if user.roles.contains("admin") || user.roles.contains("user") {
          AuthzResult::Allowed
        } else {
          AuthzResult::Denied("Insufficient permissions to write data")
        }
      }
      Permission::Admin => {
        if user.roles.contains("admin") {
          AuthzResult::Allowed
        } else {
          AuthzResult::Denied("Admin access required")
        }
      }
    }
  }
  
  let deactivate_user = fn(users : Array[User>, user_id : Int) : Bool {
    for user in users {
      if user.id == user_id {
        user.is_active = false
        return true
      }
    }
    false
  }
  
  // Test authentication
  let users = [
    create_user(1, "admin", "admin123", ["admin"]),
    create_user(2, "user", "user123", ["user"]),
    create_user(3, "viewer", "viewer123", ["viewer"]),
    create_user(4, "manager", "manager123", ["user_manager"])
  ]
  
  // Test successful authentication
  match authenticate(users, "admin", "admin123") {
    AuthResult::Success(user) => {
      assert_eq(user.id, 1)
      assert_eq(user.username, "admin")
      assert_true(user.roles.contains("admin"))
      assert_true(user.is_active)
    }
    AuthResult::Failure(_) => assert_true(false)
  }
  
  match authenticate(users, "user", "user123") {
    AuthResult::Success(user) => {
      assert_eq(user.id, 2)
      assert_eq(user.username, "user")
      assert_true(user.roles.contains("user"))
      assert_true(user.is_active)
    }
    AuthResult::Failure(_) => assert_true(false)
  }
  
  // Test failed authentication
  match authenticate(users, "nonexistent", "password") {
    AuthResult::Success(_) => assert_true(false)
    AuthResult::Failure(msg) => assert_eq(msg, "User not found")
  }
  
  match authenticate(users, "admin", "wrongpassword") {
    AuthResult::Success(_) => assert_true(false)
    AuthResult::Failure(msg) => assert_eq(msg, "Invalid password")
  }
  
  // Test authentication with inactive user
  assert_true(deactivate_user(users, 3))
  
  match authenticate(users, "viewer", "viewer123") {
    AuthResult::Success(_) => assert_true(false)
    AuthResult::Failure(msg) => assert_eq(msg, "User account is inactive")
  }
  
  // Test authorization
  match authenticate(users, "admin", "admin123") {
    AuthResult::Success(admin_user) => {
      // Admin should have all permissions
      match has_permission(admin_user, Permission::ReadUsers) {
        AuthzResult::Allowed => assert_true(true)
        AuthzResult::Denied(_) => assert_true(false)
      }
      
      match has_permission(admin_user, Permission::WriteUsers) {
        AuthzResult::Allowed => assert_true(true)
        AuthzResult::Denied(_) => assert_true(false)
      }
      
      match has_permission(admin_user, Permission::ReadData) {
        AuthzResult::Allowed => assert_true(true)
        AuthzResult::Denied(_) => assert_true(false)
      }
      
      match has_permission(admin_user, Permission::WriteData) {
        AuthzResult::Allowed => assert_true(true)
        AuthzResult::Denied(_) => assert_true(false)
      }
      
      match has_permission(admin_user, Permission::Admin) {
        AuthzResult::Allowed => assert_true(true)
        AuthzResult::Denied(_) => assert_true(false)
      }
    }
    AuthResult::Failure(_) => assert_true(false)
  }
  
  match authenticate(users, "user", "user123") {
    AuthResult::Success(user_user) => {
      // Regular user should have limited permissions
      match has_permission(user_user, Permission::ReadUsers) {
        AuthzResult::Allowed => assert_true(false)
        AuthzResult::Denied(_) => assert_true(true)
      }
      
      match has_permission(user_user, Permission::WriteUsers) {
        AuthzResult::Allowed => assert_true(false)
        AuthzResult::Denied(_) => assert_true(true)
      }
      
      match has_permission(user_user, Permission::ReadData) {
        AuthzResult::Allowed => assert_true(true)
        AuthzResult::Denied(_) => assert_true(false)
      }
      
      match has_permission(user_user, Permission::WriteData) {
        AuthzResult::Allowed => assert_true(true)
        AuthzResult::Denied(_) => assert_true(false)
      }
      
      match has_permission(user_user, Permission::Admin) {
        AuthzResult::Allowed => assert_true(false)
        AuthzResult::Denied(_) => assert_true(true)
      }
    }
    AuthResult::Failure(_) => assert_true(false)
  }
  
  match authenticate(users, "manager", "manager123") {
    AuthResult::Success(manager_user) => {
      // Manager should have specific permissions
      match has_permission(manager_user, Permission::ReadUsers) {
        AuthzResult::Allowed => assert_true(true)
        AuthzResult::Denied(_) => assert_true(false)
      }
      
      match has_permission(manager_user, Permission::WriteUsers) {
        AuthzResult::Allowed => assert_true(false)
        AuthzResult::Denied(_) => assert_true(true)
      }
      
      match has_permission(manager_user, Permission::ReadData) {
        AuthzResult::Allowed => assert_true(false)
        AuthzResult::Denied(_) => assert_true(true)
      }
      
      match has_permission(manager_user, Permission::WriteData) {
        AuthzResult::Allowed => assert_true(false)
        AuthzResult::Denied(_) => assert_true(true)
      }
      
      match has_permission(manager_user, Permission::Admin) {
        AuthzResult::Allowed => assert_true(false)
        AuthzResult::Denied(_) => assert_true(true)
      }
    }
    AuthResult::Failure(_) => assert_true(false)
  }
}

// Test 4: Encryption and Decryption
test "encryption and decryption mechanisms" {
  type EncryptionResult {
    Success(String)
    Failure(String)
  }
  
  type DecryptionResult {
    Success(String)
    Failure(String)
  }
  
  // Simple Caesar cipher for testing (not secure for production)
  let caesar_encrypt = fn(plaintext : String, shift : Int) : EncryptionResult {
    if plaintext.length() == 0 {
      return EncryptionResult::Success("")
    }
    
    let mut result = ""
    for i in 0..<plaintext.length() {
      let char = plaintext.substring(i, 1)
      
      if char >= "a" && char <= "z" {
        let char_code = char.char_code_at(0) - 97
        let shifted = (char_code + shift) % 26
        result = result + (shifted + 97).to_char()
      } else if char >= "A" && char <= "Z" {
        let char_code = char.char_code_at(0) - 65
        let shifted = (char_code + shift) % 26
        result = result + (shifted + 65).to_char()
      } else if char >= "0" && char <= "9" {
        let char_code = char.char_code_at(0) - 48
        let shifted = (char_code + shift) % 10
        result = result + (shifted + 48).to_char()
      } else {
        result = result + char
      }
    }
    
    EncryptionResult::Success(result)
  }
  
  let caesar_decrypt = fn(ciphertext : String, shift : Int) : DecryptionResult {
    if ciphertext.length() == 0 {
      return DecryptionResult::Success("")
    }
    
    let mut result = ""
    for i in 0..<ciphertext.length() {
      let char = ciphertext.substring(i, 1)
      
      if char >= "a" && char <= "z" {
        let char_code = char.char_code_at(0) - 97
        let shifted = (char_code - shift + 26) % 26
        result = result + (shifted + 97).to_char()
      } else if char >= "A" && char <= "Z" {
        let char_code = char.char_code_at(0) - 65
        let shifted = (char_code - shift + 26) % 26
        result = result + (shifted + 65).to_char()
      } else if char >= "0" && char <= "9" {
        let char_code = char.char_code_at(0) - 48
        let shifted = (char_code - shift + 10) % 10
        result = result + (shifted + 48).to_char()
      } else {
        result = result + char
      }
    }
    
    DecryptionResult::Success(result)
  }
  
  // Simple XOR cipher for testing (not secure for production)
  let xor_encrypt = fn(plaintext : String, key : String) : EncryptionResult {
    if key.length() == 0 {
      return EncryptionResult::Failure("Key cannot be empty")
    }
    
    let mut result = ""
    for i in 0..<plaintext.length() {
      let plain_char = plaintext.char_code_at(i)
      let key_char = key.char_code_at(i % key.length())
      let encrypted_char = plain_char ^ key_char
      result = result + encrypted_char.to_string() + ","
    }
    
    // Remove trailing comma
    if result.length() > 0 {
      result = result.substring(0, result.length() - 1)
    }
    
    EncryptionResult::Success(result)
  }
  
  let xor_decrypt = fn(ciphertext : String, key : String) : DecryptionResult {
    if key.length() == 0 {
      return DecryptionResult::Failure("Key cannot be empty")
    }
    
    if ciphertext.length() == 0 {
      return DecryptionResult::Success("")
    }
    
    let encrypted_values = ciphertext.split(",")
    let mut result = ""
    
    for i in 0..<encrypted_values.length() {
      match encrypted_values[i].to_int() {
        Some(encrypted_char) => {
          let key_char = key.char_code_at(i % key.length())
          let decrypted_char = encrypted_char ^ key_char
          result = result + decrypted_char.to_char()
        }
        None => return DecryptionResult::Failure("Invalid ciphertext format")
      }
    }
    
    DecryptionResult::Success(result)
  }
  
  // Test Caesar cipher
  match caesar_encrypt("", 3) {
    EncryptionResult::Success(ciphertext) => assert_eq(ciphertext, "")
    EncryptionResult::Failure(_) => assert_true(false)
  }
  
  match caesar_encrypt("Hello", 3) {
    EncryptionResult::Success(ciphertext) => assert_eq(ciphertext, "Khoor")
    EncryptionResult::Failure(_) => assert_true(false)
  }
  
  match caesar_encrypt("World123", 5) {
    EncryptionResult::Success(ciphertext) => assert_eq(ciphertext, "Btwqi678")
    EncryptionResult::Failure(_) => assert_true(false)
  }
  
  match caesar_decrypt("Khoor", 3) {
    DecryptionResult::Success(plaintext) => assert_eq(plaintext, "Hello")
    DecryptionResult::Failure(_) => assert_true(false)
  }
  
  match caesar_decrypt("Btwqi678", 5) {
    DecryptionResult::Success(plaintext) => assert_eq(plaintext, "World123")
    DecryptionResult::Failure(_) => assert_true(false)
  }
  
  // Test round-trip encryption/decryption
  let test_strings = ["", "a", "Hello", "World123", "Special!@#"]
  
  for test_str in test_strings {
    match caesar_encrypt(test_str, 7) {
      EncryptionResult::Success(ciphertext) => {
        match caesar_decrypt(ciphertext, 7) {
          DecryptionResult::Success(decrypted) => assert_eq(decrypted, test_str)
          DecryptionResult::Failure(_) => assert_true(false)
        }
      }
      EncryptionResult::Failure(_) => assert_true(false)
    }
  }
  
  // Test XOR cipher
  match xor_encrypt("", "key") {
    EncryptionResult::Success(ciphertext) => assert_eq(ciphertext, "")
    EncryptionResult::Failure(_) => assert_true(false)
  }
  
  match xor_encrypt("Hello", "key") {
    EncryptionResult::Success(ciphertext) => {
      // Verify it's not the same as plaintext
      assert_true(ciphertext != "Hello")
      assert_true(ciphertext.contains(","))
    }
    EncryptionResult::Failure(_) => assert_true(false)
  }
  
  match xor_encrypt("Hello", "") {
    EncryptionResult::Success(_) => assert_true(false)
    EncryptionResult::Failure(msg) => assert_eq(msg, "Key cannot be empty")
  }
  
  match xor_decrypt("", "key") {
    DecryptionResult::Success(plaintext) => assert_eq(plaintext, "")
    DecryptionResult::Failure(_) => assert_true(false)
  }
  
  match xor_decrypt("invalid", "key") {
    DecryptionResult::Success(_) => assert_true(false)
    DecryptionResult::Failure(msg) => assert_eq(msg, "Invalid ciphertext format")
  }
  
  match xor_decrypt("invalid", "") {
    DecryptionResult::Success(_) => assert_true(false)
    DecryptionResult::Failure(msg) => assert_eq(msg, "Key cannot be empty")
  }
  
  // Test XOR round-trip encryption/decryption
  for test_str in test_strings {
    match xor_encrypt(test_str, "secretkey") {
      EncryptionResult::Success(ciphertext) => {
        match xor_decrypt(ciphertext, "secretkey") {
          DecryptionResult::Success(decrypted) => assert_eq(decrypted, test_str)
          DecryptionResult::Failure(_) => assert_true(false)
        }
      }
      EncryptionResult::Failure(_) => assert_true(false)
    }
  }
  
  // Test that different keys produce different ciphertexts
  match xor_encrypt("Hello", "key1") {
    EncryptionResult::Success(ciphertext1) => {
      match xor_encrypt("Hello", "key2") {
        EncryptionResult::Success(ciphertext2) => {
          assert_true(ciphertext1 != ciphertext2)
        }
        EncryptionResult::Failure(_) => assert_true(false)
      }
    }
    EncryptionResult::Failure(_) => assert_true(false)
  }
  
  // Test that wrong key fails to decrypt
  match xor_encrypt("Secret", "key") {
    EncryptionResult::Success(ciphertext) => {
      match xor_decrypt(ciphertext, "wrongkey") {
        DecryptionResult::Success(decrypted) => assert_true(decrypted != "Secret")
        DecryptionResult::Failure(_) => assert_true(false)
      }
    }
    EncryptionResult::Failure(_) => assert_true(false)
  }
}
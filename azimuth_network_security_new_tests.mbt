// Network Security Test Cases for Azimuth Telemetry System
// This file contains test cases for network security features

// Test 1: Input Validation and Sanitization
test "input validation and sanitization" {
  // Simple input validation functions
  let is_safe_string = fn(input: String) -> Bool {
    // Check for common injection patterns
    let dangerous_patterns = [
      "<script", "</script>", "javascript:", "onclick=",
      "onerror=", "onload=", "eval(", "expression(",
      "import ", "require(", "document.", "window.",
      "alert(", "confirm(", "prompt("
    ]
    
    for pattern in dangerous_patterns {
      if input.contains(pattern) {
        return false
      }
    }
    
    // Check for SQL injection patterns
    let sql_patterns = ["'", "\"", ";", "--", "/*", "*/", "xp_", "sp_"]
    for pattern in sql_patterns {
      if input.contains(pattern) {
        return false
      }
    }
    
    true
  }
  
  let sanitize_input = fn(input: String) -> String {
    // Replace dangerous characters
    let mut result = input
    result = result.replace("<", "&lt;")
    result = result.replace(">", "&gt;")
    result = result.replace("\"", "&quot;")
    result = result.replace("'", "&#x27;")
    result = result.replace("/", "&#x2F;")
    result
  }
  
  // Test safe inputs
  assert_true(is_safe_string("normal text"))
  assert_true(is_safe_string("user@example.com"))
  assert_true(is_safe_string("Hello, World!"))
  
  // Test dangerous inputs
  assert_false(is_safe_string("<script>alert('xss')</script>"))
  assert_false(is_safe_string("javascript:alert('xss')"))
  assert_false(is_safe_string("'; DROP TABLE users; --"))
  assert_false(is_safe_string("onclick=alert('xss')"))
  
  // Test sanitization
  let sanitized = sanitize_input("<script>alert('xss')</script>")
  assert_eq(sanitized, "&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;")
  
  let sanitized2 = sanitize_input("Hello <world> & \"friends\"")
  assert_eq(sanitized2, "Hello &lt;world&gt; &amp; &quot;friends&quot;")
}

// Test 2: Encryption and Decryption
test "encryption and decryption" {
  // Simple Caesar cipher for demonstration
  let caesar_encrypt = fn(text: String, shift: Int) -> String {
    let mut result = ""
    for char in text.to_char_array() {
      if char >= 'a' && char <= 'z' {
        let shifted = ((char.to_int() - 'a'.to_int() + shift) % 26) + 'a'.to_int()
        result = result + shifted.to_char().to_string()
      } else if char >= 'A' && char <= 'Z' {
        let shifted = ((char.to_int() - 'A'.to_int() + shift) % 26) + 'A'.to_int()
        result = result + shifted.to_char().to_string()
      } else {
        result = result + char.to_string()
      }
    }
    result
  }
  
  let caesar_decrypt = fn(text: String, shift: Int) -> String {
    caesar_encrypt(text, 26 - (shift % 26))
  }
  
  let plaintext = "Secret Message 123!"
  let shift = 5
  
  let encrypted = caesar_encrypt(plaintext, shift)
  let decrypted = caesar_decrypt(encrypted, shift)
  
  assert_eq(plaintext, decrypted)
  assert_eq(encrypted, "Xjhwjy Rjxxflj 123!")
  
  // Test with different shifts
  let encrypted2 = caesar_encrypt(plaintext, 13) // ROT13
  let decrypted2 = caesar_decrypt(encrypted2, 13)
  assert_eq(plaintext, decrypted2)
}

// Test 3: Hashing Functions
test "hashing functions" {
  // Simple hash function for demonstration
  let simple_hash = fn(input: String) -> Int {
    let mut hash = 5381
    for char in input.to_char_array() {
      hash = ((hash << 5) + hash) + char.to_int()
    }
    hash
  }
  
  // Test hash consistency
  let input1 = "test string"
  let hash1 = simple_hash(input1)
  let hash1_again = simple_hash(input1)
  assert_eq(hash1, hash1_again)
  
  // Test hash uniqueness
  let input2 = "different string"
  let hash2 = simple_hash(input2)
  assert_true(hash1 != hash2)
  
  // Test hash properties
  let input3 = "Test String" // Different case
  let hash3 = simple_hash(input3)
  assert_true(hash1 != hash3) // Case-sensitive
  
  // Test empty string
  let empty_hash = simple_hash("")
  assert_true(empty_hash != 0)
}

// Test 4: Token Generation and Validation
test "token generation and validation" {
  type Token {
    value : String
    expires : Int
    user_id : String
  }
  
  let generate_token = fn(user_id: String, current_time: Int, ttl: Int) -> Token {
    // Simple token generation (in real systems, use proper cryptographic functions)
    let random_part = "random" + current_time.to_string()
    let token_value = user_id + ":" + random_part + ":" + (current_time + ttl).to_string()
    
    Token {
      value: token_value,
      expires: current_time + ttl,
      user_id: user_id
    }
  }
  
  let validate_token = fn(token: Token, current_time: Int) -> Bool {
    current_time < token.expires
  }
  
  let extract_user_id = fn(token_value: String) -> String {
    let parts = token_value.split(":")
    if parts.length() > 0 {
      parts[0]
    } else {
      ""
    }
  }
  
  let current_time = 1000
  let ttl = 3600 // 1 hour
  
  let token = generate_token("user123", current_time, ttl)
  assert_eq(token.user_id, "user123")
  assert_eq(token.expires, current_time + ttl)
  
  // Token should be valid initially
  assert_true(validate_token(token, current_time))
  assert_true(validate_token(token, current_time + 1800)) // 30 minutes later
  
  // Token should be invalid after expiration
  assert_false(validate_token(token, current_time + ttl + 1))
  
  // Extract user ID from token value
  let extracted_user_id = extract_user_id(token.value)
  assert_eq(extracted_user_id, "user123")
}

// Test 5: Rate Limiting
test "rate limiting" {
  type RateLimiter {
    requests : Array[Int] // Timestamps of requests
    max_requests : Int
    window_size : Int
  }
  
  let create_rate_limiter = fn(max_requests: Int, window_size: Int) -> RateLimiter {
    RateLimiter {
      requests: [],
      max_requests: max_requests,
      window_size: window_size
    }
  }
  
  let is_allowed = fn(limiter: RateLimiter, current_time: Int) -> (Bool, RateLimiter) {
    // Remove old requests outside the window
    let mut recent_requests = []
    for timestamp in limiter.requests {
      if current_time - timestamp < limiter.window_size {
        recent_requests.push(timestamp)
      }
    }
    
    // Check if under limit
    if recent_requests.length() < limiter.max_requests {
      recent_requests.push(current_time)
      (true, RateLimiter {
        requests: recent_requests,
        max_requests: limiter.max_requests,
        window_size: limiter.window_size
      })
    } else {
      (false, RateLimiter {
        requests: recent_requests,
        max_requests: limiter.max_requests,
        window_size: limiter.window_size
      })
    }
  }
  
  let limiter = create_rate_limiter(3, 60) // 3 requests per 60 seconds
  
  // First request should be allowed
  let (allowed1, limiter1) = is_allowed(limiter, 1000)
  assert_true(allowed1)
  assert_eq(limiter1.requests.length(), 1)
  
  // Second request should be allowed
  let (allowed2, limiter2) = is_allowed(limiter1, 1010)
  assert_true(allowed2)
  assert_eq(limiter2.requests.length(), 2)
  
  // Third request should be allowed
  let (allowed3, limiter3) = is_allowed(limiter2, 1020)
  assert_true(allowed3)
  assert_eq(limiter3.requests.length(), 3)
  
  // Fourth request should be denied
  let (allowed4, limiter4) = is_allowed(limiter3, 1030)
  assert_false(allowed4)
  assert_eq(limiter4.requests.length(), 3)
  
  // Request after window should be allowed
  let (allowed5, limiter5) = is_allowed(limiter4, 1100) // 100 seconds later
  assert_true(allowed5)
  assert_eq(limiter5.requests.length(), 1) // Only the current request
}

// Test 6: Access Control
test "access control" {
  type Permission {
    resource : String
    action : String
  }
  
  type User {
    id : String
    permissions : Array[Permission]
  }
  
  let has_permission = fn(user: User, resource: String, action: String) -> Bool {
    for permission in user.permissions {
      if permission.resource == resource && permission.action == action {
        return true
      }
    }
    false
  }
  
  let grant_permission = fn(user: User, resource: String, action: String) -> User {
    let new_permission = Permission {
      resource: resource,
      action: action
    }
    
    let mut new_permissions = user.permissions
    new_permissions.push(new_permission)
    
    User {
      id: user.id,
      permissions: new_permissions
    }
  }
  
  let revoke_permission = fn(user: User, resource: String, action: String) -> User {
    let mut new_permissions = []
    for permission in user.permissions {
      if !(permission.resource == resource && permission.action == action) {
        new_permissions.push(permission)
      }
    }
    
    User {
      id: user.id,
      permissions: new_permissions
    }
  }
  
  let user = User {
    id: "user123",
    permissions: [
      Permission { resource: "telemetry", action: "read" },
      Permission { resource: "metrics", action: "read" }
    ]
  }
  
  // Check existing permissions
  assert_true(has_permission(user, "telemetry", "read"))
  assert_true(has_permission(user, "metrics", "read"))
  assert_false(has_permission(user, "telemetry", "write"))
  assert_false(has_permission(user, "logs", "read"))
  
  // Grant new permission
  let user_with_write = grant_permission(user, "telemetry", "write")
  assert_true(has_permission(user_with_write, "telemetry", "write"))
  
  // Revoke permission
  let user_without_read = revoke_permission(user, "telemetry", "read")
  assert_false(has_permission(user_without_read, "telemetry", "read"))
  assert_true(has_permission(user_without_read, "metrics", "read"))
}

// Test 7: Secure Communication
test "secure communication" {
  type SecureMessage {
    content : String
    signature : String
    timestamp : Int
  }
  
  let simple_signature = fn(content: String, secret_key: String) -> String {
    // Simple signature (in real systems, use proper cryptographic signatures)
    let combined = content + secret_key
    let hash = combined.length() * 31 // Very simple hash
    hash.to_string()
  }
  
  let verify_signature = fn(message: SecureMessage, secret_key: String) -> Bool {
    let expected_signature = simple_signature(message.content, secret_key)
    message.signature == expected_signature
  }
  
  let create_secure_message = fn(content: String, secret_key: String, current_time: Int) -> SecureMessage {
    SecureMessage {
      content: content,
      signature: simple_signature(content, secret_key),
      timestamp: current_time
    }
  }
  
  let is_message_fresh = fn(message: SecureMessage, current_time: Int, max_age: Int) -> Bool {
    current_time - message.timestamp <= max_age
  }
  
  let secret_key = "my_secret_key"
  let current_time = 1000
  let max_age = 300 // 5 minutes
  
  let message = create_secure_message("Important telemetry data", secret_key, current_time)
  
  // Verify signature
  assert_true(verify_signature(message, secret_key))
  assert_false(verify_signature(message, "wrong_key"))
  
  // Check freshness
  assert_true(is_message_fresh(message, current_time, max_age))
  assert_true(is_message_fresh(message, current_time + 200, max_age))
  assert_false(is_message_fresh(message, current_time + 400, max_age))
  
  // Tampered message should fail verification
  let tampered_message = SecureMessage {
    content: "Tampered data",
    signature: message.signature,
    timestamp: message.timestamp
  }
  assert_false(verify_signature(tampered_message, secret_key))
}

// Test 8: Data Masking
test "data masking" {
  let mask_email = fn(email: String) -> String {
    let parts = email.split("@")
    if parts.length() != 2 {
      return "***@***.***"
    }
    
    let username = parts[0]
    let domain = parts[1]
    
    let mut masked_username = ""
    for i in 0..username.length() {
      if i == 0 || i == username.length() - 1 {
        masked_username = masked_username + username.substring(i, i + 1)
      } else {
        masked_username = masked_username + "*"
      }
    }
    
    let domain_parts = domain.split(".")
    if domain_parts.length() >= 2 {
      let masked_domain = "*" + "." + domain_parts[domain_parts.length() - 1]
      masked_username + "@" + masked_domain
    } else {
      masked_username + "@***"
    }
  }
  
  let mask_credit_card = fn(card_number: String) -> String {
    if card_number.length() < 4 {
      return "****"
    }
    
    let last_four = card_number.substring(card_number.length() - 4, card_number.length())
    let mut masked = ""
    for i in 0..(card_number.length() - 4) {
      masked = masked + "*"
    }
    masked + last_four
  }
  
  let mask_phone = fn(phone: String) -> String {
    if phone.length() < 4 {
      return "***-***-****"
    }
    
    let last_four = phone.substring(phone.length() - 4, phone.length())
    "***-***-" + last_four
  }
  
  // Test email masking
  assert_eq(mask_email("user@example.com"), "u***r@*.com")
  assert_eq(mask_email("john.doe@company.org"), "j*******e@*.org")
  assert_eq(mask_email("invalid"), "***@***.***")
  
  // Test credit card masking
  assert_eq(mask_credit_card("1234567890123456"), "************3456")
  assert_eq(mask_credit_card("4111111111111111"), "************1111")
  assert_eq(mask_credit_card("123"), "****")
  
  // Test phone masking
  assert_eq(mask_phone("1234567890"), "***-***-7890")
  assert_eq(mask_phone("5551234567"), "***-***-4567")
  assert_eq(mask_phone("123"), "***-***-****")
}
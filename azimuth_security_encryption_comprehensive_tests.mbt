// Azimuth Security and Encryption Comprehensive Test Suite
// This file contains comprehensive tests for security and encryption functionality in the Azimuth telemetry system

// Test 1: Data Encryption and Decryption
test "data encryption and decryption" {
  // Simple Caesar cipher for demonstration
  let caesar_encrypt = fn(text: String, shift: Int) {
    let mut result = ""
    
    for i in 0..text.length() {
      let char = text[i]
      if char >= 'a' and char <= 'z' {
        let shifted = ((char.to_int() - 'a'.to_int() + shift) % 26) + 'a'.to_int()
        result = result + shifted.to_char()
      } else if char >= 'A' and char <= 'Z' {
        let shifted = ((char.to_int() - 'A'.to_int() + shift) % 26) + 'A'.to_int()
        result = result + shifted.to_char()
      } else {
        result = result + char
      }
    }
    
    result
  }
  
  let caesar_decrypt = fn(text: String, shift: Int) {
    caesar_encrypt(text, 26 - (shift % 26))
  }
  
  // Test encryption and decryption
  let original_text = "Azimuth telemetry data trace-12345"
  let encrypted = caesar_encrypt(original_text, 5)
  let decrypted = caesar_decrypt(encrypted, 5)
  
  assert_eq(decrypted, original_text)
  assert_not_eq(encrypted, original_text)
  
  // Test with different shifts
  let encrypted_shift_10 = caesar_encrypt(original_text, 10)
  let decrypted_shift_10 = caesar_decrypt(encrypted_shift_10, 10)
  
  assert_eq(decrypted_shift_10, original_text)
  assert_not_eq(encrypted_shift_10, encrypted)
  
  // Test with numbers and special characters (should remain unchanged)
  let text_with_special = "Trace-ID: trace-12345, Status: OK"
  let encrypted_special = caesar_encrypt(text_with_special, 3)
  let decrypted_special = caesar_decrypt(encrypted_special, 3)
  
  assert_eq(decrypted_special, text_with_special)
  assert_true(encrypted_special.contains("12345"))
  assert_true(encrypted_special.contains(":"))
  assert_true(encrypted_special.contains(","))
}

// Test 2: Hash Function for Data Integrity
test "hash function for data integrity" {
  // Simple hash function for demonstration
  let simple_hash = fn(data: String) {
    let mut hash = 5381
    
    for i in 0..data.length() {
      hash = ((hash << 5) + hash) + data[i].to_int()
    }
    
    // Ensure positive result
    if hash < 0 {
      hash = -hash
    }
    
    hash
  }
  
  // Test hash consistency
  let test_data = "azimuth_telemetry_trace_data"
  let hash1 = simple_hash(test_data)
  let hash2 = simple_hash(test_data)
  
  assert_eq(hash1, hash2)
  
  // Test hash uniqueness
  let different_data = "azimuth_telemetry_span_data"
  let hash3 = simple_hash(different_data)
  
  assert_not_eq(hash1, hash3)
  
  // Test hash with different lengths
  let short_data = "abc"
  let long_data = "abcdefghijklmnopqrstuvwxyz"
  
  let hash_short = simple_hash(short_data)
  let hash_long = simple_hash(long_data)
  
  assert_not_eq(hash_short, hash_long)
  
  // Test hash for telemetry data integrity verification
  let telemetry_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    operation: "database_query",
    duration: 250,
    status: "ok"
  }
  
  let data_string = telemetry_data.trace_id + "|" + 
                   telemetry_data.span_id + "|" + 
                   telemetry_data.operation + "|" + 
                   telemetry_data.duration.to_string() + "|" + 
                   telemetry_data.status
  
  let original_hash = simple_hash(data_string)
  
  // Simulate data transmission
  let received_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    operation: "database_query",
    duration: 250,
    status: "ok"
  }
  
  let received_string = received_data.trace_id + "|" + 
                       received_data.span_id + "|" + 
                       received_data.operation + "|" + 
                       received_data.duration.to_string() + "|" + 
                       received_data.status
  
  let received_hash = simple_hash(received_string)
  
  assert_eq(original_hash, received_hash)
  
  // Test data tampering detection
  let tampered_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    operation: "database_query",
    duration: 300,  // Tampered duration
    status: "ok"
  }
  
  let tampered_string = tampered_data.trace_id + "|" + 
                       tampered_data.span_id + "|" + 
                       tampered_data.operation + "|" + 
                       tampered_data.duration.to_string() + "|" + 
                       tampered_data.status
  
  let tampered_hash = simple_hash(tampered_string)
  
  assert_not_eq(original_hash, tampered_hash)
}

// Test 3: Token-based Authentication
test "token-based authentication" {
  // Simple token generation and validation
  let generate_token = fn(user_id: String, timestamp: Int, secret: String) {
    let base_data = user_id + ":" + timestamp.to_string() + ":" + secret
    let hash = simple_hash(base_data)
    user_id + ":" + timestamp.to_string() + ":" + hash.to_string()
  }
  
  let validate_token = fn(token: String, secret: String, max_age: Int) {
    let parts = token.split(":")
    if parts.length() != 3 {
      return false
    }
    
    let user_id = parts[0]
    let timestamp_str = parts[1]
    let hash_str = parts[2]
    
    // Parse timestamp
    let mut timestamp = 0
    for i in 0..timestamp_str.length() {
      let char = timestamp_str[i]
      if char >= '0' and char <= '9' {
        timestamp = timestamp * 10 + (char.to_int() - '0'.to_int())
      } else {
        return false
      }
    }
    
    // Check token age
    let current_time = 1640999999  // Simulated current time
    if current_time - timestamp > max_age {
      return false
    }
    
    // Re-generate and compare hash
    let expected_token = generate_token(user_id, timestamp, secret)
    let expected_parts = expected_token.split(":")
    let expected_hash = expected_parts[2]
    
    hash_str == expected_hash
  }
  
  // Helper function from previous test
  let simple_hash = fn(data: String) {
    let mut hash = 5381
    
    for i in 0..data.length() {
      hash = ((hash << 5) + hash) + data[i].to_int()
    }
    
    if hash < 0 {
      hash = -hash
    }
    
    hash
  }
  
  // Test token generation and validation
  let user_id = "user-12345"
  let timestamp = 1640995200
  let secret = "azimuth-secret-key"
  
  let token = generate_token(user_id, timestamp, secret)
  assert_true(validate_token(token, secret, 3600))  // Valid for 1 hour
  
  // Test token with invalid secret
  assert_false(validate_token(token, "wrong-secret", 3600))
  
  // Test expired token
  let old_timestamp = 1640990000  // Very old timestamp
  let expired_token = generate_token(user_id, old_timestamp, secret)
  assert_false(validate_token(expired_token, secret, 3600))
  
  // Test token tampering
  let parts = token.split(":")
  let tampered_token = parts[0] + ":" + parts[1] + ":999999"  // Wrong hash
  assert_false(validate_token(tampered_token, secret, 3600))
  
  // Test token parsing
  let parse_token = fn(token: String) {
    let parts = token.split(":")
    if parts.length() != 3 {
      None
    } else {
      Some({
        user_id: parts[0],
        timestamp: parts[1],
        hash: parts[2]
      })
    }
  }
  
  match parse_token(token) {
    Some(parsed) => {
      assert_eq(parsed.user_id, user_id)
      assert_eq(parsed.timestamp, timestamp.to_string())
    }
    None => assert_true(false)
  }
  
  match parse_token("invalid-token") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 4: API Key Management
test "api key management" {
  // API key structure and validation
  type ApiKey = {
    key_id: String,
    key_hash: String,
    permissions: Array[String],
    created_at: Int,
    expires_at: Option[Int],
    is_active: Bool
  }
  
  let generate_api_key = fn(key_id: String, permissions: Array[String], expires_in: Option[Int]) {
    let current_time = 1640995200
    let expires_at = match expires_in {
      Some(seconds) => Some(current_time + seconds)
      None => None
    }
    
    let key_data = key_id + ":" + current_time.to_string() + ":azimuth-api-secret"
    let key_hash = simple_hash(key_data).to_string()
    
    {
      key_id,
      key_hash,
      permissions,
      created_at: current_time,
      expires_at,
      is_active: true
    }
  }
  
  let validate_api_key = fn(api_key: ApiKey, provided_hash: String, required_permission: String) {
    // Check if key is active
    if not(api_key.is_active) {
      return false
    }
    
    // Check expiration
    let current_time = 1640999999
    match api_key.expires_at {
      Some(expiry) => {
        if current_time > expiry {
          return false
        }
      }
      None => {}
    }
    
    // Check hash match
    if api_key.key_hash != provided_hash {
      return false
    }
    
    // Check permissions
    api_key.permissions.contains(required_permission)
  }
  
  // Helper function from previous tests
  let simple_hash = fn(data: String) {
    let mut hash = 5381
    
    for i in 0..data.length() {
      hash = ((hash << 5) + hash) + data[i].to_int()
    }
    
    if hash < 0 {
      hash = -hash
    }
    
    hash
  }
  
  // Test API key generation
  let api_key = generate_api_key(
    "key-12345",
    ["telemetry.read", "telemetry.write"],
    Some(86400)  // 24 hours
  )
  
  assert_eq(api_key.key_id, "key-12345")
  assert_eq(api_key.permissions.length(), 2)
  assert_true(api_key.permissions.contains("telemetry.read"))
  assert_true(api_key.permissions.contains("telemetry.write"))
  assert_true(api_key.is_active)
  
  match api_key.expires_at {
    Some(expiry) => assert_eq(expiry, 1640995200 + 86400)
    None => assert_true(false)
  }
  
  // Test API key validation
  let key_data = api_key.key_id + ":" + api_key.created_at.to_string() + ":azimuth-api-secret"
  let provided_hash = simple_hash(key_data).to_string()
  
  assert_true(validate_api_key(api_key, provided_hash, "telemetry.read"))
  assert_true(validate_api_key(api_key, provided_hash, "telemetry.write"))
  assert_false(validate_api_key(api_key, provided_hash, "admin.delete"))
  
  // Test with wrong hash
  assert_false(validate_api_key(api_key, "wrong-hash", "telemetry.read"))
  
  // Test with inactive key
  let inactive_key = { api_key | is_active: false }
  assert_false(validate_api_key(inactive_key, provided_hash, "telemetry.read"))
  
  // Test with expired key
  let expired_key = {
    key_id: "key-12345",
    key_hash: provided_hash,
    permissions: ["telemetry.read"],
    created_at: 1640995200,
    expires_at: Some(1640990000),  // Expired
    is_active: true
  }
  assert_false(validate_api_key(expired_key, provided_hash, "telemetry.read"))
  
  // Test key revocation
  let revoke_key = fn(key: ApiKey) {
    { key | is_active: false }
  }
  
  let revoked_key = revoke_key(api_key)
  assert_false(revoked_key.is_active)
  assert_false(validate_api_key(revoked_key, provided_hash, "telemetry.read"))
  
  // Test key permission update
  let update_permissions = fn(key: ApiKey, new_permissions: Array[String]) {
    { key | permissions: new_permissions }
  }
  
  let updated_key = update_permissions(api_key, ["telemetry.read"])
  assert_eq(updated_key.permissions.length(), 1)
  assert_true(updated_key.permissions.contains("telemetry.read"))
  assert_false(updated_key.permissions.contains("telemetry.write"))
}

// Test 5: Data Anonymization
test "data anonymization" {
  // PII (Personally Identifiable Information) anonymization
  let anonymize_email = fn(email: String) {
    let parts = email.split("@")
    if parts.length() != 2 {
      return "invalid-email"
    }
    
    let username = parts[0]
    let domain = parts[1]
    
    // Keep first 2 characters of username, replace rest with *
    let mut anonymized_username = ""
    for i in 0..username.length() {
      if i < 2 {
        anonymized_username = anonymized_username + username[i]
      } else {
        anonymized_username = anonymized_username + "*"
      }
    }
    
    anonymized_username + "@" + domain
  }
  
  let anonymize_ip = fn(ip: String) {
    let parts = ip.split(".")
    if parts.length() != 4 {
      return "invalid-ip"
    }
    
    // Keep first two octets, replace last two with *
    parts[0] + "." + parts[1] + ".*.*"
  }
  
  let anonymize_user_id = fn(user_id: String) {
    // Keep first and last 2 characters, replace middle with *
    if user_id.length() <= 4 {
      return "****"
    }
    
    let first = user_id[0]
    let last = user_id[user_id.length() - 1]
    let middle_length = user_id.length() - 2
    let mut middle = ""
    
    for i in 0..middle_length {
      middle = middle + "*"
    }
    
    first + middle + last
  }
  
  // Test email anonymization
  assert_eq(anonymize_email("john.doe@example.com"), "jo********@example.com")
  assert_eq(anonymize_email("a@short.com"), "a*@short.com")
  assert_eq(anonymize_email("invalid-email"), "invalid-email")
  
  // Test IP anonymization
  assert_eq(anonymize_ip("192.168.1.100"), "192.168.*.*")
  assert_eq(anonymize_ip("10.0.0.1"), "10.0.*.*")
  assert_eq(anonymize_ip("invalid-ip"), "invalid-ip")
  
  // Test user ID anonymization
  assert_eq(anonymize_user_id("user-12345"), "u******5")
  assert_eq(anonymize_user_id("abc"), "****")
  assert_eq(anonymize_user_id("abcd"), "a**d")
  
  // Test telemetry data anonymization
  let anonymize_telemetry_data = fn(data: {
    user_id: String,
    email: String,
    ip_address: String,
    trace_id: String,
    operation: String
  }) {
    {
      user_id: anonymize_user_id(data.user_id),
      email: anonymize_email(data.email),
      ip_address: anonymize_ip(data.ip_address),
      trace_id: data.trace_id,  // Not PII, keep as is
      operation: data.operation  // Not PII, keep as is
    }
  }
  
  let original_data = {
    user_id: "user-12345",
    email: "john.doe@example.com",
    ip_address: "192.168.1.100",
    trace_id: "trace-67890",
    operation: "database_query"
  }
  
  let anonymized_data = anonymize_telemetry_data(original_data)
  
  assert_eq(anonymized_data.user_id, "u******5")
  assert_eq(anonymized_data.email, "jo********@example.com")
  assert_eq(anonymized_data.ip_address, "192.168.*.*")
  assert_eq(anonymized_data.trace_id, "trace-67890")
  assert_eq(anonymized_data.operation, "database_query")
  
  // Verify PII is properly anonymized
  assert_not_eq(anonymized_data.user_id, original_data.user_id)
  assert_not_eq(anonymized_data.email, original_data.email)
  assert_not_eq(anonymized_data.ip_address, original_data.ip_address)
  
  // Verify non-PII data is preserved
  assert_eq(anonymized_data.trace_id, original_data.trace_id)
  assert_eq(anonymized_data.operation, original_data.operation)
}

// Test 6: Secure Configuration Management
test "secure configuration management" {
  // Secure configuration with encryption
  type SecureConfig = {
    database_url: String,
    api_key: String,
    encryption_key: String,
    is_encrypted: Bool
  }
  
  let encrypt_config_value = fn(value: String, key: String) {
    // Simple XOR encryption for demonstration
    let mut result = ""
    let key_length = key.length()
    
    for i in 0..value.length() {
      let value_char = value[i]
      let key_char = key[i % key_length]
      let encrypted_char = (value_char.to_int() ^ key_char.to_int()).to_char()
      result = result + encrypted_char
    }
    
    result
  }
  
  let decrypt_config_value = fn(encrypted_value: String, key: String) {
    // XOR decryption is the same as encryption
    encrypt_config_value(encrypted_value, key)
  }
  
  let create_secure_config = fn(database_url: String, api_key: String, encryption_key: String) {
    {
      database_url: encrypt_config_value(database_url, encryption_key),
      api_key: encrypt_config_value(api_key, encryption_key),
      encryption_key,
      is_encrypted: true
    }
  }
  
  let get_config_value = fn(config: SecureConfig, field_name: String) {
    let encrypted_value = match field_name {
      "database_url" => config.database_url
      "api_key" => config.api_key
      _ => ""
    }
    
    if config.is_encrypted {
      decrypt_config_value(encrypted_value, config.encryption_key)
    } else {
      encrypted_value
    }
  }
  
  // Test configuration encryption and decryption
  let original_db_url = "postgresql://user:password@localhost:5432/azimuth"
  let original_api_key = "azimuth-api-key-12345"
  let encryption_key = "secure-encryption-key"
  
  let secure_config = create_secure_config(original_db_url, original_api_key, encryption_key)
  
  assert_true(secure_config.is_encrypted)
  assert_not_eq(secure_config.database_url, original_db_url)
  assert_not_eq(secure_config.api_key, original_api_key)
  
  // Test decryption
  let decrypted_db_url = get_config_value(secure_config, "database_url")
  let decrypted_api_key = get_config_value(secure_config, "api_key")
  
  assert_eq(decrypted_db_url, original_db_url)
  assert_eq(decrypted_api_key, original_api_key)
  
  // Test with wrong encryption key
  let wrong_key_config = {
    database_url: secure_config.database_url,
    api_key: secure_config.api_key,
    encryption_key: "wrong-encryption-key",
    is_encrypted: true
  }
  
  let wrong_decrypted_db = get_config_value(wrong_key_config, "database_url")
  let wrong_decrypted_key = get_config_value(wrong_key_config, "api_key")
  
  assert_not_eq(wrong_decrypted_db, original_db_url)
  assert_not_eq(wrong_decrypted_key, original_api_key)
  
  // Test configuration rotation
  let rotate_encryption_key = fn(config: SecureConfig, new_key: String) {
    // First decrypt with old key
    let db_url = get_config_value(config, "database_url")
    let api_key = get_config_value(config, "api_key")
    
    // Then re-encrypt with new key
    {
      database_url: encrypt_config_value(db_url, new_key),
      api_key: encrypt_config_value(api_key, new_key),
      encryption_key: new_key,
      is_encrypted: true
    }
  }
  
  let new_encryption_key = "new-secure-encryption-key"
  let rotated_config = rotate_encryption_key(secure_config, new_encryption_key)
  
  assert_eq(rotated_config.encryption_key, new_encryption_key)
  assert_not_eq(rotated_config.database_url, secure_config.database_url)
  assert_not_eq(rotated_config.api_key, secure_config.api_key)
  
  // Verify rotation worked correctly
  let rotated_db_url = get_config_value(rotated_config, "database_url")
  let rotated_api_key = get_config_value(rotated_config, "api_key")
  
  assert_eq(rotated_db_url, original_db_url)
  assert_eq(rotated_api_key, original_api_key)
}

// Test 7: Access Control Lists (ACL)
test "access control lists" {
  // ACL implementation for telemetry resources
  type ACLRule = {
    principal: String,  // User or service ID
    resource: String,   // Resource pattern (e.g., "trace:*", "span:trace-123")
    action: String,     // "read", "write", "delete"
    effect: String      // "allow", "deny"
  }
  
  type ACLPolicy = {
    rules: Array[ACLRule],
    default_effect: String
  }
  
  let create_acl_rule = fn(principal: String, resource: String, action: String, effect: String) {
    {
      principal,
      resource,
      action,
      effect
    }
  }
  
  let create_acl_policy = fn(rules: Array[ACLRule], default_effect: String) {
    {
      rules,
      default_effect
    }
  }
  
  let check_access = fn(policy: ACLPolicy, principal: String, resource: String, action: String) {
    let mut allowed = policy.default_effect == "allow"
    
    // Check for matching rules (in order, last match wins)
    for rule in policy.rules {
      let principal_matches = rule.principal == "*" || rule.principal == principal
      let resource_matches = rule.resource == "*" || 
                            (rule.resource.ends_with("*") && resource.starts_with(rule.resource.substring(0, rule.resource.length() - 1))) ||
                            rule.resource == resource
      let action_matches = rule.action == "*" || rule.action == action
      
      if principal_matches && resource_matches && action_matches {
        allowed = rule.effect == "allow"
      }
    }
    
    allowed
  }
  
  // Create ACL rules
  let rules = [
    create_acl_rule("admin", "*", "*", "allow"),  // Admin can do everything
    create_acl_rule("service-azimuth", "trace:*", "write", "allow"),  // Service can write traces
    create_acl_rule("service-azimuth", "span:*", "write", "allow"),   // Service can write spans
    create_acl_rule("user-12345", "trace:trace-123", "read", "allow"),  // User can read specific trace
    create_acl_rule("user-12345", "trace:trace-456", "read", "allow"),  // User can read another trace
    create_acl_rule("user-67890", "trace:*", "read", "allow"),  // Another user can read all traces
    create_acl_rule("*", "*", "delete", "deny")  // Nobody can delete by default
  ]
  
  let policy = create_acl_policy(rules, "deny")  // Default deny
  
  // Test admin access
  assert_true(check_access(policy, "admin", "trace:any-trace", "read"))
  assert_true(check_access(policy, "admin", "trace:any-trace", "write"))
  assert_true(check_access(policy, "admin", "trace:any-trace", "delete"))
  
  // Test service access
  assert_true(check_access(policy, "service-azimuth", "trace:trace-123", "write"))
  assert_true(check_access(policy, "service-azimuth", "span:span-456", "write"))
  assert_false(check_access(policy, "service-azimuth", "trace:trace-123", "delete"))
  assert_false(check_access(policy, "service-azimuth", "metric:metric-789", "write"))
  
  // Test user access
  assert_true(check_access(policy, "user-12345", "trace:trace-123", "read"))
  assert_true(check_access(policy, "user-12345", "trace:trace-456", "read"))
  assert_false(check_access(policy, "user-12345", "trace:trace-789", "read"))
  assert_false(check_access(policy, "user-12345", "trace:trace-123", "write"))
  
  // Test another user with broader access
  assert_true(check_access(policy, "user-67890", "trace:trace-123", "read"))
  assert_true(check_access(policy, "user-67890", "trace:trace-456", "read"))
  assert_true(check_access(policy, "user-67890", "trace:trace-789", "read"))
  assert_false(check_access(policy, "user-67890", "trace:trace-123", "write"))
  
  // Test delete restrictions
  assert_false(check_access(policy, "admin", "trace:any-trace", "delete"))  // Overridden by deny rule
  assert_false(check_access(policy, "user-12345", "trace:trace-123", "delete"))
  assert_false(check_access(policy, "service-azimuth", "trace:trace-123", "delete"))
  
  // Test unknown user (default deny)
  assert_false(check_access(policy, "unknown-user", "trace:trace-123", "read"))
  assert_false(check_access(policy, "unknown-user", "trace:trace-123", "write"))
  
  // Test ACL rule addition
  let add_rule = fn(policy: ACLPolicy, rule: ACLRule) {
    { policy | rules: policy.rules.push(rule) }
  }
  
  let updated_policy = add_rule(policy, create_acl_rule("user-12345", "trace:*", "write", "allow"))
  assert_true(check_access(updated_policy, "user-12345", "trace:trace-789", "write"))
  assert_false(check_access(policy, "user-12345", "trace:trace-789", "write"))  // Original policy unchanged
  
  // Test ACL rule removal
  let remove_rule = fn(policy: ACLPolicy, principal: String, resource: String, action: String) {
    let mut filtered_rules = []
    for rule in policy.rules {
      if not(rule.principal == principal && rule.resource == resource && rule.action == action) {
        filtered_rules = filtered_rules.push(rule)
      }
    }
    { policy | rules: filtered_rules }
  }
  
  let restricted_policy = remove_rule(updated_policy, "user-12345", "trace:*", "write")
  assert_false(check_access(restricted_policy, "user-12345", "trace:trace-789", "write"))
}

// Test 8: Secure Logging with Sensitive Data Filtering
test "secure logging with sensitive data filtering" {
  // Secure logging with sensitive data redaction
  type LogLevel = 
    | Trace
    | Debug
    | Info
    | Warn
    | Error
  
  type LogEntry = {
    timestamp: Int,
    level: LogLevel,
    message: String,
    sensitive_fields: Array[String]
  }
  
  let redact_sensitive_data = fn(message: String, sensitive_patterns: Array[String]) {
    let mut result = message
    
    for pattern in sensitive_patterns {
      // Simple pattern matching and replacement
      if pattern == "email" {
        // Redact email addresses
        let mut start = 0
        let mut found = true
        
        while found {
          let mut at_pos = -1
          let mut dot_pos = -1
          
          // Find @ and . after @
          for i in start..result.length() {
            if result[i] == '@' && at_pos == -1 {
              at_pos = i
            } else if result[i] == '.' && at_pos != -1 && i > at_pos + 1 {
              dot_pos = i
              break
            }
          }
          
          if at_pos != -1 && dot_pos != -1 {
            // Find start of email
            let mut email_start = at_pos
            while email_start > 0 && 
                  ((result[email_start - 1] >= 'a' && result[email_start - 1] <= 'z') ||
                   (result[email_start - 1] >= 'A' && result[email_start - 1] <= 'Z') ||
                   (result[email_start - 1] >= '0' && result[email_start - 1] <= '9') ||
                   result[email_start - 1] == '.' || result[email_start - 1] == '_') {
              email_start = email_start - 1
            }
            
            // Find end of email
            let mut email_end = dot_pos
            while email_end < result.length() && 
                  ((result[email_end] >= 'a' && result[email_end] <= 'z') ||
                   (result[email_end] >= 'A' && result[email_end] <= 'Z') ||
                   (result[email_end] >= '0' && result[email_end] <= '9')) {
              email_end = email_end + 1
            }
            
            // Replace with redacted version
            let email = result.substring(email_start, email_end - email_start)
            let redacted = "email@" + "domain.com"
            result = result.substring(0, email_start) + redacted + result.substring(email_end, result.length() - email_end)
            
            start = email_start + redacted.length()
          } else {
            found = false
          }
        }
      } else if pattern == "api_key" {
        // Redact API keys (alphanumeric strings of length 20+)
        let mut i = 0
        while i < result.length() {
          if result[i] >= 'a' && result[i] <= 'z' || 
             result[i] >= 'A' && result[i] <= 'Z' || 
             result[i] >= '0' && result[i] <= '9' {
            let mut key_start = i
            let mut key_length = 0
            
            while i < result.length() && 
                  (result[i] >= 'a' && result[i] <= 'z' || 
                   result[i] >= 'A' && result[i] <= 'Z' || 
                   result[i] >= '0' && result[i] <= '9') {
              key_length = key_length + 1
              i = i + 1
            }
            
            if key_length >= 20 {
              let key = result.substring(key_start, key_length)
              let redacted = "REDACTED-API-KEY"
              result = result.substring(0, key_start) + redacted + result.substring(i, result.length() - i)
              i = key_start + redacted.length()
            }
          } else {
            i = i + 1
          }
        }
      }
    }
    
    result
  }
  
  // Test sensitive data redaction
  let message_with_email = "User john.doe@example.com logged in with API key azimuth12345apikey67890"
  let sensitive_patterns = ["email", "api_key"]
  
  let redacted_message = redact_sensitive_data(message_with_email, sensitive_patterns)
  
  assert_true(redacted_message.contains("email@domain.com"))
  assert_true(redacted_message.contains("REDACTED-API-KEY"))
  assert_false(redacted_message.contains("john.doe@example.com"))
  assert_false(redacted_message.contains("azimuth12345apikey67890"))
  
  // Test log entry creation and redaction
  let create_log_entry = fn(level: LogLevel, message: String, sensitive_fields: Array[String]) {
    let current_time = 1640999999
    
    {
      timestamp: current_time,
      level,
      message,
      sensitive_fields
    }
  }
  
  let format_log_entry = fn(entry: LogEntry) {
    let level_str = match entry.level {
      Trace => "TRACE"
      Debug => "DEBUG"
      Info => "INFO"
      Warn => "WARN"
      Error => "ERROR"
    }
    
    let redacted_message = redact_sensitive_data(entry.message, entry.sensitive_fields)
    
    "[" + entry.timestamp.to_string() + "] " + level_str + ": " + redacted_message
  }
  
  // Test log entry formatting
  let log_entry = create_log_entry(
    Info,
    "User alice@example.com performed operation with key abcdef1234567890abcdef",
    ["email", "api_key"]
  )
  
  let formatted_log = format_log_entry(log_entry)
  
  assert_true(formatted_log.contains("[1640999999]"))
  assert_true(formatted_log.contains("INFO:"))
  assert_true(formatted_log.contains("email@domain.com"))
  assert_true(formatted_log.contains("REDACTED-API-KEY"))
  assert_false(formatted_log.contains("alice@example.com"))
  assert_false(formatted_log.contains("abcdef1234567890abcdef"))
  
  // Test log filtering
  type LogFilter = {
    min_level: LogLevel,
    include_patterns: Array[String],
    exclude_patterns: Array[String]
  }
  
  let should_include_log = fn(entry: LogEntry, filter: LogFilter) {
    // Check level
    let level_match = match entry.level {
      Trace => filter.min_level == Trace
      Debug => filter.min_level == Trace || filter.min_level == Debug
      Info => filter.min_level == Trace || filter.min_level == Debug || filter.min_level == Info
      Warn => filter.min_level == Trace || filter.min_level == Debug || filter.min_level == Info || filter.min_level == Warn
      Error => true  // Always include error logs
    }
    
    if not(level_match) {
      return false
    }
    
    // Check include patterns
    if filter.include_patterns.length() > 0 {
      let mut includes = false
      for pattern in filter.include_patterns {
        if entry.message.contains(pattern) {
          includes = true
          break
        }
      }
      if not(includes) {
        return false
      }
    }
    
    // Check exclude patterns
    for pattern in filter.exclude_patterns {
      if entry.message.contains(pattern) {
        return false
      }
    }
    
    true
  }
  
  // Test log filtering
  let filter = {
    min_level: Warn,
    include_patterns: ["telemetry", "azimuth"],
    exclude_patterns: ["debug"]
  }
  
  let error_entry = create_log_entry(Error, "Azimuth system error occurred", [])
  let warn_entry = create_log_entry(Warn, "Telemetry service warning", [])
  let info_entry = create_log_entry(Info, "Telemetry service info", [])
  let debug_entry = create_log_entry(Debug, "Azimuth debug info", [])
  
  assert_true(should_include_log(error_entry, filter))
  assert_true(should_include_log(warn_entry, filter))
  assert_false(should_include_log(info_entry, filter))  // Below min level
  assert_false(should_include_log(debug_entry, filter))  // Below min level
}

// Test 9: Secure Network Communication
test "secure network communication" {
  // Simulate secure network communication with certificate validation
  type Certificate = {
    subject: String,
    issuer: String,
    serial_number: String,
    not_before: Int,
    not_after: Int,
    public_key: String
  }
  
  type TLSHandshake = {
    client_certificate: Option[Certificate],
    server_certificate: Certificate,
    cipher_suite: String,
    protocol_version: String
  }
  
  let validate_certificate = fn(cert: Certificate, current_time: Int) {
    // Check certificate validity period
    if current_time < cert.not_before || current_time > cert.not_after {
      return false
    }
    
    // Check certificate fields (simplified)
    if cert.subject.length() == 0 || cert.issuer.length() == 0 {
      return false
    }
    
    if cert.serial_number.length() == 0 || cert.public_key.length() == 0 {
      return false
    }
    
    true
  }
  
  let create_certificate = fn(subject: String, issuer: String, valid_from: Int, valid_to: Int) {
    let serial_number = "SN-" + valid_from.to_string() + "-" + valid_to.to_string()
    let public_key = "PK-" + subject + "-" + serial_number
    
    {
      subject,
      issuer,
      serial_number,
      not_before: valid_from,
      not_after: valid_to,
      public_key
    }
  }
  
  let perform_tls_handshake = fn(server_cert: Certificate, client_cert: Option[Certificate], current_time: Int) {
    // Validate server certificate
    if not(validate_certificate(server_cert, current_time)) {
      return None
    }
    
    // Validate client certificate if provided
    match client_cert {
      Some(cert) => {
        if not(validate_certificate(cert, current_time)) {
          return None
        }
      }
      None => {}
    }
    
    // Simulate successful TLS handshake
    Some({
      client_certificate: client_cert,
      server_certificate: server_cert,
      cipher_suite: "TLS_AES_256_GCM_SHA384",
      protocol_version: "TLSv1.3"
    })
  }
  
  // Test certificate creation and validation
  let current_time = 1640999999
  let server_cert = create_certificate(
    "azimuth-server.example.com",
    "CA-Example-Root",
    1640995200,
    1641081600  // Valid for 24 hours
  )
  
  assert_true(validate_certificate(server_cert, current_time))
  
  // Test expired certificate
  let expired_cert = create_certificate(
    "expired-server.example.com",
    "CA-Example-Root",
    1640900000,
    1640903600  // Expired
  )
  
  assert_false(validate_certificate(expired_cert, current_time))
  
  // Test not-yet-valid certificate
  let future_cert = create_certificate(
    "future-server.example.com",
    "CA-Example-Root",
    1641081600,
    1641168000  // Not yet valid
  )
  
  assert_false(validate_certificate(future_cert, current_time))
  
  // Test TLS handshake with server certificate only
  match perform_tls_handshake(server_cert, None, current_time) {
    Some(handshake) => {
      assert_eq(handshake.cipher_suite, "TLS_AES_256_GCM_SHA384")
      assert_eq(handshake.protocol_version, "TLSv1.3")
      assert_eq(handshake.server_certificate.subject, "azimuth-server.example.com")
      match handshake.client_certificate {
        Some(_) => assert_true(false)
        None => assert_true(true)
      }
    }
    None => assert_true(false)
  }
  
  // Test TLS handshake with both server and client certificates
  let client_cert = create_certificate(
    "azimuth-client.example.com",
    "CA-Example-Client",
    1640995200,
    1641081600
  )
  
  match perform_tls_handshake(server_cert, Some(client_cert), current_time) {
    Some(handshake) => {
      assert_eq(handshake.cipher_suite, "TLS_AES_256_GCM_SHA384")
      assert_eq(handshake.protocol_version, "TLSv1.3")
      match handshake.client_certificate {
        Some(cert) => assert_eq(cert.subject, "azimuth-client.example.com")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test TLS handshake with invalid server certificate
  match perform_tls_handshake(expired_cert, None, current_time) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test TLS handshake with invalid client certificate
  match perform_tls_handshake(server_cert, Some(expired_cert), current_time) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test secure data transmission simulation
  let encrypt_transmission_data = fn(data: String, public_key: String) {
    // Simulate encryption using public key
    let mut encrypted = ""
    for i in 0..data.length() {
      let key_char = public_key[i % public_key.length()]
      let encrypted_char = (data[i].to_int() + key_char.to_int()).to_char()
      encrypted = encrypted + encrypted_char
    }
    encrypted
  }
  
  let decrypt_transmission_data = fn(encrypted_data: String, private_key: String) {
    // Simulate decryption using private key
    let mut decrypted = ""
    for i in 0..encrypted_data.length() {
      let key_char = private_key[i % private_key.length()]
      let decrypted_char = (encrypted_data[i].to_int() - key_char.to_int()).to_char()
      decrypted = decrypted + decrypted_char
    }
    decrypted
  }
  
  // Test secure data transmission
  let telemetry_data = "trace-12345:span-67890:database_query:250:ok"
  let public_key = server_cert.public_key
  let private_key = "PRIVATE-" + public_key
  
  let encrypted_data = encrypt_transmission_data(telemetry_data, public_key)
  let decrypted_data = decrypt_transmission_data(encrypted_data, private_key)
  
  assert_eq(decrypted_data, telemetry_data)
  assert_not_eq(encrypted_data, telemetry_data)
  
  // Test with wrong private key
  let wrong_private_key = "WRONG-PRIVATE-KEY"
  let wrong_decrypted = decrypt_transmission_data(encrypted_data, wrong_private_key)
  
  assert_not_eq(wrong_decrypted, telemetry_data)
}

// Test 10: Audit Trail and Security Events
test "audit trail and security events" {
  // Security event tracking and audit trail
  type SecurityEvent = {
    timestamp: Int,
    event_type: String,
    principal: String,
    resource: String,
    action: String,
    outcome: String,
    details: Array[(String, String)]
  }
  
  type AuditTrail = {
    events: Array[SecurityEvent],
    retention_period: Int
  }
  
  let create_security_event = fn(event_type: String, principal: String, resource: String, action: String, outcome: String, details: Array[(String, String)]) {
    let current_time = 1640999999
    
    {
      timestamp: current_time,
      event_type,
      principal,
      resource,
      action,
      outcome,
      details
    }
  }
  
  let add_audit_event = fn(trail: AuditTrail, event: SecurityEvent) {
    { trail | events: trail.events.push(event) }
  }
  
  let query_audit_trail = fn(trail: AuditTrail, filters: {
    principal: Option[String],
    resource: Option[String],
    action: Option[String],
    outcome: Option[String],
    from_time: Option[Int],
    to_time: Option[Int]
  }) {
    let mut filtered_events = []
    
    for event in trail.events {
      let matches_principal = match filters.principal {
        Some(p) => event.principal == p
        None => true
      }
      
      let matches_resource = match filters.resource {
        Some(r) => event.resource == r
        None => true
      }
      
      let matches_action = match filters.action {
        Some(a) => event.action == a
        None => true
      }
      
      let matches_outcome = match filters.outcome {
        Some(o) => event.outcome == o
        None => true
      }
      
      let matches_from_time = match filters.from_time {
        Some(t) => event.timestamp >= t
        None => true
      }
      
      let matches_to_time = match filters.to_time {
        Some(t) => event.timestamp <= t
        None => true
      }
      
      if matches_principal && matches_resource && matches_action && matches_outcome && matches_from_time && matches_to_time {
        filtered_events = filtered_events.push(event)
      }
    }
    
    filtered_events
  }
  
  let cleanup_old_events = fn(trail: AuditTrail, current_time: Int) {
    let mut filtered_events = []
    
    for event in trail.events {
      if current_time - event.timestamp <= trail.retention_period {
        filtered_events = filtered_events.push(event)
      }
    }
    
    { trail | events: filtered_events }
  }
  
  // Test audit trail creation and event addition
  let audit_trail = {
    events: [],
    retention_period: 7776000  // 90 days in seconds
  }
  
  let login_event = create_security_event(
    "authentication",
    "user-12345",
    "system",
    "login",
    "success",
    [("ip_address", "192.168.1.100"), ("user_agent", "Azimuth-Client/1.0")]
  )
  
  let data_access_event = create_security_event(
    "authorization",
    "user-12345",
    "trace:trace-67890",
    "read",
    "success",
    [("query", "SELECT * FROM traces WHERE id = 'trace-67890'")]
  )
  
  let failed_access_event = create_security_event(
    "authorization",
    "user-67890",
    "trace:trace-12345",
    "delete",
    "denied",
    [("reason", "insufficient_permissions")]
  )
  
  let updated_trail = audit_trail
    |> add_audit_event(login_event)
    |> add_audit_event(data_access_event)
    |> add_audit_event(failed_access_event)
  
  assert_eq(updated_trail.events.length(), 3)
  
  // Test audit trail queries
  let user_events = query_audit_trail(updated_trail, {
    principal: Some("user-12345"),
    resource: None,
    action: None,
    outcome: None,
    from_time: None,
    to_time: None
  })
  
  assert_eq(user_events.length(), 2)
  assert_eq(user_events[0].event_type, "authentication")
  assert_eq(user_events[1].event_type, "authorization")
  
  let delete_events = query_audit_trail(updated_trail, {
    principal: None,
    resource: None,
    action: Some("delete"),
    outcome: None,
    from_time: None,
    to_time: None
  })
  
  assert_eq(delete_events.length(), 1)
  assert_eq(delete_events[0].outcome, "denied")
  
  let success_events = query_audit_trail(updated_trail, {
    principal: None,
    resource: None,
    action: None,
    outcome: Some("success"),
    from_time: None,
    to_time: None
  })
  
  assert_eq(success_events.length(), 2)
  
  // Test audit trail cleanup
  let old_timestamp = 1640999999 - 7776000 - 1000  // Older than retention period
  let old_event = {
    timestamp: old_timestamp,
    event_type: "authentication",
    principal: "user-old",
    resource: "system",
    action: "login",
    outcome: "success",
    details: []
  }
  
  let trail_with_old_event = add_audit_event(updated_trail, old_event)
  assert_eq(trail_with_old_event.events.length(), 4)
  
  let cleaned_trail = cleanup_old_events(trail_with_old_event, 1640999999)
  assert_eq(cleaned_trail.events.length(), 3)  // Old event removed
  
  // Test security event analysis
  let detect_suspicious_activity = fn(trail: AuditTrail, time_window: Int, failure_threshold: Int) {
    let mut suspicious_principals = []
    let principal_failures = {}
    
    // Count failures per principal within time window
    for event in trail.events {
      if event.outcome == "denied" || event.outcome == "failure" {
        let current_count = match principal_failures[event.principal] {
          Some(count) => count
          None => 0
        }
        principal_failures = principal_failures.insert(event.principal, current_count + 1)
      }
    }
    
    // Check for suspicious activity
    for (principal, count) in principal_failures {
      if count >= failure_threshold {
        suspicious_principals = suspicious_principals.push({
          principal,
          failure_count: count,
          risk_level: if count >= failure_threshold * 2 { "high" } else { "medium" }
        })
      }
    }
    
    suspicious_principals
  }
  
  // Add more failure events for testing
  let more_failure_events = [
    create_security_event("authorization", "user-99999", "trace:trace-11111", "read", "denied", []),
    create_security_event("authorization", "user-99999", "trace:trace-22222", "read", "denied", []),
    create_security_event("authorization", "user-99999", "trace:trace-33333", "write", "denied", []),
    create_security_event("authorization", "user-99999", "trace:trace-44444", "delete", "denied", []),
    create_security_event("authorization", "user-99999", "trace:trace-55555", "read", "denied", [])
  ]
  
  let trail_with_failures = updated_trail
  let mut i = 0
  while i < more_failure_events.length() {
    trail_with_failures = add_audit_event(trail_with_failures, more_failure_events[i])
    i = i + 1
  }
  
  let suspicious_activity = detect_suspicious_activity(trail_with_failures, 3600, 3)
  
  assert_eq(suspicious_activity.length(), 1)
  assert_eq(suspicious_activity[0].principal, "user-99999")
  assert_eq(suspicious_activity[0].failure_count, 5)
  assert_eq(suspicious_activity[0].risk_level, "high")
}
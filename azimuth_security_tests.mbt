// Azimuth Telemetry System - Security Tests
// This file contains test cases for security features

// Test 1: Sensitive Data Redaction
test "sensitive data redaction" {
  // Test redaction of sensitive attributes
  let attributes = Attributes::new()
  
  // Add sensitive attributes
  Attributes::set(attributes, "user.password", StringValue("secret123"))
  Attributes::set(attributes, "api.key", StringValue("sk-1234567890abcdef"))
  Attributes::set(attributes, "credit.card", StringValue("4111-1111-1111-1111"))
  Attributes::set(attributes, "ssn", StringValue("123-45-6789"))
  Attributes::set(attributes, "email", StringValue("user@example.com"))
  Attributes::set(attributes, "phone", StringValue("555-123-4567"))
  
  // Add non-sensitive attributes
  Attributes::set(attributes, "user.name", StringValue("John Doe"))
  Attributes::set(attributes, "request.id", StringValue("req-123456"))
  Attributes::set(attributes, "operation.type", StringValue("login"))
  
  // Apply redaction
  let redacted_attributes = Security::redact_sensitive_attributes(attributes)
  
  // Verify sensitive attributes are redacted
  let redacted_password = Attributes::get(redacted_attributes, "user.password")
  match redacted_password {
    Some(StringValue(value)) => assert_eq(value, "[REDACTED]")
    _ => assert_true(false)
  }
  
  let redacted_api_key = Attributes::get(redacted_attributes, "api.key")
  match redacted_api_key {
    Some(StringValue(value)) => assert_eq(value, "[REDACTED]")
    _ => assert_true(false)
  }
  
  let redacted_credit_card = Attributes::get(redacted_attributes, "credit.card")
  match redacted_credit_card {
    Some(StringValue(value)) => assert_eq(value, "[REDACTED]")
    _ => assert_true(false)
  }
  
  let redacted_ssn = Attributes::get(redacted_attributes, "ssn")
  match redacted_ssn {
    Some(StringValue(value)) => assert_eq(value, "[REDACTED]")
    _ => assert_true(false)
  }
  
  // Verify non-sensitive attributes are preserved
  let preserved_name = Attributes::get(redacted_attributes, "user.name")
  match preserved_name {
    Some(StringValue(value)) => assert_eq(value, "John Doe")
    _ => assert_true(false)
  }
  
  let preserved_request_id = Attributes::get(redacted_attributes, "request.id")
  match preserved_request_id {
    Some(StringValue(value)) => assert_eq(value, "req-123456")
    _ => assert_true(false)
  }
  
  // Test redaction in span events
  let span_context = SpanContext::new("trace123", "span123", true, "redaction_test")
  let span = Span::new("redaction_test_span", Internal, span_context)
  
  // Add event with sensitive data
  Span::add_event(span, "user_login", Some([
    ("user.name", StringValue("John Doe")),
    ("user.password", StringValue("secret123")),
    ("login.ip", StringValue("192.168.1.1"))
  ]))
  
  // Apply redaction to span
  let redacted_span = Security::redact_span(span)
  
  // Verify sensitive data in events is redacted
  let events = Span::get_events(redacted_span)
  assert_eq(events.length(), 1)
  
  let event = events[0]
  let redacted_event_password = Event::get_attribute(event, "user.password")
  match redacted_event_password {
    Some(StringValue(value)) => assert_eq(value, "[REDACTED]")
    _ => assert_true(false)
  }
  
  let preserved_event_name = Event::get_attribute(event, "user.name")
  match preserved_event_name {
    Some(StringValue(value)) => assert_eq(value, "John Doe")
    _ => assert_true(false)
  }
}

// Test 2: Data Encryption and Decryption
test "data encryption and decryption" {
  // Generate encryption key
  let encryption_key = Security::generate_encryption_key()
  
  // Verify key is generated
  assert_eq(encryption_key.length(), 32) // 256-bit key
  
  // Test string encryption
  let plaintext = "Sensitive telemetry data"
  let encrypted_data = Security::encrypt(plaintext, encryption_key)
  
  // Verify encryption produces different data
  assert_not_eq(encrypted_data, plaintext)
  assert_true(encrypted_data.length() > 0)
  
  // Test string decryption
  let decrypted_data = Security::decrypt(encrypted_data, encryption_key)
  
  // Verify decryption restores original data
  assert_eq(decrypted_data, plaintext)
  
  // Test with wrong key fails
  let wrong_key = Security::generate_encryption_key()
  let failed_decryption = Security::decrypt(encrypted_data, wrong_key)
  
  // Verify decryption with wrong key fails
  assert_not_eq(failed_decryption, plaintext)
  
  // Test attribute value encryption
  let sensitive_value = StringValue("api-key-1234567890")
  let encrypted_value = Security::encrypt_attribute_value(sensitive_value, encryption_key)
  
  match encrypted_value {
    StringValue(encrypted) => {
      assert_not_eq(encrypted, "api-key-1234567890")
      assert_true(encrypted.length() > 0)
    }
    _ => assert_true(false)
  }
  
  // Test attribute value decryption
  let decrypted_value = Security::decrypt_attribute_value(encrypted_value, encryption_key)
  
  match decrypted_value {
    StringValue(decrypted) => assert_eq(decrypted, "api-key-1234567890")
    _ => assert_true(false)
  }
  
  // Test span encryption
  let span_context = SpanContext::new("trace123", "span123", true, "encryption_test")
  let span = Span::new("encryption_test_span", Internal, span_context)
  
  Span::set_attribute(span, "sensitive.data", StringValue("secret information"))
  Span::set_attribute(span, "public.data", StringValue("public information"))
  
  // Encrypt span
  let encrypted_span = Security::encrypt_span(span, encryption_key)
  
  // Verify span is encrypted
  let encrypted_attribute = Span::get_attribute(encrypted_span, "sensitive.data")
  match encrypted_attribute {
    StringValue(value) => assert_not_eq(value, "secret information")
    _ => assert_true(false)
  }
  
  // Decrypt span
  let decrypted_span = Security::decrypt_span(encrypted_span, encryption_key)
  
  // Verify span is decrypted
  let decrypted_attribute = Span::get_attribute(decrypted_span, "sensitive.data")
  match decrypted_attribute {
    StringValue(value) => assert_eq(value, "secret information")
    _ => assert_true(false)
  }
  
  let public_attribute = Span::get_attribute(decrypted_span, "public.data")
  match public_attribute {
    StringValue(value) => assert_eq(value, "public information")
    _ => assert_true(false)
  }
}

// Test 3: Access Control and Authorization
test "access control and authorization" {
  // Define roles
  enum Role {
    Admin
    Operator
    Viewer
  }
  
  // Define permissions
  enum Permission {
    ReadSpans
    WriteSpans
    ReadMetrics
    WriteMetrics
    ReadLogs
    WriteLogs
    ManageUsers
  }
  
  // Create users with different roles
  let admin_user = User::new("admin", [Admin])
  let operator_user = User::new("operator", [Operator])
  let viewer_user = User::new("viewer", [Viewer])
  
  // Define role permissions
  let role_permissions = [
    (Admin, [ReadSpans, WriteSpans, ReadMetrics, WriteMetrics, ReadLogs, WriteLogs, ManageUsers]),
    (Operator, [ReadSpans, WriteSpans, ReadMetrics, WriteMetrics, ReadLogs, WriteLogs]),
    (Viewer, [ReadSpans, ReadMetrics, ReadLogs])
  ]
  
  // Test permission checking
  let has_permission = fn(user : User, permission : Permission) -> Bool {
    let user_roles = User::roles(user)
    
    for role in user_roles {
      for (role_perm, permissions) in role_permissions {
        if role == role_perm {
          for perm in permissions {
            if perm == permission {
              return true
            }
          }
        }
      }
    }
    
    false
  }
  
  // Test admin permissions
  assert_true(has_permission(admin_user, ReadSpans))
  assert_true(has_permission(admin_user, WriteSpans))
  assert_true(has_permission(admin_user, ReadMetrics))
  assert_true(has_permission(admin_user, WriteMetrics))
  assert_true(has_permission(admin_user, ReadLogs))
  assert_true(has_permission(admin_user, WriteLogs))
  assert_true(has_permission(admin_user, ManageUsers))
  
  // Test operator permissions
  assert_true(has_permission(operator_user, ReadSpans))
  assert_true(has_permission(operator_user, WriteSpans))
  assert_true(has_permission(operator_user, ReadMetrics))
  assert_true(has_permission(operator_user, WriteMetrics))
  assert_true(has_permission(operator_user, ReadLogs))
  assert_true(has_permission(operator_user, WriteLogs))
  assert_false(has_permission(operator_user, ManageUsers))
  
  // Test viewer permissions
  assert_true(has_permission(viewer_user, ReadSpans))
  assert_false(has_permission(viewer_user, WriteSpans))
  assert_true(has_permission(viewer_user, ReadMetrics))
  assert_false(has_permission(viewer_user, WriteMetrics))
  assert_true(has_permission(viewer_user, ReadLogs))
  assert_false(has_permission(viewer_user, WriteLogs))
  assert_false(has_permission(viewer_user, ManageUsers))
  
  // Test access control on telemetry data
  let span_context = SpanContext::new("trace123", "span123", true, "access_control_test")
  let span = Span::new("access_control_span", Internal, span_context)
  
  // Test span access control
  let can_read_span = fn(user : User, span : Span) -> Bool {
    has_permission(user, ReadSpans)
  }
  
  let can_write_span = fn(user : User, span : Span) -> Bool {
    has_permission(user, WriteSpans)
  }
  
  // Verify access control
  assert_true(can_read_span(admin_user, span))
  assert_true(can_write_span(admin_user, span))
  
  assert_true(can_read_span(operator_user, span))
  assert_true(can_write_span(operator_user, span))
  
  assert_true(can_read_span(viewer_user, span))
  assert_false(can_write_span(viewer_user, span))
  
  // Test metric access control
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "access_control_meter")
  let counter = Meter::create_counter(meter, "access_counter", Some("Access control counter"), Some("operations"))
  
  let can_read_metric = fn(user : User, metric : Counter) -> Bool {
    has_permission(user, ReadMetrics)
  }
  
  let can_write_metric = fn(user : User, metric : Counter) -> Bool {
    has_permission(user, WriteMetrics)
  }
  
  // Verify metric access control
  assert_true(can_read_metric(admin_user, counter))
  assert_true(can_write_metric(admin_user, counter))
  
  assert_true(can_read_metric(operator_user, counter))
  assert_true(can_write_metric(operator_user, counter))
  
  assert_true(can_read_metric(viewer_user, counter))
  assert_false(can_write_metric(viewer_user, counter))
}

// Test 4: Authentication and Token Validation
test "authentication and token validation" {
  // Test user authentication
  let authenticate_user = fn(username : String, password : String) -> Option[User] {
    // In a real implementation, this would check against a user database
    if username == "admin" && password == "admin123" {
      Some(User::new("admin", [Admin]))
    } else if username == "operator" && password == "operator123" {
      Some(User::new("operator", [Operator]))
    } else if username == "viewer" && password == "viewer123" {
      Some(User::new("viewer", [Viewer]))
    } else {
      None
    }
  }
  
  // Test valid authentication
  let admin_auth = authenticate_user("admin", "admin123")
  match admin_auth {
    Some(user) => assert_eq(User::name(user), "admin")
    None => assert_true(false)
  }
  
  let operator_auth = authenticate_user("operator", "operator123")
  match operator_auth {
    Some(user) => assert_eq(User::name(user), "operator")
    None => assert_true(false)
  }
  
  let viewer_auth = authenticate_user("viewer", "viewer123")
  match viewer_auth {
    Some(user) => assert_eq(User::name(user), "viewer")
    None => assert_true(false)
  }
  
  // Test invalid authentication
  let invalid_auth = authenticate_user("invalid", "wrong")
  match invalid_auth {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test JWT token generation and validation
  let generate_token = fn(user : User) -> String {
    // In a real implementation, this would generate a proper JWT
    let header = "{\"alg\":\"HS256\",\"typ\":\"JWT\"}"
    let payload = "{\"sub\":\"" + User::name(user) + "\",\"role\":\"" + User::roles(user)[0].to_string() + "\"}"
    
    // Simulate JWT encoding (base64url encoding + signature)
    Security::base64url_encode(header) + "." + Security::base64url_encode(payload) + ".signature"
  }
  
  let validate_token = fn(token : String) -> Option[User] {
    // In a real implementation, this would validate the JWT signature and claims
    let parts = token.split(".")
    
    if parts.length() != 3 {
      return None
    }
    
    let payload = parts[1]
    let decoded_payload = Security::base64url_decode(payload)
    
    // Parse payload to extract user info
    if decoded_payload.contains("\"sub\":\"admin\"") {
      Some(User::new("admin", [Admin]))
    } else if decoded_payload.contains("\"sub\":\"operator\"") {
      Some(User::new("operator", [Operator]))
    } else if decoded_payload.contains("\"sub\":\"viewer\"") {
      Some(User::new("viewer", [Viewer]))
    } else {
      None
    }
  }
  
  // Test token generation
  let admin_user = User::new("admin", [Admin])
  let admin_token = generate_token(admin_user)
  
  assert_true(admin_token.contains("."))
  assert_true(admin_token.length() > 10)
  
  // Test token validation
  let validated_admin = validate_token(admin_token)
  match validated_admin {
    Some(user) => assert_eq(User::name(user), "admin")
    None => assert_true(false)
  }
  
  // Test invalid token validation
  let invalid_token = "invalid.token.format"
  let invalid_validation = validate_token(invalid_token)
  match invalid_validation {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test token expiration
  let generate_expiring_token = fn(user : User, expires_in_seconds : Int) -> String {
    let header = "{\"alg\":\"HS256\",\"typ\":\"JWT\"}"
    let current_time = Platform::get_current_time()
    let expiry_time = current_time + expires_in_seconds
    
    let payload = "{\"sub\":\"" + User::name(user) + "\",\"exp\":" + expiry_time.to_string() + "}"
    
    Security::base64url_encode(header) + "." + Security::base64url_encode(payload) + ".signature"
  }
  
  let validate_expiring_token = fn(token : String) -> Option[User] {
    let parts = token.split(".")
    
    if parts.length() != 3 {
      return None
    }
    
    let payload = parts[1]
    let decoded_payload = Security::base64url_decode(payload)
    
    // Check expiration
    let current_time = Platform::get_current_time()
    
    if decoded_payload.contains("\"exp\":") {
      // Extract expiration time
      let exp_start = decoded_payload.index_of("\"exp\":") + 6
      let exp_end = decoded_payload.index_of("}", exp_start)
      let exp_str = decoded_payload.substring(exp_start, exp_end)
      let exp_time = exp_str.to_int()
      
      if exp_time < current_time {
        return None // Token expired
      }
    }
    
    // Extract user info
    if decoded_payload.contains("\"sub\":\"admin\"") {
      Some(User::new("admin", [Admin]))
    } else if decoded_payload.contains("\"sub\":\"operator\"") {
      Some(User::new("operator", [Operator]))
    } else if decoded_payload.contains("\"sub\":\"viewer\"") {
      Some(User::new("viewer", [Viewer]))
    } else {
      None
    }
  }
  
  // Test non-expiring token
  let non_expiring_token = generate_expiring_token(admin_user, 3600) // 1 hour
  let non_expiring_validation = validate_expiring_token(non_expiring_token)
  match non_expiring_validation {
    Some(user) => assert_eq(User::name(user), "admin")
    None => assert_true(false)
  }
  
  // Test expired token
  let expired_token = generate_expiring_token(admin_user, -1) // Already expired
  let expired_validation = validate_expiring_token(expired_token)
  match expired_validation {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 5: Input Validation and Sanitization
test "input validation and sanitization" {
  // Test string validation
  let validate_string = fn(input : String, max_length : Int, allowed_pattern : String) -> Bool {
    // Check length
    if input.length() > max_length {
      return false
    }
    
    // Check pattern
    // In a real implementation, this would use regex matching
    if allowed_pattern == "alphanumeric" {
      for char in input.to_chars() {
        if !((char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || (char >= '0' && char <= '9')) {
          return false
        }
      }
    } else if allowed_pattern == "email" {
      if !input.contains("@") || !input.contains(".") {
        return false
      }
    }
    
    true
  }
  
  // Test valid inputs
  assert_true(validate_string("valid123", 10, "alphanumeric"))
  assert_true(validate_string("user@example.com", 50, "email"))
  
  // Test invalid inputs
  assert_false(validate_string("invalid-char!", 20, "alphanumeric"))
  assert_false(validate_string("not-an-email", 20, "email"))
  assert_false(validate_string("toolongstring", 5, "alphanumeric"))
  
  // Test string sanitization
  let sanitize_string = fn(input : String) -> String {
    // Remove potentially dangerous characters
    let mut result = ""
    
    for char in input.to_chars() {
      if char == '<' || char == '>' || char == '\'' || char == '"' || char == '&' {
        // Replace with HTML entities
        if char == '<' {
          result = result + "&lt;"
        } else if char == '>' {
          result = result + "&gt;"
        } else if char == '\'' {
          result = result + "&#39;"
        } else if char == '"' {
          result = result + "&quot;"
        } else if char == '&' {
          result = result + "&amp;"
        }
      } else {
        result = result + char
      }
    }
    
    result
  }
  
  // Test sanitization
  let sanitized_input = sanitize_string("<script>alert('xss')</script>")
  assert_eq(sanitized_input, "&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;")
  
  // Test attribute validation
  let validate_attribute = fn(key : String, value : AttributeValue) -> Bool {
    // Check key format
    if key.length() > 255 || key.length() == 0 {
      return false
    }
    
    // Check value based on type
    match value {
      StringValue(s) => validate_string(s, 1000, "any"),
      IntValue(_) => true,
      FloatValue(_) => true,
      BoolValue(_) => true,
      ArrayStringValue(arr) => {
        if arr.length() > 100 {
          return false
        }
        
        for item in arr {
          if !validate_string(item, 255, "any") {
            return false
          }
        }
        
        true
      }
      ArrayIntValue(arr) => arr.length() <= 100,
    }
  }
  
  // Test valid attributes
  assert_true(validate_attribute("valid.key", StringValue("valid_value")))
  assert_true(validate_attribute("numeric.key", IntValue(42)))
  assert_true(validate_attribute("array.key", ArrayStringValue(["item1", "item2"])))
  
  // Test invalid attributes
  assert_false(validate_attribute("", StringValue("empty_key")))
  assert_false(validate_attribute("toolongkey" + "x".repeat(300), StringValue("value")))
  assert_false(validate_attribute("valid.key", StringValue("x".repeat(2000))))
  assert_false(validate_attribute("valid.key", ArrayStringValue(["item1", "item2", "item3", "item4", "item5", "item6", "item7", "item8", "item9", "item10", "item11", "item12", "item13", "item14", "item15", "item16", "item17", "item18", "item19", "item20", "item21", "item22", "item23", "item24", "item25", "item26", "item27", "item28", "item29", "item30", "item31", "item32", "item33", "item34", "item35", "item36", "item37", "item38", "item39", "item40", "item41", "item42", "item43", "item44", "item45", "item46", "item47", "item48", "item49", "item50", "item51", "item52", "item53", "item54", "item55", "item56", "item57", "item58", "item59", "item60", "item61", "item62", "item63", "item64", "item65", "item66", "item67", "item68", "item69", "item70", "item71", "item72", "item73", "item74", "item75", "item76", "item77", "item78", "item79", "item80", "item81", "item82", "item83", "item84", "item85", "item86", "item87", "item88", "item89", "item90", "item91", "item92", "item93", "item94", "item95", "item96", "item97", "item98", "item99", "item100", "item101"])))
  
  // Test span validation
  let validate_span = fn(span : Span) -> Bool {
    // Check span name
    let name = Span::name(span)
    if !validate_string(name, 255, "any") {
      return false
    }
    
    // Check attributes
    let attributes = Span::get_attributes(span)
    for (key, value) in attributes {
      if !validate_attribute(key, value) {
        return false
      }
    }
    
    true
  }
  
  // Test span validation
  let span_context = SpanContext::new("trace123", "span123", true, "validation_test")
  let valid_span = Span::new("valid_span", Internal, span_context)
  Span::set_attribute(valid_span, "valid.key", StringValue("valid_value"))
  
  assert_true(validate_span(valid_span))
  
  let invalid_span = Span::new("invalid_span", Internal, span_context)
  Span::set_attribute(invalid_span, "", StringValue("empty_key"))
  
  assert_false(validate_span(invalid_span))
}

// Test 6: Rate Limiting and Throttling
test "rate limiting and throttling" {
  // Create rate limiter
  let rate_limiter = RateLimiter::new(100, 60) // 100 requests per minute
  
  // Test rate limiting
  let mut requests_allowed = 0
  let mut requests_denied = 0
  
  // Make 150 requests (should allow 100, deny 50)
  for i in 1..=150 {
    if RateLimiter::allow_request(rate_limiter, "client1") {
      requests_allowed = requests_allowed + 1
    } else {
      requests_denied = requests_denied + 1
    }
  }
  
  assert_eq(requests_allowed, 100)
  assert_eq(requests_denied, 50)
  
  // Test different clients have separate limits
  let mut client2_allowed = 0
  let mut client2_denied = 0
  
  // Make 50 requests for client2 (should all be allowed)
  for i in 1..=50 {
    if RateLimiter::allow_request(rate_limiter, "client2") {
      client2_allowed = client2_allowed + 1
    } else {
      client2_denied = client2_denied + 1
    }
  }
  
  assert_eq(client2_allowed, 50)
  assert_eq(client2_denied, 0)
  
  // Test rate limiting with different limits
  let premium_rate_limiter = RateLimiter::new(500, 60) // 500 requests per minute for premium clients
  
  let mut premium_allowed = 0
  let mut premium_denied = 0
  
  // Make 300 requests for premium client (should all be allowed)
  for i in 1..=300 {
    if RateLimiter::allow_request(premium_rate_limiter, "premium_client") {
      premium_allowed = premium_allowed + 1
    } else {
      premium_denied = premium_denied + 1
    }
  }
  
  assert_eq(premium_allowed, 300)
  assert_eq(premium_denied, 0)
  
  // Test telemetry rate limiting
  let telemetry_rate_limiter = RateLimiter::new(1000, 60) // 1000 telemetry events per minute
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "rate_limit_meter")
  let counter = Meter::create_counter(meter, "rate_limit_counter", Some("Rate limit counter"), Some("operations"))
  
  let mut telemetry_allowed = 0
  let mut telemetry_denied = 0
  
  // Make 1200 telemetry operations
  for i in 1..=1200 {
    if RateLimiter::allow_request(telemetry_rate_limiter, "telemetry_client") {
      Counter::add(counter, 1.0)
      telemetry_allowed = telemetry_allowed + 1
    } else {
      telemetry_denied = telemetry_denied + 1
    }
  }
  
  assert_eq(telemetry_allowed, 1000)
  assert_eq(telemetry_denied, 200)
  
  // Test rate limiting recovery after time window
  // In a real implementation, this would involve waiting for the time window to reset
  // For testing purposes, we'll simulate this by creating a new rate limiter
  let recovery_rate_limiter = RateLimiter::new(10, 1) // 10 requests per second
  
  // Use up all requests
  let mut recovery_allowed = 0
  for i in 1..=15 {
    if RateLimiter::allow_request(recovery_rate_limiter, "recovery_client") {
      recovery_allowed = recovery_allowed + 1
    }
  }
  
  assert_eq(recovery_allowed, 10)
  
  // Simulate time passing by creating a new limiter
  let new_recovery_rate_limiter = RateLimiter::new(10, 1)
  
  recovery_allowed = 0
  for i in 1..=5 {
    if RateLimiter::allow_request(new_recovery_rate_limiter, "recovery_client") {
      recovery_allowed = recovery_allowed + 1
    }
  }
  
  assert_eq(recovery_allowed, 5) // Should allow requests again
}

// Test 7: Audit Logging and Security Events
test "audit logging and security events" {
  // Create audit logger
  let audit_logger = AuditLogger::new("azimuth_audit.log")
  
  // Test audit logging for security events
  let log_authentication_event = fn(username : String, success : Bool, ip_address : String) {
    let event_type = if success { "authentication_success" } else { "authentication_failure" }
    
    let audit_record = AuditRecord::new(
      event_type,
      username,
      ip_address,
      Some(Attributes::with([
        ("event.timestamp", StringValue(Platform::format_time(Platform::get_current_time(), "%Y-%m-%dT%H:%M:%S"))),
        ("event.result", StringValue(if success { "success" } else { "failure" }))
      ]))
    )
    
    AuditLogger::log(audit_logger, audit_record)
  }
  
  // Test successful authentication
  log_authentication_event("admin", true, "192.168.1.100")
  
  // Test failed authentication
  log_authentication_event("hacker", false, "10.0.0.1")
  
  // Test authorization events
  let log_authorization_event = fn(username : String, resource : String, action : String, success : Bool) {
    let event_type = if success { "authorization_success" } else { "authorization_failure" }
    
    let audit_record = AuditRecord::new(
      event_type,
      username,
      "system",
      Some(Attributes::with([
        ("resource", StringValue(resource)),
        ("action", StringValue(action)),
        ("event.timestamp", StringValue(Platform::format_time(Platform::get_current_time(), "%Y-%m-%dT%H:%M:%S"))),
        ("event.result", StringValue(if success { "success" } else { "failure" }))
      ]))
    )
    
    AuditLogger::log(audit_logger, audit_record)
  }
  
  // Test successful authorization
  log_authorization_event("admin", "spans", "read", true)
  
  // Test failed authorization
  log_authorization_event("viewer", "users", "write", false)
  
  // Test data access events
  let log_data_access_event = fn(username : String, data_type : String, record_count : Int) {
    let audit_record = AuditRecord::new(
      "data_access",
      username,
      "system",
      Some(Attributes::with([
        ("data_type", StringValue(data_type)),
        ("record_count", IntValue(record_count)),
        ("event.timestamp", StringValue(Platform::format_time(Platform::get_current_time(), "%Y-%m-%dT%H:%M:%S")))
      ]))
    )
    
    AuditLogger::log(audit_logger, audit_record)
  }
  
  // Test data access
  log_data_access_event("analyst", "spans", 100)
  log_data_access_event("analyst", "metrics", 50)
  log_data_access_event("analyst", "logs", 200)
  
  // Test configuration change events
  let log_configuration_change_event = fn(username : String, component : String, setting : String, old_value : String, new_value : String) {
    let audit_record = AuditRecord::new(
      "configuration_change",
      username,
      "system",
      Some(Attributes::with([
        ("component", StringValue(component)),
        ("setting", StringValue(setting)),
        ("old_value", StringValue(old_value)),
        ("new_value", StringValue(new_value)),
        ("event.timestamp", StringValue(Platform::format_time(Platform::get_current_time(), "%Y-%m-%dT%H:%M:%S")))
      ]))
    )
    
    AuditLogger::log(audit_logger, audit_record)
  }
  
  // Test configuration change
  log_configuration_change_event("admin", "rate_limiter", "max_requests", "100", "200")
  log_configuration_change_event("admin", "encryption", "key_rotation_interval", "86400", "43200")
  
  // Verify audit records are logged
  let audit_records = AuditLogger::get_records(audit_logger)
  assert_eq(audit_records.length(), 8)
  
  // Verify specific records
  let auth_success_record = audit_records[0]
  assert_eq(AuditRecord::event_type(auth_success_record), "authentication_success")
  assert_eq(AuditRecord::username(auth_success_record), "admin")
  
  let auth_failure_record = audit_records[1]
  assert_eq(AuditRecord::event_type(auth_failure_record), "authentication_failure")
  assert_eq(AuditRecord::username(auth_failure_record), "hacker")
  
  let authz_failure_record = audit_records[3]
  assert_eq(AuditRecord::event_type(authz_failure_record), "authorization_failure")
  assert_eq(AuditRecord::username(authz_failure_record), "viewer")
  
  let config_change_record = audit_records[7]
  assert_eq(AuditRecord::event_type(config_change_record), "configuration_change")
  assert_eq(AuditRecord::username(config_change_record), "admin")
  
  // Test audit record filtering
  let failure_records = AuditLogger::filter_records(audit_logger, fn(record) {
    AuditRecord::event_type(record).contains("failure")
  })
  
  assert_eq(failure_records.length(), 2) // authentication_failure and authorization_failure
  
  // Test audit record search
  let admin_records = AuditLogger::search_records(audit_logger, "admin")
  assert_eq(admin_records.length(), 4) // authentication_success, authorization_success, and 2 configuration_change
}

// Test 8: Secure Communication
test "secure communication" {
  // Test TLS/SSL certificate validation
  let validate_certificate = fn(cert_path : String) -> Bool {
    // In a real implementation, this would validate the certificate chain, expiration, etc.
    // For testing purposes, we'll simulate the validation
    
    if cert_path.contains("valid") {
      return true
    } else if cert_path.contains("expired") {
      return false
    } else if cert_path.contains("invalid") {
      return false
    } else {
      return false
    }
  }
  
  // Test certificate validation
  assert_true(validate_certificate("/path/to/valid_cert.pem"))
  assert_false(validate_certificate("/path/to/expired_cert.pem"))
  assert_false(validate_certificate("/path/to/invalid_cert.pem"))
  
  // Test HTTPS client with certificate validation
  let https_client = HttpClient::new()
  HttpClient::set_certificate_validation(https_client, true)
  
  // Test HTTPS request to a server with valid certificate
  let valid_request = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
  let valid_response = HttpClient::execute(https_client, valid_request)
  
  match valid_response {
    Ok(response) => assert_eq(HttpResponse::status_code(response), 200)
    Err(_) => assert_true(false) // Should succeed with valid certificate
  }
  
  // Test HTTPS request to a server with invalid certificate
  let invalid_request = HttpRequest::new("GET", "https://expired.badssl.com", [], None)
  let invalid_response = HttpClient::execute(https_client, invalid_request)
  
  match invalid_response {
    Ok(_) => assert_true(false) // Should fail with invalid certificate
    Err(_) => assert_true(true) // Expected to fail
  }
  
  // Test HTTPS client without certificate validation
  let insecure_client = HttpClient::new()
  HttpClient::set_certificate_validation(insecure_client, false)
  
  let insecure_response = HttpClient::execute(insecure_client, invalid_request)
  
  match insecure_response {
    Ok(response) => assert_eq(HttpResponse::status_code(response), 200) // Should succeed without validation
    Err(_) => assert_true(false) // Should succeed without validation
  }
  
  // Test mutual TLS authentication
  let configure_mutual_tls = fn(client : HttpClient, cert_path : String, key_path : String) {
    HttpClient::set_client_certificate(client, cert_path, key_path)
  }
  
  let mtls_client = HttpClient::new()
  configure_mutual_tls(mtls_client, "/path/to/client_cert.pem", "/path/to/client_key.pem")
  
  // Test mTLS request
  let mtls_request = HttpRequest::new("GET", "https://mtls.example.com/api", [], None)
  let mtls_response = HttpClient::execute(mtls_client, mtls_request)
  
  // In a real test environment with mTLS server:
  // match mtls_response {
  //   Ok(response) => assert_eq(HttpResponse::status_code(response), 200)
  //   Err(_) => assert_true(false)
  // }
  
  // Test secure telemetry transmission
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "secure_comm_meter")
  let counter = Meter::create_counter(meter, "secure_comm_counter", Some("Secure communication counter"), Some("operations"))
  
  // Record metrics
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("comm.protocol", StringValue("HTTPS")),
    ("comm.cert_validation", StringValue("enabled")),
    ("comm.mtls", StringValue("disabled"))
  ])))
  
  // Securely transmit metrics
  let secure_exporter = SecureMetricExporter::new("https://metrics.example.com", "/path/to/ca_cert.pem")
  let export_result = SecureMetricExporter::export(secure_exporter, counter)
  
  // In a real implementation:
  // match export_result {
  //   Ok(_) => assert_true(true)
  //   Err(_) => assert_true(false)
  // }
  
  // Verify counter was created
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "secure_comm_counter")
  
  // Test secure configuration
  let secure_config = SecureConfiguration::new()
  SecureConfiguration::set_tls_version(secure_config, "TLSv1.3")
  SecureConfiguration::set_cipher_suites(secure_config, ["TLS_AES_256_GCM_SHA384", "TLS_CHACHA20_POLY1305_SHA256"])
  SecureConfiguration::set_certificate_validation(secure_config, true)
  SecureConfiguration::set_mutual_tls(secure_config, false)
  
  // Verify configuration
  assert_eq(SecureConfiguration::get_tls_version(secure_config), "TLSv1.3")
  assert_eq(SecureConfiguration::get_cipher_suites(secure_config).length(), 2)
  assert_true(SecureConfiguration::get_certificate_validation(secure_config))
  assert_false(SecureConfiguration::get_mutual_tls(secure_config))
}

// Test 9: Intrusion Detection and Anomaly Detection
test "intrusion detection and anomaly detection" {
  // Create intrusion detection system
  let ids = IntrusionDetectionSystem::new()
  
  // Test suspicious IP detection
  let add_suspicious_ip = fn(ip : String, reason : String) {
    IDS::add_suspicious_ip(ids, ip, reason)
  }
  
  let is_suspicious_ip = fn(ip : String) -> Bool {
    IDS::is_suspicious_ip(ids, ip)
  }
  
  // Add suspicious IPs
  add_suspicious_ip("192.168.1.100", "Multiple failed login attempts")
  add_suspicious_ip("10.0.0.50", "SQL injection attempt")
  add_suspicious_ip("172.16.0.25", "Port scanning")
  
  // Test IP detection
  assert_true(is_suspicious_ip("192.168.1.100"))
  assert_true(is_suspicious_ip("10.0.0.50"))
  assert_true(is_suspicious_ip("172.16.0.25"))
  assert_false(is_suspicious_ip("192.168.1.1")) // Not in suspicious list
  
  // Test anomaly detection for authentication patterns
  let detect_auth_anomaly = fn(username : String, ip_address : String, time_of_day : Int) -> Bool {
    // In a real implementation, this would use machine learning or statistical analysis
    // For testing purposes, we'll use simple rules
    
    // Anomaly: Admin login from unusual IP
    if username == "admin" && ip_address != "192.168.1.100" {
      return true
    }
    
    // Anomaly: Login during unusual hours (outside 9-5)
    if time_of_day < 9 || time_of_day > 17 {
      return true
    }
    
    // Anomaly: Multiple failed logins followed by success
    // This would require tracking failed attempts, simplified for testing
    
    false
  }
  
  // Test authentication anomaly detection
  assert_true(detect_auth_anomaly("admin", "10.0.0.1", 10)) // Admin from unusual IP
  assert_false(detect_auth_anomaly("admin", "192.168.1.100", 10)) // Admin from usual IP
  assert_true(detect_auth_anomaly("user1", "192.168.1.1", 2)) // Unusual time
  assert_false(detect_auth_anomaly("user1", "192.168.1.1", 14)) // Normal time
  
  // Test anomaly detection for data access patterns
  let detect_data_access_anomaly = fn(username : String, data_type : String, record_count : Int, time_window : Int) -> Bool {
    // Anomaly: Unusually large data access
    if record_count > 1000 {
      return true
    }
    
    // Anomaly: Access to sensitive data by non-privileged user
    if data_type == "sensitive" && username != "admin" {
      return true
    }
    
    // Anomaly: High frequency access in short time window
    if record_count > 100 && time_window < 60 { // 100 records in less than a minute
      return true
    }
    
    false
  }
  
  // Test data access anomaly detection
  assert_true(detect_data_access_anomaly("user1", "spans", 1500, 300)) // Large data access
  assert_false(detect_data_access_anomaly("user1", "spans", 500, 300)) // Normal data access
  assert_true(detect_data_access_anomaly("user1", "sensitive", 10, 300)) // Access to sensitive data
  assert_false(detect_data_access_anomaly("admin", "sensitive", 10, 300)) // Admin access to sensitive data
  assert_true(detect_data_access_anomaly("user1", "logs", 150, 30)) // High frequency access
  assert_false(detect_data_access_anomaly("user1", "logs", 50, 30)) // Normal frequency access
  
  // Test intrusion response
  let block_ip = fn(ip : String, duration_seconds : Int) {
    // In a real implementation, this would update firewall rules
    IDS::block_ip(ids, ip, duration_seconds)
  }
  
  let is_ip_blocked = fn(ip : String) -> Bool {
    IDS::is_ip_blocked(ids, ip)
  }
  
  // Block suspicious IP
  block_ip("10.0.0.50", 3600) // Block for 1 hour
  
  // Verify IP is blocked
  assert_true(is_ip_blocked("10.0.0.50"))
  assert_false(is_ip_blocked("192.168.1.100")) // Not blocked
  
  // Test alert generation
  let generate_alert = fn(alert_type : String, severity : String, description : String, source_ip : String) {
    let alert = SecurityAlert::new(
      alert_type,
      severity,
      description,
      source_ip,
      Platform::get_current_time()
    )
    
    IDS::add_alert(ids, alert)
  }
  
  // Generate alerts
  generate_alert("intrusion_attempt", "high", "SQL injection attempt detected", "10.0.0.50")
  generate_alert("anomaly_detected", "medium", "Unusual data access pattern", "192.168.1.200")
  generate_alert("authentication_failure", "low", "Multiple failed login attempts", "172.16.0.25")
  
  // Verify alerts are generated
  let alerts = IDS::get_alerts(ids)
  assert_eq(alerts.length(), 3)
  
  // Verify alert details
  let intrusion_alert = alerts[0]
  assert_eq(SecurityAlert::alert_type(intrusion_alert), "intrusion_attempt")
  assert_eq(SecurityAlert::severity(intrusion_alert), "high")
  assert_eq(SecurityAlert::source_ip(intrusion_alert), "10.0.0.50")
  
  let anomaly_alert = alerts[1]
  assert_eq(SecurityAlert::alert_type(anomaly_alert), "anomaly_detected")
  assert_eq(SecurityAlert::severity(anomaly_alert), "medium")
  assert_eq(SecurityAlert::source_ip(anomaly_alert), "192.168.1.200")
  
  // Test alert filtering
  let high_severity_alerts = IDS::filter_alerts(ids, fn(alert) {
    SecurityAlert::severity(alert) == "high"
  })
  
  assert_eq(high_severity_alerts.length(), 1) // Only the intrusion attempt
}

// Test 10: Security Configuration and Hardening
test "security configuration and hardening" {
  // Test security configuration
  let security_config = SecurityConfiguration::new()
  
  // Configure security settings
  SecurityConfiguration::set_encryption_at_rest(security_config, true)
  SecurityConfiguration::set_encryption_in_transit(security_config, true)
  SecurityConfiguration::set_require_strong_authentication(security_config, true)
  SecurityConfiguration::set_session_timeout_minutes(security_config, 30)
  SecurityConfiguration::set_max_login_attempts(security_config, 5)
  SecurityConfiguration::set_password_min_length(security_config, 8)
  SecurityConfiguration::set_password_require_special_chars(security_config, true)
  SecurityConfiguration::set_audit_retention_days(security_config, 90)
  SecurityConfiguration::set_enable_intrusion_detection(security_config, true)
  
  // Verify security configuration
  assert_true(SecurityConfiguration::get_encryption_at_rest(security_config))
  assert_true(SecurityConfiguration::get_encryption_in_transit(security_config))
  assert_true(SecurityConfiguration::get_require_strong_authentication(security_config))
  assert_eq(SecurityConfiguration::get_session_timeout_minutes(security_config), 30)
  assert_eq(SecurityConfiguration::get_max_login_attempts(security_config), 5)
  assert_eq(SecurityConfiguration::get_password_min_length(security_config), 8)
  assert_true(SecurityConfiguration::get_password_require_special_chars(security_config))
  assert_eq(SecurityConfiguration::get_audit_retention_days(security_config), 90)
  assert_true(SecurityConfiguration::get_enable_intrusion_detection(security_config))
  
  // Test password validation
  let validate_password = fn(password : String, config : SecurityConfiguration) -> Bool {
    let min_length = SecurityConfiguration::get_password_min_length(config)
    let require_special = SecurityConfiguration::get_password_require_special_chars(config)
    
    // Check length
    if password.length() < min_length {
      return false
    }
    
    // Check for special characters if required
    if require_special {
      let has_special = false
      for char in password.to_chars() {
        if !((char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || (char >= '0' && char <= '9')) {
          has_special = true
          break
        }
      }
      
      if !has_special {
        return false
      }
    }
    
    true
  }
  
  // Test password validation
  assert_true(validate_password("SecureP@ssw0rd", security_config))
  assert_false(validate_password("short", security_config)) // Too short
  assert_false(validate_password("longbutnospecial", security_config)) // No special characters
  
  // Test with different configuration
  let lenient_config = SecurityConfiguration::new()
  SecurityConfiguration::set_password_min_length(lenient_config, 6)
  SecurityConfiguration::set_password_require_special_chars(lenient_config, false)
  
  assert_true(validate_password("simple", lenient_config)) // Meets lenient requirements
  assert_false(validate_password("short", lenient_config)) // Still too short
  
  // Test security hardening for telemetry components
  let harden_telemetry_components = fn(config : SecurityConfiguration) {
    // Apply security settings to telemetry components
    
    // Enable encryption for metric storage
    let provider = MeterProvider::default()
    MeterProvider::enable_encryption(provider, true)
    
    // Enable authentication for metric exporters
    let meter = MeterProvider::get_meter(provider, "hardened_meter")
    let exporter = MetricExporter::new("https://metrics.example.com")
    MetricExporter::enable_authentication(exporter, true)
    
    // Enable access control for span readers
    let span_reader = SpanReader::new()
    SpanReader::enable_access_control(span_reader, true)
    
    // Enable audit logging for loggers
    let logger_provider = LoggerProvider::default()
    let logger = LoggerProvider::get_logger(logger_provider, "hardened_logger")
    Logger::enable_audit_logging(logger, true)
  }
  
  // Apply security hardening
  harden_telemetry_components(security_config)
  
  // Test secure metric creation
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "secure_meter")
  let counter = Meter::create_counter(meter, "secure_counter", Some("Secure counter"), Some("operations"))
  
  // Record metric with sensitive data (should be encrypted)
  Counter::add(counter, 1.0, Some(Attributes::with([
    ("user.id", StringValue("user123")),
    ("sensitive.data", StringValue("secret"))
  ])))
  
  // Verify metric is created
  let counter_instrument = Counter::as_instrument(counter)
  assert_eq(Instrument::name(counter_instrument), "secure_counter")
  
  // Test secure span creation
  let span_context = SpanContext::new("trace123", "span123", true, "secure_test")
  let span = Span::new("secure_span", Internal, span_context)
  
  // Add sensitive data (should be redacted)
  Span::set_attribute(span, "user.password", StringValue("secret123"))
  Span::set_attribute(span, "api.key", StringValue("sk-1234567890abcdef"))
  
  // Verify span is created
  assert_eq(Span::name(span), "secure_span")
  
  // Test secure log creation
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "secure_logger")
  
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Secure log message"),
    Some(Attributes::with([
      ("user.id", StringValue("user123")),
      ("sensitive.data", StringValue("secret"))
    ])),
    Some(Platform::get_current_time()),
    Some(Platform::get_current_time()),
    Some("trace123"),
    Some("span123"),
    Some(Context::root())
  )
  
  Logger::emit(logger, log_record)
  
  // Verify log is created
  assert_eq(LogRecord::severity_number(log_record), Info)
  match LogRecord::body(log_record) {
    Some(body) => assert_eq(body, "Secure log message")
    None => assert_true(false)
  }
  
  // Test security configuration validation
  let validate_security_configuration = fn(config : SecurityConfiguration) -> Array[String] {
    let mut issues = []
    
    // Check for weak configurations
    if !SecurityConfiguration::get_encryption_at_rest(config) {
      issues = issues.concat(["Encryption at rest is disabled"])
    }
    
    if !SecurityConfiguration::get_encryption_in_transit(config) {
      issues = issues.concat(["Encryption in transit is disabled"])
    }
    
    if !SecurityConfiguration::get_require_strong_authentication(config) {
      issues = issues.concat(["Strong authentication is not required"])
    }
    
    if SecurityConfiguration::get_session_timeout_minutes(config) > 60 {
      issues = issues.concat(["Session timeout is too long (more than 60 minutes)"])
    }
    
    if SecurityConfiguration::get_max_login_attempts(config) > 10 {
      issues = issues.concat(["Maximum login attempts is too high (more than 10)"])
    }
    
    if SecurityConfiguration::get_password_min_length(config) < 8 {
      issues = issues.concat(["Password minimum length is too short (less than 8 characters)"])
    }
    
    if !SecurityConfiguration::get_enable_intrusion_detection(config) {
      issues = issues.concat(["Intrusion detection is disabled"])
    }
    
    issues
  }
  
  // Test secure configuration (should have no issues)
  let secure_issues = validate_security_configuration(security_config)
  assert_eq(secure_issues.length(), 0)
  
  // Test insecure configuration
  let insecure_config = SecurityConfiguration::new()
  SecurityConfiguration::set_encryption_at_rest(insecure_config, false)
  SecurityConfiguration::set_encryption_in_transit(insecure_config, false)
  SecurityConfiguration::set_require_strong_authentication(insecure_config, false)
  SecurityConfiguration::set_session_timeout_minutes(insecure_config, 120)
  SecurityConfiguration::set_max_login_attempts(insecure_config, 15)
  SecurityConfiguration::set_password_min_length(insecure_config, 6)
  SecurityConfiguration::set_enable_intrusion_detection(insecure_config, false)
  
  let insecure_issues = validate_security_configuration(insecure_config)
  assert_eq(insecure_issues.length(), 7)
  
  // Verify specific issues
  assert_true(insecure_issues.contains("Encryption at rest is disabled"))
  assert_true(insecure_issues.contains("Encryption in transit is disabled"))
  assert_true(insecure_issues.contains("Strong authentication is not required"))
  assert_true(insecure_issues.contains("Session timeout is too long (more than 60 minutes)"))
  assert_true(insecure_issues.contains("Maximum login attempts is too high (more than 10)"))
  assert_true(insecure_issues.contains("Password minimum length is too short (less than 8 characters)"))
  assert_true(insecure_issues.contains("Intrusion detection is disabled"))
}
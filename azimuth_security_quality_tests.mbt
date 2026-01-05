// Azimuth Telemetry System - Security Quality Tests
// This file contains comprehensive test cases for security functionality

// Test 1: Data Encryption and Decryption
test "data encryption and decryption" {
  let encryption_service = EncryptionService::new()
  let secret_key = "test_encryption_key_12345"
  
  // Test string encryption
  let plaintext = "Sensitive telemetry data"
  let encrypted_data = EncryptionService::encrypt(encryption_service, plaintext, secret_key)
  
  // Encrypted data should be different from plaintext
  assert_true(encrypted_data != plaintext)
  assert_true(encrypted_data.length() > 0)
  
  // Test decryption
  let decrypted_data = EncryptionService::decrypt(encryption_service, encrypted_data, secret_key)
  match decrypted_data {
    Some(decrypted) => assert_eq(decrypted, plaintext)
    None => assert_true(false)
  }
  
  // Test decryption with wrong key (should fail)
  let wrong_decrypted = EncryptionService::decrypt(encryption_service, encrypted_data, "wrong_key")
  match wrong_decrypted {
    Some(_) => assert_true(false)  // Should not succeed with wrong key
    None => assert_true(true)      // Expected to fail
  }
  
  // Test encryption of telemetry data
  let telemetry_data = TelemetryData::new(
    "secure_service",
    "secure_operation",
    1234567890L,
    1500L,
    Ok,
    [("user.id", StringValue("user123")), ("api.key", StringValue("secret_api_key"))]
  )
  
  let encrypted_telemetry = EncryptionService::encrypt_telemetry(encryption_service, telemetry_data, secret_key)
  assert_true(encrypted_telemetry != telemetry_data)
  
  // Test decryption of telemetry data
  let decrypted_telemetry = EncryptionService::decrypt_telemetry(encryption_service, encrypted_telemetry, secret_key)
  match decrypted_telemetry {
    Some(decrypted) => {
      assert_eq(TelemetryData::service_name(decrypted), "secure_service")
      assert_eq(TelemetryData::operation_name(decrypted), "secure_operation")
      
      let attributes = TelemetryData::attributes(decrypted)
      let user_id = Attributes::get(attributes, "user.id")
      match user_id {
        Some(StringValue(id)) => assert_eq(id, "user123")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 2: Access Control and Authentication
test "access control and authentication" {
  let auth_service = AuthService::new()
  
  // Create users with different roles
  let admin_user = User::new("admin", "admin_password", ["admin", "read", "write"])
  let read_user = User::new("reader", "reader_password", ["read"])
  let write_user = User::new("writer", "writer_password", ["read", "write"])
  
  AuthService::add_user(auth_service, admin_user)
  AuthService::add_user(auth_service, read_user)
  AuthService::add_user(auth_service, write_user)
  
  // Test authentication
  let admin_auth = AuthService::authenticate(auth_service, "admin", "admin_password")
  match admin_auth {
    Ok(user) => assert_eq(User::username(user), "admin")
    Err(_) => assert_true(false)
  }
  
  let invalid_auth = AuthService::authenticate(auth_service, "admin", "wrong_password")
  match invalid_auth {
    Ok(_) => assert_true(false)  // Should not succeed with wrong password
    Err(_) => assert_true(true)   // Expected to fail
  }
  
  // Test authorization
  let admin_user_obj = AuthService::get_user(auth_service, "admin")
  match admin_user_obj {
    Some(user) => {
      // Admin should have all permissions
      assert_true(AuthService::has_permission(user, "admin"))
      assert_true(AuthService::has_permission(user, "read"))
      assert_true(AuthService::has_permission(user, "write"))
      assert_true(AuthService::can_access_resource(user, "telemetry", "read"))
      assert_true(AuthService::can_access_resource(user, "telemetry", "write"))
      assert_true(AuthService::can_access_resource(user, "config", "admin"))
    }
    None => assert_true(false)
  }
  
  let read_user_obj = AuthService::get_user(auth_service, "reader")
  match read_user_obj {
    Some(user) => {
      // Reader should only have read permission
      assert_false(AuthService::has_permission(user, "admin"))
      assert_true(AuthService::has_permission(user, "read"))
      assert_false(AuthService::has_permission(user, "write"))
      assert_true(AuthService::can_access_resource(user, "telemetry", "read"))
      assert_false(AuthService::can_access_resource(user, "telemetry", "write"))
      assert_false(AuthService::can_access_resource(user, "config", "admin"))
    }
    None => assert_true(false)
  }
}

// Test 3: Input Validation and Sanitization
test "input validation and sanitization" {
  let validator = InputValidator::new()
  
  // Test SQL injection prevention
  let malicious_sql = "'; DROP TABLE users; --"
  let sanitized_sql = InputValidator::sanitize_sql(validator, malicious_sql)
  assert_false(sanitized_sql.contains("DROP"))
  assert_false(sanitized_sql.contains(";"))
  
  // Test XSS prevention
  let malicious_html = "<script>alert('XSS');</script>"
  let sanitized_html = InputValidator::sanitize_html(validator, malicious_html)
  assert_false(sanitized_html.contains("<script>"))
  assert_false(sanitized_html.contains("alert"))
  
  // Test path traversal prevention
  let malicious_path = "../../../etc/passwd"
  let sanitized_path = InputValidator::sanitize_path(validator, malicious_path)
  assert_false(sanitized_path.contains(".."))
  assert_false(sanitized_path.contains("/etc/"))
  
  // Test telemetry data validation
  let valid_telemetry = {
    "service.name": "test_service",
    "operation.name": "test_operation",
    "duration": 1500,
    "status": "ok"
  }
  
  let validation_result = InputValidator::validate_telemetry_data(validator, valid_telemetry)
  match validation_result {
    Ok(_) => assert_true(true)  // Should pass validation
    Err(_) => assert_true(false)
  }
  
  // Test telemetry data with missing required fields
  let invalid_telemetry = {
    "service.name": "test_service"
    // Missing operation.name
  }
  
  let invalid_validation_result = InputValidator::validate_telemetry_data(validator, invalid_telemetry)
  match invalid_validation_result {
    Ok(_) => assert_true(false)  // Should fail validation
    Err(errors) => assert_true(errors.contains("operation.name") && errors.contains("required"))
  }
}

// Test 4: Secure Logging
test "secure logging" {
  let secure_logger = SecureLogger::new()
  
  // Test logging sensitive data (should be masked)
  let sensitive_data = {
    "user.id": "user123",
    "credit_card": "4111-1111-1111-1111",
    "ssn": "123-45-6789",
    "password": "secret_password",
    "api_key": "sk_test_123456789"
  }
  
  SecureLogger::log_structured(secure_logger, Info, "User activity", sensitive_data)
  
  // Verify sensitive data is masked in logs
  let log_output = SecureLogger::get_recent_logs(secure_logger, 1)
  assert_eq(log_output.length(), 1)
  
  let log_entry = log_output[0]
  assert_true(log_entry.contains("user123"))  // Non-sensitive data should be visible
  assert_false(log_entry.contains("4111-1111-1111-1111"))  // Credit card should be masked
  assert_false(log_entry.contains("123-45-6789"))  // SSN should be masked
  assert_false(log_entry.contains("secret_password"))  // Password should be masked
  assert_false(log_entry.contains("sk_test_123456789"))  // API key should be masked
  
  // Verify masking patterns
  assert_true(log_entry.contains("****-****-****-1111"))  // Masked credit card
  assert_true(log_entry.contains("***-**-6789"))  // Masked SSN
  assert_true(log_entry.contains("**********"))  // Masked password
  assert_true(log_entry.contains("sk_test_****"))  // Masked API key
}

// Test 5: Secure Data Transmission
test "secure data transmission" {
  let secure_client = SecureClient::new()
  
  // Test HTTPS request
  let https_request = SecureRequest::new(
    "GET",
    "https://api.example.com/telemetry",
    [("Authorization", "Bearer token123")],
    None
  )
  
  let https_result = SecureClient::send(secure_client, https_request)
  match https_result {
    Ok(response) => {
      assert_eq(SecureResponse::status_code(response), 200)
      assert_true(SecureResponse::is_secure(response))
    }
    Err(error) => {
      // Network error is acceptable in test environment
      assert_true(error.contains("network") || error.contains("connect"))
    }
  }
  
  // Test HTTP request (should be rejected)
  let http_request = SecureRequest::new(
    "GET",
    "http://api.example.com/telemetry",
    [("Authorization", "Bearer token123")],
    None
  )
  
  let http_result = SecureClient::send(secure_client, http_request)
  match http_result {
    Ok(_) => assert_true(false)  // Should not succeed with HTTP
    Err(error) => assert_true(error.contains("secure") || error.contains("https"))
  }
  
  // Test certificate validation
  let cert_result = SecureClient::validate_certificate(secure_client, "https://expired.badssl.com")
  match cert_result {
    Ok(_) => assert_true(false)  // Should not succeed with expired certificate
    Err(error) => assert_true(error.contains("certificate") || error.contains("expired"))
  }
}

// Test 6: Token-based Authentication
test "token-based authentication" {
  let token_service = TokenService::new()
  
  // Create JWT token
  let claims = {
    "sub": "user123",
    "name": "Test User",
    "roles": ["read", "write"],
    "exp": Time::now() + 3600L  // Expires in 1 hour
  }
  
  let token_result = TokenService::create_jwt(token_service, claims, "secret_key")
  match token_result {
    Ok(token) => {
      assert_true(token.length() > 0)
      
      // Verify token
      let verification_result = TokenService::verify_jwt(token_service, token, "secret_key")
      match verification_result {
        Ok(verified_claims) => {
          assert_eq(verified_claims["sub"], "user123")
          assert_eq(verified_claims["name"], "Test User")
          assert_true(verified_claims["roles"].contains("read"))
          assert_true(verified_claims["roles"].contains("write"))
        }
        Err(_) => assert_true(false)
      }
      
      // Verify token with wrong key (should fail)
      let wrong_verification = TokenService::verify_jwt(token_service, token, "wrong_key")
      match wrong_verification {
        Ok(_) => assert_true(false)  // Should not succeed with wrong key
        Err(_) => assert_true(true)   // Expected to fail
      }
    }
    Err(_) => assert_true(false)
  }
  
  // Create expired token
  let expired_claims = {
    "sub": "user123",
    "exp": Time::now() - 3600L  // Expired 1 hour ago
  }
  
  let expired_token_result = TokenService::create_jwt(token_service, expired_claims, "secret_key")
  match expired_token_result {
    Ok(expired_token) => {
      // Verify expired token (should fail)
      let expired_verification = TokenService::verify_jwt(token_service, expired_token, "secret_key")
      match expired_verification {
        Ok(_) => assert_true(false)  // Should not succeed with expired token
        Err(error) => assert_true(error.contains("expired"))
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 7: Rate Limiting
test "rate limiting" {
  let rate_limiter = RateLimiter::new(10, 60)  // 10 requests per minute
  
  // Test normal usage
  for i in 1..=10 {
    let result = RateLimiter::check_limit(rate_limiter, "client1")
    match result {
      Ok(_) => assert_true(true)  // Should succeed for first 10 requests
      Err(_) => assert_true(false)
    }
  }
  
  // Test limit exceeded
  let exceeded_result = RateLimiter::check_limit(rate_limiter, "client1")
  match exceeded_result {
    Ok(_) => assert_true(false)  // Should not succeed when limit exceeded
    Err(error) => assert_true(error.contains("limit") || error.contains("rate"))
  }
  
  // Test different client (should have separate limits)
  let different_client_result = RateLimiter::check_limit(rate_limiter, "client2")
  match different_client_result {
    Ok(_) => assert_true(true)  // Should succeed for different client
    Err(_) => assert_true(false)
  }
  
  // Test limit reset after time passes (simulate with manual reset)
  RateLimiter::reset(rate_limiter, "client1")
  
  let reset_result = RateLimiter::check_limit(rate_limiter, "client1")
  match reset_result {
    Ok(_) => assert_true(true)  // Should succeed after reset
    Err(_) => assert_true(false)
  }
}

// Test 8: Audit Logging
test "audit logging" {
  let audit_logger = AuditLogger::new()
  
  // Log security events
  AuditLogger::log_security_event(audit_logger, "login_success", "user123", "192.168.1.100")
  AuditLogger::log_security_event(audit_logger, "data_access", "user123", "192.168.1.100", [
    ("resource", "telemetry_data"),
    ("action", "read")
  ])
  AuditLogger::log_security_event(audit_logger, "login_failure", "attacker", "192.168.1.200")
  AuditLogger::log_security_event(audit_logger, "permission_denied", "user456", "192.168.1.100", [
    ("resource", "admin_config"),
    ("action", "write")
  ])
  
  // Query audit logs
  let all_events = AuditLogger::query_events(audit_logger, AuditQuery::all())
  assert_eq(all_events.length(), 4)
  
  // Query by user
  let user_events = AuditLogger::query_events(audit_logger, AuditQuery::by_user("user123"))
  assert_eq(user_events.length(), 2)
  
  // Query by event type
  let login_events = AuditLogger::query_events(audit_logger, AuditQuery::by_event_type("login_success"))
  assert_eq(login_events.length(), 1)
  
  // Query by IP address
  let ip_events = AuditLogger::query_events(audit_logger, AuditQuery::by_ip("192.168.1.100"))
  assert_eq(ip_events.length(), 3)
  
  // Verify event details
  let login_event = login_events[0]
  assert_eq(AuditEvent::event_type(login_event), "login_success")
  assert_eq(AuditEvent::user(login_event), "user123")
  assert_eq(AuditEvent::ip_address(login_event), "192.168.1.100")
  assert_true(AuditEvent::timestamp(login_event) > 0)
}

// Test 9: Data Retention and Purging
test "data retention and purging" {
  let retention_service = DataRetentionService::new()
  
  // Set retention policies
  DataRetentionService::set_policy(retention_service, "telemetry_data", 30)  // 30 days
  DataRetentionService::set_policy(retention_service, "audit_logs", 365)      // 1 year
  DataRetentionService::set_policy(retention_service, "user_data", 2555)      // 7 years
  
  // Add test data with different ages
  let now = Time::now()
  
  // Recent telemetry data (should be retained)
  let recent_telemetry = TelemetryData::new("service", "operation", now, 100L, Ok, [])
  DataRetentionService::store_data(retention_service, "telemetry_data", recent_telemetry)
  
  // Old telemetry data (should be purged)
  let old_telemetry = TelemetryData::new("service", "operation", now - (40 * 86400L), 100L, Ok, [])
  DataRetentionService::store_data(retention_service, "telemetry_data", old_telemetry)
  
  // Recent audit log (should be retained)
  let recent_audit = AuditEvent::new("login_success", "user123", "192.168.1.1", now)
  DataRetentionService::store_data(retention_service, "audit_logs", recent_audit)
  
  // Old audit log (should be retained)
  let old_audit = AuditEvent::new("login_success", "user123", "192.168.1.1", now - (200 * 86400L))
  DataRetentionService::store_data(retention_service, "audit_logs", old_audit)
  
  // Run retention policy
  let purge_result = DataRetentionService::apply_retention_policies(retention_service)
  match purge_result {
    Ok(purge_report) => {
      assert_eq(purge_report.purged_items, 1)  // Only old telemetry should be purged
      assert_eq(purge_report.retained_items, 3)  // Recent telemetry, recent audit, old audit
    }
    Err(_) => assert_true(false)
  }
  
  // Verify purge results
  let remaining_telemetry = DataRetentionService::query_data(retention_service, "telemetry_data")
  assert_eq(remaining_telemetry.length(), 1)  // Only recent telemetry should remain
  
  let remaining_audit = DataRetentionService::query_data(retention_service, "audit_logs")
  assert_eq(remaining_audit.length(), 2)  // Both audit logs should remain
}

// Test 10: Security Scanning and Vulnerability Detection
test "security scanning and vulnerability detection" {
  let security_scanner = SecurityScanner::new()
  
  // Scan telemetry data for potential security issues
  let telemetry_data = TelemetryData::new(
    "web_service",
    "user_login",
    1234567890L,
    1500L,
    Ok,
    [
      ("user.id", StringValue("user123")),
      ("user.ip", StringValue("192.168.1.100")),
      ("user.agent", StringValue("Mozilla/5.0 (Windows NT 10.0; Win64; x64)")),
      ("sql_query", StringValue("SELECT * FROM users WHERE id = 'user123'")),  // Potential SQL injection
      ("request.path", StringValue("/api/users/123")),  // Potential information disclosure
      ("error.message", StringValue("Database connection failed to localhost:5432"))  // Potential information disclosure
    ]
  )
  
  let scan_result = SecurityScanner::scan_telemetry_data(security_scanner, telemetry_data)
  match scan_result {
    Ok(security_report) => {
      // Should detect potential security issues
      assert_true(security_report.issues.length() > 0)
      
      // Check for SQL injection detection
      let has_sql_injection = security_report.issues.any(func(issue) {
        issue.type == "sql_injection"
      })
      assert_true(has_sql_injection)
      
      // Check for information disclosure detection
      let has_info_disclosure = security_report.issues.any(func(issue) {
        issue.type == "information_disclosure"
      })
      assert_true(has_info_disclosure)
      
      // Verify severity levels
      let has_high_severity = security_report.issues.any(func(issue) {
        issue.severity == "high"
      })
      assert_true(has_high_severity)
    }
    Err(_) => assert_true(false)
  }
  
  // Scan system configuration for security issues
  let system_config = {
    "authentication.enabled": true,
    "encryption.enabled": false,  // Security issue
    "tls.version": "1.0",        // Security issue (deprecated)
    "password_policy.min_length": 6,  // Security issue (too short)
    "session.timeout": 3600      // OK
  }
  
  let config_scan_result = SecurityScanner::scan_configuration(security_scanner, system_config)
  match config_scan_result {
    Ok(config_report) => {
      // Should detect configuration security issues
      assert_true(config_report.issues.length() > 0)
      
      // Check for encryption disabled
      let has_encryption_issue = config_report.issues.any(func(issue) {
        issue.type == "encryption_disabled"
      })
      assert_true(has_encryption_issue)
      
      // Check for deprecated TLS version
      let has_tls_issue = config_report.issues.any(func(issue) {
        issue.type == "deprecated_tls"
      })
      assert_true(has_tls_issue)
      
      // Check for weak password policy
      let has_password_issue = config_report.issues.any(func(issue) {
        issue.type == "weak_password_policy"
      })
      assert_true(has_password_issue)
    }
    Err(_) => assert_true(false)
  }
}
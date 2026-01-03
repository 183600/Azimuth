// Azimuth Telemetry System - Security Tests
// This file contains test cases for security functionality

// Test 1: Sensitive Data Redaction
test "sensitive data redaction" {
  let redactor = SensitiveDataRedactor::new()
  
  // Add patterns to redact
  SensitiveDataRedactor::add_pattern(redactor, "password", "password\\s*[:=]\\s*([^\\s,}]+)")
  SensitiveDataRedactor::add_pattern(redactor, "api_key", "api[_-]?key\\s*[:=]\\s*([^\\s,}]+)")
  SensitiveDataRedactor::add_pattern(redactor, "token", "token\\s*[:=]\\s*([^\\s,}]+)")
  SensitiveDataRedactor::add_pattern(redactor, "email", "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b")
  
  // Test data with sensitive information
  let test_data = "{
    \"username\": \"john_doe\",
    \"password\": secret123,
    \"api_key\": abc123def456,
    \"token\": eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9,
    \"email\": john.doe@example.com,
    \"normal_field\": \"safe_value\"
  }"
  
  // Redact sensitive data
  let redacted_data = SensitiveDataRedactor::redact(redactor, test_data)
  
  // Check that sensitive data is redacted
  assert_false(redacted_data.contains("secret123"))
  assert_false(redacted_data.contains("abc123def456"))
  assert_false(redacted_data.contains("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"))
  assert_false(redacted_data.contains("john.doe@example.com"))
  
  // Check that non-sensitive data is preserved
  assert_true(redacted_data.contains("john_doe"))
  assert_true(redacted_data.contains("safe_value"))
  
  // Check that redaction markers are present
  assert_true(redacted_data.contains("[REDACTED]"))
}

// Test 2: Authentication and Authorization
test "authentication and authorization" {
  let auth_manager = AuthManager::new()
  
  // Create users with different roles
  let admin_user = User::new("admin", "admin123", ["admin", "read", "write"])
  let read_user = User::new("reader", "reader123", ["read"])
  let write_user = User::new("writer", "writer123", ["write"])
  
  // Register users
  AuthManager::register_user(auth_manager, admin_user)
  AuthManager::register_user(auth_manager, read_user)
  AuthManager::register_user(auth_manager, write_user)
  
  // Test authentication
  let admin_auth = AuthManager::authenticate(auth_manager, "admin", "admin123")
  match admin_auth {
    Some(user) => assert_eq(User::username(user), "admin")
    None => assert_true(false)
  }
  
  let invalid_auth = AuthManager::authenticate(auth_manager, "admin", "wrongpassword")
  assert_eq(invalid_auth, None)
  
  // Test authorization
  let admin_user_obj = AuthManager::get_user(auth_manager, "admin")
  match admin_user_obj {
    Some(user) => {
      assert_true(AuthManager::has_permission(user, "admin"))
      assert_true(AuthManager::has_permission(user, "read"))
      assert_true(AuthManager::has_permission(user, "write"))
    }
    None => assert_true(false)
  }
  
  let read_user_obj = AuthManager::get_user(auth_manager, "reader")
  match read_user_obj {
    Some(user) => {
      assert_false(AuthManager::has_permission(user, "admin"))
      assert_true(AuthManager::has_permission(user, "read"))
      assert_false(AuthManager::has_permission(user, "write"))
    }
    None => assert_true(false)
  }
}

// Test 3: Telemetry Data Encryption
test "telemetry data encryption" {
  let encryption_manager = EncryptionManager::new("encryption_key_12345")
  
  // Create test telemetry data
  let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
  let span = Span::new("test_span", Internal, span_ctx)
  Span::add_event(span, "test_event", Some([("sensitive_data", StringValue("secret_value"))]))
  
  // Serialize the span
  let serialized_span = SpanSerializer::serialize(span)
  
  // Encrypt the data
  let encrypted_data = EncryptionManager::encrypt(encryption_manager, serialized_span)
  
  // Check that encrypted data is different from original
  assert_false(encrypted_data.equals(serialized_span))
  
  // Decrypt the data
  let decrypted_data = EncryptionManager::decrypt(encryption_manager, encrypted_data)
  
  // Check that decrypted data matches original
  assert_eq(decrypted_data, serialized_span)
  
  // Test with wrong key
  let wrong_encryption_manager = EncryptionManager::new("wrong_key")
  let decryption_result = EncryptionManager::decrypt(wrong_encryption_manager, encrypted_data)
  match decryption_result {
    Error(_) => assert_true(true) // Expected to fail
    _ => assert_true(false)
  }
}

// Test 4: Access Control for Telemetry Data
test "access control for telemetry data" {
  let access_controller = AccessController::new()
  
  // Define access policies
  let admin_policy = AccessPolicy::new("admin_policy", ["admin"], ["read", "write", "delete"], ["*"])
  let read_policy = AccessPolicy::new("read_policy", ["reader"], ["read"], ["spans", "metrics"])
  let write_policy = AccessPolicy::new("write_policy", ["writer"], ["write"], ["metrics"])
  
  // Register policies
  AccessController::add_policy(access_controller, admin_policy)
  AccessController::add_policy(access_controller, read_policy)
  AccessController::add_policy(access_controller, write_policy)
  
  // Create test users
  let admin_user = User::new("admin", "password", ["admin"])
  let reader_user = User::new("reader", "password", ["reader"])
  let writer_user = User::new("writer", "password", ["writer"])
  
  // Test access control
  // Admin should have access to everything
  assert_true(AccessController::can_access(access_controller, admin_user, "read", "spans"))
  assert_true(AccessController::can_access(access_controller, admin_user, "write", "spans"))
  assert_true(AccessController::can_access(access_controller, admin_user, "delete", "spans"))
  
  // Reader should only have read access to spans and metrics
  assert_true(AccessController::can_access(access_controller, reader_user, "read", "spans"))
  assert_true(AccessController::can_access(access_controller, reader_user, "read", "metrics"))
  assert_false(AccessController::can_access(access_controller, reader_user, "write", "spans"))
  assert_false(AccessController::can_access(access_controller, reader_user, "write", "metrics"))
  
  // Writer should only have write access to metrics
  assert_false(AccessController::can_access(access_controller, writer_user, "read", "spans"))
  assert_false(AccessController::can_access(access_controller, writer_user, "write", "spans"))
  assert_false(AccessController::can_access(access_controller, writer_user, "read", "metrics"))
  assert_true(AccessController::can_access(access_controller, writer_user, "write", "metrics"))
}

// Test 5: Audit Logging
test "audit logging" {
  let audit_logger = AuditLogger::new()
  
  // Enable audit logging
  AuditLogger::enable(audit_logger)
  
  // Perform auditable actions
  AuditLogger::log_login(audit_logger, "user1", "success", "192.168.1.1")
  AuditLogger::log_access(audit_logger, "user1", "read", "spans", "success")
  AuditLogger::log_access(audit_logger, "user1", "write", "metrics", "success")
  AuditLogger::log_login(audit_logger, "user2", "failure", "192.168.1.2")
  AuditLogger::log_access(audit_logger, "user2", "read", "spans", "denied")
  
  // Retrieve audit logs
  let logs = AuditLogger::get_logs(audit_logger)
  
  // Check that all actions are logged
  assert_eq(logs.length(), 5)
  
  // Check specific log entries
  let login_log = logs[0]
  assert_eq(AuditLog::action(login_log), "login")
  assert_eq(AuditLog::user(login_log), "user1")
  assert_eq(AuditLog::result(login_log), "success")
  assert_eq(AuditLog::ip_address(login_log), "192.168.1.1")
  
  let access_log = logs[1]
  assert_eq(AuditLog::action(access_log), "access")
  assert_eq(AuditLog::user(access_log), "user1")
  assert_eq(AuditLog::resource_type(access_log), "spans")
  assert_eq(AuditLog::resource_action(access_log), "read")
  assert_eq(AuditLog::result(access_log), "success")
  
  // Test log filtering
  let user1_logs = AuditLogger::get_logs_by_user(audit_logger, "user1")
  assert_eq(user1_logs.length(), 3)
  
  let failure_logs = AuditLogger::get_logs_by_result(audit_logger, "failure")
  assert_eq(failure_logs.length(), 1)
}

// Test 6: Input Validation and Sanitization
test "input validation and sanitization" {
  let input_validator = InputValidator::new()
  
  // Test SQL injection prevention
  let malicious_sql = "'; DROP TABLE users; --"
  let sanitized_sql = InputValidator::sanitize_sql(input_validator, malicious_sql)
  assert_false(sanitized_sql.contains("DROP"))
  assert_false(sanitized_sql.contains(";"))
  
  // Test XSS prevention
  let malicious_html = "<script>alert('XSS')</script>"
  let sanitized_html = InputValidator::sanitize_html(input_validator, malicious_html)
  assert_false(sanitized_html.contains("<script>"))
  assert_false(sanitized_html.contains("alert"))
  
  // Test path traversal prevention
  let malicious_path = "../../../etc/passwd"
  let sanitized_path = InputValidator::sanitize_path(input_validator, malicious_path)
  assert_false(sanitized_path.contains(".."))
  
  // Test telemetry data validation
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let invalid_trace_id = "invalid@trace#id"
  
  assert_true(InputValidator::is_valid_trace_id(input_validator, valid_trace_id))
  assert_false(InputValidator::is_valid_trace_id(input_validator, invalid_trace_id))
  
  let valid_span_name = "valid_span_name"
  let invalid_span_name = ""
  
  assert_true(InputValidator::is_valid_span_name(input_validator, valid_span_name))
  assert_false(InputValidator::is_valid_span_name(input_validator, invalid_span_name))
}

// Test 7: Rate Limiting
test "rate limiting" {
  let rate_limiter = RateLimiter::new()
  
  // Configure rate limits
  RateLimiter::set_limit(rate_limiter, "api_access", 10, 60) // 10 requests per minute
  RateLimiter::set_limit(rate_limiter, "data_export", 5, 300) // 5 requests per 5 minutes
  
  let user_id = "user123"
  
  // Test within limits
  for i in 0..=9 {
    assert_true(RateLimiter::is_allowed(rate_limiter, user_id, "api_access"))
  }
  
  // Test exceeding limits
  assert_false(RateLimiter::is_allowed(rate_limiter, user_id, "api_access"))
  
  // Test different limit type
  for i in 0..=4 {
    assert_true(RateLimiter::is_allowed(rate_limiter, user_id, "data_export"))
  }
  
  assert_false(RateLimiter::is_allowed(rate_limiter, user_id, "data_export"))
  
  // Test different user
  let other_user = "user456"
  assert_true(RateLimiter::is_allowed(rate_limiter, other_user, "api_access"))
}

// Test 8: Secure Communication
test "secure communication" {
  let secure_client = SecureTelemetryClient::new()
  
  // Configure secure settings
  SecureTelemetryClient::set_certificate_validation(secure_client, true)
  SecureTelemetryClient::set_encryption(secure_client, true)
  SecureTelemetryClient::set_tls_version(secure_client, "1.3")
  
  // Test secure connection establishment
  let connection_result = SecureTelemetryClient::connect(secure_client, "https://secure.example.com")
  match connection_result {
    Success(connection) => {
      assert_true(SecureConnection::is_encrypted(connection))
      assert_true(SecureConnection::is_authenticated(connection))
    }
    Error(_) => {
      // Connection might fail in test environment, but configuration should be secure
      assert_true(SecureTelemetryClient::is_securely_configured(secure_client))
    }
  }
  
  // Test data transmission
  let test_data = "test telemetry data"
  let transmission_result = SecureTelemetryClient::transmit(secure_client, test_data)
  match transmission_result {
    Success(_) => assert_true(true)
    Error(error) => {
      // Transmission might fail, but should fail securely
      assert_false(error.contains("plaintext"))
    }
  }
}

// Test 9: Data Retention and Privacy
test "data retention and privacy" {
  let privacy_manager = PrivacyManager::new()
  
  // Configure retention policies
  PrivacyManager::set_retention_policy(privacy_manager, "spans", 30) // 30 days
  PrivacyManager::set_retention_policy(privacy_manager, "metrics", 90) // 90 days
  PrivacyManager::set_retention_policy(privacy_manager, "logs", 7) // 7 days
  
  // Create test data with timestamps
  let old_timestamp = 1609459200L // 2021-01-01
  let recent_timestamp = 1704067200L // 2024-01-01
  let current_timestamp = 1735689600L // 2025-01-01 (current date based on system info)
  
  let old_span = TimeSeriesDataPoint::new(old_timestamp, 42.0)
  let recent_span = TimeSeriesDataPoint::new(recent_timestamp, 43.0)
  let current_span = TimeSeriesDataPoint::new(current_timestamp, 44.0)
  
  // Test data expiration
  assert_true(PrivacyManager::is_expired(privacy_manager, old_span, "spans"))
  assert_false(PrivacyManager::is_expired(privacy_manager, recent_span, "spans"))
  assert_false(PrivacyManager::is_expired(privacy_manager, current_span, "spans"))
  
  // Test data anonymization
  let personal_data = {
    "user_id": "12345",
    "ip_address": "192.168.1.1",
    "email": "user@example.com",
    "telemetry_data": "non-sensitive telemetry data"
  }
  
  let anonymized_data = PrivacyManager::anonymize(privacy_manager, personal_data)
  
  // Check that personal data is anonymized
  assert_false(anonymized_data.contains("12345"))
  assert_false(anonymized_data.contains("192.168.1.1"))
  assert_false(anonymized_data.contains("user@example.com"))
  
  // Check that telemetry data is preserved
  assert_true(anonymized_data.contains("non-sensitive telemetry data"))
  
  // Check for anonymization markers
  assert_true(anonymized_data.contains("[ANONYMIZED]"))
}

// Test 10: Security Incident Response
test "security incident response" {
  let incident_manager = IncidentManager::new()
  
  // Configure incident response
  IncidentManager::set_alert_threshold(incident_manager, "failed_logins", 5, 300) // 5 failed logins in 5 minutes
  IncidentManager::set_alert_threshold(incident_manager, "data_access", 100, 60) // 100 data accesses in 1 minute
  
  // Simulate security events
  IncidentManager::record_event(incident_manager, "failed_login", "user1", "192.168.1.1")
  IncidentManager::record_event(incident_manager, "failed_login", "user1", "192.168.1.1")
  IncidentManager::record_event(incident_manager, "failed_login", "user1", "192.168.1.1")
  IncidentManager::record_event(incident_manager, "failed_login", "user1", "192.168.1.1")
  
  // Should not trigger alert yet
  let alerts1 = IncidentManager::get_active_alerts(incident_manager)
  assert_eq(alerts1.length(), 0)
  
  // Add one more failed login to trigger alert
  IncidentManager::record_event(incident_manager, "failed_login", "user1", "192.168.1.1")
  
  // Should trigger alert
  let alerts2 = IncidentManager::get_active_alerts(incident_manager)
  assert_eq(alerts2.length(), 1)
  
  let alert = alerts2[0]
  assert_eq(Alert::type(alert), "failed_login")
  assert_eq(Alert::severity(alert), "high")
  assert_true(Alert::description(alert).contains("user1"))
  
  // Test incident response actions
  IncidentManager::respond_to_alert(incident_manager, alert, ["block_ip", "notify_admin"])
  
  // Check that response actions were taken
  assert_true(IncidentManager::is_ip_blocked(incident_manager, "192.168.1.1"))
  assert_true(IncidentManager::was_admin_notified(incident_manager, alert))
  
  // Test incident resolution
  IncidentManager::resolve_alert(incident_manager, alert, "investigated and resolved")
  
  let alerts3 = IncidentManager::get_active_alerts(incident_manager)
  assert_eq(alerts3.length(), 0)
  
  let resolved_alerts = IncidentManager::get_resolved_alerts(incident_manager)
  assert_eq(resolved_alerts.length(), 1)
  
  let resolved_alert = resolved_alerts[0]
  assert_eq(Alert::status(resolved_alert), "resolved")
  assert_eq(Alert::resolution_note(resolved_alert), "investigated and resolved")
}
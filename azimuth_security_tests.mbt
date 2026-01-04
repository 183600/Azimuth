// Azimuth Telemetry System - Security Tests
// This file contains comprehensive test cases for security functionality

// Test 1: Authentication and Authorization
test "authentication and authorization" {
  // Create authentication manager
  let auth_manager = AuthenticationManager::new()
  
  // Register users with different roles
  let admin_user = User::new("admin", "secure_password_hash", [Role::Admin])
  let operator_user = User::new("operator", "operator_password_hash", [Role::Operator])
  let viewer_user = User::new("viewer", "viewer_password_hash", [Role::Viewer])
  
  AuthenticationManager::register_user(auth_manager, admin_user)
  AuthenticationManager::register_user(auth_manager, operator_user)
  AuthenticationManager::register_user(auth_manager, viewer_user)
  
  // Test authentication
  let admin_auth = AuthenticationManager::authenticate(auth_manager, "admin", "secure_password_hash")
  match admin_auth {
    Ok(user) => assert_eq(user.username, "admin")
    Err(_) => assert_true(false)
  }
  
  // Test authentication failure
  let failed_auth = AuthenticationManager::authenticate(auth_manager, "admin", "wrong_password")
  match failed_auth {
    Ok(_) => assert_true(false)
    Err(_) => assert_true(true)
  }
  
  // Test authorization
  let auth_context = AuthenticationManager::create_context(auth_manager, admin_user)
  
  // Admin should have all permissions
  assert_true(AuthorizationManager::has_permission(auth_context, Permission::ReadTelemetry))
  assert_true(AuthorizationManager::has_permission(auth_context, Permission::WriteTelemetry))
  assert_true(AuthorizationManager::has_permission(auth_context, Permission::DeleteTelemetry))
  assert_true(AuthorizationManager::has_permission(auth_context, Permission::ManageUsers))
  
  // Operator should have limited permissions
  let operator_context = AuthenticationManager::create_context(auth_manager, operator_user)
  assert_true(AuthorizationManager::has_permission(operator_context, Permission::ReadTelemetry))
  assert_true(AuthorizationManager::has_permission(operator_context, Permission::WriteTelemetry))
  assert_false(AuthorizationManager::has_permission(operator_context, Permission::DeleteTelemetry))
  assert_false(AuthorizationManager::has_permission(operator_context, Permission::ManageUsers))
  
  // Viewer should only have read permissions
  let viewer_context = AuthenticationManager::create_context(auth_manager, viewer_user)
  assert_true(AuthorizationManager::has_permission(viewer_context, Permission::ReadTelemetry))
  assert_false(AuthorizationManager::has_permission(viewer_context, Permission::WriteTelemetry))
  assert_false(AuthorizationManager::has_permission(viewer_context, Permission::DeleteTelemetry))
  assert_false(AuthorizationManager::has_permission(viewer_context, Permission::ManageUsers))
  
  // Test token-based authentication
  let token = AuthenticationManager::generate_token(auth_manager, admin_user)
  assert_true(token.length() > 0)
  
  let token_user = AuthenticationManager::validate_token(auth_manager, token)
  match token_user {
    Ok(user) => assert_eq(user.username, "admin")
    Err(_) => assert_true(false)
  }
  
  // Test token expiration
  let short_lived_token = AuthenticationManager::generate_token_with_expiry(auth_manager, admin_user, 1) // 1 second
  Thread::sleep(1100) // Wait for token to expire
  
  let expired_token_user = AuthenticationManager::validate_token(auth_manager, short_lived_token)
  match expired_token_user {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error.message, "Token expired")
  }
}

// Test 2: Data Encryption and Decryption
test "data encryption and decryption" {
  // Create encryption manager
  let encryption_manager = EncryptionManager::new()
  
  // Generate encryption key
  let key = EncryptionManager::generate_key(encryption_manager, EncryptionAlgorithm::AES256)
  assert_eq(key.length(), 32) // 256 bits = 32 bytes
  
  // Test symmetric encryption
  let plaintext = "Sensitive telemetry data"
  let encrypted_data = EncryptionManager::encrypt_symmetric(encryption_manager, plaintext, key)
  
  // Verify encrypted data is different from plaintext
  assert_true(encrypted_data != plaintext)
  
  // Decrypt and verify
  let decrypted_data = EncryptionManager::decrypt_symmetric(encryption_manager, encrypted_data, key)
  assert_eq(decrypted_data, plaintext)
  
  // Test with different key should fail
  let wrong_key = EncryptionManager::generate_key(encryption_manager, EncryptionAlgorithm::AES256)
  let failed_decryption = EncryptionManager::decrypt_symmetric(encryption_manager, encrypted_data, wrong_key)
  match failed_decryption {
    Ok(_) => assert_true(false)
    Err(_) => assert_true(true)
  }
  
  // Test asymmetric encryption
  let key_pair = EncryptionManager::generate_key_pair(encryption_manager, EncryptionAlgorithm::RSA2048)
  
  let public_key = key_pair.public_key
  let private_key = key_pair.private_key
  
  // Encrypt with public key
  let asymmetric_encrypted = EncryptionManager::encrypt_asymmetric(encryption_manager, plaintext, public_key)
  
  // Decrypt with private key
  let asymmetric_decrypted = EncryptionManager::decrypt_asymmetric(encryption_manager, asymmetric_encrypted, private_key)
  assert_eq(asymmetric_decrypted, plaintext)
  
  // Test with wrong key should fail
  let wrong_key_pair = EncryptionManager::generate_key_pair(encryption_manager, EncryptionAlgorithm::RSA2048)
  let failed_asymmetric_decryption = EncryptionManager::decrypt_asymmetric(
    encryption_manager, 
    asymmetric_encrypted, 
    wrong_key_pair.private_key
  )
  match failed_asymmetric_decryption {
    Ok(_) => assert_true(false)
    Err(_) => assert_true(true)
  }
  
  // Test telemetry data encryption
  let telemetry_data = TelemetryData::with_attributes("sensitive_metric", 42.0, "count", 1234567890L, [
    ("user.id", "12345"),
    ("api.key", "secret_api_key"),
    ("session.token", "session_token_value")
  ])
  
  let encrypted_telemetry = EncryptionManager::encrypt_telemetry(encryption_manager, telemetry_data, key)
  
  // Verify sensitive attributes are encrypted
  assert_true(encrypted_telemetry.metric_name == "sensitive_metric") // Non-sensitive data remains
  assert_true(encrypted_telemetry.value == 42.0)
  
  // Check that sensitive attributes are encrypted
  let encrypted_user_id = TelemetryData::get_attribute(encrypted_telemetry, "user.id")
  match encrypted_user_id {
    Some(StringValue(value)) => assert_true(value != "12345") // Should be encrypted
    _ => assert_true(false)
  }
  
  // Decrypt telemetry data
  let decrypted_telemetry = EncryptionManager::decrypt_telemetry(encryption_manager, encrypted_telemetry, key)
  
  // Verify decrypted data matches original
  assert_eq(decrypted_telemetry.metric_name, telemetry_data.metric_name)
  assert_eq(decrypted_telemetry.value, telemetry_data.value)
  
  let decrypted_user_id = TelemetryData::get_attribute(decrypted_telemetry, "user.id")
  match decrypted_user_id {
    Some(StringValue(value)) => assert_eq(value, "12345") // Should be decrypted
    _ => assert_true(false)
  }
}

// Test 3: Data Masking and Anonymization
test "data masking and anonymization" {
  // Create data masking manager
  let masking_manager = MaskingManager::new()
  
  // Configure masking rules
  MaskingManager::add_rule(masking_manager, "user.email", MaskingType::Email)
  MaskingManager::add_rule(masking_manager, "user.phone", MaskingType::Phone)
  MaskingManager::add_rule(masking_manager, "credit_card", MaskingType::CreditCard)
  MaskingManager::add_rule(masking_manager, "ssn", MaskingType::SSN)
  MaskingManager::add_rule(masking_manager, "ip_address", MaskingType::IPAddress)
  
  // Create telemetry data with sensitive information
  let sensitive_data = TelemetryData::with_attributes("user_activity", 1.0, "count", 1234567890L, [
    ("user.email", "john.doe@example.com"),
    ("user.phone", "+1-555-123-4567"),
    ("credit_card", "4532-1234-5678-9012"),
    ("ssn", "123-45-6789"),
    ("ip_address", "192.168.1.100"),
    ("user.name", "John Doe"), // No masking rule
    ("session.id", "sess_12345") // No masking rule
  ])
  
  // Apply masking
  let masked_data = MaskingManager::mask(masking_manager, sensitive_data)
  
  // Verify masked data
  let masked_email = TelemetryData::get_attribute(masked_data, "user.email")
  match masked_email {
    Some(StringValue(value)) => assert_eq(value, "j***.***@e*****.com")
    _ => assert_true(false)
  }
  
  let masked_phone = TelemetryData::get_attribute(masked_data, "user.phone")
  match masked_phone {
    Some(StringValue(value)) => assert_eq(value, "+*-***-***-****")
    _ => assert_true(false)
  }
  
  let masked_credit_card = TelemetryData::get_attribute(masked_data, "credit_card")
  match masked_credit_card {
    Some(StringValue(value)) => assert_eq(value, "****-****-****-9012")
    _ => assert_true(false)
  }
  
  let masked_ssn = TelemetryData::get_attribute(masked_data, "ssn")
  match masked_ssn {
    Some(StringValue(value)) => assert_eq(value, "***-**-6789")
    _ => assert_true(false)
  }
  
  let masked_ip = TelemetryData::get_attribute(masked_data, "ip_address")
  match masked_ip {
    Some(StringValue(value)) => assert_eq(value, "192.168.1.***")
    _ => assert_true(false)
  }
  
  // Verify unmasks data remains unchanged
  let unchanged_name = TelemetryData::get_attribute(masked_data, "user.name")
  match unchanged_name {
    Some(StringValue(value)) => assert_eq(value, "John Doe")
    _ => assert_true(false)
  }
  
  let unchanged_session = TelemetryData::get_attribute(masked_data, "session.id")
  match unchanged_session {
    Some(StringValue(value)) => assert_eq(value, "sess_12345")
    _ => assert_true(false)
  }
  
  // Test anonymization
  let anonymized_data = MaskingManager::anonymize(masking_manager, sensitive_data)
  
  // Verify anonymized data has no direct identifiers
  let anonymized_name = TelemetryData::get_attribute(anonymized_data, "user.name")
  match anonymized_name {
    Some(StringValue(value)) => assert_true(value != "John Doe")
    _ => assert_true(false)
  }
  
  // Verify consistent anonymization (same input produces same output)
  let anonymized_data2 = MaskingManager::anonymize(masking_manager, sensitive_data)
  let anonymized_name2 = TelemetryData::get_attribute(anonymized_data2, "user.name")
  match (anonymized_name, anonymized_name2) {
    (Some(StringValue(value1)), Some(StringValue(value2))) => assert_eq(value1, value2)
    _ => assert_true(false)
  }
}

// Test 4: Access Control and Audit Logging
test "access control and audit logging" {
  // Create access control manager
  let access_manager = AccessControlManager::new()
  
  // Create audit logger
  let audit_logger = AuditLogger::new()
  
  // Create users with different permissions
  let admin_user = User::new("admin", "hash", [Role::Admin])
  let viewer_user = User::new("viewer", "hash", [Role::Viewer])
  
  // Define resource
  let telemetry_resource = Resource::new("telemetry_data", ResourceScope::Organization)
  
  // Test admin access (should succeed)
  let admin_access_result = AccessControlManager::check_access(
    access_manager,
    admin_user,
    telemetry_resource,
    Permission::ReadTelemetry
  )
  assert_true(admin_access_result.allowed)
  
  // Log admin access
  AuditLogger::log_access(
    audit_logger,
    admin_user.username,
    telemetry_resource.id,
    Permission::ReadTelemetry,
    true,
    "Admin access granted"
  )
  
  // Test viewer access (should succeed for read)
  let viewer_read_result = AccessControlManager::check_access(
    access_manager,
    viewer_user,
    telemetry_resource,
    Permission::ReadTelemetry
  )
  assert_true(viewer_read_result.allowed)
  
  // Log viewer read access
  AuditLogger::log_access(
    audit_logger,
    viewer_user.username,
    telemetry_resource.id,
    Permission::ReadTelemetry,
    true,
    "Viewer read access granted"
  )
  
  // Test viewer write access (should fail)
  let viewer_write_result = AccessControlManager::check_access(
    access_manager,
    viewer_user,
    telemetry_resource,
    Permission::WriteTelemetry
  )
  assert_false(viewer_write_result.allowed)
  
  // Log denied access
  AuditLogger::log_access(
    audit_logger,
    viewer_user.username,
    telemetry_resource.id,
    Permission::WriteTelemetry,
    false,
    "Viewer write access denied"
  )
  
  // Query audit logs
  let admin_logs = AuditLogger::query_by_user(audit_logger, "admin")
  assert_eq(admin_logs.length(), 1)
  assert_eq(admin_logs[0].username, "admin")
  assert_eq(admin_logs[0].resource_id, telemetry_resource.id)
  assert_eq(admin_logs[0].permission, Permission::ReadTelemetry)
  assert_true(admin_logs[0].access_granted)
  
  let viewer_logs = AuditLogger::query_by_user(audit_logger, "viewer")
  assert_eq(viewer_logs.length(), 2)
  
  let denied_logs = AuditLogger::query_denied_access(audit_logger)
  assert_eq(denied_logs.length(), 1)
  assert_eq(denied_logs[0].username, "viewer")
  assert_false(denied_logs[0].access_granted)
  
  // Test time-based query
  let now = Time::current_timestamp()
  let recent_logs = AuditLogger::query_by_time_range(audit_logger, now - 3600, now) // Last hour
  assert_eq(recent_logs.length(), 3) // All logs should be recent
}

// Test 5: Input Validation and Sanitization
test "input validation and sanitization" {
  // Create input validator
  let validator = InputValidator::new()
  
  // Define validation rules
  Validator::add_rule(validator, "metric_name", ValidationRule::Required)
  Validator::add_rule(validator, "metric_name", ValidationRule::MaxLength(50))
  Validator::add_rule(validator, "metric_name", ValidationRule::Regex("^[a-zA-Z0-9_.-]+$"))
  
  Validator::add_rule(validator, "value", ValidationRule::Required)
  Validator::add_rule(validator, "value", ValidationRule::Numeric)
  Validator::add_rule(validator, "value", ValidationRule::Range(0.0, 1000.0))
  
  Validator::add_rule(validator, "unit", ValidationRule::AllowedValues(["ms", "count", "percent", "bytes"]))
  
  // Test valid input
  let valid_input = [
    ("metric_name", "response_time"),
    ("value", "42.5"),
    ("unit", "ms")
  ]
  
  let validation_result = Validator::validate(validator, valid_input)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // Test invalid input
  let invalid_input = [
    ("metric_name", ""), // Empty (violates Required)
    ("value", "not_a_number"), // Not numeric
    ("unit", "invalid_unit") // Not in allowed values
  ]
  
  let validation_result = Validator::validate(validator, invalid_input)
  assert_false(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 3)
  
  // Test SQL injection prevention
  let malicious_input = [
    ("metric_name", "test; DROP TABLE metrics; --"),
    ("value", "42.5"),
    ("unit", "ms")
  ]
  
  let validation_result = Validator::validate(validator, malicious_input)
  assert_false(validation_result.is_valid)
  
  // Test XSS prevention
  let xss_input = [
    ("metric_name", "<script>alert('xss')</script>"),
    ("value", "42.5"),
    ("unit", "ms")
  ]
  
  let validation_result = Validator::validate(validator, xss_input)
  assert_false(validation_result.is_valid)
  
  // Test input sanitization
  let unsanitized_input = "test <script>alert('xss')</script>"
  let sanitized_input = Validator::sanitize(validator, unsanitized_input)
  assert_eq(sanitized_input, "test &lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;")
  
  // Test telemetry data validation
  let telemetry_validator = TelemetryValidator::new()
  
  let valid_telemetry = TelemetryData::new("valid_metric", 42.5, "ms", 1234567890L)
  let telemetry_validation_result = TelemetryValidator::validate(telemetry_validator, valid_telemetry)
  assert_true(telemetry_validation_result.is_valid)
  
  let invalid_telemetry = TelemetryData::new("", -1.0, "invalid", -1L)
  let telemetry_validation_result = TelemetryValidator::validate(telemetry_validator, invalid_telemetry)
  assert_false(telemetry_validation_result.is_valid)
}

// Test 6: Rate Limiting and DDoS Protection
test "rate limiting and ddos protection" {
  // Create rate limiter
  let rate_limiter = RateLimiter::new()
  
  // Configure rate limits
  RateLimiter::set_limit(rate_limiter, "api_read", 100, 60) // 100 requests per minute
  RateLimiter::set_limit(rate_limiter, "api_write", 10, 60)  // 10 requests per minute
  RateLimiter::set_limit(rate_limiter, "auth", 5, 300)       // 5 requests per 5 minutes
  
  // Test normal usage (within limits)
  let user_id = "user123"
  
  for i in 0..=50 {
    let result = RateLimiter::check_limit(rate_limiter, user_id, "api_read")
    assert_true(result.allowed)
    assert_true(result.remaining_requests > 0)
  }
  
  // Test exceeding limits
  for i in 0..=60 {
    RateLimiter::check_limit(rate_limiter, user_id, "api_read")
  }
  
  let exceeded_result = RateLimiter::check_limit(rate_limiter, user_id, "api_read")
  assert_false(exceeded_result.allowed)
  assert_eq(exceeded_result.remaining_requests, 0)
  assert_true(exceeded_result.reset_time > Time::current_timestamp())
  
  // Test different limits for different operations
  for i in 0..=5 {
    let result = RateLimiter::check_limit(rate_limiter, user_id, "api_write")
    assert_true(result.allowed)
  }
  
  for i in 0..=5 {
    RateLimiter::check_limit(rate_limiter, user_id, "api_write")
  }
  
  let write_exceeded_result = RateLimiter::check_limit(rate_limiter, user_id, "api_write")
  assert_false(write_exceeded_result.allowed)
  
  // Test different users have independent limits
  let another_user = "user456"
  let another_user_result = RateLimiter::check_limit(rate_limiter, another_user, "api_read")
  assert_true(another_user_result.allowed) // Should be allowed as this user hasn't made requests
  
  // Test rate limit recovery after time window
  let mock_time = Time::current_timestamp() + 61 // Advance time by 61 seconds
  RateLimiter::set_mock_time(rate_limiter, mock_time)
  
  let recovered_result = RateLimiter::check_limit(rate_limiter, user_id, "api_read")
  assert_true(recovered_result.allowed) // Should be allowed after time window reset
  
  // Test DDoS detection
  let ddos_detector = DDoSDetector::new()
  
  // Configure DDoS detection
  DDoSDetector::set_threshold(ddos_detector, 1000, 60) // 1000 requests per minute triggers alert
  
  // Simulate normal traffic
  for i in 0..=100 {
    DDoSDetector::record_request(ddos_detector, "192.168.1.100")
  }
  
  let ddos_status = DDoSDetector::check_status(ddos_detector)
  assert_false(ddos_status.is_under_attack)
  
  // Simulate potential DDoS attack
  for i in 0..=1000 {
    DDoSDetector::record_request(ddos_detector, "192.168.1.200")
  }
  
  ddos_status = DDoSDetector::check_status(ddos_detector)
  assert_true(ddos_status.is_under_attack)
  assert_true(ddos_status.suspicious_ips.contains("192.168.1.200"))
}

// Test 7: Secure Communication
test "secure communication" {
  // Create secure communication manager
  let secure_comm = SecureCommunicationManager::new()
  
  // Generate SSL/TLS certificates
  let server_cert = SecureCommunicationManager::generate_certificate(secure_comm, "server.example.com")
  let client_cert = SecureCommunicationManager::generate_certificate(secure_comm, "client.example.com")
  
  // Create secure server
  let secure_server = SecureServer::new(server_cert)
  
  // Create secure client
  let secure_client = SecureClient::new(client_cert)
  
  // Test TLS handshake
  let handshake_result = SecureClient::connect(secure_client, "localhost", 8443)
  assert_true(handshake_result.success)
  
  // Test secure data transmission
  let telemetry_data = TelemetryData::new("secure_metric", 42.0, "count", 1234567890L)
  
  let encrypted_transmission = SecureClient::send_data(secure_client, telemetry_data)
  assert_true(encrypted_transmission.success)
  
  // Test mutual authentication
  let mutual_auth_server = SecureServer::new_with_mutual_auth(server_cert)
  let mutual_auth_client = SecureClient::new_with_cert(client_cert)
  
  let mutual_auth_result = SecureClient::connect_with_auth(
    mutual_auth_client, 
    "localhost", 
    8443,
    client_cert
  )
  assert_true(mutual_auth_result.success)
  
  // Test certificate validation
  let valid_cert_result = SecureCommunicationManager::validate_certificate(
    secure_comm,
    server_cert,
    "server.example.com"
  )
  assert_true(valid_cert_result.is_valid)
  
  let invalid_cert_result = SecureCommunicationManager::validate_certificate(
    secure_comm,
    server_cert,
    "wrong.domain.com"
  )
  assert_false(invalid_cert_result.is_valid)
  
  // Test secure channel establishment
  let secure_channel = SecureChannel::establish(secure_client, secure_server)
  assert_true(secure_channel.is_established)
  
  // Test data integrity verification
  let original_data = "important telemetry data"
  let transmitted_data = SecureChannel::transmit(secure_channel, original_data)
  let received_data = SecureChannel::receive(secure_channel)
  
  assert_eq(original_data, received_data)
  
  // Test tampering detection
  let tampered_data = SecureChannel::transmit(secure_channel, original_data)
  // Simulate tampering by modifying the data
  let tampered_data = tampered_data.substring(0, tampered_data.length() - 1) + "X"
  
  let tampered_result = SecureChannel::receive_with_verification(secure_channel, tampered_data)
  match tampered_result {
    Ok(_) => assert_true(false) // Should fail
    Err(error) => assert_eq(error.message, "Data integrity check failed")
  }
}

// Test 8: Security Monitoring and Incident Response
test "security monitoring and incident response" {
  // Create security monitor
  let security_monitor = SecurityMonitor::new()
  
  // Configure security alerts
  SecurityMonitor::add_alert_rule(security_monitor, SecurityAlertType::FailedLogin, 5, 300) // 5 failed logins in 5 minutes
  SecurityMonitor::add_alert_rule(security_monitor, SecurityAlertType::UnauthorizedAccess, 3, 300) // 3 unauthorized access attempts in 5 minutes
  SecurityMonitor::add_alert_rule(security_monitor, SecurityAlertType::DataExfiltration, 1, 60) // Any data exfiltration attempt
  
  // Simulate security events
  for i in 0..=3 {
    SecurityMonitor::record_event(security_monitor, SecurityEvent::new(
      SecurityEventType::FailedLogin,
      "user123",
      "192.168.1.100",
      Time::current_timestamp()
    ))
  }
  
  // Check if alert should be triggered (should not yet)
  let alerts = SecurityMonitor::check_alerts(security_monitor)
  assert_eq(alerts.length(), 0)
  
  // Add more failed login attempts to trigger alert
  for i in 0..=2 {
    SecurityMonitor::record_event(security_monitor, SecurityEvent::new(
      SecurityEventType::FailedLogin,
      "user123",
      "192.168.1.100",
      Time::current_timestamp()
    ))
  }
  
  // Check if alert is triggered
  alerts = SecurityMonitor::check_alerts(security_monitor)
  assert_eq(alerts.length(), 1)
  assert_eq(alerts[0].alert_type, SecurityAlertType::FailedLogin)
  assert_eq(alerts[0].severity, AlertSeverity::High)
  
  // Test incident response
  let incident_manager = IncidentManager::new()
  
  // Create incident from alert
  let incident = IncidentManager::create_from_alert(incident_manager, alerts[0])
  assert_eq(incident.status, IncidentStatus::Open)
  assert_eq(incident.severity, IncidentSeverity::High)
  
  // Assign incident to responder
  IncidentManager::assign(incident_manager, incident.id, "responder123")
  let updated_incident = IncidentManager::get_incident(incident_manager, incident.id)
  assert_eq(updated_incident.assigned_to, "responder123")
  
  // Add incident response actions
  IncidentManager::add_action(incident_manager, incident.id, "Block IP address 192.168.1.100")
  IncidentManager::add_action(incident_manager, incident.id, "Reset user123 password")
  IncidentManager::add_action(incident_manager, incident.id, "Notify security team")
  
  let incident_actions = IncidentManager::get_actions(incident_manager, incident.id)
  assert_eq(incident_actions.length(), 3)
  
  // Resolve incident
  IncidentManager::resolve(incident_manager, incident.id, "Account secured and IP blocked")
  let resolved_incident = IncidentManager::get_incident(incident_manager, incident.id)
  assert_eq(resolved_incident.status, IncidentStatus::Resolved)
  
  // Test security metrics
  let security_metrics = SecurityMonitor::get_metrics(security_monitor)
  assert_true(security_metrics.total_events > 0)
  assert_true(security_metrics.total_alerts > 0)
  assert_true(security_metrics.failed_login_attempts >= 6)
  assert_true(security_metrics.blocked_ip_addresses.contains("192.168.1.100"))
  
  // Test security report generation
  let report = SecurityMonitor::generate_report(security_monitor, TimeRange::last_24_hours())
  assert_true(report.contains("Security Incidents Report"))
  assert_true(report.contains("Failed Login Attempts"))
  assert_true(report.contains("Blocked IP Addresses"))
}
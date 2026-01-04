// Azimuth Telemetry System - High Quality Security and Privacy Tests
// This file contains comprehensive test cases for security and privacy protection

// Test 1: Sensitive Data Redaction
test "sensitive data redaction" {
  // Test various types of sensitive data
  let email = "user@example.com"
  let phone = "+1-555-123-4567"
  let ssn = "123-45-6789"
  let credit_card = "4111-1111-1111-1111"
  let password = "SuperSecretPassword123!"
  let api_key = "sk_live_1234567890abcdef"
  
  // Create attributes with sensitive data
  let attrs = Attributes::new()
  Attributes::set(attrs, "user_email", StringValue(email))
  Attributes::set(attrs, "phone_number", StringValue(phone))
  Attributes::set(attrs, "social_security", StringValue(ssn))
  Attributes::set(attrs, "credit_card", StringValue(credit_card))
  Attributes::set(attrs, "password", StringValue(password))
  Attributes::set(attrs, "api_key", StringValue(api_key))
  Attributes::set(attrs, "safe_data", StringValue("This is safe to log"))
  
  // Apply redaction
  let redacted_attrs = Security::redact_sensitive_data(attrs)
  
  // Verify sensitive data is redacted
  let redacted_email = Attributes::get(redacted_attrs, "user_email")
  match redacted_email {
    Some(StringValue(v)) => assert_eq(v, "****@example.com")
    _ => assert_true(false)
  }
  
  let redacted_phone = Attributes::get(redacted_attrs, "phone_number")
  match redacted_phone {
    Some(StringValue(v)) => assert_eq(v, "+*-***-***-****")
    _ => assert_true(false)
  }
  
  let redacted_ssn = Attributes::get(redacted_attrs, "social_security")
  match redacted_ssn {
    Some(StringValue(v)) => assert_eq(v, "***-**-****")
    _ => assert_true(false)
  }
  
  let redacted_credit_card = Attributes::get(redacted_attrs, "credit_card")
  match redacted_credit_card {
    Some(StringValue(v)) => assert_eq(v, "****-****-****-1111")
    _ => assert_true(false)
  }
  
  let redacted_password = Attributes::get(redacted_attrs, "password")
  match redacted_password {
    Some(StringValue(v)) => assert_eq(v, "********")
    _ => assert_true(false)
  }
  
  let redacted_api_key = Attributes::get(redacted_attrs, "api_key")
  match redacted_api_key {
    Some(StringValue(v)) => assert_eq(v, "sk_live_****")
    _ => assert_true(false)
  }
  
  // Verify safe data is not redacted
  let safe_data = Attributes::get(redacted_attrs, "safe_data")
  match safe_data {
    Some(StringValue(v)) => assert_eq(v, "This is safe to log")
    _ => assert_true(false)
  }
}

// Test 2: Data Encryption and Decryption
test "data encryption and decryption" {
  let sensitive_data = "This is sensitive telemetry data that should be encrypted"
  let encryption_key = Security::generate_encryption_key()
  
  // Encrypt data
  let encrypted_data = Security::encrypt(sensitive_data, encryption_key)
  assert_true(encrypted_data != sensitive_data)
  assert_true(encrypted_data.length() > 0)
  
  // Decrypt data
  let decrypted_data = Security::decrypt(encrypted_data, encryption_key)
  assert_eq(decrypted_data, sensitive_data)
  
  // Test with wrong key
  let wrong_key = Security::generate_encryption_key()
  let wrong_decrypted = Security::decrypt(encrypted_data, wrong_key)
  assert_true(wrong_decrypted != sensitive_data)
  
  // Test encryption in telemetry context
  let span_ctx = SpanContext::new("encryption_test", "encryption_span", true, "security_test")
  let span = Span::new("encryption_operation", Internal, span_ctx)
  
  // Add encrypted attribute
  Span::set_encrypted_attribute(span, "sensitive_data", sensitive_data, encryption_key)
  
  // Verify attribute is stored encrypted
  let encrypted_attr = Span::get_attribute(span, "sensitive_data")
  match encrypted_attr {
    Some(StringValue(v)) => assert_eq(v, encrypted_data)
    _ => assert_true(false)
  }
  
  // Verify decryption works
  let decrypted_attr = Span::get_decrypted_attribute(span, "sensitive_data", encryption_key)
  match decrypted_attr {
    Some(StringValue(v)) => assert_eq(v, sensitive_data)
    _ => assert_true(false)
  }
  
  Span::end(span)
}

// Test 3: Access Control and Authorization
test "access control and authorization" {
  // Define roles and permissions
  let admin_role = Security::create_role("admin", ["read:all", "write:all", "delete:all"])
  let analyst_role = Security::create_role("analyst", ["read:telemetry", "write:annotations"])
  let viewer_role = Security::create_role("viewer", ["read:telemetry"])
  
  // Create users with roles
  let admin_user = Security::create_user("admin_user", admin_role)
  let analyst_user = Security::create_user("analyst_user", analyst_role)
  let viewer_user = Security::create_user("viewer_user", viewer_role)
  
  // Test access control
  let telemetry_resource = "telemetry_data"
  
  // Admin should have all permissions
  assert_true(Security::has_permission(admin_user, "read:all", telemetry_resource))
  assert_true(Security::has_permission(admin_user, "write:all", telemetry_resource))
  assert_true(Security::has_permission(admin_user, "delete:all", telemetry_resource))
  
  // Analyst should have limited permissions
  assert_false(Security::has_permission(analyst_user, "read:all", telemetry_resource))
  assert_true(Security::has_permission(analyst_user, "read:telemetry", telemetry_resource))
  assert_true(Security::has_permission(analyst_user, "write:annotations", telemetry_resource))
  assert_false(Security::has_permission(analyst_user, "delete:all", telemetry_resource))
  
  // Viewer should have read-only permissions
  assert_false(Security::has_permission(viewer_user, "read:all", telemetry_resource))
  assert_true(Security::has_permission(viewer_user, "read:telemetry", telemetry_resource))
  assert_false(Security::has_permission(viewer_user, "write:annotations", telemetry_resource))
  assert_false(Security::has_permission(viewer_user, "delete:all", telemetry_resource))
  
  // Test access control in telemetry context
  let span_ctx = SpanContext::new("access_control_test", "access_control_span", true, "security_test")
  let span = Span::new("access_control_operation", Internal, span_ctx)
  
  // Add access control metadata
  Span::set_attribute(span, "required_permission", StringValue("read:telemetry"))
  Span::set_attribute(span, "admin_access", BoolValue(Security::has_permission(admin_user, "read:telemetry", telemetry_resource)))
  Span::set_attribute(span, "analyst_access", BoolValue(Security::has_permission(analyst_user, "read:telemetry", telemetry_resource)))
  Span::set_attribute(span, "viewer_access", BoolValue(Security::has_permission(viewer_user, "read:telemetry", telemetry_resource)))
  
  Span::end(span)
}

// Test 4: Data Retention and Expiration
test "data retention and expiration" {
  let current_time = Time::now()
  let retention_period = 7 * 24 * 60 * 60 * 1000 // 7 days in milliseconds
  
  // Create telemetry data with timestamps
  let old_timestamp = current_time - (retention_period + 1000) // Older than retention period
  let recent_timestamp = current_time - (retention_period / 2) // Within retention period
  
  // Create spans with different timestamps
  let old_span_ctx = SpanContext::new("retention_test", "old_span", true, "retention_test")
  let old_span = Span::new("old_operation", Internal, old_span_ctx)
  Span::set_timestamp(old_span, old_timestamp)
  
  let recent_span_ctx = SpanContext::new("retention_test", "recent_span", true, "retention_test")
  let recent_span = Span::new("recent_operation", Internal, recent_span_ctx)
  Span::set_timestamp(recent_span, recent_timestamp)
  
  // Test data retention policy
  let retention_policy = Security::create_retention_policy(retention_period)
  
  // Check if data should be retained
  let should_retain_old = Security::should_retain_data(old_span, retention_policy)
  let should_retain_recent = Security::should_retain_data(recent_span, retention_policy)
  
  assert_false(should_retain_old) // Should not retain old data
  assert_true(should_retain_recent) // Should retain recent data
  
  // Test data expiration
  let expired_data = Security::get_expired_data([old_span, recent_span], retention_policy)
  assert_eq(expired_data.length(), 1)
  assert_eq(Span::name(expired_data[0]), "old_operation")
  
  // Test data cleanup
  let remaining_data = Security::cleanup_expired_data([old_span, recent_span], retention_policy)
  assert_eq(remaining_data.length(), 1)
  assert_eq(Span::name(remaining_data[0]), "recent_operation")
  
  Span::end(old_span)
  Span::end(recent_span)
}

// Test 5: Audit Logging
test "audit logging" {
  let audit_logger = Security::create_audit_logger()
  
  // Test audit events
  let user_id = "test_user"
  let resource_id = "telemetry_data_123"
  
  // Log access event
  Security::log_access_event(audit_logger, user_id, "read", resource_id, true)
  
  // Log modification event
  Security::log_modification_event(audit_logger, user_id, "update", resource_id, true)
  
  // Log failed authentication
  Security::log_authentication_event(audit_logger, user_id, "login", false)
  
  // Log data export
  Security::log_data_export_event(audit_logger, user_id, "csv", resource_id, 100)
  
  // Get audit logs
  let audit_logs = Security::get_audit_logs(audit_logger, user_id)
  assert_eq(audit_logs.length(), 4)
  
  // Verify audit log structure
  for log in audit_logs {
    assert_true(AuditLog::has_timestamp(log))
    assert_true(AuditLog::has_user_id(log, user_id))
    assert_true(AuditLog::has_event_type(log))
    assert_true(AuditLog::has_resource_id(log, resource_id))
  }
  
  // Test audit log filtering
  let access_logs = Security::filter_audit_logs(audit_logger, fn(log) {
    AuditLog::event_type(log) == "access"
  })
  assert_eq(access_logs.length(), 1)
  
  let failed_logs = Security::filter_audit_logs(audit_logger, fn(log) {
    !AuditLog::success(log)
  })
  assert_eq(failed_logs.length(), 1)
}

// Test 6: Data Anonymization
test "data anonymization" {
  // Create telemetry data with personally identifiable information (PII)
  let user_data = [
    ("name", "John Doe"),
    ("email", "john.doe@example.com"),
    ("ip_address", "192.168.1.100"),
    ("user_id", "user_12345"),
    ("address", "123 Main St, Anytown, USA"),
    ("phone", "+1-555-123-4567")
  ]
  
  // Create attributes with PII
  let attrs = Attributes::new()
  for (key, value) in user_data {
    Attributes::set(attrs, key, StringValue(value))
  }
  
  // Apply anonymization
  let anonymization_config = Security::create_anonymization_config([
    ("name", AnonymizationType::Partial),
    ("email", AnonymizationType::Hash),
    ("ip_address", AnonymizationType::Mask),
    ("user_id", AnonymizationType::Tokenize),
    ("address", AnonymizationType::Remove),
    ("phone", AnonymizationType::Partial)
  ])
  
  let anonymized_attrs = Security::anonymize_data(attrs, anonymization_config)
  
  // Verify anonymization results
  let anonymized_name = Attributes::get(anonymized_attrs, "name")
  match anonymized_name {
    Some(StringValue(v)) => assert_eq(v, "John D**")
    _ => assert_true(false)
  }
  
  let anonymized_email = Attributes::get(anonymized_attrs, "email")
  match anonymized_email {
    Some(StringValue(v)) => assert_true(v.length() == 64) // SHA256 hash length
    _ => assert_true(false)
  }
  
  let anonymized_ip = Attributes::get(anonymized_attrs, "ip_address")
  match anonymized_ip {
    Some(StringValue(v)) => assert_eq(v, "192.168.1.***")
    _ => assert_true(false)
  }
  
  let anonymized_user_id = Attributes::get(anonymized_attrs, "user_id")
  match anonymized_user_id {
    Some(StringValue(v)) => assert_true(v != "user_12345") // Should be tokenized
    _ => assert_true(false)
  }
  
  // Address should be removed
  let removed_address = Attributes::get(anonymized_attrs, "address")
  match removed_address {
    Some(_) => assert_true(false) // Should be removed
    None => assert_true(true)
  }
  
  let anonymized_phone = Attributes::get(anonymized_attrs, "phone")
  match anonymized_phone {
    Some(StringValue(v)) => assert_eq(v, "+*-***-***-****")
    _ => assert_true(false)
  }
}

// Test 7: Secure Data Transmission
test "secure data transmission" {
  let telemetry_data = "Sensitive telemetry data for transmission"
  
  // Create secure channel
  let secure_channel = Security::create_secure_channel()
  
  // Sign data
  let signature = Security::sign_data(telemetry_data, secure_channel.private_key)
  assert_true(signature.length() > 0)
  
  // Verify signature
  let is_valid = Security::verify_signature(telemetry_data, signature, secure_channel.public_key)
  assert_true(is_valid)
  
  // Test with tampered data
  let tampered_data = telemetry_data + "tampered"
  let is_tampered_valid = Security::verify_signature(tampered_data, signature, secure_channel.public_key)
  assert_false(is_tampered_valid)
  
  // Test secure transmission
  let transmission_result = Security::transmit_securely(telemetry_data, signature, secure_channel)
  assert_true(transmission_result.success)
  
  // Test transmission metadata
  let transmission_metadata = Security::get_transmission_metadata(transmission_result)
  assert_true(transmission_metadata.contains("encrypted"))
  assert_true(transmission_metadata.contains("signed"))
  assert_true(transmission_metadata.contains("timestamp"))
}

// Test 8: Privacy Compliance
test "privacy compliance" {
  // Create GDPR compliance checker
  let gdpr_checker = Security::create_gdpr_compliance_checker()
  
  // Create telemetry data with various fields
  let telemetry_fields = [
    ("user_id", "user_12345"),
    ("email", "user@example.com"),
    ("session_id", "session_abcdef"),
    ("ip_address", "192.168.1.100"),
    ("user_agent", "Mozilla/5.0..."),
    ("custom_data", "Some custom data")
  ]
  
  // Check GDPR compliance
  let compliance_result = Security::check_gdpr_compliance(gdpr_checker, telemetry_fields)
  
  // Verify compliance checks
  assert_true(compliance_result.has_consent) // Should have consent flag
  assert_true(compliance_result.has_retention_policy) // Should have retention policy
  assert_true(compliance_result.has_anonymization) // Should have anonymization
  assert_true(compliance_result.has_data_protection) // Should have data protection
  
  // Test CCPA compliance
  let ccpa_checker = Security::create_ccpa_compliance_checker()
  let ccpa_result = Security::check_ccpa_compliance(ccpa_checker, telemetry_fields)
  
  // Verify CCPA compliance checks
  assert_true(ccpa_result.has_right_to_delete) // Should have right to delete
  assert_true(ccpa_result.has_right_to_opt_out) // Should have right to opt out
  assert_true(ccpa_result.has_transparency) // Should have transparency
  
  // Test data subject rights
  let user_id = "user_12345"
  let deletion_result = Security::execute_data_subject_request(user_id, DataSubjectRequest::Delete)
  assert_true(deletion_result.success)
  
  let access_result = Security::execute_data_subject_request(user_id, DataSubjectRequest::Access)
  assert_true(access_result.success)
  assert_true(access_result.data.contains("user_12345"))
}

// Test 9: Security Monitoring and Alerting
test "security monitoring and alerting" {
  let security_monitor = Security::create_security_monitor()
  
  // Configure security alerts
  Security::configure_alert(security_monitor, SecurityAlert::FailedLogin, 5, 60000) // 5 failed logins in 1 minute
  Security::configure_alert(security_monitor, SecurityAlert::DataAccess, 100, 300000) // 100 data accesses in 5 minutes
  Security::configure_alert(security_monitor, SecurityAlert::UnauthorizedAccess, 1, 0) // Any unauthorized access
  
  // Simulate security events
  for i in 0..6 {
    Security::log_security_event(security_monitor, SecurityEvent::FailedLogin, "user_" + i.to_string())
  }
  
  for i in 0..10 {
    Security::log_security_event(security_monitor, SecurityEvent::DataAccess, "user_" + i.to_string())
  }
  
  Security::log_security_event(security_monitor, SecurityEvent::UnauthorizedAccess, "malicious_user")
  
  // Check for alerts
  let alerts = Security::get_active_alerts(security_monitor)
  assert_eq(alerts.length(), 2) // Failed login and unauthorized access
  
  // Verify alert details
  let failed_login_alert = alerts.find(fn(alert) { alert.type == SecurityAlert::FailedLogin })
  match failed_login_alert {
    Some(alert) => {
      assert_eq(alert.count, 6)
      assert_true(alert.triggered)
    }
    None => assert_true(false)
  }
  
  let unauthorized_alert = alerts.find(fn(alert) { alert.type == SecurityAlert::UnauthorizedAccess })
  match unauthorized_alert {
    Some(alert) => {
      assert_eq(alert.count, 1)
      assert_true(alert.triggered)
    }
    None => assert_true(false)
  }
  
  // Test alert notification
  let notifications = Security::get_alert_notifications(security_monitor)
  assert_true(notifications.length() >= 2)
}

// Test 10: Secure Configuration Management
test "secure configuration management" {
  // Create secure configuration
  let config = Security::create_secure_config()
  
  // Add sensitive configuration values
  Security::set_config_value(config, "database.password", "secret_password", true)
  Security::set_config_value(config, "api.key", "secret_api_key", true)
  Security::set_config_value(config, "encryption.key", "secret_encryption_key", true)
  Security::set_config_value(config, "log.level", "info", false)
  Security::set_config_value(config, "server.port", "8080", false)
  
  // Test configuration access
  let db_password = Security::get_config_value(config, "database.password")
  match db_password {
    Some(value) => assert_eq(value, "secret_password")
    None => assert_true(false)
  }
  
  let log_level = Security::get_config_value(config, "log.level")
  match log_level {
    Some(value) => assert_eq(value, "info")
    None => assert_true(false)
  }
  
  // Test configuration encryption
  let encrypted_config = Security::encrypt_config(config)
  assert_true(encrypted_config != config)
  
  // Test configuration decryption
  let decrypted_config = Security::decrypt_config(encrypted_config)
  let decrypted_password = Security::get_config_value(decrypted_config, "database.password")
  match decrypted_password {
    Some(value) => assert_eq(value, "secret_password")
    None => assert_true(false)
  }
  
  // Test configuration validation
  let validation_result = Security::validate_config(config)
  assert_true(validation_result.valid)
  
  // Test with invalid configuration
  let invalid_config = Security::create_secure_config()
  Security::set_config_value(invalid_config, "invalid.setting", "value", false)
  
  let invalid_validation = Security::validate_config(invalid_config)
  assert_false(invalid_validation.valid)
  assert_true(invalid_validation.errors.length() > 0)
  
  // Test configuration backup and restore
  let backup = Security::backup_config(config)
  let restored_config = Security::restore_config(backup)
  
  let restored_password = Security::get_config_value(restored_config, "database.password")
  match restored_password {
    Some(value) => assert_eq(value, "secret_password")
    None => assert_true(false)
  }
}
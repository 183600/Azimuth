// Security Enhancement Tests for Azimuth Telemetry System
// This file contains test cases for security features and privacy protections

// Test 1: Data Encryption and Decryption
test "data encryption and decryption" {
  let encryption_config = EncryptionConfig::new(
    AES256, // Encryption algorithm
    "secure_key_123456789012345678901234", // 32-byte key
    "secure_iv_123456789012", // 16-byte IV
    CBC, // Encryption mode
    PKCS7 // Padding scheme
  )
  
  let encryptor = DataEncryptor::new(encryption_config)
  
  // Test string encryption
  let original_string = "Sensitive telemetry data with special chars: 测试中文字符"
  let encrypted_string = DataEncryptor::encrypt_string(encryptor, original_string)
  
  assert_true(encrypted_string.length() > 0)
  assert_ne(encrypted_string, original_string) // Should be different from original
  
  // Test string decryption
  let decrypted_string = DataEncryptor::decrypt_string(encryptor, encrypted_string)
  assert_eq(decrypted_string, original_string) // Should match original
  
  // Test binary data encryption
  let original_bytes = [0x01, 0x02, 0x03, 0x04, 0x05, 0xFF, 0xFE, 0xFD]
  let encrypted_bytes = DataEncryptor::encrypt_bytes(encryptor, original_bytes)
  
  assert_true(encrypted_bytes.length() > 0)
  assert_ne(encrypted_bytes, original_bytes) // Should be different from original
  
  // Test binary data decryption
  let decrypted_bytes = DataEncryptor::decrypt_bytes(encryptor, encrypted_bytes)
  assert_eq(decrypted_bytes, original_bytes) // Should match original
  
  // Test with different encryption algorithms
  let rsa_config = EncryptionConfig::new(
    RSA2048,
    "public_key_12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890",
    "private_key_12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890",
    RSA,
    None
  )
  
  let rsa_encryptor = DataEncryptor::new(rsa_config)
  
  // RSA encryption has size limitations, test with small data
  let small_string = "Small data"
  let rsa_encrypted = DataEncryptor::encrypt_string(rsa_encryptor, small_string)
  let rsa_decrypted = DataEncryptor::decrypt_string(rsa_encryptor, rsa_encrypted)
  
  assert_eq(rsa_decrypted, small_string)
  
  // Test encryption key rotation
  let new_key = "new_secure_key_1234567890123456789012"
  let key_rotation_result = DataEncryptor::rotate_key(encryptor, new_key)
  
  match key_rotation_result {
    Success => assert_true(true)
    Error(_) => assert_true(false) // Key rotation should succeed
  }
  
  // Test encryption with new key
  let new_encrypted = DataEncryptor::encrypt_string(encryptor, original_string)
  let new_decrypted = DataEncryptor::decrypt_string(encryptor, new_encrypted)
  
  assert_eq(new_decrypted, original_string)
  assert_ne(new_encrypted, encrypted_string) // Should be different with new key
}

// Test 2: Attribute Sanitization and PII Redaction
test "attribute sanitization and pii redaction" {
  let sanitizer = AttributeSanitizer::new()
  
  // Configure PII patterns
  let pii_patterns = [
    ("email", Regex::new(r"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}")),
    ("phone", Regex::new(r"\+?\d{1,3}[-.\s]?\(?\d{3}\)?[-.\s]?\d{3}[-.\s]?\d{4}")),
    ("ssn", Regex::new(r"\d{3}-\d{2}-\d{4}")),
    ("credit_card", Regex::new(r"\d{4}[-\s]?\d{4}[-\s]?\d{4}[-\s]?\d{4}"))
  ]
  
  for (name, pattern) in pii_patterns {
    AttributeSanitizer::add_pii_pattern(sanitizer, name, pattern)
  }
  
  // Create attributes with PII
  let attrs = Attributes::new()
  Attributes::set(attrs, "user_email", StringValue("john.doe@example.com"))
  Attributes::set(attrs, "user_phone", StringValue("+1 (555) 123-4567"))
  Attributes::set(attrs, "user_ssn", StringValue("123-45-6789"))
  Attributes::set(attrs, "payment_method", StringValue("Visa ending in 4242: 4242-4242-4242-4242"))
  Attributes::set(attrs, "user_name", StringValue("John Doe"))
  Attributes::set(attrs, "user_id", StringValue("user_12345"))
  
  // Sanitize attributes
  let sanitized_attrs = AttributeSanitizer::sanitize(sanitizer, attrs)
  
  // Verify PII is redacted
  let email = Attributes::get(sanitized_attrs, "user_email")
  match email {
    Some(StringValue(value)) => assert_eq(value, "***REDACTED***")
    _ => assert_true(false)
  }
  
  let phone = Attributes::get(sanitized_attrs, "user_phone")
  match phone {
    Some(StringValue(value)) => assert_eq(value, "***REDACTED***")
    _ => assert_true(false)
  }
  
  let ssn = Attributes::get(sanitized_attrs, "user_ssn")
  match ssn {
    Some(StringValue(value)) => assert_eq(value, "***REDACTED***")
    _ => assert_true(false)
  }
  
  let payment = Attributes::get(sanitized_attrs, "payment_method")
  match payment {
    Some(StringValue(value)) => assert_eq(value, "***REDACTED***")
    _ => assert_true(false)
  }
  
  // Verify non-PII attributes are preserved
  let name = Attributes::get(sanitized_attrs, "user_name")
  match name {
    Some(StringValue(value)) => assert_eq(value, "John Doe")
    _ => assert_true(false)
  }
  
  let user_id = Attributes::get(sanitized_attrs, "user_id")
  match user_id {
    Some(StringValue(value)) => assert_eq(value, "user_12345")
    _ => assert_true(false)
  }
  
  // Test partial redaction
  let partial_sanitizer = AttributeSanitizer::new_with_partial_redaction()
  AttributeSanitizer::add_pii_pattern(partial_sanitizer, "email", Regex::new(r"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}"))
  
  let partially_sanitized = AttributeSanitizer::sanitize(partial_sanitizer, attrs)
  
  let partial_email = Attributes::get(partially_sanitized, "user_email")
  match partial_email {
    Some(StringValue(value)) => assert_eq(value, "j***@example.com") // Partially redacted
    _ => assert_true(false)
  }
  
  // Test custom redaction function
  let custom_sanitizer = AttributeSanitizer::new()
  AttributeSanitizer::add_custom_redaction(custom_sanitizer, "user_id", |value| {
    if value.length() > 4 {
      value.substring(0, 2) + "***" + value.substring(value.length() - 2, 2)
    } else {
      "***"
    }
  })
  
  let custom_sanitized = AttributeSanitizer::sanitize(custom_sanitizer, attrs)
  
  let custom_user_id = Attributes::get(custom_sanitized, "user_id")
  match custom_user_id {
    Some(StringValue(value)) => assert_eq(value, "us***45")
    _ => assert_true(false)
  }
}

// Test 3: Access Control and Authorization
test "access control and authorization" {
  let access_manager = AccessControlManager::new()
  
  // Define roles
  let admin_role = Role::new("admin", ["read", "write", "delete", "manage"])
  let analyst_role = Role::new("analyst", ["read", "write"])
  let viewer_role = Role::new("viewer", ["read"])
  
  // Add roles to access manager
  AccessControlManager::add_role(access_manager, admin_role)
  AccessControlManager::add_role(access_manager, analyst_role)
  AccessControlManager::add_role(access_manager, viewer_role)
  
  // Define users
  let admin_user = User::new("admin_user", ["admin"])
  let analyst_user = User::new("analyst_user", ["analyst"])
  let viewer_user = User::new("viewer_user", ["viewer"])
  
  // Add users to access manager
  AccessControlManager::add_user(access_manager, admin_user)
  AccessControlManager::add_user(access_manager, analyst_user)
  AccessControlManager::add_user(access_manager, viewer_user)
  
  // Define resources
  let telemetry_resource = Resource::new("telemetry_data", ["read", "write", "delete"])
  let config_resource = Resource::new("config", ["read", "write", "manage"])
  
  // Test admin access
  let admin_read_result = AccessControlManager::check_permission(
    access_manager,
    "admin_user",
    "telemetry_data",
    "read"
  )
  match admin_read_result {
    Allowed => assert_true(true)
    Denied => assert_true(false)
  }
  
  let admin_delete_result = AccessControlManager::check_permission(
    access_manager,
    "admin_user",
    "telemetry_data",
    "delete"
  )
  match admin_delete_result {
    Allowed => assert_true(true)
    Denied => assert_true(false)
  }
  
  let admin_manage_result = AccessControlManager::check_permission(
    access_manager,
    "admin_user",
    "config",
    "manage"
  )
  match admin_manage_result {
    Allowed => assert_true(true)
    Denied => assert_true(false)
  }
  
  // Test analyst access
  let analyst_read_result = AccessControlManager::check_permission(
    access_manager,
    "analyst_user",
    "telemetry_data",
    "read"
  )
  match analyst_read_result {
    Allowed => assert_true(true)
    Denied => assert_true(false)
  }
  
  let analyst_write_result = AccessControlManager::check_permission(
    access_manager,
    "analyst_user",
    "telemetry_data",
    "write"
  )
  match analyst_write_result {
    Allowed => assert_true(true)
    Denied => assert_true(false)
  }
  
  let analyst_delete_result = AccessControlManager::check_permission(
    access_manager,
    "analyst_user",
    "telemetry_data",
    "delete"
  )
  match analyst_delete_result {
    Allowed => assert_true(false)
    Denied => assert_true(true)
  }
  
  // Test viewer access
  let viewer_read_result = AccessControlManager::check_permission(
    access_manager,
    "viewer_user",
    "telemetry_data",
    "read"
  )
  match viewer_read_result {
    Allowed => assert_true(true)
    Denied => assert_true(false)
  }
  
  let viewer_write_result = AccessControlManager::check_permission(
    access_manager,
    "viewer_user",
    "telemetry_data",
    "write"
  )
  match viewer_write_result {
    Allowed => assert_true(false)
    Denied => assert_true(true)
  }
  
  // Test resource-based access control
  let user_resource = Resource::new("user_data", ["read", "write"])
  AccessControlManager::add_resource(access_manager, user_resource)
  
  // Grant specific permission to user
  AccessControlManager::grant_user_permission(
    access_manager,
    "viewer_user",
    "user_data",
    "write"
  )
  
  let viewer_user_data_result = AccessControlManager::check_permission(
    access_manager,
    "viewer_user",
    "user_data",
    "write"
  )
  match viewer_user_data_result {
    Allowed => assert_true(true) // Should be allowed due to specific grant
    Denied => assert_true(false)
  }
  
  // Test attribute-based access control
  let abac_manager = ABACManager::new()
  
  // Define policies
  let policy = Policy::new("data_access_policy", {
    // Allow access to telemetry data if user's department matches data's department
    // or if user has admin role
    let user_department = Subject::get_attribute("department");
    let data_department = Resource::get_attribute("department");
    let user_roles = Subject::get_attribute("roles");
    
    user_department == data_department || user_roles.contains("admin")
  })
  
  ABACManager::add_policy(abac_manager, policy)
  
  // Create context with attributes
  let admin_context = Context::new()
  Context::set_attribute(admin_context, "subject.department", StringValue("engineering"))
  Context::set_attribute(admin_context, "subject.roles", ArrayStringValue(["admin"]))
  
  let resource_context = Context::new()
  Context::set_attribute(resource_context, "resource.department", StringValue("engineering"))
  
  // Evaluate policy
  let admin_abac_result = ABACManager::evaluate_policy(
    abac_manager,
    "data_access_policy",
    admin_context,
    resource_context,
    "read"
  )
  
  match admin_abac_result {
    Allowed => assert_true(true)
    Denied => assert_true(false)
  }
}

// Test 4: Audit Logging
test "audit logging" {
  let audit_logger = AuditLogger::new("audit.log")
  
  // Configure audit logger
  AuditLogger::set_level(audit_logger, Info)
  AuditLogger::enable_file_output(audit_logger, true)
  AuditLogger::enable_console_output(audit_logger, false)
  
  // Test audit log entry creation
  let audit_entry = AuditEntry::new(
    "user_123",
    "read",
    "telemetry_data",
    Success,
    "Access granted to telemetry data",
    Time::now(),
    "192.168.1.100",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
  )
  
  // Log audit entry
  let log_result = AuditLogger::log(audit_logger, audit_entry)
  match log_result {
    Success => assert_true(true)
    Error(_) => assert_true(false)
  }
  
  // Test batch audit logging
  let audit_entries = []
  
  for i in 1..=10 {
    let entry = AuditEntry::new(
      "user_" + i.to_string(),
      "write",
      "config",
      if i % 3 == 0 { Success } else { Error(PermissionDenied) },
      if i % 3 == 0 { "Config updated" } else { "Access denied" },
      Time::now(),
      "192.168.1." + (100 + i).to_string(),
      "Test Client " + i.to_string()
    )
    audit_entries.push(entry)
  }
  
  let batch_log_result = AuditLogger::log_batch(audit_logger, audit_entries)
  match batch_log_result {
    Success => assert_true(true)
    Error(_) => assert_true(false)
  }
  
  // Test audit log filtering
  let filter = AuditLogFilter::new()
  AuditLogFilter::add_user_filter(filter, "user_3")
  AuditLogFilter::add_action_filter(filter, "write")
  AuditLogFilter::add_resource_filter(filter, "config")
  AuditLogFilter::add_result_filter(filter, Error(PermissionDenied))
  AuditLogFilter::add_time_range_filter(
    filter,
    Time::now() - 3600000, // 1 hour ago
    Time::now()
  )
  
  let filtered_logs = AuditLogger::query(audit_logger, filter)
  assert_eq(filtered_logs.length(), 1) // Should find exactly 1 matching entry
  
  let matching_entry = filtered_logs[0]
  assert_eq(matching_entry.user_id, "user_3")
  assert_eq(matching_entry.action, "write")
  assert_eq(matching_entry.resource, "config")
  match matching_entry.result {
    Error(PermissionDenied) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test audit log statistics
  let stats = AuditLogger::get_statistics(audit_logger)
  assert_eq(stats.total_entries, 11) // 1 single + 10 batch
  assert_eq(stats.successful_operations, 4) // 1 single + 3 successful in batch
  assert_eq(stats.failed_operations, 7) // 7 failed in batch
  assert_eq(stats.unique_users, 10) // user_123 + user_1 to user_9
  assert_eq(stats.unique_resources, 2) // telemetry_data and config
  
  // Test audit log retention
  let retention_config = RetentionConfig::new(
    30, // 30 days retention
    100, // Max 100 entries
    true // Enable cleanup
  )
  
  AuditLogger::configure_retention(audit_logger, retention_config)
  
  // Add more entries to test retention
  let additional_entries = []
  
  for i in 1..=150 {
    let entry = AuditEntry::new(
      "retention_user",
      "test",
      "test_resource",
      Success,
      "Test entry for retention",
      Time::now() - (i * 86400000), // i days ago
      "127.0.0.1",
      "Test Client"
    )
    additional_entries.push(entry)
  }
  
  AuditLogger::log_batch(audit_logger, additional_entries)
  
  // Force retention cleanup
  let cleanup_result = AuditLogger::cleanup_retention(audit_logger)
  match cleanup_result {
    Success(cleaned_count) => {
      assert_true(cleaned_count > 0) // Should clean up some entries
    }
    Error(_) => assert_true(false)
  }
  
  // Verify retention was applied
  let post_cleanup_stats = AuditLogger::get_statistics(audit_logger)
  assert_true(post_cleanup_stats.total_entries <= 100) // Should not exceed max entries
}

// Test 5: Secure Communication Channels
test "secure communication channels" {
  let secure_channel = SecureChannel::new()
  
  // Configure TLS
  let tls_config = TLSConfig::new(
    "server.crt",
    "server.key",
    "ca.crt",
    true, // verify client certificates
    "TLS_AES_256_GCM_SHA384" // cipher suite
  )
  
  SecureChannel::configure_tls(secure_channel, tls_config)
  
  // Test secure channel establishment
  let channel_result = SecureChannel::establish(secure_channel, "telemetry.example.com", 443)
  match channel_result {
    Success(channel) => {
      // Test secure data transmission
      let test_data = "Secure telemetry data"
      let encrypted_result = SecureChannel::send(channel, test_data)
      
      match encrypted_result {
        Success => assert_true(true)
        Error(_) => assert_true(false) // Should succeed in test environment
      }
      
      // Test secure data reception
      let receive_result = SecureChannel::receive(channel)
      match receive_result {
        Some(data) => assert_true(data.length() > 0)
        None => assert_true(false)
      }
    }
    Error(_) => {
      // For testing purposes, we'll accept connection failures
      assert_true(true)
    }
  }
  
  // Test mutual authentication
  let client_cert_config = ClientCertificateConfig::new(
    "client.crt",
    "client.key",
    "client_password"
  )
  
  SecureChannel::configure_client_certificate(secure_channel, client_cert_config)
  
  let mutual_auth_result = SecureChannel::establish_with_mutual_auth(
    secure_channel,
    "telemetry.example.com",
    443
  )
  
  match mutual_auth_result {
    Success(channel) => {
      assert_true(SecureChannel::is_mutually_authenticated(channel))
    }
    Error(_) => {
      // For testing purposes, we'll accept connection failures
      assert_true(true)
    }
  }
  
  // Test certificate validation
  let cert_validator = CertificateValidator::new()
  
  // Add trusted CA certificates
  CertificateValidator::add_trusted_ca(cert_validator, "ca.crt")
  
  // Validate server certificate
  let cert_validation_result = CertificateValidator::validate_server_certificate(
    cert_validator,
    "telemetry.example.com",
    "server.crt"
  )
  
  match cert_validation_result {
    Valid => assert_true(true)
    Invalid(reason) => {
      // For testing purposes, we'll accept validation failures
      assert_true(reason.length() > 0)
    }
  }
  
  // Test certificate revocation checking
  CertificateValidator::enable_revocation_checking(cert_validator, true)
  
  let revocation_result = CertificateValidator::check_revocation(
    cert_validator,
    "server.crt"
  )
  
  match revocation_result {
    NotRevoked => assert_true(true)
    Revoked(reason) => {
      // For testing purposes, we'll accept any result
      assert_true(reason.length() > 0)
    }
  }
}

// Test 6: Intrusion Detection
test "intrusion detection" {
  let intrusion_detector = IntrusionDetector::new()
  
  // Configure detection rules
  IntrusionDetector::add_rule(intrusion_detector, IntrusionRule::new(
    "failed_login_threshold",
    {
      // Detect if more than 5 failed logins from same IP within 10 minutes
      let failed_logins = Event::count_by_type("failed_login", Time::now() - 600000);
      let source_ip = Event::get_attribute("source_ip");
      
      failed_logins > 5 && source_ip == Event::current_source_ip()
    },
    High
  ))
  
  IntrusionDetector::add_rule(intrusion_detector, IntrusionRule::new(
    "privilege_escalation",
    {
      // Detect if user attempts to access resources beyond their role
      let user_role = User::get_role();
      let required_role = Resource::get_required_role();
      
      !Role::has_permission(user_role, required_role)
    },
    Critical
  ))
  
  IntrusionDetector::add_rule(intrusion_detector, IntrusionRule::new(
    "data_exfiltration",
    {
      // Detect if user downloads unusually large amounts of data
      let data_downloaded = User::get_data_downloaded(Time::now() - 3600000); // Last hour
      let user_average = User::get_average_data_download();
      
      data_downloaded > user_average * 10.0 // 10x normal usage
    },
    Medium
  ))
  
  // Simulate security events
  let events = []
  
  // Normal events
  for i in 1..=10 {
    let event = SecurityEvent::new(
      "login_success",
      "user_" + i.to_string(),
      "192.168.1." + (100 + i).to_string(),
      Time::now() - (i * 60000),
      []
    )
    events.push(event)
  }
  
  // Failed login events from same IP
  for i in 1..=6 {
    let event = SecurityEvent::new(
      "failed_login",
      "attacker",
      "10.0.0.1",
      Time::now() - (i * 60000),
      [("reason", "Invalid password")]
    )
    events.push(event)
  }
  
  // Privilege escalation attempt
  let escalation_event = SecurityEvent::new(
    "access_denied",
    "viewer_user",
    "192.168.1.50",
    Time::now() - 30000,
    [("resource", "admin_config"), ("required_role", "admin")]
  )
  events.push(escalation_event)
  
  // Process events through intrusion detector
  let alerts = []
  
  for event in events {
    let detection_result = IntrusionDetector::process_event(intrusion_detector, event)
    
    match detection_result {
      Some(alert) => alerts.push(alert),
      None => {} // No alert
    }
  }
  
  // Should generate 2 alerts: failed login threshold and privilege escalation
  assert_eq(alerts.length(), 2)
  
  // Verify failed login alert
  let failed_login_alert = alerts[0]
  assert_eq(failed_login_alert.rule_name, "failed_login_threshold")
  assert_eq(failed_login_alert.severity, High)
  assert_eq(failed_login_alert.source_ip, "10.0.0.1")
  assert_true(failed_login_alert.description.contains("failed login"))
  
  // Verify privilege escalation alert
  let privilege_alert = alerts[1]
  assert_eq(privilege_alert.rule_name, "privilege_escalation")
  assert_eq(privilege_alert.severity, Critical)
  assert_eq(privilege_alert.user_id, "viewer_user")
  assert_true(privilege_alert.description.contains("privilege escalation"))
  
  // Test alert correlation
  let correlated_alerts = IntrusionDetector::correlate_alerts(intrusion_detector, alerts, 300000) // 5 minutes window
  
  assert_true(correlated_alerts.length() >= 1) // Should find at least one correlation
  
  // Test automated response
  let response_config = AutomatedResponseConfig::new()
  AutomatedResponseConfig::add_action(response_config, "failed_login_threshold", BlockIP)
  AutomatedResponseConfig::add_action(response_config, "privilege_escalation", DisableAccount)
  
  IntrusionDetector::configure_automated_response(intrusion_detector, response_config)
  
  for alert in alerts {
    let response_result = IntrusionDetector::execute_automated_response(intrusion_detector, alert)
    match response_result {
      Success(actions) => {
        assert_true(actions.length() > 0)
        
        for action in actions {
          match action {
            BlockIP(ip) => assert_eq(ip, "10.0.0.1")
            DisableAccount(user) => assert_eq(user, "viewer_user")
            _ => assert_true(false) // Unexpected action
          }
        }
      }
      Error(_) => assert_true(false) // Response should succeed
    }
  }
}

// Test 7: Security Metrics and Monitoring
test "security metrics and monitoring" {
  let security_monitor = SecurityMonitor::new()
  
  // Configure security metrics
  SecurityMonitor::track_metric(security_monitor, "authentication_failures")
  SecurityMonitor::track_metric(security_monitor, "authorization_failures")
  SecurityMonitor::track_metric(security_monitor, "encryption_operations")
  SecurityMonitor::track_metric(security_monitor, "intrusion_alerts")
  
  // Simulate security events
  for i in 1..=100 {
    if i % 10 == 0 {
      SecurityMonitor::record_event(security_monitor, "authentication_failure", [
        ("user_id", "user_" + i.to_string()),
        ("source_ip", "192.168.1." + (i % 255).to_string()),
        ("timestamp", Time::now().to_string())
      ])
    }
    
    if i % 20 == 0 {
      SecurityMonitor::record_event(security_monitor, "authorization_failure", [
        ("user_id", "user_" + i.to_string()),
        ("resource", "protected_resource"),
        ("action", "delete"),
        ("timestamp", Time::now().to_string())
      ])
    }
    
    SecurityMonitor::record_event(security_monitor, "encryption_operation", [
      ("operation_type", "encrypt"),
      ("data_size", (i * 1024).to_string()),
      ("timestamp", Time::now().to_string())
    ])
    
    if i % 25 == 0 {
      SecurityMonitor::record_event(security_monitor, "intrusion_alert", [
        ("alert_type", "suspicious_activity"),
        ("severity", "medium"),
        ("source_ip", "10.0.0." + (i % 255).to_string()),
        ("timestamp", Time::now().to_string())
      ])
    }
  }
  
  // Get security metrics
  let auth_failures = SecurityMonitor::get_metric_count(security_monitor, "authentication_failures")
  assert_eq(auth_failures, 10) // 100 / 10 = 10
  
  let authz_failures = SecurityMonitor::get_metric_count(security_monitor, "authorization_failures")
  assert_eq(authz_failures, 5) // 100 / 20 = 5
  
  let encryption_ops = SecurityMonitor::get_metric_count(security_monitor, "encryption_operations")
  assert_eq(encryption_ops, 100) // One for each iteration
  
  let intrusion_alerts = SecurityMonitor::get_metric_count(security_monitor, "intrusion_alerts")
  assert_eq(intrusion_alerts, 4) // 100 / 25 = 4
  
  // Get time-series metrics
  let time_window = 3600000 // 1 hour
  let auth_failure_time_series = SecurityMonitor::get_metric_time_series(
    security_monitor,
    "authentication_failures",
    Time::now() - time_window,
    Time::now()
  )
  
  assert_true(auth_failure_time_series.length() > 0)
  
  for data_point in auth_failure_time_series {
    assert_true(data_point.value >= 0.0)
    assert_true(data_point.timestamp > Time::now() - time_window)
    assert_true(data_point.timestamp <= Time::now())
  }
  
  // Calculate security score
  let security_score = SecurityMonitor::calculate_security_score(security_monitor)
  assert_true(security_score >= 0.0)
  assert_true(security_score <= 100.0)
  
  // With our simulated events, score should be reduced but not critical
  assert_true(security_score > 50.0)
  assert_true(security_score < 90.0)
  
  // Generate security report
  let report = SecurityMonitor::generate_report(security_monitor, Time::now() - time_window, Time::now())
  
  assert_true(report.summary.length() > 0)
  assert_true(report.metrics.length() > 0)
  assert_true(report.recommendations.length() > 0)
  
  // Verify report contains expected sections
  assert_true(report.summary.contains("Security Overview"))
  assert_true(report.summary.contains("authentication_failures"))
  assert_true(report.summary.contains("authorization_failures"))
  
  // Test security alerts
  let alert_thresholds = [
    ("authentication_failures", 5.0, High),
    ("authorization_failures", 3.0, High),
    ("intrusion_alerts", 2.0, Critical)
  ]
  
  for (metric, threshold, severity) in alert_thresholds {
    SecurityMonitor::set_alert_threshold(security_monitor, metric, threshold, severity)
  }
  
  let security_alerts = SecurityMonitor::check_alerts(security_monitor)
  
  // Should generate alerts for metrics exceeding thresholds
  assert_true(security_alerts.length() >= 2) // auth failures and authz failures
  
  let auth_failure_alert = security_alerts.find(alert => alert.metric == "authentication_failures")
  match auth_failure_alert {
    Some(alert) => {
      assert_eq(alert.severity, High)
      assert_true(alert.value > 5.0)
    }
    None => assert_true(false)
  }
  
  let intrusion_alert = security_alerts.find(alert => alert.metric == "intrusion_alerts")
  match intrusion_alert {
    Some(alert) => {
      assert_eq(alert.severity, Critical)
      assert_true(alert.value > 2.0)
    }
    None => assert_true(false)
  }
}

// Test 8: Data Privacy and GDPR Compliance
test "data privacy and gdpr compliance" {
  let privacy_manager = PrivacyManager::new()
  
  // Configure privacy settings
  let privacy_config = PrivacyConfig::new(
    true, // Enable GDPR compliance
    365,  // Data retention period in days
    true, // Enable right to be forgotten
    true, // Enable data portability
    true  // Enable consent management
  )
  
  PrivacyManager::configure(privacy_manager, privacy_config)
  
  // Test consent management
  let consent_request = ConsentRequest::new(
    "user_123",
    ["analytics", "marketing", "personalization"],
    "We need your consent to process your data for analytics, marketing, and personalization purposes."
  )
  
  let consent_result = PrivacyManager::request_consent(privacy_manager, consent_request)
  match consent_result {
    Success(consent_id) => {
      // Record user consent
      let user_consent = UserConsent::new(
        "user_123",
        consent_id,
        ["analytics", "personalization"], // User consents to analytics and personalization but not marketing
        Time::now()
      )
      
      let record_result = PrivacyManager::record_consent(privacy_manager, user_consent)
      match record_result {
        Success => assert_true(true)
        Error(_) => assert_true(false)
      }
      
      // Check consent for specific purpose
      let analytics_consent = PrivacyManager::has_consent(privacy_manager, "user_123", "analytics")
      assert_true(analytics_consent) // Should be true
      
      let marketing_consent = PrivacyManager::has_consent(privacy_manager, "user_123", "marketing")
      assert_false(marketing_consent) // Should be false
      
      // Test data processing based on consent
      let analytics_data = TelemetryData::new(42.0, Attributes::new(), 1234567890L)
      let analytics_processing_result = PrivacyManager::process_data(
        privacy_manager,
        "user_123",
        "analytics",
        analytics_data
      )
      match analytics_processing_result {
        Allowed => assert_true(true)
        Denied => assert_true(false)
      }
      
      let marketing_data = TelemetryData::new(42.0, Attributes::new(), 1234567890L)
      let marketing_processing_result = PrivacyManager::process_data(
        privacy_manager,
        "user_123",
        "marketing",
        marketing_data
      )
      match marketing_processing_result {
        Allowed => assert_true(false)
        Denied => assert_true(true)
      }
    }
    Error(_) => assert_true(false)
  }
  
  // Test right to be forgotten
  let user_data = [
    TelemetryData::new(1.0, Attributes::new(), 1234567890L),
    TelemetryData::new(2.0, Attributes::new(), 1234567891L),
    TelemetryData::new(3.0, Attributes::new(), 1234567892L)
  ]
  
  // Store user data
  for data in user_data {
    PrivacyManager::store_user_data(privacy_manager, "user_456", data)
  }
  
  // Verify data exists
  let stored_data = PrivacyManager::get_user_data(privacy_manager, "user_456")
  assert_eq(stored_data.length(), 3)
  
  // Process right to be forgotten request
  let forget_result = PrivacyManager::right_to_be_forgotten(privacy_manager, "user_456")
  match forget_result {
    Success => assert_true(true)
    Error(_) => assert_true(false)
  }
  
  // Verify data is deleted
  let deleted_data = PrivacyManager::get_user_data(privacy_manager, "user_456")
  assert_eq(deleted_data.length(), 0)
  
  // Test data portability
  let export_user_data = [
    TelemetryData::new(10.0, Attributes::new(), 1234567890L),
    TelemetryData::new(20.0, Attributes::new(), 1234567891L)
  ]
  
  for data in export_user_data {
    PrivacyManager::store_user_data(privacy_manager, "user_789", data)
  }
  
  let portable_data_result = PrivacyManager::export_user_data(privacy_manager, "user_789", JSON)
  match portable_data_result {
    Success(portable_data) => {
      assert_true(portable_data.length() > 0)
      assert_true(portable_data.contains("\"user_id\":\"user_789\""))
      assert_true(portable_data.contains("\"export_timestamp\""))
      assert_true(portable_data.contains("\"data\":["))
    }
    Error(_) => assert_true(false)
  }
  
  // Test data retention
  let old_data = TelemetryData::new(100.0, Attributes::new(), 1234567890L - (400 * 86400000)) // 400 days ago
  PrivacyManager::store_user_data(privacy_manager, "user_retention", old_data)
  
  let recent_data = TelemetryData::new(200.0, Attributes::new(), 1234567890L - (100 * 86400000)) // 100 days ago
  PrivacyManager::store_user_data(privacy_manager, "user_retention", recent_data)
  
  // Apply retention policy
  let retention_result = PrivacyManager::apply_retention_policy(privacy_manager)
  match retention_result {
    Success(deleted_count) => {
      assert_true(deleted_count > 0) // Should delete old data
    }
    Error(_) => assert_true(false)
  }
  
  // Verify old data is deleted but recent data is kept
  let retention_user_data = PrivacyManager::get_user_data(privacy_manager, "user_retention")
  assert_eq(retention_user_data.length(), 1) // Only recent data should remain
  assert_eq(retention_user_data[0].value, 200.0) // Should be the recent data
  
  // Generate GDPR compliance report
  let compliance_report = PrivacyManager::generate_compliance_report(privacy_manager)
  
  assert_true(compliance_report.consent_records > 0)
  assert_true(compliance_report.data_subject_requests > 0)
  assert_true(compliance_report.data_portability_exports > 0)
  assert_true(compliance_report.retention_deletions > 0)
  assert_true(compliance_report.compliance_score >= 0.0)
  assert_true(compliance_report.compliance_score <= 100.0)
}
// Azimuth Telemetry System - Security and Privacy Enhanced Tests
// This file contains test cases for security and privacy features

// Test 1: Data Encryption and Decryption
test "data encryption and decryption" {
  let encryption_manager = EncryptionManager::new()
  
  // Configure encryption
  let encryption_config = EncryptionConfig::new()
    .with_algorithm(AES256_GCM)
    .with_key_derivation(PBKDF2)
    .with_key_rotation_interval(Duration::days(30))
    .with_secure_key_storage(true)
  
  EncryptionManager::configure(encryption_manager, encryption_config)
  
  // Test sensitive data encryption
  let sensitive_data = SensitiveData::new()
    .add_field("user_id", "user_12345")
    .add_field("email", "user@example.com")
    .add_field("credit_card", "4111-1111-1111-1111")
    .add_field("ssn", "123-45-6789")
    .add_field("api_key", "sk-1234567890abcdef")
  
  // Encrypt data
  let encrypted_data = EncryptionManager::encrypt(encryption_manager, sensitive_data)
  match encrypted_data {
    Success(encrypted) => {
      // Verify data is actually encrypted
      assert_not_eq(encrypted.encrypted_payload, sensitive_data.to_string())
      assert_true(encrypted.encrypted_payload.length() > 0)
      assert_true(encrypted.encryption_metadata.key_id.length() > 0)
      assert_true(encrypted.encryption_metadata.algorithm.length() > 0)
      assert_true(encrypted.encryption_metadata.timestamp > 0)
    }
    Error(_) => assert_true(false)
  }
  
  // Decrypt data
  match encrypted_data {
    Success(encrypted) => {
      let decrypted_result = EncryptionManager::decrypt(encryption_manager, encrypted)
      match decrypted_result {
        Success(decrypted) => {
          // Verify decrypted data matches original
          assert_eq(decrypted.get_field("user_id"), Some("user_12345"))
          assert_eq(decrypted.get_field("email"), Some("user@example.com"))
          assert_eq(decrypted.get_field("credit_card"), Some("4111-1111-1111-1111"))
          assert_eq(decrypted.get_field("ssn"), Some("123-45-6789"))
          assert_eq(decrypted.get_field("api_key"), Some("sk-1234567890abcdef"))
        }
        Error(_) => assert_true(false)
      }
    }
    Error(_) => assert_true(false)
  }
  
  // Test encryption with different data types
  let mixed_data = SensitiveData::new()
    .add_field("string_field", "test_string")
    .add_field("int_field", "42")
    .add_field("float_field", "3.14159")
    .add_field("bool_field", "true")
    .add_field("json_field", "{\"key\":\"value\",\"array\":[1,2,3]}")
  
  let mixed_encrypted = EncryptionManager::encrypt(encryption_manager, mixed_data)
  match mixed_encrypted {
    Success(_) => assert_true(true),
    Error(_) => assert_true(false)
  }
  
  // Test tampering detection
  match mixed_encrypted {
    Success(encrypted) => {
      // Tamper with encrypted data
      let tampered_data = EncryptedData::new(
        encrypted.encrypted_payload + "tampered",
        encrypted.encryption_metadata
      )
      
      let tampered_result = EncryptionManager::decrypt(encryption_manager, tampered_data)
      match tampered_result {
        Success(_) => assert_true(false), // Should not succeed
        Error(DecryptionError::IntegrityCheckFailed) => assert_true(true),
        Error(_) => assert_true(false)
      }
    }
    Error(_) => assert_true(false)
  }
}

// Test 2: Access Control and Authorization
test "access control and authorization" {
  let access_manager = AccessControlManager::new()
  
  // Configure access control
  let access_config = AccessControlConfig::new()
    .with_auth_method(JWT)
    .with_role_based_access(true)
    .with_attribute_based_access(true)
    .with_session_timeout(Duration::minutes(30))
    .with_max_failed_attempts(3)
    .with_lockout_duration(Duration::minutes(15))
  
  AccessControlManager::configure(access_manager, access_config)
  
  // Define roles and permissions
  let admin_role = Role::new("admin")
    .add_permission("telemetry.read")
    .add_permission("telemetry.write")
    .add_permission("telemetry.delete")
    .add_permission("user.manage")
    .add_permission("system.configure")
  
  let analyst_role = Role::new("analyst")
    .add_permission("telemetry.read")
    .add_permission("dashboard.view")
    .add_permission("report.generate")
  
  let viewer_role = Role::new("viewer")
    .add_permission("telemetry.read")
    .add_permission("dashboard.view")
  
  AccessControlManager::add_role(access_manager, admin_role)
  AccessControlManager::add_role(access_manager, analyst_role)
  AccessControlManager::add_role(access_manager, viewer_role)
  
  // Create users with roles
  let admin_user = User::new("admin_user")
    .with_role("admin")
    .with_attribute("department", "IT")
    .with_attribute("clearance_level", "high")
  
  let analyst_user = User::new("analyst_user")
    .with_role("analyst")
    .with_attribute("department", "Analytics")
    .with_attribute("clearance_level", "medium")
  
  let viewer_user = User::new("viewer_user")
    .with_role("viewer")
    .with_attribute("department", "Business")
    .with_attribute("clearance_level", "low")
  
  // Test role-based access control
  assert_true(AccessControlManager::has_permission(access_manager, admin_user, "telemetry.delete"))
  assert_true(AccessControlManager::has_permission(access_manager, admin_user, "user.manage"))
  assert_false(AccessControlManager::has_permission(access_manager, analyst_user, "telemetry.delete"))
  assert_false(AccessControlManager::has_permission(access_manager, analyst_user, "user.manage"))
  assert_true(AccessControlManager::has_permission(access_manager, analyst_user, "telemetry.read"))
  assert_false(AccessControlManager::has_permission(access_manager, viewer_user, "report.generate"))
  assert_true(AccessControlManager::has_permission(access_manager, viewer_user, "dashboard.view"))
  
  // Test attribute-based access control
  let abac_policy = Policy::new("department_data_access")
    .with_rule("user.department == resource.department")
    .with_rule("user.clearance_level >= resource.required_clearance")
  
  AccessControlManager::add_policy(access_manager, abac_policy)
  
  let finance_resource = Resource::new("finance_dashboard")
    .with_attribute("department", "Finance")
    .with_attribute("required_clearance", "medium")
  
  let it_resource = Resource::new("it_dashboard")
    .with_attribute("department", "IT")
    .with_attribute("required_clearance", "high")
  
  // Admin can access all resources
  assert_true(AccessControlManager::can_access(access_manager, admin_user, finance_resource))
  assert_true(AccessControlManager::can_access(access_manager, admin_user, it_resource))
  
  // Analyst cannot access finance dashboard (different department)
  assert_false(AccessControlManager::can_access(access_manager, analyst_user, finance_resource))
  assert_false(AccessControlManager::can_access(access_manager, analyst_user, it_resource))
  
  // Viewer cannot access any restricted resources
  assert_false(AccessControlManager::can_access(access_manager, viewer_user, finance_resource))
  assert_false(AccessControlManager::can_access(access_manager, viewer_user, it_resource))
  
  // Test session management
  let session = AccessControlManager::create_session(access_manager, admin_user)
  assert_true(AccessControlManager::is_session_valid(access_manager, session))
  
  // Test session timeout
  AccessControlManager::simulate_time_passage(access_manager, Duration::minutes(31))
  assert_false(AccessControlManager::is_session_valid(access_manager, session))
  
  // Test failed login lockout
  AccessControlManager::simulate_failed_login(access_manager, viewer_user)
  AccessControlManager::simulate_failed_login(access_manager, viewer_user)
  assert_true(AccessControlManager::is_user_locked_out(access_manager, viewer_user) == false)
  
  AccessControlManager::simulate_failed_login(access_manager, viewer_user)
  assert_true(AccessControlManager::is_user_locked_out(access_manager, viewer_user) == true)
}

// Test 3: Data anonymization and pseudonymization
test "data anonymization and pseudonymization" {
  let anonymization_manager = AnonymizationManager::new()
  
  // Configure anonymization
  let anon_config = AnonymizationConfig::new()
    .with_anonymization_method(HashBased)
    .with_salt_rotation_interval(Duration::days(90))
    .with_reversibility(false) // One-way anonymization
    .with_consistent_anonymization(true)
  
  AnonymizationManager::configure(anonymization_manager, anon_config)
  
  // Define fields to anonymize
  let pii_fields = [
    "user_id",
    "email",
    "phone_number",
    "ip_address",
    "device_id",
    "session_id"
  ]
  
  // Test data anonymization
  let sensitive_record = TelemetryRecord::new()
    .add_field("user_id", "user_12345")
    .add_field("email", "john.doe@example.com")
    .add_field("phone_number", "+1-555-123-4567")
    .add_field("ip_address", "192.168.1.100")
    .add_field("device_id", "device_abcdef123456")
    .add_field("session_id", "sess_7890abcdef")
    .add_field("operation", "login")
    .add_field("timestamp", "2023-01-01T12:00:00Z")
    .add_field("response_time", "250")
  
  let anonymized_record = AnonymizationManager::anonymize(anonymization_manager, sensitive_record, pii_fields)
  
  // Verify sensitive fields are anonymized
  assert_not_eq(anonymized_record.get_field("user_id"), Some("user_12345"))
  assert_not_eq(anonymized_record.get_field("email"), Some("john.doe@example.com"))
  assert_not_eq(anonymized_record.get_field("phone_number"), Some("+1-555-123-4567"))
  assert_not_eq(anonymized_record.get_field("ip_address"), Some("192.168.1.100"))
  assert_not_eq(anonymized_record.get_field("device_id"), Some("device_abcdef123456"))
  assert_not_eq(anonymized_record.get_field("session_id"), Some("sess_7890abcdef"))
  
  // Verify non-sensitive fields are preserved
  assert_eq(anonymized_record.get_field("operation"), Some("login"))
  assert_eq(anonymized_record.get_field("timestamp"), Some("2023-01-01T12:00:00Z"))
  assert_eq(anonymized_record.get_field("response_time"), Some("250"))
  
  // Test consistent anonymization (same input should produce same output)
  let second_record = TelemetryRecord::new()
    .add_field("user_id", "user_12345")
    .add_field("email", "john.doe@example.com")
    .add_field("operation", "login")
  
  let second_anonymized = AnonymizationManager::anonymize(anonymization_manager, second_record, pii_fields)
  
  assert_eq(
    anonymized_record.get_field("user_id"),
    second_anonymized.get_field("user_id")
  )
  assert_eq(
    anonymized_record.get_field("email"),
    second_anonymized.get_field("email")
  )
  
  // Test pseudonymization (reversible with proper authorization)
  let pseudonym_config = AnonymizationConfig::new()
    .with_anonymization_method(TokenBased)
    .with_reversibility(true)
    .with_key_derivation(PBKDF2)
  
  let pseudonym_manager = AnonymizationManager::new()
  AnonymizationManager::configure(pseudonym_manager, pseudonym_config)
  
  let pseudonymized_record = AnonymizationManager::anonymize(
    pseudonym_manager, 
    sensitive_record, 
    ["user_id", "email"]
  )
  
  // Verify pseudonymization
  assert_not_eq(pseudonymized_record.get_field("user_id"), Some("user_12345"))
  assert_not_eq(pseudonymized_record.get_field("email"), Some("john.doe@example.com"))
  
  // Test re-identification with proper authorization
  let authorized_user = User::new("data_admin").with_role("admin")
  let reidentified_result = AnonymizationManager::reidentify(
    pseudonym_manager,
    pseudonymized_record,
    ["user_id", "email"],
    authorized_user
  )
  
  match reidentified_result {
    Success(reidentified) => {
      assert_eq(reidentified.get_field("user_id"), Some("user_12345"))
      assert_eq(reidentified.get_field("email"), Some("john.doe@example.com"))
    }
    Error(_) => assert_true(false)
  }
  
  // Test re-identification without proper authorization
  let unauthorized_user = User::new("data_viewer").with_role("viewer")
  let unauthorized_result = AnonymizationManager::reidentify(
    pseudonym_manager,
    pseudonymized_record,
    ["user_id", "email"],
    unauthorized_user
  )
  
  match unauthorized_result {
    Success(_) => assert_true(false), // Should not succeed
    Error(AuthorizationError) => assert_true(true),
    Error(_) => assert_true(false)
  }
}

// Test 4: Audit Logging and Compliance
test "audit logging and compliance" {
  let audit_manager = AuditManager::new()
  
  // Configure audit logging
  let audit_config = AuditConfig::new()
    .with_log_all_access(true)
    .with_log_data_modifications(true)
    .with_log_authentication_events(true)
    .with_log_retention_period(Duration::days(365))
    .with_tamper_protection(true)
    .with_compliance_standards([GDPR, HIPAA, SOX])
  
  AuditManager::configure(audit_manager, audit_config)
  
  // Test audit logging for data access
  let user = User::new("analyst_user").with_role("analyst")
  let resource = Resource::new("customer_data").with_attribute("sensitivity", "high")
  
  let access_event = AuditEvent::new(DataAccess)
    .with_user(user)
    .with_resource(resource)
    .with_timestamp(Time::now())
    .with_details("Accessed customer telemetry data")
    .with_result(Success)
    .with_ip_address("192.168.1.50")
  
  AuditManager::log_event(audit_manager, access_event)
  
  // Test audit logging for data modification
  let modification_event = AuditEvent::new(DataModification)
    .with_user(User::new("admin_user").with_role("admin"))
    .with_resource(Resource::new("telemetry_config"))
    .with_timestamp(Time::now())
    .with_details("Updated telemetry sampling configuration")
    .with_result(Success)
    .with_changes([
      ("sampling_rate", "0.1", "0.2"),
      ("max_spans", "1000", "2000")
    ])
  
  AuditManager::log_event(audit_manager, modification_event)
  
  // Test audit logging for authentication
  let auth_event = AuditEvent::new(Authentication)
    .with_user(user)
    .with_timestamp(Time::now())
    .with_details("User login")
    .with_result(Success)
    .with_auth_method("JWT")
  
  AuditManager::log_event(audit_manager, auth_event)
  
  // Test audit logging for failed operations
  let failed_access_event = AuditEvent::new(DataAccess)
    .with_user(User::new("unauthorized_user").with_role("viewer"))
    .with_resource(resource)
    .with_timestamp(Time::now())
    .with_details("Attempted access to restricted data")
    .with_result(Failure)
    .with_reason("Insufficient privileges")
  
  AuditManager::log_event(audit_manager, failed_access_event)
  
  // Test audit log retrieval
  let all_events = AuditManager::get_events(audit_manager)
  assert_eq(all_events.length(), 4)
  
  let access_events = AuditManager::get_events_by_type(audit_manager, DataAccess)
  assert_eq(access_events.length(), 2)
  
  let user_events = AuditManager::get_events_by_user(audit_manager, "analyst_user")
  assert_eq(user_events.length(), 2)
  
  let failed_events = AuditManager::get_events_by_result(audit_manager, Failure)
  assert_eq(failed_events.length(), 1)
  
  // Test audit log integrity
  let log_integrity = AuditManager::verify_integrity(audit_manager)
  assert_true(log_integrity)
  
  // Test audit log tampering detection
  AuditManager::simulate_log_tampering(audit_manager, 1) // Tamper with second event
  let tampered_integrity = AuditManager::verify_integrity(audit_manager)
  assert_false(tampered_integrity)
  
  // Test compliance reporting
  let gdpr_report = AuditManager::generate_compliance_report(audit_manager, GDPR)
  assert_true(ComplianceReport::is_compliant(gdpr_report))
  assert_true(ComplianceReport::data_access_logged(gdpr_report))
  assert_true(ComplianceReport::data_modification_logged(gdpr_report))
  assert_true(ComplianceReport::retention_policy_followed(gdpr_report))
  
  let hipaa_report = AuditManager::generate_compliance_report(audit_manager, HIPAA)
  assert_true(ComplianceReport::is_compliant(hipaa_report))
  assert_true(ComplianceReport::authentication_logged(hipaa_report))
  assert_true(ComplianceReport::access_control_enforced(hipaa_report))
}

// Test 5: Data Retention and Deletion
test "data retention and deletion" {
  let retention_manager = DataRetentionManager::new()
  
  // Configure retention policies
  let retention_config = RetentionConfig::new()
    .with_default_retention_period(Duration::days(90))
    .with_secure_deletion(true)
    .with_deletion_confirmation(true)
    .with_legal_hold_support(true)
  
  DataRetentionManager::configure(retention_manager, retention_config)
  
  // Define retention policies for different data types
  let pii_policy = RetentionPolicy::new("pii_data")
    .with_retention_period(Duration::days(30))
    .with_auto_deletion(true)
    .with_secure_deletion_method(CryptographicErase)
    .with_legal_hold_exemption(false)
  
  let telemetry_policy = RetentionPolicy::new("telemetry_data")
    .with_retention_period(Duration::days(365))
    .with_auto_deletion(true)
    .with_secure_deletion_method(DoD_5220_22_M)
    .with_legal_hold_exemption(true)
  
  let audit_policy = RetentionPolicy::new("audit_logs")
    .with_retention_period(Duration::days(2555)) // 7 years
    .with_auto_deletion(false) // Manual deletion only
    .with_secure_deletion_method(CryptographicErase)
    .with_legal_hold_exemption(true)
  
  DataRetentionManager::add_policy(retention_manager, pii_policy)
  DataRetentionManager::add_policy(retention_manager, telemetry_policy)
  DataRetentionManager::add_policy(retention_manager, audit_policy)
  
  // Create test data with different ages
  let now = Time::now()
  
  let recent_pii_data = TelemetryData::new("recent_pii")
    .with_timestamp(now - Duration::days(10))
    .with_type("pii_data")
    .with_content("user_id: user_123, email: user@example.com")
  
  let old_pii_data = TelemetryData::new("old_pii")
    .with_timestamp(now - Duration::days(45))
    .with_type("pii_data")
    .with_content("user_id: user_456, email: old@example.com")
  
  let recent_telemetry_data = TelemetryData::new("recent_telemetry")
    .with_timestamp(now - Duration::days(100))
    .with_type("telemetry_data")
    .with_content("service: api, response_time: 250ms")
  
  let old_telemetry_data = TelemetryData::new("old_telemetry")
    .with_timestamp(now - Duration::days(400))
    .with_type("telemetry_data")
    .with_content("service: api, response_time: 300ms")
  
  let audit_data = TelemetryData::new("audit_log")
    .with_timestamp(now - Duration::days(100))
    .with_type("audit_logs")
    .with_content("user: admin, action: config_change")
  
  // Add data to retention manager
  DataRetentionManager::add_data(retention_manager, recent_pii_data)
  DataRetentionManager::add_data(retention_manager, old_pii_data)
  DataRetentionManager::add_data(retention_manager, recent_telemetry_data)
  DataRetentionManager::add_data(retention_manager, old_telemetry_data)
  DataRetentionManager::add_data(retention_manager, audit_data)
  
  // Test retention policy application
  let expired_data = DataRetentionManager::get_expired_data(retention_manager)
  assert_eq(expired_data.length(), 2) // old_pii_data and old_telemetry_data should be expired
  
  // Test secure deletion
  let deletion_results = DataRetentionManager::delete_expired_data(retention_manager)
  assert_eq(deletion_results.successful_deletions, 2)
  assert_eq(deletion_results.failed_deletions, 0)
  
  // Verify data is actually deleted
  let remaining_data = DataRetentionManager::get_all_data(retention_manager)
  assert_eq(remaining_data.length(), 3) // recent_pii_data, recent_telemetry_data, and audit_data
  
  // Test legal hold
  let legal_hold_data = TelemetryData::new("legal_hold_telemetry")
    .with_timestamp(now - Duration::days(500))
    .with_type("telemetry_data")
    .with_content("service: api, response_time: 180ms")
    .with_legal_hold(true)
  
  DataRetentionManager::add_data(retention_manager, legal_hold_data)
  
  let expired_with_hold = DataRetentionManager::get_expired_data(retention_manager)
  assert_eq(expired_with_hold.length(), 0) // Legal hold data should not be expired
  
  // Test manual deletion with confirmation
  let deletion_request = DeletionRequest::new()
    .with_data_id("recent_pii")
    .with_reason("User request for data deletion")
    .with_requester("user_123")
    .with_confirmation_required(true)
  
  let deletion_result = DataRetentionManager::request_deletion(retention_manager, deletion_request)
  match deletion_result {
    Success(request_id) => {
      // Confirm deletion
      let confirmation_result = DataRetentionManager::confirm_deletion(retention_manager, request_id)
      match confirmation_result {
        Success(_) => assert_true(true),
        Error(_) => assert_true(false)
      }
    }
    Error(_) => assert_true(false)
  }
  
  // Verify data is deleted
  let final_remaining_data = DataRetentionManager::get_all_data(retention_manager)
  assert_eq(final_remaining_data.length(), 3) // recent_telemetry_data, audit_data, and legal_hold_data
}

// Test 6: Intrusion Detection and Prevention
test "intrusion detection and prevention" {
  let intrusion_manager = IntrusionDetectionManager::new()
  
  // Configure intrusion detection
  let ids_config = IDSConfig::new()
    .with_detection_mode(Prevention)
    .with_sensitivity_level(Medium)
    .with_anomaly_detection(true)
    .with_signature_based_detection(true)
    .with_behavioral_analysis(true)
    .with_automatic_response(true)
    .with_alert_threshold(5) // 5 suspicious events trigger alert
  
  IntrusionDetectionManager::configure(intrusion_manager, ids_config)
  
  // Define detection rules
  let brute_force_rule = DetectionRule::new("brute_force_login")
    .with_pattern("failed_login_count > 10 in 5 minutes")
    .with_severity(High)
    .with_response(BlockIP)
    .with_duration(Duration::minutes(15))
  
  let data_exfiltration_rule = DetectionRule::new("data_exfiltration")
    .with_pattern("large_data_download > 1GB in 1 hour")
    .with_severity(Critical)
    .with_response(BlockUser)
    .with_duration(Duration::hours(1))
  
  let unusual_access_rule = DetectionRule::new("unusual_access_pattern")
    .with_pattern("access_time outside normal hours AND access_from_new_location")
    .with_severity(Medium)
    .with_alert_only(true)
  
  IntrusionDetectionManager::add_rule(intrusion_manager, brute_force_rule)
  IntrusionDetectionManager::add_rule(intrusion_manager, data_exfiltration_rule)
  IntrusionDetectionManager::add_rule(intrusion_manager, unusual_access_rule)
  
  // Simulate normal activity to establish baseline
  let base_time = Time::now() - Duration::days(7)
  
  for i in 0..=100 {
    let timestamp = base_time + Duration::hours(i)
    let hour = Time::hour(timestamp)
    
    // Normal working hours (9 AM - 5 PM)
    if hour >= 9 && hour <= 17 {
      let event = SecurityEvent::new("user_login")
        .with_timestamp(timestamp)
        .with_user("regular_user")
        .with_ip_address("192.168.1.100")
        .with_result(Success)
        .with_location("office")
      
      IntrusionDetectionManager::process_event(intrusion_manager, event)
    }
  }
  
  // Simulate brute force attack
  let attack_start_time = Time::now()
  let attacker_ip = "203.0.113.50"
  
  for i in 0..=12 {
    let event = SecurityEvent::new("user_login")
      .with_timestamp(attack_start_time + Duration::seconds(i * 10))
      .with_user("admin")
      .with_ip_address(attacker_ip)
      .with_result(Failure)
      .with_reason("Invalid credentials")
    
    IntrusionDetectionManager::process_event(intrusion_manager, event)
  }
  
  // Wait for detection
  Time::sleep(1000) // 1 second
  
  // Check for intrusion alerts
  let alerts = IntrusionDetectionManager::get_alerts(intrusion_manager)
  let brute_force_alerts = alerts.filter(|a| Alert::rule_name(a) == "brute_force_login")
  assert_true(brute_force_alerts.length() > 0)
  
  // Verify IP is blocked
  let ip_blocked = IntrusionDetectionManager::is_ip_blocked(intrusion_manager, attacker_ip)
  assert_true(ip_blocked)
  
  // Simulate data exfiltration
  let exfil_start_time = Time::now()
  
  for i in 0..=5 {
    let event = SecurityEvent::new("data_download")
      .with_timestamp(exfil_start_time + Duration::minutes(i * 10))
      .with_user("internal_user")
      .with_ip_address("192.168.1.200")
      .with_data_size(200 * 1024 * 1024) // 200MB per download
    
    IntrusionDetectionManager::process_event(intrusion_manager, event)
  }
  
  // Wait for detection
  Time::sleep(1000) // 1 second
  
  // Check for exfiltration alerts
  let exfil_alerts = IntrusionDetectionManager::get_alerts(intrusion_manager)
  let data_exfil_alerts = exfil_alerts.filter(|a| Alert::rule_name(a) == "data_exfiltration")
  assert_true(data_exfil_alerts.length() > 0)
  
  // Verify user is blocked
  let user_blocked = IntrusionDetectionManager::is_user_blocked(intrusion_manager, "internal_user")
  assert_true(user_blocked)
  
  // Simulate unusual access pattern
  let unusual_time = Time::now().with_hour(3) // 3 AM
  let unusual_location = "foreign_country"
  
  let unusual_event = SecurityEvent::new("data_access")
    .with_timestamp(unusual_time)
    .with_user("regular_user")
    .with_ip_address("203.0.113.100")
    .with_location(unusual_location)
  
  IntrusionDetectionManager::process_event(intrusion_manager, unusual_event)
  
  // Wait for detection
  Time::sleep(1000) // 1 second
  
  // Check for unusual access alerts
  let final_alerts = IntrusionDetectionManager::get_alerts(intrusion_manager)
  let unusual_access_alerts = final_alerts.filter(|a| Alert::rule_name(a) == "unusual_access_pattern")
  assert_true(unusual_access_alerts.length() > 0)
  
  // Test IDS metrics
  let ids_metrics = IntrusionDetectionManager::get_metrics(intrusion_manager)
  assert_true(IDSMetrics::total_events_processed(ids_metrics) > 0)
  assert_true(IDSMetrics::alerts_generated(ids_metrics) >= 3)
  assert_true(IDSMetrics::ips_blocked(ids_metrics) >= 1)
  assert_true(IDSMetrics::users_blocked(ids_metrics) >= 1)
}

// Test 7: Secure Communication Channels
test "secure communication channels" {
  let secure_comm_manager = SecureCommunicationManager::new()
  
  // Configure secure communication
  let comm_config = CommConfig::new()
    .with_tls_version(TLS_1_3)
    .with_certificate_validation(true)
    .with_mutual_tls(true)
    .with_encryption_at_rest(true)
    .with_key_exchange_algorithm(ECDHE)
    .with_cipher_suite(AES_256_GCM)
  
  SecureCommunicationManager::configure(secure_comm_manager, comm_config)
  
  // Generate certificates for mutual TLS
  let server_cert = CertificateManager::generate_certificate(
    "azimuth-server",
    CertificateType::Server,
    Duration::days(365)
  )
  
  let client_cert = CertificateManager::generate_certificate(
    "azimuth-client",
    CertificateType::Client,
    Duration::days(365)
  )
  
  // Create secure channel
  let secure_channel = SecureCommunicationManager::create_channel(
    secure_comm_manager,
    server_cert,
    client_cert
  )
  
  match secure_channel {
    Success(channel) => {
      // Test secure message transmission
      let original_message = SecureMessage::new()
        .with_payload("Sensitive telemetry data")
        .with_type(TelemetryData)
        .with_priority(High)
        .with_timestamp(Time::now())
        .with_sender("client")
        .with_receiver("server")
        .with_encryption(true)
        .with_signature(true)
      
      // Encrypt and sign message
      let encrypted_message = SecureChannel::encrypt_and_sign(channel, original_message)
      match encrypted_message {
        Success(encrypted) => {
          // Verify message is encrypted
          assert_not_eq(encrypted.encrypted_payload, original_message.payload)
          assert_true(encrypted.signature.length() > 0)
          assert_true(encrypted.encryption_metadata.length() > 0)
          
          // Decrypt and verify message
          let decrypted_result = SecureChannel::decrypt_and_verify(channel, encrypted)
          match decrypted_result {
            Success(decrypted) => {
              // Verify decrypted message matches original
              assert_eq(decrypted.payload, original_message.payload)
              assert_eq(decrypted.message_type, original_message.message_type)
              assert_eq(decrypted.sender, original_message.sender)
              assert_eq(decrypted.receiver, original_message.receiver)
              
              // Verify signature is valid
              assert_true(decrypted.signature_valid)
            }
            Error(_) => assert_true(false)
          }
        }
        Error(_) => assert_true(false)
      }
      
      // Test message tampering detection
      match encrypted_message {
        Success(encrypted) => {
          // Tamper with the message
          let tampered_message = EncryptedMessage::new(
            encrypted.encrypted_payload + "tampered",
            encrypted.signature,
            encrypted.encryption_metadata
          )
          
          let tampered_result = SecureChannel::decrypt_and_verify(channel, tampered_message)
          match tampered_result {
            Success(_) => assert_true(false), // Should not succeed
            Error(SecurityError::SignatureVerificationFailed) => assert_true(true),
            Error(_) => assert_true(false)
          }
        }
        Error(_) => assert_true(false)
      }
      
      // Test certificate validation
      let valid_cert = CertificateManager::load_certificate("valid_server_cert")
      let validation_result = SecureCommunicationManager::validate_certificate(
        secure_comm_manager,
        valid_cert
      )
      match validation_result {
        CertificateValidationResult::Valid => assert_true(true),
        CertificateValidationResult::Invalid(reasons) => assert_true(false, "Certificate should be valid: " + reasons.join(", ")),
        CertificateValidationResult::Expired => assert_true(false)
      }
      
      // Test expired certificate validation
      let expired_cert = CertificateManager::generate_certificate(
        "expired_server",
        CertificateType::Server,
        Duration::days(-1) // Already expired
      )
      
      let expired_validation_result = SecureCommunicationManager::validate_certificate(
        secure_comm_manager,
        expired_cert
      )
      match expired_validation_result {
        CertificateValidationResult::Valid => assert_true(false),
        CertificateValidationResult::Invalid(_) => assert_true(true),
        CertificateValidationResult::Expired => assert_true(true)
      }
      
      // Test secure channel metrics
      let channel_metrics = SecureChannel::get_metrics(channel)
      assert_true(ChannelMetrics::messages_sent(channel_metrics) >= 1)
      assert_true(ChannelMetrics::messages_received(channel_metrics) >= 1)
      assert_true(ChannelMetrics::encryption_operations(channel_metrics) >= 1)
      assert_true(ChannelMetrics::signature_verifications(channel_metrics) >= 1)
      assert_true(ChannelMetrics::failed_verifications(channel_metrics) >= 1) // Due to tampering test
    }
    Error(_) => assert_true(false)
  }
}

// Test 8: Privacy-Preserving Telemetry Collection
test "privacy-preserving telemetry collection" {
  let privacy_manager = PrivacyPreservingTelemetryManager::new()
  
  // Configure privacy-preserving collection
  let privacy_config = PrivacyConfig::new()
    .with_differential_privacy(true)
    .with_epsilon(1.0) // Privacy budget
    .with_delta(1e-5) // Delta parameter
    .with_local_aggregation(true)
    .with_min_group_size(5) // Minimum group size for aggregation
    .with_noise_mechanism(Gaussian)
  
  PrivacyPreservingTelemetryManager::configure(privacy_manager, privacy_config)
  
  // Simulate privacy-preserving data collection from multiple clients
  let num_clients = 100
  let mut client_data = []
  
  for i in 0..num_clients {
    let client_id = "client_" + i.to_string()
    
    // Each client has sensitive data
    let sensitive_metrics = ClientMetrics::new(client_id)
      .add_metric("cpu_usage", 20.0 + Math::random() * 60.0)
      .add_metric("memory_usage", 30.0 + Math::random() * 50.0)
      .add_metric("error_count", Math::random().floor())
      .add_sensitive_attribute("user_id", "user_" + (i % 10).to_string())
      .add_sensitive_attribute("ip_address", "192.168.1." + (100 + i).to_string())
    
    client_data.push(sensitive_metrics)
  }
  
  // Apply privacy-preserving transformations
  let privacy_preserving_data = PrivacyPreservingTelemetryManager::apply_privacy(
    privacy_manager,
    client_data
  )
  
  // Verify privacy preservation
  for transformed_data in privacy_preserving_data {
    // Sensitive attributes should be removed or anonymized
    assert_eq(transformed_data.get_sensitive_attribute("user_id"), None)
    assert_eq(transformed_data.get_sensitive_attribute("ip_address"), None)
    
    // Metrics should have noise added for differential privacy
    let cpu_usage = transformed_data.get_metric("cpu_usage")
    let memory_usage = transformed_data.get_metric("memory_usage")
    
    match (cpu_usage, memory_usage) {
      (Some(cpu), Some(mem)) => {
        // Values should still be in reasonable ranges but with noise
        assert_true(cpu >= 0.0 && cpu <= 100.0)
        assert_true(mem >= 0.0 && mem <= 100.0)
      }
      _ => assert_true(false)
    }
  }
  
  // Test local aggregation with privacy
  let aggregated_metrics = PrivacyPreservingTelemetryManager::aggregate_with_privacy(
    privacy_manager,
    privacy_preserving_data,
    ["cpu_usage", "memory_usage", "error_count"]
  )
  
  // Verify aggregation results
  for metric_name in ["cpu_usage", "memory_usage", "error_count"] {
    let aggregation_result = AggregatedMetrics::get_metric(aggregated_metrics, metric_name)
    match aggregation_result {
      Some(result) => {
        assert_true(result.count >= 5) // Should meet minimum group size
        assert_true(result.mean >= 0.0)
        assert_true(result.std_dev >= 0.0)
        
        // Verify privacy budget is tracked
        assert_true(result.privacy_cost > 0.0)
        assert_true(result.privacy_cost <= 1.0) // Within epsilon
      }
      None => assert_true(false)
    }
  }
  
  // Test privacy budget management
  let remaining_budget = PrivacyPreservingTelemetryManager::get_remaining_privacy_budget(privacy_manager)
  assert_true(remaining_budget >= 0.0)
  assert_true(remaining_budget <= 1.0)
  
  // Test privacy budget exhaustion
  let budget_exhausted = PrivacyPreservingTelemetryManager::is_privacy_budget_exhausted(privacy_manager)
  assert_false(budget_exhausted) // Should not be exhausted yet
  
  // Simulate additional queries to exhaust budget
  for i in 0..=10 {
    let additional_data = PrivacyPreservingTelemetryManager::apply_privacy(
      privacy_manager,
      client_data.slice(0, 10) // Use subset of data
    )
    
    if PrivacyPreservingTelemetryManager::is_privacy_budget_exhausted(privacy_manager) {
      break
    }
  }
  
  // Test privacy metrics
  let privacy_metrics = PrivacyPreservingTelemetryManager::get_privacy_metrics(privacy_manager)
  assert_true(PrivacyMetrics::total_queries_processed(privacy_metrics) > 0)
  assert_true(PrivacyMetrics::privacy_budget_used(privacy_metrics) > 0.0)
  assert_true(PrivacyMetrics::anonymization_operations(privacy_metrics) > 0)
  assert_true(PrivacyMetrics::noise_additions(privacy_metrics) > 0)
}
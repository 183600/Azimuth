// Premium Security and Privacy Tests for Azimuth
// This file contains comprehensive test cases for security and privacy features in telemetry

// Test 1: Data Encryption and Decryption
test "data encryption and decryption" {
  let encryption_manager = EncryptionManager::new()
  
  // Generate encryption key
  let key = EncryptionManager::generate_key(encryption_manager, KeyAlgorithm::AES256)
  assert_true(key.length() > 0)
  
  // Test symmetric encryption
  let sensitive_data = "user_id=12345&email=user@example.com&ssn=123-45-6789"
  let encrypted_data = EncryptionManager::encrypt_symmetric(encryption_manager, sensitive_data, key)
  
  assert_ne(encrypted_data, sensitive_data)
  assert_true(encrypted_data.length() > 0)
  
  // Test symmetric decryption
  let decrypted_data = EncryptionManager::decrypt_symmetric(encryption_manager, encrypted_data, key)
  assert_eq(decrypted_data, sensitive_data)
  
  // Test asymmetric encryption
  let (public_key, private_key) = EncryptionManager::generate_key_pair(encryption_manager, KeyAlgorithm::RSA2048)
  
  let asymmetric_encrypted = EncryptionManager::encrypt_asymmetric(encryption_manager, sensitive_data, public_key)
  assert_ne(asymmetric_encrypted, sensitive_data)
  
  let asymmetric_decrypted = EncryptionManager::decrypt_asymmetric(encryption_manager, asymmetric_encrypted, private_key)
  assert_eq(asymmetric_decrypted, sensitive_data)
  
  // Test key rotation
  let new_key = EncryptionManager::rotate_key(encryption_manager, key)
  assert_true(new_key.length() > 0)
  assert_ne(new_key, key)
  
  // Verify old key can't decrypt data encrypted with new key
  let new_encrypted = EncryptionManager::encrypt_symmetric(encryption_manager, sensitive_data, new_key)
  let failed_decryption = EncryptionManager::decrypt_symmetric(encryption_manager, new_encrypted, key)
  assert_ne(failed_decryption, sensitive_data)
  
  // Verify new key can decrypt data encrypted with new key
  let successful_decryption = EncryptionManager::decrypt_symmetric(encryption_manager, new_encrypted, new_key)
  assert_eq(successful_decryption, sensitive_data)
}

// Test 2: PII (Personally Identifiable Information) Detection and Redaction
test "pii detection and redaction" {
  let pii_detector = PiiDetector::new()
  let redactor = DataRedactor::new()
  
  // Configure PII detection patterns
  PiiDetector::add_pattern(pii_detector, "email", Regex::new(r"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}"))
  PiiDetector::add_pattern(pii_detector, "ssn", Regex::new(r"\d{3}-\d{2}-\d{4}"))
  PiiDetector::add_pattern(pii_detector, "credit_card", Regex::new(r"\d{4}[-\s]?\d{4}[-\s]?\d{4}[-\s]?\d{4}"))
  PiiDetector::add_pattern(pii_detector, "phone", Regex::new(r"\d{3}-\d{3}-\d{4}"))
  
  // Test data with PII
  let test_data = "User john.doe@example.com with SSN 123-45-6789 called 555-123-4567 from credit card 4532-1234-5678-9012"
  
  // Detect PII
  let pii_instances = PiiDetector::detect(pii_detector, test_data)
  assert_eq(pii_instances.length(), 4) // email, SSN, phone, credit card
  
  // Verify detected PII types
  let mut email_found = false
  let mut ssn_found = false
  let mut phone_found = false
  let mut credit_card_found = false
  
  for instance in pii_instances {
    match PiiInstance::type(instance) {
      "email" => email_found = true
      "ssn" => ssn_found = true
      "phone" => phone_found = true
      "credit_card" => credit_card_found = true
      _ => assert_true(false)
    }
  }
  
  assert_true(email_found)
  assert_true(ssn_found)
  assert_true(phone_found)
  assert_true(credit_card_found)
  
  // Test redaction
  let redaction_strategy = RedactionStrategy::PartialRedaction(3) // Show first 3 chars
  let redacted_data = DataRedactor::redact(redactor, test_data, pii_instances, redaction_strategy)
  
  assert_true(redacted_data.contains("joh...")) // Partially redacted email
  assert_true(redacted_data.contains("123...")) // Partially redacted SSN
  assert_true(redacted_data.contains("555...")) // Partially redacted phone
  assert_true(redacted_data.contains("453...")) // Partially redacted credit card
  
  // Test complete redaction
  let complete_redaction = DataRedactor::redact(redactor, test_data, pii_instances, RedactionStrategy::Complete)
  assert_false(complete_redaction.contains("john.doe@example.com"))
  assert_false(complete_redaction.contains("123-45-6789"))
  assert_false(complete_redaction.contains("555-123-4567"))
  assert_false(complete_redaction.contains("4532-1234-5678-9012"))
  
  // Test hash-based redaction
  let hash_redaction = DataRedactor::redact(redactor, test_data, pii_instances, RedactionStrategy::Hash)
  assert_true(hash_redaction.contains("email:"))
  assert_true(hash_redaction.contains("ssn:"))
  assert_true(hash_redaction.contains("phone:"))
  assert_true(hash_redaction.contains("credit_card:"))
  
  // Verify hash values are consistent
  let hash_redaction2 = DataRedactor::redact(redactor, test_data, pii_instances, RedactionStrategy::Hash)
  assert_eq(hash_redaction, hash_redaction2)
}

// Test 3: Access Control and Authorization
test "access control and authorization" {
  let access_manager = AccessManager::new()
  
  // Define roles and permissions
  AccessManager::define_role(access_manager, "admin", ["read_all", "write_all", "delete_all"])
  AccessManager::define_role(access_manager, "analyst", ["read_telemetry", "write_annotations"])
  AccessManager::define_role(access_manager, "viewer", ["read_telemetry"])
  
  // Create users
  let admin_user = User::new("admin_user", ["admin"])
  let analyst_user = User::new("analyst_user", ["analyst"])
  let viewer_user = User::new("viewer_user", ["viewer"])
  
  // Test admin permissions
  assert_true(AccessManager::check_permission(access_manager, admin_user, "read_all"))
  assert_true(AccessManager::check_permission(access_manager, admin_user, "write_all"))
  assert_true(AccessManager::check_permission(access_manager, admin_user, "delete_all"))
  
  // Test analyst permissions
  assert_false(AccessManager::check_permission(access_manager, analyst_user, "read_all"))
  assert_true(AccessManager::check_permission(access_manager, analyst_user, "read_telemetry"))
  assert_true(AccessManager::check_permission(access_manager, analyst_user, "write_annotations"))
  assert_false(AccessManager::check_permission(access_manager, analyst_user, "delete_all"))
  
  // Test viewer permissions
  assert_false(AccessManager::check_permission(access_manager, viewer_user, "read_all"))
  assert_true(AccessManager::check_permission(access_manager, viewer_user, "read_telemetry"))
  assert_false(AccessManager::check_permission(access_manager, viewer_user, "write_annotations"))
  assert_false(AccessManager::check_permission(access_manager, viewer_user, "delete_all"))
  
  // Test resource-based access control
  let telemetry_resource = Resource::new("telemetry_data", "service123", "production")
  
  // Add resource-specific permissions
  AccessManager::add_resource_permission(access_manager, analyst_user, telemetry_resource, "read")
  
  // Verify resource-based access
  assert_true(AccessManager::check_resource_access(access_manager, admin_user, telemetry_resource, "read"))
  assert_true(AccessManager::check_resource_access(access_manager, admin_user, telemetry_resource, "write"))
  assert_true(AccessManager::check_resource_access(access_manager, analyst_user, telemetry_resource, "read"))
  assert_false(AccessManager::check_resource_access(access_manager, analyst_user, telemetry_resource, "write"))
  assert_false(AccessManager::check_resource_access(access_manager, viewer_user, telemetry_resource, "read"))
  
  // Test time-based access control
  let time_restricted_user = User::new("time_restricted", ["viewer"])
  AccessManager::set_time_restriction(access_manager, time_restricted_user, 
                                      TimeRestriction::BusinessHours(9, 17)) // 9 AM to 5 PM
  
  // Mock current time (in a real implementation, this would use actual time)
  let business_hours_time = 1609459200000L // 2021-01-01 00:00:00 UTC (simplified)
  let after_hours_time = 1609502400000L // 2021-01-01 12:00:00 UTC (simplified)
  
  assert_true(AccessManager::check_timed_access(access_manager, time_restricted_user, "read_telemetry", business_hours_time))
  assert_false(AccessManager::check_timed_access(access_manager, time_restricted_user, "read_telemetry", after_hours_time))
}

// Test 4: Audit Logging and Security Events
test "audit logging and security events" {
  let audit_logger = AuditLogger::new()
  let security_monitor = SecurityMonitor::new()
  
  // Configure audit logging
  AuditLogger::configure(audit_logger, [
    ("log_all_access", true),
    ("log_data_changes", true),
    ("log_permission_denials", true),
    ("log_authentication_events", true)
  ])
  
  // Test authentication event logging
  let auth_event = SecurityEvent::new(
    SecurityEventType::Authentication,
    "user123",
    "login_success",
    "User logged in successfully",
    Attributes::new()
  )
  
  AuditLogger::log_security_event(audit_logger, auth_event)
  SecurityMonitor::process_event(security_monitor, auth_event)
  
  // Test access event logging
  let access_event = SecurityEvent::new(
    SecurityEventType::DataAccess,
    "analyst456",
    "telemetry_access",
    "Accessed telemetry data for service123",
    Attributes::new()
  )
  
  AuditLogger::log_security_event(audit_logger, access_event)
  SecurityMonitor::process_event(security_monitor, access_event)
  
  // Test permission denial logging
  let denial_event = SecurityEvent::new(
    SecurityEventType::PermissionDenied,
    "viewer789",
    "delete_attempt",
    "Attempted to delete telemetry data without permission",
    Attributes::new()
  )
  
  AuditLogger::log_security_event(audit_logger, denial_event)
  SecurityMonitor::process_event(security_monitor, denial_event)
  
  // Test data modification logging
  let modification_event = SecurityEvent::new(
    SecurityEventType::DataModification,
    "admin123",
    "config_change",
    "Modified telemetry collection configuration",
    Attributes::new()
  )
  
  AuditLogger::log_security_event(audit_logger, modification_event)
  SecurityMonitor::process_event(security_monitor, modification_event)
  
  // Verify audit log entries
  let audit_entries = AuditLogger::get_entries(audit_logger)
  assert_eq(audit_entries.length(), 4)
  
  // Verify security monitoring
  let security_stats = SecurityMonitor::get_statistics(security_monitor)
  assert_eq(security_stats.total_events, 4)
  assert_eq(security_stats.authentication_events, 1)
  assert_eq(security_stats.data_access_events, 1)
  assert_eq(security_stats.permission_denials, 1)
  assert_eq(security_stats.data_modifications, 1)
  
  // Test anomaly detection
  let suspicious_events = SecurityMonitor::detect_anomalies(security_monitor)
  
  // In this simple case, no anomalies should be detected
  assert_eq(suspicious_events.length(), 0)
  
  // Add suspicious activity
  for i in 0..=10 {
    let failed_auth = SecurityEvent::new(
      SecurityEventType::Authentication,
      "attacker",
      "login_failed",
      "Failed login attempt " + i.to_string(),
      Attributes::new()
    )
    
    AuditLogger::log_security_event(audit_logger, failed_auth)
    SecurityMonitor::process_event(security_monitor, failed_auth)
  }
  
  // Now anomalies should be detected
  let new_suspicious_events = SecurityMonitor::detect_anomalies(security_monitor)
  assert_true(new_suspicious_events.length() > 0)
  
  for event in new_suspicious_events {
    assert_eq(SecurityEvent::type(event), SecurityEventType::Anomaly)
  }
}

// Test 5: Data Retention and Cleanup Policies
test "data retention and cleanup policies" {
  let retention_manager = RetentionManager::new()
  
  // Define retention policies
  RetentionManager::add_policy(retention_manager, RetentionPolicy::new(
    "telemetry_metrics",
    RetentionPeriod::Days(30),
    DataAction::Archive
  ))
  
  RetentionManager::add_policy(retention_manager, RetentionPolicy::new(
    "user_activity_logs",
    RetentionPeriod::Days(90),
    DataAction::Delete
  ))
  
  RetentionManager::add_policy(retention_manager, RetentionPolicy::new(
    "security_events",
    RetentionPeriod::Years(7),
    DataAction::Archive
  ))
  
  RetentionManager::add_policy(retention_manager, RetentionPolicy::new(
    "pii_data",
    RetentionPeriod::Days(30),
    DataAction::Redact
  ))
  
  // Create test data with different ages
  let current_time = Time::now()
  let thirty_days_ago = current_time - (30 * 24 * 60 * 60 * 1000)
  let sixty_days_ago = current_time - (60 * 24 * 60 * 60 * 1000)
  let ninety_days_ago = current_time - (90 * 24 * 60 * 60 * 1000)
  
  let telemetry_data = DataRecord::new("telemetry_metrics", thirty_days_ago, "test_metrics_data")
  let user_activity_data = DataRecord::new("user_activity_logs", sixty_days_ago, "test_user_activity")
  let security_data = DataRecord::new("security_events", current_time, "test_security_event")
  let pii_data = DataRecord::new("pii_data", ninety_days_ago, "test_pii_data")
  
  // Apply retention policies
  let retention_results = RetentionManager::apply_policies(retention_manager, [
    telemetry_data,
    user_activity_data,
    security_data,
    pii_data
  ])
  
  // Verify retention results
  assert_eq(retention_results.length(), 4)
  
  // Check specific results
  let telemetry_result = retention_results[0]
  assert_eq(DataRecord::type(telemetry_result), "telemetry_metrics")
  assert_eq(DataRecord::action(telemetry_result), DataAction::Archive)
  
  let user_activity_result = retention_results[1]
  assert_eq(DataRecord::type(user_activity_result), "user_activity_logs")
  assert_eq(DataRecord::action(user_activity_result), DataAction::Delete)
  
  let security_result = retention_results[2]
  assert_eq(DataRecord::type(security_result), "security_events")
  assert_eq(DataRecord::action(security_result), DataAction::NoAction) // Still within retention period
  
  let pii_result = retention_results[3]
  assert_eq(DataRecord::type(pii_result), "pii_data")
  assert_eq(DataRecord::action(pii_result), DataAction::Redact)
  
  // Test automatic retention enforcement
  RetentionManager::enable_automatic_enforcement(retention_manager, 24 * 60 * 60 * 1000) // Daily
  
  // Verify enforcement is enabled
  assert_true(RetentionManager::is_enforcement_enabled(retention_manager))
  
  // Get retention statistics
  let stats = RetentionManager::get_statistics(retention_manager)
  assert_eq(stats.policies_defined, 4)
  assert_true(stats.records_processed > 0)
  assert_eq(stats.records_archived, 1)
  assert_eq(stats.records_deleted, 1)
  assert_eq(stats.records_redacted, 1)
}

// Test 6: Secure Data Transmission
test "secure data transmission" {
  let transmission_manager = SecureTransmissionManager::new()
  
  // Configure TLS settings
  TransmissionManager::configure_tls(transmission_manager, TLSConfig::new(
    TLSVersion::V1_3,
    [CipherSuite::AES256_GCM_SHA384, CipherSuite::CHACHA20_POLY1305_SHA256],
    true // Verify certificates
  ))
  
  // Create test data
  let telemetry_data = TelemetryData::new(12345, "auth_service", "login_operation", 150.5, true, Attributes::new())
  
  // Serialize data for transmission
  let serialized_data = TransmissionManager::serialize(telemetry_data)
  
  // Encrypt data for transmission
  let encrypted_data = TransmissionManager::encrypt_for_transmission(transmission_manager, serialized_data)
  
  // Simulate transmission (in a real implementation, this would send over network)
  let received_data = encrypted_data
  
  // Decrypt received data
  let decrypted_data = TransmissionManager::decrypt_from_transmission(transmission_manager, received_data)
  
  // Deserialize data
  let deserialized_data = TransmissionManager::deserialize(decrypted_data)
  
  // Verify data integrity
  assert_eq(TelemetryData::service_name(telemetry_data), TelemetryData::service_name(deserialized_data))
  assert_eq(TelemetryData::operation_name(telemetry_data), TelemetryData::operation_name(deserialized_data))
  assert_eq(TelemetryData::response_time(telemetry_data), TelemetryData::response_time(deserialized_data))
  
  // Test certificate validation
  let valid_cert = Certificate::new("valid_cert.pem")
  let invalid_cert = Certificate::new("invalid_cert.pem")
  
  assert_true(TransmissionManager::validate_certificate(transmission_manager, valid_cert))
  assert_false(TransmissionManager::validate_certificate(transmission_manager, invalid_cert))
  
  // Test mutual authentication
  let client_cert = Certificate::new("client_cert.pem")
  let server_cert = Certificate::new("server_cert.pem")
  
  TransmissionManager::enable_mutual_auth(transmission_manager, true)
  assert_true(TransmissionManager::is_mutual_auth_enabled(transmission_manager))
  
  // Verify mutual authentication setup
  let auth_result = TransmissionManager::setup_mutual_auth(transmission_manager, client_cert, server_cert)
  assert_true(auth_result)
  
  // Test secure channel establishment
  let secure_channel = TransmissionManager::establish_secure_channel(transmission_manager, "telemetry.example.com", 443)
  assert_true(SecureChannel::is_established(secure_channel))
  
  // Verify channel properties
  assert_eq(SecureChannel::get_protocol(secure_channel), "TLSv1.3")
  assert_eq(SecureChannel::get_cipher_suite(secure_channel), "AES256_GCM_SHA384")
}

// Test 7: Data Anonymization Techniques
test "data anonymization techniques" {
  let anonymizer = DataAnonymizer::new()
  
  // Configure anonymization techniques
  Anonymizer::add_technique(anonymizer, AnonymizationTechnique::Generalization)
  Anonymizer::add_technique(anonymizer, AnonymizationTechnique::Suppression)
  Anonymizer::add_technique(anonymizer, AnonymizationTechnique::Perturbation)
  Anonymizer::add_technique(anonymizer, AnonymizationTechnique::Pseudonymization)
  
  // Test data with various sensitive fields
  let test_record = DataRecord::new("user_activity", Time::now(), {
    "user_id": "user_12345",
    "name": "John Doe",
    "email": "john.doe@example.com",
    "age": "35",
    "zip_code": "90210",
    "ip_address": "192.168.1.100",
    "device_id": "device_abcdef123456",
    "session_id": "session_789012345678"
  })
  
  // Test generalization (age ranges, zip code prefixes)
  let generalized_record = Anonymizer::apply_technique(anonymizer, test_record, AnonymizationTechnique::Generalization)
  
  assert_eq(DataRecord::get_field(generalized_record, "age"), "30-40") // Generalized age range
  assert_eq(DataRecord::get_field(generalized_record, "zip_code"), "902**") // Generalized zip code
  
  // Test suppression (removing sensitive fields)
  let suppressed_record = Anonymizer::apply_technique(anonymizer, test_record, AnonymizationTechnique::Suppression)
  
  assert_eq(DataRecord::get_field(suppressed_record, "name"), "[REDACTED]")
  assert_eq(DataRecord::get_field(suppressed_record, "email"), "[REDACTED]")
  
  // Test perturbation (adding noise to numerical values)
  let perturbed_record = Anonymizer::apply_technique(anonymizer, test_record, AnonymizationTechnique::Perturbation)
  
  let perturbed_age = DataRecord::get_field(perturbed_record, "age").to_int()
  assert_true(perturbed_age >= 30 && perturbed_age <= 40) // Should be close to original
  
  // Test pseudonymization (replacing identifiers with pseudonyms)
  let pseudonymized_record = Anonymizer::apply_technique(anonymizer, test_record, AnonymizationTechnique::Pseudonymization)
  
  assert_ne(DataRecord::get_field(pseudonymized_record, "user_id"), "user_12345")
  assert_ne(DataRecord::get_field(pseudonymized_record, "device_id"), "device_abcdef123456")
  assert_ne(DataRecord::get_field(pseudonymized_record, "session_id"), "session_789012345678")
  
  // Verify pseudonym consistency (same input should produce same pseudonym)
  let pseudonymized_record2 = Anonymizer::apply_technique(anonymizer, test_record, AnonymizationTechnique::Pseudonymization)
  
  assert_eq(
    DataRecord::get_field(pseudonymized_record, "user_id"),
    DataRecord::get_field(pseudonymized_record2, "user_id")
  )
  
  // Test k-anonymity
  let dataset = [
    test_record,
    DataRecord::new("user_activity", Time::now(), {
      "user_id": "user_67890",
      "name": "Jane Smith",
      "email": "jane.smith@example.com",
      "age": "32",
      "zip_code": "90211",
      "ip_address": "192.168.1.101",
      "device_id": "device_123456abcdef",
      "session_id": "session_345678901234"
    }),
    DataRecord::new("user_activity", Time::now(), {
      "user_id": "user_11111",
      "name": "Bob Johnson",
      "email": "bob.johnson@example.com",
      "age": "38",
      "zip_code": "90212",
      "ip_address": "192.168.1.102",
      "device_id": "device_fedcba654321",
      "session_id": "session_567890123456"
    })
  ]
  
  let k_anonymized_dataset = Anonymizer::achieve_k_anonymity(anonymizer, dataset, 2)
  
  // Verify k-anonymity (each equivalence class should have at least k records)
  for record in k_anonymized_dataset {
    let equivalence_class = Anonymizer::get_equivalence_class(anonymizer, record)
    assert_true(equivalence_class.size() >= 2)
  }
  
  // Test differential privacy
  let dp_result = Anonymizer::apply_differential_privacy(anonymizer, dataset, 1.0) // epsilon = 1.0
  
  // Verify differential privacy properties
  assert_true(dp_result.noise_added)
  assert_true(dp_result.epsilon == 1.0)
  assert_true(dp_result.anonymized_dataset.length() == dataset.length())
}

// Test 8: Secure Configuration Management
test "secure configuration management" {
  let config_manager = SecureConfigManager::new()
  
  // Add sensitive configuration
  ConfigManager::add_config(config_manager, "database.password", "secret_password123", true)
  ConfigManager::add_config(config_manager, "api.key", "api_key_abcdef123456", true)
  ConfigManager::add_config(config_manager, "encryption.key", "encryption_key_xyz789", true)
  
  // Add non-sensitive configuration
  ConfigManager::add_config(config_manager, "service.name", "telemetry_service", false)
  ConfigManager::add_config(config_manager, "service.port", "8080", false)
  ConfigManager::add_config(config_manager, "log.level", "INFO", false)
  
  // Test configuration retrieval
  let db_password = ConfigManager::get_config(config_manager, "database.password")
  match db_password {
    Some(value) => assert_eq(value, "secret_password123")
    None => assert_true(false)
  }
  
  let service_name = ConfigManager::get_config(config_manager, "service.name")
  match service_name {
    Some(value) => assert_eq(value, "telemetry_service")
    None => assert_true(false)
  }
  
  // Test sensitive configuration masking
  let all_configs = ConfigManager::get_all_configs(config_manager, true) // Mask sensitive values
  let masked_db_password = all_configs.get("database.password")
  match masked_db_password {
    Some(value) => assert_eq(value, "******")
    None => assert_true(false)
  }
  
  let unmasked_service_name = all_configs.get("service.name")
  match unmasked_service_name {
    Some(value) => assert_eq(value, "telemetry_service") // Non-sensitive values should not be masked
    None => assert_true(false)
  }
  
  // Test configuration encryption at rest
  ConfigManager::enable_encryption_at_rest(config_manager, true)
  assert_true(ConfigManager::is_encryption_enabled(config_manager))
  
  // Store configuration to encrypted storage
  let storage_path = "/tmp/encrypted_config.json"
  let store_result = ConfigManager::store_to_file(config_manager, storage_path)
  assert_true(store_result)
  
  // Load configuration from encrypted storage
  let loaded_config_manager = SecureConfigManager::new()
  ConfigManager::enable_encryption_at_rest(loaded_config_manager, true)
  
  let load_result = ConfigManager::load_from_file(loaded_config_manager, storage_path)
  assert_true(load_result)
  
  // Verify loaded configuration
  let loaded_db_password = ConfigManager::get_config(loaded_config_manager, "database.password")
  match loaded_db_password {
    Some(value) => assert_eq(value, "secret_password123")
    None => assert_true(false)
  }
  
  // Test configuration validation
  ConfigManager::add_validator(config_manager, "service.port", fn(value) {
    let port = value.to_int()
    port >= 1 && port <= 65535
  })
  
  // Test valid configuration
  let valid_port_result = ConfigManager::validate_config(config_manager, "service.port", "8080")
  assert_true(valid_port_result)
  
  // Test invalid configuration
  let invalid_port_result = ConfigManager::validate_config(config_manager, "service.port", "99999")
  assert_false(invalid_port_result)
  
  // Test configuration change audit
  ConfigManager::enable_change_audit(config_manager, true)
  
  ConfigManager::update_config(config_manager, "log.level", "DEBUG")
  
  let audit_log = ConfigManager::get_change_audit_log(config_manager)
  assert_true(audit_log.length() > 0)
  
  let latest_change = audit_log[audit_log.length() - 1]
  assert_eq(ConfigChange::key(latest_change), "log.level")
  assert_eq(ConfigChange::old_value(latest_change), "INFO")
  assert_eq(ConfigChange::new_value(latest_change), "DEBUG")
}

// Test 9: Intrusion Detection and Prevention
test "intrusion detection and prevention" {
  let ids = IntrusionDetectionSystem::new()
  
  // Configure detection rules
  IDS::add_rule(ids, DetectionRule::new(
    "multiple_failed_logins",
    RuleType::Threshold,
    5, // threshold
    300, // time window in seconds
    "authentication.failure",
    ["user", "ip_address"]
  ))
  
  IDS::add_rule(ids, DetectionRule::new(
    "unusual_data_access",
    RuleType::Anomaly,
    0.8, // sensitivity
    0, // time window (continuous)
    "data.access",
    ["user", "resource", "volume"]
  ))
  
  IDS::add_rule(ids, DetectionRule::new(
    "privilege_escalation",
    RuleType::Pattern,
    0, // not applicable for pattern rules
    0, // not applicable for pattern rules
    "privilege.change",
    ["user", "old_role", "new_role"]
  ))
  
  // Test multiple failed logins detection
  for i in 0..=6 {
    let event = SecurityEvent::new(
      SecurityEventType::Authentication,
      "attacker",
      "login_failed",
      "Failed login attempt " + i.to_string(),
      Attributes::new()
    )
    
    IDS::process_event(ids, event)
  }
  
  let alerts = IDS::get_alerts(ids)
  let failed_login_alerts = alerts.filter(fn(alert) {
    Alert::rule_id(alert) == "multiple_failed_logins"
  })
  
  assert_true(failed_login_alerts.length() > 0)
  
  // Test unusual data access detection
  let normal_access_event = SecurityEvent::new(
    SecurityEventType::DataAccess,
    "analyst",
    "normal_access",
    "Normal data access",
    Attributes::new()
  )
  
  // Process normal events to establish baseline
  for i in 0..=20 {
    IDS::process_event(ids, normal_access_event)
  }
  
  // Process unusual event
  let unusual_access_event = SecurityEvent::new(
    SecurityEventType::DataAccess,
    "analyst",
    "unusual_access",
    "Unusually large data access",
    Attributes::new()
  )
  
  IDS::process_event(ids, unusual_access_event)
  
  let updated_alerts = IDS::get_alerts(ids)
  let unusual_access_alerts = updated_alerts.filter(fn(alert) {
    Alert::rule_id(alert) == "unusual_data_access"
  })
  
  assert_true(unusual_access_alerts.length() > 0)
  
  // Test privilege escalation detection
  let escalation_event = SecurityEvent::new(
    SecurityEventType::PrivilegeChange,
    "user123",
    "role_change",
    "User role changed from viewer to admin",
    Attributes::new()
  )
  
  IDS::process_event(ids, escalation_event)
  
  let final_alerts = IDS::get_alerts(ids)
  let escalation_alerts = final_alerts.filter(fn(alert) {
    Alert::rule_id(alert) == "privilege_escalation"
  })
  
  assert_true(escalation_alerts.length() > 0)
  
  // Test automated response
  IDS::enable_automated_response(ids, true)
  assert_true(IDS::is_automated_response_enabled(ids))
  
  // Configure response actions
  IDS::add_response_action(ids, "multiple_failed_logins", ResponseAction::BlockIP)
  IDS::add_response_action(ids, "privilege_escalation", ResponseAction::RequireReauthentication)
  
  // Verify response actions are configured
  let failed_login_response = IDS::get_response_action(ids, "multiple_failed_logins")
  match failed_login_response {
    Some(action) => assert_eq(action, ResponseAction::BlockIP)
    None => assert_true(false)
  }
  
  // Test prevention
  let blocked_ip = "192.168.1.100"
  let prevention_result = IDS::prevent_action(ids, blocked_ip, "authentication")
  assert_true(prevention_result)
  
  // Verify IP is blocked
  assert_true(IDS::is_ip_blocked(ids, blocked_ip))
  
  // Test IDS statistics
  let ids_stats = IDS::get_statistics(ids)
  assert_true(ids_stats.events_processed > 0)
  assert_true(ids_stats.alerts_generated > 0)
  assert_true(ids_stats.preventive_actions > 0)
}

// Test 10: Compliance and Regulatory Requirements
test "compliance and regulatory requirements" {
  let compliance_manager = ComplianceManager::new()
  
  // Configure compliance frameworks
  ComplianceManager::add_framework(compliance_manager, ComplianceFramework::GDPR)
  ComplianceManager::add_framework(compliance_manager, ComplianceFramework::CCPA)
  ComplianceManager::add_framework(compliance_manager, ComplianceFramework::HIPAA)
  ComplianceManager::add_framework(compliance_manager, ComplianceFramework::SOX)
  
  // Add GDPR requirements
  ComplianceManager::add_requirement(compliance_manager, "GDPR", "Art.5", "Purpose limitation")
  ComplianceManager::add_requirement(compliance_manager, "GDPR", "Art.25", "Data protection by design and by default")
  ComplianceManager::add_requirement(compliance_manager, "GDPR", "Art.32", "Security of processing")
  
  // Add CCPA requirements
  ComplianceManager::add_requirement(compliance_manager, "CCPA", "1798.150", "Right to know")
  ComplianceManager::add_requirement(compliance_manager, "CCPA", "1798.105", "Right to delete")
  
  // Add HIPAA requirements
  ComplianceManager::add_requirement(compliance_manager, "HIPAA", "164.312(a)", "Access controls")
  ComplianceManager::add_requirement(compliance_manager, "HIPAA", "164.312(e)(1)", "Transmission security")
  
  // Add SOX requirements
  ComplianceManager::add_requirement(compliance_manager, "SOX", "302", "Corporate responsibility for financial reports")
  ComplianceManager::add_requirement(compliance_manager, "SOX", "404", "Management assessment of internal controls")
  
  // Test compliance checking
  let data_processing_record = DataProcessingRecord::new(
    "user_analytics",
    ["user_id", "email", "activity"],
    "Analytics and service improvement",
    ["marketing", "product"],
    30, // retention in days
    true, // encryption enabled
    true, // pseudonymization enabled
    ["EU", "US"] // data locations
  )
  
  let gdpr_compliance = ComplianceManager::check_compliance(compliance_manager, data_processing_record, "GDPR")
  assert_true(gdpr_compliance.is_compliant)
  
  let ccpa_compliance = ComplianceManager::check_compliance(compliance_manager, data_processing_record, "CCPA")
  assert_true(ccpa_compliance.is_compliant)
  
  // Test non-compliant scenario
  let non_compliant_record = DataProcessingRecord::new(
    "user_analytics",
    ["user_id", "email", "ssn", "health_records"], // Sensitive data without proper protection
    "Analytics",
    ["marketing"],
    365, // Extended retention
    false, // No encryption
    false, // No pseudonymization
    ["EU", "US", "Unknown"] // Unknown data location
  )
  
  let hipaa_compliance = ComplianceManager::check_compliance(compliance_manager, non_compliant_record, "HIPAA")
  assert_false(hipaa_compliance.is_compliant)
  
  // Verify specific violations
  assert_true(hipaa_compliance.violations.contains("164.312(a)")) // Access controls
  assert_true(hipaa_compliance.violations.contains("164.312(e)(1)")) // Transmission security
  
  // Test data subject rights (GDPR)
  let dsr_request = DataSubjectRequest::new(
    "user_12345",
    DataSubjectRight::Access,
    "user@example.com",
    "Provide all personal data"
  )
  
  let dsr_result = ComplianceManager::process_data_subject_request(compliance_manager, dsr_request)
  assert_true(dsr_result.processed)
  assert_true(dsr_result.response_data.length() > 0)
  
  // Test data subject right to be forgotten
  let deletion_request = DataSubjectRequest::new(
    "user_12345",
    DataSubjectRight::Erasure,
    "user@example.com",
    "Delete all personal data"
  )
  
  let deletion_result = ComplianceManager::process_data_subject_request(compliance_manager, deletion_request)
  assert_true(deletion_result.processed)
  
  // Verify data was actually deleted
  let deleted_data = ComplianceManager::get_data_for_subject(compliance_manager, "user_12345")
  assert_eq(deleted_data.length(), 0)
  
  // Test compliance reporting
  let compliance_report = ComplianceManager::generate_report(compliance_manager)
  
  assert_true(compliance_report.frameworks.contains("GDPR"))
  assert_true(compliance_report.frameworks.contains("CCPA"))
  assert_true(compliance_report.frameworks.contains("HIPAA"))
  assert_true(compliance_report.frameworks.contains("SOX"))
  
  assert_true(compliance_report.overall_compliance_score >= 0.0)
  assert_true(compliance_report.overall_compliance_score <= 1.0)
  
  assert_true(compliance_report.issues.length() > 0) // Should have identified issues with non-compliant record
  
  // Test audit trail for compliance
  let audit_trail = ComplianceManager::get_audit_trail(compliance_manager)
  assert_true(audit_trail.length() > 0)
  
  // Verify audit trail contains compliance checks
  let compliance_checks = audit_trail.filter(fn(entry) {
    AuditEntry::type(entry) == "compliance_check"
  })
  assert_true(compliance_checks.length() > 0)
  
  // Verify audit trail contains data subject requests
  let dsr_entries = audit_trail.filter(fn(entry) {
    AuditEntry::type(entry) == "data_subject_request"
  })
  assert_true(dsr_entries.length() >= 2) // Access and deletion requests
}
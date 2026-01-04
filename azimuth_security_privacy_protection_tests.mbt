// Security and Privacy Protection Tests for Azimuth Telemetry System
// This file contains test cases for security and privacy protection features

// Test 1: Data Encryption and Decryption
test "data encryption and decryption" {
  let encryption_manager = EncryptionManager::new()
  
  // Initialize encryption manager
  encryption_manager.initialize(EncryptionConfig::new()
    .with_algorithm("aes-256-gcm")
    .with_key_derivation("pbkdf2")
    .with_key_rotation_interval(86400000) // 24 hours
    .with_secure_key_storage(true))
  
  // Start encryption manager
  encryption_manager.start()
  
  // Generate encryption keys
  let key_generation_result = encryption_manager.generate_encryption_keys()
  assert_true(key_generation_result.success)
  assert_true(key_generation_result.key_id.length() > 0)
  assert_true(key_generation_result.key_version > 0)
  
  // Test data encryption
  let test_data = [
    "sensitive_telemetry_data_1",
    "sensitive_telemetry_data_2",
    "sensitive_telemetry_data_3",
    "sensitive_telemetry_data_4",
    "sensitive_telemetry_data_5"
  ]
  
  let encryption_results = []
  for data in test_data {
    let result = encryption_manager.encrypt_data(data)
    encryption_results.push(result)
  }
  
  // Verify encryption results
  assert_eq(encryption_results.length(), 5)
  
  for result in encryption_results {
    assert_true(result.success)
    assert_true(result.encrypted_data.length() > 0)
    assert_true(result.encrypted_data != result.original_data)
    assert_true(result.encryption_time_ms > 0)
    assert_true(result.key_id.length() > 0)
    assert_true(result.iv.length() > 0)
    assert_true(result.tag.length() > 0)
  }
  
  // Test data decryption
  let decryption_results = []
  for encryption_result in encryption_results {
    let result = encryption_manager.decrypt_data(encryption_result.encrypted_data)
    decryption_results.push(result)
  }
  
  // Verify decryption results
  assert_eq(decryption_results.length(), 5)
  
  for (i, result) in decryption_results.enumerate() {
    assert_true(result.success)
    assert_eq(result.decrypted_data, test_data[i])
    assert_true(result.decryption_time_ms > 0)
    assert_true(result.key_id.length() > 0)
  }
  
  // Test encryption with different algorithms
  let algorithms = ["aes-256-gcm", "aes-256-cbc", "chacha20-poly1305"]
  
  for algorithm in algorithms {
    encryption_manager.set_encryption_algorithm(algorithm)
    
    let algorithm_data = "test_data_for_" + algorithm
    let encrypt_result = encryption_manager.encrypt_data(algorithm_data)
    assert_true(encrypt_result.success)
    
    let decrypt_result = encryption_manager.decrypt_data(encrypt_result.encrypted_data)
    assert_true(decrypt_result.success)
    assert_eq(decrypt_result.decrypted_data, algorithm_data)
  }
  
  // Test key rotation
  let key_rotation_result = encryption_manager.rotate_encryption_keys()
  assert_true(key_rotation_result.success)
  assert_true(key_rotation_result.old_key_id.length() > 0)
  assert_true(key_rotation_result.new_key_id.length() > 0)
  assert_ne(key_rotation_result.old_key_id, key_rotation_result.new_key_id)
  
  // Test decryption with old keys
  let old_key_decryption_results = []
  for encryption_result in encryption_results {
    let result = encryption_manager.decrypt_data(encryption_result.encrypted_data)
    old_key_decryption_results.push(result)
  }
  
  // Verify old key decryption still works
  for (i, result) in old_key_decryption_results.enumerate() {
    assert_true(result.success)
    assert_eq(result.decrypted_data, test_data[i])
  }
  
  // Test encryption performance
  let large_data = generate_large_telemetry_data(10000) // 10KB of data
  let performance_result = encryption_manager.encrypt_data(large_data)
  assert_true(performance_result.success)
  assert_true(performance_result.encryption_time_ms < 1000) // Should complete within 1 second
  
  // Test encryption metrics
  let encryption_metrics = encryption_manager.get_encryption_metrics()
  assert_true(encryption_metrics.total_encryptions > 0)
  assert_true(encryption_metrics.total_decryptions > 0)
  assert_true(encryption_metrics.average_encryption_time_ms > 0.0)
  assert_true(encryption_metrics.average_decryption_time_ms > 0.0)
  assert_true(encryption_metrics.key_rotations > 0)
  
  // Stop encryption manager
  encryption_manager.stop()
}

// Test 2: Access Control and Authentication
test "access control and authentication" {
  let access_control_manager = AccessControlManager::new()
  
  // Initialize access control manager
  access_control_manager.initialize(AccessControlConfig::new()
    .with_authentication_method("jwt")
    .with_authorization_model("rbac")
    .with_session_timeout(3600000) // 1 hour
    .with_multi_factor_auth_enabled(true))
  
  // Start access control manager
  access_control_manager.start()
  
  // Define users and roles
  let users = [
    User::new("admin", "admin_password", ["admin", "read", "write", "delete"]),
    User::new("operator", "operator_password", ["operator", "read", "write"]),
    User::new("viewer", "viewer_password", ["viewer", "read"]),
    User::new("auditor", "auditor_password", ["auditor", "read"])
  ]
  
  // Register users
  for user in users {
    let registration_result = access_control_manager.register_user(user)
    assert_true(registration_result.success)
    assert_true(registration_result.user_id.length() > 0)
  }
  
  // Test user authentication
  let authentication_results = []
  for user in users {
    let result = access_control_manager.authenticate_user(user.username, user.password)
    authentication_results.push(result)
  }
  
  // Verify authentication results
  assert_eq(authentication_results.length(), 4)
  
  for result in authentication_results {
    assert_true(result.authenticated)
    assert_true(result.session_token.length() > 0)
    assert_true(result.expires_at > get_current_time_ms())
    assert_true(result.user_roles.length() > 0)
  }
  
  // Test role-based access control
  let access_requests = [
    AccessRequest::new("admin", authentication_results[0].session_token, "read", "telemetry_data"),
    AccessRequest::new("admin", authentication_results[0].session_token, "write", "telemetry_data"),
    AccessRequest::new("admin", authentication_results[0].session_token, "delete", "telemetry_data"),
    AccessRequest::new("operator", authentication_results[1].session_token, "read", "telemetry_data"),
    AccessRequest::new("operator", authentication_results[1].session_token, "write", "telemetry_data"),
    AccessRequest::new("operator", authentication_results[1].session_token, "delete", "telemetry_data"),
    AccessRequest::new("viewer", authentication_results[2].session_token, "read", "telemetry_data"),
    AccessRequest::new("viewer", authentication_results[2].session_token, "write", "telemetry_data"),
    AccessRequest::new("viewer", authentication_results[2].session_token, "delete", "telemetry_data"),
    AccessRequest::new("auditor", authentication_results[3].session_token, "read", "audit_logs"),
    AccessRequest::new("auditor", authentication_results[3].session_token, "write", "audit_logs"),
    AccessRequest::new("auditor", authentication_results[3].session_token, "delete", "audit_logs")
  ]
  
  let access_results = []
  for request in access_requests {
    let result = access_control_manager.check_access(request)
    access_results.push(result)
  }
  
  // Verify access control results
  assert_eq(access_results.length(), 12)
  
  // Admin should have all permissions
  assert_true(access_results[0].granted) // read
  assert_true(access_results[1].granted) // write
  assert_true(access_results[2].granted) // delete
  
  // Operator should have read and write but not delete
  assert_true(access_results[3].granted) // read
  assert_true(access_results[4].granted) // write
  assert_false(access_results[5].granted) // delete
  
  // Viewer should only have read
  assert_true(access_results[6].granted) // read
  assert_false(access_results[7].granted) // write
  assert_false(access_results[8].granted) // delete
  
  // Auditor should only have read on audit logs
  assert_true(access_results[9].granted) // read audit logs
  assert_false(access_results[10].granted) // write audit logs
  assert_false(access_results[11].granted) // delete audit logs
  
  // Test session management
  let session_result = access_control_manager.create_session(authentication_results[0].user_id, "web", "192.168.1.100")
  assert_true(session_result.success)
  assert_true(session_result.session_id.length() > 0)
  assert_true(session_result.created_at > 0)
  
  // Verify session
  let session_verification = access_control_manager.verify_session(session_result.session_id)
  assert_true(session_verification.valid)
  assert_eq(session_verification.user_id, authentication_results[0].user_id)
  
  // Test session expiration
  let expired_session_result = access_control_manager.expire_session(session_result.session_id)
  assert_true(expired_session_result.success)
  
  let expired_verification = access_control_manager.verify_session(session_result.session_id)
  assert_false(expired_verification.valid)
  
  // Test multi-factor authentication
  let mfa_result = access_control_manager.setup_mfa(authentication_results[0].user_id, "totp")
  assert_true(mfa_result.success)
  assert_true(mfa_result.mfa_enabled)
  assert_true(mfa_result.secret_key.length() > 0)
  assert_true(mfa_result.backup_codes.length() > 0)
  
  // Test MFA verification
  let mfa_code = generate_totp_code(mfa_result.secret_key)
  let mfa_verification = access_control_manager.verify_mfa(authentication_results[0].user_id, mfa_code)
  assert_true(mfa_verification.success)
  assert_true(mfa_verification.verified)
  
  // Test access control metrics
  let access_metrics = access_control_manager.get_access_metrics()
  assert_true(access_metrics.total_authentication_requests > 0)
  assert_true(access_metrics.successful_authentications > 0)
  assert_true(access_metrics.failed_authentications >= 0)
  assert_true(access_metrics.total_access_checks > 0)
  assert_true(access_metrics.granted_access_requests > 0)
  assert_true(access_metrics.denied_access_requests > 0)
  assert_true(access_metrics.active_sessions >= 0)
  
  // Stop access control manager
  access_control_manager.stop()
}

// Test 3: Data Anonymization and Pseudonymization
test "data anonymization and pseudonymization" {
  let privacy_manager = PrivacyManager::new()
  
  // Initialize privacy manager
  privacy_manager.initialize(PrivacyConfig::new()
    .with_anonymization_techniques(["hashing", "tokenization", "generalization"])
    .with_pseudonymization_enabled(true)
    .with_data_retention_policy(365) // 1 year
    .with_right_to_be_forgotten_enabled(true))
  
  // Start privacy manager
  privacy_manager.start()
  
  // Test data anonymization
  let personal_data = [
    {"name": "John Doe", "email": "john.doe@example.com", "phone": "555-123-4567", "address": "123 Main St, Anytown, USA"},
    {"name": "Jane Smith", "email": "jane.smith@example.com", "phone": "555-987-6543", "address": "456 Oak Ave, Sometown, USA"},
    {"name": "Bob Johnson", "email": "bob.johnson@example.com", "phone": "555-456-7890", "address": "789 Pine Rd, Otherville, USA"}
  ]
  
  let anonymization_results = []
  for data in personal_data {
    let result = privacy_manager.anonymize_data(data)
    anonymization_results.push(result)
  }
  
  // Verify anonymization results
  assert_eq(anonymization_results.length(), 3)
  
  for result in anonymization_results {
    assert_true(result.success)
    assert_true(result.anonymized_data.contains_key("name"))
    assert_true(result.anonymized_data.contains_key("email"))
    assert_true(result.anonymized_data.contains_key("phone"))
    assert_true(result.anonymized_data.contains_key("address"))
    
    // Verify PII is anonymized
    assert_ne(result.anonymized_data.get("name"), result.original_data.get("name"))
    assert_ne(result.anonymized_data.get("email"), result.original_data.get("email"))
    assert_ne(result.anonymized_data.get("phone"), result.original_data.get("phone"))
    assert_ne(result.anonymized_data.get("address"), result.original_data.get("address"))
    
    // Verify structure is preserved
    assert_eq(result.anonymized_data.keys().length(), result.original_data.keys().length())
  }
  
  // Test data pseudonymization
  let pseudonymization_results = []
  for data in personal_data {
    let result = privacy_manager.pseudonymize_data(data)
    pseudonymization_results.push(result)
  }
  
  // Verify pseudonymization results
  assert_eq(pseudonymization_results.length(), 3)
  
  for result in pseudonymization_results {
    assert_true(result.success)
    assert_true(result.pseudonymized_data.contains_key("name"))
    assert_true(result.pseudonymized_data.contains_key("email"))
    assert_true(result.pseudonymized_data.contains_key("phone"))
    assert_true(result.pseudonymized_data.contains_key("address"))
    
    // Verify PII is pseudonymized
    assert_ne(result.pseudonymized_data.get("name"), result.original_data.get("name"))
    assert_ne(result.pseudonymized_data.get("email"), result.original_data.get("email"))
    assert_ne(result.pseudonymized_data.get("phone"), result.original_data.get("phone"))
    assert_ne(result.pseudonymized_data.get("address"), result.original_data.get("address"))
    
    // Verify pseudonym mapping is stored
    assert_true(result.pseudonym_mappings.length() > 0)
  }
  
  // Test reversible pseudonymization
  let reidentification_results = []
  for pseudonymization_result in pseudonymization_results {
    let result = privacy_manager.reidentify_data(pseudonymization_result.pseudonymized_data)
    reidentification_results.push(result)
  }
  
  // Verify reidentification results
  assert_eq(reidentification_results.length(), 3)
  
  for (i, result) in reidentification_results.enumerate() {
    assert_true(result.success)
    assert_eq(result.reidentified_data.get("name"), personal_data[i].get("name"))
    assert_eq(result.reidentified_data.get("email"), personal_data[i].get("email"))
    assert_eq(result.reidentified_data.get("phone"), personal_data[i].get("phone"))
    assert_eq(result.reidentified_data.get("address"), personal_data[i].get("address"))
  }
  
  // Test different anonymization techniques
  let techniques = ["hashing", "tokenization", "generalization", "suppression"]
  
  for technique in techniques {
    privacy_manager.set_anonymization_technique(technique)
    
    let technique_data = {"name": "Test User", "email": "test@example.com", "age": 30, "salary": 50000}
    let technique_result = privacy_manager.anonymize_data(technique_data)
    assert_true(technique_result.success)
    
    // Verify technique-specific behavior
    match technique {
      "hashing" => {
        // Should produce consistent hashes for same values
        let hash1 = privacy_manager.anonymize_data(technique_data).anonymized_data.get("name")
        let hash2 = privacy_manager.anonymize_data(technique_data).anonymized_data.get("name")
        assert_eq(hash1, hash2)
      },
      "tokenization" => {
        // Should produce reversible tokens
        assert_true(technique_result.tokens_generated > 0)
      },
      "generalization" => {
        // Should produce generalized values
        if technique_result.anonymized_data.contains_key("age") {
          let generalized_age = technique_result.anonymized_data.get("age")
          assert_true(generalized_age == "25-35" || generalized_age == "20-40")
        }
      },
      "suppression" => {
        // Should remove sensitive fields
        assert_true(technique_result.fields_suppressed > 0)
      },
      _ => assert_true(false)
    }
  }
  
  // Test right to be forgotten
  let user_id = "user_123"
  let user_data = [
    {"user_id": user_id, "name": "John Doe", "email": "john@example.com"},
    {"user_id": user_id, "name": "John Doe", "phone": "555-123-4567"},
    {"user_id": user_id, "name": "John Doe", "address": "123 Main St"}
  ]
  
  // Store user data
  for data in user_data {
    privacy_manager.store_user_data(data)
  }
  
  // Verify data exists
  let search_result = privacy_manager.find_user_data(user_id)
  assert_true(search_result.success)
  assert_eq(search_result.records_found, 3)
  
  // Execute right to be forgotten
  let deletion_result = privacy_manager.execute_right_to_be_forgotten(user_id)
  assert_true(deletion_result.success)
  assert_true(deletion_result.records_deleted > 0)
  
  // Verify data is deleted
  let post_deletion_search = privacy_manager.find_user_data(user_id)
  assert_true(post_deletion_search.success)
  assert_eq(post_deletion_search.records_found, 0)
  
  // Test privacy metrics
  let privacy_metrics = privacy_manager.get_privacy_metrics()
  assert_true(privacy_metrics.total_anonymizations > 0)
  assert_true(privacy_metrics.total_pseudonymizations > 0)
  assert_true(privacy_metrics.data_subject_requests >= 0)
  assert_true(privacy_metrics.records_anonymized > 0)
  assert_true(privacy_metrics.average_anonymization_time_ms > 0.0)
  
  // Stop privacy manager
  privacy_manager.stop()
}

// Test 4: Security Audit and Compliance
test "security audit and compliance" {
  let audit_manager = SecurityAuditManager::new()
  
  // Initialize audit manager
  audit_manager.initialize(AuditConfig::new()
    .with_audit_standards(["iso27001", "gdpr", "soc2", "hipaa"])
    .with_audit_retention_period(2555) // 7 years
    .with_continuous_monitoring_enabled(true)
    .with_alert_thresholds(AuditThresholds::new()
      .with_failed_login_threshold(5)
      .with_data_access_threshold(1000)
      .with_privilege_escalation_threshold(1)))
  
  // Start audit manager
  audit_manager.start()
  
  // Test audit log creation
  let audit_events = [
    AuditEvent::new("user_login", "user_123", "success", {"ip": "192.168.1.100", "timestamp": get_current_time_ms()}),
    AuditEvent::new("data_access", "user_123", "success", {"resource": "telemetry_data", "action": "read"}),
    AuditEvent::new("config_change", "admin", "success", {"setting": "encryption_key", "old_value": "key1", "new_value": "key2"}),
    AuditEvent::new("privilege_escalation", "user_456", "success", {"old_role": "viewer", "new_role": "operator"}),
    AuditEvent::new("data_export", "user_789", "success", {"records_count": 100, "format": "csv"})
  ]
  
  let audit_results = []
  for event in audit_events {
    let result = audit_manager.log_audit_event(event)
    audit_results.push(result)
  }
  
  // Verify audit results
  assert_eq(audit_results.length(), 5)
  
  for result in audit_results {
    assert_true(result.success)
    assert_true(result.event_id.length() > 0)
    assert_true(result.logged_at > 0)
  }
  
  // Test audit log retrieval
  let retrieval_result = audit_manager.get_audit_logs(AuditQuery::new()
    .with_time_range(get_current_time_ms() - 3600000, get_current_time_ms()) // Last hour
    .with_event_types(["user_login", "data_access"])
    .with_limit(10))
  
  assert_true(retrieval_result.success)
  assert_true(retrieval_result.logs.length() > 0)
  
  for log in retrieval_result.logs {
    assert_true(log.event_id.length() > 0)
    assert_true(log.event_type.length() > 0)
    assert_true(log.user_id.length() > 0)
    assert_true(log.result.length() > 0)
    assert_true(log.timestamp > 0)
  }
  
  // Test compliance checking
  let compliance_standards = ["iso27001", "gdpr", "soc2", "hipaa"]
  
  for standard in compliance_standards {
    let compliance_result = audit_manager.check_compliance(standard)
    assert_true(compliance_result.success)
    assert_eq(compliance_result.standard, standard)
    assert_true(compliance_result.compliance_score >= 0.0 && compliance_result.compliance_score <= 1.0)
    assert_true(compliance_result.requirements_checked > 0)
    assert_true(compliance_result.requirements_passed >= 0)
    
    // Verify compliance requirements
    for requirement in compliance_result.requirements {
      assert_true(requirement.requirement_id.length() > 0)
      assert_true(requirement.requirement_name.length() > 0)
      assert_true(requirement.compliant == false || requirement.compliant == true)
      assert_true(requirement.evidence.length() > 0)
    }
  }
  
  // Test security incident detection
  let incident_data = generate_security_incident_data(50)
  
  let incident_results = []
  for data in incident_data {
    let result = audit_manager.detect_security_incident(data)
    incident_results.push(result)
  }
  
  // Verify incident detection
  let detected_incidents = incident_results.filter(|r| r.incident_detected).length()
  assert_true(detected_incidents > 0)
  
  for result in incident_results {
    if result.incident_detected {
      assert_true(result.incident_id.length() > 0)
      assert_true(result.incident_type.length() > 0)
      assert_true(result.severity == "low" || result.severity == "medium" || result.severity == "high" || result.severity == "critical")
      assert_true(result.detected_at > 0)
    }
  }
  
  // Test audit report generation
  let report_result = audit_manager.generate_audit_report(AuditReportQuery::new()
    .with_standard("gdpr")
    .with_time_range(get_current_time_ms() - 86400000, get_current_time_ms()) // Last 24 hours
    .with_format("pdf"))
  
  assert_true(report_result.success)
  assert_true(report_result.report_id.length() > 0)
  assert_true(report_result.report_size > 0)
  assert_true(report_result.generated_at > 0)
  
  // Test continuous monitoring
  let monitoring_result = audit_manager.enable_continuous_monitoring()
  assert_true(monitoring_result.success)
  assert_true(monitoring_result.monitoring_enabled)
  
  // Generate monitoring events
  let monitoring_events = generate_monitoring_events(20)
  
  let monitoring_results = []
  for event in monitoring_events {
    let result = audit_manager.process_monitoring_event(event)
    monitoring_results.push(result)
  }
  
  // Verify monitoring results
  let alerts_triggered = monitoring_results.filter(|r| r.alert_triggered).length()
  assert_true(alerts_triggered >= 0)
  
  // Test audit trail integrity
  let integrity_result = audit_manager.verify_audit_trail_integrity()
  assert_true(integrity_result.success)
  assert_true(integrity_result.integrity_verified)
  assert_true(integrity_result.verified_records > 0)
  
  // Test audit metrics
  let audit_metrics = audit_manager.get_audit_metrics()
  assert_true(audit_metrics.total_audit_events > 0)
  assert_true(audit_metrics.compliance_checks > 0)
  assert_true(audit_metrics.security_incidents_detected >= 0)
  assert_true(audit_metrics.alerts_triggered >= 0)
  assert_true(audit_metrics.average_event_processing_time_ms > 0.0)
  
  // Stop audit manager
  audit_manager.stop()
}

// Test 5: Threat Detection and Prevention
test "threat detection and prevention" {
  let threat_manager = ThreatDetectionManager::new()
  
  // Initialize threat manager
  threat_manager.initialize(ThreatDetectionConfig::new()
    .with_detection_engines(["signature_based", "anomaly_based", "behavioral"])
    .with_prevention_actions(["block", "quarantine", "alert"])
    .with_threat_intelligence_enabled(true)
    .with_real_time_monitoring_enabled(true))
  
  // Start threat manager
  threat_manager.start()
  
  // Test threat detection rules
  let threat_rules = [
    ThreatRule::new("brute_force_login", "multiple_failed_logins", ">", 5, "block_ip", {"duration": 3600}),
    ThreatRule::new("unusual_data_access", "data_access_volume", ">", 1000, "alert", {"severity": "medium"}),
    ThreatRule::new("privilege_escalation", "role_change", "any", "quarantine_user", {"review_required": true}),
    ThreatRule::new("data_exfiltration", "data_export_volume", ">", 10000, "block", {"duration": 7200})
  ]
  
  // Register threat rules
  for rule in threat_rules {
    let registration_result = threat_manager.register_threat_rule(rule)
    assert_true(registration_result.success)
    assert_true(registration_result.rule_id.length() > 0)
  }
  
  // Test threat detection
  let security_events = generate_security_events(100)
  
  let detection_results = []
  for event in security_events {
    let result = threat_manager.detect_threat(event)
    detection_results.push(result)
  }
  
  // Verify detection results
  assert_eq(detection_results.length(), 100)
  
  let threats_detected = detection_results.filter(|r| r.threat_detected).length()
  assert_true(threats_detected > 0)
  
  for result in detection_results {
    if result.threat_detected {
      assert_true(result.threat_id.length() > 0)
      assert_true(result.threat_type.length() > 0)
      assert_true(result.severity == "low" || result.severity == "medium" || result.severity == "high" || result.severity == "critical")
      assert_true(result.confidence >= 0.0 && result.confidence <= 1.0)
      assert_true(result.detected_at > 0)
    }
  }
  
  // Test threat prevention
  let prevention_results = []
  for result in detection_results {
    if result.threat_detected {
      let prevention_result = threat_manager.prevent_threat(result.threat_id)
      prevention_results.push(prevention_result)
    }
  }
  
  // Verify prevention results
  assert_true(prevention_results.length() > 0)
  
  for result in prevention_results {
    assert_true(result.success)
    assert_true(result.action_taken.length() > 0)
    assert_true(result.action_time_ms > 0)
  }
  
  // Test threat intelligence integration
  let intelligence_feeds = [
    ThreatIntelligenceFeed::new("malicious_ips", "https://threatintel.example.com/ips", "json"),
    ThreatIntelligenceFeed::new("malware_hashes", "https://threatintel.example.com/hashes", "json"),
    ThreatIntelligenceFeed::new("vulnerability_database", "https://threatintel.example.com/vulns", "json")
  ]
  
  for feed in intelligence_feeds {
    let subscription_result = threat_manager.subscribe_intelligence_feed(feed)
    assert_true(subscription_result.success)
    assert_true(subscription_result.feed_id.length() > 0)
  }
  
  // Test threat intelligence updates
  let update_result = threat_manager.update_threat_intelligence()
  assert_true(update_result.success)
  assert_true(update_result.feeds_updated > 0)
  assert_true(update_result.new_indicators > 0)
  
  // Test behavioral analysis
  let behavioral_data = generate_behavioral_data(200)
  
  let behavioral_results = []
  for data in behavioral_data {
    let result = threat_manager.analyze_behavior(data)
    behavioral_results.push(result)
  }
  
  // Verify behavioral analysis
  let anomalies_detected = behavioral_results.filter(|r| r.anomaly_detected).length()
  assert_true(anomalies_detected >= 0)
  
  for result in behavioral_results {
    if result.anomaly_detected {
      assert_true(result.anomaly_id.length() > 0)
      assert_true(result.anomaly_type.length() > 0)
      assert_true(result.deviation_score > 0.0)
      assert_true(result.confidence >= 0.0 && result.confidence <= 1.0)
    }
  }
  
  // Test different detection engines
  let engines = ["signature_based", "anomaly_based", "behavioral", "heuristic"]
  
  for engine in engines {
    threat_manager.set_detection_engine(engine)
    
    let engine_data = generate_security_events(20)
    let engine_results = []
    
    for event in engine_data {
      let result = threat_manager.detect_threat(event)
      engine_results.push(result)
    }
    
    // Verify engine-specific behavior
    let engine_detections = engine_results.filter(|r| r.threat_detected).length()
    assert_true(engine_detections >= 0)
  }
  
  // Test threat hunting
  let hunting_result = threat_manager.initiate_threat_hunt(ThreatHuntQuery::new()
    .with_hypothesis("lateral_movement")
    .with_time_range(get_current_time_ms() - 86400000, get_current_time_ms()) // Last 24 hours
    .with_indicators(["unusual_login_patterns", "abnormal_data_access"]))
  
  assert_true(hunting_result.success)
  assert_true(hunting_result.hunt_id.length() > 0)
  assert_true(hunting_result.hypothesis.length() > 0)
  
  // Test threat metrics
  let threat_metrics = threat_manager.get_threat_metrics()
  assert_true(threat_metrics.total_threats_detected > 0)
  assert_true(threat_metrics.threats_prevented > 0)
  assert_true(threat_metrics.false_positives >= 0)
  assert_true(threat_metrics.average_detection_time_ms > 0.0)
  assert_true(threat_metrics.intelligence_feeds_active > 0)
  
  // Stop threat manager
  threat_manager.stop()
}

// Test 6: Network Security
test "network security" {
  let network_security_manager = NetworkSecurityManager::new()
  
  // Initialize network security manager
  network_security_manager.initialize(NetworkSecurityConfig::new()
    .with_firewall_enabled(true)
    .with_intrusion_detection_enabled(true)
    .with_ddos_protection_enabled(true)
    .with_ssl_tls_enabled(true)
    .with_certificate_management_enabled(true))
  
  // Start network security manager
  network_security_manager.start()
  
  // Test firewall rules
  let firewall_rules = [
    FirewallRule::new("allow_telemetry_ingress", "ingress", "tcp", 8080, "192.168.1.0/24", "allow"),
    FirewallRule::new("allow_database_egress", "egress", "tcp", 5432, "10.0.0.0/8", "allow"),
    FirewallRule::new("block_external_ssh", "ingress", "tcp", 22, "0.0.0.0/0", "block"),
    FirewallRule::new("allow_https", "ingress", "tcp", 443, "0.0.0.0/0", "allow")
  ]
  
  // Configure firewall rules
  for rule in firewall_rules {
    let configuration_result = network_security_manager.configure_firewall_rule(rule)
    assert_true(configuration_result.success)
    assert_true(configuration_result.rule_id.length() > 0)
  }
  
  // Test firewall rule evaluation
  let network_packets = generate_network_packets(100)
  
  let firewall_results = []
  for packet in network_packets {
    let result = network_security_manager.evaluate_firewall_rules(packet)
    firewall_results.push(result)
  }
  
  // Verify firewall results
  assert_eq(firewall_results.length(), 100)
  
  let allowed_packets = firewall_results.filter(|r| r.action == "allow").length()
  let blocked_packets = firewall_results.filter(|r| r.action == "block").length()
  
  assert_true(allowed_packets > 0)
  assert_true(blocked_packets > 0)
  
  // Test intrusion detection
  let intrusion_data = generate_intrusion_data(50)
  
  let intrusion_results = []
  for data in intrusion_data {
    let result = network_security_manager.detect_intrusion(data)
    intrusion_results.push(result)
  }
  
  // Verify intrusion detection
  let intrusions_detected = intrusion_results.filter(|r| r.intrusion_detected).length()
  assert_true(intrusions_detected > 0)
  
  for result in intrusion_results {
    if result.intrusion_detected {
      assert_true(result.intrusion_id.length() > 0)
      assert_true(result.intrusion_type.length() > 0)
      assert_true(result.severity == "low" || result.severity == "medium" || result.severity == "high" || result.severity == "critical")
      assert_true(result.source_ip.length() > 0)
      assert_true(result.destination_ip.length() > 0)
    }
  }
  
  // Test DDoS protection
  let ddos_data = generate_ddos_data(200)
  
  let ddos_results = []
  for data in ddos_data {
    let result = network_security_manager.detect_ddos(data)
    ddos_results.push(result)
  }
  
  // Verify DDoS detection
  let ddos_attacks_detected = ddos_results.filter(|r| r.ddos_detected).length()
  assert_true(ddos_attacks_detected > 0)
  
  for result in ddos_results {
    if result.ddos_detected {
      assert_true(result.attack_id.length() > 0)
      assert_true(result.attack_type.length() > 0)
      assert_true(result.source_ips.length() > 0)
      assert_true(result.packets_per_second > 0)
    }
  }
  
  // Test SSL/TLS certificate management
  let certificate_result = network_security_manager.generate_ssl_certificate("azimuth.example.com", 365)
  assert_true(certificate_result.success)
  assert_true(certificate_result.certificate_id.length() > 0)
  assert_true(certificate_result.certificate_path.length() > 0)
  assert_true(certificate_result.private_key_path.length() > 0)
  assert_true(certificate_result.expires_at > get_current_time_ms())
  
  // Test certificate validation
  let validation_result = network_security_manager.validate_ssl_certificate(certificate_result.certificate_id)
  assert_true(validation_result.valid)
  assert_true(validation_result.chain_valid)
  assert_true(validation_result.hostname_valid)
  assert_true(validation_result.not_expired)
  
  // Test certificate renewal
  let renewal_result = network_security_manager.renew_ssl_certificate(certificate_result.certificate_id, 730) // 2 years
  assert_true(renewal_result.success)
  assert_true(renewal_result.new_certificate_id.length() > 0)
  assert_ne(renewal_result.new_certificate_id, certificate_result.certificate_id)
  
  // Test network segmentation
  let segmentation_result = network_security_manager.create_network_segment("telemetry_segment", "192.168.10.0/24")
  assert_true(segmentation_result.success)
  assert_true(segmentation_result.segment_id.length() > 0)
  
  // Configure segment rules
  let segment_rules = [
    SegmentRule::new("allow_internal", "telemetry_segment", "192.168.10.0/24", "allow"),
    SegmentRule::new("block_external", "telemetry_segment", "0.0.0.0/0", "block")
  ]
  
  for rule in segment_rules {
    let rule_result = network_security_manager.configure_segment_rule(segmentation_result.segment_id, rule)
    assert_true(rule_result.success)
  }
  
  // Test network security metrics
  let network_metrics = network_security_manager.get_network_security_metrics()
  assert_true(network_metrics.total_packets_processed > 0)
  assert_true(network_metrics.packets_allowed > 0)
  assert_true(network_metrics.packets_blocked > 0)
  assert_true(network_metrics.intrusions_detected > 0)
  assert_true(network_metrics.ddos_attacks_detected > 0)
  assert_true(network_metrics.ssl_handshakes > 0)
  assert_true(network_metrics.certificate_validations > 0)
  
  // Stop network security manager
  network_security_manager.stop()
}

// Test 7: Data Loss Prevention
test "data loss prevention" {
  let dlp_manager = DataLossPreventionManager::new()
  
  // Initialize DLP manager
  dlp_manager.initialize(DLPConfig::new()
    .with_data_classification_enabled(true)
    .with_content_inspection_enabled(true)
    .with_exfiltration_prevention_enabled(true)
    .with_sensitive_data_patterns(["credit_card", "ssn", "email", "phone"]))
  
  // Start DLP manager
  dlp_manager.start()
  
  // Test data classification
  let data_samples = [
    {"content": "John Doe, 555-123-4567, john@example.com", "classification": "pii"},
    {"content": "Credit card: 4111-1111-1111-1111", "classification": "financial"},
    {"content": "SSN: 123-45-6789", "classification": "pii"},
    {"content": "Regular telemetry data", "classification": "public"},
    {"content": "Internal system configuration", "classification": "confidential"}
  ]
  
  let classification_results = []
  for sample in data_samples {
    let result = dlp_manager.classify_data(sample.content)
    classification_results.push(result)
  }
  
  // Verify classification results
  assert_eq(classification_results.length(), 5)
  
  for (i, result) in classification_results.enumerate() {
    assert_true(result.success)
    assert_eq(result.classification, data_samples[i].classification)
    assert_true(result.confidence >= 0.0 && result.confidence <= 1.0)
    assert_true(result.processing_time_ms > 0)
  }
  
  // Test content inspection
  let inspection_results = []
  for sample in data_samples {
    let result = dlp_manager.inspect_content(sample.content)
    inspection_results.push(result)
  }
  
  // Verify inspection results
  assert_eq(inspection_results.length(), 5)
  
  for result in inspection_results {
    assert_true(result.success)
    assert_true(result.sensitive_data_detected == false || result.sensitive_data_detected == true)
    
    if result.sensitive_data_detected {
      assert_true(result.detected_patterns.length() > 0)
      
      for pattern in result.detected_patterns {
        assert_true(pattern.pattern_type.length() > 0)
        assert_true(pattern.matches.length() > 0)
        assert_true(pattern.confidence >= 0.0 && pattern.confidence <= 1.0)
      }
    }
  }
  
  // Test data exfiltration prevention
  let exfiltration_attempts = [
    ExfiltrationAttempt::new("email", "john@example.com", "sensitive_data_1"),
    ExfiltrationAttempt::new("usb", "/dev/usb1", "sensitive_data_2"),
    ExfiltrationAttempt::new("cloud_upload", "s3://bucket/data", "sensitive_data_3"),
    ExfiltrationAttempt::new("ftp", "ftp.example.com", "public_data")
  ]
  
  let exfiltration_results = []
  for attempt in exfiltration_attempts {
    let result = dlp_manager.prevent_exfiltration(attempt)
    exfiltration_results.push(result)
  }
  
  // Verify exfiltration prevention
  assert_eq(exfiltration_results.length(), 4)
  
  // Sensitive data attempts should be blocked
  assert_true(exfiltration_results[0].blocked) // email with sensitive data
  assert_true(exfiltration_results[1].blocked) // USB with sensitive data
  assert_true(exfiltration_results[2].blocked) // cloud upload with sensitive data
  
  // Public data attempt should be allowed
  assert_false(exfiltration_results[3].blocked) // FTP with public data
  
  // Test custom sensitive data patterns
  let custom_patterns = [
    SensitiveDataPattern::new("api_key", "api[_-]?key[\\s:=]+([a-zA-Z0-9]{32,})"),
    SensitiveDataPattern::new("password", "password[\\s:=]+([a-zA-Z0-9@#$%^&*]{8,})"),
    SensitiveDataPattern::new("token", "token[\\s:=]+([a-zA-Z0-9._-]{20,})")
  ]
  
  for pattern in custom_patterns {
    let registration_result = dlp_manager.register_sensitive_pattern(pattern)
    assert_true(registration_result.success)
    assert_true(registration_result.pattern_id.length() > 0)
  }
  
  // Test custom pattern detection
  let custom_data = [
    "api_key=abcdef1234567890abcdef1234567890",
    "password=SecretPassword123",
    "token=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
  ]
  
  let custom_detection_results = []
  for data in custom_data {
    let result = dlp_manager.inspect_content(data)
    custom_detection_results.push(result)
  }
  
  // Verify custom pattern detection
  assert_eq(custom_detection_results.length(), 3)
  
  for result in custom_detection_results {
    assert_true(result.success)
    assert_true(result.sensitive_data_detected)
    assert_true(result.detected_patterns.length() > 0)
  }
  
  // Test data masking
  let masking_results = []
  for sample in data_samples {
    let result = dlp_manager.mask_sensitive_data(sample.content)
    masking_results.push(result)
  }
  
  // Verify data masking
  assert_eq(masking_results.length(), 5)
  
  for result in masking_results {
    assert_true(result.success)
    assert_true(result.masked_content.length() > 0)
    assert_ne(result.masked_content, result.original_content)
    
    // Verify sensitive data is masked
    for pattern in result.masked_patterns {
      assert_true(pattern.original_value.length() > 0)
      assert_true(pattern.masked_value.length() > 0)
      assert_ne(pattern.original_value, pattern.masked_value)
    }
  }
  
  // Test DLP policies
  let dlp_policies = [
    DLPPolicy::new("block_pii_email", "pii", "email", "block", {"notification": true}),
    DLPPolicy::new("allow_internal_financial", "financial", "internal_network", "allow", {}),
    DLPPolicy::new("quarantine_confidential", "confidential", "external", "quarantine", {"review": true})
  ]
  
  for policy in dlp_policies {
    let registration_result = dlp_manager.register_dlp_policy(policy)
    assert_true(registration_result.success)
    assert_true(registration_result.policy_id.length() > 0)
  }
  
  // Test policy enforcement
  let policy_enforcement_results = []
  for sample in data_samples {
    let result = dlp_manager.enforce_policies(sample.content, "email")
    policy_enforcement_results.push(result)
  }
  
  // Verify policy enforcement
  assert_eq(policy_enforcement_results.length(), 5)
  
  for result in policy_enforcement_results {
    assert_true(result.success)
    assert_true(result.action_taken.length() > 0)
    assert_true(result.applied_policies.length() >= 0)
  }
  
  // Test DLP metrics
  let dlp_metrics = dlp_manager.get_dlp_metrics()
  assert_true(dlp_metrics.total_data_classified > 0)
  assert_true(dlp_metrics.sensitive_data_detected > 0)
  assert_true(dlp_metrics.exfiltration_attempts_blocked > 0)
  assert_true(dlp_metrics.data_masked > 0)
  assert_true(dlp_metrics.policies_enforced > 0)
  assert_true(dlp_metrics.average_processing_time_ms > 0.0)
  
  // Stop DLP manager
  dlp_manager.stop()
}

// Test 8: Vulnerability Management
test "vulnerability management" {
  let vulnerability_manager = VulnerabilityManager::new()
  
  // Initialize vulnerability manager
  vulnerability_manager.initialize(VulnerabilityConfig::new()
    .with_vulnerability_scanning_enabled(true)
    .with_patch_management_enabled(true)
    .with_risk_assessment_enabled(true)
    .with_vulnerability_feeds(["cve", "nist", "vendor_specific"]))
  
  // Start vulnerability manager
  vulnerability_manager.start()
  
  // Test vulnerability scanning
  let scan_targets = [
    ScanTarget::new("web_server", "192.168.1.10", 8080, ["http", "ssl"]),
    ScanTarget::new("database", "192.168.1.20", 5432, ["postgresql"]),
    ScanTarget::new("application", "192.168.1.30", 8081, ["java", "spring"])
  ]
  
  let scan_results = []
  for target in scan_targets {
    let result = vulnerability_manager.scan_vulnerabilities(target)
    scan_results.push(result)
  }
  
  // Verify scan results
  assert_eq(scan_results.length(), 3)
  
  for result in scan_results {
    assert_true(result.success)
    assert_true(result.scan_id.length() > 0)
    assert_true(result.target_id == target.target_id)
    assert_true(result.vulnerabilities_found >= 0)
    assert_true(result.scan_time_ms > 0)
    
    // Verify vulnerability details
    for vulnerability in result.vulnerabilities {
      assert_true(vulnerability.cve_id.length() > 0)
      assert_true(vulnerability.severity == "low" || vulnerability.severity == "medium" || vulnerability.severity == "high" || vulnerability.severity == "critical")
      assert_true(vulnerability.cvss_score >= 0.0 && vulnerability.cvss_score <= 10.0)
      assert_true(vulnerability.description.length() > 0)
    }
  }
  
  // Test vulnerability database update
  let update_result = vulnerability_manager.update_vulnerability_database()
  assert_true(update_result.success)
  assert_true(update_result.feeds_updated > 0)
  assert_true(update_result.new_vulnerabilities > 0)
  
  // Test risk assessment
  let risk_assessment_results = []
  for scan_result in scan_results {
    let result = vulnerability_manager.assess_risk(scan_result.scan_id)
    risk_assessment_results.push(result)
  }
  
  // Verify risk assessment
  assert_eq(risk_assessment_results.length(), 3)
  
  for result in risk_assessment_results {
    assert_true(result.success)
    assert_true(result.risk_score >= 0.0 && result.risk_score <= 10.0)
    assert_true(result.risk_level == "low" || result.risk_level == "medium" || result.risk_level == "high" || result.risk_level == "critical")
    assert_true(result.remediation_priority >= 1 && result.remediation_priority <= 5)
  }
  
  // Test patch management
  let patch_results = []
  for scan_result in scan_results {
    for vulnerability in scan_result.vulnerabilities {
      if vulnerability.severity == "high" || vulnerability.severity == "critical" {
        let patch_result = vulnerability_manager.apply_patch(scan_result.target_id, vulnerability.cve_id)
        patch_results.push(patch_result)
      }
    }
  }
  
  // Verify patch results
  assert_true(patch_results.length() > 0)
  
  for result in patch_results {
    assert_true(result.success || result.failed)
    assert_true(result.patch_id.length() > 0)
    assert_true(result.target_id.length() > 0)
    assert_true(result.cve_id.length() > 0)
    
    if result.success {
      assert_true(result.applied_at > 0)
      assert_true(result.reboot_required == false || result.reboot_required == true)
    }
  }
  
  // Test vulnerability verification
  let verification_results = []
  for patch_result in patch_results {
    if patch_result.success {
      let result = vulnerability_manager.verify_patch(patch_result.patch_id)
      verification_results.push(result)
    }
  }
  
  // Verify patch verification
  assert_true(verification_results.length() > 0)
  
  for result in verification_results {
    assert_true(result.success)
    assert_true(result.verified)
    assert_true(result.vulnerability_resolved)
  }
  
  // Test vulnerability reporting
  let report_result = vulnerability_manager.generate_vulnerability_report(VulnerabilityReportQuery::new()
    .with_time_range(get_current_time_ms() - 86400000, get_current_time_ms()) // Last 24 hours
    .with_severity_filter(["high", "critical"])
    .with_format("pdf"))
  
  assert_true(report_result.success)
  assert_true(report_result.report_id.length() > 0)
  assert_true(report_result.report_size > 0)
  assert_true(report_result.generated_at > 0)
  
  // Test vulnerability metrics
  let vulnerability_metrics = vulnerability_manager.get_vulnerability_metrics()
  assert_true(vulnerability_metrics.total_vulnerabilities > 0)
  assert_true(vulnerability_metrics.critical_vulnerabilities > 0)
  assert_true(vulnerability_metrics.high_vulnerabilities > 0)
  assert_true(vulnerability_metrics.patches_applied > 0)
  assert_true(vulnerability_metrics.average_risk_score > 0.0)
  assert_true(vulnerability_metrics.average_scan_time_ms > 0.0)
  
  // Stop vulnerability manager
  vulnerability_manager.stop()
}

// Test 9: Security Incident Response
test "security incident response" {
  let incident_response_manager = IncidentResponseManager::new()
  
  // Initialize incident response manager
  incident_response_manager.initialize(IncidentResponseConfig::new()
    .with_automation_enabled(true)
    .with_escalation_policies_enabled(true)
    .with_notification_channels(["email", "slack", "sms"])
    .with_incident_workflows_enabled(true))
  
  // Start incident response manager
  incident_response_manager.start()
  
  // Test incident creation
  let security_incidents = [
    SecurityIncident::new("data_breach", "high", "Unauthorized access to sensitive data", {"affected_records": 1000}),
    SecurityIncident::new("malware_infection", "critical", "Ransomware detected on production server", {"affected_servers": 3}),
    SecurityIncident::new("ddos_attack", "medium", "DDoS attack against web services", {"attack_duration": 3600}),
    SecurityIncident::new("privilege_escalation", "high", "User privilege escalation detected", {"user_id": "user_123"})
  ]
  
  let incident_creation_results = []
  for incident in security_incidents {
    let result = incident_response_manager.create_incident(incident)
    incident_creation_results.push(result)
  }
  
  // Verify incident creation
  assert_eq(incident_creation_results.length(), 4)
  
  for result in incident_creation_results {
    assert_true(result.success)
    assert_true(result.incident_id.length() > 0)
    assert_true(result.created_at > 0)
    assert_true(result.status == "open")
  }
  
  // Test incident triage
  let triage_results = []
  for creation_result in incident_creation_results {
    let result = incident_response_manager.triage_incident(creation_result.incident_id)
    triage_results.push(result)
  }
  
  // Verify incident triage
  assert_eq(triage_results.length(), 4)
  
  for result in triage_results {
    assert_true(result.success)
    assert_true(result.priority_assigned)
    assert_true(result.assigned_to.length() > 0)
    assert_true(result.estimated_resolution_time > 0)
  }
  
  // Test incident investigation
  let investigation_results = []
  for triage_result in triage_results {
    let result = incident_response_manager.investigate_incident(triage_result.incident_id)
    investigation_results.push(result)
  }
  
  // Verify incident investigation
  assert_eq(investigation_results.length(), 4)
  
  for result in investigation_results {
    assert_true(result.success)
    assert_true(result.investigation_started)
    assert_true(result.evidence_collected >= 0)
    assert_true(result.root_cause_identified == false || result.root_cause_identified == true)
  }
  
  // Test incident containment
  let containment_results = []
  for investigation_result in investigation_results {
    let result = incident_response_manager.contain_incident(investigation_result.incident_id)
    containment_results.push(result)
  }
  
  // Verify incident containment
  assert_eq(containment_results.length(), 4)
  
  for result in containment_results {
    assert_true(result.success)
    assert_true(result.containment_actions.length() > 0)
    assert_true(result.containment_achieved == false || result.containment_achieved == true)
  }
  
  // Test incident eradication
  let eradication_results = []
  for containment_result in containment_results {
    let result = incident_response_manager.eradicate_incident(containment_result.incident_id)
    eradication_results.push(result)
  }
  
  // Verify incident eradication
  assert_eq(eradication_results.length(), 4)
  
  for result in eradication_results {
    assert_true(result.success)
    assert_true(result.eradication_actions.length() > 0)
    assert_true(result.threat_eliminated == false || result.threat_eliminated == true)
  }
  
  // Test incident recovery
  let recovery_results = []
  for eradication_result in eradication_results {
    let result = incident_response_manager.recover_from_incident(eradication_result.incident_id)
    recovery_results.push(result)
  }
  
  // Verify incident recovery
  assert_eq(recovery_results.length(), 4)
  
  for result in recovery_results {
    assert_true(result.success)
    assert_true(result.recovery_actions.length() > 0)
    assert_true(result.services_restored >= 0)
    assert_true(result.normal_operation_resumed == false || result.normal_operation_resumed == true)
  }
  
  // Test incident closure
  let closure_results = []
  for recovery_result in recovery_results {
    let result = incident_response_manager.close_incident(recovery_result.incident_id)
    closure_results.push(result)
  }
  
  // Verify incident closure
  assert_eq(closure_results.length(), 4)
  
  for result in closure_results {
    assert_true(result.success)
    assert_true(result.incident_closed)
    assert_true(result.closed_at > 0)
    assert_true(result.lessons_learned.length() > 0)
  }
  
  // Test incident escalation
  let escalation_incident = SecurityIncident::new("massive_breach", "critical", "Massive data breach affecting millions", {"affected_records": 1000000})
  let escalation_creation = incident_response_manager.create_incident(escalation_incident)
  assert_true(escalation_creation.success)
  
  let escalation_result = incident_response_manager.escalate_incident(escalation_creation.incident_id)
  assert_true(escalation_result.success)
  assert_true(escalation_result.escalated)
  assert_true(escalation_result.escalation_level > 1)
  assert_true(escalation_result.notified_stakeholders.length() > 0)
  
  // Test incident automation
  let automation_result = incident_response_manager.configure_automation_automation_rule(
    AutomationRule::new("auto_contain_malware", "malware_infection", "critical", "auto_contain", {"isolate_host": true})
  )
  assert_true(automation_result.success)
  
  // Test incident reporting
  let report_result = incident_response_manager.generate_incident_report(IncidentReportQuery::new()
    .with_time_range(get_current_time_ms() - 86400000, get_current_time_ms()) // Last 24 hours
    .with_severity_filter(["high", "critical"])
    .with_format("pdf"))
  
  assert_true(report_result.success)
  assert_true(report_result.report_id.length() > 0)
  assert_true(report_result.report_size > 0)
  assert_true(report_result.generated_at > 0)
  
  // Test incident response metrics
  let incident_metrics = incident_response_manager.get_incident_metrics()
  assert_true(incident_metrics.total_incidents > 0)
  assert_true(incident_metrics.critical_incidents > 0)
  assert_true(incident_metrics.high_incidents > 0)
  assert_true(incident_metrics.average_resolution_time_ms > 0.0)
  assert_true(incident_metrics.containment_achieved_rate > 0.0)
  assert_true(incident_metrics.escalation_rate >= 0.0)
  
  // Stop incident response manager
  incident_response_manager.stop()
}
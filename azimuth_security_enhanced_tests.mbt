// Security Enhanced Tests for Azimuth Telemetry System
// This file contains test cases for enhanced security functionality

// Test 1: Data Encryption and Decryption
test "data encryption and decryption" {
  let plaintext = "Sensitive telemetry data that needs encryption"
  
  // Test AES encryption
  let aes_key = Encryption::generate_aes_key(256)
  let aes_encrypted = Encryption::aes_encrypt(plaintext, aes_key)
  assert_true(aes_encrypted.length() > 0)
  assert_ne(aes_encrypted, plaintext)
  
  let aes_decrypted = Encryption::aes_decrypt(aes_encrypted, aes_key)
  assert_eq(aes_decrypted, plaintext)
  
  // Test RSA encryption
  let rsa_key_pair = Encryption::generate_rsa_key_pair(2048)
  let rsa_encrypted = Encryption::rsa_encrypt(plaintext, rsa_key_pair.public_key)
  assert_true(rsa_encrypted.length() > 0)
  assert_ne(rsa_encrypted, plaintext)
  
  let rsa_decrypted = Encryption::rsa_decrypt(rsa_encrypted, rsa_key_pair.private_key)
  assert_eq(rsa_decrypted, plaintext)
  
  // Test hybrid encryption (RSA + AES)
  let hybrid_encrypted = Encryption::hybrid_encrypt(plaintext, rsa_key_pair.public_key)
  let hybrid_decrypted = Encryption::hybrid_decrypt(hybrid_encrypted, rsa_key_pair.private_key)
  assert_eq(hybrid_decrypted, plaintext)
}

// Test 2: Secure Key Management
test "secure key management" {
  let key_manager = KeyManager::new()
  
  // Test key generation
  let key_id = KeyManager::generate_key(key_manager, "AES", 256)
  assert_true(key_id.length() > 0)
  
  // Test key retrieval
  let retrieved_key = KeyManager::get_key(key_manager, key_id)
  assert_true(retrieved_key.is_some())
  
  // Test key rotation
  let rotated_key_id = KeyManager::rotate_key(key_manager, key_id)
  assert_ne(key_id, rotated_key_id)
  
  let rotated_key = KeyManager::get_key(key_manager, rotated_key_id)
  assert_true(rotated_key.is_some())
  
  // Test key expiration
  let expiring_key_id = KeyManager::generate_key_with_expiry(key_manager, "AES", 256, 3600) // 1 hour
  let expiring_key = KeyManager::get_key(key_manager, expiring_key_id)
  assert_true(expiring_key.is_some())
  
  // Test key revocation
  let revoke_result = KeyManager::revoke_key(key_manager, key_id)
  assert_true(revoke_result)
  
  let revoked_key = KeyManager::get_key(key_manager, key_id)
  assert_true(revoked_key.is_none())
  
  // Test key audit log
  let audit_log = KeyManager::get_audit_log(key_manager)
  assert_true(audit_log.length() > 0)
}

// Test 3: Access Control and Authentication
test "access control and authentication" {
  let access_manager = AccessManager::new()
  
  // Test user creation
  let user_id = AccessManager::create_user(access_manager, "testuser", "password123")
  assert_true(user_id.length() > 0)
  
  // Test authentication
  let auth_result = AccessManager::authenticate(access_manager, "testuser", "password123")
  assert_true(auth_result.is_success)
  
  let invalid_auth = AccessManager::authenticate(access_manager, "testuser", "wrongpassword")
  assert_false(invalid_auth.is_success)
  
  // Test role creation and assignment
  let admin_role = AccessManager::create_role(access_manager, "admin")
  let viewer_role = AccessManager::create_role(access_manager, "viewer")
  
  AccessManager::assign_role(access_manager, user_id, admin_role)
  
  // Test permission checking
  let can_read = AccessManager::check_permission(access_manager, user_id, "telemetry.read")
  let can_write = AccessManager::check_permission(access_manager, user_id, "telemetry.write")
  let can_delete = AccessManager::check_permission(access_manager, user_id, "telemetry.delete")
  
  assert_true(can_read)
  assert_true(can_write)
  assert_true(can_delete)
  
  // Test role change
  AccessManager::assign_role(access_manager, user_id, viewer_role)
  
  let viewer_can_read = AccessManager::check_permission(access_manager, user_id, "telemetry.read")
  let viewer_can_write = AccessManager::check_permission(access_manager, user_id, "telemetry.write")
  let viewer_can_delete = AccessManager::check_permission(access_manager, user_id, "telemetry.delete")
  
  assert_true(viewer_can_read)
  assert_false(viewer_can_write)
  assert_false(viewer_can_delete)
}

// Test 4: Data Integrity and Hashing
test "data integrity and hashing" {
  let data = "Telemetry data that needs integrity verification"
  
  // Test SHA-256 hashing
  let sha256_hash = Hashing::sha256(data)
  assert_eq(sha256_hash.length(), 64) // 256 bits = 64 hex characters
  
  // Test SHA-512 hashing
  let sha512_hash = Hashing::sha512(data)
  assert_eq(sha512_hash.length(), 128) // 512 bits = 128 hex characters
  
  // Test HMAC
  let hmac_key = "secret_key_for_hmac"
  let hmac_sha256 = Hashing::hmac_sha256(data, hmac_key)
  assert_eq(hmac_sha256.length(), 64)
  
  // Test hash verification
  let is_valid_sha256 = Hashing::verify_sha256(data, sha256_hash)
  assert_true(is_valid_sha256)
  
  let is_invalid_sha256 = Hashing::verify_sha256("tampered data", sha256_hash)
  assert_false(is_invalid_sha256)
  
  // Test digital signature
  let key_pair = Encryption::generate_rsa_key_pair(2048)
  let signature = Signing::sign(data, key_pair.private_key)
  assert_true(signature.length() > 0)
  
  let is_valid_signature = Signing::verify(data, signature, key_pair.public_key)
  assert_true(is_valid_signature)
  
  let is_invalid_signature = Signing::verify("tampered data", signature, key_pair.public_key)
  assert_false(is_invalid_signature)
}

// Test 5: Secure Communication
test "secure communication" {
  // Test TLS certificate generation
  let cert_info = CertificateInfo::new(
    "test.example.com",
    "Test Organization",
    "US",
    "California",
    "San Francisco"
  )
  
  let cert_key_pair = Certificate::generate_self_signed(cert_info, 365) // 1 year validity
  assert_true(Certificate::is_valid(cert_key_pair.certificate))
  
  // Test TLS connection
  let tls_server = TLSServer::new(cert_key_pair.certificate, cert_key_pair.private_key)
  let tls_client = TLSClient::new()
  
  // Test secure message exchange
  let secure_channel = SecureChannel::establish(tls_client, tls_server)
  assert_true(SecureChannel::is_authenticated(secure_channel))
  assert_true(SecureChannel::is_encrypted(secure_channel))
  
  // Test secure message sending
  let message = "Secure telemetry data"
  SecureChannel::send_message(secure_channel, message)
  
  let received_message = SecureChannel::receive_message(secure_channel)
  assert_eq(received_message, message)
  
  // Test certificate validation
  let is_valid_cert = Certificate::validate(cert_key_pair.certificate)
  assert_true(is_valid_cert)
  
  // Test certificate chain validation
  let chain_result = Certificate::validate_chain([cert_key_pair.certificate])
  assert_true(chain_result.is_valid)
}

// Test 6: Audit Logging and Forensics
test "audit logging and forensics" {
  let audit_logger = AuditLogger::new()
  
  // Test audit log entry creation
  let log_entry = AuditLogEntry::new(
    "user123",
    "telemetry.write",
    "success",
    "Wrote metric data to database",
    [("metric_name", "cpu_usage"), ("value", "75.5")]
  )
  
  AuditLogger::log_entry(audit_logger, log_entry)
  
  // Test log querying
  let user_logs = AuditLogger::query_by_user(audit_logger, "user123")
  assert_eq(user_logs.length(), 1)
  
  let action_logs = AuditLogger::query_by_action(audit_logger, "telemetry.write")
  assert_eq(action_logs.length(), 1)
  
  // Test log tampering detection
  let original_hash = AuditLogger::calculate_log_hash(audit_logger)
  
  // Attempt to tamper with logs (should fail in real implementation)
  let tamper_result = AuditLogger::tamper_with_log(audit_logger, 0, "tampered entry")
  assert_false(tamper_result)
  
  let current_hash = AuditLogger::calculate_log_hash(audit_logger)
  assert_eq(original_hash, current_hash) // Hash should remain unchanged
  
  // Test log export for forensics
  let exported_logs = AuditLogger::export_logs(audit_logger, "json")
  assert_true(exported_logs.contains("user123"))
  assert_true(exported_logs.contains("telemetry.write"))
}

// Test 7: Privacy and Data Masking
test "privacy and data masking" {
  let sensitive_data = TelemetryData::new("user_activity", 42.5)
  sensitive_data.add_attribute("user_id", "12345")
  sensitive_data.add_attribute("ip_address", "192.168.1.100")
  sensitive_data.add_attribute("email", "user@example.com")
  sensitive_data.add_attribute("ssn", "123-45-6789")
  
  // Test PII detection
  let pii_fields = Privacy::detect_pii_fields(sensitive_data)
  assert_true(pii_fields.contains("user_id"))
  assert_true(pii_fields.contains("ip_address"))
  assert_true(pii_fields.contains("email"))
  assert_true(pii_fields.contains("ssn"))
  
  // Test data masking
  let masking_rules = [
    MaskingRule::new("user_id", MaskingType::Hash),
    MaskingRule::new("ip_address", MaskingType::Partial, 2),
    MaskingRule::new("email", MaskingType::Replace, "***@***.***"),
    MaskingRule::new("ssn", MaskingType::Partial, 4)
  ]
  
  let masked_data = Privacy::apply_masking(sensitive_data, masking_rules)
  
  let masked_user_id = masked_data.get_attribute("user_id")
  let masked_ip = masked_data.get_attribute("ip_address")
  let masked_email = masked_data.get_attribute("email")
  let masked_ssn = masked_data.get_attribute("ssn")
  
  match masked_user_id {
    StringValue(value) => assert_ne(value, "12345") // Should be hashed
    _ => assert_true(false)
  }
  
  match masked_ip {
    StringValue(value) => assert_eq(value, "19********.***") // Partially masked
    _ => assert_true(false)
  }
  
  match masked_email {
    StringValue(value) => assert_eq(value, "***@***.***") // Replaced
    _ => assert_true(false)
  }
  
  match masked_ssn {
    StringValue(value) => assert_eq(value, "****-**-6789") // Partially masked
    _ => assert_true(false)
  }
  
  // Test data anonymization
  let anonymized_data = Privacy::anonymize(sensitive_data)
  let anonymized_user_id = anonymized_data.get_attribute("user_id")
  
  match anonymized_user_id {
    StringValue(value) => assert_true(value != "12345" && value.length() > 0)
    _ => assert_true(false)
  }
}

// Test 8: Threat Detection and Prevention
test "threat detection and prevention" {
  let threat_detector = ThreatDetector::new()
  
  // Test rule-based threat detection
  ThreatDetector::add_rule(threat_detector, ThreatRule::new(
    "sql_injection",
    "SELECT.*FROM",
    ThreatSeverity::High,
    "Potential SQL injection attempt"
  ))
  
  ThreatDetector::add_rule(threat_detector, ThreatRule::new(
    "xss_attack",
    "<script>",
    ThreatSeverity::Medium,
    "Potential XSS attack"
  ))
  
  // Test threat detection
  let sql_query = "SELECT * FROM users WHERE id = 1"
  let xss_payload = "<script>alert('XSS')</script>"
  let normal_query = "GET /api/metrics HTTP/1.1"
  
  let sql_threat = ThreatDetector::analyze(threat_detector, sql_query)
  let xss_threat = ThreatDetector::analyze(threat_detector, xss_payload)
  let normal_result = ThreatDetector::analyze(threat_detector, normal_query)
  
  assert_true(sql_threat.is_threat)
  assert_true(sql_threat.severity == ThreatSeverity::High)
  assert_true(sql_threat.threat_type == "sql_injection")
  
  assert_true(xss_threat.is_threat)
  assert_true(xss_threat.severity == ThreatSeverity::Medium)
  assert_true(xss_threat.threat_type == "xss_attack")
  
  assert_false(normal_result.is_threat)
  
  // Test anomaly-based threat detection
  let baseline_requests = [
    "GET /api/metrics HTTP/1.1",
    "GET /api/health HTTP/1.1",
    "POST /api/telemetry HTTP/1.1"
  ]
  
  ThreatDetector::establish_baseline(threat_detector, baseline_requests)
  
  let anomalous_request = "DELETE /admin/users HTTP/1.1"
  let anomaly_threat = ThreatDetector::analyze(threat_detector, anomalous_request)
  
  assert_true(anomaly_threat.is_threat)
  assert_true(anomaly_threat.threat_type == "anomaly")
  
  // Test threat response
  let response = ThreatDetector::generate_response(threat_detector, sql_threat)
  assert_eq(response.action, "block")
  assert_true(response.reason.contains("SQL injection"))
}

// Test 9: Secure Configuration Management
test "secure configuration management" {
  let config_manager = SecureConfigManager::new()
  
  // Test secure configuration storage
  let sensitive_config = [
    ("database.password", "secret_password"),
    ("api.key", "secret_api_key"),
    ("encryption.key", "encryption_key_value")
  ]
  
  SecureConfigManager::store_config(config_manager, sensitive_config)
  
  // Test encrypted configuration retrieval
  let db_password = SecureConfigManager::get_config(config_manager, "database.password")
  let api_key = SecureConfigManager::get_config(config_manager, "api.key")
  let encryption_key = SecureConfigManager::get_config(config_manager, "encryption.key")
  
  assert_eq(db_password, Some("secret_password"))
  assert_eq(api_key, Some("secret_api_key"))
  assert_eq(encryption_key, Some("encryption_key_value"))
  
  // Test configuration validation
  SecureConfigManager::add_validator(config_manager, "database.password", lambda { value =>
    value.length() >= 8
  })
  
  let valid_password = SecureConfigManager::validate_and_set(config_manager, "database.password", "new_secure_password")
  assert_true(valid_password)
  
  let invalid_password = SecureConfigManager::validate_and_set(config_manager, "database.password", "weak")
  assert_false(invalid_password)
  
  // Test secure configuration backup
  let backup_result = SecureConfigManager::create_encrypted_backup(config_manager, "backup_password")
  assert_true(backup_result.is_success)
  
  // Test configuration restoration
  let restore_result = SecureConfigManager::restore_from_encrypted_backup(config_manager, backup_result.backup_path, "backup_password")
  assert_true(restore_result.is_success)
}

// Test 10: Security Compliance and Reporting
test "security compliance and reporting" {
  let compliance_manager = ComplianceManager::new()
  
  // Test compliance framework setup
  ComplianceManager::add_framework(compliance_manager, "GDPR")
  ComplianceManager::add_framework(compliance_manager, "HIPAA")
  ComplianceManager::add_framework(compliance_manager, "SOC2")
  
  // Test compliance rule definition
  ComplianceManager::add_rule(compliance_manager, ComplianceRule::new(
    "GDPR",
    "data_minimization",
    "Only collect necessary data",
    lambda { data => data.attributes.length() <= 10 }
  ))
  
  ComplianceManager::add_rule(compliance_manager, ComplianceRule::new(
    "HIPAA",
    "phi_protection",
    "Protect PHI data",
    lambda { data => !data.has_attribute("ssn") || data.is_encrypted }
  ))
  
  // Test compliance checking
  let compliant_data = TelemetryData::new("metric1", 42.5)
  compliant_data.add_attribute("source", "application")
  
  let non_compliant_data = TelemetryData::new("metric2", 75.0)
  for i = 0; i < 15; i = i + 1 {
    non_compliant_data.add_attribute("attr" + i.to_string(), "value" + i.to_string())
  }
  non_compliant_data.add_attribute("ssn", "123-45-6789")
  
  let compliant_result = ComplianceManager::check_compliance(compliance_manager, compliant_data)
  let non_compliant_result = ComplianceManager::check_compliance(compliance_manager, non_compliant_data)
  
  assert_true(compliant_result.is_compliant)
  assert_false(non_compliant_result.is_compliant)
  assert_true(non_compliant_result.violations.length() > 0)
  
  // Test compliance reporting
  let gdpr_report = ComplianceManager::generate_report(compliance_manager, "GDPR")
  assert_true(gdpr_report.contains("GDPR"))
  assert_true(gdpr_report.contains("compliance"))
  
  let all_frameworks_report = ComplianceManager::generate_comprehensive_report(compliance_manager)
  assert_true(all_frameworks_report.contains("GDPR"))
  assert_true(all_frameworks_report.contains("HIPAA"))
  assert_true(all_frameworks_report.contains("SOC2"))
  
  // Test security metrics
  let security_metrics = ComplianceManager::calculate_security_metrics(compliance_manager)
  assert_true(security_metrics.compliance_score >= 0.0 && security_metrics.compliance_score <= 100.0)
  assert_true(security_metrics.violation_count >= 0)
  assert_true(security_metrics.framework_count == 3)
}
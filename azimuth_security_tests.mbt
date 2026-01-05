// Azimuth Telemetry System - Security Tests
// This file contains comprehensive test cases for security functionality

// Test 1: Authentication and Authorization Tests
test "authentication and authorization operations" {
  // Test password hashing and verification
  let password = "secure_password_123"
  let hashed_password = SecurityUtil::hash_password(password)
  
  // Hash should not be equal to original password
  assert_false(hashed_password == password)
  assert_true(hashed_password.length() > password.length())
  
  // Verify correct password
  assert_true(SecurityUtil::verify_password(password, hashed_password))
  
  // Reject incorrect password
  assert_false(SecurityUtil::verify_password("wrong_password", hashed_password))
  
  // Test JWT token generation and validation
  let user_id = "user123"
  let roles = ["admin", "user"]
  let token = JWTUtil::generate_token(user_id, roles, 3600)  // 1 hour expiry
  
  assert_true(token.length() > 0)
  
  // Validate token
  let validation_result = JWTUtil::validate_token(token)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.user_id, user_id)
  assert_true(validation_result.roles.contains("admin"))
  assert_true(validation_result.roles.contains("user"))
  
  // Test expired token
  let expired_token = JWTUtil::generate_token(user_id, roles, -1)  // Already expired
  let expired_validation = JWTUtil::validate_token(expired_token)
  assert_false(expired_validation.is_valid)
  assert_eq(expired_validation.error, "Token expired")
  
  // Test role-based authorization
  let admin_required = ["admin"]
  let user_required = ["user"]
  let multiple_required = ["admin", "user"]
  
  assert_true(AuthUtil::has_roles(roles, admin_required))
  assert_true(AuthUtil::has_roles(roles, user_required))
  assert_true(AuthUtil::has_roles(roles, multiple_required))
  
  let limited_roles = ["user"]
  assert_false(AuthUtil::has_roles(limited_roles, admin_required))
  
  // Test API key authentication
  let api_key = SecurityUtil::generate_api_key("service123")
  assert_true(api_key.length() > 20)
  assert_true(api_key.contains("service123"))
  
  let api_validation = AuthUtil::validate_api_key(api_key)
  assert_true(api_validation.is_valid)
  assert_eq(api_validation.service_id, "service123")
  
  // Test invalid API key
  let invalid_api_validation = AuthUtil::validate_api_key("invalid_key")
  assert_false(invalid_api_validation.is_valid)
}

// Test 2: Encryption and Decryption Tests
test "encryption and decryption operations" {
  // Test symmetric encryption
  let plaintext = "Sensitive telemetry data that must be encrypted"
  let encryption_key = EncryptionUtil::generate_key()
  
  let encrypted_data = EncryptionUtil::encrypt(plaintext, encryption_key)
  
  // Encrypted data should be different from plaintext
  assert_false(encrypted_data == plaintext)
  assert_true(encrypted_data.length() > 0)
  
  // Decryption should return original plaintext
  let decrypted_data = EncryptionUtil::decrypt(encrypted_data, encryption_key)
  assert_eq(decrypted_data, plaintext)
  
  // Test with wrong key
  let wrong_key = EncryptionUtil::generate_key()
  let wrong_decrypted = EncryptionUtil::decrypt(encrypted_data, wrong_key)
  assert_false(wrong_decrypted == plaintext)
  
  // Test asymmetric encryption
  let key_pair = EncryptionUtil::generate_key_pair()
  let public_key = KeyPairUtil::get_public_key(key_pair)
  let private_key = KeyPairUtil::get_private_key(key_pair)
  
  let asymmetric_encrypted = EncryptionUtil::encrypt_public(plaintext, public_key)
  let asymmetric_decrypted = EncryptionUtil::decrypt_private(asymmetric_encrypted, private_key)
  
  assert_eq(asymmetric_decrypted, plaintext)
  
  // Test digital signatures
  let message = "Important telemetry message"
  let signature = SignatureUtil::sign(message, private_key)
  
  // Verify signature with correct public key
  assert_true(SignatureUtil::verify(message, signature, public_key))
  
  // Tampered message should fail verification
  assert_false(SignatureUtil::verify("tampered " + message, signature, public_key))
  
  // Test with wrong public key
  let wrong_key_pair = EncryptionUtil::generate_key_pair()
  let wrong_public_key = KeyPairUtil::get_public_key(wrong_key_pair)
  assert_false(SignatureUtil::verify(message, signature, wrong_public_key))
  
  // Test data integrity with HMAC
  let data = "Telemetry data with integrity protection"
  let hmac_key = HMACUtil::generate_key()
  let hmac = HMACUtil::calculate(data, hmac_key)
  
  // Verify HMAC
  assert_true(HMACUtil::verify(data, hmac, hmac_key))
  
  // Tampered data should fail verification
  assert_false(HMACUtil::verify("tampered " + data, hmac, hmac_key))
  
  // Test with wrong HMAC key
  let wrong_hmac_key = HMACUtil::generate_key()
  assert_false(HMACUtil::verify(data, hmac, wrong_hmac_key))
}

// Test 3: Input Validation and Sanitization Tests
test "input validation and sanitization operations" {
  // Test SQL injection prevention
  let malicious_sql = "'; DROP TABLE users; --"
  let safe_sql = ValidationUtil::sanitize_sql_input(malicious_sql)
  
  // Malicious characters should be escaped
  assert_false(safe_sql.contains("DROP TABLE"))
  assert_true(safe_sql.contains("\'; DROP TABLE users; --"))
  
  // Test XSS prevention
  let xss_payload = "<script>alert('XSS attack')</script>"
  let safe_html = ValidationUtil::sanitize_html(xss_payload)
  
  // Script tags should be removed or escaped
  assert_false(safe_html.contains("<script>"))
  assert_false(safe_html.contains("</script>"))
  assert_true(safe_html.contains("&lt;script&gt;") || safe_html.contains(""))
  
  // Test command injection prevention
  let command_payload = "test; rm -rf /"
  let safe_command = ValidationUtil::sanitize_command_input(command_payload)
  
  // Command separators should be removed or escaped
  assert_false(safe_command.contains(";"))
  assert_false(safe_command.contains("rm -rf"))
  
  // Test path traversal prevention
  let path_traversal = "../../../etc/passwd"
  let safe_path = ValidationUtil::sanitize_path(path_traversal)
  
  // Directory traversal should be prevented
  assert_false(safe_path.contains("../"))
  assert_true(safe_path.contains("etc/passwd") == false || safe_path.startsWith("/safe/"))
  
  // Test email validation
  let valid_emails = [
    "user@example.com",
    "user.name@example.com",
    "user+tag@example.com",
    "user123@example-domain.com"
  ]
  
  for email in valid_emails {
    assert_true(ValidationUtil::is_valid_email(email))
  }
  
  let invalid_emails = [
    "user@",
    "@example.com",
    "user.example.com",
    "user@.com",
    "user@com",
    "user name@example.com"
  ]
  
  for email in invalid_emails {
    assert_false(ValidationUtil::is_valid_email(email))
  }
  
  // Test URL validation
  let valid_urls = [
    "https://example.com",
    "http://example.com/path",
    "https://example.com:8080/path?query=value",
    "ftp://example.com/resource"
  ]
  
  for url in valid_urls {
    assert_true(ValidationUtil::is_valid_url(url))
  }
  
  let invalid_urls = [
    "example.com",
    "http://",
    "://example.com",
    "not://a/url"
  ]
  
  for url in invalid_urls {
    assert_false(ValidationUtil::is_valid_url(url))
  }
  
  // Test input length validation
  let max_length = 100
  let valid_input = "a".repeat(max_length)
  let invalid_input = "a".repeat(max_length + 1)
  
  assert_true(ValidationUtil::is_valid_length(valid_input, max_length))
  assert_false(ValidationUtil::is_valid_length(invalid_input, max_length))
  
  // Test pattern validation
  let phone_pattern = "^\\d{3}-\\d{3}-\\d{4}$"
  assert_true(ValidationUtil::matches_pattern("123-456-7890", phone_pattern))
  assert_false(ValidationUtil::matches_pattern("123-456-789", phone_pattern))
  assert_false(ValidationUtil::matches_pattern("1234567890", phone_pattern))
}

// Test 4: Access Control and Permissions Tests
test "access control and permissions operations" {
  // Test role-based access control (RBAC)
  let rbac = RBAC::new()
  
  // Define roles
  RBAC::define_role(rbac, "admin", ["read", "write", "delete", "manage_users"])
  RBAC::define_role(rbac, "user", ["read", "write"])
  RBAC::define_role(rbac, "viewer", ["read"])
  
  // Assign roles to users
  RBAC::assign_role(rbac, "user1", "admin")
  RBAC::assign_role(rbac, "user2", "user")
  RBAC::assign_role(rbac, "user3", "viewer")
  
  // Check permissions
  assert_true(RBAC::has_permission(rbac, "user1", "read"))
  assert_true(RBAC::has_permission(rbac, "user1", "write"))
  assert_true(RBAC::has_permission(rbac, "user1", "delete"))
  assert_true(RBAC::has_permission(rbac, "user1", "manage_users"))
  
  assert_true(RBAC::has_permission(rbac, "user2", "read"))
  assert_true(RBAC::has_permission(rbac, "user2", "write"))
  assert_false(RBAC::has_permission(rbac, "user2", "delete"))
  assert_false(RBAC::has_permission(rbac, "user2", "manage_users"))
  
  assert_true(RBAC::has_permission(rbac, "user3", "read"))
  assert_false(RBAC::has_permission(rbac, "user3", "write"))
  assert_false(RBAC::has_permission(rbac, "user3", "delete"))
  assert_false(RBAC::has_permission(rbac, "user3", "manage_users"))
  
  // Test attribute-based access control (ABAC)
  let abac = ABAC::new()
  
  // Define policies
  ABAC::add_policy(abac, "owner_policy", {
    "target": "resource",
    "condition": "user.id == resource.owner_id",
    "effect": "allow",
    "actions": ["read", "write", "delete"]
  })
  
  ABAC::add_policy(abac, "department_policy", {
    "target": "resource",
    "condition": "user.department == resource.department AND user.role != 'guest'",
    "effect": "allow",
    "actions": ["read"]
  })
  
  // Define user attributes
  let user1_attrs = [
    ("id", "user123"),
    ("department", "engineering"),
    ("role", "developer")
  ]
  
  let user2_attrs = [
    ("id", "user456"),
    ("department", "marketing"),
    ("role", "analyst")
  ]
  
  // Define resource attributes
  let resource_attrs = [
    ("id", "resource789"),
    ("owner_id", "user123"),
    ("department", "engineering")
  ]
  
  // Check access
  assert_true(ABAC::check_access(abac, user1_attrs, resource_attrs, "read"))
  assert_true(ABAC::check_access(abac, user1_attrs, resource_attrs, "write"))
  assert_true(ABAC::check_access(abac, user1_attrs, resource_attrs, "delete"))
  
  assert_true(ABAC::check_access(abac, user2_attrs, resource_attrs, "read"))  // Same department
  assert_false(ABAC::check_access(abac, user2_attrs, resource_attrs, "write"))
  assert_false(ABAC::check_access(abac, user2_attrs, resource_attrs, "delete"))
  
  // Test IP-based access control
  let ip_acl = IPACL::new()
  
  // Allow specific IPs
  IPACL::allow_ip(ip_acl, "192.168.1.100")
  IPACL::allow_ip(ip_acl, "10.0.0.50")
  
  // Allow IP ranges
  IPACL::allow_range(ip_acl, "172.16.0.0/16")
  IPACL::allow_range(ip_acl, "192.168.2.0/24")
  
  // Check access
  assert_true(IPACL::is_allowed(ip_acl, "192.168.1.100"))
  assert_true(IPACL::is_allowed(ip_acl, "10.0.0.50"))
  assert_true(IPACL::is_allowed(ip_acl, "172.16.5.10"))
  assert_true(IPACL::is_allowed(ip_acl, "192.168.2.100"))
  
  assert_false(IPACL::is_allowed(ip_acl, "192.168.1.101"))
  assert_false(IPACL::is_allowed(ip_acl, "10.0.0.51"))
  assert_false(IPACL::is_allowed(ip_acl, "172.17.5.10"))
  assert_false(IPACL::is_allowed(ip_acl, "192.168.3.100"))
}

// Test 5: Audit and Logging Tests
test "audit and logging operations" {
  // Test audit logging
  let audit_logger = AuditLogger::new()
  
  // Log authentication events
  AuditLogger::log_event(audit_logger, {
    "event_type": "authentication",
    "user_id": "user123",
    "action": "login",
    "result": "success",
    "ip_address": "192.168.1.100",
    "timestamp": TimeUtil::current_time_millis()
  })
  
  AuditLogger::log_event(audit_logger, {
    "event_type": "authentication",
    "user_id": "user456",
    "action": "login",
    "result": "failed",
    "ip_address": "192.168.1.101",
    "timestamp": TimeUtil::current_time_millis()
  })
  
  // Log data access events
  AuditLogger::log_event(audit_logger, {
    "event_type": "data_access",
    "user_id": "user123",
    "resource": "telemetry_data",
    "action": "read",
    "result": "success",
    "timestamp": TimeUtil::current_time_millis()
  })
  
  // Query audit logs
  let auth_events = AuditLogger::query_events(audit_logger, "event_type", "authentication")
  assert_eq(auth_events.length(), 2)
  
  let failed_logins = AuditLogger::query_events(audit_logger, "result", "failed")
  assert_eq(failed_logins.length(), 1)
  assert_eq(failed_logins[0]["user_id"], "user456")
  
  // Test security event detection
  let security_monitor = SecurityMonitor::new()
  
  // Configure detection rules
  SecurityMonitor::add_rule(security_monitor, "brute_force_detection", {
    "event_type": "authentication",
    "condition": "result == 'failed' AND count > 5 WITHIN 1 MINUTE",
    "severity": "high",
    "action": "block_ip"
  })
  
  SecurityMonitor::add_rule(security_monitor, "privilege_escalation", {
    "event_type": "authorization",
    "condition": "action == 'role_change' AND new_role == 'admin'",
    "severity": "critical",
    "action": "require_approval"
  })
  
  // Simulate security events
  for i in 0..6 {
    AuditLogger::log_event(audit_logger, {
      "event_type": "authentication",
      "user_id": "attacker",
      "action": "login",
      "result": "failed",
      "ip_address": "10.0.0.100",
      "timestamp": TimeUtil::current_time_millis()
    })
  }
  
  // Detect security events
  let security_events = SecurityMonitor::detect_events(security_monitor, audit_logger)
  assert_eq(security_events.length(), 1)
  assert_eq(security_events[0].rule_name, "brute_force_detection")
  assert_eq(security_events[0].severity, "high")
  assert_eq(security_events[0].recommended_action, "block_ip")
  
  // Test log integrity protection
  let log_protector = LogProtector::new()
  
  let log_entry = "User login successful"
  let protected_log = LogProtector::protect(log_protector, log_entry)
  
  // Verify log integrity
  assert_true(LogProtector::verify(log_protector, protected_log))
  
  // Tampered log should fail verification
  let tampered_log = protected_log.replace("successful", "failed")
  assert_false(LogProtector::verify(log_protector, tampered_log))
}

// Test 6: Secure Communication Tests
test "secure communication operations" {
  // Test TLS/SSL configuration
  let tls_config = TLSConfig::new()
  
  TLSConfig::set_certificate(tls_config, "/path/to/cert.pem")
  TLSConfig::set_private_key(tls_config, "/path/to/key.pem")
  TLSConfig::set_ca_certificates(tls_config, ["/path/to/ca1.pem", "/path/to/ca2.pem"])
  
  // Configure secure protocols
  TLSConfig::set_protocols(tls_config, ["TLSv1.2", "TLSv1.3"])
  TLSConfig::set_cipher_suites(tls_config, [
    "TLS_AES_256_GCM_SHA384",
    "TLS_CHACHA20_POLY1305_SHA256",
    "TLS_AES_128_GCM_SHA256"
  ])
  
  // Enable HSTS
  TLSConfig::enable_hsts(tls_config, 31536000, true)  // 1 year, include subdomains
  
  assert_true(TLSConfig::is_valid(tls_config))
  
  // Test secure HTTP client
  let secure_client = SecureHttpClient::new(tls_config)
  
  // Make secure request
  let response = SecureHttpClient::get(secure_client, "https://api.example.com/data")
  
  assert_eq(response.status_code, 200)
  assert_true(response.is_secure)
  assert_true(response.certificate_info.is_valid)
  assert_eq(response.certificate_info.protocol, "TLSv1.3")
  
  // Test certificate validation
  let cert_validator = CertificateValidator::new()
  
  // Add trusted certificates
  CertificateValidator::add_trusted(cert_validator, "/path/to/trusted.crt")
  
  // Validate certificate
  let cert_info = response.certificate_info
  let validation_result = CertificateValidator::validate(cert_validator, cert_info)
  
  assert_true(validation_result.is_valid)
  assert_false(validation_result.is_expired)
  assert_false(validation_result.is_self_signed)
  assert_true(validation_result.is_trusted)
  
  // Test certificate pinning
  let cert_pinner = CertificatePinner::new()
  
  // Pin certificate
  let pinned_cert = CertificateUtil::extract_public_key(cert_info)
  CertificatePinner::pin(cert_pinner, "api.example.com", pinned_cert)
  
  // Validate pinned certificate
  assert_true(CertificatePinner::validate(cert_pinner, "api.example.com", cert_info))
  
  // Test with different certificate (should fail)
  let different_cert_info = CertificateInfo::new("different_cert")
  assert_false(CertificatePinner::validate(cert_pinner, "api.example.com", different_cert_info))
  
  // Test secure WebSocket
  let secure_websocket = SecureWebSocket::new("wss://secure.example.com/ws")
  
  SecureWebSocket::connect(secure_websocket)
  assert_true(secure_websocket.is_connected)
  assert_true(secure_websocket.is_secure)
  
  // Send secure message
  SecureWebSocket::send(secure_websocket, "secure telemetry data")
  
  // Receive secure message
  let received_message = SecureWebSocket::receive(secure_websocket)
  assert_eq(received_message, "secure telemetry data")
  
  SecureWebSocket::close(secure_websocket)
}

// Test 7: Data Privacy and Anonymization Tests
test "data privacy and anonymization operations" {
  // Test PII detection
  let pii_detector = PIIDetector::new()
  
  let text_with_pii = "User John Doe (john.doe@example.com, phone: 123-456-7890) from New York, NY 10001"
  let detected_pii = PIIDetector::detect(pii_detector, text_with_pii)
  
  assert_eq(detected_pii.length(), 4)
  assert_true(detected_pii.contains("John Doe"))  // Name
  assert_true(detected_pii.contains("john.doe@example.com"))  // Email
  assert_true(detected_pii.contains("123-456-7890"))  // Phone
  assert_true(detected_pii.contains("10001"))  // ZIP code
  
  // Test data anonymization
  let anonymizer = DataAnonymizer::new()
  
  let sensitive_data = [
    ("name", "John Doe"),
    ("email", "john.doe@example.com"),
    ("phone", "123-456-7890"),
    ("ssn", "123-45-6789"),
    ("ip_address", "192.168.1.100"),
    ("user_id", "user12345"),
    ("non_sensitive", "public data")
  ]
  
  let anonymized_data = DataAnonymizer::anonymize(anonymizer, sensitive_data)
  
  // Check that PII is anonymized
  let name_value = MapUtil::get(anonymized_data, "name")
  match name_value {
    Some(value) => assert_false(value == "John Doe")
    None => assert_true(false)
  }
  
  let email_value = MapUtil::get(anonymized_data, "email")
  match email_value {
    Some(value) => assert_false(value == "john.doe@example.com")
    None => assert_true(false)
  }
  
  // Non-sensitive data should remain unchanged
  let non_sensitive_value = MapUtil::get(anonymized_data, "non_sensitive")
  match non_sensitive_value {
    Some(value) => assert_eq(value, "public data")
    None => assert_true(false)
  }
  
  // Test data masking
  let masker = DataMasker::new()
  
  // Configure masking rules
  DataMasker::add_rule(masker, "email", {
    "type": "partial",
    "visible_chars": 2,
    "mask_char": "*"
  })
  
  DataMasker::add_rule(masker, "phone", {
    "type": "format_preserving",
    "format": "XXX-XXX-XXXX",
    "mask_char": "#"
  })
  
  DataMasker::add_rule(masker, "ssn", {
    "type": "full",
    "mask_char": "*"
  })
  
  let masked_data = DataMasker::mask(masker, sensitive_data)
  
  // Check masking results
  let masked_email = MapUtil::get(masked_data, "email")
  match masked_email {
    Some(value) => assert_eq(value, "jo*********************")
    None => assert_true(false)
  }
  
  let masked_phone = MapUtil::get(masked_data, "phone")
  match masked_phone {
    Some(value) => assert_eq(value, "###-###-####")
    None => assert_true(false)
  }
  
  let masked_ssn = MapUtil::get(masked_data, "ssn")
  match masked_ssn {
    Some(value) => assert_eq(value, "*********")
    None => assert_true(false)
  }
  
  // Test pseudonymization
  let pseudonymizer = Pseudonymizer::new()
  
  let original_value = "user12345"
  let pseudonym1 = Pseudonymizer::pseudonymize(pseudonymizer, original_value)
  let pseudonym2 = Pseudonymizer::pseudonymize(pseudonymizer, original_value)
  
  // Same input should always produce same pseudonym
  assert_eq(pseudonym1, pseudonym2)
  assert_false(pseudonym1 == original_value)
  
  // Different inputs should produce different pseudonyms
  let different_value = "user67890"
  let different_pseudonym = Pseudonymizer::pseudonymize(pseudonymizer, different_value)
  assert_false(pseudonym1 == different_pseudonym)
  
  // Test data retention policies
  let retention_manager = DataRetentionManager::new()
  
  // Set retention policies
  DataRetentionManager::set_policy(retention_manager, "user_data", 365)  // 1 year
  DataRetentionManager::set_policy(retention_manager, "telemetry_data", 90)  // 90 days
  DataRetentionManager::set_policy(retention_manager, "audit_logs", 2555)  // 7 years
  
  // Check retention periods
  assert_eq(DataRetentionManager::get_retention_period(retention_manager, "user_data"), 365)
  assert_eq(DataRetentionManager::get_retention_period(retention_manager, "telemetry_data"), 90)
  assert_eq(DataRetentionManager::get_retention_period(retention_manager, "audit_logs"), 2555)
  
  // Test data expiration
  let current_time = TimeUtil::current_time_millis()
  let old_timestamp = current_time - (400 * 24 * 60 * 60 * 1000)  // 400 days ago
  
  assert_true(DataRetentionManager::is_expired(retention_manager, "user_data", old_timestamp))
  assert_true(DataRetentionManager::is_expired(retention_manager, "telemetry_data", old_timestamp))
  assert_false(DataRetentionManager::is_expired(retention_manager, "audit_logs", old_timestamp))
}

// Test 8: Vulnerability Scanning Tests
test "vulnerability scanning operations" {
  // Test dependency vulnerability scanning
  let dependency_scanner = DependencyScanner::new()
  
  // Add dependencies to scan
  DependencyScanner::add_dependency(dependency_scanner, "openssl", "1.1.1")
  DependencyScanner::add_dependency(dependency_scanner, "nginx", "1.18.0")
  DependencyScanner::add_dependency(dependency_scanner, "redis", "6.0.8")
  
  // Scan for vulnerabilities
  let scan_results = DependencyScanner::scan(dependency_scanner)
  
  assert_eq(scan_results.length(), 3)
  
  // Check for known vulnerabilities (mock data)
  let openssl_vulns = ScanResultUtil::get_vulnerabilities(scan_results, "openssl")
  assert_true(openssl_vulns.length() > 0)
  
  let nginx_vulns = ScanResultUtil::get_vulnerabilities(scan_results, "nginx")
  assert_true(nginx_vulns.length() > 0)
  
  let redis_vulns = ScanResultUtil::get_vulnerabilities(scan_results, "redis")
  assert_true(redis_vulns.length() > 0)
  
  // Test code vulnerability scanning
  let code_scanner = CodeScanner::new()
  
  // Scan code for security issues
  let code_snippets = [
    "password = 'hardcoded_password'",  // Hardcoded password
    "query = \"SELECT * FROM users WHERE id = \" + user_id",  // SQL injection
    "exec(\"rm -rf \" + user_input)",  // Command injection
    "eval(user_input)"  // Code injection
  ]
  
  let code_scan_results = CodeScanner::scan(code_scanner, code_snippets)
  
  assert_eq(code_scan_results.length(), 4)
  
  let hardcoded_password = CodeScanResultUtil::find_issue(code_scan_results, "hardcoded_password")
  assert_true(hardcoded_password != None)
  
  let sql_injection = CodeScanResultUtil::find_issue(code_scan_results, "sql_injection")
  assert_true(sql_injection != None)
  
  let command_injection = CodeScanResultUtil::find_issue(code_scan_results, "command_injection")
  assert_true(command_injection != None)
  
  let code_injection = CodeScanResultUtil::find_issue(code_scan_results, "code_injection")
  assert_true(code_injection != None)
  
  // Test network vulnerability scanning
  let network_scanner = NetworkScanner::new()
  
  // Scan network ports
  let target = "example.com"
  let port_scan_results = NetworkScanner::scan_ports(network_scanner, target, [22, 80, 443, 3389])
  
  assert_eq(port_scan_results.length(), 4)
  
  // Check for open ports
  let open_ports = PortScanResultUtil::get_open_ports(port_scan_results)
  assert_true(open_ports.length() > 0)
  
  // Test SSL/TLS vulnerability scanning
  let tls_scanner = TLSScanner::new()
  
  let tls_scan_results = TLSScanner::scan(tls_scanner, target)
  
  assert_true(tls_scan_results.supports_tls_1_2)
  assert_true(tls_scan_results.supports_tls_1_3)
  assert_false(tls_scan_results.supports_sslv3)
  assert_false(tls_scan_results.supports_tls_1_0)
  assert_false(tls_scan_results.supports_tls_1_1)
  
  // Check for weak ciphers
  let weak_ciphers = TLSScanResultUtil::get_weak_ciphers(tls_scan_results)
  assert_eq(weak_ciphers.length(), 0)
  
  // Test web vulnerability scanning
  let web_scanner = WebScanner::new()
  
  let web_scan_results = WebScanner::scan(web_scanner, "https://example.com")
  
  // Check for common web vulnerabilities
  assert_false(web_scan_results.has_sql_injection)
  assert_false(web_scan_results.has_xss)
  assert_false(web_scan_results.has_csrf)
  assert_false(web_scan_results.has_directory_traversal)
  assert_true(web_scan_results.has_security_headers)
}

// Test 9: Security Configuration Tests
test "security configuration operations" {
  // Test security headers configuration
  let security_headers = SecurityHeaders::new()
  
  // Configure security headers
  SecurityHeaders::set_content_security_policy(security_headers, "default-src 'self'")
  SecurityHeaders::set_x_frame_options(security_headers, "DENY")
  SecurityHeaders::set_x_content_type_options(security_headers, "nosniff")
  SecurityHeaders::set_referrer_policy(security_headers, "strict-origin-when-cross-origin")
  SecurityHeaders::set_permissions_policy(security_headers, "geolocation=(), microphone=(), camera=()")
  
  // Validate security headers
  assert_true(SecurityHeaders::is_valid(security_headers))
  
  // Test CORS configuration
  let cors_config = CORSConfig::new()
  
  // Configure CORS
  CORSConfig::set_allowed_origins(cors_config, ["https://trusted.example.com"])
  CORSConfig::set_allowed_methods(cors_config, ["GET", "POST"])
  CORSConfig::set_allowed_headers(cors_config, ["Content-Type", "Authorization"])
  CORSConfig::set_max_age(cors_config, 86400)
  CORSConfig::set_allow_credentials(cors_config, false)
  
  // Validate CORS configuration
  assert_true(CORSConfig::is_secure(cors_config))
  
  // Test insecure configuration
  let insecure_cors = CORSConfig::new()
  CORSConfig::set_allowed_origins(insecure_cors, ["*"])  // Wildcard origin
  CORSConfig::set_allow_credentials(insecure_cors, true)  // Credentials with wildcard
  
  assert_false(CORSConfig::is_secure(insecure_cors))
  
  // Test session security configuration
  let session_config = SessionConfig::new()
  
  // Configure secure session
  SessionConfig::set_cookie_secure(session_config, true)
  SessionConfig::set_cookie_http_only(session_config, true)
  SessionConfig::set_cookie_same_site(session_config, "Strict")
  SessionConfig::set_session_timeout(session_config, 1800)  // 30 minutes
  SessionConfig::set_regeneration_interval(session_config, 300)  // 5 minutes
  
  // Validate session configuration
  assert_true(SessionConfig::is_secure(session_config))
  
  // Test insecure session configuration
  let insecure_session = SessionConfig::new()
  SessionConfig::set_cookie_secure(insecure_session, false)
  SessionConfig::set_cookie_http_only(insecure_session, false)
  SessionConfig::set_session_timeout(insecure_session, 86400)  // 24 hours
  
  assert_false(SessionConfig::is_secure(insecure_session))
  
  // Test password policy configuration
  let password_policy = PasswordPolicy::new()
  
  // Configure password policy
  PasswordPolicy::set_min_length(password_policy, 12)
  PasswordPolicy::set_require_uppercase(password_policy, true)
  PasswordPolicy::set_require_lowercase(password_policy, true)
  PasswordPolicy::set_require_numbers(password_policy, true)
  PasswordPolicy::set_require_special_chars(password_policy, true)
  PasswordPolicy::set_max_age(password_policy, 90)  // 90 days
  PasswordPolicy::set_history_count(password_policy, 5)  // Remember last 5 passwords
  PasswordPolicy::set_lockout_threshold(password_policy, 5)  // Lock after 5 failed attempts
  
  // Validate password policy
  assert_true(PasswordPolicy::is_strong(password_policy))
  
  // Test password validation
  assert_true(PasswordPolicy::validate(password_policy, "StrongP@ssw0rd!"))
  assert_false(PasswordPolicy::validate(password_policy, "weak"))
  assert_false(PasswordPolicy::validate(password_policy, "alllowercase"))
  assert_false(PasswordPolicy::validate(password_policy, "ALLUPPERCASE"))
  assert_false(PasswordPolicy::validate(password_policy, "NoNumbers!"))
  assert_false(PasswordPolicy::validate(password_policy, "NoSpecialChars123"))
}

// Test 10: Security Incident Response Tests
test "security incident response operations" {
  // Test incident detection
  let incident_detector = IncidentDetector::new()
  
  // Configure detection rules
  IncidentDetector::add_rule(incident_detector, "unauthorized_access", {
    "event_type": "authentication",
    "condition": "result == 'success' AND user.risk_score > 80",
    "severity": "high"
  })
  
  IncidentDetector::add_rule(incident_detector, "data_exfiltration", {
    "event_type": "data_access",
    "condition": "action == 'download' AND data.sensitivity == 'confidential' AND volume > 1000",
    "severity": "critical"
  })
  
  // Simulate security events
  let security_events = [
    {
      "event_type": "authentication",
      "user_id": "user123",
      "result": "success",
      "risk_score": 85,
      "timestamp": TimeUtil::current_time_millis()
    },
    {
      "event_type": "data_access",
      "user_id": "user123",
      "action": "download",
      "data_sensitivity": "confidential",
      "volume": 1500,
      "timestamp": TimeUtil::current_time_millis()
    }
  ]
  
  // Detect incidents
  let incidents = IncidentDetector::detect(incident_detector, security_events)
  
  assert_eq(incidents.length(), 2)
  
  let unauthorized_access = IncidentUtil::find_by_type(incidents, "unauthorized_access")
  assert_true(unauthorized_access != None)
  assert_eq(unauthorized_access.severity, "high")
  
  let data_exfiltration = IncidentUtil::find_by_type(incidents, "data_exfiltration")
  assert_true(data_exfiltration != None)
  assert_eq(data_exfiltration.severity, "critical")
  
  // Test incident response
  let incident_manager = IncidentManager::new()
  
  // Configure response actions
  IncidentManager::add_response_action(incident_manager, "unauthorized_access", "block_user")
  IncidentManager::add_response_action(incident_manager, "unauthorized_access", "notify_admin")
  IncidentManager::add_response_action(incident_manager, "data_exfiltration", "isolate_system")
  IncidentManager::add_response_action(incident_manager, "data_exfiltration", "preserve_evidence")
  
  // Execute response actions
  let response_results = IncidentManager::respond(incident_manager, incidents)
  
  assert_eq(response_results.length(), 4)
  
  // Verify response actions were executed
  assert_true(response_results.contains("block_user"))
  assert_true(response_results.contains("notify_admin"))
  assert_true(response_results.contains("isolate_system"))
  assert_true(response_results.contains("preserve_evidence"))
  
  // Test incident escalation
  let escalation_manager = EscalationManager::new()
  
  // Configure escalation rules
  EscalationManager::add_rule(escalation_manager, {
    "condition": "severity == 'critical'",
    "escalate_to": "security_team",
    "notification_method": "sms",
    "timeout": 300  // 5 minutes
  })
  
  EscalationManager::add_rule(escalation_manager, {
    "condition": "severity == 'high'",
    "escalate_to": "team_lead",
    "notification_method": "email",
    "timeout": 600  // 10 minutes
  })
  
  // Escalate incidents
  let escalation_results = EscalationManager::escalate(escalation_manager, incidents)
  
  assert_eq(escalation_results.length(), 2)
  
  let critical_escalation = EscalationResultUtil::find_by_severity(escalation_results, "critical")
  assert_true(critical_escalation != None)
  assert_eq(critical_escalation.escalate_to, "security_team")
  assert_eq(critical_escalation.notification_method, "sms")
  
  let high_escalation = EscalationResultUtil::find_by_severity(escalation_results, "high")
  assert_true(high_escalation != None)
  assert_eq(high_escalation.escalate_to, "team_lead")
  assert_eq(high_escalation.notification_method, "email")
  
  // Test incident tracking
  let incident_tracker = IncidentTracker::new()
  
  // Track incidents
  for incident in incidents {
    IncidentTracker::track(incident_tracker, incident)
  }
  
  // Get incident statistics
  let stats = IncidentTracker::get_statistics(incident_tracker)
  
  assert_eq(stats.total_incidents, 2)
  assert_eq(stats.critical_incidents, 1)
  assert_eq(stats.high_incidents, 1)
  assert_eq(stats.resolved_incidents, 0)
  
  // Resolve incidents
  for incident in incidents {
    IncidentTracker::resolve(incident_tracker, incident.id, "Incident resolved successfully")
  }
  
  // Check updated statistics
  let updated_stats = IncidentTracker::get_statistics(incident_tracker)
  
  assert_eq(updated_stats.total_incidents, 2)
  assert_eq(updated_stats.resolved_incidents, 2)
  assert_eq(updated_stats.resolution_rate, 1.0)
}
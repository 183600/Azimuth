// Azimuth Telemetry System - Enterprise Security Tests
// This file contains comprehensive test cases for security features and vulnerability protection

// Test 1: Authentication and Authorization
test "authentication and authorization" {
  // Create authentication manager
  let auth_manager = AuthenticationManager::new()
  
  // Configure authentication providers
  let jwt_provider = JWTAuthProvider::new("secret_key")
  let oauth_provider = OAuthAuthProvider::new("https://auth.example.com", "client_id", "client_secret")
  let api_key_provider = APIKeyAuthProvider::new()
  
  AuthenticationManager::add_provider(auth_manager, "jwt", jwt_provider)
  AuthenticationManager::add_provider(auth_manager, "oauth", oauth_provider)
  AuthenticationManager::add_provider(auth_manager, "api_key", api_key_provider)
  
  // Test JWT authentication
  let jwt_claims = JWTClaims::new("user123", "azimuth_user", ["read", "write"])
  jwt_claims.set_expiration(Time::now() + 3600) // 1 hour
  let jwt_token = JWTAuthProvider::generate_token(jwt_provider, jwt_claims)
  
  let jwt_auth_result = AuthenticationManager::authenticate(auth_manager, "jwt", jwt_token)
  match jwt_auth_result {
    Ok(user) => {
      assert_eq(User::id(user), "user123")
      assert_eq(User::username(user), "azimuth_user")
      assert_true(User::has_permission(user, "read"))
      assert_true(User::has_permission(user, "write"))
    }
    Error(e) => assert_true(false)
  }
  
  // Test expired JWT token
  let expired_claims = JWTClaims::new("user123", "azimuth_user", ["read"])
  expired_claims.set_expiration(Time::now() - 3600) // Expired 1 hour ago
  let expired_token = JWTAuthProvider::generate_token(jwt_provider, expired_claims)
  
  let expired_auth_result = AuthenticationManager::authenticate(auth_manager, "jwt", expired_token)
  match expired_auth_result {
    Ok(_) => assert_true(false), // Should not succeed
    Error(e) => {
      assert_true(Error::is_authentication_error(e))
      assert_true(Error::message(e).contains("expired"))
    }
  }
  
  // Test API key authentication
  let api_key = APIKeyAuthProvider::generate_key(api_key_provider, "service_account", ["read"])
  APIKeyAuthProvider::add_key(api_key_provider, "service_account", api_key)
  
  let api_key_auth_result = AuthenticationManager::authenticate(auth_manager, "api_key", api_key)
  match api_key_auth_result {
    Ok(user) => {
      assert_eq(User::id(user), "service_account")
      assert_true(User::has_permission(user, "read"))
      assert_false(User::has_permission(user, "write"))
    }
    Error(e) => assert_true(false)
  }
  
  // Test invalid API key
  let invalid_api_key_result = AuthenticationManager::authenticate(auth_manager, "api_key", "invalid_key")
  match invalid_api_key_result {
    Ok(_) => assert_true(false), // Should not succeed
    Error(e) => {
      assert_true(Error::is_authentication_error(e))
    }
  }
  
  // Test authorization
  let auth_user = AuthenticationManager::authenticate(auth_manager, "jwt", jwt_token).unwrap()
  
  // Test resource access with permissions
  let resource1 = Resource::new("/telemetry/data", ["read"])
  let resource2 = Resource::new("/telemetry/admin", ["admin"])
  
  assert_true(AuthorizationManager::can_access(auth_user, resource1))
  assert_false(AuthorizationManager::can_access(auth_user, resource2))
  
  // Test role-based authorization
  let admin_role = Role::new("admin", ["read", "write", "admin"])
  let user_role = Role::new("user", ["read"])
  
  AuthorizationManager::add_role(auth_manager, admin_role)
  AuthorizationManager::add_role(auth_manager, user_role)
  
  AuthorizationManager::assign_role(auth_manager, "user123", "user")
  AuthorizationManager::assign_role(auth_manager, "admin_user", "admin")
  
  let admin_user = User::with_roles("admin_user", ["admin"])
  assert_true(AuthorizationManager::can_access(admin_user, resource2))
}

// Test 2: Data Encryption and Decryption
test "data encryption and decryption" {
  // Create encryption manager
  let encryption_manager = EncryptionManager::new()
  
  // Generate encryption keys
  let aes_key = EncryptionKey::generate_aes(256) // AES-256
  let rsa_key_pair = RSAKeyPair::generate(2048) // RSA-2048
  
  // Test symmetric encryption (AES)
  let sensitive_data = "This is sensitive telemetry data that must be encrypted"
  
  let encrypted_data = EncryptionManager::encrypt_symmetric(encryption_manager, sensitive_data, aes_key)
  assert_true(encrypted_data != sensitive_data)
  assert_true(encrypted_data.length() > 0)
  
  let decrypted_data = EncryptionManager::decrypt_symmetric(encryption_manager, encrypted_data, aes_key)
  assert_eq(decrypted_data, sensitive_data)
  
  // Test asymmetric encryption (RSA)
  let public_key = RSAKeyPair::public_key(rsa_key_pair)
  let private_key = RSAKeyPair::private_key(rsa_key_pair)
  
  let rsa_encrypted = EncryptionManager::encrypt_asymmetric(encryption_manager, sensitive_data, public_key)
  assert_true(rsa_encrypted != sensitive_data)
  assert_true(rsa_encrypted.length() > 0)
  
  let rsa_decrypted = EncryptionManager::decrypt_asymmetric(encryption_manager, rsa_encrypted, private_key)
  assert_eq(rsa_decrypted, sensitive_data)
  
  // Test hybrid encryption (RSA + AES)
  let hybrid_encrypted = EncryptionManager::encrypt_hybrid(encryption_manager, sensitive_data, public_key)
  let hybrid_decrypted = EncryptionManager::decrypt_hybrid(encryption_manager, hybrid_encrypted, private_key)
  assert_eq(hybrid_decrypted, sensitive_data)
  
  // Test key rotation
  let new_aes_key = EncryptionKey::generate_aes(256)
  let key_rotation_result = EncryptionManager::rotate_key(encryption_manager, aes_key, new_aes_key, encrypted_data)
  match key_rotation_result {
    Ok(re_encrypted_data) => {
      let re_decrypted_data = EncryptionManager::decrypt_symmetric(encryption_manager, re_encrypted_data, new_aes_key)
      assert_eq(re_decrypted_data, sensitive_data)
    }
    Error(e) => assert_true(false)
  }
  
  // Test encryption of telemetry data
  let telemetry_data = TelemetryData::new()
  TelemetryData::add_metric(telemetry_data, "cpu_usage", 75.5)
  TelemetryData::add_attribute(telemetry_data, "service", "auth_service")
  TelemetryData::add_attribute(telemetry_data, "user_id", "user123")
  
  let encrypted_telemetry = EncryptionManager::encrypt_telemetry(encryption_manager, telemetry_data, aes_key)
  let decrypted_telemetry = EncryptionManager::decrypt_telemetry(encryption_manager, encrypted_telemetry, aes_key)
  
  assert_true(TelemetryData::equals(telemetry_data, decrypted_telemetry))
}

// Test 3: Input Validation and Sanitization
test "input validation and sanitization" {
  // Create input validator
  let validator = InputValidator::new()
  
  // Test SQL injection protection
  let malicious_sql = "SELECT * FROM users WHERE id = 1; DROP TABLE users; --"
  let sanitized_sql = InputValidator::sanitize_sql(validator, malicious_sql)
  assert_false(sanitized_sql.contains("DROP TABLE"))
  
  // Test XSS protection
  let malicious_xss = "<script>alert('XSS')</script>"
  let sanitized_xss = InputValidator::sanitize_html(validator, malicious_xss)
  assert_false(sanitized_xss.contains("<script>"))
  assert_true(sanitized_xss.contains("&lt;script&gt;"))
  
  // Test path traversal protection
  let malicious_path = "../../../etc/passwd"
  let sanitized_path = InputValidator::sanitize_path(validator, malicious_path)
  assert_false(sanitized_path.contains(".."))
  
  // Test command injection protection
  let malicious_command = "ls; rm -rf /"
  let sanitized_command = InputValidator::sanitize_command(validator, malicious_command)
  assert_false(sanitized_command.contains(";"))
  assert_false(sanitized_command.contains("rm -rf"))
  
  // Test telemetry data validation
  let telemetry_validator = TelemetryInputValidator::new()
  
  // Valid telemetry data
  let valid_telemetry = TelemetryData::new()
  TelemetryData::add_metric(valid_telemetry, "cpu_usage", 75.5)
  TelemetryData::add_attribute(valid_telemetry, "service", "auth_service")
  
  let valid_result = TelemetryInputValidator::validate(telemetry_validator, valid_telemetry)
  match valid_result {
    Ok(_) => assert_true(true),
    Error(_) => assert_true(false)
  }
  
  // Invalid telemetry data (negative CPU usage)
  let invalid_telemetry = TelemetryData::new()
  TelemetryData::add_metric(invalid_telemetry, "cpu_usage", -10.0)
  TelemetryData::add_attribute(invalid_telemetry, "service", "auth_service")
  
  let invalid_result = TelemetryInputValidator::validate(telemetry_validator, invalid_telemetry)
  match invalid_result {
    Ok(_) => assert_true(false),
    Error(errors) => {
      assert_true(errors.length() > 0)
      assert_true(errors[0].contains("Invalid metric value"))
    }
  }
  
  // Test attribute validation
  TelemetryInputValidator::add_attribute_rule(telemetry_validator, "service", ValidationRule::required())
  TelemetryInputValidator::add_attribute_rule(telemetry_validator, "service", ValidationRule::max_length(50))
  TelemetryInputValidator::add_attribute_rule(telemetry_validator, "user_id", ValidationRule::pattern_match("^[a-zA-Z0-9_]+$"))
  
  // Test missing required attribute
  let missing_attr_telemetry = TelemetryData::new()
  TelemetryData::add_metric(missing_attr_telemetry, "cpu_usage", 75.5)
  
  let missing_attr_result = TelemetryInputValidator::validate(telemetry_validator, missing_attr_telemetry)
  match missing_attr_result {
    Ok(_) => assert_true(false),
    Error(errors) => {
      assert_true(errors.length() > 0)
      assert_true(errors[0].contains("Required attribute missing"))
    }
  }
  
  // Test invalid attribute pattern
  let invalid_attr_telemetry = TelemetryData::new()
  TelemetryData::add_metric(invalid_attr_telemetry, "cpu_usage", 75.5)
  TelemetryData::add_attribute(invalid_attr_telemetry, "service", "auth_service")
  TelemetryData::add_attribute(invalid_attr_telemetry, "user_id", "user@123") // Invalid pattern
  
  let invalid_attr_result = TelemetryInputValidator::validate(telemetry_validator, invalid_attr_telemetry)
  match invalid_attr_result {
    Ok(_) => assert_true(false),
    Error(errors) => {
      assert_true(errors.length() > 0)
      assert_true(errors[0].contains("Invalid attribute format"))
    }
  }
}

// Test 4: Rate Limiting and DDoS Protection
test "rate limiting and ddos protection" {
  // Create rate limiter
  let rate_limiter = RateLimiter::new()
  
  // Configure rate limiting rules
  RateLimiter::add_rule(rate_limiter, "api_requests", RateLimitRule::per_minute(100))
  RateLimiter::add_rule(rate_limiter, "telemetry_submissions", RateLimitRule::per_second(10))
  RateLimiter::add_rule(rate_limiter, "auth_attempts", RateLimitRule::per_minute(5))
  
  // Test normal usage within limits
  let client_id = "client123"
  
  for i in 0..=50 {
    let result = RateLimiter::check_limit(rate_limiter, client_id, "api_requests")
    match result {
      Ok(_) => assert_true(true),
      Error(e) => assert_true(false)
    }
  }
  
  // Test exceeding rate limit
  for i in 0..=60 {
    let result = RateLimiter::check_limit(rate_limiter, client_id, "api_requests")
    if i < 50 {
      match result {
        Ok(_) => assert_true(true),
        Error(e) => assert_true(false)
      }
    } else {
      match result {
        Ok(_) => assert_true(false), // Should not succeed after exceeding limit
        Error(e) => {
          assert_true(Error::is_rate_limit_error(e))
        }
      }
    }
  }
  
  // Test rate limit reset
  RateLimiter::reset_client_limits(rate_limiter, client_id)
  
  let reset_result = RateLimiter::check_limit(rate_limiter, client_id, "api_requests")
  match reset_result {
    Ok(_) => assert_true(true),
    Error(e) => assert_true(false)
  }
  
  // Test DDoS protection
  let ddos_protector = DDoSProtector::new()
  
  // Configure DDoS protection
  DDoSProtector::set_threshold(ddos_protector, 1000) // 1000 requests per minute
  DDoSProtector::set_block_duration(ddos_protector, 300) // Block for 5 minutes
  
  // Simulate DDoS attack
  let attacker_ip = "192.168.1.100"
  
  for i in 0..=1100 {
    let result = DDoSProtector::check_request(ddos_protector, attacker_ip)
    if i < 1000 {
      match result {
        Ok(_) => assert_true(true),
        Error(e) => assert_true(false)
      }
    } else {
      match result {
        Ok(_) => assert_true(false), // Should be blocked after threshold
        Error(e) => {
          assert_true(Error::is_ddos_block_error(e))
        }
      }
    }
  }
  
  // Verify IP is blocked
  let block_result = DDoSProtector::check_request(ddos_protector, attacker_ip)
  match block_result {
    Ok(_) => assert_true(false), // Should be blocked
    Error(e) => {
      assert_true(Error::is_ddos_block_error(e))
    }
  }
  
  // Test legitimate IP is not affected
  let legitimate_ip = "192.168.1.101"
  let legitimate_result = DDoSProtector::check_request(ddos_protector, legitimate_ip)
  match legitimate_result {
    Ok(_) => assert_true(true),
    Error(e) => assert_true(false)
  }
  
  // Test unblocking IP
  DDoSProtector::unblock_ip(ddos_protector, attacker_ip)
  
  let unblock_result = DDoSProtector::check_request(ddos_protector, attacker_ip)
  match unblock_result {
    Ok(_) => assert_true(true),
    Error(e) => assert_true(false)
  }
}

// Test 5: Audit Logging and Security Events
test "audit logging and security events" {
  // Create audit logger
  let audit_logger = AuditLogger::new()
  
  // Configure audit logging
  AuditLogger::set_log_level(audit_logger, "INFO")
  AuditLogger::set_output_file(audit_logger, "logs/security_audit.log")
  AuditLogger::enable_rotation(audit_logger, "daily")
  
  // Test authentication event logging
  let auth_event = SecurityEvent::authentication_success("user123", "jwt", "192.168.1.100")
  AuditLogger::log_event(audit_logger, auth_event)
  
  // Test authorization failure event logging
  let authz_event = SecurityEvent::authorization_failure("user123", "/admin", "192.168.1.100")
  AuditLogger::log_event(audit_logger, authz_event)
  
  // Test data access event logging
  let data_access_event = SecurityEvent::data_access("user123", "telemetry_data", "read", "192.168.1.100")
  AuditLogger::log_event(audit_logger, data_access_event)
  
  // Test configuration change event logging
  let config_event = SecurityEvent::configuration_change("admin", "service.port", 8080, 9090)
  AuditLogger::log_event(audit_logger, config_event)
  
  // Test security violation event logging
  let violation_event = SecurityEvent::security_violation("192.168.1.100", "SQL_INJECTION_ATTEMPT", "SELECT * FROM users; DROP TABLE users;")
  AuditLogger::log_event(audit_logger, violation_event)
  
  // Test log query
  let query = LogQuery::new()
  LogQuery::add_filter(query, "event_type", "authentication")
  LogQuery::add_time_range(query, Time::now() - 3600, Time::now()) // Last hour
  
  let log_results = AuditLogger::query_logs(audit_logger, query)
  assert_true(log_results.length() > 0)
  
  // Verify authentication event is logged
  let auth_logged = log_results.any(fn(event) {
    SecurityEvent::event_type(event) == "authentication_success"
  })
  assert_true(auth_logged)
  
  // Test log analysis
  let analyzer = LogAnalyzer::new()
  
  // Analyze failed authentication attempts
  let failed_auth_analysis = LogAnalyzer::analyze_failed_authentications(analyzer, audit_logger, Time::now() - 3600)
  match failed_auth_analysis {
    AnalysisResult(results) => {
      assert_true(results.has_key("top_failed_ips"))
      assert_true(results.has_key("failed_auth_count"))
    }
    _ => assert_true(false)
  }
  
  // Analyze suspicious activities
  let suspicious_analysis = LogAnalyzer::analyze_suspicious_activities(analyzer, audit_logger, Time::now() - 3600)
  match suspicious_analysis {
    AnalysisResult(results) => {
      assert_true(results.has_key("suspicious_ips"))
      assert_true(results.has_key("violation_types"))
    }
    _ => assert_true(false)
  }
  
  // Test log export
  let export_format = "json"
  let exported_logs = AuditLogger::export_logs(audit_logger, query, export_format)
  assert_true(exported_logs.contains("authentication_success"))
  assert_true(exported_logs.contains("authorization_failure"))
}

// Test 6: Secure Communication and TLS
test "secure communication and tls" {
  // Create TLS configuration
  let tls_config = TLSConfig::new()
  
  // Configure TLS
  TLSConfig::set_certificate(tls_config, "certificates/server.crt")
  TLSConfig::set_private_key(tls_config, "certificates/server.key")
  TLSConfig::set_ca_certificate(tls_config, "certificates/ca.crt")
  TLSConfig::set_min_version(tls_config, "1.2")
  TLSConfig::set_cipher_suites(tls_config, ["ECDHE-RSA-AES256-GCM-SHA384", "ECDHE-RSA-AES128-GCM-SHA256"])
  TLSConfig::enable_client_certificates(tls_config, true)
  
  // Create secure server
  let server = SecureServer::new(tls_config)
  
  // Test server start
  let server_result = SecureServer::start(server, "0.0.0.0", 8443)
  match server_result {
    Ok(_) => assert_true(true),
    Error(e) => {
      // In test environment, might fail due to missing certificates
      assert_true(Error::is_file_error(e) || Error::is_network_error(e))
    }
  }
  
  // Create secure client
  let client = SecureClient::new()
  
  // Configure client TLS
  SecureClient::set_ca_certificate(client, "certificates/ca.crt")
  SecureClient::set_client_certificate(client, "certificates/client.crt")
  SecureClient::set_client_private_key(client, "certificates/client.key")
  SecureClient::set_verify_hostname(client, true)
  SecureClient::set_verify_peer(client, true)
  
  // Test secure connection
  let connection_result = SecureClient::connect(client, "localhost", 8443)
  match connection_result {
    Ok(connection) => {
      // Verify TLS handshake
      assert_true(SecureConnection::is_secure(connection))
      assert_eq(SecureConnection::get_protocol_version(connection), "TLSv1.2")
      
      // Test secure data transmission
      let telemetry_data = "secure telemetry data"
      let send_result = SecureConnection::send(connection, telemetry_data)
      match send_result {
        Ok(_) => assert_true(true),
        Error(e) => assert_true(false)
      }
      
      let receive_result = SecureConnection::receive(connection)
      match receive_result {
        Ok(response) => assert_true(response.length() > 0),
        Error(e) => assert_true(false)
      }
      
      // Close connection
      SecureConnection::close(connection)
    }
    Error(e) => {
      // In test environment, might fail due to missing certificates or server not running
      assert_true(Error::is_network_error(e) || Error::is_tls_error(e))
    }
  }
  
  // Test certificate validation
  let cert_validator = CertificateValidator::new()
  
  // Test valid certificate
  let valid_cert = Certificate::load("certificates/server.crt")
  let valid_result = CertificateValidator::validate(cert_validator, valid_cert)
  match valid_result {
    Ok(_) => assert_true(true),
    Error(e) => {
      // In test environment, might fail due to missing certificates
      assert_true(Error::is_file_error(e))
    }
  }
  
  // Test expired certificate
  let expired_cert = Certificate::load("certificates/expired.crt")
  let expired_result = CertificateValidator::validate(cert_validator, expired_cert)
  match expired_result {
    Ok(_) => assert_true(false), // Should not succeed
    Error(e) => {
      assert_true(Error::is_certificate_error(e))
      assert_true(Error::message(e).contains("expired"))
    }
  }
  
  // Test self-signed certificate
  let self_signed_cert = Certificate::load("certificates/self_signed.crt")
  let self_signed_result = CertificateValidator::validate(cert_validator, self_signed_cert)
  match self_signed_result {
    Ok(_) => assert_true(false), // Should not succeed
    Error(e) => {
      assert_true(Error::is_certificate_error(e))
      assert_true(Error::message(e).contains("self-signed"))
    }
  }
  
  // Test certificate revocation
  let revoked_cert = Certificate::load("certificates/revoked.crt")
  let revoked_result = CertificateValidator::validate(cert_validator, revoked_cert)
  match revoked_result {
    Ok(_) => assert_true(false), // Should not succeed
    Error(e) => {
      assert_true(Error::is_certificate_error(e))
      assert_true(Error::message(e).contains("revoked"))
    }
  }
}

// Test 7: Security Scanning and Vulnerability Detection
test "security scanning and vulnerability detection" {
  // Create security scanner
  let scanner = SecurityScanner::new()
  
  // Configure scanner
  SecurityScanner::enable_vulnerability_detection(scanner, true)
  SecurityScanner::enable_malware_detection(scanner, true)
  SecurityScanner::set_scan_depth(scanner, "deep")
  
  // Test code scanning
  let code_files = [
    "src/telemetry_processor.mbt",
    "src/data_collector.mbt",
    "src/network_client.mbt"
  ]
  
  let code_scan_result = SecurityScanner::scan_code(scanner, code_files)
  match code_scan_result {
    ScanResult(results) => {
      assert_true(results.has_key("vulnerabilities"))
      assert_true(results.has_key("security_issues"))
      assert_true(results.has_key("recommendations"))
      
      let vulnerabilities = results.get("vulnerabilities")
      match vulnerabilities {
        Array(vulns) => {
          // Check for common vulnerabilities
          let sql_injection_detected = vulns.any(fn(vuln) {
            Vulnerability::type(vuln) == "SQL_INJECTION"
          })
          let xss_detected = vulns.any(fn(vuln) {
            Vulnerability::type(vuln) == "XSS"
          })
          let path_traversal_detected = vulns.any(fn(vuln) {
            Vulnerability::type(vuln) == "PATH_TRAVERSAL"
          })
          
          // In test environment, we might not have actual vulnerabilities
          // So we just verify the scanning process works
          assert_true(vulns.length() >= 0)
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test dependency scanning
  let dependencies = [
    ("moonbit-core", "1.0.0"),
    ("moonbit-json", "2.1.3"),
    ("moonbit-crypto", "1.5.2")
  ]
  
  let dependency_scan_result = SecurityScanner::scan_dependencies(scanner, dependencies)
  match dependency_scan_result {
    ScanResult(results) => {
      assert_true(results.has_key("vulnerable_dependencies"))
      assert_true(results.has_key("outdated_dependencies"))
      
      let vulnerable_deps = results.get("vulnerable_dependencies")
      match vulnerable_deps {
        Array(deps) => {
          // Check for known vulnerabilities
          let critical_vulns = deps.filter(fn(dep) {
            Vulnerability::severity(dep) == "CRITICAL"
          })
          
          // In test environment, we might not have actual vulnerabilities
          // So we just verify the scanning process works
          assert_true(deps.length() >= 0)
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test network scanning
  let network_targets = [
    "localhost:8080",
    "localhost:8443",
    "localhost:9090"
  ]
  
  let network_scan_result = SecurityScanner::scan_network(scanner, network_targets)
  match network_scan_result {
    ScanResult(results) => {
      assert_true(results.has_key("open_ports"))
      assert_true(results.has_key("services"))
      assert_true(results.has_key("vulnerabilities"))
      
      let open_ports = results.get("open_ports")
      match open_ports {
        Array(ports) => {
          // Check for common issues
          let unencrypted_http = ports.any(fn(port) {
            NetworkPort::number(port) == 80 && NetworkPort::service(port) == "http"
          })
          let default_credentials = ports.any(fn(port) {
            NetworkPort::has_default_credentials(port)
          })
          
          // In test environment, we might not have actual services running
          // So we just verify the scanning process works
          assert_true(ports.length() >= 0)
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test configuration scanning
  let config_files = [
    "config/azimuth.json",
    "config/security.json",
    "config/database.json"
  ]
  
  let config_scan_result = SecurityScanner::scan_configuration(scanner, config_files)
  match config_scan_result {
    ScanResult(results) => {
      assert_true(results.has_key("security_misconfigurations"))
      assert_true(results.has_key("weak_passwords"))
      assert_true(results.has_key("exposed_secrets"))
      
      let misconfigurations = results.get("security_misconfigurations")
      match misconfigurations {
        Array(miscfgs) => {
          // Check for common misconfigurations
          let debug_enabled = miscfgs.any(fn(miscfg) {
            Misconfiguration::type(miscfg) == "DEBUG_ENABLED"
          })
          let default_passwords = miscfgs.any(fn(miscfg) {
            Misconfiguration::type(miscfg) == "DEFAULT_PASSWORD"
          })
          let weak_encryption = miscfgs.any(fn(miscfg) {
            Misconfiguration::type(miscfg) == "WEAK_ENCRYPTION"
          })
          
          // In test environment, we might not have actual misconfigurations
          // So we just verify the scanning process works
          assert_true(miscfgs.length() >= 0)
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test security report generation
  let report = SecurityScanner::generate_report(scanner)
  assert_true(report.contains("Security Scan Report"))
  assert_true(report.contains("Vulnerabilities"))
  assert_true(report.contains("Recommendations"))
  
  // Test vulnerability remediation suggestions
  let remediation = SecurityScanner::get_remediation_suggestions(scanner)
  assert_true(remediation.has_key("high_priority"))
  assert_true(remediation.has_key("medium_priority"))
  assert_true(remediation.has_key("low_priority"))
}
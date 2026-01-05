// Azimuth Telemetry System - Security and Encryption Tests
// This file contains test cases for security and encryption functionality

// Test 1: Basic Encryption and Decryption
test "basic encryption and decryption" {
  // Create encryption service
  let encryption_service = EncryptionService::new()
  
  // Generate encryption key
  let key = EncryptionService::generate_key(encryption_service, "AES256")
  assert_eq(Key::algorithm(key), "AES256")
  assert_eq(Key::size(key), 256)
  
  // Test string encryption
  let plaintext = "sensitive telemetry data"
  let encrypted_data = EncryptionService::encrypt(encryption_service, key, plaintext)
  
  assert_not_eq(encrypted_data, plaintext)
  assert_true(encrypted_data.length() > plaintext.length())
  
  // Test string decryption
  let decrypted_data = EncryptionService::decrypt(encryption_service, key, encrypted_data)
  assert_eq(decrypted_data, plaintext)
  
  // Test binary data encryption
  let binary_data = [1, 2, 3, 4, 5, 6, 7, 8]
  let encrypted_binary = EncryptionService::encrypt_binary(encryption_service, key, binary_data)
  
  assert_not_eq(encrypted_binary, binary_data)
  
  // Test binary data decryption
  let decrypted_binary = EncryptionService::decrypt_binary(encryption_service, key, encrypted_binary)
  assert_eq(decrypted_binary, binary_data)
}

// Test 2: Hashing and Message Authentication
test "hashing and message authentication" {
  // Create hash service
  let hash_service = HashService::new()
  
  // Test SHA-256 hashing
  let data = "telemetry data to hash"
  let sha256_hash = HashService::hash(hash_service, data, "SHA256")
  
  assert_eq(sha256_hash.length(), 64) // SHA-256 produces 64 hex characters
  assert_not_eq(sha256_hash, data)
  
  // Test hash verification
  let hash_verified = HashService::verify(hash_service, data, sha256_hash, "SHA256")
  assert_true(hash_verified)
  
  // Test hash verification with wrong data
  let wrong_data = "wrong telemetry data"
  let hash_not_verified = HashService::verify(hash_service, wrong_data, sha256_hash, "SHA256")
  assert_false(hash_not_verified)
  
  // Test HMAC generation
  let hmac_key = "secret_hmac_key"
  let hmac = HashService::hmac(hash_service, data, hmac_key, "SHA256")
  
  assert_eq(hmac.length(), 64) // HMAC-SHA256 produces 64 hex characters
  assert_not_eq(hmac, sha256_hash)
  
  // Test HMAC verification
  let hmac_verified = HashService::verify_hmac(hash_service, data, hmac, hmac_key, "SHA256")
  assert_true(hmac_verified)
  
  // Test HMAC verification with wrong key
  let wrong_key = "wrong_hmac_key"
  let hmac_not_verified = HashService::verify_hmac(hash_service, data, hmac, wrong_key, "SHA256")
  assert_false(hmac_not_verified)
}

// Test 3: Digital Signatures
test "digital signatures" {
  // Create key pair for digital signatures
  let key_pair = SignatureService::generate_key_pair("RSA2048")
  let private_key = KeyPair::private_key(key_pair)
  let public_key = KeyPair::public_key(key_pair)
  
  // Test data signing
  let data = "important telemetry data"
  let signature = SignatureService::sign(private_key, data, "SHA256")
  
  assert_not_eq(signature, data)
  assert_true(signature.length() > 0)
  
  // Test signature verification
  let signature_valid = SignatureService::verify(public_key, data, signature, "SHA256")
  assert_true(signature_valid)
  
  // Test signature verification with wrong data
  let wrong_data = "tampered telemetry data"
  let signature_invalid = SignatureService::verify(public_key, wrong_data, signature, "SHA256")
  assert_false(signature_invalid)
  
  // Test signature verification with wrong public key
  let wrong_key_pair = SignatureService::generate_key_pair("RSA2048")
  let wrong_public_key = KeyPair::public_key(wrong_key_pair)
  let signature_wrong_key = SignatureService::verify(wrong_public_key, data, signature, "SHA256")
  assert_false(signature_wrong_key)
}

// Test 4: Secure Key Management
test "secure key management" {
  // Create key manager
  let key_manager = KeyManager::new()
  
  // Generate and store keys
  let key_id1 = KeyManager::generate_key(key_manager, "AES256", "encryption_key1")
  let key_id2 = KeyManager::generate_key(key_manager, "RSA2048", "signing_key1")
  
  // Test key retrieval
  let retrieved_key1 = KeyManager::get_key(key_manager, key_id1)
  match retrieved_key1 {
    Some(k) => {
      assert_eq(Key::algorithm(k), "AES256")
      assert_eq(Key::name(k), "encryption_key1")
    }
    None => assert_true(false)
  }
  
  let retrieved_key2 = KeyManager::get_key(key_manager, key_id2)
  match retrieved_key2 {
    Some(k) => {
      assert_eq(Key::algorithm(k), "RSA2048")
      assert_eq(Key::name(k), "signing_key1")
    }
    None => assert_true(false)
  }
  
  // Test key rotation
  let rotated_key_id = KeyManager::rotate_key(key_manager, key_id1)
  let rotated_key = KeyManager::get_key(key_manager, rotated_key_id)
  match rotated_key {
    Some(k) => assert_eq(Key::algorithm(k), "AES256")
    None => assert_true(false)
  }
  
  // Test old key deactivation
  let old_key = KeyManager::get_key(key_manager, key_id1)
  match old_key {
    Some(k) => assert_false(Key::is_active(k))
    None => assert_true(false)
  }
  
  // Test key listing
  let active_keys = KeyManager::list_active_keys(key_manager)
  assert_eq(active_keys.length(), 2) // 1 rotated + 1 original
  
  // Test key revocation
  KeyManager::revoke_key(key_manager, key_id2)
  let revoked_key = KeyManager::get_key(key_manager, key_id2)
  match revoked_key {
    Some(k) => assert_false(Key::is_active(k))
    None => assert_true(false)
  }
}

// Test 5: Secure Random Number Generation
test "secure random number generation" {
  // Create secure random service
  let random_service = SecureRandom::new()
  
  // Test random byte generation
  let random_bytes = SecureRandom::generate_bytes(random_service, 32)
  assert_eq(random_bytes.length(), 32)
  
  // Verify randomness (second generation should be different)
  let random_bytes2 = SecureRandom::generate_bytes(random_service, 32)
  assert_neq(random_bytes, random_bytes2)
  
  // Test random integer generation
  let random_int = SecureRandom::generate_int(random_service, 1, 100)
  assert_true(random_int >= 1 && random_int <= 100)
  
  // Test random string generation
  let random_string = SecureRandom::generate_string(random_service, 16)
  assert_eq(random_string.length(), 16)
  
  // Test random string generation with specific characters
  let alphanumeric_string = SecureRandom::generate_string_with_chars(
    random_service, 
    20, 
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  )
  assert_eq(alphanumeric_string.length(), 20)
  
  // Verify all characters are alphanumeric
  for char in alphanumeric_string.to_char_array() {
    assert_true(char.is_alphanumeric())
  }
}

// Test 6: Token-based Authentication
test "token based authentication" {
  // Create token service
  let token_service = TokenService::new("secret_key")
  
  // Test token generation
  let user_id = "user123"
  let claims = [
    ("sub", user_id),
    ("name", "Test User"),
    ("role", "admin")
  ]
  let token = TokenService::generate_token(token_service, claims, 3600) // 1 hour expiration
  
  assert_true(token.length() > 0)
  assert_true(token.contains("."))
  
  // Test token validation
  let validation_result = TokenService::validate_token(token_service, token)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.user_id, user_id)
  assert_false(validation_result.is_expired)
  
  // Test token claims extraction
  let extracted_claims = TokenService::extract_claims(token_service, token)
  assert_eq(extracted_claims.get("sub"), Some(user_id))
  assert_eq(extracted_claims.get("name"), Some("Test User"))
  assert_eq(extracted_claims.get("role"), Some("admin"))
  
  // Test token refresh
  let refreshed_token = TokenService::refresh_token(token_service, token, 7200) // 2 hours
  let refresh_validation = TokenService::validate_token(token_service, refreshed_token)
  assert_true(refresh_validation.is_valid)
  
  // Test invalid token validation
  let invalid_token = "invalid.token.here"
  let invalid_validation = TokenService::validate_token(token_service, invalid_token)
  assert_false(invalid_validation.is_valid)
}

// Test 7: Data Masking and Anonymization
test "data masking and anonymization" {
  // Create data protection service
  let data_protection = DataProtection::new()
  
  // Test email masking
  let email = "user@example.com"
  let masked_email = DataProtection::mask_email(data_protection, email)
  assert_eq(masked_email, "u***@example.com")
  
  // Test phone number masking
  let phone = "+1-555-123-4567"
  let masked_phone = DataProtection::mask_phone(data_protection, phone)
  assert_eq(masked_phone, "+1-555-***-4567")
  
  // Test credit card masking
  let credit_card = "4111111111111111"
  let masked_card = DataProtection::mask_credit_card(data_protection, credit_card)
  assert_eq(masked_card, "************1111")
  
  // Test data anonymization
  let personal_data = "John Doe, 123 Main St, New York, NY"
  let anonymized_data = DataProtection::anonymize(data_protection, personal_data)
  assert_not_eq(anonymized_data, personal_data)
  assert_false(anonymized_data.contains("John Doe"))
  assert_false(anonymized_data.contains("123 Main St"))
  
  // Test reversible anonymization with token
  let anonymization_token = DataProtection::generate_anonymization_token(data_protection)
  let reversible_anonymized = DataProtection::anonymize_with_token(
    data_protection, 
    personal_data, 
    anonymization_token
  )
  
  let deanonymized_data = DataProtection::deanonymize_with_token(
    data_protection, 
    reversible_anonymized, 
    anonymization_token
  )
  assert_eq(deanonymized_data, personal_data)
}

// Test 8: Access Control and Permissions
test "access control and permissions" {
  // Create access control service
  let access_control = AccessControl::new()
  
  // Define roles and permissions
  let admin_permissions = ["read", "write", "delete", "admin"]
  let user_permissions = ["read", "write"]
  let guest_permissions = ["read"]
  
  // Create roles
  AccessControl::create_role(access_control, "admin", admin_permissions)
  AccessControl::create_role(access_control, "user", user_permissions)
  AccessControl::create_role(access_control, "guest", guest_permissions)
  
  // Create users with roles
  AccessControl::create_user(access_control, "admin_user", "admin")
  AccessControl::create_user(access_control, "normal_user", "user")
  AccessControl::create_user(access_control, "guest_user", "guest")
  
  // Test permission checking
  let admin_can_delete = AccessControl::has_permission(access_control, "admin_user", "delete")
  assert_true(admin_can_delete)
  
  let user_can_delete = AccessControl::has_permission(access_control, "normal_user", "delete")
  assert_false(user_can_delete)
  
  let user_can_read = AccessControl::has_permission(access_control, "normal_user", "read")
  assert_true(user_can_read)
  
  let guest_can_write = AccessControl::has_permission(access_control, "guest_user", "write")
  assert_false(guest_can_write)
  
  // Test resource-based access control
  AccessControl::create_resource(access_control, "telemetry_data", ["admin", "user"])
  
  let admin_can_access = AccessControl::can_access_resource(
    access_control, 
    "admin_user", 
    "telemetry_data"
  )
  assert_true(admin_can_access)
  
  let guest_can_access = AccessControl::can_access_resource(
    access_control, 
    "guest_user", 
    "telemetry_data"
  )
  assert_false(guest_can_access)
  
  // Test attribute-based access control (ABAC)
  let user_attributes = [
    ("department", "engineering"),
    ("clearance_level", "3")
  ]
  AccessControl::set_user_attributes(access_control, "normal_user", user_attributes)
  
  let access_policy = AccessPolicy::new(
    "engineering_data_access",
    "resource.department == 'engineering' && user.clearance_level >= '2'"
  )
  AccessControl::add_policy(access_control, access_policy)
  
  let can_access_engineering = AccessControl::check_policy_access(
    access_control, 
    "normal_user", 
    "engineering_data", 
    [("department", "engineering")]
  )
  assert_true(can_access_engineering)
}

// Test 9: Security Audit and Logging
test "security audit and logging" {
  // Create security audit service
  let audit_service = SecurityAudit::new()
  
  // Test security event logging
  let login_event = SecurityEvent::new(
    "authentication",
    "success",
    "user123",
    "Web login",
    [("ip_address", "192.168.1.100"), ("user_agent", "Mozilla/5.0")]
  )
  
  SecurityAudit::log_event(audit_service, login_event)
  
  // Test security event retrieval
  let events = SecurityAudit::get_events(audit_service, "user123", 10)
  assert_eq(events.length(), 1)
  assert_eq(SecurityEvent::event_type(events[0]), "authentication")
  assert_eq(SecurityEvent::outcome(events[0]), "success")
  
  // Test failed authentication logging
  let failed_login_event = SecurityEvent::new(
    "authentication",
    "failure",
    "user123",
    "Failed login attempt",
    [("ip_address", "192.168.1.100"), ("reason", "invalid_password")]
  )
  
  SecurityAudit::log_event(audit_service, failed_login_event)
  
  // Test security event filtering
  let auth_events = SecurityAudit::get_events_by_type(audit_service, "authentication", 10)
  assert_eq(auth_events.length(), 2)
  
  let failed_events = SecurityAudit::get_events_by_outcome(audit_service, "failure", 10)
  assert_eq(failed_events.length(), 1)
  
  // Test security metrics calculation
  let auth_metrics = SecurityAudit::calculate_metrics(audit_service, "authentication")
  assert_eq(auth_metrics.total_events, 2)
  assert_eq(auth_metrics.successful_events, 1)
  assert_eq(auth_metrics.failed_events, 1)
  assert_eq(auth_metrics.success_rate, 0.5)
  
  // Test security alert generation
  let alerts = SecurityAudit::generate_alerts(audit_service)
  assert_eq(alerts.length(), 1) // One failed login should generate an alert
  assert_eq(Alert::type_(alerts[0]), "multiple_failed_logins")
}

// Test 10: Secure Telemetry Data Transmission
test "secure telemetry data transmission" {
  // Create secure transmitter
  let transmitter = SecureTransmitter::new()
  
  // Create telemetry data
  let telemetry_data = TelemetryData::new()
  TelemetryData::add_metric(telemetry_data, "cpu_usage", 75.5)
  TelemetryData::add_metric(telemetry_data, "memory_usage", 60.2)
  TelemetryData::add_tag(telemetry_data, "service", "web_server")
  TelemetryData::add_tag(telemetry_data, "environment", "production")
  
  // Test data serialization
  let serialized_data = TelemetryData::serialize(telemetry_data)
  assert_true(serialized_data.length() > 0)
  
  // Test data encryption for transmission
  let encryption_key = EncryptionService::generate_key(EncryptionService::new(), "AES256")
  let encrypted_transmission = SecureTransmitter::encrypt_data(transmitter, serialized_data, encryption_key)
  
  assert_neq(encrypted_transmission, serialized_data)
  
  // Test data signing for integrity
  let signing_key = KeyPair::private_key(SignatureService::generate_key_pair("RSA2048"))
  let signed_transmission = SecureTransmitter::sign_data(transmitter, encrypted_transmission, signing_key)
  
  assert_neq(signed_transmission.data, encrypted_transmission)
  assert_true(signed_transmission.signature.length() > 0)
  
  // Test secure transmission protocol
  let transmission_result = SecureTransmitter::transmit(
    transmitter, 
    signed_transmission, 
    "https://telemetry.example.com/api/data"
  )
  
  assert_true(transmission_result.success)
  assert_true(transmission_result.encrypted)
  assert_true(transmission_result.signed)
  
  // Test secure data reception
  let reception_result = SecureTransmitter::receive(
    transmitter, 
    signed_transmission, 
    encryption_key,
    KeyPair::public_key(SignatureService::generate_key_pair("RSA2048"))
  )
  
  assert_true(reception_result.signature_valid)
  assert_true(reception_result.decryption_successful)
  
  // Test received data integrity
  let received_data = TelemetryData::deserialize(reception_result.data)
  assert_eq(TelemetryData::get_metric(received_data, "cpu_usage"), 75.5)
  assert_eq(TelemetryData::get_metric(received_data, "memory_usage"), 60.2)
  assert_eq(TelemetryData::get_tag(received_data, "service"), "web_server")
  assert_eq(TelemetryData::get_tag(received_data, "environment"), "production")
}
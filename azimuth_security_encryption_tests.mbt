// Azimuth Telemetry System - Security and Encryption Tests
// This file contains test cases for security and encryption functionality

// Test 1: Data Encryption and Decryption
test "data encryption and decryption" {
  // Create an encryption key
  let key = EncryptionKey::generate(AES256)
  assert_eq(EncryptionKey::algorithm(key), AES256)
  assert_eq(EncryptionKey::size(key), 32) // 256 bits = 32 bytes
  
  // Test string encryption
  let plaintext = "This is sensitive telemetry data that must be encrypted"
  let encrypted_data = Encryption::encrypt(key, plaintext.to_bytes())
  
  // Verify encrypted data is different from plaintext
  assert_true(encrypted_data.length() > 0)
  assert_neq(encrypted_data.to_string(), plaintext)
  
  // Test decryption
  let decrypted_data = Encryption::decrypt(key, encrypted_data)
  let decrypted_text = decrypted_data.to_string()
  assert_eq(decrypted_text, plaintext)
  
  // Test binary data encryption
  let binary_data = [0x00, 0x01, 0x02, 0x03, 0xFF, 0xFE, 0xFD, 0xFC] * 16
  let encrypted_binary = Encryption::encrypt(key, binary_data)
  let decrypted_binary = Encryption::decrypt(key, encrypted_binary)
  assert_eq(decrypted_binary, binary_data)
  
  // Test with different key sizes
  let aes128_key = EncryptionKey::generate(AES128)
  let aes192_key = EncryptionKey::generate(AES192)
  
  let encrypted_128 = Encryption::encrypt(aes128_key, plaintext.to_bytes())
  let encrypted_192 = Encryption::encrypt(aes192_key, plaintext.to_bytes())
  
  let decrypted_128 = Encryption::decrypt(aes128_key, encrypted_128)
  let decrypted_192 = Encryption::decrypt(aes192_key, encrypted_192)
  
  assert_eq(decrypted_128.to_string(), plaintext)
  assert_eq(decrypted_192.to_string(), plaintext)
}

// Test 2: Asymmetric Encryption
test "asymmetric encryption" {
  // Generate RSA key pair
  let key_pair = RSAKeyPair::generate(2048)
  let public_key = RSAKeyPair::public_key(key_pair)
  let private_key = RSAKeyPair::private_key(key_pair)
  
  // Test encryption with public key and decryption with private key
  let plaintext = "This is data encrypted with a public key"
  let encrypted_data = RSAEncryption::encrypt(public_key, plaintext.to_bytes())
  let decrypted_data = RSAEncryption::decrypt(private_key, encrypted_data)
  assert_eq(decrypted_data.to_string(), plaintext)
  
  // Test signing with private key and verification with public key
  let message = "This is a message to be signed"
  let signature = RSASignature::sign(private_key, message.to_bytes())
  let is_valid = RSASignature::verify(public_key, message.to_bytes(), signature)
  assert_true(is_valid)
  
  // Test with tampered message
  let tampered_message = "This is a tampered message"
  let is_tampered_valid = RSASignature::verify(public_key, tampered_message.to_bytes(), signature)
  assert_false(is_tampered_valid)
  
  // Test with different key sizes
  let key_pair_4096 = RSAKeyPair::generate(4096)
  let public_key_4096 = RSAKeyPair::public_key(key_pair_4096)
  let private_key_4096 = RSAKeyPair::private_key(key_pair_4096)
  
  let encrypted_4096 = RSAEncryption::encrypt(public_key_4096, plaintext.to_bytes())
  let decrypted_4096 = RSAEncryption::decrypt(private_key_4096, encrypted_4096)
  assert_eq(decrypted_4096.to_string(), plaintext)
}

// Test 3: Key Management
test "key management" {
  // Create a key manager
  let key_manager = KeyManager::new()
  
  // Generate and store a symmetric key
  let key_id = KeyManager::generate_key(key_manager, AES256, "telemetry_encryption")
  assert_true(key_id.length() > 0)
  
  // Retrieve the key
  let retrieved_key = KeyManager::get_key(key_manager, key_id)
  assert_true(retrieved_key.is_some())
  
  match retrieved_key {
    Some(key) => {
      assert_eq(EncryptionKey::algorithm(key), AES256)
      assert_eq(EncryptionKey::size(key), 32)
    }
    None => assert_true(false)
  }
  
  // Test key rotation
  let new_key_id = KeyManager::rotate_key(key_manager, key_id)
  assert_neq(new_key_id, key_id)
  
  // Old key should still be available for decryption but marked as deprecated
  let old_key = KeyManager::get_key(key_manager, key_id)
  assert_true(old_key.is_some())
  
  match old_key {
    Some(key) => assert_true(EncryptionKey::is_deprecated(key))
    None => assert_true(false)
  }
  
  // New key should be active
  let new_key = KeyManager::get_key(key_manager, new_key_id)
  assert_true(new_key.is_some())
  
  match new_key {
    Some(key) => assert_false(EncryptionKey::is_deprecated(key))
    None => assert_true(false)
  }
  
  // Test key expiration
  let expiring_key_id = KeyManager::generate_key_with_ttl(key_manager, AES128, "temp_key", 3600) // 1 hour TTL
  let expiring_key = KeyManager::get_key(key_manager, expiring_key_id)
  
  match expiring_key {
    Some(key) => {
      assert_false(EncryptionKey::is_expired(key))
      // Simulate expiration for testing
      EncryptionKey::set_expiration(key, 0L) // Expired now
      assert_true(EncryptionKey::is_expired(key))
    }
    None => assert_true(false)
  }
  
  // Test key deletion
  let delete_result = KeyManager::delete_key(key_manager, key_id)
  assert_true(delete_result.is_ok())
  
  let deleted_key = KeyManager::get_key(key_manager, key_id)
  assert_true(deleted_key.is_none())
}

// Test 4: Secure Hash Functions
test "secure hash functions" {
  // Test SHA-256
  let data = "This is data to be hashed"
  let sha256_hash = Hash::sha256(data.to_bytes())
  assert_eq(sha256_hash.length(), 32) // 256 bits = 32 bytes
  
  // Verify hash consistency
  let sha256_hash_again = Hash::sha256(data.to_bytes())
  assert_eq(sha256_hash, sha256_hash_again)
  
  // Test SHA-512
  let sha512_hash = Hash::sha512(data.to_bytes())
  assert_eq(sha512_hash.length(), 64) // 512 bits = 64 bytes
  
  // Test SHA-1 (for legacy compatibility)
  let sha1_hash = Hash::sha1(data.to_bytes())
  assert_eq(sha1_hash.length(), 20) // 160 bits = 20 bytes
  
  // Test MD5 (for legacy compatibility, not recommended for security)
  let md5_hash = Hash::md5(data.to_bytes())
  assert_eq(md5_hash.length(), 16) // 128 bits = 16 bytes
  
  // Test HMAC
  let hmac_key = "hmac_secret_key"
  let hmac_sha256 = HMAC::sha256(hmac_key.to_bytes(), data.to_bytes())
  assert_eq(hmac_sha256.length(), 32)
  
  // Verify HMAC consistency
  let hmac_sha256_again = HMAC::sha256(hmac_key.to_bytes(), data.to_bytes())
  assert_eq(hmac_sha256, hmac_sha256_again)
  
  // Test with different HMAC algorithms
  let hmac_sha1 = HMAC::sha1(hmac_key.to_bytes(), data.to_bytes())
  assert_eq(hmac_sha1.length(), 20)
  
  let hmac_sha512 = HMAC::sha512(hmac_key.to_bytes(), data.to_bytes())
  assert_eq(hmac_sha512.length(), 64)
}

// Test 5: Telemetry Data Security
test "telemetry data security" {
  // Create a secure telemetry processor
  let key = EncryptionKey::generate(AES256)
  let secure_processor = SecureTelemetryProcessor::new(key)
  
  // Create telemetry data with sensitive information
  let telemetry_data = TelemetryData::new()
  
  // Add metrics
  TelemetryData::add_metric(telemetry_data, "cpu.usage", 75.5)
  TelemetryData::add_metric(telemetry_data, "memory.usage", 60.2)
  
  // Add events with potentially sensitive data
  let sensitive_attrs = Attributes::new()
  Attributes::set(sensitive_attrs, "user_id", StringValue("user-12345"))
  Attributes::set(sensitive_attrs, "ip_address", StringValue("192.168.1.100"))
  Attributes::set(sensitive_attrs, "session_token", StringValue("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"))
  
  TelemetryData::add_event(telemetry_data, "user.login", sensitive_attrs)
  
  // Encrypt sensitive fields
  let secure_config = SecureTelemetryConfig::new()
  SecureTelemetryConfig::add_encrypted_field(secure_config, "user_id")
  SecureTelemetryConfig::add_encrypted_field(secure_config, "ip_address")
  SecureTelemetryConfig::add_encrypted_field(secure_config, "session_token")
  
  let encrypted_data = SecureTelemetryProcessor::encrypt_fields(secure_processor, telemetry_data, secure_config)
  
  // Verify sensitive fields are encrypted
  let encrypted_events = TelemetryData::events(encrypted_data)
  assert_eq(encrypted_events.length(), 1)
  
  let encrypted_event = encrypted_events[0]
  let encrypted_attrs = TelemetryEvent::attributes(encrypted_event)
  
  let user_id_value = Attributes::get(encrypted_attrs, "user_id")
  match user_id_value {
    Some(StringValue(value)) => {
      // Should be encrypted, not the original value
      assert_neq(value, "user-12345")
      assert_true(value.length() > 20) // Encrypted values are typically longer
    }
    _ => assert_true(false)
  }
  
  // Decrypt the data
  let decrypted_data = SecureTelemetryProcessor::decrypt_fields(secure_processor, encrypted_data, secure_config)
  
  // Verify sensitive fields are decrypted back to original values
  let decrypted_events = TelemetryData::events(decrypted_data)
  assert_eq(decrypted_events.length(), 1)
  
  let decrypted_event = decrypted_events[0]
  let decrypted_attrs = TelemetryEvent::attributes(decrypted_event)
  
  let decrypted_user_id = Attributes::get(decrypted_attrs, "user_id")
  match decrypted_user_id {
    Some(StringValue(value)) => assert_eq(value, "user-12345")
    _ => assert_true(false)
  }
  
  let decrypted_ip = Attributes::get(decrypted_attrs, "ip_address")
  match decrypted_ip {
    Some(StringValue(value)) => assert_eq(value, "192.168.1.100")
    _ => assert_true(false)
  }
  
  let decrypted_token = Attributes::get(decrypted_attrs, "session_token")
  match decrypted_token {
    Some(StringValue(value)) => assert_eq(value, "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9")
    _ => assert_true(false)
  }
}

// Test 6: Access Control
test "access control" {
  // Create an access control manager
  let access_manager = AccessControlManager::new()
  
  // Define roles
  AccessControlManager::add_role(access_manager, "admin", ["read", "write", "delete", "manage"])
  AccessControlManager::add_role(access_manager, "analyst", ["read", "write"])
  AccessControlManager::add_role(access_manager, "viewer", ["read"])
  
  // Define users
  AccessControlManager::add_user(access_manager, "user1", ["admin"])
  AccessControlManager::add_user(access_manager, "user2", ["analyst"])
  AccessControlManager::add_user(access_manager, "user3", ["viewer"])
  
  // Test permissions
  assert_true(AccessControlManager::has_permission(access_manager, "user1", "read"))
  assert_true(AccessControlManager::has_permission(access_manager, "user1", "write"))
  assert_true(AccessControlManager::has_permission(access_manager, "user1", "delete"))
  assert_true(AccessControlManager::has_permission(access_manager, "user1", "manage"))
  
  assert_true(AccessControlManager::has_permission(access_manager, "user2", "read"))
  assert_true(AccessControlManager::has_permission(access_manager, "user2", "write"))
  assert_false(AccessControlManager::has_permission(access_manager, "user2", "delete"))
  assert_false(AccessControlManager::has_permission(access_manager, "user2", "manage"))
  
  assert_true(AccessControlManager::has_permission(access_manager, "user3", "read"))
  assert_false(AccessControlManager::has_permission(access_manager, "user3", "write"))
  assert_false(AccessControlManager::has_permission(access_manager, "user3", "delete"))
  assert_false(AccessControlManager::has_permission(access_manager, "user3", "manage"))
  
  // Test resource-based access control
  AccessControlManager::add_resource_rule(access_manager, "telemetry.data", "admin", ["read", "write", "delete"])
  AccessControlManager::add_resource_rule(access_manager, "telemetry.data", "analyst", ["read"])
  AccessControlManager::add_resource_rule(access_manager, "telemetry.config", "admin", ["read", "write", "delete"])
  
  assert_true(AccessControlManager::can_access_resource(access_manager, "user1", "telemetry.data", "read"))
  assert_true(AccessControlManager::can_access_resource(access_manager, "user1", "telemetry.data", "write"))
  assert_true(AccessControlManager::can_access_resource(access_manager, "user1", "telemetry.data", "delete"))
  
  assert_true(AccessControlManager::can_access_resource(access_manager, "user2", "telemetry.data", "read"))
  assert_false(AccessControlManager::can_access_resource(access_manager, "user2", "telemetry.data", "write"))
  
  assert_false(AccessControlManager::can_access_resource(access_manager, "user3", "telemetry.data", "read"))
  
  assert_true(AccessControlManager::can_access_resource(access_manager, "user1", "telemetry.config", "read"))
  assert_false(AccessControlManager::can_access_resource(access_manager, "user2", "telemetry.config", "read"))
}

// Test 7: Secure Transport
test "secure transport" {
  // Create a secure transport client
  let tls_config = TLSConfig::new()
  TLSConfig::set_verify_mode(tls_config, RequirePeerCertificate)
  TLSConfig::add_ca_certificate(tls_config, "ca_cert.pem")
  
  let secure_client = SecureTransportClient::new(tls_config)
  
  // Test secure connection establishment
  let server_url = "https://secure-telemetry.example.com:443"
  let connection_result = SecureTransportClient::connect(secure_client, server_url)
  
  match connection_result {
    Ok(connection) => {
      // Verify connection is secure
      assert_true(SecureTransportClient::is_secure(connection))
      
      // Get connection details
      let connection_info = SecureTransportClient::get_connection_info(connection)
      assert_eq(connection_info.protocol, "TLS")
      assert_true(connection_info.cipher_suite.length() > 0)
      assert_true(connection_info.peer_certificate.length() > 0)
      
      // Test secure data transmission
      let telemetry_data = TelemetryData::new()
      TelemetryData::add_metric(telemetry_data, "test.metric", 42.0)
      
      let send_result = SecureTransportClient::send_telemetry(connection, telemetry_data)
      assert_true(send_result.is_ok())
      
      // Test secure data reception
      let receive_result = SecureTransportClient::receive_telemetry(connection)
      match receive_result {
        Ok(received_data) => {
          // Verify received data integrity
          assert_true(TelemetryData::has_metric(received_data, "test.metric"))
        }
        Err(_) => assert_true(false)
      }
      
      // Close the connection
      SecureTransportClient::close(connection)
    }
    Err(_) => assert_true(false) // Connection should succeed
  }
  
  // Test mutual TLS
  let mtls_config = MTLSConfig::new()
  MTLSConfig::set_client_certificate(mtls_config, "client_cert.pem")
  MTLSConfig::set_client_private_key(mtls_config, "client_key.pem")
  MTLSConfig::add_ca_certificate(mtls_config, "ca_cert.pem")
  
  let mtls_client = SecureTransportClient::with_mtls(mtls_config)
  
  let mtls_connection_result = SecureTransportClient::connect(mtls_client, server_url)
  match mtls_connection_result {
    Ok(connection) => {
      // Verify mutual TLS is active
      assert_true(SecureTransportClient::is_mutual_tls(connection))
      
      // Test client authentication
      let auth_result = SecureTransportClient::authenticate(connection)
      assert_true(auth_result.is_ok())
      
      SecureTransportClient::close(connection)
    }
    Err(_) => assert_true(false) // Connection should succeed
  }
}

// Test 8: Security Audit Logging
test "security audit logging" {
  // Create a security audit logger
  let audit_logger = SecurityAuditLogger::new()
  SecurityAuditLogger::set_log_level(audit_logger, Security)
  
  // Log security events
  let login_event = SecurityEvent::new(
    "user.login",
    "user1",
    "192.168.1.100",
    "User logged in successfully"
  )
  SecurityAuditLogger::log_event(audit_logger, login_event)
  
  let access_denied_event = SecurityEvent::new(
    "access.denied",
    "user2",
    "192.168.1.101",
    "Access denied to resource telemetry.config"
  )
  SecurityAuditLogger::log_event(audit_logger, access_denied_event)
  
  let data_access_event = SecurityEvent::new(
    "data.access",
    "user1",
    "192.168.1.100",
    "Accessed telemetry data for service-a"
  )
  SecurityAuditLogger::log_event(audit_logger, data_access_event)
  
  // Query audit logs
  let all_events = SecurityAuditLogger::get_all_events(audit_logger)
  assert_eq(all_events.length(), 3)
  
  // Query by user
  let user1_events = SecurityAuditLogger::get_events_by_user(audit_logger, "user1")
  assert_eq(user1_events.length(), 2)
  
  let user2_events = SecurityAuditLogger::get_events_by_user(audit_logger, "user2")
  assert_eq(user2_events.length(), 1)
  
  // Query by event type
  let login_events = SecurityAuditLogger::get_events_by_type(audit_logger, "user.login")
  assert_eq(login_events.length(), 1)
  
  let access_denied_events = SecurityAuditLogger::get_events_by_type(audit_logger, "access.denied")
  assert_eq(access_denied_events.length(), 1)
  
  // Query by time range
  let now = Time::now()
  let one_hour_ago = now - 3600000L // 1 hour ago in milliseconds
  
  let recent_events = SecurityAuditLogger::get_events_by_time_range(audit_logger, one_hour_ago, now)
  assert_eq(recent_events.length(), 3) // All events should be recent
  
  // Verify event details
  let first_event = all_events[0]
  assert_eq(SecurityEvent::event_type(first_event), "user.login")
  assert_eq(SecurityEvent::user_id(first_event), "user1")
  assert_eq(SecurityEvent::ip_address(first_event), "192.168.1.100")
  assert_eq(SecurityEvent::description(first_event), "User logged in successfully")
  
  // Test event serialization
  let serialized_events = SecurityAuditLogger::serialize_events(audit_logger, all_events)
  assert_true(serialized_events.length() > 0)
  
  // Test event deserialization
  let deserialized_events = SecurityAuditLogger::deserialize_events(audit_logger, serialized_events)
  assert_eq(deserialized_events.length(), all_events.length())
  
  // Verify deserialized events match original
  for i in 0..=2 {
    let original = all_events[i]
    let deserialized = deserialized_events[i]
    
    assert_eq(SecurityEvent::event_type(original), SecurityEvent::event_type(deserialized))
    assert_eq(SecurityEvent::user_id(original), SecurityEvent::user_id(deserialized))
    assert_eq(SecurityEvent::ip_address(original), SecurityEvent::ip_address(deserialized))
    assert_eq(SecurityEvent::description(original), SecurityEvent::description(deserialized))
  }
}
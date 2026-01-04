// Azimuth Security and Encryption Test Suite
// This file contains comprehensive test cases for security and encryption scenarios

// Test 1: Basic Data Encryption
test "basic data encryption" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "encryption_tracer")
  
  // Create encryption configuration
  let encryption_config = {
    algorithm: "AES-256-GCM",
    key_derivation: "PBKDF2",
    key_iterations: 10000,
    salt_length: 16
  }
  
  // Initialize encryption
  let encryption = Encryption::new(encryption_config)
  
  // Test data to encrypt
  let test_data = "Sensitive telemetry data that should be encrypted"
  let test_key = "test_encryption_key_12345"
  
  // Encrypt data
  let encrypted_data = Encryption::encrypt(encryption, test_data, test_key)
  
  // Verify encryption worked
  assert_true(encrypted_data.length() > 0)
  assert_true(encrypted_data != test_data)
  
  // Decrypt data
  let decrypted_data = Encryption::decrypt(encryption, encrypted_data, test_key)
  
  // Verify decryption worked
  assert_true(decrypted_data.is_some())
  
  match decrypted_data {
    Some(decrypted) => {
      assert_eq(decrypted, test_data)
    }
    None => assert_true(false)
  }
  
  // Create a span with encrypted attributes
  let span = Tracer::start_span(tracer, "encrypted_span")
  
  // Add encrypted attribute
  let encrypted_attr = Encryption::encrypt(encryption, "user_id_12345", test_key)
  Span::add_attribute(span, "user.id", encrypted_attr)
  
  // Add regular attribute
  Span::add_attribute(span, "operation", "test")
  
  Span::end(span)
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Verify encrypted attribute
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 1)
  
  let span = spans[0]
  assert_true(span.attributes.contains(("operation", "test")))
  
  // Verify user.id is encrypted
  match span.attributes.find(fn(a) { a.0 == "user.id" }) {
    Some((_, encrypted_value)) => {
      assert_true(encrypted_value != "user_id_12345")
      
      // Decrypt the attribute value
      let decrypted_value = Encryption::decrypt(encryption, encrypted_value, test_key)
      match decrypted_value {
        Some(value) => assert_eq(value, "user_id_12345")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 2: End-to-End Encryption for Telemetry Data
test "end-to-end encryption for telemetry data" {
  // Initialize providers for different services
  let client_provider = TelemetryProvider::new()
  let server_provider = TelemetryProvider::new()
  
  // Get tracers
  let client_tracer = TelemetryProvider::get_tracer(client_provider, "client_tracer")
  let server_tracer = TelemetryProvider::get_tracer(server_provider, "server_tracer")
  
  // Create encryption configuration
  let encryption_config = {
    algorithm: "AES-256-GCM",
    key_derivation: "PBKDF2",
    key_iterations: 10000,
    salt_length: 16
  }
  
  // Initialize encryption with shared key
  let client_encryption = Encryption::new(encryption_config)
  let server_encryption = Encryption::new(encryption_config)
  let shared_key = "shared_encryption_key_12345"
  
  // Client creates a span with sensitive data
  let client_span = Tracer::start_span(client_tracer, "client_request")
  
  // Add sensitive attributes
  let sensitive_data = [
    ("user.id", "user123"),
    ("credit_card", "4111-1111-1111-1111"),
    ("ssn", "123-45-6789"),
    ("api_key", "sk_test_123456789")
  ]
  
  for (key, value) in sensitive_data {
    let encrypted_value = Encryption::encrypt(client_encryption, value, shared_key)
    Span::add_attribute(client_span, key, encrypted_value)
  }
  
  // Add non-sensitive attributes
  Span::add_attribute(client_span, "operation", "process_payment")
  Span::add_attribute(client_span, "amount", "99.99")
  
  // Add event with encrypted data
  let event_data = [
    ("transaction_id", "txn_12345"),
    ("merchant_id", "merchant_67890")
  ]
  
  let mut encrypted_event_data = []
  for (key, value) in event_data {
    let encrypted_value = Encryption::encrypt(client_encryption, value, shared_key)
    encrypted_event_data = encrypted_event_data.push((key, encrypted_value))
  }
  
  Span::add_event(client_span, "payment_processed", Some(encrypted_event_data))
  
  // Serialize and send span to server
  let serialized_span = Span::serialize(client_span)
  let encrypted_serialized = Encryption::encrypt(client_encryption, serialized_span, shared_key)
  
  // Server receives and decrypts span
  let decrypted_serialized = Encryption::decrypt(server_encryption, encrypted_serialized, shared_key)
  
  assert_true(decrypted_serialized.is_some())
  
  match decrypted_serialized {
    Some(serialized) => {
      // Deserialize span
      let server_span = Span::deserialize(serialized)
      
      assert_true(server_span.is_some())
      
      match server_span {
        Some(span) => {
          // Verify non-sensitive attributes
          assert_true(span.attributes.contains(("operation", "process_payment")))
          assert_true(span.attributes.contains(("amount", "99.99")))
          
          // Decrypt and verify sensitive attributes
          for (key, expected_value) in sensitive_data {
            match span.attributes.find(fn(a) { a.0 == key }) {
              Some((_, encrypted_value)) => {
                let decrypted_value = Encryption::decrypt(server_encryption, encrypted_value, shared_key);
                match decrypted_value {
                  Some(value) => assert_eq(value, expected_value)
                  None => assert_true(false)
                }
              }
              None => assert_true(false)
            }
          }
          
          // Verify event
          assert_eq(span.events.length(), 1)
          let event = span.events[0]
          assert_eq(event.name, "payment_processed")
          
          // Decrypt and verify event data
          for (key, expected_value) in event_data {
            match event.attributes.find(fn(a) { a.0 == key }) {
              Some((_, encrypted_value)) => {
                let decrypted_value = Encryption::decrypt(server_encryption, encrypted_value, shared_key);
                match decrypted_value {
                  Some(value) => assert_eq(value, expected_value)
                  None => assert_true(false)
                }
              }
              None => assert_true(false)
            }
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  Span::end(client_span)
}

// Test 3: Key Rotation and Management
test "key rotation and management" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "key_rotation_tracer")
  
  // Create key manager
  let key_manager = KeyManager::new()
  
  // Generate initial key
  let initial_key_id = KeyManager::generate_key(key_manager, "AES-256")
  
  // Create encryption with initial key
  let encryption_config = {
    algorithm: "AES-256-GCM",
    key_derivation: "PBKDF2",
    key_iterations: 10000,
    salt_length: 16
  }
  
  let encryption = Encryption::new_with_key_manager(encryption_config, key_manager)
  
  // Encrypt data with initial key
  let test_data = "Data encrypted with initial key"
  let encrypted_with_initial = Encryption::encrypt_with_key(encryption, test_data, initial_key_id)
  
  // Verify encryption works
  assert_true(encrypted_with_initial.length() > 0)
  
  // Decrypt with initial key
  let decrypted_with_initial = Encryption::decrypt_with_key(encryption, encrypted_with_initial, initial_key_id)
  
  match decrypted_with_initial {
    Some(decrypted) => assert_eq(decrypted, test_data)
    None => assert_true(false)
  }
  
  // Rotate to new key
  let new_key_id = KeyManager::rotate_key(key_manager, initial_key_id)
  
  // Verify key rotation
  assert_true(new_key_id != initial_key_id)
  
  // Encrypt data with new key
  let encrypted_with_new = Encryption::encrypt_with_key(encryption, test_data, new_key_id)
  
  // Decrypt with new key
  let decrypted_with_new = Encryption::decrypt_with_key(encryption, encrypted_with_new, new_key_id)
  
  match decrypted_with_new {
    Some(decrypted) => assert_eq(decrypted, test_data)
    None => assert_true(false)
  }
  
  // Try to decrypt new data with old key (should fail)
  let failed_decryption = Encryption::decrypt_with_key(encryption, encrypted_with_new, initial_key_id)
  assert_true(failed_decryption.is_none())
  
  // Create a span with data encrypted with different keys
  let span = Tracer::start_span(tracer, "key_rotation_span")
  
  // Add attribute encrypted with initial key
  let old_attr = Encryption::encrypt_with_key(encryption, "old_value", initial_key_id)
  Span::add_attribute(span, "old_attribute", old_attr)
  
  // Add attribute encrypted with new key
  let new_attr = Encryption::encrypt_with_key(encryption, "new_value", new_key_id)
  Span::add_attribute(span, "new_attribute", new_attr)
  
  Span::end(span)
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Verify attributes can be decrypted with correct keys
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 1)
  
  let span = spans[0]
  
  // Decrypt old attribute with initial key
  match span.attributes.find(fn(a) { a.0 == "old_attribute" }) {
    Some((_, encrypted_value)) => {
      let decrypted_value = Encryption::decrypt_with_key(encryption, encrypted_value, initial_key_id);
      match decrypted_value {
        Some(value) => assert_eq(value, "old_value")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Decrypt new attribute with new key
  match span.attributes.find(fn(a) { a.0 == "new_attribute" }) {
    Some((_, encrypted_value)) => {
      let decrypted_value = Encryption::decrypt_with_key(encryption, encrypted_value, new_key_id);
      match decrypted_value {
        Some(value) => assert_eq(value, "new_value")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 4: Secure Data Transmission
test "secure data transmission" {
  // Initialize providers for client and server
  let client_provider = TelemetryProvider::new()
  let server_provider = TelemetryProvider::new()
  
  // Get tracers
  let client_tracer = TelemetryProvider::get_tracer(client_provider, "client_tracer")
  let server_tracer = TelemetryProvider::get_tracer(server_provider, "server_tracer")
  
  // Create TLS configuration
  let tls_config = {
    protocol_version: "TLSv1.3",
    cipher_suites: ["TLS_AES_256_GCM_SHA384"],
    certificate_validation: true
  }
  
  // Initialize secure channel
  let client_channel = SecureChannel::new_client(tls_config)
  let server_channel = SecureChannel::new_server(tls_config)
  
  // Perform TLS handshake
  let handshake_result = SecureChannel::handshake(client_channel, server_channel)
  assert_true(handshake_result)
  
  // Client creates a span with sensitive data
  let client_span = Tracer::start_span(client_tracer, "secure_transmission")
  
  // Add sensitive attributes
  Span::add_attribute(client_span, "user.id", "user123")
  Span::add_attribute(client_span, "session.token", "token_abc123")
  
  // Serialize span
  let serialized_span = Span::serialize(client_span)
  
  // Transmit securely
  let transmitted_data = SecureChannel::transmit(client_channel, serialized_span)
  
  // Server receives data
  let received_data = SecureChannel::receive(server_channel, transmitted_data)
  
  assert_true(received_data.is_some())
  
  match received_data {
    Some(data) => {
      // Deserialize span
      let server_span = Span::deserialize(data)
      
      assert_true(server_span.is_some())
      
      match server_span {
        Some(span) => {
          // Verify attributes
          assert_true(span.attributes.contains(("user.id", "user123")))
          assert_true(span.attributes.contains(("session.token", "token_abc123")))
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  Span::end(client_span)
}

// Test 5: Access Control and Authorization
test "access control and authorization" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "access_control_tracer")
  
  // Create access control manager
  let access_manager = AccessControlManager::new()
  
  // Define roles and permissions
  AccessControlManager::define_role(access_manager, "admin", [
    "telemetry.read",
    "telemetry.write",
    "telemetry.delete",
    "user.manage"
  ])
  
  AccessControlManager::define_role(access_manager, "analyst", [
    "telemetry.read",
    "telemetry.write"
  ])
  
  AccessControlManager::define_role(access_manager, "viewer", [
    "telemetry.read"
  ])
  
  // Create users
  let admin_user = AccessControlManager::create_user(access_manager, "admin_user", ["admin"])
  let analyst_user = AccessControlManager::create_user(access_manager, "analyst_user", ["analyst"])
  let viewer_user = AccessControlManager::create_user(access_manager, "viewer_user", ["viewer"])
  
  // Create a span with access control
  let span = Tracer::start_span(tracer, "access_control_span")
  
  // Add attributes with different sensitivity levels
  Span::add_attribute(span, "public.data", "public_value")
  Span::add_attribute(span, "internal.data", "internal_value")
  Span::add_attribute(span, "confidential.data", "confidential_value")
  
  // Set access control on span
  Span::set_access_control(span, [
    ("public.data", ["viewer", "analyst", "admin"]),
    ("internal.data", ["analyst", "admin"]),
    ("confidential.data", ["admin"])
  ])
  
  Span::end(span)
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Get spans
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 1)
  
  let span = spans[0]
  
  // Test access control for different users
  // Admin should see all attributes
  let admin_view = AccessControlManager::filter_attributes(access_manager, span.attributes, admin_user)
  assert_eq(admin_view.length(), 3)
  assert_true(admin_view.contains(("public.data", "public_value")))
  assert_true(admin_view.contains(("internal.data", "internal_value")))
  assert_true(admin_view.contains(("confidential.data", "confidential_value")))
  
  // Analyst should see public and internal attributes
  let analyst_view = AccessControlManager::filter_attributes(access_manager, span.attributes, analyst_user)
  assert_eq(analyst_view.length(), 2)
  assert_true(analyst_view.contains(("public.data", "public_value")))
  assert_true(analyst_view.contains(("internal.data", "internal_value")))
  assert_false(analyst_view.contains(("confidential.data", "confidential_value")))
  
  // Viewer should only see public attributes
  let viewer_view = AccessControlManager::filter_attributes(access_manager, span.attributes, viewer_user)
  assert_eq(viewer_view.length(), 1)
  assert_true(viewer_view.contains(("public.data", "public_value")))
  assert_false(viewer_view.contains(("internal.data", "internal_value")))
  assert_false(viewer_view.contains(("confidential.data", "confidential_value")))
}

// Test 6: Data Anonymization
test "data anonymization" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "anonymization_tracer")
  
  // Create anonymizer
  let anonymizer = DataAnonymizer::new()
  
  // Define anonymization rules
  DataAnonymizer::add_rule(anonymizer, "email", AnonymizationType::Hash)
  DataAnonymizer::add_rule(anonymizer, "ip_address", AnonymizationType::Mask)
  DataAnonymizer::add_rule(anonymizer, "phone", AnonymizationType::PartialMask)
  DataAnonymizer::add_rule(anonymizer, "credit_card", AnonymizationType::Tokenize)
  
  // Create a span with PII data
  let span = Tracer::start_span(tracer, "anonymization_span")
  
  // Add PII attributes
  Span::add_attribute(span, "email", "user@example.com")
  Span::add_attribute(span, "ip_address", "192.168.1.100")
  Span::add_attribute(span, "phone", "+1-555-123-4567")
  Span::add_attribute(span, "credit_card", "4111-1111-1111-1111")
  Span::add_attribute(span, "non_pii", "safe_data")
  
  // Anonymize the span
  let anonymized_span = DataAnonymizer::anonymize_span(anonymizer, span)
  
  Span::end(span)
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Get spans
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 1)
  
  let span = spans[0]
  
  // Verify anonymization
  // Email should be hashed
  match span.attributes.find(fn(a) { a.0 == "email" }) {
    Some((_, value)) => {
      assert_true(value != "user@example.com")
      assert_true(value.length() == 64) // SHA-256 hash length
    }
    None => assert_true(false)
  }
  
  // IP address should be masked
  match span.attributes.find(fn(a) { a.0 == "ip_address" }) {
    Some((_, value)) => {
      assert_true(value != "192.168.1.100")
      assert_true(value.starts_with("192.168."))
      assert_true(value.ends_with(".0"))
    }
    None => assert_true(false)
  }
  
  // Phone should be partially masked
  match span.attributes.find(fn(a) { a.0 == "phone" }) {
    Some((_, value)) => {
      assert_true(value != "+1-555-123-4567")
      assert_true(value.starts_with("+1-555-"))
      assert_true(value.ends_with("-4567"))
      assert_true(value.contains("***"))
    }
    None => assert_true(false)
  }
  
  // Credit card should be tokenized
  match span.attributes.find(fn(a) { a.0 == "credit_card" }) {
    Some((_, value)) => {
      assert_true(value != "4111-1111-1111-1111")
      assert_true(value.starts_with("tok_"))
    }
    None => assert_true(false)
  }
  
  // Non-PII data should be unchanged
  assert_true(span.attributes.contains(("non_pii", "safe_data")))
}

// Test 7: Audit Logging
test "audit logging" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "audit_tracer")
  
  // Create audit logger
  let audit_logger = AuditLogger::new()
  
  // Configure audit logging
  AuditLogger::configure(audit_logger, {
    log_access: true,
    log_modifications: true,
    log_data_access: true,
    retention_days: 365
  })
  
  // Create a span with audit logging
  let span = Tracer::start_span(tracer, "audit_span")
  
  // Add sensitive attributes
  Span::add_attribute(span, "user.id", "user123")
  Span::add_attribute(span, "operation", "data_access")
  
  // Log access event
  AuditLogger::log_access(audit_logger, "user123", "telemetry", "read", "audit_span")
  
  // Log modification event
  Span::add_attribute(span, "status", "modified")
  AuditLogger::log_modification(audit_logger, "user123", "telemetry", "update", "status", "modified")
  
  // Log data access event
  AuditLogger::log_data_access(audit_logger, "user123", "telemetry", "user.id", "read")
  
  Span::end(span)
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Get audit logs
  let audit_logs = AuditLogger::get_logs(audit_logger)
  
  // Verify audit logs
  assert_eq(audit_logs.length(), 3)
  
  // Verify access log
  let access_log = audit_logs.find(fn(log) { log.event_type == "access" })
  match access_log {
    Some(log) => {
      assert_eq(log.user_id, "user123")
      assert_eq(log.resource, "telemetry")
      assert_eq(log.action, "read")
      assert_eq(log.details, "audit_span")
    }
    None => assert_true(false)
  }
  
  // Verify modification log
  let modification_log = audit_logs.find(fn(log) { log.event_type == "modification" })
  match modification_log {
    Some(log) => {
      assert_eq(log.user_id, "user123")
      assert_eq(log.resource, "telemetry")
      assert_eq(log.action, "update")
      assert_eq(log.field, "status")
      assert_eq(log.old_value, "")
      assert_eq(log.new_value, "modified")
    }
    None => assert_true(false)
  }
  
  // Verify data access log
  let data_access_log = audit_logs.find(fn(log) { log.event_type == "data_access" })
  match data_access_log {
    Some(log) => {
      assert_eq(log.user_id, "user123")
      assert_eq(log.resource, "telemetry")
      assert_eq(log.field, "user.id")
      assert_eq(log.action, "read")
    }
    None => assert_true(false)
  }
}

// Test 8: Data Integrity Verification
test "data integrity verification" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "integrity_tracer")
  
  // Create integrity verifier
  let integrity_verifier = IntegrityVerifier::new()
  
  // Configure integrity verification
  IntegrityVerifier::configure(integrity_verifier, {
    algorithm: "SHA-256",
    include_timestamp: true,
    include_user_id: true,
    sign_data: true
  })
  
  // Create a span
  let span = Tracer::start_span(tracer, "integrity_span")
  
  // Add attributes
  Span::add_attribute(span, "user.id", "user123")
  Span::add_attribute(span, "operation", "data_processing")
  Span::add_attribute(span, "record_count", "1000")
  
  // Calculate integrity hash
  let integrity_hash = IntegrityVerifier::calculate_hash(integrity_verifier, span)
  
  // Add integrity hash to span
  Span::add_attribute(span, "integrity.hash", integrity_hash)
  
  Span::end(span)
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Get spans
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 1)
  
  let span = spans[0]
  
  // Verify integrity
  let current_hash = IntegrityVerifier::calculate_hash(integrity_verifier, span)
  match span.attributes.find(fn(a) { a.0 == "integrity.hash" }) {
    Some((_, original_hash)) => {
      assert_eq(current_hash, original_hash)
    }
    None => assert_true(false)
  }
  
  // Simulate data tampering
  let mut tampered_span = span
  // Change an attribute
  for i in 0..tampered_span.attributes.length() {
    if tampered_span.attributes[i].0 == "record_count" {
      tampered_span.attributes[i] = ("record_count", "2000")
    }
  }
  
  // Verify integrity should fail
  let tampered_hash = IntegrityVerifier::calculate_hash(integrity_verifier, tampered_span)
  match tampered_span.attributes.find(fn(a) { a.0 == "integrity.hash" }) {
    Some((_, original_hash)) => {
      assert_true(tampered_hash != original_hash)
    }
    None => assert_true(false)
  }
}

// Test 9: Secure Configuration Management
test "secure configuration management" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "config_tracer")
  
  // Create secure config manager
  let config_manager = SecureConfigManager::new()
  
  // Add sensitive configuration
  SecureConfigManager::set(config_manager, "database.password", "secret_password", true)
  SecureConfigManager::set(config_manager, "api.key", "api_key_12345", true)
  SecureConfigManager::set(config_manager, "encryption.key", "encryption_key_67890", true)
  
  // Add non-sensitive configuration
  SecureConfigManager::set(config_manager, "service.name", "telemetry_service", false)
  SecureConfigManager::set(config_manager, "service.port", "8080", false)
  
  // Create a span using secure configuration
  let span = Tracer::start_span(tracer, "config_span")
  
  // Access non-sensitive config
  match SecureConfigManager::get(config_manager, "service.name") {
    Some(value) => Span::add_attribute(span, "service.name", value)
    None => assert_true(false)
  }
  
  match SecureConfigManager::get(config_manager, "service.port") {
    Some(value) => Span::add_attribute(span, "service.port", value)
    None => assert_true(false)
  }
  
  // Access sensitive config (should be encrypted in attributes)
  match SecureConfigManager::get(config_manager, "database.password") {
    Some(value) => {
      // Encrypt the sensitive value
      let encryption_config = {
        algorithm: "AES-256-GCM",
        key_derivation: "PBKDF2",
        key_iterations: 10000,
        salt_length: 16
      }
      
      let encryption = Encryption::new(encryption_config)
      let encrypted_value = Encryption::encrypt(encryption, value, "config_key")
      Span::add_attribute(span, "db.password", encrypted_value)
    }
    None => assert_true(false)
  }
  
  Span::end(span)
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Get spans
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 1)
  
  let span = spans[0]
  
  // Verify non-sensitive config is in plain text
  assert_true(span.attributes.contains(("service.name", "telemetry_service")))
  assert_true(span.attributes.contains(("service.port", "8080")))
  
  // Verify sensitive config is encrypted
  match span.attributes.find(fn(a) { a.0 == "db.password" }) {
    Some((_, encrypted_value)) => {
      assert_true(encrypted_value != "secret_password")
      
      // Decrypt to verify
      let encryption_config = {
        algorithm: "AES-256-GCM",
        key_derivation: "PBKDF2",
        key_iterations: 10000,
        salt_length: 16
      }
      
      let encryption = Encryption::new(encryption_config)
      let decrypted_value = Encryption::decrypt(encryption, encrypted_value, "config_key");
      
      match decrypted_value {
        Some(value) => assert_eq(value, "secret_password")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Verify sensitive config is not directly accessible
  assert_false(span.attributes.contains(("database.password", "secret_password")))
  assert_false(span.attributes.contains(("api.key", "api_key_12345")))
  assert_false(span.attributes.contains(("encryption.key", "encryption_key_67890")))
}

// Test 10: Security Metrics and Monitoring
test "security metrics and monitoring" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "security_metrics_tracer")
  let meter = TelemetryProvider::get_meter(provider, "security_meter")
  
  // Create security metrics
  let encryption_operations_counter = Meter::create_counter(meter, "encryption_operations")
  let decryption_operations_counter = Meter::create_counter(meter, "decryption_operations")
  let key_rotations_counter = Meter::create_counter(meter, "key_rotations")
  let access_denied_counter = Meter::create_counter(meter, "access_denied")
  let data_breach_gauge = Meter::create_gauge(meter, "data_breach_count")
  
  // Create security monitor
  let security_monitor = SecurityMonitor::new()
  
  // Configure security alerts
  SecurityMonitor::add_alert_rule(security_monitor, {
    name: "high_encryption_failures",
    condition: "encryption_failures > 10",
    severity: "warning",
    action: "log"
  })
  
  SecurityMonitor::add_alert_rule(security_monitor, {
    name: "data_breach_detected",
    condition: "data_breach_count > 0",
    severity: "critical",
    action: "alert"
  })
  
  // Simulate security operations
  let encryption_config = {
    algorithm: "AES-256-GCM",
    key_derivation: "PBKDF2",
    key_iterations: 10000,
    salt_length: 16
  }
  
  let encryption = Encryption::new(encryption_config)
  let test_key = "security_test_key"
  
  // Simulate encryption operations
  let encryption_success_count = 15
  let encryption_failure_count = 3
  
  for i in 0..encryption_success_count {
    let test_data = "test_data_" + i.to_string()
    let encrypted = Encryption::encrypt(encryption, test_data, test_key)
    
    if encrypted.length() > 0 {
      Counter::add(encryption_operations_counter, 1.0)
      SecurityMonitor::record_operation(security_monitor, "encryption_success")
    }
  }
  
  for i in 0..encryption_failure_count {
    // Simulate encryption failure with invalid key
    let test_data = "test_data_" + i.to_string()
    let encrypted = Encryption::encrypt(encryption, test_data, "")
    
    if encrypted.length() == 0 {
      SecurityMonitor::record_operation(security_monitor, "encryption_failure")
    }
  }
  
  // Simulate decryption operations
  let decryption_success_count = 12
  let decryption_failure_count = 2
  
  for i in 0..decryption_success_count {
    let test_data = "test_data_" + i.to_string()
    let encrypted = Encryption::encrypt(encryption, test_data, test_key)
    let decrypted = Encryption::decrypt(encryption, encrypted, test_key)
    
    match decrypted {
      Some(_) => {
        Counter::add(decryption_operations_counter, 1.0)
        SecurityMonitor::record_operation(security_monitor, "decryption_success")
      }
      None => SecurityMonitor::record_operation(security_monitor, "decryption_failure")
    }
  }
  
  // Simulate key rotation
  let key_rotation_count = 2
  for i in 0..key_rotation_count {
    Counter::add(key_rotations_counter, 1.0)
    SecurityMonitor::record_operation(security_monitor, "key_rotation")
  }
  
  // Simulate access denied events
  let access_denied_count = 5
  for i in 0..access_denied_count {
    Counter::add(access_denied_counter, 1.0)
    SecurityMonitor::record_operation(security_monitor, "access_denied")
  }
  
  // Simulate data breach (should trigger alert)
  Gauge::set(data_breach_gauge, 1.0)
  SecurityMonitor::record_operation(security_monitor, "data_breach")
  
  // Create a span with security monitoring
  let span = Tracer::start_span(tracer, "security_monitoring_span")
  
  // Add security metrics as attributes
  Span::add_attribute(span, "encryption_operations", encryption_success_count.to_string())
  Span::add_attribute(span, "encryption_failures", encryption_failure_count.to_string())
  Span::add_attribute(span, "decryption_operations", decryption_success_count.to_string())
  Span::add_attribute(span, "decryption_failures", decryption_failure_count.to_string())
  Span::add_attribute(span, "key_rotations", key_rotation_count.to_string())
  Span::add_attribute(span, "access_denied", access_denied_count.to_string())
  Span::add_attribute(span, "data_breach", "1")
  
  Span::end(span)
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Get security alerts
  let alerts = SecurityMonitor::get_alerts(security_monitor)
  
  // Verify alerts
  assert_true(alerts.length() >= 1)
  
  // Should have data breach alert
  let data_breach_alert = alerts.find(fn(alert) { alert.name == "data_breach_detected" })
  match data_breach_alert {
    Some(alert) => {
      assert_eq(alert.severity, "critical")
      assert_eq(alert.triggered, true)
    }
    None => assert_true(false)
  }
  
  // Get security metrics
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  
  // Verify security metrics
  let encryption_metrics = metrics.filter(fn(m) { m.name == "encryption_operations" })
  let decryption_metrics = metrics.filter(fn(m) { m.name == "decryption_operations" })
  let key_rotation_metrics = metrics.filter(fn(m) { m.name == "key_rotations" })
  let access_denied_metrics = metrics.filter(fn(m) { m.name == "access_denied" })
  let data_breach_metrics = metrics.filter(fn(m) { m.name == "data_breach_count" })
  
  assert_eq(encryption_metrics.length(), 1)
  assert_eq(decryption_metrics.length(), 1)
  assert_eq(key_rotation_metrics.length(), 1)
  assert_eq(access_denied_metrics.length(), 1)
  assert_eq(data_breach_metrics.length(), 1)
  
  assert_eq(encryption_metrics[0].value, encryption_success_count.to_float())
  assert_eq(decryption_metrics[0].value, decryption_success_count.to_float())
  assert_eq(key_rotation_metrics[0].value, key_rotation_count.to_float())
  assert_eq(access_denied_metrics[0].value, access_denied_count.to_float())
  assert_eq(data_breach_metrics[0].value, 1.0)
}
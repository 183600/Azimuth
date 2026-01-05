// Azimuth Security Enhanced Test Suite
// This file contains comprehensive test cases for security features and threat protection

// Test 1: Data Encryption and Decryption
test "data encryption and decryption" {
  // Test symmetric encryption
  let symmetric_key = EncryptionKey::generate_symmetric(256) // 256-bit key
  let plaintext = "Sensitive telemetry data that must be encrypted"
  
  let encrypted_data = SymmetricEncryption::encrypt(symmetric_key, plaintext)
  assert_true(String::length(encrypted_data) > 0)
  assert_not_eq(encrypted_data, plaintext)
  
  let decrypted_data = SymmetricEncryption::decrypt(symmetric_key, encrypted_data)
  assert_eq(decrypted_data, plaintext)
  
  // Test asymmetric encryption
  let key_pair = AsymmetricEncryption::generate_key_pair(2048) // 2048-bit RSA
  let private_key = AsymmetricEncryption::private_key(key_pair)
  let public_key = AsymmetricEncryption::public_key(key_pair)
  
  let asym_encrypted = AsymmetricEncryption::encrypt(public_key, plaintext)
  assert_true(String::length(asym_encrypted) > 0)
  assert_not_eq(asym_encrypted, plaintext)
  
  let asym_decrypted = AsymmetricEncryption::decrypt(private_key, asym_encrypted)
  assert_eq(asym_decrypted, plaintext)
  
  // Test key derivation
  let password = "SecurePassword123"
  let salt = "RandomSaltValue"
  let derived_key = KeyDerivation::derive_key(password, salt, 32) // 32-byte key
  assert_true(KeyDerivation::key_length(derived_key) == 32)
  
  // Test key serialization
  let serialized_key = EncryptionKey::serialize(symmetric_key)
  let deserialized_key = EncryptionKey::deserialize(serialized_key)
  
  let re_encrypted = SymmetricEncryption::encrypt(deserialized_key, plaintext)
  let re_decrypted = SymmetricEncryption::decrypt(deserialized_key, re_encrypted)
  assert_eq(re_decrypted, plaintext)
  
  // Test encryption of telemetry data
  let telemetry_data = TelemetryData::new()
  TelemetryData::add_field(telemetry_data, "user.id", "12345")
  TelemetryData::add_field(telemetry_data, "session.token", "abc123token")
  TelemetryData::add_field(telemetry_data, "transaction.amount", "99.99")
  
  let encrypted_telemetry = TelemetryEncryption::encrypt_data(telemetry_data, symmetric_key)
  let decrypted_telemetry = TelemetryEncryption::decrypt_data(encrypted_telemetry, symmetric_key)
  
  assert_eq(TelemetryData::get_field(decrypted_telemetry, "user.id"), "12345")
  assert_eq(TelemetryData::get_field(decrypted_telemetry, "session.token"), "abc123token")
  assert_eq(TelemetryData::get_field(decrypted_telemetry, "transaction.amount"), "99.99")
}

// Test 2: Access Control and Authorization
test "access control and authorization" {
  // Test role-based access control
  let rbac = RoleBasedAccessControl::new()
  
  // Define roles
  RoleBasedAccessControl::define_role(rbac, "admin", ["read", "write", "delete", "manage"])
  RoleBasedAccessControl::define_role(rbac, "operator", ["read", "write"])
  RoleBasedAccessControl::define_role(rbac, "viewer", ["read"])
  
  // Create users with roles
  let admin_user = User::new("admin_user", "admin")
  let operator_user = User::new("operator_user", "operator")
  let viewer_user = User::new("viewer_user", "viewer")
  
  // Test permission checks
  assert_true(RoleBasedAccessControl::has_permission(rbac, admin_user, "read"))
  assert_true(RoleBasedAccessControl::has_permission(rbac, admin_user, "delete"))
  assert_true(RoleBasedAccessControl::has_permission(rbac, admin_user, "manage"))
  
  assert_true(RoleBasedAccessControl::has_permission(rbac, operator_user, "read"))
  assert_true(RoleBasedAccessControl::has_permission(rbac, operator_user, "write"))
  assert_false(RoleBasedAccessControl::has_permission(rbac, operator_user, "delete"))
  assert_false(RoleBasedAccessControl::has_permission(rbac, operator_user, "manage"))
  
  assert_true(RoleBasedAccessControl::has_permission(rbac, viewer_user, "read"))
  assert_false(RoleBasedAccessControl::has_permission(rbac, viewer_user, "write"))
  assert_false(RoleBasedAccessControl::has_permission(rbac, viewer_user, "delete"))
  
  // Test resource-based access control
  let resource1 = Resource::new("telemetry.data", "user123", ["read", "write"])
  let resource2 = Resource::new("telemetry.config", "admin", ["read", "write", "delete"])
  
  let resource_ac = ResourceBasedAccessControl::new()
  ResourceBasedAccessControl::add_resource(resource_ac, resource1)
  ResourceBasedAccessControl::add_resource(resource_ac, resource2)
  
  // Test resource access
  assert_true(ResourceBasedAccessControl::can_access(resource_ac, viewer_user, resource1, "read"))
  assert_false(ResourceBasedAccessControl::can_access(resource_ac, viewer_user, resource1, "write"))
  
  assert_true(ResourceBasedAccessControl::can_access(resource_ac, admin_user, resource2, "delete"))
  assert_false(ResourceBasedAccessControl::can_access(resource_ac, operator_user, resource2, "delete"))
  
  // Test attribute-based access control
  let abac = AttributeBasedAccessControl::new()
  
  // Define policies
  let policy1 = Policy::new("high_value_data_access", [
    AttributeCondition::equals("data.sensitivity", "high"),
    AttributeCondition::equals("user.clearance_level", "high"),
    AttributeCondition::in_time_range("user.access_time", "09:00", "17:00")
  ])
  
  let policy2 = Policy::new("personal_data_access", [
    AttributeCondition::equals("data.type", "personal"),
    AttributeCondition::equals("user.department", "hr"),
    AttributeCondition::equals("user.purpose", "legitimate_business")
  ])
  
  AttributeBasedAccessControl::add_policy(abac, policy1)
  AttributeBasedAccessControl::add_policy(abac, policy2)
  
  // Test policy evaluation
  let user_attributes = [
    ("user.clearance_level", "high"),
    ("user.department", "hr"),
    ("user.access_time", "14:30"),
    ("user.purpose", "legitimate_business")
  ]
  
  let data_attributes1 = [
    ("data.sensitivity", "high"),
    ("data.type", "operational")
  ]
  
  let data_attributes2 = [
    ("data.sensitivity", "medium"),
    ("data.type", "personal")
  ]
  
  assert_true(AttributeBasedAccessControl::evaluate(abac, "high_value_data_access", user_attributes, data_attributes1))
  assert_false(AttributeBasedAccessControl::evaluate(abac, "high_value_data_access", user_attributes, data_attributes2))
  
  assert_true(AttributeBasedAccessControl::evaluate(abac, "personal_data_access", user_attributes, data_attributes2))
  assert_false(AttributeBasedAccessControl::evaluate(abac, "personal_data_access", user_attributes, data_attributes1))
}

// Test 3: Data Masking and Anonymization
test "data masking and anonymization" {
  // Test PII detection
  let pii_detector = PIIDetector::new()
  
  let text_with_pii = "User John Doe with email john.doe@example.com and phone 123-456-7890 has credit card 4111-1111-1111-1111"
  
  let detected_pii = PIIDetector::detect(pii_detector, text_with_pii)
  assert_true(detected_pii.contains("email"))
  assert_true(detected_pii.contains("phone"))
  assert_true(detected_pii.contains("credit_card"))
  
  // Test data masking
  let email_masker = DataMasker::email()
  let masked_email = DataMasker::mask(email_masker, "john.doe@example.com")
  assert_eq(masked_email, "j***.***@example.com")
  
  let phone_masker = DataMasker::phone()
  let masked_phone = DataMasker::mask(phone_masker, "123-456-7890")
  assert_eq(masked_phone, "123-***-***0")
  
  let credit_card_masker = DataMasker::credit_card()
  let masked_credit_card = DataMasker::mask(credit_card_masker, "4111-1111-1111-1111")
  assert_eq(masked_credit_card, "4111-****-****-1111")
  
  let ssn_masker = DataMasker::ssn()
  let masked_ssn = DataMasker::mask(ssn_masker, "123-45-6789")
  assert_eq(masked_ssn, "***-**-6789")
  
  // Test field-level masking in telemetry data
  let telemetry_data = TelemetryData::new()
  TelemetryData::add_field(telemetry_data, "user.name", "John Doe")
  TelemetryData::add_field(telemetry_data, "user.email", "john.doe@example.com")
  TelemetryData::add_field(telemetry_data, "user.phone", "123-456-7890")
  TelemetryData::add_field(telemetry_data, "user.ssn", "123-45-6789")
  TelemetryData::add_field(telemetry_data, "transaction.id", "txn_12345")
  TelemetryData::add_field(telemetry_data, "transaction.amount", "99.99")
  
  let masking_config = MaskingConfig::new([
    ("user.email", EmailMask),
    ("user.phone", PhoneMask),
    ("user.ssn", SSNMask)
  ])
  
  let masked_telemetry = DataMasker::mask_telemetry(telemetry_data, masking_config)
  
  assert_eq(TelemetryData::get_field(masked_telemetry, "user.name"), "John Doe") // Not masked
  assert_eq(TelemetryData::get_field(masked_telemetry, "user.email"), "j***.***@example.com") // Masked
  assert_eq(TelemetryData::get_field(masked_telemetry, "user.phone"), "123-***-***0") // Masked
  assert_eq(TelemetryData::get_field(masked_telemetry, "user.ssn"), "***-**-6789") // Masked
  assert_eq(TelemetryData::get_field(masked_telemetry, "transaction.id"), "txn_12345") // Not masked
  assert_eq(TelemetryData::get_field(masked_telemetry, "transaction.amount"), "99.99") // Not masked
  
  // Test data anonymization
  let anonymizer = DataAnonymizer::new()
  
  let anonymized_name = DataAnonymizer::anonymize_name(anonymizer, "John Doe")
  assert_true(String::length(anonymized_name) > 0)
  assert_not_eq(anonymized_name, "John Doe")
  
  let anonymized_email = DataAnonymizer::anonymize_email(anonymizer, "john.doe@example.com")
  assert_true(String::length(anonymized_email) > 0)
  assert_not_eq(anonymized_email, "john.doe@example.com")
  assert_true(String::contains(anonymized_email, "@")) // Should preserve email format
  
  let anonymized_ip = DataAnonymizer::anonymize_ip(anonymizer, "192.168.1.100")
  assert_true(String::length(anonymized_ip) > 0)
  assert_not_eq(anonymized_ip, "192.168.1.100")
  
  // Test telemetry data anonymization
  let anonymization_config = AnonymizationConfig::new([
    ("user.name", NameAnonymizer),
    ("user.email", EmailAnonymizer),
    ("user.ip", IPAnonymizer)
  ])
  
  let telemetry_with_ip = TelemetryData::new()
  TelemetryData::add_field(telemetry_with_ip, "user.name", "John Doe")
  TelemetryData::add_field(telemetry_with_ip, "user.email", "john.doe@example.com")
  TelemetryData::add_field(telemetry_with_ip, "user.ip", "192.168.1.100")
  
  let anonymized_telemetry = DataAnonymizer::anonymize_telemetry(telemetry_with_ip, anonymization_config)
  
  assert_not_eq(TelemetryData::get_field(anonymized_telemetry, "user.name"), "John Doe")
  assert_not_eq(TelemetryData::get_field(anonymized_telemetry, "user.email"), "john.doe@example.com")
  assert_not_eq(TelemetryData::get_field(anonymized_telemetry, "user.ip"), "192.168.1.100")
}

// Test 4: Threat Detection and Prevention
test "threat detection and prevention" {
  // Test anomaly detection
  let anomaly_detector = AnomalyDetector::new()
  
  // Train with normal patterns
  let normal_patterns = [
    [10, 15, 12, 18, 20, 14, 16], // Response times in ms
    [100, 120, 95, 110, 105, 115, 98], // CPU usage
    [500, 600, 550, 480, 520, 580, 510] // Memory usage
  ]
  
  for pattern in normal_patterns {
    AnomalyDetector::train(anomaly_detector, pattern)
  }
  
  // Test with normal data
  let normal_data = [13, 17, 15, 19, 16]
  let normal_score = AnomalyDetector::analyze(anomaly_detector, normal_data)
  assert_true(normal_score < 0.5) // Should be low (normal)
  
  // Test with anomalous data
  let anomalous_data = [10, 15, 80, 12, 18] // 80 is anomalous
  let anomalous_score = AnomalyDetector::analyze(anomaly_detector, anomalous_data)
  assert_true(anomalous_score > 0.7) // Should be high (anomalous)
  
  // Test intrusion detection
  let intrusion_detector = IntrusionDetector::new()
  
  // Define normal access patterns
  IntrusionDetector::add_normal_pattern(intrusion_detector, [
    ("source.ip", "192.168.1.0/24"),
    ("access.time", "09:00-17:00"),
    ("request.rate", "10/min"),
    ("user.agent", "legitimate_browser")
  ])
  
  // Test legitimate access
  let legitimate_access = [
    ("source.ip", "192.168.1.100"),
    ("access.time", "14:30"),
    ("request.rate", "8/min"),
    ("user.agent", "legitimate_browser")
  ]
  
  let legitimate_score = IntrusionDetector::analyze(intrusion_detector, legitimate_access)
  assert_true(legitimate_score < 0.3) // Should be low (legitimate)
  
  // Test suspicious access
  let suspicious_access = [
    ("source.ip", "203.0.113.100"), // External IP
    ("access.time", "02:30"), // Unusual time
    ("request.rate", "100/min"), // High rate
    ("user.agent", "script_bot")
  ]
  
  let suspicious_score = IntrusionDetector::analyze(intrusion_detector, suspicious_access)
  assert_true(suspicious_score > 0.8) // Should be high (suspicious)
  
  // Test DDoS detection
  let ddos_detector = DDoSDetector::new()
  DDoSDetector::configure_threshold(ddos_detector, 1000, 60) // 1000 requests per minute
  
  // Simulate normal traffic
  for i = 0; i < 500; i = i + 1 {
    DDoSDetector::record_request(ddos_detector, "192.168.1.100")
  }
  
  assert_false(DDoSDetector::is_attack_detected(ddos_detector))
  
  // Simulate attack traffic
  for i = 0; i < 1500; i = i + 1 {
    DDoSDetector::record_request(ddos_detector, "203.0.113.100")
  }
  
  assert_true(DDoSDetector::is_attack_detected(ddos_detector))
  
  let attack_sources = DDoSDetector::get_attack_sources(ddos_detector)
  assert_true(List::contains(attack_sources, "203.0.113.100"))
  
  // Test SQL injection detection
  let sql_injection_detector = SQLInjectionDetector::new()
  
  let legitimate_queries = [
    "SELECT * FROM users WHERE id = 123",
    "UPDATE products SET price = 19.99 WHERE id = 456",
    "INSERT INTO orders (user_id, amount) VALUES (789, 99.99)"
  ]
  
  for query in legitimate_queries {
    assert_false(SQLInjectionDetector::detect(sql_injection_detector, query))
  }
  
  let malicious_queries = [
    "SELECT * FROM users WHERE id = 123 OR 1=1",
    "UPDATE products SET price = 19.99; DROP TABLE users; --",
    "INSERT INTO orders (user_id, amount) VALUES (789, 99.99); SELECT * FROM credit_cards; --"
  ]
  
  for query in malicious_queries {
    assert_true(SQLInjectionDetector::detect(sql_injection_detector, query))
  }
  
  // Test XSS detection
  let xss_detector = XSSDetector::new()
  
  let legitimate_inputs = [
    "This is a normal text input",
    "Product description with <b>bold</b> text",
    "User entered: Hello, world!"
  ]
  
  for input in legitimate_inputs {
    assert_false(XSSDetector::detect(xss_detector, input))
  }
  
  let malicious_inputs = [
    "<script>alert('XSS')</script>",
    "<img src=x onerror=alert('XSS')>",
    "javascript:alert('XSS')"
  ]
  
  for input in malicious_inputs {
    assert_true(XSSDetector::detect(xss_detector, input))
  }
}

// Test 5: Security Auditing and Logging
test "security auditing and logging" {
  // Test security event logging
  let security_logger = SecurityLogger::new()
  
  // Log authentication events
  SecurityLogger::log_authentication_event(security_logger, AuthenticationEvent::new(
    "user123",
    "login",
    "success",
    "192.168.1.100",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
  ))
  
  SecurityLogger::log_authentication_event(security_logger, AuthenticationEvent::new(
    "user456",
    "login",
    "failure",
    "203.0.113.100",
    "python-requests/2.25.1"
  ))
  
  // Log authorization events
  SecurityLogger::log_authorization_event(security_logger, AuthorizationEvent::new(
    "user123",
    "read",
    "telemetry.data",
    "success",
    "user123 has read access to telemetry.data"
  ))
  
  SecurityLogger::log_authorization_event(security_logger, AuthorizationEvent::new(
    "user456",
    "delete",
    "telemetry.config",
    "failure",
    "user456 does not have delete access to telemetry.config"
  ))
  
  // Log data access events
  SecurityLogger::log_data_access_event(security_logger, DataAccessEvent::new(
    "user123",
    "view",
    "user.profile",
    "12345",
    "success"
  ))
  
  // Log configuration changes
  SecurityLogger::log_configuration_change_event(security_logger, ConfigurationChangeEvent::new(
    "admin",
    "update",
    "security.policy",
    "Updated password complexity requirements",
    "previous_policy.json",
    "new_policy.json"
  ))
  
  // Test security log querying
  let auth_events = SecurityLogger::query_authentication_events(security_logger, TimeRange::last_24h())
  assert_true(auth_events.length() >= 2)
  
  let failed_logins = SecurityLogger::query_failed_authentication_events(security_logger, TimeRange::last_24h())
  assert_true(failed_logins.length() >= 1)
  
  let data_access_by_user = SecurityLogger::query_data_access_events_by_user(security_logger, "user123", TimeRange::last_24h())
  assert_true(data_access_by_user.length() >= 1)
  
  // Test security audit report generation
  let audit_report = SecurityLogger::generate_audit_report(security_logger, TimeRange::last_24h())
  
  assert_true(AuditReport::authentication_events_count(audit_report) >= 2)
  assert_true(AuditReport::authorization_events_count(audit_report) >= 2)
  assert_true(AuditReport::data_access_events_count(audit_report) >= 1)
  assert_true(AuditReport::configuration_change_events_count(audit_report) >= 1)
  
  // Test security log integrity
  let log_integrity_checker = LogIntegrityChecker::new()
  
  // Calculate hash of current logs
  let current_hash = LogIntegrityChecker::calculate_hash(security_logger)
  
  // Verify log integrity
  assert_true(LogIntegrityChecker::verify_integrity(security_logger, current_hash))
  
  // Test log tampering detection
  // Simulate log tampering by modifying a log entry
  SecurityLogger::simulate_tampering(security_logger)
  
  let tampered_hash = LogIntegrityChecker::calculate_hash(security_logger)
  assert_not_eq(current_hash, tampered_hash)
  assert_false(LogIntegrityChecker::verify_integrity(security_logger, current_hash))
  
  // Test security log retention
  let retention_policy = RetentionPolicy::new(90) // 90 days
  SecurityLogger::apply_retention_policy(security_logger, retention_policy)
  
  // Verify old logs are removed
  let old_events = SecurityLogger::query_events_older_than(security_logger, 91)
  assert_eq(old_events.length(), 0)
}

// Test 6: Secure Communication
test "secure communication" {
  // Test TLS configuration
  let tls_config = TLSConfig::new()
  TLSConfig::set_min_version(tls_config, TLSVersion::V1_2)
  TLSConfig::set_cipher_suites(tls_config, [
    "TLS_AES_256_GCM_SHA384",
    "TLS_CHACHA20_POLY1305_SHA256",
    "TLS_AES_128_GCM_SHA256"
  ])
  TLSConfig::enable_certificate_validation(tls_config, true)
  TLSConfig::enable_hostname_verification(tls_config, true)
  
  assert_eq(TLSConfig::min_version(tls_config), TLSVersion::V1_2)
  assert_true(TLSConfig::certificate_validation_enabled(tls_config))
  assert_true(TLSConfig::hostname_verification_enabled(tls_config))
  
  // Test certificate validation
  let cert_validator = CertificateValidator::new()
  
  // Add trusted CA certificates
  CertificateValidator::add_trusted_ca(cert_validator, "/path/to/ca1.pem")
  CertificateValidator::add_trusted_ca(cert_validator, "/path/to/ca2.pem")
  
  // Test certificate chain validation
  let valid_cert_chain = CertificateChain::new([
    "/path/to/server.pem",
    "/path/to/intermediate.pem",
    "/path/to/ca.pem"
  ])
  
  assert_true(CertificateValidator::validate_chain(cert_validator, valid_cert_chain))
  
  // Test certificate revocation checking
  CertificateValidator::enable_revocation_checking(cert_validator, true)
  assert_true(CertificateValidator::revocation_checking_enabled(cert_validator))
  
  // Test mutual TLS
  let mtls_config = MTLSConfig::new()
  MTLSConfig::set_client_certificate(mtls_config, "/path/to/client.pem")
  MTLSConfig::set_client_private_key(mtls_config, "/path/to/client.key")
  MTLSConfig::set_trusted_ca_certificates(mtls_config, [
    "/path/to/ca1.pem",
    "/path/to/ca2.pem"
  ])
  
  assert_true(MTLSConfig::is_configured(mtls_config))
  
  // Test secure HTTP client
  let secure_http_client = SecureHttpClient::with_tls(tls_config)
  
  let secure_request = HttpRequest::get("https://secure-api.example.com/data")
  SecureHttpClient::add_header(secure_request, "Authorization", "Bearer token123")
  
  let secure_response = SecureHttpClient::execute(secure_http_client, secure_request)
  assert_eq(HttpResponse::status(secure_response), 200)
  
  // Test secure WebSocket
  let secure_websocket_config = SecureWebSocketConfig::new()
  SecureWebSocketConfig::set_tls_config(secure_websocket_config, tls_config)
  
  let secure_websocket_client = SecureWebSocketClient::with_config(secure_websocket_config)
  let secure_connection = SecureWebSocketClient::connect(secure_websocket_client, "wss://secure-api.example.com/ws")
  
  assert_true(SecureWebSocketConnection::is_secure(secure_connection))
  assert_true(SecureWebSocketConnection::is_encrypted(secure_connection))
  
  // Test message encryption over WebSocket
  let message = WebSocketMessage::text("Sensitive data")
  let encrypted_message = SecureWebSocketConnection::encrypt_message(secure_connection, message)
  SecureWebSocketConnection::send(secure_connection, encrypted_message)
  
  let received_message = SecureWebSocketConnection::receive(secure_connection)
  match received_message {
    Some(msg) => {
      assert_eq(WebSocketMessage::message_type(msg), TextMessage)
      let decrypted_message = SecureWebSocketConnection::decrypt_message(secure_connection, msg)
      assert_eq(WebSocketMessage::text(decrypted_message), "Sensitive data")
    }
    None => assert_true(false)
  }
  
  // Test secure gRPC
  let secure_grpc_config = SecureGrpcConfig::new()
  SecureGrpcConfig::set_tls_config(secure_grpc_config, tls_config)
  SecureGrpcConfig::enable_per_rpc_credentials(secure_grpc_config, true)
  
  let secure_grpc_client = SecureGrpcClient::with_config("secure-grpc.example.com:443", secure_grpc_config)
  
  let grpc_credentials = GrpcCredentials::bearer("token123")
  let secure_grpc_request = GrpcRequest::new("UserService", "GetUser", "{\"id\":\"123\"}")
  GrpcRequest::set_credentials(secure_grpc_request, grpc_credentials)
  
  let secure_grpc_response = SecureGrpcClient::execute(secure_grpc_client, secure_grpc_request)
  assert_eq(GrpcResponse::status(secure_grpc_response), 200)
  
  // Test secure communication metrics
  let comm_metrics = SecureCommunicationMetrics::new()
  SecureCommunicationMetrics::record_secure_connection(comm_metrics, "https://secure-api.example.com")
  SecureCommunicationMetrics::record_certificate_validation(comm_metrics, true)
  SecureCommunicationMetrics::record_encrypted_message(comm_metrics, "sent")
  SecureCommunicationMetrics::record_encrypted_message(comm_metrics, "received")
  
  let metrics = SecureCommunicationMetrics::get_metrics(comm_metrics)
  assert_eq(Map::get(metrics, "secure.connections.total"), Some(1))
  assert_eq(Map::get(metrics, "certificate.validations.success"), Some(1))
  assert_eq(Map::get(metrics, "encrypted.messages.sent"), Some(1))
  assert_eq(Map::get(metrics, "encrypted.messages.received"), Some(1))
}

// Test 7: Key Management
test "key management" {
  // Test key generation
  let symmetric_key = KeyGenerator::generate_symmetric(256)
  assert_eq(Key::length(symmetric_key), 256)
  assert_eq(Key::type(symmetric_key), SymmetricKey)
  
  let asymmetric_key_pair = KeyGenerator::generate_asymmetric(2048)
  let private_key = KeyPair::private_key(asymmetric_key_pair)
  let public_key = KeyPair::public_key(asymmetric_key_pair)
  
  assert_eq(Key::length(private_key), 2048)
  assert_eq(Key::type(private_key), PrivateKey)
  assert_eq(Key::length(public_key), 2048)
  assert_eq(Key::type(public_key), PublicKey)
  
  // Test key storage
  let key_store = KeyStore::new()
  
  let key_id = "encryption_key_1"
  KeyStore::store_key(key_store, key_id, symmetric_key)
  
  let retrieved_key = KeyStore::get_key(key_store, key_id)
  assert_true(Option::is_some(retrieved_key))
  
  match retrieved_key {
    Some(key) => {
      assert_eq(Key::length(key), 256)
      assert_eq(Key::type(key), SymmetricKey)
    }
    None => assert_true(false)
  }
  
  // Test key rotation
  let old_key = symmetric_key
  let new_key = KeyGenerator::generate_symmetric(256)
  
  KeyStore::rotate_key(key_store, key_id, new_key)
  
  let rotated_key = KeyStore::get_key(key_store, key_id)
  match rotated_key {
    Some(key) => assert_not_eq(Key::id(key), Key::id(old_key)),
    None => assert_true(false)
  }
  
  // Test key versioning
  let versioned_key_store = VersionedKeyStore::new()
  
  VersionedKeyStore::store_key(versioned_key_store, "api_key", symmetric_key, 1)
  VersionedKeyStore::store_key(versioned_key_store, "api_key", new_key, 2)
  
  let latest_key = VersionedKeyStore::get_latest_key(versioned_key_store, "api_key")
  match latest_key {
    Some(key) => assert_eq(Key::version(key), 2),
    None => assert_true(false)
  }
  
  let version_1_key = VersionedKeyStore::get_key_by_version(versioned_key_store, "api_key", 1)
  match version_1_key {
    Some(key) => assert_eq(Key::version(key), 1),
    None => assert_true(false)
  }
  
  // Test key expiration
  let expiring_key = KeyGenerator::generate_symmetric(256)
  let expiration_time = Time::now() + 3600 // 1 hour from now
  Key::set_expiration(expiring_key, expiration_time)
  
  let expiring_key_store = ExpiringKeyStore::new()
  ExpiringKeyStore::store_key(expiring_key_store, "temp_key", expiring_key)
  
  // Key should be valid now
  let current_key = ExpiringKeyStore::get_key(expiring_key_store, "temp_key")
  assert_true(Option::is_some(current_key))
  
  // Simulate time passing (in a real implementation)
  // Time::sleep(3600)
  
  // Key should be expired
  // let expired_key = ExpiringKeyStore::get_key(expiring_key_store, "temp_key")
  // assert_true(Option::is_none(expired_key))
  
  // Test key metadata
  let key_metadata = KeyMetadata::new()
  KeyMetadata::add_tag(key_metadata, "purpose", "data.encryption")
  KeyMetadata::add_tag(key_metadata, "owner", "security.team")
  KeyMetadata::add_tag(key_metadata, "environment", "production")
  
  assert_eq(KeyMetadata::get_tag(key_metadata, "purpose"), Some("data.encryption"))
  assert_eq(KeyMetadata::get_tag(key_metadata, "owner"), Some("security.team"))
  assert_eq(KeyMetadata::get_tag(key_metadata, "environment"), Some("production"))
  
  // Test key access logging
  let access_logger = KeyAccessLogger::new()
  
  KeyAccessLogger::log_access(access_logger, key_id, "read", "user123")
  KeyAccessLogger::log_access(access_logger, key_id, "read", "user456")
  KeyAccessLogger::log_access(access_logger, key_id, "update", "admin")
  
  let access_logs = KeyAccessLogger::get_access_logs(access_logger, key_id)
  assert_eq(access_logs.length(), 3)
  
  let read_accesses = KeyAccessLogger::filter_access_logs(access_logs, "read")
  assert_eq(read_accesses.length(), 2)
  
  let admin_accesses = KeyAccessLogger::filter_access_logs_by_user(access_logs, "admin")
  assert_eq(admin_accesses.length(), 1)
}

// Test 8: Security Policy Enforcement
test "security policy enforcement" {
  // Test policy definition
  let policy_manager = PolicyManager::new()
  
  // Define data encryption policy
  let encryption_policy = SecurityPolicy::new("data_encryption_policy")
  SecurityPolicy::add_rule(encryption_policy, PolicyRule::new(
    "encrypt_sensitive_data",
    [
      PolicyCondition::field_equals("data.sensitivity", "high"),
      PolicyCondition::field_not_equals("data.encrypted", "true")
    ],
    PolicyAction::encrypt("data.content")
  ))
  
  // Define access control policy
  let access_policy = SecurityPolicy::new("access_control_policy")
  SecurityPolicy::add_rule(access_policy, PolicyRule::new(
    "restrict_admin_access",
    [
      PolicyCondition::field_equals("user.role", "admin"),
      PolicyCondition::field_in_time_range("access.time", "09:00", "17:00"),
      PolicyCondition::field_in_ip_range("source.ip", "192.168.1.0/24")
    ],
    PolicyAction::allow("access.request")
  ))
  
  // Define retention policy
  let retention_policy = SecurityPolicy::new("data_retention_policy")
  SecurityPolicy::add_rule(retention_policy, PolicyRule::new(
    "delete_old_data",
    [
      PolicyCondition::field_older_than("data.created_at", 365), // 365 days
      PolicyCondition::field_not_equals("data.retention_exempt", "true")
    ],
    PolicyAction::delete("data.record")
  ))
  
  PolicyManager::add_policy(policy_manager, encryption_policy)
  PolicyManager::add_policy(policy_manager, access_policy)
  PolicyManager::add_policy(policy_manager, retention_policy)
  
  // Test policy evaluation
  let sensitive_data = [
    ("data.sensitivity", "high"),
    ("data.encrypted", "false"),
    ("data.content", "sensitive information")
  ]
  
  let encryption_result = PolicyManager::evaluate_policies(policy_manager, "data_encryption_policy", sensitive_data)
  assert_true(PolicyEvaluationResult::has_action(encryption_result, "encrypt"))
  
  // Test policy enforcement
  let policy_enforcer = PolicyEnforcer::new(policy_manager)
  
  let data_record = DataRecord::new()
  DataRecord::set_field(data_record, "sensitivity", "high")
  DataRecord::set_field(data_record, "encrypted", "false")
  DataRecord::set_field(data_record, "content", "sensitive information")
  
  let enforced_record = PolicyEnforcer::enforce_policies(policy_enforcer, data_record)
  
  assert_eq(DataRecord::get_field(enforced_record, "sensitivity"), "high")
  assert_eq(DataRecord::get_field(enforced_record, "encrypted"), "true")
  assert_not_eq(DataRecord::get_field(enforced_record, "content"), "sensitive information") // Should be encrypted
  
  // Test policy violation detection
  let access_request = [
    ("user.role", "admin"),
    ("access.time", "22:30"), // Outside business hours
    ("source.ip", "203.0.113.100"), // Outside corporate network
    ("access.request", "delete_all_data")
  ]
  
  let violation_result = PolicyManager::evaluate_policies(policy_manager, "access_control_policy", access_request)
  assert_true(PolicyEvaluationResult::has_violation(violation_result))
  
  // Test policy violation reporting
  let violation_reporter = PolicyViolationReporter::new()
  PolicyViolationReporter::report_violation(violation_reporter, violation_result)
  
  let violations = PolicyViolationReporter::get_violations(violation_reporter, TimeRange::last_24h())
  assert_true(violations.length() >= 1)
  
  // Test policy compliance checking
  let compliance_checker = PolicyComplianceChecker::new(policy_manager)
  
  let compliance_report = PolicyComplianceChecker::check_compliance(compliance_checker, "all_policies")
  assert_true(ComplianceReport::overall_score(compliance_report) >= 0.0)
  assert_true(ComplianceReport::overall_score(compliance_report) <= 1.0)
  
  // Test policy updates
  let updated_encryption_policy = SecurityPolicy::new("data_encryption_policy_v2")
  SecurityPolicy::add_rule(updated_encryption_policy, PolicyRule::new(
    "encrypt_sensitive_data",
    [
      PolicyCondition::field_equals("data.sensitivity", "high"),
      PolicyCondition::field_not_equals("data.encrypted", "true")
    ],
    PolicyAction::encrypt_with_algorithm("data.content", "AES-256-GCM")
  ))
  
  PolicyManager::update_policy(policy_manager, "data_encryption_policy", updated_encryption_policy)
  
  let updated_policy = PolicyManager::get_policy(policy_manager, "data_encryption_policy")
  assert_eq(SecurityPolicy::name(updated_policy), "data_encryption_policy_v2")
  
  // Test policy versioning
  let policy_history = PolicyManager::get_policy_history(policy_manager, "data_encryption_policy")
  assert_true(policy_history.length() >= 2) // Original and updated versions
}

// Test 9: Security Metrics and Monitoring
test "security metrics and monitoring" {
  // Test security metrics collection
  let security_metrics = SecurityMetrics::new()
  
  // Record authentication metrics
  SecurityMetrics::record_authentication_attempt(security_metrics, "success")
  SecurityMetrics::record_authentication_attempt(security_metrics, "success")
  SecurityMetrics::record_authentication_attempt(security_metrics, "failure")
  
  // Record authorization metrics
  SecurityMetrics::record_authorization_attempt(security_metrics, "success")
  SecurityMetrics::record_authorization_attempt(security_metrics, "failure")
  
  // Record data access metrics
  SecurityMetrics::record_data_access(security_metrics, "read", "user.profile")
  SecurityMetrics::record_data_access(security_metrics, "write", "user.profile")
  SecurityMetrics::record_data_access(security_metrics, "read", "telemetry.data")
  
  // Record threat detection metrics
  SecurityMetrics::record_threat_detected(security_metrics, "sql_injection")
  SecurityMetrics::record_threat_detected(security_metrics, "xss")
  SecurityMetrics::record_threat_detected(security_metrics, "ddos")
  
  // Get metrics
  let metrics = SecurityMetrics::get_metrics(security_metrics)
  
  assert_eq(Map::get(metrics, "auth.attempts.total"), Some(3))
  assert_eq(Map::get(metrics, "auth.attempts.success"), Some(2))
  assert_eq(Map::get(metrics, "auth.attempts.failure"), Some(1))
  
  assert_eq(Map::get(metrics, "authz.attempts.total"), Some(2))
  assert_eq(Map::get(metrics, "authz.attempts.success"), Some(1))
  assert_eq(Map::get(metrics, "authz.attempts.failure"), Some(1))
  
  assert_eq(Map::get(metrics, "data.access.read"), Some(2))
  assert_eq(Map::get(metrics, "data.access.write"), Some(1))
  
  assert_eq(Map::get(metrics, "threats.detected.total"), Some(3))
  assert_eq(Map::get(metrics, "threats.sql_injection"), Some(1))
  assert_eq(Map::get(metrics, "threats.xss"), Some(1))
  assert_eq(Map::get(metrics, "threats.ddos"), Some(1))
  
  // Test security dashboard
  let security_dashboard = SecurityDashboard::new(security_metrics)
  
  let authentication_summary = SecurityDashboard::get_authentication_summary(security_dashboard)
  assert_eq(AuthenticationSummary::total_attempts(authentication_summary), 3)
  assert_eq(AuthenticationSummary::success_rate(authentication_summary), 2.0 / 3.0)
  
  let threat_summary = SecurityDashboard::get_threat_summary(security_dashboard)
  assert_eq(ThreatSummary::total_threats(threat_summary), 3)
  
  let top_threats = SecurityDashboard::get_top_threats(security_dashboard, 5)
  assert_true(top_threats.length() >= 3)
  
  // Test security alerts
  let alert_manager = SecurityAlertManager::new()
  
  // Configure alerts
  SecurityAlertManager::configure_threshold_alert(alert_manager, "auth.failure.rate", 0.2) // 20% failure rate
  SecurityAlertManager::configure_threshold_alert(alert_manager, "threats.detected.rate", 10.0) // 10 threats per hour
  
  // Trigger alerts
  SecurityAlertManager::check_and_trigger_alerts(alert_manager, security_metrics)
  
  // Test security incident management
  let incident_manager = SecurityIncidentManager::new()
  
  // Create incidents
  let sql_injection_incident = SecurityIncident::new(
    "sql_injection_attack",
    "High",
    "SQL injection attack detected from IP 203.0.113.100",
    Time::now()
  )
  
  SecurityIncident::add_evidence(sql_injection_incident, "source_ip", "203.0.113.100")
  SecurityIncident::add_evidence(sql_injection_incident, "attack_vector", "sql_injection")
  SecurityIncident::add_evidence(sql_injection_incident, "query", "SELECT * FROM users WHERE id = 123 OR 1=1")
  
  let ddos_incident = SecurityIncident::new(
    "ddos_attack",
    "Critical",
    "DDoS attack detected with 1500 requests per minute",
    Time::now()
  )
  
  SecurityIncident::add_evidence(ddos_incident, "attack_type", "ddos")
  SecurityIncident::add_evidence(ddos_incident, "request_rate", "1500/min")
  SecurityIncident::add_evidence(ddos_incident, "duration", "5 minutes")
  
  SecurityIncidentManager::add_incident(incident_manager, sql_injection_incident)
  SecurityIncidentManager::add_incident(incident_manager, ddos_incident)
  
  // Get incidents
  let incidents = SecurityIncidentManager::get_incidents(incident_manager, TimeRange::last_24h())
  assert_eq(incidents.length(), 2)
  
  let critical_incidents = SecurityIncidentManager::get_incidents_by_severity(incident_manager, "Critical")
  assert_eq(critical_incidents.length(), 1)
  
  // Test security trends
  let security_trends = SecurityTrendsAnalyzer::new()
  
  // Analyze authentication trends
  let auth_trend = SecurityTrendsAnalyzer::analyze_authentication_trends(security_trends, security_metrics, TimeRange::last_7d())
  assert_true(AuthenticationTrend::is_anomaly_detected(auth_trend))
  
  // Analyze threat trends
  let threat_trend = SecurityTrendsAnalyzer::analyze_threat_trends(security_trends, security_metrics, TimeRange::last_7d())
  assert_true(ThreatTrend::top_threat_type(threat_trend) == "sql_injection")
  
  // Test security recommendations
  let recommendations = SecurityTrendsAnalyzer::generate_recommendations(security_trends, security_metrics)
  assert_true(recommendations.length() > 0)
  
  let has_auth_recommendation = recommendations.any(|r| Recommendation::category(r) == "authentication")
  let has_threat_recommendation = recommendations.any(|r| Recommendation::category(r) == "threat_detection")
  
  assert_true(has_auth_recommendation)
  assert_true(has_threat_recommendation)
}

// Test 10: Security Compliance
test "security compliance" {
  // Test GDPR compliance
  let gdpr_compliance_checker = GDPRComplianceChecker::new()
  
  let personal_data = PersonalData::new()
  PersonalData::add_field(personal_data, "name", "John Doe")
  PersonalData::add_field(personal_data, "email", "john.doe@example.com")
  PersonalData::add_field(personal_data, "phone", "123-456-7890")
  PersonalData::add_field(personal_data, "address", "123 Main St, Anytown, USA")
  
  // Test lawful basis for processing
  GDPRComplianceChecker::set_lawful_basis(gdpr_compliance_checker, personal_data, "consent")
  assert_eq(PersonalData::lawful_basis(personal_data), Some("consent"))
  
  // Test data retention
  GDPRComplianceChecker::set_retention_period(gdpr_compliance_checker, personal_data, 365) // 1 year
  assert_eq(PersonalData::retention_period(personal_data), Some(365))
  
  // Test data subject rights
  let data_subject_request = DataSubjectRequest::new("john.doe@example.com", "access")
  GDPRComplianceChecker::process_request(gdpr_compliance_checker, data_subject_request, personal_data)
  
  // Test right to be forgotten
  let deletion_request = DataSubjectRequest::new("john.doe@example.com", "deletion")
  let deletion_result = GDPRComplianceChecker::process_request(gdpr_compliance_checker, deletion_request, personal_data)
  assert_true(DataSubjectRequestResult::is_approved(deletion_result))
  
  // Test HIPAA compliance
  let hipaa_compliance_checker = HIPAAComplianceChecker::new()
  
  let phi_data = PHIData::new()
  PHIData::add_field(phi_data, "patient_name", "John Doe")
  PHIData::add_field(phi_data, "patient_id", "12345")
  PHIData::add_field(phi_data, "diagnosis", "Hypertension")
  PHIData::add_field(phi_data, "treatment", "Medication")
  
  // Test access controls
  HIPAAComplianceChecker::require_authentication(hipaa_compliance_checker, phi_data)
  HIPAAComplianceChecker::require_authorization(hipaa_compliance_checker, phi_data, "healthcare.provider")
  
  // Test audit logging
  HIPAAComplianceChecker::enable_audit_logging(hipaa_compliance_checker, phi_data)
  assert_true(PHIData::audit_logging_enabled(phi_data))
  
  // Test PCI DSS compliance
  let pci_compliance_checker = PCIDSSComplianceChecker::new()
  
  let payment_data = PaymentData::new()
  PaymentData::add_field(payment_data, "card_number", "4111-1111-1111-1111")
  PaymentData::add_field(payment_data, "cardholder_name", "John Doe")
  PaymentData::add_field(payment_data, "expiry_date", "12/25")
  PaymentData::add_field(payment_data, "cvv", "123")
  
  // Test encryption requirements
  PCIDSSComplianceChecker::require_encryption_at_rest(pci_compliance_checker, payment_data)
  PCIDSSComplianceChecker::require_encryption_in_transit(pci_compliance_checker, payment_data)
  
  // Test network security
  PCIDSSComplianceChecker::require_firewall(pci_compliance_checker, payment_data)
  PCIDSSComplianceChecker::require_secure_authentication(pci_compliance_checker, payment_data)
  
  // Test SOC 2 compliance
  let soc2_compliance_checker = SOC2ComplianceChecker::new()
  
  // Test security principle
  SOC2ComplianceChecker::implement_security_controls(soc2_compliance_checker)
  
  // Test availability principle
  SOC2ComplianceChecker::implement_availability_controls(soc2_compliance_checker)
  
  // Test processing integrity principle
  SOC2ComplianceChecker::implement_processing_integrity_controls(soc2_compliance_checker)
  
  // Test confidentiality principle
  SOC2ComplianceChecker::implement_confidentiality_controls(soc2_compliance_checker)
  
  // Test privacy principle
  SOC2ComplianceChecker::implement_privacy_controls(soc2_compliance_checker)
  
  // Test compliance reporting
  let compliance_reporter = ComplianceReporter::new()
  
  // Generate GDPR compliance report
  let gdpr_report = ComplianceReporter::generate_gdpr_report(compliance_reporter, personal_data)
  assert_true(GDPRReport::is_compliant(gdpr_report))
  
  // Generate HIPAA compliance report
  let hipaa_report = ComplianceReporter::generate_hipaa_report(compliance_reporter, phi_data)
  assert_true(HIPAAReport::is_compliant(hipaa_report))
  
  // Generate PCI DSS compliance report
  let pci_report = ComplianceReporter::generate_pci_report(compliance_reporter, payment_data)
  assert_true(PCIReport::is_compliant(pci_report))
  
  // Generate SOC 2 compliance report
  let soc2_report = ComplianceReporter::generate_soc2_report(compliance_reporter)
  assert_true(SOC2Report::is_compliant(soc2_report))
  
  // Test overall compliance score
  let overall_compliance = ComplianceReporter::calculate_overall_compliance(compliance_reporter, [
    gdpr_report,
    hipaa_report,
    pci_report,
    soc2_report
  ])
  
  assert_true(overall_compliance >= 0.8) // Should be highly compliant
}
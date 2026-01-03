// Azimuth Telemetry System - Security and Privacy Protection Tests
// This file contains test cases for security and privacy protection functionality

// Test 1: Authentication and Authorization
test "authentication and authorization" {
  // Create authentication manager
  let auth_manager = AuthenticationManager::new()
  
  // Configure authentication providers
  let jwt_provider = JwtAuthProvider::new("secret-key")
  let oauth_provider = OAuthAuthProvider::new("https://auth.example.com")
  let ldap_provider = LdapAuthProvider::new("ldap://ldap.example.com")
  
  AuthenticationManager::add_provider(auth_manager, "jwt", jwt_provider)
  AuthenticationManager::add_provider(auth_manager, "oauth", oauth_provider)
  AuthenticationManager::add_provider(auth_manager, "ldap", ldap_provider)
  
  // Create user accounts
  let admin_user = User::new("admin", "admin@example.com", UserRole::Admin)
  let operator_user = User::new("operator", "operator@example.com", UserRole::Operator)
  let viewer_user = User::new("viewer", "viewer@example.com", UserRole::Viewer)
  
  User::set_password(admin_user, "admin_password")
  User::set_password(operator_user, "operator_password")
  User::set_password(viewer_user, "viewer_password")
  
  AuthenticationManager::add_user(auth_manager, admin_user)
  AuthenticationManager::add_user(auth_manager, operator_user)
  AuthenticationManager::add_user(auth_manager, viewer_user)
  
  // Test authentication
  let admin_auth_result = AuthenticationManager::authenticate(auth_manager, "admin", "admin_password")
  match admin_auth_result {
    AuthResult::Success(token) => {
      assert_true(token.length() > 0)
      
      // Verify token
      let verify_result = AuthenticationManager::verify_token(auth_manager, token)
      match verify_result {
        TokenVerifyResult::Valid(user) => {
          assert_eq(user.username, "admin")
          assert_eq(user.role, UserRole::Admin)
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test failed authentication
  let failed_auth = AuthenticationManager::authenticate(auth_manager, "admin", "wrong_password")
  match failed_auth {
    AuthResult::Failed(reason) => assert_eq(reason, "Invalid credentials")
    _ => assert_true(false)
  }
  
  // Test authorization
  let admin_token = match admin_auth_result {
    AuthResult::Success(token) => token
    _ => ""
  }
  
  // Admin should have access to all resources
  assert_true(AuthenticationManager::can_access(auth_manager, admin_token, "/api/users"))
  assert_true(AuthenticationManager::can_access(auth_manager, admin_token, "/api/config"))
  assert_true(AuthenticationManager::can_access(auth_manager, admin_token, "/api/logs"))
  
  // Authenticate operator user
  let operator_auth_result = AuthenticationManager::authenticate(auth_manager, "operator", "operator_password")
  let operator_token = match operator_auth_result {
    AuthResult::Success(token) => token
    _ => ""
  }
  
  // Operator should have limited access
  assert_true(AuthenticationManager::can_access(auth_manager, operator_token, "/api/metrics"))
  assert_true(AuthenticationManager::can_access(auth_manager, operator_token, "/api/logs"))
  assert_false(AuthenticationManager::can_access(auth_manager, operator_token, "/api/users"))
  assert_false(AuthenticationManager::can_access(auth_manager, operator_token, "/api/config"))
  
  // Authenticate viewer user
  let viewer_auth_result = AuthenticationManager::authenticate(auth_manager, "viewer", "viewer_password")
  let viewer_token = match viewer_auth_result {
    AuthResult::Success(token) => token
    _ => ""
  }
  
  // Viewer should have read-only access
  assert_true(AuthenticationManager::can_access(auth_manager, viewer_token, "/api/metrics", "GET"))
  assert_false(AuthenticationManager::can_access(auth_manager, viewer_token, "/api/metrics", "POST"))
  assert_true(AuthenticationManager::can_access(auth_manager, viewer_token, "/api/logs", "GET"))
  assert_false(AuthenticationManager::can_access(auth_manager, viewer_token, "/api/logs", "DELETE"))
  
  // Test role-based permissions
  let permissions = AuthenticationManager::get_permissions(auth_manager, UserRole::Admin)
  assert_true(permissions.contains(Permission::UserManagement))
  assert_true(permissions.contains(Permission::ConfigManagement))
  assert_true(permissions.contains(Permission::LogAccess))
  
  let operator_permissions = AuthenticationManager::get_permissions(auth_manager, UserRole::Operator)
  assert_false(operator_permissions.contains(Permission::UserManagement))
  assert_false(operator_permissions.contains(Permission::ConfigManagement))
  assert_true(operator_permissions.contains(Permission::LogAccess))
  
  // Test token expiration
  let expired_token = AuthenticationManager::generate_token(auth_manager, admin_user, -1)  // Already expired
  let expired_result = AuthenticationManager::verify_token(auth_manager, expired_token)
  match expired_result {
    TokenVerifyResult::Expired => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 2: Data Encryption and Protection
test "data encryption and protection" {
  // Create encryption manager
  let encryption_manager = EncryptionManager::new()
  
  // Generate encryption keys
  let aes_key = EncryptionManager::generate_key(encryption_manager, EncryptionAlgorithm::AES256)
  let rsa_key_pair = EncryptionManager::generate_key_pair(encryption_manager, EncryptionAlgorithm::RSA2048)
  
  // Test symmetric encryption
  let sensitive_data = "This is sensitive telemetry data that must be encrypted"
  let encrypted_data = EncryptionManager::encrypt_symmetric(encryption_manager, sensitive_data.to_bytes(), aes_key)
  
  // Verify encrypted data is different from original
  assert_ne(encrypted_data.to_string(), sensitive_data)
  assert_true(encrypted_data.length() > 0)
  
  // Decrypt data
  let decrypted_data = EncryptionManager::decrypt_symmetric(encryption_manager, encrypted_data, aes_key)
  let decrypted_string = @bytes_to_string(decrypted_data)
  assert_eq(decrypted_string, sensitive_data)
  
  // Test asymmetric encryption
  let public_key = KeyPair::public_key(rsa_key_pair)
  let private_key = KeyPair::private_key(rsa_key_pair)
  
  let asymmetric_encrypted = EncryptionManager::encrypt_asymmetric(encryption_manager, sensitive_data.to_bytes(), public_key)
  let asymmetric_decrypted = EncryptionManager::decrypt_asymmetric(encryption_manager, asymmetric_encrypted, private_key)
  let asymmetric_decrypted_string = @bytes_to_string(asymmetric_decrypted)
  assert_eq(asymmetric_decrypted_string, sensitive_data)
  
  // Test data hashing
  let hash_algorithm = HashAlgorithm::SHA256
  let data_hash = EncryptionManager::hash_data(encryption_manager, sensitive_data.to_bytes(), hash_algorithm)
  
  // Verify hash is consistent
  let hash_again = EncryptionManager::hash_data(encryption_manager, sensitive_data.to_bytes(), hash_algorithm)
  assert_eq(data_hash, hash_again)
  
  // Verify hash is different for different data
  let different_data = "This is different telemetry data"
  let different_hash = EncryptionManager::hash_data(encryption_manager, different_data.to_bytes(), hash_algorithm)
  assert_ne(data_hash, different_hash)
  
  // Test digital signatures
  let signature = EncryptionManager::sign_data(encryption_manager, sensitive_data.to_bytes(), private_key)
  let is_valid = EncryptionManager::verify_signature(encryption_manager, sensitive_data.to_bytes(), signature, public_key)
  assert_true(is_valid)
  
  // Test signature with tampered data
  let tampered_data = "This is tampered telemetry data"
  let is_tampered_valid = EncryptionManager::verify_signature(encryption_manager, tampered_data.to_bytes(), signature, public_key)
  assert_false(is_tampered_valid)
  
  // Test key rotation
  let old_key = aes_key
  let new_key = EncryptionManager::generate_key(encryption_manager, EncryptionAlgorithm::AES256)
  
  let data_with_old_key = EncryptionManager::encrypt_symmetric(encryption_manager, "key rotation test".to_bytes(), old_key)
  
  // Re-encrypt with new key
  let reencrypted_data = EncryptionManager::reencrypt(encryption_manager, data_with_old_key, old_key, new_key)
  let decrypted_with_new_key = EncryptionManager::decrypt_symmetric(encryption_manager, reencrypted_data, new_key)
  let reencrypted_string = @bytes_to_string(decrypted_with_new_key)
  assert_eq(reencrypted_string, "key rotation test")
  
  // Test secure random generation
  let random_bytes = EncryptionManager::generate_secure_random(encryption_manager, 32)
  assert_eq(random_bytes.length(), 32)
  
  // Verify randomness
  let random_bytes2 = EncryptionManager::generate_secure_random(encryption_manager, 32)
  assert_ne(random_bytes, random_bytes2)
  
  // Test secure data storage
  let secure_storage = SecureStorage::new(encryption_manager)
  SecureStorage::store(secure_storage, "sensitive_config", sensitive_data, aes_key)
  
  let retrieved_data = SecureStorage::retrieve(secure_storage, "sensitive_config", aes_key)
  match retrieved_data {
    Some(data) => {
      let retrieved_string = @bytes_to_string(data)
      assert_eq(retrieved_string, sensitive_data)
    }
    None => assert_true(false)
  }
  
  // Test data masking
  let pii_data = "User: john.doe@example.com, Phone: +1-555-123-4567, SSN: 123-45-6789"
  let masked_data = DataMasking::mask_pii(pii_data)
  
  assert_false(masked_data.contains("john.doe@example.com"))
  assert_false(masked_data.contains("+1-555-123-4567"))
  assert_false(masked_data.contains("123-45-6789"))
  assert_true(masked_data.contains("jo***@***.com"))
  assert_true(masked_data.contains("***-***-****"))
  
  // Test tokenization
  let tokenizer = Tokenizer::new(encryption_manager)
  let original_value = "4111-1111-1111-1111"  // Credit card number
  let token = Tokenizer::tokenize(tokenizer, original_value)
  
  assert_ne(token, original_value)
  assert_true(token.length() > 0)
  
  let detokenized_value = Tokenizer::detokenize(tokenizer, token)
  assert_eq(detokenized_value, original_value)
}

// Test 3: Access Control and Permissions
test "access control and permissions" {
  // Create access control manager
  let access_manager = AccessControlManager::new()
  
  // Define resources
  let telemetry_metrics = Resource::new("telemetry.metrics", "Telemetry metrics data")
  let telemetry_logs = Resource::new("telemetry.logs", "Telemetry log data")
  let user_management = Resource::new("system.users", "User management")
  let system_config = Resource::new("system.config", "System configuration")
  
  AccessControlManager::add_resource(access_manager, telemetry_metrics)
  AccessControlManager::add_resource(access_manager, telemetry_logs)
  AccessControlManager::add_resource(access_manager, user_management)
  AccessControlManager::add_resource(access_manager, system_config)
  
  // Define permissions
  let read_metrics = Permission::new("read_metrics", "Read telemetry metrics")
  let write_metrics = Permission::new("write_metrics", "Write telemetry metrics")
  let read_logs = Permission::new("read_logs", "Read telemetry logs")
  let delete_logs = Permission::new("delete_logs", "Delete telemetry logs")
  let manage_users = Permission::new("manage_users", "Manage user accounts")
  let modify_config = Permission::new("modify_config", "Modify system configuration")
  
  AccessControlManager::add_permission(access_manager, read_metrics)
  AccessControlManager::add_permission(access_manager, write_metrics)
  AccessControlManager::add_permission(access_manager, read_logs)
  AccessControlManager::add_permission(access_manager, delete_logs)
  AccessControlManager::add_permission(access_manager, manage_users)
  AccessControlManager::add_permission(access_manager, modify_config)
  
  // Create roles
  let admin_role = Role::new("admin", "Administrator")
  let operator_role = Role::new("operator", "Operator")
  let analyst_role = Role::new("analyst", "Data analyst")
  
  // Assign permissions to roles
  Role::add_permission(admin_role, read_metrics)
  Role::add_permission(admin_role, write_metrics)
  Role::add_permission(admin_role, read_logs)
  Role::add_permission(admin_role, delete_logs)
  Role::add_permission(admin_role, manage_users)
  Role::add_permission(admin_role, modify_config)
  
  Role::add_permission(operator_role, read_metrics)
  Role::add_permission(operator_role, write_metrics)
  Role::add_permission(operator_role, read_logs)
  
  Role::add_permission(analyst_role, read_metrics)
  Role::add_permission(analyst_role, read_logs)
  
  AccessControlManager::add_role(access_manager, admin_role)
  AccessControlManager::add_role(access_manager, operator_role)
  AccessControlManager::add_role(access_manager, analyst_role)
  
  // Create users
  let admin_user = User::new("admin", "admin@example.com")
  let operator_user = User::new("operator", "operator@example.com")
  let analyst_user = User::new("analyst", "analyst@example.com")
  
  // Assign roles to users
  User::add_role(admin_user, "admin")
  User::add_role(operator_user, "operator")
  User::add_role(analyst_user, "analyst")
  
  AccessControlManager::add_user(access_manager, admin_user)
  AccessControlManager::add_user(access_manager, operator_user)
  AccessControlManager::add_user(access_manager, analyst_user)
  
  // Test role-based access control
  // Admin should have all permissions
  assert_true(AccessControlManager::has_permission(access_manager, "admin", "read_metrics"))
  assert_true(AccessControlManager::has_permission(access_manager, "admin", "write_metrics"))
  assert_true(AccessControlManager::has_permission(access_manager, "admin", "read_logs"))
  assert_true(AccessControlManager::has_permission(access_manager, "admin", "delete_logs"))
  assert_true(AccessControlManager::has_permission(access_manager, "admin", "manage_users"))
  assert_true(AccessControlManager::has_permission(access_manager, "admin", "modify_config"))
  
  // Operator should have limited permissions
  assert_true(AccessControlManager::has_permission(access_manager, "operator", "read_metrics"))
  assert_true(AccessControlManager::has_permission(access_manager, "operator", "write_metrics"))
  assert_true(AccessControlManager::has_permission(access_manager, "operator", "read_logs"))
  assert_false(AccessControlManager::has_permission(access_manager, "operator", "delete_logs"))
  assert_false(AccessControlManager::has_permission(access_manager, "operator", "manage_users"))
  assert_false(AccessControlManager::has_permission(access_manager, "operator", "modify_config"))
  
  // Analyst should have read-only permissions
  assert_true(AccessControlManager::has_permission(access_manager, "analyst", "read_metrics"))
  assert_false(AccessControlManager::has_permission(access_manager, "analyst", "write_metrics"))
  assert_true(AccessControlManager::has_permission(access_manager, "analyst", "read_logs"))
  assert_false(AccessControlManager::has_permission(access_manager, "analyst", "delete_logs"))
  assert_false(AccessControlManager::has_permission(access_manager, "analyst", "manage_users"))
  assert_false(AccessControlManager::has_permission(access_manager, "analyst", "modify_config"))
  
  // Test resource-based access control
  let resource_access = AccessControlManager::can_access_resource(access_manager, "admin", "telemetry.metrics", "read")
  assert_true(resource_access)
  
  let operator_resource_access = AccessControlManager::can_access_resource(access_manager, "operator", "system.users", "write")
  assert_false(operator_resource_access)
  
  // Test attribute-based access control (ABAC)
  let abac_context = AbacContext::new()
  AbacContext::add_attribute(abac_context, "user.department", "operations")
  AbacContext::add_attribute(abac_context, "resource.sensitivity", "high")
  AbacContext::add_attribute(abac_context, "time_of_day", "09:00")
  AbacContext::add_attribute(abac_context, "location", "office")
  
  // Define ABAC policy
  let policy = AbacPolicy::new("high_sensitivity_access")
  AbacPolicy::add_rule(policy, AbacRule::new(
    "user.department == 'operations' AND resource.sensitivity == 'high' AND time_of_day >= '09:00' AND time_of_day <= '17:00'"
  ))
  AbacPolicy::set_effect(policy, PolicyEffect::Allow)
  
  AccessControlManager::add_abac_policy(access_manager, policy)
  
  // Test ABAC evaluation
  let abac_result = AccessControlManager::evaluate_abac(access_manager, "operator", "telemetry.logs", abac_context)
  assert_true(abac_result)
  
  // Test time-based restrictions
  AbacContext::add_attribute(abac_context, "time_of_day", "20:00")  // Outside business hours
  let after_hours_result = AccessControlManager::evaluate_abac(access_manager, "operator", "telemetry.logs", abac_context)
  assert_false(after_hours_result)
  
  // Test permission inheritance
  let senior_analyst_role = Role::new("senior_analyst", "Senior Data Analyst")
  Role::inherit_from(senior_analyst_role, "analyst")
  Role::add_permission(senior_analyst_role, write_metrics)
  
  AccessControlManager::add_role(access_manager, senior_analyst_role)
  
  let senior_analyst_user = User::new("senior_analyst", "senior.analyst@example.com")
  User::add_role(senior_analyst_user, "senior_analyst")
  
  AccessControlManager::add_user(access_manager, senior_analyst_user)
  
  // Senior analyst should inherit analyst permissions plus additional ones
  assert_true(AccessControlManager::has_permission(access_manager, "senior_analyst", "read_metrics"))  // Inherited
  assert_true(AccessControlManager::has_permission(access_manager, "senior_analyst", "write_metrics"))  // Added
  assert_true(AccessControlManager::has_permission(access_manager, "senior_analyst", "read_logs"))  // Inherited
  
  // Test permission delegation
  AccessControlManager::delegate_permission(access_manager, "admin", "operator", "read_logs", 3600)  // Delegate for 1 hour
  
  // Verify delegation
  let delegated_permission = AccessControlManager::get_delegated_permissions(access_manager, "operator")
  assert_true(delegated_permission.contains("read_logs"))
  
  // Test delegation expiration (simulate time passing)
  // In a real implementation, time would pass naturally
  AccessControlManager::expire_delegation(access_manager, "operator", "read_logs")
  
  let expired_delegation = AccessControlManager::get_delegated_permissions(access_manager, "operator")
  assert_false(expired_delegation.contains("read_logs"))
}

// Test 4: Audit Logging and Compliance
test "audit logging and compliance" {
  // Create audit manager
  let audit_manager = AuditManager::new()
  
  // Configure audit settings
  AuditManager::enable_logging(audit_manager, true)
  AuditManager::set_retention_period(audit_manager, 365 * 24 * 60 * 60)  // 1 year
  AuditManager::set_log_level(audit_manager, AuditLogLevel::Detail)
  
  // Create audit events
  let login_event = AuditEvent::new(
    "user.login",
    "admin",
    "Authentication",
    "User admin logged in successfully",
    AuditEventSeverity::Info,
    [
      ("ip_address", "192.168.1.100"),
      ("user_agent", "Mozilla/5.0"),
      ("timestamp", "2022-01-01T12:00:00Z")
    ]
  )
  
  let config_change_event = AuditEvent::new(
    "config.change",
    "admin",
    "Configuration",
    "User admin changed system configuration",
    AuditEventSeverity::Warning,
    [
      ("config_key", "service.port"),
      ("old_value", "8080"),
      ("new_value", "9090"),
      ("timestamp", "2022-01-01T12:05:00Z")
    ]
  )
  
  let data_access_event = AuditEvent::new(
    "data.access",
    "analyst",
    "DataAccess",
    "User analyst accessed sensitive telemetry data",
    AuditEventSeverity::Info,
    [
      ("resource", "telemetry.metrics"),
      ("operation", "read"),
      ("record_count", "1000"),
      ("timestamp", "2022-01-01T12:10:00Z")
    ]
  )
  
  let security_event = AuditEvent::new(
    "security.breach",
    "system",
    "Security",
    "Multiple failed login attempts detected",
    AuditEventSeverity::Critical,
    [
      ("source_ip", "10.0.0.50"),
      ("attempt_count", "5"),
      ("timestamp", "2022-01-01T12:15:00Z")
    ]
  )
  
  // Log audit events
  AuditManager::log_event(audit_manager, login_event)
  AuditManager::log_event(audit_manager, config_change_event)
  AuditManager::log_event(audit_manager, data_access_event)
  AuditManager::log_event(audit_manager, security_event)
  
  // Test audit log retrieval
  let all_events = AuditManager::get_events(audit_manager)
  assert_eq(all_events.length(), 4)
  
  // Test filtering by user
  let admin_events = AuditManager::get_events_by_user(audit_manager, "admin")
  assert_eq(admin_events.length(), 2)
  
  // Test filtering by event type
  let security_events = AuditManager::get_events_by_type(audit_manager, "security.breach")
  assert_eq(security_events.length(), 1)
  
  // Test filtering by severity
  let critical_events = AuditManager::get_events_by_severity(audit_manager, AuditEventSeverity::Critical)
  assert_eq(critical_events.length(), 1)
  
  // Test time-based filtering
  let start_time = "2022-01-01T12:05:00Z"
  let end_time = "2022-01-01T12:15:00Z"
  let time_filtered_events = AuditManager::get_events_by_time_range(audit_manager, start_time, end_time)
  assert_eq(time_filtered_events.length(), 2)
  
  // Test audit log search
  let search_results = AuditManager::search_events(audit_manager, "configuration")
  assert_eq(search_results.length(), 1)
  assert_eq(search_results[0].event_type, "config.change")
  
  // Test audit log export
  let csv_export = AuditManager::export_to_csv(audit_manager)
  assert_true(csv_export.contains("user.login"))
  assert_true(csv_export.contains("config.change"))
  assert_true(csv_export.contains("data.access"))
  assert_true(csv_export.contains("security.breach"))
  
  let json_export = AuditManager::export_to_json(audit_manager)
  assert_true(json_export.contains("admin"))
  assert_true(json_export.contains("analyst"))
  
  // Test compliance reporting
  let compliance_report = AuditManager::generate_compliance_report(audit_manager, "2022-01-01", "2022-01-31")
  
  assert_eq(ComplianceReport::period_start(compliance_report), "2022-01-01")
  assert_eq(ComplianceReport::period_end(compliance_report), "2022-01-31")
  assert_eq(ComplianceReport::total_events(compliance_report), 4)
  assert_eq(ComplianceReport::critical_events(compliance_report), 1)
  assert_eq(ComplianceReport::warning_events(compliance_report), 1)
  assert_eq(ComplianceReport::info_events(compliance_report), 2)
  
  // Test GDPR compliance features
  let gdpr_report = AuditManager::generate_gdpr_report(audit_manager, "analyst")
  
  assert_eq(GdprReport::data_subject(gdpr_report), "analyst")
  assert_eq(GdprReport::data_access_events(gdpr_report), 1)
  assert_eq(GdprReport::data_processing_purposes(gdpr_report).length(), 1)
  assert_true(GdprReport::data_processing_purposes(gdpr_report).contains("telemetry analysis"))
  
  // Test audit log integrity
  let initial_hash = AuditManager::calculate_log_hash(audit_manager)
  
  // Add another event
  let new_event = AuditEvent::new(
    "user.logout",
    "admin",
    "Authentication",
    "User admin logged out",
    AuditEventSeverity::Info,
    [
      ("timestamp", "2022-01-01T12:20:00Z")
    ]
  )
  
  AuditManager::log_event(audit_manager, new_event)
  
  let updated_hash = AuditManager::calculate_log_hash(audit_manager)
  assert_ne(initial_hash, updated_hash)
  
  // Test log tampering detection
  let is_tampered = AuditManager::verify_integrity(audit_manager)
  assert_true(is_tampered)
  
  // Test audit log retention
  AuditManager::cleanup_old_logs(audit_manager)
  
  // Verify old logs are cleaned up (in a real implementation, this would remove logs older than retention period)
  let remaining_events = AuditManager::get_events(audit_manager)
  assert_true(remaining_events.length() >= 0)
  
  // Test audit trail for specific resource
  let resource_audit_trail = AuditManager::get_resource_audit_trail(audit_manager, "telemetry.metrics")
  assert_true(resource_audit_trail.length() > 0)
  
  let data_access_trail = resource_audit_trail.find(@(e) e.event_type == "data.access")
  match data_access_trail {
    Some(event) => {
      assert_eq(event.user, "analyst")
      assert_eq(event.resource, "telemetry.metrics")
    }
    None => assert_true(false)
  }
}

// Test 5: Network Security and Communication
test "network security and communication" {
  // Create network security manager
  let security_manager = NetworkSecurityManager::new()
  
  // Configure TLS settings
  let tls_config = TlsConfig::new()
  TlsConfig::set_version(tls_config, TlsVersion::V1_3)
  TlsConfig::set_cipher_suites(tls_config, [
    "TLS_AES_256_GCM_SHA384",
    "TLS_CHACHA20_POLY1305_SHA256",
    "TLS_AES_128_GCM_SHA256"
  ])
  TlsConfig::set_certificate_validation(tls_config, true)
  TlsConfig::set_client_auth(tls_config, ClientAuth::Required)
  
  NetworkSecurityManager::set_tls_config(security_manager, tls_config)
  
  // Test secure connection establishment
  let server = SecureServer::new("0.0.0.0", 8443)
  SecureServer::use_tls(server, tls_config)
  
  let client = SecureClient::new("localhost", 8443)
  SecureClient::use_tls(client, tls_config)
  
  // Test certificate loading
  let server_cert = Certificate::load_from_file("/path/to/server.crt")
  let server_key = PrivateKey::load_from_file("/path/to/server.key")
  let ca_cert = Certificate::load_from_file("/path/to/ca.crt")
  
  SecureServer::set_certificate(server, server_cert)
  SecureServer::set_private_key(server, server_key)
  SecureServer::set_ca_certificate(server, ca_cert)
  
  SecureClient::set_ca_certificate(client, ca_cert)
  
  // Test secure communication
  let test_data = "Secure telemetry data"
  let encrypted_data = NetworkSecurityManager::encrypt_data(security_manager, test_data.to_bytes())
  let decrypted_data = NetworkSecurityManager::decrypt_data(security_manager, encrypted_data)
  let decrypted_string = @bytes_to_string(decrypted_data)
  assert_eq(decrypted_string, test_data)
  
  // Test IP whitelisting
  let ip_whitelist = ["192.168.1.0/24", "10.0.0.0/8", "172.16.0.0/12"]
  NetworkSecurityManager::set_ip_whitelist(security_manager, ip_whitelist)
  
  // Test IP filtering
  assert_true(NetworkSecurityManager::is_ip_allowed(security_manager, "192.168.1.100"))
  assert_true(NetworkSecurityManager::is_ip_allowed(security_manager, "10.0.0.50"))
  assert_false(NetworkSecurityManager::is_ip_allowed(security_manager, "8.8.8.8"))
  assert_false(NetworkSecurityManager::is_ip_allowed(security_manager, "203.0.113.1"))
  
  // Test rate limiting
  let rate_limiter = RateLimiter::new(100, 60)  // 100 requests per minute
  NetworkSecurityManager::set_rate_limiter(security_manager, rate_limiter)
  
  // Test rate limiting
  let client_ip = "192.168.1.100"
  assert_true(NetworkSecurityManager::allow_request(security_manager, client_ip))
  
  // Simulate multiple requests
  for i in 0..=99 {
    NetworkSecurityManager::allow_request(security_manager, client_ip)
  }
  
  // Next request should be rate limited
  assert_false(NetworkSecurityManager::allow_request(security_manager, client_ip))
  
  // Test DDoS protection
  let ddos_protection = DdosProtection::new()
  DdosProtection::set_threshold(ddos_protection, 1000)  // 1000 requests per minute
  DdosProtection::set_block_duration(ddos_protection, 300)  // 5 minutes
  DdosProtection::enable_auto_blocking(ddos_protection, true)
  
  NetworkSecurityManager::set_ddos_protection(security_manager, ddos_protection)
  
  // Simulate DDoS attack
  let attacker_ip = "203.0.113.100"
  for i in 0..=1000 {
    NetworkSecurityManager::allow_request(security_manager, attacker_ip)
  }
  
  // Attacker should be blocked
  assert_false(NetworkSecurityManager::is_ip_allowed(security_manager, attacker_ip))
  
  // Test firewall rules
  let firewall = Firewall::new()
  Firewall::add_rule(firewall, FirewallRule::new(
    "Allow internal traffic",
    "192.168.0.0/16",
    "any",
    "allow"
  ))
  
  Firewall::add_rule(firewall, FirewallRule::new(
    "Block external traffic to admin ports",
    "any",
    "22,23,3389",
    "deny"
  ))
  
  NetworkSecurityManager::set_firewall(security_manager, firewall)
  
  // Test firewall rules
  assert_true(NetworkSecurityManager::check_firewall_rule(security_manager, "192.168.1.100", "8080"))
  assert_false(NetworkSecurityManager::check_firewall_rule(security_manager, "203.0.113.100", "22"))
  
  // Test intrusion detection
  let intrusion_detector = IntrusionDetector::new()
  IntrusionDetector::enable_pattern_matching(intrusion_detector, true)
  IntrusionDetector::enable_anomaly_detection(intrusion_detector, true)
  
  // Add intrusion patterns
  IntrusionDetector::add_pattern(intrusion_detector, IntrusionPattern::new(
    "SQL Injection",
    "union.*select.*from",
    PatternType::Regex,
    Severity::High
  ))
  
  IntrusionDetector::add_pattern(intrusion_detector, IntrusionPattern::new(
    "XSS Attack",
    "<script.*>.*</script>",
    PatternType::Regex,
    Severity::Medium
  ))
  
  NetworkSecurityManager::set_intrusion_detector(security_manager, intrusion_detector)
  
  // Test intrusion detection
  let malicious_request = "SELECT * FROM users WHERE username='admin' UNION SELECT password FROM admin"
  let is_intrusion = NetworkSecurityManager::detect_intrusion(security_manager, malicious_request)
  assert_true(is_intrusion)
  
  let clean_request = "GET /api/metrics HTTP/1.1"
  let is_clean = NetworkSecurityManager::detect_intrusion(security_manager, clean_request)
  assert_false(is_clean)
  
  // Test secure API key management
  let api_key_manager = ApiKeyManager::new()
  let api_key = ApiKeyManager::generate_key(api_key_manager, "telemetry-api")
  
  assert_true(api_key.length() >= 32)
  assert_true(api_key.contains("-"))
  
  // Test API key validation
  assert_true(ApiKeyManager::validate_key(api_key_manager, api_key))
  assert_false(ApiKeyManager::validate_key(api_key_manager, "invalid-key"))
  
  // Test API key permissions
  ApiKeyManager::set_permissions(api_key_manager, api_key, ["read:metrics", "read:logs"])
  assert_true(ApiKeyManager::has_permission(api_key_manager, api_key, "read:metrics"))
  assert_false(ApiKeyManager::has_permission(api_key_manager, api_key, "write:metrics"))
  
  // Test network security metrics
  let security_metrics = NetworkSecurityManager::get_security_metrics(security_manager)
  
  assert_true(SecurityMetrics::total_requests(security_metrics) > 0)
  assert_true(SecurityMetrics::blocked_requests(security_metrics) > 0)
  assert_true(SecurityMetrics::intrusion_attempts(security_metrics) > 0)
  assert_true(SecurityMetrics::active_ip_blocks(security_metrics) > 0)
}

// Test 6: Privacy and Data Protection
test "privacy and data protection" {
  // Create privacy manager
  let privacy_manager = PrivacyManager::new()
  
  // Configure privacy settings
  PrivacyManager::set_data_retention_period(privacy_manager, "telemetry.data", 90 * 24 * 60 * 60)  // 90 days
  PrivacyManager::set_data_retention_period(privacy_manager, "user.logs", 30 * 24 * 60 * 60)  // 30 days
  PrivacyManager::set_anonymization_threshold(privacy_manager, 5)  // Anonymize data with fewer than 5 records
  
  // Test data classification
  let public_data = DataClassification::new("telemetry.metrics", DataSensitivity::Public)
  let internal_data = DataClassification::new("system.performance", DataSensitivity::Internal)
  let confidential_data = DataClassification::new("user.personal_info", DataSensitivity::Confidential)
  let restricted_data = DataClassification::new("security.credentials", DataSensitivity::Restricted)
  
  PrivacyManager::add_classification(privacy_manager, public_data)
  PrivacyManager::add_classification(privacy_manager, internal_data)
  PrivacyManager::add_classification(privacy_manager, confidential_data)
  PrivacyManager::add_classification(privacy_manager, restricted_data)
  
  // Test data access based on classification
  assert_true(PrivacyManager::can_access(privacy_manager, "analyst", "telemetry.metrics"))
  assert_true(PrivacyManager::can_access(privacy_manager, "operator", "system.performance"))
  assert_false(PrivacyManager::can_access(privacy_manager, "analyst", "user.personal_info"))
  assert_false(PrivacyManager::can_access(privacy_manager, "operator", "security.credentials"))
  
  // Test consent management
  let consent_manager = ConsentManager::new()
  
  // Create consent records
  let analytics_consent = Consent::new(
    "user123",
    "analytics",
    ConsentType::Explicit,
    true,
    "2022-01-01T00:00:00Z",
    "User consented to analytics data collection"
  )
  
  let marketing_consent = Consent::new(
    "user123",
    "marketing",
    ConsentType::Explicit,
    false,
    "2022-01-01T00:00:00Z",
    "User declined marketing communications"
  )
  
  ConsentManager::record_consent(consent_manager, analytics_consent)
  ConsentManager::record_consent(consent_manager, marketing_consent)
  
  // Test consent validation
  assert_true(ConsentManager::has_consent(consent_manager, "user123", "analytics"))
  assert_false(ConsentManager::has_consent(consent_manager, "user123", "marketing"))
  
  // Test consent withdrawal
  ConsentManager::withdraw_consent(consent_manager, "user123", "analytics")
  assert_false(ConsentManager::has_consent(consent_manager, "user123", "analytics"))
  
  // Test data anonymization
  let personal_data = [
    {
      "user_id": "user123",
      "name": "John Doe",
      "email": "john.doe@example.com",
      "age": 35,
      "city": "New York"
    },
    {
      "user_id": "user456",
      "name": "Jane Smith",
      "email": "jane.smith@example.com",
      "age": 28,
      "city": "Los Angeles"
    },
    {
      "user_id": "user789",
      "name": "Bob Johnson",
      "email": "bob.johnson@example.com",
      "age": 42,
      "city": "Chicago"
    }
  ]
  
  let anonymized_data = PrivacyManager::anonymize_data(privacy_manager, personal_data)
  
  // Verify direct identifiers are removed or masked
  for record in anonymized_data {
    assert_ne(record.get("user_id"), Some("user123"))
    assert_ne(record.get("user_id"), Some("user456"))
    assert_ne(record.get("user_id"), Some("user789"))
    
    assert_ne(record.get("name"), Some("John Doe"))
    assert_ne(record.get("name"), Some("Jane Smith"))
    assert_ne(record.get("name"), Some("Bob Johnson"))
    
    assert_ne(record.get("email"), Some("john.doe@example.com"))
    assert_ne(record.get("email"), Some("jane.smith@example.com"))
    assert_ne(record.get("email"), Some("bob.johnson@example.com"))
  }
  
  // Test pseudonymization
  let pseudonymized_data = PrivacyManager::pseudonymize_data(privacy_manager, personal_data)
  
  // Verify pseudonyms are consistent
  let user1_records = pseudonymized_data.filter(@(r) r.get("user_id") == Some("user123"))
  let user1_pseudonyms = user1_records.map(@(r) r.get("pseudonym_id"))
  
  assert_true(user1_pseudonyms.all(@(p) p == user1_pseudonyms[0]))
  
  // Test data minimization
  let full_data = {
    "user_id": "user123",
    "name": "John Doe",
    "email": "john.doe@example.com",
    "phone": "+1-555-123-4567",
    "address": "123 Main St, New York, NY 10001",
    "ssn": "123-45-6789",
    "last_login": "2022-01-01T12:00:00Z",
    "preferences": {
      "theme": "dark",
      "notifications": true
    }
  }
  
  let minimized_data = PrivacyManager::minimize_data(privacy_manager, full_data, ["last_login", "preferences"])
  
  // Verify only necessary data is retained
  assert_true(minimized_data.contains("last_login"))
  assert_true(minimized_data.contains("preferences"))
  assert_false(minimized_data.contains("name"))
  assert_false(minimized_data.contains("email"))
  assert_false(minimized_data.contains("phone"))
  assert_false(minimized_data.contains("address"))
  assert_false(minimized_data.contains("ssn"))
  
  // Test right to be forgotten (GDPR Article 17)
  let deletion_request = DataDeletionRequest::new(
    "user123",
    "account_closure",
    "User requested account deletion",
    "2022-01-01T12:00:00Z"
  )
  
  PrivacyManager::process_deletion_request(privacy_manager, deletion_request)
  
  // Verify user data is marked for deletion
  assert_true(PrivacyManager::is_user_data_marked_for_deletion(privacy_manager, "user123"))
  
  // Test data portability (GDPR Article 20)
  let portable_data = PrivacyManager::export_user_data(privacy_manager, "user456")
  
  assert_true(portable_data.contains("user456"))
  assert_true(portable_data.contains("application/json"))
  
  // Test privacy impact assessment
  let pia = PrivacyImpactAssessment::new("new_analytics_feature")
  
  PrivacyImpactAssessment::add_data_type(pia, "user_behavior", "User interaction patterns")
  PrivacyImpactAssessment::add_purpose(pia, "product_improvement", "Improve user experience")
  PrivacyImpactAssessment::add_legal_basis(pia, "legitimate_interest", "Product improvement")
  PrivacyImpactAssessment::add_risk(pia, "re_identification", "Low risk with proper anonymization")
  PrivacyImpactAssessment::add_mitigation(pia, "anonymization", "Apply strong anonymization techniques")
  
  let pia_result = PrivacyManager::conduct_assessment(privacy_manager, pia)
  assert_eq(pia_result.overall_risk, RiskLevel::Low)
  assert_eq(pia_result.approved, true)
  
  // Test privacy policy compliance
  let privacy_policy = PrivacyPolicy::new()
  PrivacyPolicy::add_data_use(privacy_policy, "analytics", "Improve product features")
  PrivacyPolicy::add_data_use(privacy_policy, "personalization", "Customize user experience")
  PrivacyPolicy::add_data_retention(privacy_policy, "analytics", 90)
  PrivacyPolicy::add_data_retention(privacy_policy, "personalization", 365)
  
  let compliance_result = PrivacyManager::check_policy_compliance(privacy_manager, privacy_policy)
  assert_true(compliance_result.is_compliant)
  assert_eq(compliance_result.violations.length(), 0)
}

// Test 7: Security Monitoring and Threat Detection
test "security monitoring and threat detection" {
  // Create security monitoring manager
  let security_monitor = SecurityMonitor::new()
  
  // Configure monitoring settings
  SecurityMonitor::enable_real_time_monitoring(security_monitor, true)
  SecurityMonitor::set_alert_threshold(security_monitor, 5)  // Alert after 5 suspicious events
  SecurityMonitor::set_monitoring_window(security_monitor, 300)  // 5 minutes
  
  // Create threat detection rules
  let brute_force_rule = ThreatRule::new(
    "brute_force_attack",
    "Detect potential brute force attacks",
    [
      ThreatCondition::new("failed_login_count", ">", 5),
      ThreatCondition::new("time_window", "<", 300)
    ],
    ThreatSeverity::High
  )
  
  let data_exfiltration_rule = ThreatRule::new(
    "data_exfiltration",
    "Detect potential data exfiltration",
    [
      ThreatCondition::new("data_download_size", ">", 1000000),  // > 1MB
      ThreatCondition::new("unusual_access_pattern", "=", true)
    ],
    ThreatSeverity::Critical
  )
  
  let privilege_escalation_rule = ThreatRule::new(
    "privilege_escalation",
    "Detect privilege escalation attempts",
    [
      ThreatCondition::new("role_change", "=", true),
      ThreatCondition::new("self_approval", "=", true)
    ],
    ThreatSeverity::Medium
  )
  
  SecurityMonitor::add_threat_rule(security_monitor, brute_force_rule)
  SecurityMonitor::add_threat_rule(security_monitor, data_exfiltration_rule)
  SecurityMonitor::add_threat_rule(security_monitor, privilege_escalation_rule)
  
  // Simulate security events
  let brute_force_events = [
    SecurityEvent::new("failed_login", "user1", "192.168.1.100", "2022-01-01T12:00:00Z"),
    SecurityEvent::new("failed_login", "user1", "192.168.1.100", "2022-01-01T12:01:00Z"),
    SecurityEvent::new("failed_login", "user1", "192.168.1.100", "2022-01-01T12:02:00Z"),
    SecurityEvent::new("failed_login", "user1", "192.168.1.100", "2022-01-01T12:03:00Z"),
    SecurityEvent::new("failed_login", "user1", "192.168.1.100", "2022-01-01T12:04:00Z"),
    SecurityEvent::new("failed_login", "user1", "192.168.1.100", "2022-01-01T12:05:00Z")
  ]
  
  let data_exfiltration_event = SecurityEvent::new(
    "large_download",
    "admin",
    "192.168.1.200",
    "2022-01-01T12:10:00Z",
    [
      ("data_size", "2000000"),
      ("file_type", "csv"),
      ("destination", "external")
    ]
  )
  
  let privilege_escalation_event = SecurityEvent::new(
    "role_change",
    "user2",
    "192.168.1.150",
    "2022-01-01T12:15:00Z",
    [
      ("old_role", "analyst"),
      ("new_role", "admin"),
      ("approved_by", "user2")
    ]
  )
  
  // Process security events
  for event in brute_force_events {
    SecurityMonitor::process_event(security_monitor, event)
  }
  
  SecurityMonitor::process_event(security_monitor, data_exfiltration_event)
  SecurityMonitor::process_event(security_monitor, privilege_escalation_event)
  
  // Check for detected threats
  let detected_threats = SecurityMonitor::get_detected_threats(security_monitor)
  assert_eq(detected_threats.length(), 3)
  
  // Verify brute force threat
  let brute_force_threat = detected_threats.find(@(t) Threat::rule_name(t) == "brute_force_attack")
  match brute_force_threat {
    Some(threat) => {
      assert_eq(Threat::severity(threat), ThreatSeverity::High)
      assert_eq(Threat::source_ip(threat), "192.168.1.100")
      assert_eq(Threat::target_user(threat), "user1")
      assert_eq(Threat::event_count(threat), 6)
    }
    None => assert_true(false)
  }
  
  // Verify data exfiltration threat
  let exfiltration_threat = detected_threats.find(@(t) Threat::rule_name(t) == "data_exfiltration")
  match exfiltration_threat {
    Some(threat) => {
      assert_eq(Threat::severity(threat), ThreatSeverity::Critical)
      assert_eq(Threat::source_ip(threat), "192.168.1.200")
      assert_eq(Threat::target_user(threat), "admin")
    }
    None => assert_true(false)
  }
  
  // Verify privilege escalation threat
  let escalation_threat = detected_threats.find(@(t) Threat::rule_name(t) == "privilege_escalation")
  match escalation_threat {
    Some(threat) => {
      assert_eq(Threat::severity(threat), ThreatSeverity::Medium)
      assert_eq(Threat::source_ip(threat), "192.168.1.150")
      assert_eq(Threat::target_user(threat), "user2")
    }
    None => assert_true(false)
  }
  
  // Test automated response
  let response_manager = ThreatResponseManager::new()
  
  // Configure response actions
  ResponseManager::add_action(response_manager, "brute_force_attack", [
    ResponseAction::BlockIp(3600),  // Block IP for 1 hour
    ResponseAction::AlertAdmin("Brute force attack detected"),
    ResponseAction::RequireMfa(1800)  // Require MFA for 30 minutes
  ])
  
  ResponseManager::add_action(response_manager, "data_exfiltration", [
    ResponseAction::BlockIp(7200),  // Block IP for 2 hours
    ResponseAction::SuspendUser("admin"),
    ResponseAction::AlertSecurityTeam("Potential data exfiltration"),
    ResponseAction::QuarantineData("downloaded_files")
  ])
  
  ResponseManager::add_action(response_manager, "privilege_escalation", [
    ResponseAction::RevokeRoleChange("user2"),
    ResponseAction::AlertAdmin("Privilege escalation attempt"),
    ResponseAction::RequireApproval("role_changes")
  ])
  
  // Process threat responses
  for threat in detected_threats {
    ResponseManager::execute_response(response_manager, threat)
  }
  
  // Verify response actions were executed
  let executed_actions = ResponseManager::get_executed_actions(response_manager)
  assert_true(executed_actions.length() > 0)
  
  // Test security metrics
  let security_metrics = SecurityMonitor::get_metrics(security_monitor)
  
  assert_eq(SecurityMetrics::total_events(security_metrics), 8)
  assert_eq(SecurityMetrics::detected_threats(security_metrics), 3)
  assert_eq(SecurityMetrics::blocked_ips(security_metrics), 2)
  assert_eq(SecurityMetrics::suspended_users(security_metrics), 1)
  
  // Test security dashboard
  let dashboard = SecurityMonitor::generate_dashboard(security_monitor)
  
  assert_true(Dashboard::contains(dashboard, "Threat Overview"))
  assert_true(Dashboard::contains(dashboard, "Recent Security Events"))
  assert_true(Dashboard::contains(dashboard, "Blocked IPs"))
  assert_true(Dashboard::contains(dashboard, "Security Metrics"))
  
  // Test threat intelligence integration
  let threat_intel = ThreatIntelligence::new()
  
  // Add threat intelligence feeds
  ThreatIntelligence::add_feed(threat_intel, "malicious_ips", "https://threatintel.example.com/ips")
  ThreatIntelligence::add_feed(threat_intel, "malicious_domains", "https://threatintel.example.com/domains")
  ThreatIntelligence::add_feed(threat_intel, "vulnerabilities", "https://threatintel.example.com/vulns")
  
  // Update threat intelligence
  ThreatIntelligence::update_feeds(threat_intel)
  
  // Test IP reputation checking
  let ip_reputation = ThreatIntelligence::check_ip_reputation(threat_intel, "192.168.1.100")
  assert_true(ip_reputation.score >= 0)
  assert_true(ip_reputation.score <= 100)
  
  // Test vulnerability scanning
  let vuln_scanner = VulnerabilityScanner::new()
  
  // Scan system for vulnerabilities
  let scan_result = VulnerabilityScanner::scan_system(vuln_scanner, "localhost")
  
  assert_true(scan_result.total_scanned > 0)
  assert_true(scan_result.vulnerabilities_found >= 0)
  
  // Test security reporting
  let security_report = SecurityMonitor::generate_report(security_monitor, "2022-01-01", "2022-01-31")
  
  assert_eq(SecurityReport::period_start(security_report), "2022-01-01")
  assert_eq(SecurityReport::period_end(security_report), "2022-01-31")
  assert_eq(SecurityReport::total_threats(security_report), 3)
  assert_eq(SecurityReport::critical_threats(security_report), 1)
  assert_eq(SecurityReport::high_threats(security_report), 1)
  assert_eq(SecurityReport::medium_threats(security_report), 1)
}

// Test 8: Incident Response and Recovery
test "incident response and recovery" {
  // Create incident response manager
  let incident_manager = IncidentManager::new()
  
  // Configure incident response settings
  IncidentManager::set_severity_threshold(incident_manager, IncidentSeverity::Medium)
  IncidentManager::set_escalation_timeout(incident_manager, 1800)  // 30 minutes
  IncidentManager::set_auto_escalation(incident_manager, true)
  
  // Create incident response plan
  let response_plan = IncidentResponsePlan::new("data_breach")
  
  ResponsePlan::add_step(response_plan, ResponseStep::new(
    1,
    "Containment",
    "Isolate affected systems to prevent further data loss",
    15,  // 15 minutes
    [
      "Disconnect affected servers from network",
      "Change access credentials",
      "Enable additional logging"
    ]
  ))
  
  ResponsePlan::add_step(response_plan, ResponseStep::new(
    2,
    "Investigation",
    "Identify the scope and cause of the breach",
    60,  // 1 hour
    [
      "Analyze system logs",
      "Review access records",
      "Identify affected data"
    ]
  ))
  
  ResponsePlan::add_step(response_plan, ResponseStep::new(
    3,
    "Eradication",
    "Remove the cause of the breach",
    30,  // 30 minutes
    [
      "Patch vulnerabilities",
      "Remove malware",
      "Close unauthorized access points"
    ]
  ))
  
  ResponsePlan::add_step(response_plan, ResponseStep::new(
    4,
    "Recovery",
    "Restore normal operations",
    120,  // 2 hours
    [
      "Restore from backups",
      "Validate system integrity",
      "Monitor for recurrence"
    ]
  ))
  
  IncidentManager::add_response_plan(incident_manager, response_plan)
  
  // Create incident
  let incident = Incident::new(
    "data_breach_001",
    "Customer data breach detected",
    IncidentSeverity::Critical,
    "2022-01-01T12:00:00Z",
    "Unauthorized access to customer database detected"
  )
  
  Incident::add_affected_system(incident, "customer_db")
  Incident::add_affected_system(incident, "api_gateway")
  Incident::add_affected_data(incident, "customer_pii")
  Incident::add_affected_data(incident, "payment_info")
  
  // Create incident team
  let incident_team = IncidentTeam::new("data_breach_team")
  Team::add_member(incident_team, TeamMember::new("security_lead", "Security Lead", "alice@example.com"))
  Team::add_member(incident_team, TeamMember::new("it_lead", "IT Lead", "bob@example.com"))
  Team::add_member(incident_team, TeamMember::new("legal", "Legal Counsel", "carol@example.com"))
  Team::add_member(incident_team, TeamMember::new("comms", "Communications", "dave@example.com"))
  
  Incident::assign_team(incident, incident_team)
  
  // Report incident
  IncidentManager::report_incident(incident_manager, incident)
  
  // Verify incident is recorded
  let incidents = IncidentManager::get_incidents(incident_manager)
  assert_eq(incidents.length(), 1)
  
  let recorded_incident = incidents[0]
  assert_eq(Incident::id(recorded_incident), "data_breach_001")
  assert_eq(Incident::title(recorded_incident), "Customer data breach detected")
  assert_eq(Incident::severity(recorded_incident), IncidentSeverity::Critical)
  assert_eq(Incident::status(recorded_incident), IncidentStatus::Reported)
  
  // Activate incident response
  IncidentManager::activate_response(incident_manager, "data_breach_001")
  
  let activated_incident = IncidentManager::get_incident(incident_manager, "data_breach_001")
  assert_eq(Incident::status(activated_incident), IncidentStatus::Active)
  
  // Execute response plan
  let response_result = IncidentManager::execute_response_plan(incident_manager, "data_breach_001", "data_breach")
  
  match response_result {
    ResponseResult::Success(executed_steps) => {
      assert_eq(executed_steps.length(), 4)
      
      // Verify containment step
      let containment_step = executed_steps.find(@(s) ResponseStep::name(s) == "Containment")
      match containment_step {
        Some(step) => {
          assert_eq(ResponseStep::status(step), StepStatus::Completed)
          assert_true(ResponseStep::completion_time(step) > 0)
        }
        None => assert_true(false)
      }
    }
    ResponseResult::Failed(error) => assert_true(false)
  }
  
  // Test incident escalation
  IncidentManager::escalate_incident(incident_manager, "data_breach_001", "C-level notification required")
  
  let escalated_incident = IncidentManager::get_incident(incident_manager, "data_breach_001")
  assert_eq(Incident::escalation_level(escalated_incident), 1)
  assert_true(Incident::escalation_reason(escalated_incident).contains("C-level notification"))
  
  // Test incident communication
  let communication_manager = CommunicationManager::new()
  
  // Create communication templates
  let internal_template = CommunicationTemplate::new(
    "internal_incident",
    "Incident Notification - {incident_title}",
    "An incident has been reported: {incident_title}\n\nSeverity: {severity}\nDescription: {description}\nNext steps: {next_steps}"
  )
  
  let external_template = CommunicationTemplate::new(
    "external_incident",
    "Service Update",
    "We are investigating an issue with our services. We apologize for any inconvenience and will provide updates as soon as possible."
  )
  
  CommunicationManager::add_template(communication_manager, internal_template)
  CommunicationManager::add_template(communication_manager, external_template)
  
  // Send internal notification
  let internal_notification = CommunicationManager::create_notification(
    communication_manager,
    "internal_incident",
    [
      ("incident_title", "Customer data breach detected"),
      ("severity", "Critical"),
      ("description", "Unauthorized access to customer database detected"),
      ("next_steps", "Containment, Investigation, Eradication, Recovery")
    ]
  )
  
  CommunicationManager::send_to_team(communication_manager, internal_notification, incident_team)
  
  // Send external notification
  let external_notification = CommunicationManager::create_notification(
    communication_manager,
    "external_incident",
    []
  )
  
  CommunicationManager::send_to_customers(communication_manager, external_notification)
  
  // Verify communications were sent
  let sent_communications = CommunicationManager::get_sent_communications(communication_manager)
  assert_true(sent_communications.length() >= 2)
  
  // Test incident resolution
  IncidentManager::resolve_incident(incident_manager, "data_breach_001", "All systems secured and monitored")
  
  let resolved_incident = IncidentManager::get_incident(incident_manager, "data_breach_001")
  assert_eq(Incident::status(resolved_incident), IncidentStatus::Resolved)
  assert_true(Incident::resolution_summary(resolved_incident).contains("All systems secured"))
  
  // Test post-incident review
  let review = PostIncidentReview::new("data_breach_001")
  
  Review::add_finding(review, ReviewFinding::new(
    "Root Cause",
    "Unpatched vulnerability in customer database",
    FindingSeverity::High
  ))
  
  Review::add_finding(review, ReviewFinding::new(
    "Response Time",
    "Initial response took 45 minutes, exceeding the 30-minute target",
    FindingSeverity::Medium
  ))
  
  Review::add_recommendation(review, ReviewRecommendation::new(
    "Implement automated vulnerability scanning",
    "Set up weekly automated scans to identify vulnerabilities promptly",
    RecommendationPriority::High,
    "2022-01-15"
  ))
  
  Review::add_recommendation(review, ReviewRecommendation::new(
    "Improve incident response training",
    "Conduct quarterly incident response drills",
    RecommendationPriority::Medium,
    "2022-02-01"
  ))
  
  IncidentManager::add_post_incident_review(incident_manager, review)
  
  // Test incident metrics
  let incident_metrics = IncidentManager::get_metrics(incident_manager)
  
  assert_eq(IncidentMetrics::total_incidents(incident_metrics), 1)
  assert_eq(IncidentMetrics::critical_incidents(incident_metrics), 1)
  assert_eq(IncidentMetrics::resolved_incidents(incident_metrics), 1)
  assert_true(IncidentMetrics::average_resolution_time(incident_metrics) > 0)
  
  // Test incident report generation
  let incident_report = IncidentManager::generate_report(incident_manager, "data_breach_001")
  
  assert_true(IncidentReport::contains(incident_report, "Customer data breach detected"))
  assert_true(IncidentReport::contains(incident_report, "Critical"))
  assert_true(IncidentReport::contains(incident_report, "Post-Incident Review"))
  assert_true(IncidentReport::contains(incident_report, "Recommendations"))
}

// Test 9: Security Compliance and Certifications
test "security compliance and certifications" {
  // Create compliance manager
  let compliance_manager = ComplianceManager::new()
  
  // Configure compliance frameworks
  let gdpr_framework = ComplianceFramework::new("GDPR", "General Data Protection Regulation")
  Framework::add_requirement(gdpr_framework, ComplianceRequirement::new(
    "data_protection_by_design",
    "Implement data protection measures by design and by default",
    RequirementCategory::Technical,
    RequirementPriority::Mandatory
  ))
  
  Framework::add_requirement(gdpr_framework, ComplianceRequirement::new(
    "data_subject_rights",
    "Respect data subject rights including access, rectification, and erasure",
    RequirementCategory::Process,
    RequirementPriority::Mandatory
  ))
  
  Framework::add_requirement(gdpr_framework, ComplianceRequirement::new(
    "breach_notification",
    "Notify authorities of data breaches within 72 hours",
    RequirementCategory::Process,
    RequirementPriority::Mandatory
  ))
  
  let iso27001_framework = ComplianceFramework::new("ISO27001", "Information Security Management")
  Framework::add_requirement(iso27001_framework, ComplianceRequirement::new(
    "information_security_policy",
    "Maintain a comprehensive information security policy",
    RequirementCategory::Governance,
    RequirementPriority::Mandatory
  ))
  
  Framework::add_requirement(iso27001_framework, ComplianceRequirement::new(
    "risk_assessment",
    "Conduct regular information security risk assessments",
    RequirementCategory::Risk,
    RequirementPriority::Mandatory
  ))
  
  Framework::add_requirement(iso27001_framework, ComplianceRequirement::new(
    "access_control",
    "Implement appropriate access controls",
    RequirementCategory::Technical,
    RequirementPriority::Mandatory
  ))
  
  let pci_dss_framework = ComplianceFramework::new("PCI_DSS", "Payment Card Industry Data Security Standard")
  Framework::add_requirement(pci_dss_framework, ComplianceRequirement::new(
    "network_security",
    "Install and maintain network security controls",
    RequirementCategory::Technical,
    RequirementPriority::Mandatory
  ))
  
  Framework::add_requirement(pci_dss_framework, ComplianceRequirement::new(
    "data_protection",
    "Protect cardholder data",
    RequirementCategory::Technical,
    RequirementPriority::Mandatory
  ))
  
  Framework::add_requirement(pci_dss_framework, ComplianceRequirement::new(
    "vulnerability_management",
    "Maintain a vulnerability management program",
    RequirementCategory::Process,
    RequirementPriority::Mandatory
  ))
  
  ComplianceManager::add_framework(compliance_manager, gdpr_framework)
  ComplianceManager::add_framework(compliance_manager, iso27001_framework)
  ComplianceManager::add_framework(compliance_manager, pci_dss_framework)
  
  // Create compliance controls
  let encryption_control = ComplianceControl::new(
    "encryption_at_rest",
    "Encrypt sensitive data at rest",
    "AES-256 encryption is used for all sensitive data",
    ControlStatus::Implemented
  )
  
  encryption_control.add_fulfillment("data_protection_by_design", "gdpr")
  encryption_control.add_fulfillment("data_protection", "pci_dss")
  
  let access_control = ComplianceControl::new(
    "role_based_access",
    "Implement role-based access control",
    "Users are assigned roles based on job responsibilities",
    ControlStatus::Implemented
  )
  
  access_control.add_fulfillment("access_control", "iso27001")
  
  let incident_response_control = ComplianceControl::new(
    "incident_response_plan",
    "Maintain an incident response plan",
    "Comprehensive incident response procedures are documented and tested",
    ControlStatus::Implemented
  )
  
  incident_response_control.add_fulfillment("risk_assessment", "iso27001")
  
  ComplianceManager::add_control(compliance_manager, encryption_control)
  ComplianceManager::add_control(compliance_manager, access_control)
  ComplianceManager::add_control(compliance_manager, incident_response_control)
  
  // Conduct compliance assessment
  let assessment = ComplianceManager::conduct_assessment(compliance_manager)
  
  // Verify assessment results
  assert_eq(Assessment::framework_count(assessment), 3)
  assert_eq(Assessment::requirement_count(assessment), 9)
  assert_eq(Assessment::control_count(assessment), 3)
  
  // Check framework compliance
  let gdpr_compliance = Assessment::get_framework_compliance(assessment, "GDPR")
  assert_eq(gdpr_compliance.compliant_requirements, 2)
  assert_eq(gdpr_compliance.total_requirements, 3)
  assert_eq(gdpr_compliance.compliance_percentage, 66.7)
  
  let iso27001_compliance = Assessment::get_framework_compliance(assessment, "ISO27001")
  assert_eq(iso27001_compliance.compliant_requirements, 3)
  assert_eq(iso27001_compliance.total_requirements, 3)
  assert_eq(iso27001_compliance.compliance_percentage, 100.0)
  
  let pci_dss_compliance = Assessment::get_framework_compliance(assessment, "PCI_DSS")
  assert_eq(pci_dss_compliance.compliant_requirements, 1)
  assert_eq(pci_dss_compliance.total_requirements, 3)
  assert_eq(pci_dss_compliance.compliance_percentage, 33.3)
  
  // Generate compliance report
  let compliance_report = ComplianceManager::generate_report(compliance_manager, assessment)
  
  assert_true(ComplianceReport::contains(compliance_report, "GDPR"))
  assert_true(ComplianceReport::contains(compliance_report, "ISO27001"))
  assert_true(ComplianceReport::contains(compliance_report, "PCI_DSS"))
  assert_true(ComplianceReport::contains(compliance_report, "Compliance Score"))
  
  // Test evidence collection
  let evidence_manager = EvidenceManager::new()
  
  // Add evidence items
  let encryption_evidence = Evidence::new(
    "encryption_evidence_001",
    "Encryption configuration documentation",
    EvidenceType::Document,
    "2022-01-01T00:00:00Z",
    "john@example.com"
  )
  
  encryption_evidence.add_fulfillment("encryption_at_rest")
  
  let access_control_evidence = Evidence::new(
    "access_control_evidence_001",
    "Access control policy",
    EvidenceType::Document,
    "2022-01-01T00:00:00Z",
    "jane@example.com"
  )
  
  access_control_evidence.add_fulfillment("role_based_access")
  
  let audit_evidence = Evidence::new(
    "audit_evidence_001",
    "Access control audit logs",
    EvidenceType::Log,
    "2022-01-01T00:00:00Z",
    "system@example.com"
  )
  
  audit_evidence.add_fulfillment("role_based_access")
  
  EvidenceManager::add_evidence(evidence_manager, encryption_evidence)
  EvidenceManager::add_evidence(evidence_manager, access_control_evidence)
  EvidenceManager::add_evidence(evidence_manager, audit_evidence)
  
  // Link evidence to controls
  ComplianceManager::link_evidence(compliance_manager, "encryption_at_rest", "encryption_evidence_001")
  ComplianceManager::link_evidence(compliance_manager, "role_based_access", "access_control_evidence_001")
  ComplianceManager::link_evidence(compliance_manager, "role_based_access", "audit_evidence_001")
  
  // Verify evidence linkage
  let encryption_control_evidence = ComplianceManager::get_control_evidence(compliance_manager, "encryption_at_rest")
  assert_eq(encryption_control_evidence.length(), 1)
  assert_eq(encryption_control_evidence[0], "encryption_evidence_001")
  
  let access_control_evidence = ComplianceManager::get_control_evidence(compliance_manager, "role_based_access")
  assert_eq(access_control_evidence.length(), 2)
  
  // Test certification preparation
  let certification_manager = CertificationManager::new()
  
  // Prepare for ISO27001 certification
  let iso_certification = CertificationManager::prepare_certification(
    certification_manager,
    "ISO27001",
    "2022-06-01",
    "British Standards Institution (BSI)"
  )
  
  Certification::add_requirement(iso_certification, CertificationRequirement::new(
    "stage1_audit",
    "Complete Stage 1 audit",
    "2022-04-01",
    RequirementStatus::NotStarted
  ))
  
  Certification::add_requirement(iso_certification, CertificationRequirement::new(
    "stage2_audit",
    "Complete Stage 2 audit",
    "2022-05-01",
    RequirementStatus::NotStarted
  ))
  
  Certification::add_requirement(iso_certification, CertificationRequirement::new(
    "certification_issuance",
    "Receive certification",
    "2022-06-01",
    RequirementStatus::NotStarted
  ))
  
  // Test compliance monitoring
  let monitoring_manager = ComplianceMonitoringManager::new()
  
  // Set up continuous monitoring
  MonitoringManager::enable_continuous_monitoring(monitoring_manager, true)
  MonitoringManager::set_monitoring_frequency(monitoring_manager, 7 * 24 * 60 * 60)  // Weekly
  
  // Create monitoring rules
  let encryption_monitoring_rule = MonitoringRule::new(
    "encryption_monitoring",
    "Monitor encryption status",
    "SELECT * FROM system_config WHERE encryption_enabled = false",
    Severity::High
  )
  
  let access_monitoring_rule = MonitoringRule::new(
    "access_monitoring",
    "Monitor unauthorized access attempts",
    "SELECT * FROM audit_logs WHERE action = 'denied'",
    Severity::Medium
  )
  
  MonitoringManager::add_rule(monitoring_manager, encryption_monitoring_rule)
  MonitoringManager::add_rule(monitoring_manager, access_monitoring_rule)
  
  // Run compliance monitoring
  let monitoring_results = MonitoringManager::run_checks(monitoring_manager)
  
  // Verify monitoring results
  assert_true(monitoring_results.total_checks >= 0)
  assert_true(monitoring_results.passed_checks >= 0)
  assert_true(monitoring_results.failed_checks >= 0)
  
  // Generate compliance dashboard
  let compliance_dashboard = ComplianceManager::generate_dashboard(compliance_manager)
  
  assert_true(Dashboard::contains(compliance_dashboard, "Compliance Overview"))
  assert_true(Dashboard::contains(compliance_dashboard, "Framework Compliance"))
  assert_true(Dashboard::contains(compliance_dashboard, "Control Status"))
  assert_true(Dashboard::contains(compliance_dashboard, "Recent Findings"))
}

// Test 10: Security Testing and Vulnerability Management
test "security testing and vulnerability management" {
  // Create security testing manager
  let testing_manager = SecurityTestingManager::new()
  
  // Configure testing settings
  TestingManager::set_scan_frequency(testing_manager, 7 * 24 * 60 * 60)  // Weekly
  TestingManager::set_vulnerability_threshold(testing_manager, Severity::Medium)
  TestingManager::enable_automated_remediation(testing_manager, false)
  
  // Create test suites
  let penetration_test_suite = TestSuite::new("penetration_testing")
  TestSuite::add_test(penetration_test_suite, SecurityTest::new(
    "sql_injection_test",
    "Test for SQL injection vulnerabilities",
    TestType::Automated,
    ["api.endpoints"]
  ))
  
  TestSuite::add_test(penetration_test_suite, SecurityTest::new(
    "xss_test",
    "Test for cross-site scripting vulnerabilities",
    TestType::Automated,
    ["web.application"]
  ))
  
  TestSuite::add_test(penetration_test_suite, SecurityTest::new(
    "authentication_bypass_test",
    "Test for authentication bypass vulnerabilities",
    TestType::Manual,
    ["authentication.system"]
  ))
  
  let vulnerability_scan_suite = TestSuite::new("vulnerability_scanning")
  TestSuite::add_test(vulnerability_scan_suite, SecurityTest::new(
    "network_scan",
    "Scan network for open ports and services",
    TestType::Automated,
    ["network.infrastructure"]
  ))
  
  TestSuite::add_test(vulnerability_scan_suite, SecurityTest::new(
    "dependency_scan",
    "Scan dependencies for known vulnerabilities",
    TestType::Automated,
    ["software.dependencies"]
  ))
  
  TestSuite::add_test(vulnerability_scan_suite, SecurityTest::new(
    "configuration_scan",
    "Scan system configurations for security issues",
    TestType::Automated,
    ["system.configuration"]
  ))
  
  TestingManager::add_test_suite(testing_manager, penetration_test_suite)
  TestingManager::add_test_suite(testing_manager, vulnerability_scan_suite)
  
  // Run penetration tests
  let penetration_results = TestingManager::run_test_suite(testing_manager, "penetration_testing")
  
  // Verify penetration test results
  assert_eq(penetration_results.total_tests, 3)
  assert_eq(penetration_results.passed_tests, 2)
  assert_eq(penetration_results.failed_tests, 1)
  
  // Verify specific test results
  let sql_injection_result = penetration_results.get_test_result("sql_injection_test")
  assert_eq(sql_injection_result.status, TestStatus::Passed)
  assert_eq(sql_injection_result.severity, Severity::Low)
  
  let xss_result = penetration_results.get_test_result("xss_test")
  assert_eq(xss_result.status, TestStatus::Passed)
  assert_eq(xss_result.severity, Severity::Low)
  
  let auth_bypass_result = penetration_results.get_test_result("authentication_bypass_test")
  assert_eq(auth_bypass_result.status, TestStatus::Failed)
  assert_eq(auth_bypass_result.severity, Severity::High)
  
  // Run vulnerability scans
  let vulnerability_results = TestingManager::run_test_suite(testing_manager, "vulnerability_scanning")
  
  // Verify vulnerability scan results
  assert_eq(vulnerability_results.total_tests, 3)
  assert_eq(vulnerability_results.passed_tests, 1)
  assert_eq(vulnerability_results.failed_tests, 2)
  
  // Create vulnerability manager
  let vulnerability_manager = VulnerabilityManager::new()
  
  // Process test results and create vulnerabilities
  for test_result in penetration_results.get_all_results() {
    if test_result.status == TestStatus::Failed {
      let vulnerability = Vulnerability::new(
        "vuln_" + @uuid(),
        test_result.test_name,
        test_result.description,
        test_result.severity,
        VulnerabilityType::SecurityFlaw,
        test_result.affected_components
      )
      
      VulnerabilityManager::add_vulnerability(vulnerability_manager, vulnerability)
    }
  }
  
  for test_result in vulnerability_results.get_all_results() {
    if test_result.status == TestStatus::Failed {
      let vulnerability = Vulnerability::new(
        "vuln_" + @uuid(),
        test_result.test_name,
        test_result.description,
        test_result.severity,
        VulnerabilityType::ConfigurationIssue,
        test_result.affected_components
      )
      
      VulnerabilityManager::add_vulnerability(vulnerability_manager, vulnerability)
    }
  }
  
  // Verify vulnerabilities were created
  let vulnerabilities = VulnerabilityManager::get_vulnerabilities(vulnerability_manager)
  assert_eq(vulnerabilities.length(), 3)
  
  // Test vulnerability prioritization
  let prioritized_vulnerabilities = VulnerabilityManager::prioritize(vulnerability_manager)
  
  // Highest severity should be first
  assert_eq(prioritized_vulnerabilities[0].severity, Severity::High)
  
  // Test vulnerability remediation
  let auth_bypass_vuln = vulnerabilities.find(@(v) v.title == "authentication_bypass_test")
  match auth_bypass_vuln {
    Some(vuln) => {
      let remediation = Remediation::new(
        "fix_auth_bypass",
        "Implement proper authentication checks",
        5,  // 5 days
        RemediationType::CodeFix
      )
      
      VulnerabilityManager::assign_remediation(vulnerability_manager, vuln.id, remediation)
      
      // Update vulnerability status
      VulnerabilityManager::update_status(vulnerability_manager, vuln.id, VulnerabilityStatus::InProgress)
    }
    None => assert_true(false)
  }
  
  // Test vulnerability tracking
  let vulnerability_metrics = VulnerabilityManager::get_metrics(vulnerability_manager)
  
  assert_eq(VulnerabilityMetrics::total_vulnerabilities(vulnerability_metrics), 3)
  assert_eq(VulnerabilityMetrics::critical_vulnerabilities(vulnerability_metrics), 0)
  assert_eq(VulnerabilityMetrics::high_vulnerabilities(vulnerability_metrics), 1)
  assert_eq(VulnerabilityMetrics::medium_vulnerabilities(vulnerability_metrics), 2)
  assert_eq(VulnerabilityMetrics::in_progress_vulnerabilities(vulnerability_metrics), 1)
  
  // Test security code review
  let code_review_manager = CodeReviewManager::new()
  
  // Create code review rules
  CodeReviewManager::add_rule(code_review_manager, CodeReviewRule::new(
    "hardcoded_credentials",
    "Check for hardcoded credentials",
    "password|secret|token|key",
    RuleType::Pattern,
    Severity::Critical
  ))
  
  CodeReviewManager::add_rule(code_review_manager, CodeReviewRule::new(
    "sql_injection",
    "Check for SQL injection vulnerabilities",
    "executeQuery|rawQuery",
    RuleType::Pattern,
    Severity::High
  ))
  
  CodeReviewManager::add_rule(code_review_manager, CodeReviewRule::new(
    "input_validation",
    "Check for input validation",
    "request\.getParameter|request\.getHeader",
    RuleType::Pattern,
    Severity::Medium
  ))
  
  // Review code
  let code_sample = "
    public class UserService {
      private String password = \"admin123\";  // Hardcoded password
      private String dbUrl = \"jdbc:mysql://localhost/users\";
      
      public User getUser(String userId) {
        String query = \"SELECT * FROM users WHERE id = \" + userId;  // SQL injection
        return database.executeQuery(query);
      }
      
      public void updateUser(String userId, String email) {
        String userEmail = request.getParameter(\"email\");  // No input validation
        // Update user logic
      }
    }
  "
  
  let code_review_results = CodeReviewManager::review_code(code_review_manager, code_sample)
  
  // Verify code review results
  assert_eq(code_review_results.total_issues, 3)
  assert_eq(code_review_results.critical_issues, 1)
  assert_eq(code_review_results.high_issues, 1)
  assert_eq(code_review_results.medium_issues, 1)
  
  // Test security configuration review
  let config_review_manager = ConfigReviewManager::new()
  
  // Add configuration review rules
  ConfigReviewManager::add_rule(config_review_manager, ConfigReviewRule::new(
    "default_passwords",
    "Check for default passwords",
    "admin|password|123456",
    RuleType::Pattern,
    Severity::Critical
  ))
  
  ConfigReviewManager::add_rule(config_review_manager, ConfigReviewRule::new(
    "weak_encryption",
    "Check for weak encryption algorithms",
    "DES|RC4|MD5|SHA1",
    RuleType::Pattern,
    Severity::High
  ))
  
  ConfigReviewManager::add_rule(config_review_manager, ConfigReviewRule::new(
    "unnecessary_services",
    "Check for unnecessary services",
    "telnet|ftp|rsh",
    RuleType::Pattern,
    Severity::Medium
  ))
  
  // Review configuration
  let config_sample = "
    # Database Configuration
    db.username=admin
    db.password=password123
    db.encryption=DES
    
    # Network Services
    services.enabled=telnet,ftp,ssh,http
    
    # Security Settings
    ssl.protocol=TLSv1.0
    ssl.cipher=RC4-MD5
  "
  
  let config_review_results = ConfigReviewManager::review_config(config_review_manager, config_sample)
  
  // Verify configuration review results
  assert_eq(config_review_results.total_issues, 6)
  assert_eq(config_review_results.critical_issues, 2)
  assert_eq(config_review_results.high_issues, 3)
  assert_eq(config_review_results.medium_issues, 1)
  
  // Generate security testing report
  let testing_report = TestingManager::generate_report(testing_manager)
  
  assert_true(TestingReport::contains(testing_report, "Penetration Testing"))
  assert_true(TestingReport::contains(testing_report, "Vulnerability Scanning"))
  assert_true(TestingReport::contains(testing_report, "Code Review"))
  assert_true(TestingReport::contains(testing_report, "Configuration Review"))
  assert_true(TestingReport::contains(testing_report, "Remediation Plan"))
}
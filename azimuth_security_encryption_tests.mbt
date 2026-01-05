// Azimuth Security and Encryption Test Suite
// This file contains tests for security features and encryption functionality

// Test 1: Basic Cryptographic Operations
test "basic cryptographic operations" {
  // Simple XOR cipher for demonstration
  let xor_encrypt = fn(plaintext: String, key: String) {
    let key_bytes = key.to_byte_array()
    let text_bytes = plaintext.to_byte_array()
    let mut encrypted_bytes = []
    
    for i in 0..text_bytes.length() {
      let key_byte = key_bytes[i % key_bytes.length()]
      let encrypted_byte = text_bytes[i] ^ key_byte
      encrypted_bytes = encrypted_bytes.push(encrypted_byte)
    }
    
    // Convert to hex string for display
    let mut hex_string = ""
    for byte in encrypted_bytes {
      let hex = byte.to_string(16)
      if hex.length() == 1 {
        hex_string = hex_string + "0"
      }
      hex_string = hex_string + hex
    }
    
    hex_string
  }
  
  let xor_decrypt = fn(ciphertext: String, key: String) {
    // Convert hex string to bytes
    let mut encrypted_bytes = []
    for i in 0..(ciphertext.length() / 2) {
      let hex_byte = ciphertext.substring(i * 2, 2)
      let byte_value = hex_byte.to_int(16)
      encrypted_bytes = encrypted_bytes.push(byte_value)
    }
    
    let key_bytes = key.to_byte_array()
    let mut decrypted_bytes = []
    
    for i in 0..encrypted_bytes.length() {
      let key_byte = key_bytes[i % key_bytes.length()]
      let decrypted_byte = encrypted_bytes[i] ^ key_byte
      decrypted_bytes = decrypted_bytes.push(decrypted_byte)
    }
    
    // Convert bytes back to string
    let mut result = ""
    for byte in decrypted_bytes {
      result = result + byte.to_char().to_string()
    }
    
    result
  }
  
  // Simple hash function (demonstration only, not secure)
  let simple_hash = fn(input: String) {
    let mut hash = 5381
    let bytes = input.to_byte_array()
    
    for byte in bytes {
      hash = ((hash << 5) + hash) + byte.to_int()
    }
    
    // Convert to hex string
    let hex_hash = hash.to_string(16)
    if hex_hash.length() < 8 {
      "0".repeat(8 - hex_hash.length()) + hex_hash
    } else {
      hex_hash.substring(0, 8)
    }
  }
  
  // Test encryption and decryption
  let plaintext = "Hello, Azimuth!"
  let key = "secretkey123"
  
  let encrypted = xor_encrypt(plaintext, key)
  assert_true(encrypted.length() > 0)
  assert_false(encrypted.contains("Hello"))  // Should not contain plaintext
  
  let decrypted = xor_decrypt(encrypted, key)
  assert_eq(decrypted, plaintext)
  
  // Test with different key
  let wrong_key = "wrongkey456"
  let wrong_decrypted = xor_decrypt(encrypted, wrong_key)
  assert_false(wrong_decrypted == plaintext)
  
  // Test hash function
  let hash1 = simple_hash("test")
  let hash2 = simple_hash("test")
  let hash3 = simple_hash("different")
  
  assert_eq(hash1, hash2)  // Same input should produce same hash
  assert_false(hash1 == hash3)  // Different input should produce different hash
  
  // Test hash properties
  assert_eq(hash1.length(), 8)  // Fixed length
  assert_true(hash1.match_regex("[0-9a-f]+"))  // Hex characters only
  
  // Test with empty string
  let empty_encrypted = xor_encrypt("", key)
  assert_eq(empty_encrypted, "")
  
  let empty_decrypted = xor_decrypt("", key)
  assert_eq(empty_decrypted, "")
  
  let empty_hash = simple_hash("")
  assert_eq(empty_hash.length(), 8)
}

// Test 2: Data Integrity Verification
test "data integrity verification" {
  // Checksum structure
  type Checksum = {
    algorithm: String,
    value: String
  }
  
  // Data with checksum
  type DataWithChecksum = {
    data: String,
    checksum: Checksum
  }
  
  // Simple checksum algorithm (sum of bytes mod 256)
  let calculate_checksum = fn(data: String) {
    let bytes = data.to_byte_array()
    let mut sum = 0
    
    for byte in bytes {
      sum = sum + byte.to_int()
    }
    
    (sum % 256).to_string(16)
  }
  
  // Verify data integrity
  let verify_integrity = fn(data_with_checksum: DataWithChecksum) {
    match data_with_checksum.checksum.algorithm {
      "simple" => {
        let calculated = calculate_checksum(data_with_checksum.data)
        calculated == data_with_checksum.checksum.value
      }
      _ => false
    }
  }
  
  // Create data with checksum
  let create_with_checksum = fn(data: String, algorithm: String) {
    let checksum_value = match algorithm {
      "simple" => calculate_checksum(data)
      _ => ""
    }
    
    {
      data,
      checksum: {
        algorithm,
        value: checksum_value
      }
    }
  }
  
  // Test checksum calculation
  let data1 = "Hello, world!"
  let checksum1 = calculate_checksum(data1)
  
  let data2 = "Hello, World!"  // Different case
  let checksum2 = calculate_checksum(data2)
  
  assert_false(checksum1 == checksum2)  // Different data should have different checksums
  
  // Test data with checksum
  let data_with_checksum = create_with_checksum(data1, "simple")
  assert_eq(data_with_checksum.data, data1)
  assert_eq(data_with_checksum.checksum.algorithm, "simple")
  assert_eq(data_with_checksum.checksum.value, checksum1)
  
  // Test integrity verification
  assert_true(verify_integrity(data_with_checksum))
  
  // Test with tampered data
  let tampered_data = {
    data_with_checksum |
    data: "Hello, world?"  // Changed last character
  }
  
  assert_false(verify_integrity(tampered_data))
  
  // Test with empty data
  let empty_data_with_checksum = create_with_checksum("", "simple")
  assert_true(verify_integrity(empty_data_with_checksum))
  
  // Test with unsupported algorithm
  let unsupported_checksum = {
    data: data1,
    checksum: {
      algorithm: "unsupported",
      value: "somevalue"
    }
  }
  
  assert_false(verify_integrity(unsupported_checksum))
  
  // Test batch verification
  let data_items = [
    create_with_checksum("item1", "simple"),
    create_with_checksum("item2", "simple"),
    create_with_checksum("item3", "simple")
  ]
  
  let all_valid = data_items.all(fn(item) { verify_integrity(item) })
  assert_true(all_valid)
  
  // Introduce corruption
  let corrupted_items = [
    data_items[0],
    { data_items[1] | data: "corrupted" },
    data_items[2]
  ]
  
  let all_valid_after_corruption = corrupted_items.all(fn(item) { verify_integrity(item) })
  assert_false(all_valid_after_corruption)
  
  // Find corrupted items
  let corrupted = corrupted_items.filter(fn(item) { not(verify_integrity(item)) })
  assert_eq(corrupted.length(), 1)
}

// Test 3: Access Control and Permissions
test "access control and permissions" {
  // Permission types
  enum Permission {
    Read
    Write
    Execute
    Admin
  }
  
  // User role
  type Role = {
    name: String,
    permissions: Array[Permission]
  }
  
  // User
  type User = {
    id: String,
    username: String,
    roles: Array[String]
  }
  
  // Resource
  type Resource = {
    id: String,
    name: String,
    required_permissions: Array[Permission>
  }
  
  // Access control system
  type AccessControl = {
    users: Array[User],
    roles: Array[Role],
    resources: Array[Resource>
  }
  
  // Check if user has permission
  let user_has_permission = fn(ac: AccessControl, user_id: String, permission: Permission) {
    let mut user = None
    for u in ac.users {
      if u.id == user_id {
        user = Some(u)
        break
      }
    }
    
    match user {
      Some(u) => {
        let mut user_permissions = []
        
        // Collect permissions from all user roles
        for role_name in u.roles {
          for role in ac.roles {
            if role.name == role_name {
              user_permissions = user_permissions + role.permissions
              break
            }
          }
        }
        
        // Check if permission exists
        user_permissions.any(fn(p) { 
          match (p, permission) {
            (Permission::Read, Permission::Read) => true
            (Permission::Write, Permission::Write) => true
            (Permission::Execute, Permission::Execute) => true
            (Permission::Admin, Permission::Admin) => true
            _ => false
          }
        })
      }
      None => false
    }
  }
  
  // Check if user can access resource
  let can_access_resource = fn(ac: AccessControl, user_id: String, resource_id: String) {
    let mut resource = None
    for r in ac.resources {
      if r.id == resource_id {
        resource = Some(r)
        break
      }
    }
    
    match resource {
      Some(res) => {
        // Check if user has all required permissions
        res.required_permissions.all(fn(permission) {
          user_has_permission(ac, user_id, permission)
        })
      }
      None => false
    }
  }
  
  // Create test access control system
  let roles = [
    {
      name: "viewer",
      permissions: [Permission::Read]
    },
    {
      name: "editor",
      permissions: [Permission::Read, Permission::Write]
    },
    {
      name: "executor",
      permissions: [Permission::Read, Permission::Execute]
    },
    {
      name: "admin",
      permissions: [Permission::Read, Permission::Write, Permission::Execute, Permission::Admin]
    }
  ]
  
  let users = [
    {
      id: "user1",
      username: "alice",
      roles: ["viewer"]
    },
    {
      id: "user2",
      username: "bob",
      roles: ["editor"]
    },
    {
      id: "user3",
      username: "charlie",
      roles: ["executor"]
    },
    {
      id: "user4",
      username: "diana",
      roles: ["admin"]
    },
    {
      id: "user5",
      username: "eve",
      roles: ["viewer", "editor"]  // Multiple roles
    }
  ]
  
  let resources = [
    {
      id: "doc1",
      name: "Public Document",
      required_permissions: [Permission::Read]
    },
    {
      id: "doc2",
      name: "Editable Document",
      required_permissions: [Permission::Read, Permission::Write]
    },
    {
      id: "script1",
      name: "Executable Script",
      required_permissions: [Permission::Read, Permission::Execute]
    },
    {
      id: "admin_panel",
      name: "Admin Panel",
      required_permissions: [Permission::Admin]
    }
  ]
  
  let ac = {
    users,
    roles,
    resources
  }
  
  // Test permission checks
  assert_true(user_has_permission(ac, "user1", Permission::Read))   // viewer has read
  assert_false(user_has_permission(ac, "user1", Permission::Write)) // viewer doesn't have write
  assert_false(user_has_permission(ac, "user1", Permission::Admin)) // viewer doesn't have admin
  
  assert_true(user_has_permission(ac, "user2", Permission::Read))   // editor has read
  assert_true(user_has_permission(ac, "user2", Permission::Write))  // editor has write
  assert_false(user_has_permission(ac, "user2", Permission::Execute)) // editor doesn't have execute
  
  assert_true(user_has_permission(ac, "user4", Permission::Admin))   // admin has all permissions
  
  // Test multiple roles
  assert_true(user_has_permission(ac, "user5", Permission::Read))   // user5 has viewer and editor
  assert_true(user_has_permission(ac, "user5", Permission::Write))  // user5 has viewer and editor
  assert_false(user_has_permission(ac, "user5", Permission::Execute)) // user5 doesn't have execute
  
  // Test resource access
  assert_true(can_access_resource(ac, "user1", "doc1"))  // viewer can read public doc
  assert_false(can_access_resource(ac, "user1", "doc2")) // viewer can't edit
  assert_false(can_access_resource(ac, "user1", "script1")) // viewer can't execute
  assert_false(can_access_resource(ac, "user1", "admin_panel")) // viewer can't access admin
  
  assert_true(can_access_resource(ac, "user2", "doc1"))  // editor can read public doc
  assert_true(can_access_resource(ac, "user2", "doc2"))  // editor can edit
  assert_false(can_access_resource(ac, "user2", "script1")) // editor can't execute
  assert_false(can_access_resource(ac, "user2", "admin_panel")) // editor can't access admin
  
  assert_true(can_access_resource(ac, "user3", "doc1"))  // executor can read public doc
  assert_false(can_access_resource(ac, "user3", "doc2")) // executor can't edit
  assert_true(can_access_resource(ac, "user3", "script1")) // executor can execute
  assert_false(can_access_resource(ac, "user3", "admin_panel")) // executor can't access admin
  
  assert_true(can_access_resource(ac, "user4", "doc1"))  // admin can access everything
  assert_true(can_access_resource(ac, "user4", "doc2"))
  assert_true(can_access_resource(ac, "user4", "script1"))
  assert_true(can_access_resource(ac, "user4", "admin_panel"))
  
  // Test non-existent user
  assert_false(user_has_permission(ac, "nonexistent", Permission::Read))
  assert_false(can_access_resource(ac, "nonexistent", "doc1"))
  
  // Test non-existent resource
  assert_false(can_access_resource(ac, "user1", "nonexistent"))
}

// Test 4: Secure Token Generation and Validation
test "secure token generation and validation" {
  // Token structure
  type Token = {
    value: String,
    type: String,
    expires_at: Int,
    user_id: String,
    metadata: Array[(String, String)]
  }
  
  // Token validation result
  type TokenValidationResult = {
    valid: Bool,
    user_id: Option[String>,
    reason: Option<String>
  }
  
  // Token store
  type TokenStore = {
    tokens: Array[Token>,
    current_time: Int
  }
  
  // Generate random token (simplified)
  let generate_token_value = fn(length: Int) {
    let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    let mut token = ""
    
    for i in 0..length {
      let index = (i * 7 + 13) % chars.length()  // Simple pseudo-random
      token = token + chars[index].to_string()
    }
    
    token
  }
  
  // Create token
  let create_token = fn(store: TokenStore, token_type: String, user_id: String, ttl_seconds: Int, metadata: Array[(String, String)>) {
    let token_value = generate_token_value(32)
    let expires_at = store.current_time + ttl_seconds
    
    let token = {
      value: token_value,
      type: token_type,
      expires_at,
      user_id,
      metadata
    }
    
    {
      store |
      tokens: store.tokens.push(token)
    }
  }
  
  // Validate token
  let validate_token = fn(store: TokenStore, token_value: String) {
    let mut found_token = None
    
    for token in store.tokens {
      if token.value == token_value {
        found_token = Some(token)
        break
      }
    }
    
    match found_token {
      Some(token) => {
        if token.expires_at < store.current_time {
          {
            valid: false,
            user_id: None,
            reason: Some("Token expired")
          }
        } else {
          {
            valid: true,
            user_id: Some(token.user_id),
            reason: None
          }
        }
      }
      None => {
        {
          valid: false,
          user_id: None,
          reason: Some("Token not found")
        }
      }
    }
  }
  
  // Revoke token
  let revoke_token = fn(store: TokenStore, token_value: String) {
    let mut updated_tokens = []
    
    for token in store.tokens {
      if token.value != token_value {
        updated_tokens = updated_tokens.push(token)
      }
    }
    
    {
      store |
      tokens: updated_tokens
    }
  }
  
  // Clean up expired tokens
  let cleanup_expired_tokens = fn(store: TokenStore) {
    let valid_tokens = store.tokens.filter(fn(token) { token.expires_at >= store.current_time })
    
    {
      store |
      tokens: valid_tokens
    }
  }
  
  // Create token store
  let store = {
    tokens: [],
    current_time: 1640995200  // Base timestamp
  }
  
  // Test token generation
  let store1 = create_token(store, "auth", "user1", 3600, [])
  assert_eq(store1.tokens.length(), 1)
  
  let token = store1.tokens[0]
  assert_eq(token.type, "auth")
  assert_eq(token.user_id, "user1")
  assert_eq(token.expires_at, 1640995200 + 3600)
  assert_eq(token.value.length(), 32)
  
  // Test token validation
  let valid_result = validate_token(store1, token.value)
  assert_true(valid_result.valid)
  assert_eq(valid_result.user_id, Some("user1"))
  assert_eq(valid_result.reason, None)
  
  // Test invalid token
  let invalid_result = validate_token(store1, "invalid_token")
  assert_false(invalid_result.valid)
  assert_eq(invalid_result.user_id, None)
  assert_true(invalid_result.reason.is_some())
  
  // Test expired token
  let future_store = { store1 | current_time: 1640995200 + 7200 }  // 2 hours later
  let expired_result = validate_token(future_store, token.value)
  assert_false(expired_result.valid)
  assert_eq(expired_result.reason, Some("Token expired"))
  
  // Test token revocation
  let store2 = revoke_token(store1, token.value)
  assert_eq(store2.tokens.length(), 0)
  
  let revoked_result = validate_token(store2, token.value)
  assert_false(revoked_result.valid)
  assert_eq(revoked_result.reason, Some("Token not found"))
  
  // Test multiple tokens
  let store3 = create_token(store2, "auth", "user1", 3600, [])
  let store4 = create_token(store3, "refresh", "user1", 7200, [])
  let store5 = create_token(store4, "auth", "user2", 1800, [])
  
  assert_eq(store5.tokens.length(), 3)
  
  // Validate different tokens
  let user1_auth_result = validate_token(store5, store5.tokens[0].value)
  assert_true(user1_auth_result.valid)
  assert_eq(user1_auth_result.user_id, Some("user1"))
  
  let user1_refresh_result = validate_token(store5, store5.tokens[1].value)
  assert_true(user1_refresh_result.valid)
  assert_eq(user1_refresh_result.user_id, Some("user1"))
  
  let user2_auth_result = validate_token(store5, store5.tokens[2].value)
  assert_true(user2_auth_result.valid)
  assert_eq(user2_auth_result.user_id, Some("user2"))
  
  // Test cleanup expired tokens
  let expired_store = { store5 | current_time: 1640995200 + 3600 }
  let cleaned_store = cleanup_expired_tokens(expired_store)
  
  // user2's auth token should be expired (1800 TTL)
  assert_true(cleaned_store.tokens.length() < expired_store.tokens.length())
  
  // Check remaining tokens
  let remaining_user_ids = cleaned_store.tokens.map(fn(token) { token.user_id })
  assert_true(remaining_user_ids.contains("user1"))
  assert_false(remaining_user_ids.contains("user2"))  // Should be cleaned up
}

// Test 5: Security Auditing and Logging
test "security auditing and logging" {
  // Audit event types
  enum AuditEventType {
    LoginSuccess
    LoginFailure
    PermissionDenied
    DataAccess
    DataModification
    SystemError
  }
  
  // Audit event
  type AuditEvent = {
    timestamp: Int,
    event_type: AuditEventType,
    user_id: Option<String>,
    resource: Option<String>,
    details: String,
    ip_address: String,
    user_agent: String
  }
  
  // Audit log
  type AuditLog = {
    events: Array[AuditEvent>,
    current_time: Int
  }
  
  // Create audit event
  let create_audit_event = fn(log: AuditLog, event_type: AuditEventType, user_id: Option<String>, resource: Option<String>, details: String, ip_address: String, user_agent: String) {
    let event = {
      timestamp: log.current_time,
      event_type,
      user_id,
      resource,
      details,
      ip_address,
      user_agent
    }
    
    {
      log |
      events: log.events.push(event)
    }
  }
  
  // Filter events by type
  let filter_by_type = fn(log: AuditLog, event_type: AuditEventType) {
    log.events.filter(fn(event) {
      match (event.event_type, event_type) {
        (AuditEventType::LoginSuccess, AuditEventType::LoginSuccess) => true
        (AuditEventType::LoginFailure, AuditEventType::LoginFailure) => true
        (AuditEventType::PermissionDenied, AuditEventType::PermissionDenied) => true
        (AuditEventType::DataAccess, AuditEventType::DataAccess) => true
        (AuditEventType::DataModification, AuditEventType::DataModification) => true
        (AuditEventType::SystemError, AuditEventType::SystemError) => true
        _ => false
      }
    })
  }
  
  // Filter events by user
  let filter_by_user = fn(log: AuditLog, user_id: String) {
    log.events.filter(fn(event) {
      match event.user_id {
        Some(id) => id == user_id
        None => false
      }
    })
  }
  
  // Filter events by time range
  let filter_by_time_range = fn(log: AuditLog, start_time: Int, end_time: Int) {
    log.events.filter(fn(event) {
      event.timestamp >= start_time and event.timestamp <= end_time
    })
  }
  
  // Get security events (login failures, permission denied, etc.)
  let get_security_events = fn(log: AuditLog) {
    let security_types = [
      AuditEventType::LoginFailure,
      AuditEventType::PermissionDenied,
      AuditEventType::SystemError
    ]
    
    log.events.filter(fn(event) {
      security_types.any_fn(security_type => {
        match (event.event_type, security_type) {
          (AuditEventType::LoginFailure, AuditEventType::LoginFailure) => true
          (AuditEventType::PermissionDenied, AuditEventType::PermissionDenied) => true
          (AuditEventType::SystemError, AuditEventType::SystemError) => true
          _ => false
        }
      })
    })
  }
  
  // Create audit log
  let log = {
    events: [],
    current_time: 1640995200
  }
  
  // Add audit events
  let log1 = create_audit_event(log, AuditEventType::LoginSuccess, Some("user1"), None, "User logged in successfully", "192.168.1.100", "Mozilla/5.0")
  let log2 = create_audit_event(log1, AuditEventType::LoginFailure, Some("user2"), None, "Invalid password", "192.168.1.101", "Mozilla/5.0")
  let log3 = create_audit_event({ log2 | current_time: 1640995210 }, AuditEventType::DataAccess, Some("user1"), Some("doc1"), "Accessed document", "192.168.1.100", "Mozilla/5.0")
  let log4 = create_audit_event({ log3 | current_time: 1640995220 }, AuditEventType::PermissionDenied, Some("user2"), Some("admin_panel"), "Access denied to admin panel", "192.168.1.101", "Mozilla/5.0")
  let log5 = create_audit_event({ log4 | current_time: 1640995230 }, AuditEventType::DataModification, Some("user1"), Some("doc2"), "Modified document", "192.168.1.100", "Mozilla/5.0")
  let log6 = create_audit_event({ log5 | current_time: 1640995240 }, AuditEventType::SystemError, None, None, "Database connection failed", "127.0.0.1", "System")
  
  // Test filtering by type
  let login_events = filter_by_type(log6, AuditEventType::LoginSuccess)
  assert_eq(login_events.length(), 1)
  assert_eq(login_events[0].user_id, Some("user1"))
  
  let failure_events = filter_by_type(log6, AuditEventType::LoginFailure)
  assert_eq(failure_events.length(), 1)
  assert_eq(failure_events[0].user_id, Some("user2"))
  
  // Test filtering by user
  let user1_events = filter_by_user(log6, "user1")
  assert_eq(user1_events.length(), 3)  // Login, data access, data modification
  
  let user2_events = filter_by_user(log6, "user2")
  assert_eq(user2_events.length(), 2)  // Login failure, permission denied
  
  // Test filtering by time range
  let recent_events = filter_by_time_range(log6, 1640995215, 1640995235)
  assert_eq(recent_events.length(), 3)  // Data access, permission denied, data modification
  
  // Test security events
  let security_events = get_security_events(log6)
  assert_eq(security_events.length(), 3)  // Login failure, permission denied, system error
  
  // Verify security event types
  let has_login_failure = security_events.any_fn(event => {
    match event.event_type {
      AuditEventType::LoginFailure => true
      _ => false
    }
  })
  
  let has_permission_denied = security_events.any_fn(event => {
    match event.event_type {
      AuditEventType::PermissionDenied => true
      _ => false
    }
  })
  
  let has_system_error = security_events.any_fn(event => {
    match event.event_type {
      AuditEventType::SystemError => true
      _ => false
    }
  })
  
  assert_true(has_login_failure)
  assert_true(has_permission_denied)
  assert_true(has_system_error)
  
  // Test event properties
  let login_event = filter_by_type(log6, AuditEventType::LoginSuccess)[0]
  assert_eq(login_event.timestamp, 1640995200)
  assert_eq(login_event.ip_address, "192.168.1.100")
  assert_eq(login_event.user_agent, "Mozilla/5.0")
  
  let permission_event = filter_by_type(log6, AuditEventType::PermissionDenied)[0]
  assert_eq(permission_event.user_id, Some("user2"))
  assert_eq(permission_event.resource, Some("admin_panel"))
  assert_eq(permission_event.details, "Access denied to admin panel")
  
  // Test empty log
  let empty_log = { events: [], current_time: 1640995200 }
  let empty_login_events = filter_by_type(empty_log, AuditEventType::LoginSuccess)
  assert_eq(empty_login_events.length(), 0)
  
  let empty_user_events = filter_by_user(empty_log, "user1")
  assert_eq(empty_user_events.length(), 0)
  
  let empty_security_events = get_security_events(empty_log)
  assert_eq(empty_security_events.length(), 0)
}
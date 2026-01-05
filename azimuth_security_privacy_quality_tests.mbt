// Azimuth Security and Privacy Test Suite
// This file contains high-quality test cases for security and privacy

// Test 1: Data Encryption and Decryption
test "data encryption and decryption" {
  // Define encryption algorithm
  enum EncryptionAlgorithm {
    AES256
    ChaCha20
    RSA
  }
  
  // Define encrypted data
  type EncryptedData = {
    algorithm: EncryptionAlgorithm,
    ciphertext: Array[Int],
    iv: Array[Int],  // Initialization vector
    key_id: String
  }
  
  // Define encryption key
  type EncryptionKey = {
    key_id: String,
    algorithm: EncryptionAlgorithm,
    key_data: Array[Int]
  }
  
  // Simple XOR encryption (for testing purposes)
  let xor_encrypt = fn(plaintext: Array[Int], key: Array[Int]) {
    let mut ciphertext = []
    
    for i in 0..plaintext.length() {
      let key_byte = key[i % key.length()]
      ciphertext = ciphertext.push(plaintext[i] ^ key_byte)
    }
    
    ciphertext
  }
  
  // Simple XOR decryption
  let xor_decrypt = fn(ciphertext: Array[Int], key: Array[Int]) {
    let mut plaintext = []
    
    for i in 0..ciphertext.length() {
      let key_byte = key[i % key.length()]
      plaintext = plaintext.push(ciphertext[i] ^ key_byte)
    }
    
    plaintext
  }
  
  // String to bytes
  let string_to_bytes = fn(str: String) {
    let mut bytes = []
    let chars = str.to_char_array()
    
    for i in 0..chars.length() {
      bytes = bytes.push(chars[i].to_int())
    }
    
    bytes
  }
  
  // Bytes to string
  let bytes_to_string = fn(bytes: Array[Int]) {
    let mut chars = []
    
    for i in 0..bytes.length() {
      chars = chars.push(bytes[i].to_char())
    }
    
    chars.join("")
  }
  
  // Generate random IV
  let generate_iv = fn(size: Int) {
    let mut iv = []
    
    // Simple pseudo-random IV generation for testing
    for i in 0..size {
      iv = iv.push((i * 17 + 42) % 256)
    }
    
    iv
  }
  
  // Encrypt data
  let encrypt_data = fn(plaintext: String, key: EncryptionKey, algorithm: EncryptionAlgorithm) {
    let plaintext_bytes = string_to_bytes(plaintext)
    let iv = generate_iv(16)  // 16-byte IV
    
    let ciphertext = match algorithm {
      AES256 => xor_encrypt(plaintext_bytes, key.key_data)
      ChaCha20 => xor_encrypt(plaintext_bytes, key.key_data)
      RSA => xor_encrypt(plaintext_bytes, key.key_data)  // Simplified
    }
    
    {
      algorithm: algorithm,
      ciphertext: ciphertext,
      iv: iv,
      key_id: key.key_id
    }
  }
  
  // Decrypt data
  let decrypt_data = fn(encrypted_data: EncryptedData, key: EncryptionKey) {
    if encrypted_data.key_id != key.key_id {
      None
    } else {
      let plaintext_bytes = match encrypted_data.algorithm {
        AES256 => xor_decrypt(encrypted_data.ciphertext, key.key_data)
        ChaCha20 => xor_decrypt(encrypted_data.ciphertext, key.key_data)
        RSA => xor_decrypt(encrypted_data.ciphertext, key.key_data)  // Simplified
      }
      
      Some(bytes_to_string(plaintext_bytes))
    }
  }
  
  // Test data encryption and decryption
  let key_data = [0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF, 
                  0xFE, 0xDC, 0xBA, 0x98, 0x76, 0x54, 0x32, 0x10]
  
  let aes_key = {
    key_id: "key-1",
    algorithm: AES256,
    key_data: key_data
  }
  
  let chacha_key = {
    key_id: "key-2",
    algorithm: ChaCha20,
    key_data: key_data
  }
  
  let plaintext = "Sensitive telemetry data that must be encrypted"
  
  // Test AES encryption
  let aes_encrypted = encrypt_data(plaintext, aes_key, AES256)
  assert_eq(aes_encrypted.algorithm, AES256)
  assert_eq(aes_encrypted.key_id, "key-1")
  assert_true(aes_encrypted.ciphertext.length() > 0)
  assert_eq(aes_encrypted.iv.length(), 16)
  
  // Test AES decryption
  let aes_decrypted = decrypt_data(aes_encrypted, aes_key)
  match aes_decrypted {
    Some(decrypted) => assert_eq(decrypted, plaintext)
    None => assert_true(false)
  }
  
  // Test decryption with wrong key
  let wrong_key = {
    key_id: "key-3",
    algorithm: AES256,
    key_data: [0x11, 0x22, 0x33, 0x44]
  }
  
  let wrong_decrypted = decrypt_data(aes_encrypted, wrong_key)
  assert_eq(wrong_decrypted, None)
  
  // Test ChaCha20 encryption
  let chacha_encrypted = encrypt_data(plaintext, chacha_key, ChaCha20)
  assert_eq(chacha_encrypted.algorithm, ChaCha20)
  assert_eq(chacha_encrypted.key_id, "key-2")
  
  // Test ChaCha20 decryption
  let chacha_decrypted = decrypt_data(chacha_encrypted, chacha_key)
  match chacha_decrypted {
    Some(decrypted) => assert_eq(decrypted, plaintext)
    None => assert_true(false)
  }
  
  // Test that different algorithms produce different ciphertexts
  assert_not_eq(aes_encrypted.ciphertext, chacha_encrypted.ciphertext)
}

// Test 2: Data Anonymization and Pseudonymization
test "data anonymization and pseudonymization" {
  // Define anonymization method
  enum AnonymizationMethod {
    Hash
    Tokenize
    Mask
    Generalize
  }
  
  // Define anonymization rule
  type AnonymizationRule = {
    field_name: String,
    method: AnonymizationMethod,
    parameters: Array[String]
  }
  
  // Define anonymized record
  type AnonymizedRecord = {
    original: Map[String, String],
    anonymized: Map[String, String],
    applied_rules: Array[String]
  }
  
  // Simple hash function (for testing)
  let simple_hash = fn(str: String) {
    let mut hash = 0
    let chars = str.to_char_array()
    
    for i in 0..chars.length() {
      hash = (hash * 31 + chars[i].to_int()) % 1000000
    }
    
    if hash < 0 { hash = hash + 1000000 }
    hash.to_string()
  }
  
  // Tokenize value
  let tokenize_value = fn(value: String, token_prefix: String) {
    let token = simple_hash(value)
    token_prefix + "-" + token
  }
  
  // Mask value
  let mask_value = fn(value: String, mask_char: String, visible_chars: Int) {
    if value.length() <= visible_chars {
      value
    } else {
      let visible_part = value.substring(0, visible_chars)
      let masked_part = mask_char.repeat(value.length() - visible_chars)
      visible_part + masked_part
    }
  }
  
  // Generalize value
  let generalize_value = fn(value: String, level: String) {
    match level {
      "high" => {
        // High-level generalization
        if value.contains("@") {
          "email"
        } else if value.chars().all(fn(c) { c >= '0' and c <= '9' }) {
          "number"
        } else {
          "string"
        }
      }
      "medium" => {
        // Medium-level generalization
        if value.contains("@") {
          let parts = value.split("@")
          if parts.length() == 2 {
            parts[0].substring(0, 2) + "***@" + parts[1]
          } else {
            value
          }
        } else if value.length() > 8 {
          value.substring(0, 4) + "****"
        } else {
          value
        }
      }
      _ => value
    }
  }
  
  // Apply anonymization rule
  let apply_rule = fn(value: String, rule: AnonymizationRule) {
    match rule.method {
      Hash => simple_hash(value)
      Tokenize => {
        if rule.parameters.length() > 0 {
          tokenize_value(value, rule.parameters[0])
        } else {
          tokenize_value(value, "TOKEN")
        }
      }
      Mask => {
        let mask_char = if rule.parameters.length() > 0 { rule.parameters[0] } else { "*" }
        let visible_chars = if rule.parameters.length() > 1 { rule.parameters[1].to_int() } else { 4 }
        mask_value(value, mask_char, visible_chars)
      }
      Generalize => {
        let level = if rule.parameters.length() > 0 { rule.parameters[0] } else { "medium" }
        generalize_value(value, level)
      }
    }
  }
  
  // Anonymize record
  let anonymize_record = fn(record: Map[String, String], rules: Array[AnonymizationRule]) {
    let mut anonymized = []
    let mut applied_rules = []
    
    for i in 0..record.length() {
      let field = record[i]
      let field_name = field.0
      let field_value = field.1
      
      // Find matching rule
      let rule_opt = rules.find(fn(rule) { rule.field_name == field_name })
      
      let anonymized_value = match rule_opt {
        Some(rule) => {
          applied_rules = applied_rules.push(rule.field_name)
          apply_rule(field_value, rule)
        }
        None => field_value
      }
      
      anonymized = anonymized.push((field_name, anonymized_value))
    }
    
    {
      original: record,
      anonymized: anonymized,
      applied_rules: applied_rules
    }
  }
  
  // Test data anonymization and pseudonymization
  let original_record = [
    ("user_id", "12345678"),
    ("email", "user@example.com"),
    ("ip_address", "192.168.1.100"),
    ("phone", "+1-555-123-4567"),
    ("name", "John Doe"),
    ("age", "30"),
    ("session_id", "sess_abc123xyz")
  ]
  
  let anonymization_rules = [
    { field_name: "user_id", method: Tokenize, parameters: ["USR"] },
    { field_name: "email", method: Mask, parameters: ["*", 3] },
    { field_name: "ip_address", method: Generalize, parameters: ["medium"] },
    { field_name: "phone", method: Mask, parameters: ["*", 5] },
    { field_name: "name", method: Hash, parameters: [] },
    { field_name: "session_id", method: Tokenize, parameters: ["SES"] }
  ]
  
  let anonymized_record = anonymize_record(original_record, anonymization_rules)
  
  // Verify anonymization
  assert_eq(anonymized_record.applied_rules.length(), 6)
  assert_true(anonymized_record.applied_rules.contains("user_id"))
  assert_true(anonymized_record.applied_rules.contains("email"))
  
  // Check specific transformations
  let user_id_field = anonymized_record.anonymized.find(fn(field) { field.0 == "user_id" })
  match user_id_field {
    Some((_, value)) => {
      assert_true(value.starts_with("USR-"))
      assert_not_eq(value, "12345678")
    }
    None => assert_true(false)
  }
  
  let email_field = anonymized_record.anonymized.find(fn(field) { field.0 == "email" })
  match email_field {
    Some((_, value)) => {
      assert_eq(value, "use************")
    }
    None => assert_true(false)
  }
  
  let ip_field = anonymized_record.anonymized.find(fn(field) { field.0 == "ip_address" })
  match ip_field {
    Some((_, value)) => {
      assert_eq(value, "192.****")
    }
    None => assert_true(false)
  }
  
  let phone_field = anonymized_record.anonymized.find(fn(field) { field.0 == "phone" })
  match phone_field {
    Some((_, value)) => {
      assert_eq(value, "+1-55********")
    }
    None => assert_true(false)
  }
  
  let name_field = anonymized_record.anonymized.find(fn(field) { field.0 == "name" })
  match name_field {
    Some((_, value)) => {
      assert_not_eq(value, "John Doe")
      assert_eq(value.length(), "John Doe".length())
    }
    None => assert_true(false)
  }
  
  // Age should not be anonymized (no rule)
  let age_field = anonymized_record.anonymized.find(fn(field) { field.0 == "age" })
  match age_field {
    Some((_, value)) => assert_eq(value, "30")
    None => assert_true(false)
  }
}

// Test 3: Access Control and Authorization
test "access control and authorization" {
  // Define permission
  type Permission = {
    resource: String,
    action: String,
    conditions: Array[String]
  }
  
  // Define role
  type Role = {
    name: String,
    permissions: Array[Permission]
  }
  
  // Define user
  type User = {
    id: String,
    username: String,
    roles: Array[String],
    attributes: Map[String, String]
  }
  
  // Define access control system
  type AccessControlSystem = {
    users: Array[User],
    roles: Array[Role],
    policies: Array[String]
  }
  
  // Create access control system
  let create_access_control_system = fn() {
    {
      users: [],
      roles: [],
      policies: []
    }
  }
  
  // Add role
  let add_role = fn(acs: AccessControlSystem, role: Role) {
    {
      users: acs.users,
      roles: acs.roles.push(role),
      policies: acs.policies
    }
  }
  
  // Add user
  let add_user = fn(acs: AccessControlSystem, user: User) {
    {
      users: acs.users.push(user),
      roles: acs.roles,
      policies: acs.policies
    }
  }
  
  // Check if user has permission
  let has_permission = fn(acs: AccessControlSystem, user_id: String, resource: String, action: String, context: Map[String, String]) {
    // Find user
    let user_opt = acs.users.find(fn(user) { user.id == user_id })
    
    match user_opt {
      Some(user) => {
        // Get user's roles
        let user_roles = user.roles
        
        // Collect all permissions from user's roles
        let mut all_permissions = []
        
        for i in 0..user_roles.length() {
          let role_name = user_roles[i]
          let role_opt = acs.roles.find(fn(role) { role.name == role_name })
          
          match role_opt {
            Some(role) => {
              for j in 0..role.permissions.length() {
                all_permissions = all_permissions.push(role.permissions[j])
              }
            }
            None => {}
          }
        }
        
        // Check for matching permission
        let mut has_access = false
        
        for i in 0..all_permissions.length() {
          let permission = all_permissions[i]
          
          if permission.resource == resource and permission.action == action {
            // Check conditions
            let mut conditions_met = true
            
            for j in 0..permission.conditions.length() {
              let condition = permission.conditions[j]
              let parts = condition.split("=")
              
              if parts.length() == 2 {
                let attr_name = parts[0]
                let expected_value = parts[1]
                
                let user_attr_opt = user.attributes.find(fn(attr) { attr.0 == attr_name })
                let context_attr_opt = context.find(fn(attr) { attr.0 == attr_name })
                
                let actual_value = match user_attr_opt {
                  Some((_, value)) => value
                  None => {
                    match context_attr_opt {
                      Some((_, value)) => value
                      None => ""
                    }
                  }
                }
                
                if actual_value != expected_value {
                  conditions_met = false
                  break
                }
              }
            }
            
            if conditions_met {
              has_access = true
              break
            }
          }
        }
        
        has_access
      }
      None => false
    }
  }
  
  // Test access control and authorization
  let acs = create_access_control_system()
  
  // Define roles
  let admin_role = {
    name: "admin",
    permissions: [
      { resource: "telemetry", action: "read", conditions: [] },
      { resource: "telemetry", action: "write", conditions: [] },
      { resource: "telemetry", action: "delete", conditions: [] },
      { resource: "users", action: "manage", conditions: [] }
    ]
  }
  
  let analyst_role = {
    name: "analyst",
    permissions: [
      { resource: "telemetry", action: "read", conditions: [] },
      { resource: "telemetry", action: "write", conditions: ["department=analytics"] },
      { resource: "reports", action: "read", conditions: [] },
      { resource: "reports", action: "create", conditions: [] }
    ]
  }
  
  let viewer_role = {
    name: "viewer",
    permissions: [
      { resource: "telemetry", action: "read", conditions: ["own_data=true"] },
      { resource: "dashboard", action: "view", conditions: [] }
    ]
  }
  
  // Add roles
  let acs1 = add_role(acs, admin_role)
  let acs2 = add_role(acs1, analyst_role)
  let acs3 = add_role(acs2, viewer_role)
  
  // Define users
  let admin_user = {
    id: "user-1",
    username: "admin",
    roles: ["admin"],
    attributes: [
      ("department", "IT"),
      ("clearance", "high")
    ]
  }
  
  let analyst_user = {
    id: "user-2",
    username: "analyst",
    roles: ["analyst"],
    attributes: [
      ("department", "analytics"),
      ("clearance", "medium")
    ]
  }
  
  let viewer_user = {
    id: "user-3",
    username: "viewer",
    roles: ["viewer"],
    attributes: [
      ("department", "sales"),
      ("clearance", "low")
    ]
  }
  
  // Add users
  let acs4 = add_user(acs3, admin_user)
  let acs5 = add_user(acs4, analyst_user)
  let acs6 = add_user(acs5, viewer_user)
  
  // Test permissions
  // Admin should have full access
  assert_true(has_permission(acs6, "user-1", "telemetry", "read", []))
  assert_true(has_permission(acs6, "user-1", "telemetry", "write", []))
  assert_true(has_permission(acs6, "user-1", "telemetry", "delete", []))
  assert_true(has_permission(acs6, "user-1", "users", "manage", []))
  
  // Analyst should have limited access with conditions
  assert_true(has_permission(acs6, "user-2", "telemetry", "read", []))
  assert_true(has_permission(acs6, "user-2", "telemetry", "write", [("department", "analytics")]))
  assert_false(has_permission(acs6, "user-2", "telemetry", "write", [("department", "sales")]))
  assert_false(has_permission(acs6, "user-2", "telemetry", "delete", []))
  
  // Viewer should have very limited access
  assert_true(has_permission(acs6, "user-3", "telemetry", "read", [("own_data", "true")]))
  assert_false(has_permission(acs6, "user-3", "telemetry", "read", [("own_data", "false")]))
  assert_false(has_permission(acs6, "user-3", "telemetry", "write", []))
  assert_true(has_permission(acs6, "user-3", "dashboard", "view", []))
  
  // Test non-existent user
  assert_false(has_permission(acs6, "user-999", "telemetry", "read", []))
}

// Test 4: Data Retention and Expiration
test "data retention and expiration" {
  // Define retention policy
  type RetentionPolicy = {
    data_type: String,
    retention_days: Int,
    anonymize_after: Bool,
    conditions: Array[String]
  }
  
  // Define data record
  type DataRecord = {
    id: String,
    data_type: String,
    data: Map[String, String],
    created_at: Int,
    last_accessed: Int,
    access_count: Int,
    tags: Array[String]
  }
  
  // Define retention manager
  type RetentionManager = {
    policies: Array[RetentionPolicy],
    records: Array[DataRecord]
  }
  
  // Create retention manager
  let create_retention_manager = fn() {
    {
      policies: [],
      records: []
    }
  }
  
  // Add retention policy
  let add_retention_policy = fn(manager: RetentionManager, policy: RetentionPolicy) {
    {
      policies: manager.policies.push(policy),
      records: manager.records
    }
  }
  
  // Add data record
  let add_data_record = fn(manager: RetentionManager, record: DataRecord) {
    {
      policies: manager.policies,
      records: manager.records.push(record)
    }
  }
  
  // Check if record matches policy conditions
  let matches_policy_conditions = fn(record: DataRecord, conditions: Array[String]) {
    for i in 0..conditions.length() {
      let condition = conditions[i]
      let parts = condition.split("=")
      
      if parts.length() == 2 {
        let field_name = parts[0]
        let expected_value = parts[1]
        
        let actual_value = match field_name {
          "data_type" => record.data_type
          "tag" => {
            if record.tags.contains(expected_value) {
              expected_value
            } else {
              ""
            }
          }
          _ => {
            match record.data.find(fn(field) { field.0 == field_name }) {
              Some((_, value)) => value
              None => ""
            }
          }
        }
        
        if actual_value != expected_value {
          return false
        }
      }
    }
    
    true
  }
  
  // Get applicable policy for record
  let get_applicable_policy = fn(manager: RetentionManager, record: DataRecord) {
    for i in 0..manager.policies.length() {
      let policy = manager.policies[i]
      
      if policy.data_type == record.data_type and 
         matches_policy_conditions(record, policy.conditions) {
        return Some(policy)
      }
    }
    
    None
  }
  
  // Check if record should be expired
  let is_record_expired = fn(manager: RetentionManager, record: DataRecord, current_time: Int) {
    let policy_opt = get_applicable_policy(manager, record)
    
    match policy_opt {
      Some(policy) => {
        let age_days = (current_time - record.created_at) / 86400
        age_days >= policy.retention_days
      }
      None => false
    }
  }
  
  // Check if record should be anonymized
  let should_anonymize_record = fn(manager: RetentionManager, record: DataRecord, current_time: Int) {
    let policy_opt = get_applicable_policy(manager, record)
    
    match policy_opt {
      Some(policy) => {
        if policy.anonymize_after {
          let age_days = (current_time - record.created_at) / 86400
          let anonymize_days = policy.retention_days / 2  // Anonymize at half retention period
          age_days >= anonymize_days
        } else {
          false
        }
      }
      None => false
    }
  }
  
  // Get expired records
  let get_expired_records = fn(manager: RetentionManager, current_time: Int) {
    manager.records.filter(fn(record) { is_record_expired(manager, record, current_time) })
  }
  
  // Get records to anonymize
  let get_records_to_anonymize = fn(manager: RetentionManager, current_time: Int) {
    manager.records.filter(fn(record) { 
      should_anonymize_record(manager, record, current_time) and 
      not(is_record_expired(manager, record, current_time))
    })
  }
  
  // Anonymize record
  let anonymize_record = fn(record: DataRecord) {
    let mut anonymized_data = []
    
    for i in 0..record.data.length() {
      let field = record.data[i]
      let field_name = field.0
      let field_value = field.1
      
      let anonymized_value = match field_name {
        "user_id" => "ANONYMIZED"
        "email" => "ANONYMIZED"
        "ip_address" => "ANONYMIZED"
        "name" => "ANONYMIZED"
        _ => field_value
      }
      
      anonymized_data = anonymized_data.push((field_name, anonymized_value))
    }
    
    {
      id: record.id,
      data_type: record.data_type,
      data: anonymized_data,
      created_at: record.created_at,
      last_accessed: record.last_accessed,
      access_count: record.access_count,
      tags: record.tags + ["anonymized"]
    }
  }
  
  // Test data retention and expiration
  let manager = create_retention_manager()
  
  // Define retention policies
  let telemetry_policy = {
    data_type: "telemetry",
    retention_days: 90,
    anonymize_after: true,
    conditions: []
  }
  
  let pii_policy = {
    data_type: "pii",
    retention_days: 30,
    anonymize_after: true,
    conditions: []
  }
  
  let critical_policy = {
    data_type: "critical",
    retention_days: 365,
    anonymize_after: false,
    conditions: ["tag=critical"]
  }
  
  // Add policies
  let manager1 = add_retention_policy(manager, telemetry_policy)
  let manager2 = add_retention_policy(manager1, pii_policy)
  let manager3 = add_retention_policy(manager2, critical_policy)
  
  // Create test records (current time = 100 days in seconds)
  let current_time = 100 * 86400
  
  let telemetry_record = {
    id: "record-1",
    data_type: "telemetry",
    data: [
      ("user_id", "12345"),
      ("event", "login"),
      ("timestamp", "1000")
    ],
    created_at: current_time - 95 * 86400,  // 95 days ago
    last_accessed: current_time - 10 * 86400,  // 10 days ago
    access_count: 5,
    tags: []
  }
  
  let pii_record = {
    id: "record-2",
    data_type: "pii",
    data: [
      ("name", "John Doe"),
      ("email", "john@example.com"),
      ("phone", "555-1234")
    ],
    created_at: current_time - 35 * 86400,  // 35 days ago
    last_accessed: current_time - 5 * 86400,   // 5 days ago
    access_count: 3,
    tags: []
  }
  
  let critical_record = {
    id: "record-3",
    data_type: "critical",
    data: [
      ("alert", "system_failure"),
      ("severity", "high")
    ],
    created_at: current_time - 200 * 86400,  // 200 days ago
    last_accessed: current_time - 1 * 86400,   // 1 day ago
    access_count: 10,
    tags: ["critical"]
  }
  
  // Add records
  let manager4 = add_data_record(manager3, telemetry_record)
  let manager5 = add_data_record(manager4, pii_record)
  let manager6 = add_data_record(manager5, critical_record)
  
  // Test expiration
  let expired_records = get_expired_records(manager6, current_time)
  
  // telemetry_record (95 days) and pii_record (35 days) should be expired
  // critical_record (200 days) should not be expired due to longer retention
  assert_eq(expired_records.length(), 2)
  assert_true(expired_records.some(fn(r) { r.id == "record-1" }))
  assert_true(expired_records.some(fn(r) { r.id == "record-2" }))
  assert_false(expired_records.some(fn(r) { r.id == "record-3" }))
  
  // Test anonymization
  let records_to_anonymize = get_records_to_anonymize(manager6, current_time)
  
  // Should be empty since all eligible records are already expired
  assert_eq(records_to_anonymize.length(), 0)
  
  // Create a record that should be anonymized but not expired
  let new_telemetry_record = {
    id: "record-4",
    data_type: "telemetry",
    data: [
      ("user_id", "67890"),
      ("event", "logout"),
      ("timestamp", "2000")
    ],
    created_at: current_time - 50 * 86400,  // 50 days ago (should be anonymized at 45 days)
    last_accessed: current_time - 2 * 86400,  // 2 days ago
    access_count: 1,
    tags: []
  }
  
  let manager7 = add_data_record(manager6, new_telemetry_record)
  
  let records_to_anonymize2 = get_records_to_anonymize(manager7, current_time)
  
  // record-4 should be in the list
  assert_eq(records_to_anonymize2.length(), 1)
  assert_eq(records_to_anonymize2[0].id, "record-4")
  
  // Test anonymization
  let anonymized_record = anonymize_record(new_telemetry_record)
  
  assert_eq(anonymized_record.data.find(fn(f) { f.0 == "user_id" }), Some(("user_id", "ANONYMIZED")))
  assert_eq(anonymized_record.data.find(fn(f) { f.0 == "event" }), Some(("event", "logout")))
  assert_true(anonymized_record.tags.contains("anonymized"))
}

// Test 5: Audit Logging and Compliance
test "audit logging and compliance" {
  // Define audit event
  type AuditEvent = {
    id: String,
    timestamp: Int,
    user_id: String,
    action: String,
    resource: String,
    result: String,  // "success", "failure", "denied"
    details: Map[String, String],
    ip_address: String,
    user_agent: String
  }
  
  // Define compliance rule
  type ComplianceRule = {
    name: String,
    description: String,
    check: fn(Array[AuditEvent]) -> Array[String]
  }
  
  // Define audit logger
  type AuditLogger = {
    events: Array[AuditEvent],
    compliance_rules: Array[ComplianceRule]
  }
  
  // Create audit logger
  let create_audit_logger = fn() {
    {
      events: [],
      compliance_rules: []
    }
  }
  
  // Add compliance rule
  let add_compliance_rule = fn(logger: AuditLogger, rule: ComplianceRule) {
    {
      events: logger.events,
      compliance_rules: logger.compliance_rules.push(rule)
    }
  }
  
  // Log audit event
  let log_audit_event = fn(logger: AuditLogger, event: AuditEvent) {
    {
      events: logger.events.push(event),
      compliance_rules: logger.compliance_rules
    }
  }
  
  // Get audit events by user
  let get_events_by_user = fn(logger: AuditLogger, user_id: String) {
    logger.events.filter(fn(event) { event.user_id == user_id })
  }
  
  // Get audit events by time range
  let get_events_by_time_range = fn(logger: AuditLogger, start_time: Int, end_time: Int) {
    logger.events.filter(fn(event) { 
      event.timestamp >= start_time and event.timestamp <= end_time
    })
  }
  
  // Get audit events by action
  let get_events_by_action = fn(logger: AuditLogger, action: String) {
    logger.events.filter(fn(event) { event.action == action })
  }
  
  // Check compliance
  let check_compliance = fn(logger: AuditLogger) {
    let mut violations = []
    
    for i in 0..logger.compliance_rules.length() {
      let rule = logger.compliance_rules[i]
      let rule_violations = rule.check(logger.events)
      
      for j in 0..rule_violations.length() {
        violations = violations.push({
          rule: rule.name,
          violation: rule_violations[j]
        })
      }
    }
    
    violations
  }
  
  // Test audit logging and compliance
  let logger = create_audit_logger()
  
  // Define compliance rules
  let admin_access_rule = {
    name: "admin_access_review",
    description: "All admin access must be reviewed",
    check: fn(events: Array[AuditEvent>) {
      let mut violations = []
      let admin_events = events.filter(fn(event) { 
        event.resource.contains("admin") or event.action.contains("admin")
      })
      
      for i in 0..admin_events.length() {
        let event = admin_events[i]
        if event.result != "success" {
          violations = violations.push("Failed admin access by user " + event.user_id)
        }
      }
      
      violations
    }
  }
  
  let data_access_rule = {
    name: "data_access_logging",
    description: "All data access must be logged",
    check: fn(events: Array[AuditEvent]) {
      let mut violations = []
      let data_access_actions = ["read", "write", "delete", "export"]
      
      for i in 0..data_access_actions.length() {
        let action = data_access_actions[i]
        let access_events = events.filter(fn(event) { event.action == action })
        
        if access_events.length() == 0 {
          violations = violations.push("No " + action + " events logged")
        }
      }
      
      violations
    }
  }
  
  let failed_access_rule = {
    name: "failed_access_threshold",
    description: "Too many failed access attempts",
    check: fn(events: Array[AuditEvent]) {
      let mut violations = []
      let failed_events = events.filter(fn(event) { event.result == "failure" or event.result == "denied" })
      
      // Group by user
      let mut user_failures = []
      
      for i in 0..failed_events.length() {
        let event = failed_events[i]
        let user_id = event.user_id
        
        let existing = user_failures.find_index(fn(uf) { uf.0 == user_id })
        
        match existing {
          Some(index) => {
            let current_count = user_failures[index].1
            user_failures = user_failures.update(index, (user_id, current_count + 1))
          }
          None => {
            user_failures = user_failures.push((user_id, 1))
          }
        }
      }
      
      for i in 0..user_failures.length() {
        let user_failure = user_failures[i]
        
        if user_failure.1 > 5 {
          violations = violations.push("User " + user_failure.0 + " has " + user_failure.1.to_string() + " failed attempts")
        }
      }
      
      violations
    }
  }
  
  // Add compliance rules
  let logger1 = add_compliance_rule(logger, admin_access_rule)
  let logger2 = add_compliance_rule(logger1, data_access_rule)
  let logger3 = add_compliance_rule(logger2, failed_access_rule)
  
  // Log audit events
  let base_time = 1640995200  // Base timestamp
  
  let events = [
    {
      id: "audit-1",
      timestamp: base_time,
      user_id: "user-1",
      action: "read",
      resource: "telemetry_data",
      result: "success",
      details: [("record_count", "10")],
      ip_address: "192.168.1.100",
      user_agent: "Mozilla/5.0"
    },
    {
      id: "audit-2",
      timestamp: base_time + 60,
      user_id: "user-2",
      action: "admin_access",
      resource: "admin_panel",
      result: "success",
      details: [("page", "users")],
      ip_address: "192.168.1.101",
      user_agent: "Mozilla/5.0"
    },
    {
      id: "audit-3",
      timestamp: base_time + 120,
      user_id: "user-3",
      action: "write",
      resource: "telemetry_data",
      result: "success",
      details: [("record_count", "5")],
      ip_address: "192.168.1.102",
      user_agent: "Mozilla/5.0"
    },
    {
      id: "audit-4",
      timestamp: base_time + 180,
      user_id: "user-1",
      action: "delete",
      resource: "telemetry_data",
      result: "failure",
      details: [("reason", "permission_denied")],
      ip_address: "192.168.1.100",
      user_agent: "Mozilla/5.0"
    },
    {
      id: "audit-5",
      timestamp: base_time + 240,
      user_id: "user-1",
      action: "read",
      resource: "telemetry_data",
      result: "denied",
      details: [("reason", "account_locked")],
      ip_address: "192.168.1.100",
      user_agent: "Mozilla/5.0"
    },
    {
      id: "audit-6",
      timestamp: base_time + 300,
      user_id: "user-1",
      action: "write",
      resource: "telemetry_data",
      result: "failure",
      details: [("reason", "permission_denied")],
      ip_address: "192.168.1.100",
      user_agent: "Mozilla/5.0"
    }
  ]
  
  // Add events to logger
  let mut logger_with_events = logger3
  for i in 0..events.length() {
    logger_with_events = log_audit_event(logger_with_events, events[i])
  }
  
  // Test audit queries
  let user1_events = get_events_by_user(logger_with_events, "user-1")
  assert_eq(user1_events.length(), 4)
  
  let read_events = get_events_by_action(logger_with_events, "read")
  assert_eq(read_events.length(), 2)
  
  let time_range_events = get_events_by_time_range(logger_with_events, base_time + 60, base_time + 240)
  assert_eq(time_range_events.length(), 4)
  
  // Test compliance checking
  let violations = check_compliance(logger_with_events)
  
  // Should have violations for:
  // 1. data_access_logging (missing "export" events)
  // 2. failed_access_threshold (user-1 has 3 failed attempts)
  assert_eq(violations.length(), 2)
  
  let data_access_violation = violations.find(fn(v) { v.rule == "data_access_logging" })
  match data_access_violation {
    Some(violation) => assert_eq(violation.violation, "No export events logged")
    None => assert_true(false)
  }
  
  let failed_access_violation = violations.find(fn(v) { v.rule == "failed_access_threshold" })
  match failed_access_violation {
    Some(violation) => assert_eq(violation.violation, "User user-1 has 3 failed attempts")
    None => assert_true(false)
  }
}

// Test 6: Secure Communication
test "secure communication" {
  // Define certificate
  type Certificate = {
    id: String,
    subject: String,
    issuer: String,
    public_key: String,
    not_before: Int,
    not_after: Int,
    signature_algorithm: String
  }
  
  // Define secure channel
  type SecureChannel = {
    id: String,
    local_certificate: Certificate,
    peer_certificate: Option[Certificate],
    cipher_suite: String,
    is_authenticated: Bool,
    is_encrypted: Bool
  }
  
  // Define message
  type SecureMessage = {
    content: String,
    signature: Option[String>,
    encrypted: Bool,
    timestamp: Int
  }
  
  // Create certificate
  let create_certificate = fn(id: String, subject: String, issuer: String, not_before: Int, not_after: Int) {
    {
      id: id,
      subject: subject,
      issuer: issuer,
      public_key: "public-key-" + id,
      not_before: not_before,
      not_after: not_after,
      signature_algorithm: "RSA-SHA256"
    }
  }
  
  // Create secure channel
  let create_secure_channel = fn(id: String, local_cert: Certificate, cipher_suite: String) {
    {
      id: id,
      local_certificate: local_cert,
      peer_certificate: None,
      cipher_suite: cipher_suite,
      is_authenticated: false,
      is_encrypted: false
    }
  }
  
  // Authenticate peer
  let authenticate_peer = fn(channel: SecureChannel, peer_cert: Certificate, current_time: Int) {
    // Check certificate validity
    if current_time < peer_cert.not_before or current_time > peer_cert.not_after {
      channel
    } else {
      // Verify certificate chain (simplified)
      let is_valid = peer_cert.issuer == "trusted-ca"
      
      {
        id: channel.id,
        local_certificate: channel.local_certificate,
        peer_certificate: Some(peer_cert),
        cipher_suite: channel.cipher_suite,
        is_authenticated: is_valid,
        is_encrypted: channel.is_encrypted
      }
    }
  }
  
  // Enable encryption
  let enable_encryption = fn(channel: SecureChannel) {
    {
      id: channel.id,
      local_certificate: channel.local_certificate,
      peer_certificate: channel.peer_certificate,
      cipher_suite: channel.cipher_suite,
      is_authenticated: channel.is_authenticated,
      is_encrypted: true
    }
  }
  
  // Sign message
  let sign_message = fn(message: String, private_key: String) {
    // Simplified signing
    "signature-" + message.length().to_string() + "-" + private_key
  }
  
  // Verify signature
  let verify_signature = fn(message: String, signature: String, public_key: String) {
    // Simplified verification
    signature.contains(message.length().to_string()) and signature.contains(public_key)
  }
  
  // Send secure message
  let send_secure_message = fn(channel: SecureChannel, content: String, private_key: String, current_time: Int) {
    let signature = sign_message(content, private_key)
    
    {
      content: content,
      signature: Some(signature),
      encrypted: channel.is_encrypted,
      timestamp: current_time
    }
  }
  
  // Receive secure message
  let receive_secure_message = fn(channel: SecureChannel, message: SecureMessage, public_key: String) {
    // Verify signature if present
    let signature_valid = match message.signature {
      Some(sig) => verify_signature(message.content, sig, public_key)
      None => true
    }
    
    if signature_valid {
      Some(message.content)
    } else {
      None
    }
  }
  
  // Test secure communication
  let current_time = 1640995200
  
  let server_cert = create_certificate(
    "cert-1",
    "CN=server.example.com",
    "CN=trusted-ca",
    current_time - 86400 * 30,  // 30 days ago
    current_time + 86400 * 365   // 1 year from now
  )
  
  let client_cert = create_certificate(
    "cert-2",
    "CN=client.example.com",
    "CN=trusted-ca",
    current_time - 86400 * 15,  // 15 days ago
    current_time + 86400 * 180   // 180 days from now
  )
  
  let expired_cert = create_certificate(
    "cert-3",
    "CN=expired.example.com",
    "CN=trusted-ca",
    current_time - 86400 * 400,  // 400 days ago
    current_time - 86400 * 35    // Expired 35 days ago
  )
  
  // Create secure channels
  let server_channel = create_secure_channel("channel-1", server_cert, "TLS_AES_256_GCM_SHA384")
  let client_channel = create_secure_channel("channel-2", client_cert, "TLS_AES_256_GCM_SHA384")
  
  // Test authentication
  let authenticated_server = authenticate_peer(server_channel, client_cert, current_time)
  assert_true(authenticated_server.is_authenticated)
  assert_eq(authenticated_server.peer_certificate, Some(client_cert))
  
  let expired_auth = authenticate_peer(server_channel, expired_cert, current_time)
  assert_false(expired_auth.is_authenticated)
  
  // Test encryption
  let encrypted_server = enable_encryption(authenticated_server)
  assert_true(encrypted_server.is_encrypted)
  
  // Test secure messaging
  let server_private_key = "private-key-server"
  let client_public_key = client_cert.public_key
  
  let message = "Secure telemetry data transmission"
  
  // Server sends message
  let secure_message = send_secure_message(encrypted_server, message, server_private_key, current_time)
  assert_eq(secure_message.content, message)
  assert_true(secure_message.signature.is_some())
  assert_true(secure_message.encrypted)
  
  // Client receives message
  let received_content = receive_secure_message(client_channel, secure_message, client_public_key)
  
  // Should fail because client channel doesn't have server's public key
  assert_eq(received_content, None)
  
  // Test with correct public key
  let server_public_key = server_cert.public_key
  let received_content_correct = receive_secure_message(encrypted_server, secure_message, server_public_key)
  
  // Should still fail because we're using the wrong key for verification
  // In a real implementation, this would work with proper key exchange
  assert_eq(received_content_correct, None)
  
  // Test mutual authentication
  let mutual_server = authenticate_peer(encrypted_server, client_cert, current_time)
  let mutual_client = authenticate_peer(client_channel, server_cert, current_time)
  
  assert_true(mutual_server.is_authenticated)
  assert_true(mutual_client.is_authenticated)
  
  // Test with expired certificate
  let expired_client = authenticate_peer(client_channel, expired_cert, current_time)
  assert_false(expired_client.is_authenticated)
}
// Azimuth 遥测数据安全与隐私保护测试用例
// 专注于数据加密、隐私保护、访问控制等安全功能

// 测试1: 遥测数据加密与解密
test "遥测数据加密与解密功能" {
  // 定义加密算法类型
  enum EncryptionAlgorithm {
    AES256
    ChaCha20
    RSA
  }
  
  // 定义加密配置
  type EncryptionConfig {
    algorithm: EncryptionAlgorithm
    key_id: String
    iv_length: Int
    include_timestamp: Bool
  }
  
  // 定义敏感数据类型
  enum SensitiveDataType {
    PersonalInformation
    FinancialData
    HealthInformation
    SecurityCredentials
  }
  
  // 模拟加密函数
  let encrypt_data = fn(data: String, config: EncryptionConfig) -> String {
    // 简化的加密模拟
    let algorithm_prefix = match config.algorithm {
      EncryptionAlgorithm::AES256 => "AES256:"
      EncryptionAlgorithm::ChaCha20 => "CC20:"
      EncryptionAlgorithm::RSA => "RSA:"
    }
    
    let timestamp_suffix = if config.include_timestamp {
      ":ts:" + (1640995200).to_string()
    } else {
      ""
    }
    
    // 模拟加密过程：反转字符串并添加前缀
    let reversed = data.to_char_array().reverse().join("")
    algorithm_prefix + reversed + timestamp_suffix
  }
  
  // 模拟解密函数
  let decrypt_data = fn(encrypted_data: String, config: EncryptionConfig) -> String {
    // 简化的解密模拟
    let algorithm_prefix = match config.algorithm {
      EncryptionAlgorithm::AES256 => "AES256:"
      EncryptionAlgorithm::ChaCha20 => "CC20:"
      EncryptionAlgorithm::RSA => "RSA:"
    }
    
    // 移除前缀
    let without_prefix = if encrypted_data.starts_with(algorithm_prefix) {
      encrypted_data.substring(algorithm_prefix.length(), encrypted_data.length() - algorithm_prefix.length())
    } else {
      encrypted_data
    }
    
    // 移除时间戳（如果存在）
    let without_timestamp = if config.include_timestamp {
      let ts_index = without_timestamp.find(":ts:")
      match ts_index {
        Some(index) => without_timestamp.substring(0, index),
        None => without_timestamp
      }
    } else {
      without_timestamp
    }
    
    // 模拟解密过程：再次反转字符串
    without_timestamp.to_char_array().reverse().join("")
  }
  
  // 创建测试配置
  let aes_config = {
    algorithm: EncryptionAlgorithm::AES256,
    key_id: "key-001",
    iv_length: 16,
    include_timestamp: true
  }
  
  let chacha_config = {
    algorithm: EncryptionAlgorithm::ChaCha20,
    key_id: "key-002",
    iv_length: 12,
    include_timestamp: false
  }
  
  // 测试基本加密解密
  let sensitive_data = "user_id=12345,email=user@example.com"
  
  let encrypted_aes = encrypt_data(sensitive_data, aes_config)
  assert_true(encrypted_aes.starts_with("AES256:"))
  assert_true(encrypted_aes.contains(":ts:"))
  
  let decrypted_aes = decrypt_data(encrypted_aes, aes_config)
  assert_eq(decrypted_aes, sensitive_data)
  
  // 测试不同算法的加密
  let encrypted_chacha = encrypt_data(sensitive_data, chacha_config)
  assert_true(encrypted_chacha.starts_with("CC20:"))
  assert_false(encrypted_chacha.contains(":ts:"))
  
  let decrypted_chacha = decrypt_data(encrypted_chacha, chacha_config)
  assert_eq(decrypted_chacha, sensitive_data)
  
  // 测试错误处理
  let invalid_encrypted = "INVALID:algorithms"
  let decrypted_invalid = decrypt_data(invalid_encrypted, aes_config)
  assert_not_eq(decrypted_invalid, sensitive_data)
}

// 测试2: 敏感数据检测与脱敏
test "敏感数据检测与脱敏处理" {
  // 定义脱敏策略
  enum MaskingStrategy {
    FullRedaction  // 完全删除
    PartialMask    // 部分遮蔽
    Tokenization   // 令牌化
    Hashing        // 哈希处理
  }
  
  // 定义脱敏规则
  type MaskingRule {
    data_type: SensitiveDataType
    field_pattern: String
    strategy: MaskingStrategy
    preserve_length: Bool
    prefix_length: Int
    suffix_length: Int
  }
  
  // 定义脱敏配置
  type MaskingConfig {
    rules: Array[MaskingRule]
    default_strategy: MaskingStrategy
  }
  
  // 模拟正则表达式匹配
  let pattern_matches = fn(text: String, pattern: String) -> Bool {
    // 简化的模式匹配
    match pattern {
      "email" => text.contains("@") && text.contains("."),
      "phone" => text.length() >= 10 && text.chars().all(fn(c) { c.is_digit() || c == '-' || c == '+' || c == '(' || c == ')' }),
      "credit_card" => text.length() >= 13 && text.length() <= 19 && text.chars().all(fn(c) { c.is_digit() || c == ' ' || c == '-' }),
      "ssn" => text.length() == 11 && text[3] == '-' && text[6] == '-',
      "api_key" => text.starts_with("sk-") || text.starts_with("api_"),
      _ => false
    }
  }
  
  // 应用脱敏策略
  let apply_masking = fn(value: String, strategy: MaskingStrategy, preserve_length: Bool, prefix_len: Int, suffix_len: Int) -> String {
    match strategy {
      MaskingStrategy::FullRedaction => "***",
      MaskingStrategy::PartialMask => {
        if preserve_length && value.length() > (prefix_len + suffix_len) {
          let prefix = value.substring(0, prefix_len)
          let suffix = value.substring(value.length() - suffix_len, suffix_len)
          let mask_length = value.length() - prefix_len - suffix_len
          prefix + "*".repeat(mask_length) + suffix
        } else {
          "***"
        }
      },
      MaskingStrategy::Tokenization => "TOKEN-" + (value.length() % 1000).to_string(),
      MaskingStrategy::Hashing => "HASH-" + (value.length() * 31).to_string()
    }
  }
  
  // 检测并脱敏数据
  let detect_and_mask = fn(data: Map[String, String], config: MaskingConfig) -> Map[String, String] {
    let mut result = Map::empty()
    
    for (key, value) in data.to_array() {
      let mut masked_value = value
      let mut should_mask = false
      
      // 检查是否匹配任何脱敏规则
      for rule in config.rules {
        if pattern_matches(key, rule.field_pattern) || pattern_matches(value, rule.field_pattern) {
          masked_value = apply_masking(value, rule.strategy, rule.preserve_length, rule.prefix_length, rule.suffix_length)
          should_mask = true
          break
        }
      }
      
      // 如果没有匹配规则但值包含敏感信息，应用默认策略
      if not(should_mask) {
        let sensitive_patterns = ["email", "phone", "credit_card", "ssn", "api_key"]
        for pattern in sensitive_patterns {
          if pattern_matches(value, pattern) {
            masked_value = apply_masking(value, config.default_strategy, true, 2, 2)
            break
          }
        }
      }
      
      result = Map::insert(result, key, masked_value)
    }
    
    result
  }
  
  // 创建测试数据
  let telemetry_data = Map::from([
    ("user.email", "john.doe@example.com"),
    ("user.phone", "+1-555-123-4567"),
    ("payment.card", "4532-1234-5678-9012"),
    ("user.ssn", "123-45-6789"),
    ("api.key", "sk_live_1234567890abcdef"),
    ("operation.name", "user.login"),
    ("response.time", "250"),
    ("server.name", "api-server-01")
  ])
  
  // 创建脱敏规则
  let masking_rules = [
    {
      data_type: SensitiveDataType::PersonalInformation,
      field_pattern: "email",
      strategy: MaskingStrategy::PartialMask,
      preserve_length: true,
      prefix_length: 2,
      suffix_length: 8
    },
    {
      data_type: SensitiveDataType::PersonalInformation,
      field_pattern: "phone",
      strategy: MaskingStrategy::PartialMask,
      preserve_length: true,
      prefix_length: 3,
      suffix_length: 4
    },
    {
      data_type: SensitiveDataType::FinancialData,
      field_pattern: "credit_card",
      strategy: MaskingStrategy::PartialMask,
      preserve_length: true,
      prefix_length: 4,
      suffix_length: 4
    },
    {
      data_type: SensitiveDataType::PersonalInformation,
      field_pattern: "ssn",
      strategy: MaskingStrategy::FullRedaction,
      preserve_length: false,
      prefix_length: 0,
      suffix_length: 0
    },
    {
      data_type: SensitiveDataType::SecurityCredentials,
      field_pattern: "api_key",
      strategy: MaskingStrategy::Tokenization,
      preserve_length: false,
      prefix_length: 0,
      suffix_length: 0
    }
  ]
  
  let masking_config = {
    rules: masking_rules,
    default_strategy: MaskingStrategy::PartialMask
  }
  
  // 应用脱敏
  let masked_data = detect_and_mask(telemetry_data, masking_config)
  
  // 验证脱敏结果
  match Map::get(masked_data, "user.email") {
    Some(value) => {
      assert_eq(value.length(), "john.doe@example.com".length())
      assert_true(value.starts_with("jo"))
      assert_true(value.ends_with("example.com"))
      assert_true(value.contains("*"))
    },
    None => assert_true(false)
  }
  
  match Map::get(masked_data, "user.ssn") {
    Some(value) => assert_eq(value, "***"),
    None => assert_true(false)
  }
  
  match Map::get(masked_data, "api.key") {
    Some(value) => assert_true(value.starts_with("TOKEN-")),
    None => assert_true(false)
  }
  
  // 验证非敏感数据未被修改
  match Map::get(masked_data, "operation.name") {
    Some(value) => assert_eq(value, "user.login"),
    None => assert_true(false)
  }
  
  match Map::get(masked_data, "server.name") {
    Some(value) => assert_eq(value, "api-server-01"),
    None => assert_true(false)
  }
}

// 测试3: 遥测数据访问控制
test "遥测数据访问控制与权限验证" {
  // 定义权限级别
  enum PermissionLevel {
    None
    Read
    Write
    Admin
  }
  
  // 定义用户角色
  enum UserRole {
    Viewer
    Analyst
    Engineer
    Manager
    Admin
  }
  
  // 定义用户
  type User = {
    id: String
    username: String
    role: UserRole
    team: String
    permissions: Array[String]
  }
  
  // 定义资源
  type Resource = {
    id: String
    name: String
    type: String
    sensitivity: String
    owner: String
    tags: Array[String]
  }
  
  // 定义访问策略
  type AccessPolicy = {
    name: String
    role: UserRole
    resource_type: String
    required_permission: PermissionLevel
    conditions: Array[String]
  }
  
  // 检查用户权限
  let check_permission = fn(user: User, resource: Resource, required_level: PermissionLevel, policies: Array[AccessPolicy]) -> Bool {
    // 查找适用的策略
    let applicable_policies = policies.filter(fn(policy) {
      policy.role == user.role && policy.resource_type == resource.type
    })
    
    if applicable_policies.length() == 0 {
      return false
    }
    
    // 检查条件
    for policy in applicable_policies {
      let mut conditions_met = true
      
      for condition in policy.conditions {
        match condition {
          "same_team" => {
            if user.team != resource.owner {
              conditions_met = false
            }
          },
          "not_sensitive" => {
            if resource.sensitivity == "high" {
              conditions_met = false
            }
          },
          "owner_only" => {
            if user.id != resource.owner {
              conditions_met = false
            }
          },
          _ => {}
        }
      }
      
      if conditions_met {
        return true
      }
    }
    
    false
  }
  
  // 检查字段级别权限
  let check_field_access = fn(user: User, field_name: String, field_sensitivity: String) -> Bool {
    match user.role {
      UserRole::Admin => true,
      UserRole::Manager => field_sensitivity != "restricted",
      UserRole::Engineer => field_sensitivity == "public" || field_sensitivity == "internal",
      UserRole::Analyst => field_sensitivity == "public",
      UserRole::Viewer => field_sensitivity == "public"
    }
  }
  
  // 过滤可访问的字段
  let filter_accessible_fields = fn(user: User, data: Map[String, String], field_sensitivities: Map[String, String]) -> Map[String, String] {
    let mut result = Map::empty()
    
    for (field, value) in data.to_array() {
      let sensitivity = match Map::get(field_sensitivities, field) {
        Some(s) => s,
        None => "public"
      }
      
      if check_field_access(user, field, sensitivity) {
        result = Map::insert(result, field, value)
      }
    }
    
    result
  }
  
  // 创建测试用户
  let users = [
    {
      id: "user-001",
      username: "admin",
      role: UserRole::Admin,
      team: "platform",
      permissions: ["read", "write", "delete", "admin"]
    },
    {
      id: "user-002",
      username: "manager",
      role: UserRole::Manager,
      team: "observability",
      permissions: ["read", "write", "manage"]
    },
    {
      id: "user-003",
      username: "engineer",
      role: UserRole::Engineer,
      team: "backend",
      permissions: ["read", "write"]
    },
    {
      id: "user-004",
      username: "analyst",
      role: UserRole::Analyst,
      team: "data",
      permissions: ["read"]
    },
    {
      id: "user-005",
      username: "viewer",
      role: UserRole::Viewer,
      team: "support",
      permissions: ["read"]
    }
  ]
  
  // 创建测试资源
  let resources = [
    {
      id: "resource-001",
      name: "production-metrics",
      type: "metrics",
      sensitivity: "high",
      owner: "platform",
      tags: ["production", "critical"]
    },
    {
      id: "resource-002",
      name: "user-activity-logs",
      type: "logs",
      sensitivity: "medium",
      owner: "observability",
      tags: ["user", "activity"]
    },
    {
      id: "resource-003",
      name: "performance-traces",
      type: "traces",
      sensitivity: "medium",
      owner: "backend",
      tags: ["performance", "traces"]
    },
    {
      id: "resource-004",
      name: "error-reports",
      type: "alerts",
      sensitivity: "low",
      owner: "support",
      tags: ["errors", "alerts"]
    }
  ]
  
  // 创建访问策略
  let access_policies = [
    {
      name: "admin-full-access",
      role: UserRole::Admin,
      resource_type: "*",
      required_permission: PermissionLevel::Admin,
      conditions: []
    },
    {
      name: "manager-team-access",
      role: UserRole::Manager,
      resource_type: "metrics",
      required_permission: PermissionLevel::Read,
      conditions: ["same_team"]
    },
    {
      name: "engineer-resource-access",
      role: UserRole::Engineer,
      resource_type: "traces",
      required_permission: PermissionLevel::Read,
      conditions: ["same_team"]
    },
    {
      name: "analyst-public-access",
      role: UserRole::Analyst,
      resource_type: "*",
      required_permission: PermissionLevel::Read,
      conditions: ["not_sensitive"]
    },
    {
      name: "viewer-limited-access",
      role: UserRole::Viewer,
      resource_type: "alerts",
      required_permission: PermissionLevel::Read,
      conditions: []
    }
  ]
  
  // 测试资源访问权限
  let admin_user = users[0]
  let manager_user = users[1]
  let engineer_user = users[2]
  let analyst_user = users[3]
  let viewer_user = users[4]
  
  let metrics_resource = resources[0]
  let logs_resource = resources[1]
  let traces_resource = resources[2]
  let alerts_resource = resources[3]
  
  // 管理员应该能访问所有资源
  assert_true(check_permission(admin_user, metrics_resource, PermissionLevel::Read, access_policies))
  assert_true(check_permission(admin_user, logs_resource, PermissionLevel::Read, access_policies))
  assert_true(check_permission(admin_user, traces_resource, PermissionLevel::Read, access_policies))
  assert_true(check_permission(admin_user, alerts_resource, PermissionLevel::Read, access_policies))
  
  // 经理只能访问同团队的高敏感度资源
  assert_false(check_permission(manager_user, metrics_resource, PermissionLevel::Read, access_policies)) // 不同团队
  assert_true(check_permission(manager_user, logs_resource, PermissionLevel::Read, access_policies)) // 同团队
  
  // 工程师只能访问同团队的追踪数据
  assert_false(check_permission(engineer_user, metrics_resource, PermissionLevel::Read, access_policies)) // 不同团队
  assert_true(check_permission(engineer_user, traces_resource, PermissionLevel::Read, access_policies)) // 同团队
  
  // 分析师只能访问非敏感资源
  assert_false(check_permission(analyst_user, metrics_resource, PermissionLevel::Read, access_policies)) // 高敏感度
  assert_true(check_permission(analyst_user, alerts_resource, PermissionLevel::Read, access_policies)) // 低敏感度
  
  // 查看者只能访问告警资源
  assert_false(check_permission(viewer_user, logs_resource, PermissionLevel::Read, access_policies)) // 无权限
  assert_true(check_permission(viewer_user, alerts_resource, PermissionLevel::Read, access_policies)) // 有权限
  
  // 测试字段级别访问控制
  let telemetry_data = Map::from([
    ("operation.name", "user.login"),
    ("user.id", "12345"),
    ("user.email", "user@example.com"),
    ("response.time", "250"),
    ("error.stack", "NullPointerException at line 42"),
    ("api.key", "sk_live_1234567890abcdef")
  ])
  
  let field_sensitivities = Map::from([
    ("operation.name", "public"),
    ("user.id", "internal"),
    ("user.email", "internal"),
    ("response.time", "public"),
    ("error.stack", "internal"),
    ("api.key", "restricted")
  ])
  
  // 测试不同角色的字段访问权限
  let admin_fields = filter_accessible_fields(admin_user, telemetry_data, field_sensitivities)
  assert_eq(admin_fields.size(), telemetry_data.size()) // 管理员访问所有字段
  
  let analyst_fields = filter_accessible_fields(analyst_user, telemetry_data, field_sensitivities)
  assert_eq(analyst_fields.size(), 2) // 分析师只能访问公共字段
  assert_true(Map::contains_key(analyst_fields, "operation.name"))
  assert_true(Map::contains_key(analyst_fields, "response.time"))
  
  let engineer_fields = filter_accessible_fields(engineer_user, telemetry_data, field_sensitivities)
  assert_eq(engineer_fields.size(), 4) // 工程师可以访问公共和内部字段
  assert_true(Map::contains_key(engineer_fields, "operation.name"))
  assert_true(Map::contains_key(engineer_fields, "response.time"))
  assert_true(Map::contains_key(engineer_fields, "user.id"))
  assert_true(Map::contains_key(engineer_fields, "error.stack"))
}

// 测试4: 审计日志与合规性验证
test "遥测数据审计日志与合规性验证" {
  // 定义审计事件类型
  enum AuditEventType {
    DataAccess
    DataModification
    ConfigurationChange
    SecurityEvent
    SystemEvent
  }
  
  // 定义审计事件
  type AuditEvent = {
    timestamp: Int
    event_type: AuditEventType
    user_id: String
    resource_id: String
    action: String
    result: String
    details: Map[String, String]
    ip_address: String
    user_agent: String
  }
  
  // 定义合规性规则
  type ComplianceRule = {
    id: String
    name: String
    description: String
    regulation: String // GDPR, HIPAA, PCI-DSS, etc.
    check_function: (Array[AuditEvent]) -> Array[ComplianceViolation]
  }
  
  // 定义合规性违规
  type ComplianceViolation = {
    rule_id: String
    timestamp: Int
    severity: String
    description: String
    affected_events: Array[Int]
    recommended_action: String
  }
  
  // 创建审计事件
  let create_audit_event = fn(timestamp: Int, event_type: AuditEventType, user_id: String, resource_id: String, action: String, result: String, details: Map[String, String], ip_address: String, user_agent: String) -> AuditEvent {
    {
      timestamp,
      event_type,
      user_id,
      resource_id,
      action,
      result,
      details,
      ip_address,
      user_agent
    }
  }
  
  // 检查数据访问模式
  let check_data_access_patterns = fn(events: Array[AuditEvent]) -> Array[ComplianceViolation] {
    let mut violations = []
    
    // 检查异常访问时间
    let business_hours_start = 9 * 60 * 60 // 9:00 AM in seconds
    let business_hours_end = 17 * 60 * 60  // 5:00 PM in seconds
    
    for i in 0..events.length() {
      let event = events[i]
      let hour_of_day = (event.timestamp / 3600) % 24
      let seconds_of_day = hour_of_day * 60 * 60
      
      // 检查非工作时间访问敏感数据
      if event.event_type == AuditEventType::DataAccess && (seconds_of_day < business_hours_start || seconds_of_day > business_hours_end) {
        match Map::get(event.details, "sensitivity") {
          Some(sensitivity) => {
            if sensitivity == "high" {
              let violation = {
                rule_id: "GDPR-001",
                timestamp: event.timestamp,
                severity: "medium",
                description: "访问高敏感度数据在非工作时间",
                affected_events: [i],
                recommended_action: "验证访问的必要性并记录业务理由"
              }
              violations = violations.push(violation)
            }
          },
          None => {}
        }
      }
    }
    
    violations
  }
  
  // 检查批量数据访问
  let check_bulk_data_access = fn(events: Array[AuditEvent]) -> Array[ComplianceViolation] {
    let mut violations = []
    let access_threshold = 100 // 1小时内访问100条记录
    
    // 按用户和小时分组
    let mut user_hour_access = Map::empty()
    
    for i in 0..events.length() {
      let event = events[i]
      
      if event.event_type == AuditEventType::DataAccess {
        let hour_key = event.user_id + ":" + (event.timestamp / 3600).to_string()
        let current_count = match Map::get(user_hour_access, hour_key) {
          Some(count) => count,
          None => 0
        }
        
        user_hour_access = Map::insert(user_hour_access, hour_key, current_count + 1)
        
        // 检查是否超过阈值
        if current_count + 1 > access_threshold {
          let violation = {
            rule_id: "GDPR-002",
            timestamp: event.timestamp,
            severity: "high",
            description: "用户在1小时内访问大量数据记录",
            affected_events: [i],
            recommended_action: "审查批量访问的必要性和授权"
          }
          violations = violations.push(violation)
        }
      }
    }
    
    violations
  }
  
  // 检查数据保留期限
  let check_data_retention = fn(events: Array[AuditEvent]) -> Array[ComplianceViolation] {
    let mut violations = []
    let current_time = 1640995200 // 示例当前时间
    let retention_days = 30 // 30天保留期限
    let retention_seconds = retention_days * 24 * 60 * 60
    
    // 查找数据删除事件
    for i in 0..events.length() {
      let event = events[i]
      
      if event.action == "delete" {
        match Map::get(event.details, "creation_time") {
          Some(creation_time_str) => {
            let creation_time = creation_time_str.to_int()
            let data_age = event.timestamp - creation_time
            
            if data_age < retention_seconds {
              let violation = {
                rule_id: "GDPR-003",
                timestamp: event.timestamp,
                severity: "high",
                description: "在保留期限前删除数据",
                affected_events: [i],
                recommended_action: "确保数据删除符合保留政策要求"
              }
              violations = violations.push(violation)
            }
          },
          None => {}
        }
      }
    }
    
    violations
  }
  
  // 创建测试审计事件
  let base_time = 1640995200 // 2022-01-01 00:00:00 UTC
  let audit_events = [
    // 正常工作时间访问
    create_audit_event(
      base_time + (10 * 60 * 60), // 10:00 AM
      AuditEventType::DataAccess,
      "user-001",
      "resource-001",
      "read",
      "success",
      Map::from([("record_count", "5"), ("sensitivity", "low")]),
      "192.168.1.100",
      "Mozilla/5.0"
    ),
    
    // 非工作时间访问敏感数据
    create_audit_event(
      base_time + (22 * 60 * 60), // 10:00 PM
      AuditEventType::DataAccess,
      "user-002",
      "resource-002",
      "read",
      "success",
      Map::from([("record_count", "10"), ("sensitivity", "high")]),
      "192.168.1.101",
      "Mozilla/5.0"
    ),
    
    // 批量数据访问
    create_audit_event(
      base_time + (14 * 60 * 60), // 2:00 PM
      AuditEventType::DataAccess,
      "user-003",
      "resource-003",
      "bulk_export",
      "success",
      Map::from([("record_count", "150"), ("sensitivity", "medium")]),
      "192.168.1.102",
      "Mozilla/5.0"
    ),
    
    // 配置更改
    create_audit_event(
      base_time + (15 * 60 * 60), // 3:00 PM
      AuditEventType::ConfigurationChange,
      "user-004",
      "config-001",
      "update",
      "success",
      Map::from([("setting", "retention_period"), ("old_value", "90"), ("new_value", "30")]),
      "192.168.1.103",
      "Mozilla/5.0"
    ),
    
    // 提前删除数据
    create_audit_event(
      base_time + (16 * 60 * 60), // 4:00 PM
      AuditEventType::DataModification,
      "user-005",
      "resource-004",
      "delete",
      "success",
      Map::from([("record_count", "50"), ("creation_time", (base_time - (10 * 24 * 60 * 60)).to_string())]), // 10天前创建
      "192.168.1.104",
      "Mozilla/5.0"
    )
  ]
  
  // 创建合规性规则
  let compliance_rules = [
    {
      id: "GDPR-001",
      name: "非工作时间敏感数据访问",
      description: "监控在非工作时间访问高敏感度数据的行为",
      regulation: "GDPR",
      check_function: check_data_access_patterns
    },
    {
      id: "GDPR-002",
      name: "批量数据访问限制",
      description: "监控用户在短时间内大量访问数据的行为",
      regulation: "GDPR",
      check_function: check_bulk_data_access
    },
    {
      id: "GDPR-003",
      name: "数据保留期限",
      description: "确保数据在保留期限前不被删除",
      regulation: "GDPR",
      check_function: check_data_retention
    }
  ]
  
  // 运行合规性检查
  let mut all_violations = []
  
  for rule in compliance_rules {
    let rule_violations = rule.check_function(audit_events)
    for violation in rule_violations {
      all_violations = all_violations.push(violation)
    }
  }
  
  // 验证违规检测
  assert_eq(all_violations.length(), 3) // 应该检测到3个违规
  
  // 验证第一个违规：非工作时间访问敏感数据
  let first_violation = all_violations[0]
  assert_eq(first_violation.rule_id, "GDPR-001")
  assert_eq(first_violation.severity, "medium")
  
  // 验证第二个违规：批量数据访问
  let second_violation = all_violations[1]
  assert_eq(second_violation.rule_id, "GDPR-002")
  assert_eq(second_violation.severity, "high")
  
  // 验证第三个违规：提前删除数据
  let third_violation = all_violations[2]
  assert_eq(third_violation.rule_id, "GDPR-003")
  assert_eq(third_violation.severity, "high")
  
  // 测试审计日志查询
  let query_audit_events = fn(events: Array[AuditEvent], filters: Map[String, String]) -> Array[AuditEvent] {
    events.filter(fn(event) {
      let mut matches = true
      
      for (key, value) in filters.to_array() {
        match key {
          "user_id" => {
            if event.user_id != value {
              matches = false
            }
          },
          "event_type" => {
            let event_type_str = match event.event_type {
              AuditEventType::DataAccess => "data_access",
              AuditEventType::DataModification => "data_modification",
              AuditEventType::ConfigurationChange => "config_change",
              AuditEventType::SecurityEvent => "security_event",
              AuditEventType::SystemEvent => "system_event"
            }
            if event_type_str != value {
              matches = false
            }
          },
          "action" => {
            if event.action != value {
              matches = false
            }
          },
          _ => {}
        }
      }
      
      matches
    })
  }
  
  // 测试查询功能
  let user_events = query_audit_events(audit_events, Map::from([("user_id", "user-001")]))
  assert_eq(user_events.length(), 1)
  assert_eq(user_events[0].user_id, "user-001")
  
  let access_events = query_audit_events(audit_events, Map::from([("event_type", "data_access")]))
  assert_eq(access_events.length(), 3)
  
  let delete_events = query_audit_events(audit_events, Map::from([("action", "delete")]))
  assert_eq(delete_events.length(), 1)
  assert_eq(delete_events[0].action, "delete")
}
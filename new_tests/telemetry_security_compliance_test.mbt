// 遥测系统安全性测试用例

test "telemetry_data_encryption_security" {
  // 测试遥测数据加密安全性
  
  let encryption_config = {
    "encryption_algorithm": "AES-256-GCM",
    "key_rotation_interval_hours": 24,
    "data_classification": ["public", "internal", "confidential", "restricted"],
    "encryption_required_for": ["confidential", "restricted"]
  }
  
  // 验证加密配置
  assert_eq(encryption_config["encryption_algorithm"], "AES-256-GCM")
  assert_eq(encryption_config["key_rotation_interval_hours"], 24)
  assert_eq(encryption_config["data_classification"].length(), 4)
  
  // 敏感数据类型
  type SensitiveData = {
    data_type: String,
    classification: String,
    data_value: String,
    encrypted: Bool,
    encryption_key_id: String,
    access_level_required: String
  }
  
  // 创建敏感数据测试场景
  let sensitive_data_scenarios = [
    // 个人身份信息（PII）
    SensitiveData {
      data_type: "email_address",
      classification: "confidential",
      data_value: "user@example.com",
      encrypted: false,
      encryption_key_id: "",
      access_level_required: "pii_access"
    },
    // 财务信息
    SensitiveData {
      data_type: "credit_card_number",
      classification: "restricted",
      data_value: "4532-1234-5678-9012",
      encrypted: false,
      encryption_key_id: "",
      access_level_required: "financial_access"
    },
    // 健康信息
    SensitiveData {
      data_type: "medical_record",
      classification: "restricted",
      data_value: "patient_diagnosis_12345",
      encrypted: false,
      encryption_key_id: "",
      access_level_required: "healthcare_access"
    },
    // 内部业务数据
    SensitiveData {
      data_type: "revenue_data",
      classification: "internal",
      data_value: "quarterly_revenue_2023",
      encrypted: false,
      encryption_key_id: "",
      access_level_required: "business_access"
    },
    // 公开信息
    SensitiveData {
      data_type: "product_name",
      classification: "public",
      data_value: "awesome_product",
      encrypted: false,
      encryption_key_id: "",
      access_level_required: "public_access"
    }
  ]
  
  // 验证敏感数据场景
  assert_eq(sensitive_data_scenarios.length(), 5)
  
  // 加密处理函数
  let apply_encryption = fn(data: SensitiveData) -> SensitiveData {
    let requires_encryption = encryption_config["encryption_required_for"].contains(data.classification)
    
    if requires_encryption {
      // 模拟加密过程
      let encrypted_data = SensitiveData {
        data_type: data.data_type,
        classification: data.classification,
        data_value: "encrypted_" + data.data_value,  // 简化的加密表示
        encrypted: true,
        encryption_key_id: "key_" + data.data_type + "_v1",
        access_level_required: data.access_level_required
      }
      encrypted_data
    } else {
      data
    }
  }
  
  // 执行加密处理
  let mut encrypted_data_set = []
  let mut i = 0
  while i < sensitive_data_scenarios.length() {
    let processed_data = apply_encryption(sensitive_data_scenarios[i])
    encrypted_data_set.push(processed_data)
    i = i + 1
  }
  
  // 验证加密结果
  assert_eq(encrypted_data_set.length(), 5)
  
  // 统计加密数据
  let mut encrypted_count = 0
  let mut unencrypted_count = 0
  let mut confidential_encrypted = 0
  let mut restricted_encrypted = 0
  
  i = 0
  while i < encrypted_data_set.length() {
    let data = encrypted_data_set[i]
    
    if data.encrypted {
      encrypted_count = encrypted_count + 1
    } else {
      unencrypted_count = unencrypted_count + 1
    }
    
    if data.classification == "confidential" and data.encrypted {
      confidential_encrypted = confidential_encrypted + 1
    }
    
    if data.classification == "restricted" and data.encrypted {
      restricted_encrypted = restricted_encrypted + 1
    }
    
    i = i + 1
  }
  
  // 验证加密统计
  assert_eq(encrypted_count, 3)        // confidential和restricted数据应该被加密
  assert_eq(unencrypted_count, 2)      // internal和public数据不应该被加密
  assert_eq(confidential_encrypted, 1) // 1个confidential数据被加密
  assert_eq(restricted_encrypted, 2)   // 2个restricted数据被加密
  
  // 密钥轮换测试
  type KeyRotation = {
    key_id: String,
    creation_time: Int,
    expiration_time: Int,
    is_active: Bool,
    data_encrypted_count: Int
  }
  
  let mut encryption_keys = [
    KeyRotation {
      key_id: "key_email_address_v1",
      creation_time: 1640995200000,
      expiration_time: 1641081600000,  // 24小时后过期
      is_active: true,
      data_encrypted_count: 0
    },
    KeyRotation {
      key_id: "key_credit_card_number_v1",
      creation_time: 1640995200000,
      expiration_time: 1641081600000,
      is_active: true,
      data_encrypted_count: 0
    },
    KeyRotation {
      key_id: "key_medical_record_v1",
      creation_time: 1640995200000,
      expiration_time: 1641081600000,
      is_active: true,
      data_encrypted_count: 0
    }
  ]
  
  // 模拟密钥使用
  i = 0
  while i < encrypted_data_set.length() {
    let data = encrypted_data_set[i]
    if data.encrypted {
      let mut j = 0
      while j < encryption_keys.length() {
        if encryption_keys[j].key_id == data.encryption_key_id {
          let key = encryption_keys[j]
          let updated_key = KeyRotation {
            key_id: key.key_id,
            creation_time: key.creation_time,
            expiration_time: key.expiration_time,
            is_active: key.is_active,
            data_encrypted_count: key.data_encrypted_count + 1
          }
          encryption_keys[j] = updated_key
          break
        }
        j = j + 1
      }
    }
    i = i + 1
  }
  
  // 验证密钥使用统计
  let mut total_encrypted_data = 0
  let mut active_keys_count = 0
  
  i = 0
  while i < encryption_keys.length() {
    let key = encryption_keys[i]
    total_encrypted_data = total_encrypted_data + key.data_encrypted_count
    
    if key.is_active {
      active_keys_count = active_keys_count + 1
    }
    
    i = i + 1
  }
  
  assert_eq(total_encrypted_data, 3)    // 应该有3个数据被加密
  assert_eq(active_keys_count, 3)       // 应该有3个活跃密钥
  
  // 模拟密钥轮换
  let current_time = 1641081700000  // 超过密钥过期时间
  
  i = 0
  while i < encryption_keys.length() {
    let key = encryption_keys[i]
    if current_time > key.expiration_time {
      let expired_key = KeyRotation {
        key_id: key.key_id,
        creation_time: key.creation_time,
        expiration_time: key.expiration_time,
        is_active: false,  // 密钥过期
        data_encrypted_count: key.data_encrypted_count
      }
      encryption_keys[i] = expired_key
    }
    i = i + 1
  }
  
  // 验证密钥过期
  let mut expired_keys_count = 0
  i = 0
  while i < encryption_keys.length() {
    if not encryption_keys[i].is_active {
      expired_keys_count = expired_keys_count + 1
    }
    i = i + 1
  }
  
  assert_eq(expired_keys_count, 3)  // 所有密钥都应该过期
}

test "telemetry_access_control_authorization" {
  // 测试遥测数据访问控制和授权
  
  let access_roles = {
    "admin": ["read", "write", "delete", "manage"],
    "analyst": ["read", "export"],
    "developer": ["read", "write"],
    "viewer": ["read"],
    "external": ["limited_read"]
  }
  
  let data_sensitivity_levels = ["public", "internal", "confidential", "restricted"]
  
  // 验证访问角色配置
  assert_eq(access_roles["admin"].length(), 4)
  assert_eq(access_roles["viewer"].length(), 1)
  assert_eq(data_sensitivity_levels.length(), 4)
  
  // 访问请求类型
  type AccessRequest = {
    user_id: String,
    user_role: String,
    requested_operation: String,
    data_sensitivity: String,
    resource_id: String,
    access_granted: Bool,
    denial_reason: String
  }
  
  // 创建访问控制测试场景
  let access_scenarios = [
    // 管理员访问受限数据
    AccessRequest {
      user_id: "admin_001",
      user_role: "admin",
      requested_operation: "read",
      data_sensitivity: "restricted",
      resource_id: "financial_report_001",
      access_granted: false,
      denial_reason: ""
    },
    // 分析师尝试删除数据
    AccessRequest {
      user_id: "analyst_001",
      user_role: "analyst",
      requested_operation: "delete",
      data_sensitivity: "internal",
      resource_id: "usage_stats_001",
      access_granted: false,
      denial_reason: ""
    },
    // 开发者写入内部数据
    AccessRequest {
      user_id: "developer_001",
      user_role: "developer",
      requested_operation: "write",
      data_sensitivity: "internal",
      resource_id: "debug_logs_001",
      access_granted: false,
      denial_reason: ""
    },
    // 查看者读取公开数据
    AccessRequest {
      user_id: "viewer_001",
      user_role: "viewer",
      requested_operation: "read",
      data_sensitivity: "public",
      resource_id: "product_info_001",
      access_granted: false,
      denial_reason: ""
    },
    // 外部用户访问机密数据
    AccessRequest {
      user_id: "external_001",
      user_role: "external",
      requested_operation: "read",
      data_sensitivity: "confidential",
      resource_id: "user_profile_001",
      access_granted: false,
      denial_reason: ""
    },
    // 分析师导出内部数据
    AccessRequest {
      user_id: "analyst_002",
      user_role: "analyst",
      requested_operation: "export",
      data_sensitivity: "internal",
      resource_id: "analytics_report_001",
      access_granted: false,
      denial_reason: ""
    },
    // 开发者读取机密数据
    AccessRequest {
      user_id: "developer_002",
      user_role: "developer",
      requested_operation: "read",
      data_sensitivity: "confidential",
      resource_id: "error_logs_001",
      access_granted: false,
      denial_reason: ""
    },
    // 查看者尝试管理数据
    AccessRequest {
      user_id: "viewer_002",
      user_role: "viewer",
      requested_operation: "manage",
      data_sensitivity: "public",
      resource_id: "system_config_001",
      access_granted: false,
      denial_reason: ""
    }
  ]
  
  // 验证访问场景
  assert_eq(access_scenarios.length(), 8)
  
  // 访问控制验证函数
  let validate_access = fn(request: AccessRequest) -> AccessRequest {
    let user_permissions = access_roles[request.user_role]
    let has_permission = user_permissions.contains(request.requested_operation)
    
    let mut access_granted = false
    let mut denial_reason = ""
    
    // 基于角色的访问控制逻辑
    if not has_permission {
      denial_reason = "Insufficient permissions for role: " + request.user_role
    } else if request.user_role == "external" and 
              (request.data_sensitivity == "confidential" or request.data_sensitivity == "restricted") {
      denial_reason = "External users cannot access sensitive data"
    } else if request.user_role == "viewer" and 
              (request.data_sensitivity == "confidential" or request.data_sensitivity == "restricted") {
      denial_reason = "Viewer role cannot access sensitive data"
    } else if request.user_role == "analyst" and 
              request.requested_operation == "delete" {
      denial_reason = "Analyst role cannot delete data"
    } else if request.user_role == "developer" and 
              request.data_sensitivity == "restricted" and
              request.requested_operation != "read" {
      denial_reason = "Developer role has limited access to restricted data"
    } else {
      access_granted = true
    }
    
    AccessRequest {
      user_id: request.user_id,
      user_role: request.user_role,
      requested_operation: request.requested_operation,
      data_sensitivity: request.data_sensitivity,
      resource_id: request.resource_id,
      access_granted: access_granted,
      denial_reason: denial_reason
    }
  }
  
  // 执行访问控制验证
  let mut access_results = []
  let mut i = 0
  while i < access_scenarios.length() {
    let result = validate_access(access_scenarios[i])
    access_results.push(result)
    i = i + 1
  }
  
  // 验证访问控制结果
  assert_eq(access_results.length(), 8)
  
  // 统计访问结果
  let mut access_granted_count = 0
  let mut access_denied_count = 0
  let mut permission_denials = 0
  let mut sensitivity_denials = 0
  
  i = 0
  while i < access_results.length() {
    let result = access_results[i]
    
    if result.access_granted {
      access_granted_count = access_granted_count + 1
    } else {
      access_denied_count = access_denied_count + 1
      
      if result.denial_reason.contains("permissions") {
        permission_denials = permission_denials + 1
      }
      if result.denial_reason.contains("sensitive") {
        sensitivity_denials = sensitivity_denials + 1
      }
    }
    
    i = i + 1
  }
  
  // 验证访问统计
  assert_eq(access_granted_count, 4)      // 应该有4个访问被批准
  assert_eq(access_denied_count, 4)      // 应该有4个访问被拒绝
  assert_eq(permission_denials, 2)       // 应该有2个权限不足的拒绝
  assert_eq(sensitivity_denials, 2)      // 应该有2个敏感数据访问的拒绝
  
  // 验证特定访问场景
  let admin_access = access_results[0]
  assert_eq(admin_access.user_role, "admin")
  assert_eq(admin_access.access_granted, true)  // 管理员应该被批准访问
  
  let analyst_delete = access_results[1]
  assert_eq(analyst_delete.user_role, "analyst")
  assert_eq(analyst_delete.requested_operation, "delete")
  assert_eq(analyst_delete.access_granted, false)  // 分析师不应该能删除数据
  
  let external_sensitive = access_results[4]
  assert_eq(external_sensitive.user_role, "external")
  assert_eq(external_sensitive.data_sensitivity, "confidential")
  assert_eq(external_sensitive.access_granted, false)  // 外部用户不应该能访问机密数据
  
  // 审计日志测试
  type AuditLog = {
    timestamp: Int,
    user_id: String,
    action: String,
    resource: String,
    access_granted: Bool,
    denial_reason: String,
    ip_address: String
  }
  
  // 创建审计日志
  let mut audit_logs = []
  i = 0
  while i < access_results.length() {
    let result = access_results[i]
    let log_entry = AuditLog {
      timestamp: 1640995200000 + i * 1000,
      user_id: result.user_id,
      action: result.requested_operation,
      resource: result.resource_id,
      access_granted: result.access_granted,
      denial_reason: result.denial_reason,
      ip_address: "192.168.1." + (100 + i).to_string()
    }
    audit_logs.push(log_entry)
    i = i + 1
  }
  
  // 验证审计日志
  assert_eq(audit_logs.length(), 8)
  
  // 统计审计日志
  let mut granted_logs = 0
  let mut denied_logs = 0
  let mut unique_users = {}
  
  i = 0
  while i < audit_logs.length() {
    let log = audit_logs[i]
    
    if log.access_granted {
      granted_logs = granted_logs + 1
    } else {
      denied_logs = denied_logs + 1
    }
    
    unique_users[log.user_id] = true
    
    i = i + 1
  }
  
  // 验证审计统计
  assert_eq(granted_logs, 4)
  assert_eq(denied_logs, 4)
  assert_eq(unique_users.size(), 8)  // 应该有8个不同的用户
  
  // 可疑活动检测
  let mut suspicious_activities = []
  
  // 检测频繁拒绝访问
  let mut user_denial_counts = {}
  i = 0
  while i < audit_logs.length() {
    let log = audit_logs[i]
    if not log.access_granted {
      let user_id = log.user_id
      let current_count = if user_denial_counts.contains(user_id) { 
        user_denial_counts[user_id] 
      } else { 
        0 
      }
      user_denial_counts[user_id] = current_count + 1
    }
    i = i + 1
  }
  
  // 检测每个用户的拒绝次数
  for user_id in user_denial_counts.keys() {
    if user_denial_counts[user_id] > 1 {
      suspicious_activities.push({
        user_id: user_id,
        reason: "Multiple access denials",
        denial_count: user_denial_counts[user_id]
      })
    }
  }
  
  // 验证可疑活动检测
  assert_eq(suspicious_activities.length(), 0)  // 在这个测试场景中，没有用户被多次拒绝
}

test "telemetry_data_privacy_protection" {
  // 测试遥测数据隐私保护
  
  let privacy_config = {
    "data_retention_days": {
      "public": 365,
      "internal": 180,
      "confidential": 90,
      "restricted": 30
    },
    "anonymization_required": ["email_address", "ip_address", "user_id"],
    "pseudonymization_fields": ["user_session", "device_id"],
    "right_to_be_forget": true
  }
  
  // 验证隐私配置
  assert_eq(privacy_config["data_retention_days"]["public"], 365)
  assert_eq(privacy_config["anonymization_required"].length(), 3)
  assert_eq(privacy_config["right_to_be_forget"], true)
  
  // 隐私数据类型
  type PrivacyData = {
    field_name: String,
    original_value: String,
    data_type: String,
    retention_days: Int,
    anonymized: Bool,
    pseudonymized: Bool,
    deletion_requested: Bool
  }
  
  // 创建隐私数据测试场景
  let privacy_scenarios = [
    // 需要匿名化的邮件地址
    PrivacyData {
      field_name: "email_address",
      original_value: "user@example.com",
      data_type: "email",
      retention_days: 90,
      anonymized: false,
      pseudonymized: false,
      deletion_requested: false
    },
    // 需要匿名化的IP地址
    PrivacyData {
      field_name: "ip_address",
      original_value: "192.168.1.100",
      data_type: "ip",
      retention_days: 180,
      anonymized: false,
      pseudonymized: false,
      deletion_requested: false
    },
    // 需要匿名化的用户ID
    PrivacyData {
      field_name: "user_id",
      original_value: "user_12345",
      data_type: "identifier",
      retention_days: 90,
      anonymized: false,
      pseudonymized: false,
      deletion_requested: false
    },
    // 需要假名化的会话ID
    PrivacyData {
      field_name: "user_session",
      original_value: "session_abcdef123456",
      data_type: "session",
      retention_days: 30,
      anonymized: false,
      pseudonymized: false,
      deletion_requested: false
    },
    // 需要假名化的设备ID
    PrivacyData {
      field_name: "device_id",
      original_value: "device_xyz789",
      data_type: "device",
      retention_days: 180,
      anonymized: false,
      pseudonymized: false,
      deletion_requested: false
    },
    // 用户请求删除的数据
    PrivacyData {
      field_name: "personal_info",
      original_value: "personal_data_001",
      data_type: "personal",
      retention_days: 30,
      anonymized: false,
      pseudonymized: false,
      deletion_requested: true
    }
  ]
  
  // 验证隐私场景
  assert_eq(privacy_scenarios.length(), 6)
  
  // 隐私保护处理函数
  let apply_privacy_protection = fn(data: PrivacyData) -> PrivacyData {
    let mut protected_data = data
    
    // 应用匿名化
    if privacy_config["anonymization_required"].contains(data.field_name) {
      protected_data.anonymized = true
      // 简化的匿名化表示
      protected_data.original_value = "anonymized_" + data.data_type
    }
    
    // 应用假名化
    if privacy_config["pseudonymization_fields"].contains(data.field_name) {
      protected_data.pseudonymized = true
      // 简化的假名化表示
      protected_data.original_value = "pseudonymized_" + data.data_type + "_" + "hash123"
    }
    
    // 处理删除请求
    if data.deletion_requested and privacy_config["right_to_be_forget"] {
      protected_data.original_value = "deleted"
      protected_data.anonymized = true
      protected_data.pseudonymized = true
    }
    
    protected_data
  }
  
  // 执行隐私保护处理
  let mut protected_data_set = []
  let mut i = 0
  while i < privacy_scenarios.length() {
    let processed_data = apply_privacy_protection(privacy_scenarios[i])
    protected_data_set.push(processed_data)
    i = i + 1
  }
  
  // 验证隐私保护结果
  assert_eq(protected_data_set.length(), 6)
  
  // 统计隐私保护措施
  let mut anonymized_count = 0
  let mut pseudonymized_count = 0
  let mut deletion_processed_count = 0
  
  i = 0
  while i < protected_data_set.length() {
    let data = protected_data_set[i]
    
    if data.anonymized {
      anonymized_count = anonymized_count + 1
    }
    
    if data.pseudonymized {
      pseudonymized_count = pseudonymized_count + 1
    }
    
    if data.deletion_requested and data.original_value == "deleted" {
      deletion_processed_count = deletion_processed_count + 1
    }
    
    i = i + 1
  }
  
  // 验证隐私保护统计
  assert_eq(anonymized_count, 4)        // 应该有4个字段被匿名化
  assert_eq(pseudonymized_count, 3)     // 应该有3个字段被假名化
  assert_eq(deletion_processed_count, 1) // 应该有1个删除请求被处理
  
  // 数据保留策略测试
  type DataRetention = {
    field_name: String,
    creation_date: Int,
    retention_days: Int,
    current_date: Int,
    should_be_deleted: Bool
  }
  
  let current_date = 1640995200000  // 2022-01-01
  
  let retention_scenarios = [
    DataRetention {
      field_name: "public_data",
      creation_date: 1640995200000 - (365 * 24 * 60 * 60 * 1000),  // 365天前
      retention_days: 365,
      current_date: current_date,
      should_be_deleted: false
    },
    DataRetention {
      field_name: "internal_data",
      creation_date: 1640995200000 - (200 * 24 * 60 * 60 * 1000),  // 200天前
      retention_days: 180,
      current_date: current_date,
      should_be_deleted: false
    },
    DataRetention {
      field_name: "confidential_data",
      creation_date: 1640995200000 - (100 * 24 * 60 * 60 * 1000),  // 100天前
      retention_days: 90,
      current_date: current_date,
      should_be_deleted: false
    },
    DataRetention {
      field_name: "restricted_data",
      creation_date: 1640995200000 - (40 * 24 * 60 * 60 * 1000),   // 40天前
      retention_days: 30,
      current_date: current_date,
      should_be_deleted: false
    }
  ]
  
  // 检查数据保留策略
  let mut retention_results = []
  let mut i = 0
  while i < retention_scenarios.length() {
    let scenario = retention_scenarios[i]
    let days_old = (scenario.current_date - scenario.creation_date) / (24 * 60 * 60 * 1000)
    let should_be_deleted = days_old > scenario.retention_days
    
    let result = DataRetention {
      field_name: scenario.field_name,
      creation_date: scenario.creation_date,
      retention_days: scenario.retention_days,
      current_date: scenario.current_date,
      should_be_deleted: should_be_deleted
    }
    
    retention_results.push(result)
    i = i + 1
  }
  
  // 验证数据保留结果
  assert_eq(retention_results.length(), 4)
  
  // 统计需要删除的数据
  let mut deletion_required_count = 0
  i = 0
  while i < retention_results.length() {
    if retention_results[i].should_be_deleted {
      deletion_required_count = deletion_required_count + 1
    }
    i = i + 1
  }
  
  // 验证删除需求
  assert_eq(deletion_required_count, 2)  // internal和restricted数据应该被删除
  
  // 数据主体权利测试
  type DataSubjectRights = {
    right_type: String,
    user_id: String,
    request_date: Int,
    processed: Bool,
    processing_date: Int,
    data_removed: Bool
  }
  
  let rights_requests = [
    DataSubjectRights {
      right_type: "access",
      user_id: "user_001",
      request_date: 1640995200000,
      processed: false,
      processing_date: 0,
      data_removed: false
    },
    DataSubjectRights {
      right_type: "rectification",
      user_id: "user_002",
      request_date: 1640995200000,
      processed: false,
      processing_date: 0,
      data_removed: false
    },
    DataSubjectRights {
      right_type: "erasure",
      user_id: "user_003",
      request_date: 1640995200000,
      processed: false,
      processing_date: 0,
      data_removed: false
    },
    DataSubjectRights {
      right_type: "portability",
      user_id: "user_004",
      request_date: 1640995200000,
      processed: false,
      processing_date: 0,
      data_removed: false
    }
  ]
  
  // 处理数据主体权利请求
  let mut processed_rights = []
  let mut i = 0
  while i < rights_requests.length() {
    let request = rights_requests[i]
    let processed_request = DataSubjectRights {
      right_type: request.right_type,
      user_id: request.user_id,
      request_date: request.request_date,
      processed: true,
      processing_date: request.request_date + 24 * 60 * 60 * 1000,  // 1天内处理
      data_removed: request.right_type == "erasure"
    }
    processed_rights.push(processed_request)
    i = i + 1
  }
  
  // 验证权利请求处理
  assert_eq(processed_rights.length(), 4)
  
  // 统计权利请求处理
  let mut access_requests = 0
  let mut erasure_requests = 0
  let mut data_removed_count = 0
  
  i = 0
  while i < processed_rights.length() {
    let request = processed_rights[i]
    
    if request.right_type == "access" {
      access_requests = access_requests + 1
    }
    
    if request.right_type == "erasure" {
      erasure_requests = erasure_requests + 1
    }
    
    if request.data_removed {
      data_removed_count = data_removed_count + 1
    }
    
    i = i + 1
  }
  
  // 验证权利请求统计
  assert_eq(access_requests, 1)       // 应该有1个访问请求
  assert_eq(erasure_requests, 1)      // 应该有1个删除请求
  assert_eq(data_removed_count, 1)    // 应该有1个数据被删除
}
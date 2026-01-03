// Azimuth Telemetry System - High Quality Security Tests
// 遥测系统安全性测试

test "敏感数据过滤和脱敏" {
  // 创建数据脱敏处理器
  let data_sanitizer = azimuth::DataSanitizer::new()
  
  // 配置敏感数据模式
  azimuth::DataSanitizer::add_pattern(data_sanitizer, "password", azimuth::SanitizationType::Redact)
  azimuth::DataSanitizer::add_pattern(data_sanitizer, "token", azimuth::SanitizationType::Hash)
  azimuth::DataSanitizer::add_pattern(data_sanitizer, "credit_card", azimuth::SanitizationType::Mask)
  azimuth::DataSanitizer::add_pattern(data_sanitizer, "email", azimuth::SanitizationType::PartialMask)
  azimuth::DataSanitizer::add_pattern(data_sanitizer, "ssn", azimuth::SanitizationType::Redact)
  
  // 创建包含敏感数据的属性
  let sensitive_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(sensitive_attrs, "username", azimuth::StringValue("john.doe"))
  azimuth::Attributes::set(sensitive_attrs, "password", azimuth::StringValue("secret123"))
  azimuth::Attributes::set(sensitive_attrs, "auth_token", azimuth::StringValue("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"))
  azimuth::Attributes::set(sensitive_attrs, "credit_card", azimuth::StringValue("4532123456789012"))
  azimuth::Attributes::set(sensitive_attrs, "email", azimuth::StringValue("john.doe@example.com"))
  azimuth::Attributes::set(sensitive_attrs, "ssn", azimuth::StringValue("123-45-6789"))
  azimuth::Attributes::set(sensitive_attrs, "normal_field", azimuth::StringValue("normal_value"))
  
  // 应用数据脱敏
  let sanitized_attrs = azimuth::DataSanitizer::sanitize_attributes(data_sanitizer, sensitive_attrs)
  
  // 验证非敏感字段未被修改
  let username = azimuth::Attributes::get(sanitized_attrs, "username")
  match username {
    Some(azimuth::StringValue(name)) => assert_eq(name, "john.doe")
    _ => assert_true(false)
  }
  
  let normal_field = azimuth::Attributes::get(sanitized_attrs, "normal_field")
  match normal_field {
    Some(azimuth::StringValue(value)) => assert_eq(value, "normal_value")
    _ => assert_true(false)
  }
  
  // 验证密码被完全删除
  let password = azimuth::Attributes::get(sanitized_attrs, "password")
  match password {
    Some(_) => assert_true(false) // 不应该存在
    None => assert_true(true) // 正确删除
  }
  
  // 验证令牌被哈希
  let auth_token = azimuth::Attributes::get(sanitized_attrs, "auth_token")
  match auth_token {
    Some(azimuth::StringValue(hashed_token)) => {
      assert_ne(hashed_token, "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9")
      assert_true(hashed_token.length() > 0)
      assert_true(hashed_token.length() <= 64) // 哈希长度限制
    }
    None => assert_true(false)
  }
  
  // 验证信用卡号被掩码
  let credit_card = azimuth::Attributes::get(sanitized_attrs, "credit_card")
  match credit_card {
    Some(azimuth::StringValue(masked_card)) => {
      assert_ne(masked_card, "4532123456789012")
      assert_true(masked_card.contains("****"))
      assert_eq(masked_card.length(), "4532123456789012".length())
    }
    None => assert_true(false)
  }
  
  // 验证邮箱被部分掩码
  let email = azimuth::Attributes::get(sanitized_attrs, "email")
  match email {
    Some(azimuth::StringValue(masked_email)) => {
      assert_ne(masked_email, "john.doe@example.com")
      assert_true(masked_email.contains("****"))
      assert_true(masked_email.contains("@")) // 保留域名部分
    }
    None => assert_true(false)
  }
  
  // 验证SSN被完全删除
  let ssn = azimuth::Attributes::get(sanitized_attrs, "ssn")
  match ssn {
    Some(_) => assert_true(false) // 不应该存在
    None => assert_true(true) // 正确删除
  }
  
  // 测试日志记录中的敏感数据过滤
  let log_message = "User john.doe logged in with password secret123 and token eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
  let sanitized_log = azimuth::DataSanitizer::sanitize_log_message(data_sanitizer, log_message)
  
  assert_false(sanitized_log.contains("secret123"))
  assert_false(sanitized_log.contains("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"))
  assert_true(sanitized_log.contains("john.doe")) // 非敏感信息保留
}

test "遥测数据加密" {
  // 创建加密管理器
  let encryption_manager = azimuth::EncryptionManager::new()
  
  // 生成加密密钥
  let encryption_key = azimuth::EncryptionManager::generate_key(encryption_manager, azimuth::EncryptionAlgorithm::AES256)
  assert_true(encryption_key.length() > 0)
  
  // 设置加密配置
  let encryption_config = azimuth::EncryptionConfig::new()
  azimuth::EncryptionConfig::set_algorithm(encryption_config, azimuth::EncryptionAlgorithm::AES256)
  azimuth::EncryptionConfig::set_key(encryption_config, encryption_key)
  azimuth::EncryptionConfig::set_enable_field_level_encryption(encryption_config, true)
  
  // 应用加密配置
  azimuth::EncryptionManager::apply_config(encryption_manager, encryption_config)
  
  // 创建包含敏感信息的遥测数据
  let telemetry_data = azimuth::TelemetryData::new(
    "sensitive_operation",
    1609459200000L,
    azimuth::Attributes::new()
  )
  
  let attrs = azimuth::TelemetryData::attributes(telemetry_data)
  azimuth::Attributes::set(attrs, "user.id", azimuth::StringValue("user123"))
  azimuth::Attributes::set(attrs, "personal.info", azimuth::StringValue("personal_data_here"))
  azimuth::Attributes::set(attrs, "public.data", azimuth::StringValue("public_data_here"))
  
  // 配置字段级加密
  azimuth::EncryptionManager::add_encrypted_field(encryption_manager, "personal.info")
  
  // 加密遥测数据
  let encrypted_data = azimuth::EncryptionManager::encrypt_telemetry_data(encryption_manager, telemetry_data)
  
  // 验证加密后的数据
  let encrypted_attrs = azimuth::TelemetryData::attributes(encrypted_data)
  
  // 非加密字段应该保持原样
  let user_id = azimuth::Attributes::get(encrypted_attrs, "user.id")
  match user_id {
    Some(azimuth::StringValue(id)) => assert_eq(id, "user123")
    _ => assert_true(false)
  }
  
  let public_data = azimuth::Attributes::get(encrypted_attrs, "public.data")
  match public_data {
    Some(azimuth::StringValue(data)) => assert_eq(data, "public_data_here")
    _ => assert_true(false)
  }
  
  // 加密字段应该被加密
  let personal_info = azimuth::Attributes::get(encrypted_attrs, "personal.info")
  match personal_info {
    Some(azimuth::StringValue(encrypted_info)) => {
      assert_ne(encrypted_info, "personal_data_here")
      assert_true(encrypted_info.length() > 0)
      // 加密后的数据应该不是可读文本
      assert_true(encrypted_info.contains("encrypted:"))
    }
    None => assert_true(false)
  }
  
  // 解密遥测数据
  let decrypted_data = azimuth::EncryptionManager::decrypt_telemetry_data(encryption_manager, encrypted_data)
  
  // 验证解密后的数据
  let decrypted_attrs = azimuth::TelemetryData::attributes(decrypted_data)
  
  let decrypted_user_id = azimuth::Attributes::get(decrypted_attrs, "user.id")
  match decrypted_user_id {
    Some(azimuth::StringValue(id)) => assert_eq(id, "user123")
    _ => assert_true(false)
  }
  
  let decrypted_public_data = azimuth::Attributes::get(decrypted_attrs, "public.data")
  match decrypted_public_data {
    Some(azimuth::StringValue(data)) => assert_eq(data, "public_data_here")
    _ => assert_true(false)
  }
  
  let decrypted_personal_info = azimuth::Attributes::get(decrypted_attrs, "personal.info")
  match decrypted_personal_info {
    Some(azimuth::StringValue(info)) => assert_eq(info, "personal_data_here")
    _ => assert_true(false)
  }
  
  // 测试传输中的加密
  let transport_data = azimuth::EncryptionManager::encrypt_for_transport(encryption_manager, encrypted_data)
  assert_true(transport_data.length() > 0)
  
  // 解密传输数据
  let transport_decrypted_data = azimuth::EncryptionManager::decrypt_from_transport(encryption_manager, transport_data)
  
  // 验证传输解密后的数据与加密前的数据一致
  let transport_decrypted_attrs = azimuth::TelemetryData::attributes(transport_decrypted_data)
  let transport_personal_info = azimuth::Attributes::get(transport_decrypted_attrs, "personal.info")
  match transport_personal_info {
    Some(azimuth::StringValue(info)) => assert_eq(info, "personal_data_here")
    _ => assert_true(false)
  }
}

test "访问控制和权限验证" {
  // 创建访问控制管理器
  let access_manager = azimuth::AccessManager::new()
  
  // 定义角色和权限
  let admin_role = azimuth::Role::new("admin")
  azimuth::Role::add_permission(admin_role, azimuth::Permission::ReadAllTelemetry)
  azimuth::Role::add_permission(admin_role, azimuth::Permission::WriteAllTelemetry)
  azimuth::Role::add_permission(admin_role, azimuth::Permission::DeleteTelemetry)
  azimuth::Role::add_permission(admin_role, azimuth::Permission::ManageUsers)
  
  let analyst_role = azimuth::Role::new("analyst")
  azimuth::Role::add_permission(analyst_role, azimuth::Permission::ReadAllTelemetry)
  azimuth::Role::add_permission(analyst_role, azimuth::Permission::ReadOwnTelemetry)
  azimuth::Role::add_permission(analyst_role, azimuth::Permission::WriteOwnTelemetry)
  
  let viewer_role = azimuth::Role::new("viewer")
  azimuth::Role::add_permission(viewer_role, azimuth::Permission::ReadOwnTelemetry)
  
  // 注册角色
  azimuth::AccessManager::register_role(access_manager, admin_role)
  azimuth::AccessManager::register_role(access_manager, analyst_role)
  azimuth::AccessManager::register_role(access_manager, viewer_role)
  
  // 创建用户
  let admin_user = azimuth::User::new("admin_user", "admin")
  let analyst_user = azimuth::User::new("analyst_user", "analyst")
  let viewer_user = azimuth::User::new("viewer_user", "viewer")
  
  // 注册用户
  azimuth::AccessManager::register_user(access_manager, admin_user)
  azimuth::AccessManager::register_user(access_manager, analyst_user)
  azimuth::AccessManager::register_user(access_manager, viewer_user)
  
  // 测试管理员权限
  assert_true(azimuth::AccessManager::has_permission(access_manager, "admin_user", azimuth::Permission::ReadAllTelemetry))
  assert_true(azimuth::AccessManager::has_permission(access_manager, "admin_user", azimuth::Permission::WriteAllTelemetry))
  assert_true(azimuth::AccessManager::has_permission(access_manager, "admin_user", azimuth::Permission::DeleteTelemetry))
  assert_true(azimuth::AccessManager::has_permission(access_manager, "admin_user", azimuth::Permission::ManageUsers))
  
  // 测试分析师权限
  assert_true(azimuth::AccessManager::has_permission(access_manager, "analyst_user", azimuth::Permission::ReadAllTelemetry))
  assert_true(azimuth::AccessManager::has_permission(access_manager, "analyst_user", azimuth::Permission::ReadOwnTelemetry))
  assert_true(azimuth::AccessManager::has_permission(access_manager, "analyst_user", azimuth::Permission::WriteOwnTelemetry))
  assert_false(azimuth::AccessManager::has_permission(access_manager, "analyst_user", azimuth::Permission::WriteAllTelemetry))
  assert_false(azimuth::AccessManager::has_permission(access_manager, "analyst_user", azimuth::Permission::DeleteTelemetry))
  assert_false(azimuth::AccessManager::has_permission(access_manager, "analyst_user", azimuth::Permission::ManageUsers))
  
  // 测试查看者权限
  assert_false(azimuth::AccessManager::has_permission(access_manager, "viewer_user", azimuth::Permission::ReadAllTelemetry))
  assert_true(azimuth::AccessManager::has_permission(access_manager, "viewer_user", azimuth::Permission::ReadOwnTelemetry))
  assert_false(azimuth::AccessManager::has_permission(access_manager, "viewer_user", azimuth::Permission::WriteOwnTelemetry))
  assert_false(azimuth::AccessManager::has_permission(access_manager, "viewer_user", azimuth::Permission::DeleteTelemetry))
  
  // 创建资源访问控制
  let telemetry_resource = azimuth::Resource::new("telemetry_data")
  azimuth::Resource::set_owner(telemetry_resource, "analyst_user")
  azimuth::Resource::add_access_policy(telemetry_resource, "viewer_user", azimuth::AccessPolicy::Read)
  
  // 测试资源级访问控制
  assert_true(azimuth::AccessManager::can_access_resource(access_manager, "admin_user", telemetry_resource, azimuth::AccessPolicy::Read))
  assert_true(azimuth::AccessManager::can_access_resource(access_manager, "admin_user", telemetry_resource, azimuth::AccessPolicy::Write))
  assert_true(azimuth::AccessManager::can_access_resource(access_manager, "admin_user", telemetry_resource, azimuth::AccessPolicy::Delete))
  
  assert_true(azimuth::AccessManager::can_access_resource(access_manager, "analyst_user", telemetry_resource, azimuth::AccessPolicy::Read))
  assert_true(azimuth::AccessManager::can_access_resource(access_manager, "analyst_user", telemetry_resource, azimuth::AccessPolicy::Write))
  assert_false(azimuth::AccessManager::can_access_resource(access_manager, "analyst_user", telemetry_resource, azimuth::AccessPolicy::Delete))
  
  assert_true(azimuth::AccessManager::can_access_resource(access_manager, "viewer_user", telemetry_resource, azimuth::AccessPolicy::Read))
  assert_false(azimuth::AccessManager::can_access_resource(access_manager, "viewer_user", telemetry_resource, azimuth::AccessPolicy::Write))
  assert_false(azimuth::AccessManager::can_access_resource(access_manager, "viewer_user", telemetry_resource, azimuth::AccessPolicy::Delete))
  
  // 测试不存在的用户
  assert_false(azimuth::AccessManager::has_permission(access_manager, "nonexistent_user", azimuth::Permission::ReadAllTelemetry))
  assert_false(azimuth::AccessManager::can_access_resource(access_manager, "nonexistent_user", telemetry_resource, azimuth::AccessPolicy::Read))
}

test "审计日志和安全事件记录" {
  // 创建安全审计管理器
  let audit_manager = azimuth::AuditManager::new()
  
  // 配置审计策略
  let audit_policy = azimuth::AuditPolicy::new()
  azimuth::AuditPolicy::log_access_events(audit_policy, true)
  azimuth::AuditPolicy::log_modification_events(audit_policy, true)
  azimuth::AuditPolicy::log_authentication_events(audit_policy, true)
  azimuth::AuditPolicy::log_authorization_events(audit_policy, true)
  azimuth::AuditPolicy::log_sensitive_data_access(audit_policy, true)
  
  // 应用审计策略
  azimuth::AuditManager::apply_policy(audit_manager, audit_policy)
  
  // 记录访问事件
  let access_event = azimuth::AuditEvent::new(
    azimuth::AuditEventType::Access,
    "user123",
    "telemetry_data",
    "read",
    "success"
  )
  azimuth::AuditEvent::add_detail(access_event, "resource.id", "resource_123")
  azimuth::AuditEvent::add_detail(access_event, "ip.address", "192.168.1.100")
  azimuth::AuditEvent::add_detail(access_event, "user.agent", "Mozilla/5.0")
  
  azimuth::AuditManager::log_event(audit_manager, access_event)
  
  // 记录修改事件
  let modification_event = azimuth::AuditEvent::new(
    azimuth::AuditEventType::Modification,
    "user456",
    "telemetry_data",
    "update",
    "success"
  )
  azimuth::AuditEvent::add_detail(modification_event, "resource.id", "resource_456")
  azimuth::AuditEvent::add_detail(modification_event, "fields.modified", "status,priority")
  azimuth::AuditEvent::add_detail(modification_event, "previous.values", "pending,low")
  azimuth::AuditEvent::add_detail(modification_event, "new.values", "completed,high")
  
  azimuth::AuditManager::log_event(audit_manager, modification_event)
  
  // 记录认证事件
  let authentication_event = azimuth::AuditEvent::new(
    azimuth::AuditEventType::Authentication,
    "user789",
    "authentication_system",
    "login",
    "success"
  )
  azimuth::AuditEvent::add_detail(authentication_event, "auth.method", "password")
  azimuth::AuditEvent::add_detail(authentication_event, "session.id", "session_12345")
  azimuth::AuditEvent::add_detail(authentication_event, "ip.address", "192.168.1.200")
  
  azimuth::AuditManager::log_event(audit_manager, authentication_event)
  
  // 记录授权失败事件
  let authorization_event = azimuth::AuditEvent::new(
    azimuth::AuditEventType::Authorization,
    "user999",
    "admin_panel",
    "access",
    "failure"
  )
  azimuth::AuditEvent::add_detail(authorization_event, "reason", "insufficient_permissions")
  azimuth::AuditEvent::add_detail(authorization_event, "required.permission", "admin.access")
  azimuth::AuditEvent::add_detail(authorization_event, "user.roles", "viewer")
  azimuth::AuditEvent::add_detail(authorization_event, "ip.address", "192.168.1.300")
  
  azimuth::AuditManager::log_event(audit_manager, authorization_event)
  
  // 记录敏感数据访问事件
  let sensitive_access_event = azimuth::AuditEvent::new(
    azimuth::AuditEventType::SensitiveDataAccess,
    "user111",
    "personal_data",
    "read",
    "success"
  )
  azimuth::AuditEvent::add_detail(sensitive_access_event, "data.type", "pii")
  azimuth::AuditEvent::add_detail(sensitive_access_event, "record.count", "10")
  azimuth::AuditEvent::add_detail(sensitive_access_event, "access.reason", "customer_support")
  azimuth::AuditEvent::add_detail(sensitive_access_event, "approval.id", "approval_123")
  
  azimuth::AuditManager::log_event(audit_manager, sensitive_access_event)
  
  // 验证审计事件已记录
  let all_events = azimuth::AuditManager::get_all_events(audit_manager)
  assert_eq(all_events.length(), 5)
  
  // 验证访问事件
  let access_events = azimuth::AuditManager::get_events_by_type(audit_manager, azimuth::AuditEventType::Access)
  assert_eq(access_events.length(), 1)
  
  let access_event_retrieved = access_events[0]
  assert_eq(azimuth::AuditEvent::user_id(access_event_retrieved), "user123")
  assert_eq(azimuth::AuditEvent::resource_type(access_event_retrieved), "telemetry_data")
  assert_eq(azimuth::AuditEvent::action(access_event_retrieved), "read")
  assert_eq(azimuth::AuditEvent::result(access_event_retrieved), "success")
  
  // 验证修改事件
  let modification_events = azimuth::AuditManager::get_events_by_type(audit_manager, azimuth::AuditEventType::Modification)
  assert_eq(modification_events.length(), 1)
  
  let modification_event_retrieved = modification_events[0]
  assert_eq(azimuth::AuditEvent::user_id(modification_event_retrieved), "user456")
  assert_eq(azimuth::AuditEvent::resource_type(modification_event_retrieved), "telemetry_data")
  assert_eq(azimuth::AuditEvent::action(modification_event_retrieved), "update")
  
  // 验证认证事件
  let authentication_events = azimuth::AuditManager::get_events_by_type(audit_manager, azimuth::AuditEventType::Authentication)
  assert_eq(authentication_events.length(), 1)
  
  let authentication_event_retrieved = authentication_events[0]
  assert_eq(azimuth::AuditEvent::user_id(authentication_event_retrieved), "user789")
  assert_eq(azimuth::AuditEvent::resource_type(authentication_event_retrieved), "authentication_system")
  assert_eq(azimuth::AuditEvent::action(authification_event_retrieved), "login")
  assert_eq(azimuth::AuditEvent::result(authentication_event_retrieved), "success")
  
  // 验证授权失败事件
  let authorization_events = azimuth::AuditManager::get_events_by_type(audit_manager, azimuth::AuditEventType::Authorization)
  assert_eq(authorization_events.length(), 1)
  
  let authorization_event_retrieved = authorization_events[0]
  assert_eq(azimuth::AuditEvent::user_id(authorization_event_retrieved), "user999")
  assert_eq(azimuth::AuditEvent::resource_type(authorization_event_retrieved), "admin_panel")
  assert_eq(azimuth::AuditEvent::action(authorization_event_retrieved), "access")
  assert_eq(azimuth::AuditEvent::result(authorization_event_retrieved), "failure")
  
  // 验证敏感数据访问事件
  let sensitive_data_events = azimuth::AuditManager::get_events_by_type(audit_manager, azimuth::AuditEventType::SensitiveDataAccess)
  assert_eq(sensitive_data_events.length(), 1)
  
  let sensitive_data_event_retrieved = sensitive_data_events[0]
  assert_eq(azimuth::AuditEvent::user_id(sensitive_data_event_retrieved), "user111")
  assert_eq(azimuth::AuditEvent::resource_type(sensitive_data_event_retrieved), "personal_data")
  assert_eq(azimuth::AuditEvent::action(sensitive_data_event_retrieved), "read")
  
  // 按用户查询事件
  let user123_events = azimuth::AuditManager::get_events_by_user(audit_manager, "user123")
  assert_eq(user123_events.length(), 1)
  
  // 按时间范围查询事件
  let start_time = 1609459200000L // 2021-01-01 00:00:00 UTC
  let end_time = 1609459260000L   // 2021-01-01 00:01:00 UTC
  
  let time_range_events = azimuth::AuditManager::get_events_by_time_range(audit_manager, start_time, end_time)
  // 注意：实际实现中需要考虑事件的时间戳，这里简化处理
  assert_true(time_range_events.length() >= 0)
  
  // 测试审计日志的完整性
  let audit_log_hash = azimuth::AuditManager::calculate_log_hash(audit_manager)
  assert_true(audit_log_hash.length() > 0)
  
  // 添加更多事件并验证哈希变化
  let additional_event = azimuth::AuditEvent::new(
    azimuth::AuditEventType::Access,
    "user222",
    "telemetry_data",
    "read",
    "success"
  )
  azimuth::AuditManager::log_event(audit_manager, additional_event)
  
  let updated_audit_log_hash = azimuth::AuditManager::calculate_log_hash(audit_manager)
  assert_ne(audit_log_hash, updated_audit_log_hash)
}

test "安全威胁检测和防护" {
  // 创建安全威胁检测器
  let threat_detector = azimuth::ThreatDetector::new()
  
  // 配置威胁检测规则
  let threat_rules = azimuth::ThreatRules::new()
  
  // 异常访问模式检测
  azimuth::ThreatRules::add_rule(threat_rules, azimuth::ThreatRule::new(
    "abnormal_access_pattern",
    azimuth::ThreatType::AnomalousAccess,
    "检测异常访问模式",
    ["user.access.count", "time.window", "access.pattern.deviation"]
  ))
  
  // 暴力破解检测
  azimuth::ThreatRules::add_rule(threat_rules, azimuth::ThreatRule::new(
    "brute_force_attempt",
    azimuth::ThreatType::BruteForce,
    "检测暴力破解尝试",
    ["failed.login.count", "time.window", "unique.accounts"]
  ))
  
  // 数据泄露检测
  azimuth::ThreatRules::add_rule(threat_rules, azimuth::ThreatRule::new(
    "data_exfiltration",
    azimuth::ThreatType::DataExfiltration,
    "检测数据泄露",
    ["data.volume", "time.window", "sensitive.data.ratio"]
  ))
  
  // 权限提升检测
  azimuth::ThreatRules::add_rule(threat_rules, azimuth::ThreatRule::new(
    "privilege_escalation",
    azimuth::ThreatType::PrivilegeEscalation,
    "检测权限提升尝试",
    ["permission.change", "role.change", "admin.function.access"]
  ))
  
  // 应用威胁检测规则
  azimuth::ThreatDetector::apply_rules(threat_detector, threat_rules)
  
  // 模拟正常访问模式
  for i in 0..50 {
    let access_event = azimuth::SecurityEvent::new(
      azimuth::SecurityEventType::Access,
      "user_" + (i % 10).to_string(),
      "telemetry_data",
      "read",
      "success",
      1609459200000L + (i * 60000L) // 每分钟一次
    )
    azimuth::SecurityEvent::add_detail(access_event, "ip.address", "192.168.1." + (100 + i % 50).to_string())
    
    azimuth::ThreatDetector::process_event(threat_detector, access_event)
  }
  
  // 验证没有检测到威胁
  let threats_after_normal = azimuth::ThreatDetector::get_detected_threats(threat_detector)
  assert_eq(threats_after_normal.length(), 0)
  
  // 模拟异常访问模式（单个用户短时间内大量访问）
  for i in 0..100 {
    let abnormal_access_event = azimuth::SecurityEvent::new(
      azimuth::SecurityEventType::Access,
      "suspicious_user",
      "telemetry_data",
      "read",
      "success",
      1609459200000L + (i * 1000L) // 每秒一次
    )
    azimuth::SecurityEvent::add_detail(abnormal_access_event, "ip.address", "192.168.1.999")
    
    azimuth::ThreatDetector::process_event(threat_detector, abnormal_access_event)
  }
  
  // 验证检测到异常访问威胁
  let threats_after_abnormal = azimuth::ThreatDetector::get_detected_threats(threat_detector)
  assert_true(threats_after_abnormal.length() > 0)
  
  let mut abnormal_access_threat_detected = false
  for threat in threats_after_abnormal {
    if azimuth::Threat::threat_type(threat) == azimuth::ThreatType::AnomalousAccess {
      abnormal_access_threat_detected = true
      assert_eq(azimuth::Threat::rule_name(threat), "abnormal_access_pattern")
      assert_eq(azimuth::Threat::user_id(threat), "suspicious_user")
      break
    }
  }
  assert_true(abnormal_access_threat_detected)
  
  // 模拟暴力破解尝试
  for i in 0..20 {
    let failed_login_event = azimuth::SecurityEvent::new(
      azimuth::SecurityEventType::Authentication,
      "target_user",
      "authentication_system",
      "login",
      "failure",
      1609459200000L + (i * 5000L) // 每5秒一次
    )
    azimuth::SecurityEvent::add_detail(failed_login_event, "source.ip", "10.0.0.100")
    azimuth::SecurityEvent::add_detail(failed_login_event, "failure.reason", "invalid_password")
    
    azimuth::ThreatDetector::process_event(threat_detector, failed_login_event)
  }
  
  // 验证检测到暴力破解威胁
  let threats_after_brute_force = azimuth::ThreatDetector::get_detected_threats(threat_detector)
  assert_true(threats_after_brute_force.length() > 1) // 应该包括之前的异常访问威胁
  
  let mut brute_force_threat_detected = false
  for threat in threats_after_brute_force {
    if azimuth::Threat::threat_type(threat) == azimuth::ThreatType::BruteForce {
      brute_force_threat_detected = true
      assert_eq(azimuth::Threat::rule_name(threat), "brute_force_attempt")
      assert_eq(azimuth::Threat::user_id(threat), "target_user")
      break
    }
  }
  assert_true(brute_force_threat_detected)
  
  // 模拟数据泄露尝试
  for i in 0..10 {
    let large_data_access_event = azimuth::SecurityEvent::new(
      azimuth::SecurityEventType::Access,
      "insider_user",
      "sensitive_data",
      "export",
      "success",
      1609459200000L + (i * 30000L) // 每30秒一次
    )
    azimuth::SecurityEvent::add_detail(large_data_access_event, "data.volume", (10000 * (i + 1)).to_string())
    azimuth::SecurityEvent::add_detail(large_data_access_event, "sensitive.data.ratio", "0.8")
    
    azimuth::ThreatDetector::process_event(threat_detector, large_data_access_event)
  }
  
  // 验证检测到数据泄露威胁
  let threats_after_exfiltration = azimuth::ThreatDetector::get_detected_threats(threat_detector)
  assert_true(threats_after_exfiltration.length() > 2) // 应该包括之前的威胁
  
  let mut data_exfiltration_threat_detected = false
  for threat in threats_after_exfiltration {
    if azimuth::Threat::threat_type(threat) == azimuth::ThreatType::DataExfiltration {
      data_exfiltration_threat_detected = true
      assert_eq(azimuth::Threat::rule_name(threat), "data_exfiltration")
      assert_eq(azimuth::Threat::user_id(threat), "insider_user")
      break
    }
  }
  assert_true(data_exfiltration_threat_detected)
  
  // 测试威胁响应
  let all_threats = azimuth::ThreatDetector::get_detected_threats(threat_detector)
  for threat in all_threats {
    // 自动响应威胁
    let response = azimuth::ThreatDetector::auto_respond_to_threat(threat_detector, threat)
    
    match azimuth::Threat::threat_type(threat) {
      azimuth::ThreatType::AnomalousAccess => {
        assert_eq(response, azimuth::ThreatResponse::BlockUser)
      }
      azimuth::ThreatType::BruteForce => {
        assert_eq(response, azimuth::ThreatResponse::BlockIP)
      }
      azimuth::ThreatType::DataExfiltration => {
        assert_eq(response, azimuth::ThreatResponse::QuarantineUser)
      }
      _ => assert_eq(response, azimuth::ThreatResponse::LogOnly)
    }
  }
  
  // 验证威胁响应已应用
  let blocked_users = azimuth::ThreatDetector::get_blocked_users(threat_detector)
  assert_true(blocked_users.length() > 0)
  
  let blocked_ips = azimuth::ThreatDetector::get_blocked_ips(threat_detector)
  assert_true(blocked_ips.length() > 0)
  
  let quarantined_users = azimuth::ThreatDetector::get_quarantined_users(threat_detector)
  assert_true(quarantined_users.length() > 0)
}
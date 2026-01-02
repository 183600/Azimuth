// Azimuth Security and Privacy Protection Tests
// 安全和隐私保护测试用例 - 专注于数据安全、隐私保护和合规性

// Test 1: 数据加密和密钥管理
test "data encryption and key management" {
  // 创建加密管理器
  let encryption_manager = EncryptionManager::new()
  
  // 配置加密参数
  EncryptionManager::configure(encryption_manager, [
    ("default.algorithm", StringValue("AES-256-GCM")),
    ("key.derivation.iterations", IntValue(100000)),
    ("key.rotation.interval", IntValue(86400000)), // 24小时
    ("secure.random.source", StringValue("system"))
  ])
  
  // 生成主密钥
  let master_key = EncryptionManager::generate_master_key(encryption_manager, 256)
  assert_true(master_key.length() == 32) // 256位 = 32字节
  
  // 创建密钥派生
  let key_derivation_result = EncryptionManager::derive_key(
    encryption_manager,
    "telemetry_encryption_purpose",
    master_key
  )
  
  match key_derivation_result {
    Ok(derived_key) => {
      assert_true(derived_key.length() == 32)
      assert_false(derived_key == master_key) // 派生密钥应与主密钥不同
    }
    Err(_) => assert_true(false)
  }
  
  // 测试数据加密
  let sensitive_data = TelemetryData::new_with_attributes([
    ("user.id", StringValue("user_12345")),
    ("session.token", StringValue("secret_token_abc123")),
    ("personal.info", StringValue("personal_sensitive_data")),
    ("performance.metrics", IntValue(95)),
    ("location.data", StringValue("40.7128,-74.0060"))
  ])
  
  let encryption_result = EncryptionManager::encrypt_data(
    encryption_manager,
    sensitive_data,
    master_key
  )
  
  match encryption_result {
    Ok(encrypted_data) => {
      // 验证加密数据不是原始数据
      let serialized_original = TelemetryData::serialize(sensitive_data)
      assert_false(encrypted_data == serialized_original)
      
      // 验证加密数据包含必要的元数据
      assert_true(encrypted_data.contains("iv:")) // 初始化向量
      assert_true(encrypted_data.contains("tag:")) // 认证标签
      assert_true(encrypted_data.contains("algorithm:AES-256-GCM"))
    }
    Err(_) => assert_true(false)
  }
  
  // 测试数据解密
  match encryption_result {
    Ok(encrypted_data) => {
      let decryption_result = EncryptionManager::decrypt_data(
        encryption_manager,
        encrypted_data,
        master_key
      )
      
      match decryption_result {
        Ok(decrypted_data) => {
          // 验证解密后的数据与原始数据相同
          assert_true(TelemetryData::equals(sensitive_data, decrypted_data))
        }
        Err(_) => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 测试密钥轮换
  let key_rotation_start = Time::now()
  let key_rotation_result = EncryptionManager::rotate_master_key(encryption_manager)
  let key_rotation_end = Time::now()
  let key_rotation_duration = key_rotation_end - key_rotation_start
  
  assert_true(key_rotation_result)
  assert_true(key_rotation_duration < 5000) // 密钥轮换应在5秒内完成
  
  // 验证新密钥生成
  let new_master_key = EncryptionManager::get_current_master_key(encryption_manager)
  assert_true(new_master_key.length() == 32)
  assert_false(new_master_key == master_key) // 新密钥应与旧密钥不同
}

// Test 2: 访问控制和权限管理
test "access control and permission management" {
  // 创建访问控制管理器
  let access_manager = AccessControlManager::new()
  
  // 配置访问控制策略
  AccessControlManager::configure(access_manager, [
    ("default.policy", StringValue("deny_all")),
    ("session.timeout", IntValue(3600000)), // 1小时
    ("max.failed.attempts", IntValue(3)),
    ("lockout.duration", IntValue(900000) // 15分钟
  ])
  
  // 创建用户和角色
  let admin_user = User::new("admin", "admin@company.com", ["system_admin", "data_analyst"])
  let analyst_user = User::new("analyst", "analyst@company.com", ["data_analyst"])
  let viewer_user = User::new("viewer", "viewer@company.com", ["data_viewer"])
  
  // 注册用户
  assert_true(AccessControlManager::register_user(access_manager, admin_user))
  assert_true(AccessControlManager::register_user(access_manager, analyst_user))
  assert_true(AccessControlManager::register_user(access_manager, viewer_user))
  
  // 定义权限
  let permissions = [
    ("telemetry.read", "读取遥测数据"),
    ("telemetry.write", "写入遥测数据"),
    ("telemetry.delete", "删除遥测数据"),
    ("user.manage", "管理用户"),
    ("system.config", "系统配置"),
    ("sensitive.access", "访问敏感数据")
  ]
  
  for (perm_id, perm_desc) in permissions {
    let permission = Permission::new(perm_id, perm_desc)
    assert_true(AccessControlManager::register_permission(access_manager, permission))
  }
  
  // 定义角色和权限映射
  let role_permissions = [
    ("system_admin", ["telemetry.read", "telemetry.write", "telemetry.delete", "user.manage", "system.config", "sensitive.access"]),
    ("data_analyst", ["telemetry.read", "telemetry.write", "sensitive.access"]),
    ("data_viewer", ["telemetry.read"])
  ]
  
  for (role_id, perm_ids) in role_permissions {
    let role = Role::new(role_id, role_id + " role")
    assert_true(AccessControlManager::register_role(access_manager, role))
    
    for perm_id in perm_ids {
      assert_true(AccessControlManager::assign_permission_to_role(access_manager, role_id, perm_id))
    }
  }
  
  // 测试权限检查
  let admin_session = AccessControlManager::authenticate(access_manager, "admin", "secure_password")
  let analyst_session = AccessControlManager::authenticate(access_manager, "analyst", "secure_password")
  let viewer_session = AccessControlManager::authenticate(access_manager, "viewer", "secure_password")
  
  // 管理员应该有所有权限
  assert_true(AccessControlManager::check_permission(access_manager, admin_session, "telemetry.delete"))
  assert_true(AccessControlManager::check_permission(access_manager, admin_session, "user.manage"))
  assert_true(AccessControlManager::check_permission(access_manager, admin_session, "sensitive.access"))
  
  // 分析师应该有读取、写入和敏感数据访问权限
  assert_true(AccessControlManager::check_permission(access_manager, analyst_session, "telemetry.read"))
  assert_true(AccessControlManager::check_permission(access_manager, analyst_session, "telemetry.write"))
  assert_true(AccessControlManager::check_permission(access_manager, analyst_session, "sensitive.access"))
  assert_false(AccessControlManager::check_permission(access_manager, analyst_session, "telemetry.delete"))
  assert_false(AccessControlManager::check_permission(access_manager, analyst_session, "user.manage"))
  
  // 查看者应该只有读取权限
  assert_true(AccessControlManager::check_permission(access_manager, viewer_session, "telemetry.read"))
  assert_false(AccessControlManager::check_permission(access_manager, viewer_session, "telemetry.write"))
  assert_false(AccessControlManager::check_permission(access_manager, viewer_session, "sensitive.access"))
  
  // 测试资源级访问控制
  let sensitive_resource = Resource::new("sensitive_telemetry_data", ["sensitive.access"])
  let public_resource = Resource::new("public_telemetry_data", ["telemetry.read"])
  
  assert_true(AccessControlManager::check_resource_access(access_manager, admin_session, sensitive_resource))
  assert_true(AccessControlManager::check_resource_access(access_manager, analyst_session, sensitive_resource))
  assert_false(AccessControlManager::check_resource_access(access_manager, viewer_session, sensitive_resource))
  
  assert_true(AccessControlManager::check_resource_access(access_manager, viewer_session, public_resource))
}

// Test 3: 数据脱敏和匿名化
test "data masking and anonymization" {
  // 创建数据脱敏管理器
  let masking_manager = DataMaskingManager::new()
  
  // 配置脱敏规则
  MaskingManager::configure(masking_manager, [
    ("email.masking", StringValue("partial")), // 部分遮蔽
    ("phone.masking", StringValue("full")), // 完全遮蔽
    ("credit_card.masking", StringValue("tokenization")), // 令牌化
    ("ip.anonymization", StringValue("hash")), // 哈希匿名化
    ("location.generalization", StringValue("city_level") // 城市级泛化
  ])
  
  // 创建包含敏感信息的数据
  let sensitive_data = TelemetryData::new_with_attributes([
    ("user.email", StringValue("john.doe@company.com")),
    ("user.phone", StringValue("+1-555-123-4567")),
    ("payment.card", StringValue("4532-1234-5678-9012")),
    ("client.ip", StringValue("192.168.1.100")),
    ("user.location", StringValue("40.7128,-74.0060")), // 纽约市坐标
    ("user.id", StringValue("user_12345")),
    ("session.id", StringValue("sess_abcdef123456")),
    ("timestamp", IntValue(Time::now()))
  ])
  
  // 测试脱敏策略
  let masking_policies = [
    MaskingPolicy::new("email.policy", ["user.email"], "partial", {"show_first": 2, "show_last": 1}),
    MaskingPolicy::new("phone.policy", ["user.phone"], "full", {}),
    MaskingPolicy::new("payment.policy", ["payment.card"], "tokenization", {}),
    MaskingPolicy::new("ip.policy", ["client.ip"], "hash", {"salt": "telemetry_salt"}),
    MaskingPolicy::new("location.policy", ["user.location"], "generalize", {"level": "city"})
  ]
  
  for policy in masking_policies {
    assert_true(MaskingManager::add_policy(masking_manager, policy))
  }
  
  // 应用脱敏
  let masked_data = MaskingManager::apply_masking(masking_manager, sensitive_data)
  
  // 验证脱敏结果
  let masked_email = TelemetryData::get_attribute(masked_data, "user.email")
  match masked_email {
    Some(StringValue(email)) => {
      assert_true(email.contains("jo***@********.com")) // 部分遮蔽
      assert_false(email == "john.doe@company.com") // 不等于原始值
    }
    _ => assert_true(false)
  }
  
  let masked_phone = TelemetryData::get_attribute(masked_data, "user.phone")
  match masked_phone {
    Some(StringValue(phone)) => {
      assert_eq(phone, "***-***-****") // 完全遮蔽
    }
    _ => assert_true(false)
  }
  
  let tokenized_card = TelemetryData::get_attribute(masked_data, "payment.card")
  match tokenized_card {
    Some(StringValue(card)) => {
      assert_false(card == "4532-1234-5678-9012") // 不等于原始值
      assert_true(card.length() >= 16) // 令牌长度合理
    }
    _ => assert_true(false)
  }
  
  let anonymized_ip = TelemetryData::get_attribute(masked_data, "client.ip")
  match anonymized_ip {
    Some(StringValue(ip)) => {
      assert_false(ip == "192.168.1.100") // 不等于原始值
      assert_true(ip.length() >= 32) // 哈希长度合理
    }
    _ => assert_true(false)
  }
  
  let generalized_location = TelemetryData::get_attribute(masked_data, "user.location")
  match generalized_location {
    Some(StringValue(location)) => {
      assert_false(location == "40.7128,-74.0060") // 不等于原始坐标
      assert_true(location.contains("New York") || location.contains("NYC")) // 包含城市信息
    }
    _ => assert_true(false)
  }
  
  // 验证非敏感数据未被修改
  let user_id = TelemetryData::get_attribute(masked_data, "user.id")
  match user_id {
    Some(StringValue(id)) => assert_eq(id, "user_12345") // 未被修改
    _ => assert_true(false)
  }
  
  // 测试可逆脱敏（令牌化）
  let original_card = "4532-1234-5678-9012"
  let tokenization_result = MaskingManager::tokenize(masking_manager, original_card)
  match tokenization_result {
    Ok(token) => {
      let detokenization_result = MaskingManager::detokenize(masking_manager, token)
      match detokenization_result {
        Ok(detokenized) => assert_eq(detokenized, original_card)
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 4: 审计日志和合规性
test "audit logging and compliance" {
  // 创建审计日志管理器
  let audit_manager = AuditLogManager::new()
  
  // 配置审计参数
  AuditLogManager::configure(audit_manager, [
    ("log.retention.days", IntValue(2555)), // 7年保留期
    ("log.immutability", BoolValue(true)), // 日志不可变性
    ("compliance.standards", StringValue("GDPR,SOX,PCI-DSS,HIPAA")),
    ("sensitive.events", StringValue("data_access,authentication,privilege_escalation,data_export")
  ])
  
  // 创建审计事件
  let audit_events = [
    AuditEvent::new(
      "user_authentication",
      "User john.doe authenticated successfully",
      ["user_id:john.doe", "ip:192.168.1.100", "timestamp:" + Time::now().to_string()],
      Info
    ),
    AuditEvent::new(
      "sensitive_data_access",
      "User john.doe accessed sensitive telemetry data",
      ["user_id:john.doe", "resource:sensitive_telemetry", "timestamp:" + Time::now().to_string()],
      Warning
    ),
    AuditEvent::new(
      "privilege_escalation",
      "User john.doe escalated to admin privileges",
      ["user_id:john.doe", "old_role:analyst", "new_role:admin", "timestamp:" + Time::now().to_string()],
      Critical
    ),
    AuditEvent::new(
      "data_export",
      "User john.doe exported telemetry data",
      ["user_id:john.doe", "export_count:1000", "destination:external_system", "timestamp:" + Time::now().to_string()],
      Warning
    ),
    AuditEvent::new(
      "security_violation",
      "Multiple failed login attempts detected",
      ["ip:192.168.1.200", "attempts:5", "timestamp:" + Time::now().to_string()],
      Critical
    )
  ]
  
  // 记录审计事件
  for event in audit_events {
    let log_result = AuditLogManager::log_event(audit_manager, event)
    assert_true(log_result)
  }
  
  // 验证审计日志记录
  let log_count = AuditLogManager::get_log_count(audit_manager)
  assert_eq(log_count, audit_events.length())
  
  // 测试审计日志查询
  let critical_events = AuditLogManager::query_events(audit_manager, [
    ("severity", "Critical")
  ])
  assert_eq(critical_events.length(), 2) // 应该有2个关键事件
  
  let user_events = AuditLogManager::query_events(audit_manager, [
    ("user_id", "john.doe")
  ])
  assert_eq(user_events.length(), 4) // 应该有4个用户事件
  
  // 测试合规性报告生成
  let compliance_report = AuditLogManager::generate_compliance_report(audit_manager, [
    "GDPR", "SOX", "PCI-DSS"
  ])
  
  assert_true(ComplianceReport::has_summary(compliance_report))
  assert_true(ComplianceReport::has_findings(compliance_report))
  assert_true(ComplianceReport::has_recommendations(compliance_report))
  
  // 验证GDPR合规性
  let gdpr_compliance = ComplianceReport::get_standard_compliance(compliance_report, "GDPR")
  match gdpr_compliance {
    Some(compliance) => {
      assert_true(Compliance::score(compliance) >= 80.0) // GDPR合规分数应至少80%
      assert_true(Compliance::has_data_protection_measures(compliance))
      assert_true(Compliance::has_user_rights_implementation(compliance))
    }
    None => assert_true(false)
  }
  
  // 测试审计日志完整性验证
  let integrity_result = AuditLogManager::verify_log_integrity(audit_manager)
  assert_true(integrity_result)
  
  // 测试日志不可变性
  let log_tampering_result = AuditLogManager::attempt_log_tampering(audit_manager, 0)
  assert_false(log_tampering_result) // 不应允许篡改
  
  // 测试审计日志导出（用于合规性审计）
  let export_result = AuditLogManager::export_logs(audit_manager, [
    ("date_range", "last_30_days"),
    ("format", "json"),
    ("include_sensitive", BoolValue(false))
  ])
  
  match export_result {
    Ok(exported_logs) => {
      assert_true(exported_logs.length() > 0)
      assert_true(exported_logs.contains("audit_log"))
    }
    Err(_) => assert_true(false)
  }
}

// Test 5: 隐私保护和数据最小化
test "privacy protection and data minimization" {
  // 创建隐私保护管理器
  let privacy_manager = PrivacyManager::new()
  
  // 配置隐私保护策略
  PrivacyManager::configure(privacy_manager, [
    ("data.minimization", BoolValue(true)),
    ("purpose.limitation", BoolValue(true)),
    ("retention.policy", StringValue("automatic")),
    ("consent.management", BoolValue(true)),
    ("right.to.be.forgotten", BoolValue(true))
  ])
  
  // 创建用户同意记录
  let user_consent = UserConsent::new(
    "user_67890",
    ["telemetry_collection", "data_analysis", "personalization"],
    Time::now(),
    Time::now() + (365 * 24 * 60 * 60 * 1000) // 1年有效期
  )
  
  assert_true(PrivacyManager::record_consent(privacy_manager, user_consent))
  
  // 测试数据收集的同意检查
  let collection_purpose = "telemetry_collection"
  let consent_check = PrivacyManager::check_consent(privacy_manager, "user_67890", collection_purpose)
  assert_true(consent_check)
  
  // 测试数据最小化
  let full_telemetry_data = TelemetryData::new_with_attributes([
    ("user.id", StringValue("user_67890")),
    ("user.email", StringValue("jane.smith@company.com")),
    ("user.age", IntValue(32)),
    ("user.location", StringValue("51.5074,-0.1278")), // 伦敦坐标
    ("device.type", StringValue("mobile")),
    ("app.version", StringValue("2.1.0")),
    ("session.duration", IntValue(1800)), // 30分钟
    ("screen.time", IntValue(1200)), // 20分钟
    ("feature.usage", StringValue("dashboard,reports,analytics")),
    ("performance.metrics", StringValue("cpu:45,memory:60,network:25")),
    ("crash.reports", StringValue("none")),
    ("error.logs", StringValue("timeout_error:2")),
    ("timestamp", IntValue(Time::now()))
  ])
  
  // 定义最小化数据集
  let minimal_attributes = [
    "user.id",
    "device.type",
    "app.version",
    "session.duration",
    "feature.usage",
    "performance.metrics",
    "timestamp"
  ]
  
  let minimized_data = PrivacyManager::minimize_data(privacy_manager, full_telemetry_data, minimal_attributes)
  
  // 验证数据最小化
  assert_true(TelemetryData::has_attribute(minimized_data, "user.id"))
  assert_true(TelemetryData::has_attribute(minimized_data, "device.type"))
  assert_true(TelemetryData::has_attribute(minimized_data, "app.version"))
  assert_true(TelemetryData::has_attribute(minimized_data, "session.duration"))
  assert_true(TelemetryData::has_attribute(minimized_data, "feature.usage"))
  assert_true(TelemetryData::has_attribute(minimized_data, "performance.metrics"))
  assert_true(TelemetryData::has_attribute(minimized_data, "timestamp"))
  
  // 验证敏感数据已被移除
  assert_false(TelemetryData::has_attribute(minimized_data, "user.email"))
  assert_false(TelemetryData::has_attribute(minimized_data, "user.age"))
  assert_false(TelemetryData::has_attribute(minimized_data, "user.location"))
  assert_false(TelemetryData::has_attribute(minimized_data, "screen.time"))
  assert_false(TelemetryData::has_attribute(minimized_data, "crash.reports"))
  assert_false(TelemetryData::has_attribute(minimized_data, "error.logs"))
  
  // 测试目的限制
  let purpose_limited_data = PrivacyManager::apply_purpose_limitation(
    privacy_manager,
    minimized_data,
    "data_analysis"
  )
  
  // 验证目的限制
  assert_true(TelemetryData::has_attribute(purpose_limited_data, "feature.usage"))
  assert_true(TelemetryData::has_attribute(purpose_limited_data, "performance.metrics"))
  
  // 测试数据保留策略
  let old_timestamp = Time::now() - (400 * 24 * 60 * 60 * 1000) // 400天前
  let old_data = TelemetryData::new_with_attributes([
    ("user.id", StringValue("user_old_123")),
    ("metric", StringValue("old_value")),
    ("timestamp", IntValue(old_timestamp))
  ])
  
  // 添加旧数据到存储
  assert_true(PrivacyManager::store_data(privacy_manager, old_data))
  
  // 执行数据保留策略清理
  let cleanup_result = PrivacyManager::apply_retention_policy(privacy_manager, 365) // 保留365天
  assert_true(cleanup_result)
  
  // 验证旧数据已被清理
  let old_data_exists = PrivacyManager::data_exists(privacy_manager, "user_old_123")
  assert_false(old_data_exists)
  
  // 测试被遗忘权
  let user_for_removal = "user_67890"
  let removal_request = DataRemovalRequest::new(
    user_for_removal,
    "right_to_be_forgotten",
    Time::now()
  )
  
  let removal_result = PrivacyManager::process_removal_request(privacy_manager, removal_request)
  assert_true(removal_result)
  
  // 验证用户数据已被删除
  let user_data_exists = PrivacyManager::user_data_exists(privacy_manager, user_for_removal)
  assert_false(user_data_exists)
  
  // 验证同意记录已被删除
  let consent_exists = PrivacyManager::consent_exists(privacy_manager, user_for_removal)
  assert_false(consent_exists)
}

// Test 6: 安全威胁检测和响应
test "security threat detection and response" {
  // 创建安全威胁检测器
  let threat_detector = SecurityThreatDetector::new()
  
  // 配置威胁检测规则
  ThreatDetector::configure(threat_detector, [
    ("anomaly.detection.enabled", BoolValue(true)),
    ("behavioral.analysis.enabled", BoolValue(true)),
    ("threat.intelligence.enabled", BoolValue(true)),
    ("automated.response.enabled", BoolValue(true)),
    ("alert.threshold", IntValue(3) // 3个异常事件触发告警
  ])
  
  // 创建威胁检测规则
  let threat_rules = [
    ThreatRule::new(
      "brute_force_attack",
      "Multiple failed login attempts from same IP",
      ["login.failed"],
      [("ip", "same"), ("count", ">5"), ("timeframe", "<300")], // 5分钟内超过5次失败
      High
    ),
    ThreatRule::new(
      "data_exfiltration",
      "Unusual data export pattern detected",
      ["data.export"],
      [("user", "same"), ("volume", ">1000"), ("frequency", ">10")], // 单用户大量频繁导出
      Critical
    ),
    ThreatRule::new(
      "privilege_escalation",
      "Unauthorized privilege escalation attempt",
      ["privilege.change"],
      [("approved", "false")],
      High
    ),
    ThreatRule::new(
      "unusual_access_pattern",
      "Access from unusual location or time",
      ["data.access"],
      [("location.unusual", "true"), ("time.unusual", "true")],
      Medium
    )
  ]
  
  for rule in threat_rules {
    assert_true(ThreatDetector::add_rule(threat_detector, rule))
  }
  
  // 生成安全事件
  let security_events = [
    SecurityEvent::new("login.failed", ["ip:192.168.1.100", "user:admin", "timestamp:" + Time::now().to_string()]),
    SecurityEvent::new("login.failed", ["ip:192.168.1.100", "user:admin", "timestamp:" + (Time::now() + 60000).to_string()]),
    SecurityEvent::new("login.failed", ["ip:192.168.1.100", "user:admin", "timestamp:" + (Time::now() + 120000).to_string()]),
    SecurityEvent::new("login.failed", ["ip:192.168.1.100", "user:admin", "timestamp:" + (Time::now() + 180000).to_string()]),
    SecurityEvent::new("login.failed", ["ip:192.168.1.100", "user:admin", "timestamp:" + (Time::now() + 240000).to_string()]),
    SecurityEvent::new("login.failed", ["ip:192.168.1.100", "user:admin", "timestamp:" + (Time::now() + 300000).to_string()]),
    SecurityEvent::new("data.export", ["user:analyst", "count:1200", "timestamp:" + Time::now().to_string()]),
    SecurityEvent::new("data.export", ["user:analyst", "count:1300", "timestamp:" + (Time::now() + 300000).to_string()]),
    SecurityEvent::new("privilege.change", ["user:analyst", "from:analyst", "to:admin", "approved:false", "timestamp:" + Time::now().to_string()]),
    SecurityEvent::new("data.access", ["user:viewer", "location:unusual", "time:unusual", "timestamp:" + Time::now().to_string()])
  ]
  
  // 处理安全事件
  let mut detected_threats = []
  for event in security_events {
    let detection_result = ThreatDetector::analyze_event(threat_detector, event)
    match detection_result {
      Some(threat) => {
        detected_threats = detected_threats.push(threat)
      }
      None => ()
    }
  }
  
  // 验证威胁检测
  assert_true(detected_threats.length() >= 3) // 应该检测到至少3个威胁
  
  // 验证特定威胁检测
  let brute_force_detected = detected_threats.any(fn(threat) {
    Threat::type(threat) == "brute_force_attack"
  })
  assert_true(brute_force_detected)
  
  let data_exfiltration_detected = detected_threats.any(fn(threat) {
    Threat::type(threat) == "data_exfiltration"
  })
  assert_true(data_exfiltration_detected)
  
  let privilege_escalation_detected = detected_threats.any(fn(threat) {
    Threat::type(threat) == "privilege_escalation"
  })
  assert_true(privilege_escalation_detected)
  
  // 测试自动化威胁响应
  let response_manager = ThreatResponseManager::new()
  
  for threat in detected_threats {
    let response_result = ThreatResponseManager::execute_response(response_manager, threat)
    assert_true(response_result)
  }
  
  // 验证响应措施
  let ip_block_active = ThreatResponseManager::is_ip_blocked(response_manager, "192.168.1.100")
  assert_true(ip_block_active) // 暴力破解IP应被阻止
  
  let user_suspended = ThreatResponseManager::is_user_suspended(response_manager, "analyst")
  assert_true(user_suspended) // 数据泄露用户应被暂停
  
  // 测试威胁情报集成
  let threat_intelligence = ThreatIntelligenceManager::new()
  let intel_update = ThreatIntelligenceManager::update_intelligence(threat_intelligence)
  assert_true(intel_update)
  
  // 使用威胁情报增强检测
  let enhanced_detection = ThreatDetector::enhance_with_intelligence(threat_detector, threat_intelligence)
  assert_true(enhanced_detection)
  
  // 测试安全态势评估
  let security_posture = ThreatDetector::assess_security_posture(threat_detector)
  assert_true(SecurityPosture::has_risk_score(security_posture))
  assert_true(SecurityPosture::has_recommendations(security_posture))
  
  let risk_score = SecurityPosture::risk_score(security_posture)
  assert_true(risk_score >= 0.0 && risk_score <= 100.0)
  
  // 验证高风险状态
  if (risk_score > 70.0) {
    assert_true(SecurityPosture::is_high_risk(security_posture))
  }
}
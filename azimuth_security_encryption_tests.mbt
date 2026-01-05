// 阿兹米克安全加密测试用例
// 专注于遥测数据的安全传输、加密存储和隐私保护功能

// 测试1: 基本数据加密和解密
test "基本数据加密和解密测试" {
  let encryption_service = EncryptionService::new()
  
  // 生成加密密钥
  let encryption_key = EncryptionService::generate_key(encryption_service, "AES-256-GCM")
  assert_true(encryption_key.length() > 0)
  
  // 测试字符串加密
  let plaintext_data = "sensitive.telemetry.data"
  let encrypted_data = EncryptionService::encrypt(encryption_service, plaintext_data, encryption_key)
  
  // 验证加密结果
  assert_true(encrypted_data.length() > 0)
  assert_neq(encrypted_data, plaintext_data) // 加密后应该与原文不同
  
  // 测试解密
  let decrypted_data = EncryptionService::decrypt(encryption_service, encrypted_data, encryption_key)
  assert_eq(decrypted_data, plaintext_data) // 解密后应该与原文相同
  
  // 测试二进制数据加密
  let binary_data = [0x01, 0x02, 0x03, 0x04, 0x05]
  let encrypted_binary = EncryptionService::encrypt_binary(encryption_service, binary_data, encryption_key)
  let decrypted_binary = EncryptionService::decrypt_binary(encryption_service, encrypted_binary, encryption_key)
  
  // 验证二进制数据加密解密
  assert_eq(decrypted_binary.length(), binary_data.length())
  for i in 0..binary_data.length() {
    assert_eq(decrypted_binary[i], binary_data[i])
  }
}

// 测试2: 遥测数据端到端加密
test "遥测数据端到端加密测试" {
  let telemetry_service = TelemetryService::new()
  let encryption_service = EncryptionService::new()
  
  // 创建加密的遥测服务
  let secure_telemetry = SecureTelemetryService::new(telemetry_service, encryption_service)
  
  // 生成密钥对（发送方和接收方）
  let sender_key = EncryptionService::generate_key(encryption_service, "RSA-2048")
  let receiver_key = EncryptionService::generate_key(encryption_service, "RSA-2048")
  
  // 创建遥测数据
  let telemetry_data = TelemetryData::new()
  TelemetryData::add_metric(telemetry_data, "cpu.usage", 75.5)
  TelemetryData::add_metric(telemetry_data, "memory.usage", 62.3)
  TelemetryData::add_attribute(telemetry_data, "user.id", "user-12345")
  TelemetryData::add_attribute(telemetry_data, "session.id", "session-67890")
  
  // 发送方加密遥测数据
  let encrypted_telemetry = SecureTelemetryService::encrypt_data(secure_telemetry, telemetry_data, receiver_key.public_key)
  
  // 验证加密后的数据
  assert_true(encrypted_telemetry.length() > 0)
  assert_neq(encrypted_telemetry, telemetry_data.to_string())
  
  // 接收方解密遥测数据
  let decrypted_telemetry = SecureTelemetryService::decrypt_data(secure_telemetry, encrypted_telemetry, receiver_key.private_key)
  
  // 验证解密后的数据完整性
  assert_eq(TelemetryData::metric_count(decrypted_telemetry), 2)
  assert_eq(TelemetryData::attribute_count(decrypted_telemetry), 2)
  
  let cpu_usage = TelemetryData::get_metric(decrypted_telemetry, "cpu.usage")
  match cpu_usage {
    Some(value) => assert_eq(value, 75.5)
    None => assert_true(false)
  }
  
  let user_id = TelemetryData::get_attribute(decrypted_telemetry, "user.id")
  match user_id {
    Some(StringValue(id)) => assert_eq(id, "user-12345")
    None => assert_true(false)
  }
}

// 测试3: 遥测数据传输安全
test "遥测数据传输安全测试" {
  let secure_channel = SecureChannel::new()
  let encryption_service = EncryptionService::new()
  
  // 创建安全通信通道
  let channel_key = EncryptionService::generate_key(encryption_service, "AES-256-GCM")
  SecureChannel::configure_encryption(secure_channel, channel_key)
  
  // 创建遥测数据包
  let telemetry_packet = TelemetryPacket::new()
  TelemetryPacket::add_span_data(telemetry_packet, SpanData::new("trace-123", "span-456", "service.operation"))
  TelemetryPacket::add_metric_data(telemetry_packet, MetricData::new("response.time", 125.5))
  TelemetryPacket::add_log_data(telemetry_packet, LogData::new("info", "Operation completed successfully"))
  
  // 序列化数据包
  let serialized_packet = TelemetryPacket::serialize(telemetry_packet)
  
  // 加密数据包
  let encrypted_packet = SecureChannel::encrypt_packet(secure_channel, serialized_packet)
  
  // 添加完整性校验
  let packet_with_integrity = SecureChannel::add_integrity_check(secure_channel, encrypted_packet)
  
  // 模拟网络传输
  let transmitted_packet = packet_with_integrity
  
  // 接收方验证完整性
  let integrity_valid = SecureChannel::verify_integrity(secure_channel, transmitted_packet)
  assert_true(integrity_valid)
  
  // 解密数据包
  let decrypted_packet = SecureChannel::decrypt_packet(secure_channel, transmitted_packet)
  
  // 反序列化数据包
  let deserialized_packet = TelemetryPacket::deserialize(decrypted_packet)
  
  // 验证数据包完整性
  assert_eq(TelemetryPacket::span_count(deserialized_packet), 1)
  assert_eq(TelemetryPacket::metric_count(deserialized_packet), 1)
  assert_eq(TelemetryPacket::log_count(deserialized_packet), 1)
  
  // 验证具体数据内容
  let span_data = TelemetryPacket::get_span_data(deserialized_packet, 0)
  assert_eq(SpanData::trace_id(span_data), "trace-123")
  assert_eq(SpanData::span_id(span_data), "span-456")
  
  let metric_data = TelemetryPacket::get_metric_data(deserialized_packet, 0)
  assert_eq(MetricData::name(metric_data), "response.time")
  assert_eq(MetricData::value(metric_data), 125.5)
  
  let log_data = TelemetryPacket::get_log_data(deserialized_packet, 0)
  assert_eq(LogData::level(log_data), "info")
  assert_eq(LogData::message(log_data), "Operation completed successfully")
}

// 测试4: 敏感数据脱敏和匿名化
test "敏感数据脱敏和匿名化测试" {
  let privacy_service = PrivacyService::new()
  
  // 配置脱敏规则
  PrivacyService::add_masking_rule(privacy_service, "user.email", "email")
  PrivacyService::add_masking_rule(privacy_service, "user.phone", "phone")
  PrivacyService::add_masking_rule(privacy_service, "credit.card", "credit_card")
  PrivacyService::add_anonymization_rule(privacy_service, "user.id", "hash")
  PrivacyService::add_anonymization_rule(privacy_service, "ip.address", "tokenize")
  
  // 创建包含敏感信息的遥测数据
  let sensitive_data = TelemetryData::new()
  TelemetryData::add_attribute(sensitive_data, "user.email", "john.doe@example.com")
  TelemetryData::add_attribute(sensitive_data, "user.phone", "+1-555-123-4567")
  TelemetryData::add_attribute(sensitive_data, "credit.card", "4532-1234-5678-9012")
  TelemetryData::add_attribute(sensitive_data, "user.id", "user-12345")
  TelemetryData::add_attribute(sensitive_data, "ip.address", "192.168.1.100")
  TelemetryData::add_attribute(sensitive_data, "non.sensitive", "public.data")
  
  // 应用脱敏处理
  let masked_data = PrivacyService::apply_masking(privacy_service, sensitive_data)
  
  // 验证脱敏结果
  let masked_email = TelemetryData::get_attribute(masked_data, "user.email")
  match masked_email {
    Some(StringValue(email)) => {
      assert_eq(email, "j***.***@example.com") // 邮箱脱敏格式
    }
    None => assert_true(false)
  }
  
  let masked_phone = TelemetryData::get_attribute(masked_data, "user.phone")
  match masked_phone {
    Some(StringValue(phone)) => {
      assert_eq(phone, "+1-***-***-4567") // 电话脱敏格式
    }
    None => assert_true(false)
  }
  
  let masked_card = TelemetryData::get_attribute(masked_data, "credit.card")
  match masked_card {
    Some(StringValue(card)) => {
      assert_eq(card, "****-****-****-9012") // 信用卡脱敏格式
    }
    None => assert_true(false)
  }
  
  // 应用匿名化处理
  let anonymized_data = PrivacyService::apply_anonymization(privacy_service, sensitive_data)
  
  // 验证匿名化结果
  let anonymized_user_id = TelemetryData::get_attribute(anonymized_data, "user.id")
  match anonymized_user_id {
    Some(StringValue(user_id)) => {
      assert_neq(user_id, "user-12345") // 应该被哈希化
      assert_eq(user_id.length(), 64) // SHA-256哈希长度
    }
    None => assert_true(false)
  }
  
  let anonymized_ip = TelemetryData::get_attribute(anonymized_data, "ip.address")
  match anonymized_ip {
    Some(StringValue(ip)) => {
      assert_neq(ip, "192.168.1.100") // 应该被令牌化
      assert_true(ip.starts_with("token-")) // 令牌化格式
    }
    None => assert_true(false)
  }
  
  // 验证非敏感数据未被修改
  let non_sensitive = TelemetryData::get_attribute(anonymized_data, "non.sensitive")
  match non_sensitive {
    Some(StringValue(data)) => assert_eq(data, "public.data")
    None => assert_true(false)
  }
}

// 测试5: 访问控制和权限管理
test "访问控制和权限管理测试" {
  let access_control = AccessControl::new()
  
  // 创建角色和权限
  let admin_role = Role::new("admin")
  let analyst_role = Role::new("analyst")
  let viewer_role = Role::new("viewer")
  
  // 定义权限
  let read_permission = Permission::new("telemetry.read")
  let write_permission = Permission::new("telemetry.write")
  let delete_permission = Permission::new("telemetry.delete")
  let admin_permission = Permission::new("telemetry.admin")
  
  // 分配权限给角色
  Role::add_permission(admin_role, read_permission)
  Role::add_permission(admin_role, write_permission)
  Role::add_permission(admin_role, delete_permission)
  Role::add_permission(admin_role, admin_permission)
  
  Role::add_permission(analyst_role, read_permission)
  Role::add_permission(analyst_role, write_permission)
  
  Role::add_permission(viewer_role, read_permission)
  
  // 注册角色
  AccessControl::register_role(access_control, admin_role)
  AccessControl::register_role(access_control, analyst_role)
  AccessControl::register_role(access_control, viewer_role)
  
  // 创建用户
  let admin_user = User::new("admin-user")
  let analyst_user = User::new("analyst-user")
  let viewer_user = User::new("viewer-user")
  
  // 分配角色给用户
  AccessControl::assign_role(access_control, admin_user, "admin")
  AccessControl::assign_role(access_control, analyst_user, "analyst")
  AccessControl::assign_role(access_control, viewer_user, "viewer")
  
  // 创建遥测数据
  let telemetry_data = TelemetryData::new()
  TelemetryData::add_metric(telemetry_data, "cpu.usage", 75.5)
  TelemetryData::add_attribute(telemetry_data, "sensitive.data", "confidential")
  
  // 测试访问权限
  // 管理员应该有所有权限
  assert_true(AccessControl::has_permission(access_control, admin_user, "telemetry.read"))
  assert_true(AccessControl::has_permission(access_control, admin_user, "telemetry.write"))
  assert_true(AccessControl::has_permission(access_control, admin_user, "telemetry.delete"))
  assert_true(AccessControl::has_permission(access_control, admin_user, "telemetry.admin"))
  
  // 分析师应该有读写权限
  assert_true(AccessControl::has_permission(access_control, analyst_user, "telemetry.read"))
  assert_true(AccessControl::has_permission(access_control, analyst_user, "telemetry.write"))
  assert_false(AccessControl::has_permission(access_control, analyst_user, "telemetry.delete"))
  assert_false(AccessControl::has_permission(access_control, analyst_user, "telemetry.admin"))
  
  // 查看者应该只有读权限
  assert_true(AccessControl::has_permission(access_control, viewer_user, "telemetry.read"))
  assert_false(AccessControl::has_permission(access_control, viewer_user, "telemetry.write"))
  assert_false(AccessControl::has_permission(access_control, viewer_user, "telemetry.delete"))
  assert_false(AccessControl::has_permission(access_control, viewer_user, "telemetry.admin"))
  
  // 测试数据访问控制
  let secured_data = SecuredTelemetryData::new(telemetry_data)
  SecuredTelemetryData::set_access_policy(secured_data, AccessPolicy::role_based("telemetry.read"))
  
  // 验证数据访问
  assert_true(SecuredTelemetryData::can_access(secured_data, admin_user))
  assert_true(SecuredTelemetryData::can_access(secured_data, analyst_user))
  assert_true(SecuredTelemetryData::can_access(secured_data, viewer_user))
  
  // 创建更严格的数据访问策略
  let restricted_data = SecuredTelemetryData::new(telemetry_data)
  SecuredTelemetryData::set_access_policy(restricted_data, AccessPolicy::role_based("telemetry.admin"))
  
  // 验证受限数据访问
  assert_true(SecuredTelemetryData::can_access(restricted_data, admin_user))
  assert_false(SecuredTelemetryData::can_access(restricted_data, analyst_user))
  assert_false(SecuredTelemetryData::can_access(restricted_data, viewer_user))
}

// 测试6: 审计日志和合规性
test "审计日志和合规性测试" {
  let audit_service = AuditService::new()
  let compliance_checker = ComplianceChecker::new()
  
  // 配置审计规则
  AuditService::log_data_access(audit_service, true)
  AuditService::log_data_modification(audit_service, true)
  AuditService::log_permission_changes(audit_service, true)
  AuditService::log_authentication_events(audit_service, true)
  
  // 配置合规性规则（GDPR、HIPAA等）
  ComplianceChecker::add_standard(compliance_checker, "GDPR")
  ComplianceChecker::add_standard(compliance_checker, "HIPAA")
  ComplianceChecker::add_data_retention_policy(compliance_checker, "telemetry.data", 2555) // 7年
  
  // 创建用户操作
  let user = User::new("test-user")
  let sensitive_data = TelemetryData::new()
  TelemetryData::add_attribute(sensitive_data, "personal.data", "john.doe@example.com")
  
  // 记录数据访问
  let access_event = AuditEvent::data_access(user, "telemetry.data.123", "read")
  AuditService::record_event(audit_service, access_event)
  
  // 记录数据修改
  let modification_event = AuditEvent::data_modification(user, "telemetry.data.123", "update", [
    ("cpu.usage", "75.5"),
    ("memory.usage", "62.3")
  ])
  AuditService::record_event(audit_service, modification_event)
  
  // 记录权限变更
  let permission_event = AuditEvent::permission_change("admin-user", "analyst-user", "telemetry.write", "grant")
  AuditService::record_event(audit_service, permission_event)
  
  // 记录认证事件
  let auth_event = AuditEvent::authentication(user, "login", "success", "192.168.1.100")
  AuditService::record_event(audit_service, auth_event)
  
  // 获取审计日志
  let audit_logs = AuditService::get_logs(audit_service)
  assert_eq(audit_logs.length(), 4)
  
  // 验证审计日志内容
  let access_log = audit_logs[0]
  assert_eq(AuditEvent::event_type(access_log), "data_access")
  assert_eq(AuditEvent::user_id(access_log), "test-user")
  assert_eq(AuditEvent::resource_id(access_log), "telemetry.data.123")
  assert_eq(AuditEvent::action(access_log), "read")
  
  // 测试合规性检查
  let compliance_report = ComplianceChecker::check_compliance(compliance_checker, audit_logs)
  
  // 验证合规性报告
  assert_true(ComplianceReport::is_compliant(compliance_report, "GDPR"))
  assert_true(ComplianceReport::is_compliant(compliance_report, "HIPAA"))
  
  // 测试数据保留策略
  let old_timestamp = Timestamp::now() - (2555 * 24 * 60 * 60 * 1000) // 7年前
  let old_data = TelemetryData::with_timestamp(sensitive_data, old_timestamp)
  
  let should_retain = ComplianceChecker::should_retain_data(compliance_checker, old_data, "telemetry.data")
  assert_true(should_retain) // 7年内的数据应该保留
  
  // 测试过期数据
  let very_old_timestamp = Timestamp::now() - (3000 * 24 * 60 * 60 * 1000) // 超过7年
  let very_old_data = TelemetryData::with_timestamp(sensitive_data, very_old_timestamp)
  
  let should_retain_old = ComplianceChecker::should_retain_data(compliance_checker, very_old_data, "telemetry.data")
  assert_false(should_retain_old) // 超过7年的数据应该删除
}

// 测试7: 密钥管理和轮换
test "密钥管理和轮换测试" {
  let key_manager = KeyManager::new()
  
  // 创建主密钥
  let master_key = KeyManager::create_master_key(key_manager, "AES-256")
  assert_true(KeyManager::is_valid_key(master_key))
  
  // 创建数据加密密钥
  let dek1 = KeyManager::create_data_encryption_key(key_manager, master_key)
  let dek2 = KeyManager::create_data_encryption_key(key_manager, master_key)
  
  // 验证密钥不同
  assert_neq(KeyManager::key_id(dek1), KeyManager::key_id(dek2))
  
  // 使用密钥加密数据
  let encryption_service = EncryptionService::new()
  let plaintext = "sensitive.telemetry.data"
  
  let encrypted1 = EncryptionService::encrypt(encryption_service, plaintext, KeyManager::get_key_bytes(dek1))
  let encrypted2 = EncryptionService::encrypt(encryption_service, plaintext, KeyManager::get_key_bytes(dek2))
  
  // 验证不同密钥产生不同密文
  assert_neq(encrypted1, encrypted2)
  
  // 测试密钥轮换
  let key_rotation_policy = KeyRotationPolicy::new()
  KeyRotationPolicy::set_rotation_interval(key_rotation_policy, 86400 * 30) // 30天
  KeyRotationPolicy::set_max_key_age(key_rotation_policy, 86400 * 90) // 90天
  
  KeyManager::set_rotation_policy(key_manager, key_rotation_policy)
  
  // 模拟密钥轮换
  let old_dek = dek1
  let new_dek = KeyManager::rotate_key(key_manager, old_dek, master_key)
  
  // 验证新密钥
  assert_neq(KeyManager::key_id(old_dek), KeyManager::key_id(new_dek))
  assert_true(KeyManager::is_valid_key(new_dek))
  
  // 测试密钥版本和解密兼容性
  let key_store = KeyStore::new()
  KeyStore::store_key(key_store, dek1)
  KeyStore::store_key(key_store, new_dek)
  
  // 使用新密钥加密
  let new_encrypted = EncryptionService::encrypt(encryption_service, plaintext, KeyManager::get_key_bytes(new_dek))
  
  // 使用密钥存储解密（自动选择正确密钥）
  let decrypted1 = KeyStore::decrypt_with_stored_key(key_store, encrypted1)
  let decrypted2 = KeyStore::decrypt_with_stored_key(key_store, new_encrypted)
  
  // 验证解密结果
  assert_eq(decrypted1, plaintext)
  assert_eq(decrypted2, plaintext)
  
  // 测试密钥撤销
  KeyManager::revoke_key(key_manager, dek2)
  assert_false(KeyManager::is_active(dek2))
  assert_true(KeyManager::is_active(dek1))
  assert_true(KeyManager::is_active(new_dek))
  
  // 测试密钥销毁
  KeyManager::destroy_key(key_manager, dek2)
  assert_false(KeyManager::exists(key_manager, KeyManager::key_id(dek2)))
}

// 测试8: 安全威胁检测和防护
test "安全威胁检测和防护测试" {
  let threat_detector = ThreatDetector::new()
  let security_monitor = SecurityMonitor::new()
  
  // 配置威胁检测规则
  ThreatDetector::add_rule(threat_detector, ThreatRule::abnormal_access_pattern())
  ThreatDetector::add_rule(threat_detector, ThreatRule::data_exfiltration())
  ThreatDetector::add_rule(threat_detector, ThreatRule::privilege_escalation())
  ThreatDetector::add_rule(threat_detector, ThreatRule::unusual_encryption_usage())
  
  // 配置安全监控
  SecurityMonitor::enable_real_time_detection(security_monitor, true)
  SecurityMonitor::set_alert_threshold(security_monitor, 3) // 3个事件触发警报
  
  // 模拟正常访问模式
  let normal_user = User::new("normal-user")
  for i in 1..=5 {
    let access_event = AccessEvent::new(normal_user, "telemetry.data." + i.to_string(), "read")
    SecurityMonitor::process_access_event(security_monitor, access_event)
  }
  
  // 验证正常访问不触发威胁
  let normal_threats = ThreatDetector::detect_threats(threat_detector, SecurityMonitor::get_events(security_monitor))
  assert_eq(normal_threats.length(), 0)
  
  // 模拟异常访问模式
  let suspicious_user = User::new("suspicious-user")
  for i in 1..=100 {
    let access_event = AccessEvent::new(suspicious_user, "sensitive.data." + i.to_string(), "read")
    SecurityMonitor::process_access_event(security_monitor, access_event)
  }
  
  // 检测异常访问模式
  let suspicious_events = SecurityMonitor::get_events_by_user(security_monitor, suspicious_user)
  let detected_threats = ThreatDetector::detect_threats(threat_detector, suspicious_events)
  
  // 验证威胁检测
  assert_true(detected_threats.length() > 0)
  
  let abnormal_access_threat = detected_threats.find(fn(threat) {
    Threat::type(threat) == "abnormal_access_pattern"
  })
  assert_true(abnormal_access_threat.is_some())
  
  // 模拟数据渗漏
  let exfiltration_user = User::new("exfiltration-user")
  let large_data_events = []
  for i in 1..=10 {
    let large_data = TelemetryData::new()
    TelemetryData::add_large_binary_data(large_data, "x".repeat(1000000)) // 1MB数据
    
    let export_event = ExportEvent::new(exfiltration_user, large_data, "external.system")
    large_data_events = large_data_events.push(export_event)
    SecurityMonitor::process_export_event(security_monitor, export_event)
  }
  
  // 检测数据渗漏
  let exfiltration_threats = ThreatDetector::detect_threats(threat_detector, SecurityMonitor::get_export_events(security_monitor))
  assert_true(exfiltration_threats.length() > 0)
  
  let data_exfiltration_threat = exfiltration_threats.find(fn(threat) {
    Threat::type(threat) == "data_exfiltration"
  })
  assert_true(data_exfiltration_threat.is_some())
  
  // 测试自动防护响应
  let auto_response = AutoResponse::new()
  AutoResponse::add_action(auto_response, "abnormal_access_pattern", "block_user")
  AutoResponse::add_action(auto_response, "data_exfiltration", "quarantine_data")
  
  // 应用自动响应
  for threat in detected_threats.concat(exfiltration_threats) {
    AutoResponse::execute(auto_response, threat)
  }
  
  // 验证防护措施
  assert_true(User::is_blocked(suspicious_user))
  assert_true(Data::is_quarantined("large_binary_data"))
  
  // 测试安全警报生成
  let security_alerts = SecurityMonitor::generate_alerts(security_monitor)
  assert_true(security_alerts.length() > 0)
  
  let high_severity_alert = security_alerts.find(fn(alert) {
    SecurityAlert::severity(alert) == "high"
  })
  assert_true(high_severity_alert.is_some())
}
// 遥测系统安全性和隐私保护测试
// 测试数据加密、访问控制、隐私保护和安全合规

pub test "遥测数据加密测试" {
  // 创建安全管理器
  let security_manager = azimuth::SecurityManager::new()
  
  // 创建加密管理器
  let encryption_manager = azimuth::SecurityManager::create_encryption_manager(security_manager)
  
  // 生成加密密钥
  let encryption_key = azimuth::EncryptionManager::generate_key(encryption_manager, "AES-256-GCM")
  assert_true(encryption_key.length() > 0)
  
  // 测试基础加密功能
  let sensitive_data = "user_id=12345&email=user@example.com&phone=+1234567890"
  let trace_data = azimuth::TraceData::new("secure-trace", "secure-operation")
  let trace_attributes = azimuth::Attributes::new()
  azimuth::Attributes::set(trace_attributes, "user.id", azimuth::StringValue("12345"))
  azimuth::Attributes::set(trace_attributes, "user.email", azimuth::StringValue("user@example.com"))
  azimuth::Attributes::set(trace_attributes, "user.phone", azimuth::StringValue("+1234567890"))
  
  // 加密敏感属性
  let encrypted_attributes = azimuth::EncryptionManager::encrypt_attributes(
    encryption_manager, 
    trace_attributes, 
    encryption_key
  )
  
  // 验证加密后的数据不可读
  let encrypted_user_id = azimuth::Attributes::get(encrypted_attributes, "user.id")
  match encrypted_user_id {
    Some(azimuth::StringValue(encrypted_value)) => {
      assert_true(encrypted_value != "12345")
      assert_true(!encrypted_value.contains("12345"))
    }
    _ => assert_false(true)  # 应该是加密的字符串值
  }
  
  // 解密属性
  let decrypted_attributes = azimuth::EncryptionManager::decrypt_attributes(
    encryption_manager, 
    encrypted_attributes, 
    encryption_key
  )
  
  // 验证解密后数据完整性
  let decrypted_user_id = azimuth::Attributes::get(decrypted_attributes, "user.id")
  let decrypted_email = azimuth::Attributes::get(decrypted_attributes, "user.email")
  let decrypted_phone = azimuth::Attributes::get(decrypted_attributes, "user.phone")
  
  assert_eq(decrypted_user_id, Some(azimuth::StringValue("12345")))
  assert_eq(decrypted_email, Some(azimuth::StringValue("user@example.com")))
  assert_eq(decrypted_phone, Some(azimuth::StringValue("+1234567890")))
  
  // 测试不同加密算法
  let encryption_algorithms = ["AES-256-GCM", "AES-128-CBC", "ChaCha20-Poly1305"]
  let encryption_results = []
  
  for algorithm in encryption_algorithms {
    let key = azimuth::EncryptionManager::generate_key(encryption_manager, algorithm)
    let encrypted = azimuth::EncryptionManager::encrypt_attributes(encryption_manager, trace_attributes, key)
    let decrypted = azimuth::EncryptionManager::decrypt_attributes(encryption_manager, encrypted, key)
    
    let original_user_id = azimuth::Attributes::get(trace_attributes, "user.id")
    let restored_user_id = azimuth::Attributes::get(decrypted, "user.id")
    
    assert_eq(original_user_id, restored_user_id)
    
    encryption_results.push({
      algorithm,
      key_size: key.length(),
      encryption_time: azimuth::EncryptionManager::get_last_encryption_time(encryption_manager),
      decryption_time: azimuth::EncryptionManager::get_last_decryption_time(encryption_manager)
    })
  }
  
  // 验证不同算法的性能差异
  for i in 1..encryption_results.length() {
    let prev_result = encryption_results[i-1]
    let curr_result = encryption_results[i]
    
    # AES-256应该有更长的密钥
    if prev_result.algorithm == "AES-256-GCM" {
      assert_true(prev_result.key_size >= curr_result.key_size)
    }
  }
  
  // 测试密钥轮换
  let old_key = encryption_key
  let new_key = azimuth::EncryptionManager::generate_key(encryption_manager, "AES-256-GCM")
  
  // 用新密钥重新加密数据
  let reencrypted_attributes = azimuth::EncryptionManager::reencrypt_attributes(
    encryption_manager, 
    encrypted_attributes, 
    old_key, 
    new_key
  )
  
  // 验证密钥轮换后数据仍可解密
  let rdecrypted_attributes = azimuth::EncryptionManager::decrypt_attributes(
    encryption_manager, 
    reencrypted_attributes, 
    new_key
  )
  
  let rdecrypted_user_id = azimuth::Attributes::get(rdecrypted_attributes, "user.id")
  assert_eq(rdecrypted_user_id, Some(azimuth::StringValue("12345")))
  
  // 测试加密数据完整性验证
  let tampered_data = azimuth::EncryptionManager::tamper_data(encryption_manager, encrypted_attributes)
  let integrity_result = azimuth::EncryptionManager::verify_integrity(encryption_manager, tampered_data, encryption_key)
  assert_false(integrity_result)  # 被篡改的数据应该验证失败
}

pub test "访问控制和权限管理测试" {
  let security_manager = azimuth::SecurityManager::new()
  
  // 创建访问控制管理器
  let access_control = azimuth::SecurityManager::create_access_control_manager(security_manager)
  
  // 定义角色和权限
  let roles = [
    {
      name: "admin",
      permissions: ["read:all", "write:all", "delete:all", "manage:users"],
      priority: 100
    },
    {
      name: "operator", 
      permissions: ["read:telemetry", "write:telemetry", "read:metrics"],
      priority: 50
    },
    {
      name: "viewer",
      permissions: ["read:telemetry", "read:metrics"],
      priority: 10
    },
    {
      name: "anonymous",
      permissions: ["read:public"],
      priority: 1
    }
  ]
  
  // 注册角色
  for role in roles {
    azimuth::AccessControlManager::register_role(access_control, role)
  }
  
  // 创建用户
  let users = [
    { id: "user-001", username: "admin", roles: ["admin"] },
    { id: "user-002", username: "operator1", roles: ["operator"] },
    { id: "user-003", username: "viewer1", roles: ["viewer"] },
    { id: "user-004", username: "guest", roles: ["anonymous"] }
  ]
  
  for user in users {
    azimuth::AccessControlManager::register_user(access_control, user)
  }
  
  // 测试权限验证
  let admin_user = azimuth::AccessControlManager::get_user(access_control, "user-001")
  let operator_user = azimuth::AccessControlManager::get_user(access_control, "user-002")
  let viewer_user = azimuth::AccessControlManager::get_user(access_control, "user-003")
  let guest_user = azimuth::AccessControlManager::get_user(access_control, "user-004")
  
  // 管理员应该有所有权限
  assert_true(azimuth::AccessControlManager::has_permission(access_control, admin_user, "read:all"))
  assert_true(azimuth::AccessControlManager::has_permission(access_control, admin_user, "write:all"))
  assert_true(azimuth::AccessControlManager::has_permission(access_control, admin_user, "delete:all"))
  assert_true(azimuth::AccessControlManager::has_permission(access_control, admin_user, "manage:users"))
  
  // 操作员应该有有限的读写权限
  assert_true(azimuth::AccessControlManager::has_permission(access_control, operator_user, "read:telemetry"))
  assert_true(azimuth::AccessControlManager::has_permission(access_control, operator_user, "write:telemetry"))
  assert_true(azimuth::AccessControlManager::has_permission(access_control, operator_user, "read:metrics"))
  assert_false(azimuth::AccessControlManager::has_permission(access_control, operator_user, "delete:all"))
  assert_false(azimuth::AccessControlManager::has_permission(access_control, operator_user, "manage:users"))
  
  // 查看者只有读权限
  assert_true(azimuth::AccessControlManager::has_permission(access_control, viewer_user, "read:telemetry"))
  assert_true(azimuth::AccessControlManager::has_permission(access_control, viewer_user, "read:metrics"))
  assert_false(azimuth::AccessControlManager::has_permission(access_control, viewer_user, "write:telemetry"))
  assert_false(azimuth::AccessControlManager::has_permission(access_control, viewer_user, "delete:all"))
  
  // 访客只有最小权限
  assert_true(azimuth::AccessControlManager::has_permission(access_control, guest_user, "read:public"))
  assert_false(azimuth::AccessControlManager::has_permission(access_control, guest_user, "read:telemetry"))
  assert_false(azimuth::AccessControlManager::has_permission(access_control, guest_user, "write:telemetry"))
  
  // 测试资源级别的访问控制
  let telemetry_resources = [
    { id: "trace-001", type: "trace", sensitivity: "public", owner: "service-a" },
    { id: "trace-002", type: "trace", sensitivity: "internal", owner: "service-b" },
    { id: "metrics-001", type: "metrics", sensitivity: "confidential", owner: "service-c" },
    { id: "logs-001", type: "logs", sensitivity: "secret", owner: "service-d" }
  ]
  
  for resource in telemetry_resources {
    azimuth::AccessControlManager::register_resource(access_control, resource)
  }
  
  // 测试资源访问权限
  let public_resource = azimuth::AccessControlManager::get_resource(access_control, "trace-001")
  let confidential_resource = azimuth::AccessControlManager::get_resource(access_control, "metrics-001")
  
  # 所有用户都应该能访问公共资源
  assert_true(azimuth::AccessControlManager::can_access_resource(access_control, admin_user, public_resource))
  assert_true(azimuth::AccessControlManager::can_access_resource(access_control, operator_user, public_resource))
  assert_true(azimuth::AccessControlManager::can_access_resource(access_control, viewer_user, public_resource))
  assert_true(azimuth::AccessControlManager::can_access_resource(access_control, guest_user, public_resource))
  
  # 只有管理员和操作员能访问机密资源
  assert_true(azimuth::AccessControlManager::can_access_resource(access_control, admin_user, confidential_resource))
  assert_true(azimuth::AccessControlManager::can_access_resource(access_control, operator_user, confidential_resource))
  assert_false(azimuth::AccessControlManager::can_access_resource(access_control, viewer_user, confidential_resource))
  assert_false(azimuth::AccessControlManager::can_access_resource(access_control, guest_user, confidential_resource))
  
  // 测试动态权限分配
  azimuth::AccessControlManager::grant_permission(access_control, viewer_user, "write:telemetry")
  assert_true(azimuth::AccessControlManager::has_permission(access_control, viewer_user, "write:telemetry"))
  
  azimuth::AccessControlManager::revoke_permission(access_control, viewer_user, "write:telemetry")
  assert_false(azimuth::AccessControlManager::has_permission(access_control, viewer_user, "write:telemetry"))
  
  // 测试临时权限
  let temp_permission_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let temp_permission_end = temp_permission_start + 3600000000000L  # 1小时后
  
  azimuth::AccessControlManager::grant_temporary_permission(
    access_control, 
    guest_user, 
    "read:telemetry", 
    temp_permission_start, 
    temp_permission_end
  )
  
  # 临时权限在有效期内应该可用
  assert_true(azimuth::AccessControlManager::has_permission(access_control, guest_user, "read:telemetry"))
  
  # 模拟时间过期
  azimuth::AccessControlManager::set_current_time(access_control, temp_permission_end + 1000000000L)
  assert_false(azimuth::AccessControlManager::has_permission(access_control, guest_user, "read:telemetry"))
}

pub test "隐私保护和数据脱敏测试" {
  let security_manager = azimuth::SecurityManager::new()
  
  // 创建隐私保护管理器
  let privacy_manager = azimuth::SecurityManager::create_privacy_manager(security_manager)
  
  // 定义敏感数据类型和脱敏规则
  let sensitive_data_types = [
    {
      name: "email",
      pattern: "\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b",
      masking_rule: "email_mask"
    },
    {
      name: "phone",
      pattern: "\+?[1-9]\d{1,14}",
      masking_rule: "phone_mask"
    },
    {
      name: "credit_card",
      pattern: "\b\d{4}[ -]?\d{4}[ -]?\d{4}[ -]?\d{4}\b",
      masking_rule: "credit_card_mask"
    },
    {
      name: "ssn",
      pattern: "\b\d{3}-\d{2}-\d{4}\b",
      masking_rule: "ssn_mask"
    },
    {
      name: "ip_address",
      pattern: "\b(?:[0-9]{1,3}\.){3}[0-9]{1,3}\b",
      masking_rule: "ip_mask"
    }
  ]
  
  // 注册敏感数据类型
  for data_type in sensitive_data_types {
    azimuth::PrivacyManager::register_sensitive_data_type(privacy_manager, data_type)
  }
  
  // 定义脱敏规则
  azimuth::PrivacyManager::register_masking_rule(privacy_manager, "email_mask", {
    method: "partial",
    visible_chars: 2,
    mask_char: "*",
    preserve_domain: true
  })
  
  azimuth::PrivacyManager::register_masking_rule(privacy_manager, "phone_mask", {
    method: "partial",
    visible_chars: 3,
    mask_char: "*",
    preserve_length: true
  })
  
  azimuth::PrivacyManager::register_masking_rule(privacy_manager, "credit_card_mask", {
    method: "partial",
    visible_chars: 4,
    mask_char: "*",
    show_last: true
  })
  
  azimuth::PrivacyManager::register_masking_rule(privacy_manager, "ssn_mask", {
    method: "complete",
    mask_char: "X"
  })
  
  azimuth::PrivacyManager::register_masking_rule(privacy_manager, "ip_mask", {
    method: "hash",
    algorithm: "SHA-256",
    salt: "telemetry_salt"
  })
  
  // 创建包含敏感信息的遥测数据
  let sensitive_trace_data = azimuth::TraceData::new("privacy-test", "privacy-operation")
  let sensitive_attributes = azimuth::Attributes::new()
  azimuth::Attributes::set(sensitive_attributes, "user.email", azimuth::StringValue("john.doe@example.com"))
  azimuth::Attributes::set(sensitive_attributes, "user.phone", azimuth::StringValue("+1234567890"))
  azimuth::Attributes::set(sensitive_attributes, "payment.card", azimuth::StringValue("4532 1234 5678 9012"))
  azimuth::Attributes::set(sensitive_attributes, "user.ssn", azimuth::StringValue("123-45-6789"))
  azimuth::Attributes::set(sensitive_attributes, "client.ip", azimuth::StringValue("192.168.1.100"))
  azimuth::Attributes::set(sensitive_attributes, "user.name", azimuth::StringValue("John Doe"))
  
  // 应用隐私保护
  let masked_attributes = azimuth::PrivacyManager::apply_privacy_protection(privacy_manager, sensitive_attributes)
  
  // 验证脱敏结果
  let masked_email = azimuth::Attributes::get(masked_attributes, "user.email")
  match masked_email {
    Some(azimuth::StringValue(email)) => {
      assert_true(email.contains("jo"))
      assert_true(email.contains("@example.com"))
      assert_true(email.contains("*"))
      assert_false(email.contains("john.doe"))
    }
    _ => assert_false(true)
  }
  
  let masked_phone = azimuth::Attributes::get(masked_attributes, "user.phone")
  match masked_phone {
    Some(azimuth::StringValue(phone)) => {
      assert_true(phone.contains("+12"))
      assert_true(phone.contains("*"))
      assert_false(phone.contains("1234567890"))
    }
    _ => assert_false(true)
  }
  
  let masked_card = azimuth::Attributes::get(masked_attributes, "payment.card")
  match masked_card {
    Some(azimuth::StringValue(card)) => {
      assert_true(card.contains("9012"))
      assert_true(card.contains("*"))
      assert_false(card.contains("4532 1234 5678"))
    }
    _ => assert_false(true)
  }
  
  let masked_ssn = azimuth::Attributes::get(masked_attributes, "user.ssn")
  match masked_ssn {
    Some(azimuth::StringValue(ssn)) => {
      assert_eq(ssn, "XXX-XX-XXXX")
      assert_false(ssn.contains("123-45-6789"))
    }
    _ => assert_false(true)
  }
  
  let masked_ip = azimuth::Attributes::get(masked_attributes, "client.ip")
  match masked_ip {
    Some(azimuth::StringValue(ip)) => {
      assert_false(ip.contains("192.168.1.100"))
      assert_true(ip.length() == 64)  # SHA-256 hash length
    }
    _ => assert_false(true)
  }
  
  // 非敏感数据应该保持不变
  let non_sensitive_name = azimuth::Attributes::get(masked_attributes, "user.name")
  assert_eq(non_sensitive_name, Some(azimuth::StringValue("John Doe")))
  
  // 测试数据匿名化
  let anonymized_attributes = azimuth::PrivacyManager::anonymize_data(privacy_manager, sensitive_attributes)
  let anonymized_name = azimuth::Attributes::get(anonymized_attributes, "user.name")
  
  match anonymized_name {
    Some(azimuth::StringValue(name)) => {
      assert_false(name.contains("John Doe"))
      assert_true(name.contains("user_"))
    }
    _ => assert_false(true)
  }
  
  // 测试差分隐私
  let differential_privacy_manager = azimuth::PrivacyManager::create_differential_privacy_manager(privacy_manager, 1.0)  # epsilon=1.0
  
  let sensitive_metrics = [25.3, 30.7, 28.9, 32.1, 29.5, 31.2, 27.8, 30.4]
  let noisy_metrics = azimuth::DifferentialPrivacyManager::add_noise(differential_privacy_manager, sensitive_metrics)
  
  # 验证噪声添加
  assert_true(noisy_metrics.length() == sensitive_metrics.length())
  let noise_added = false
  
  for i in 0..sensitive_metrics.length() {
    if (noisy_metrics[i] - sensitive_metrics[i]).abs() > 0.01 {
      noise_added = true
      break
    }
  }
  
  assert_true(noise_added)
  
  # 验证统计特性保持
  let original_mean = sensitive_metrics.reduce(0.0, fn(acc, val) { acc + val }) / sensitive_metrics.length().to_double()
  let noisy_mean = noisy_metrics.reduce(0.0, fn(acc, val) { acc + val }) / noisy_metrics.length().to_double()
  
  # 均值应该在合理范围内
  assert_true((noisy_mean - original_mean).abs() < 5.0)
  
  // 测试隐私预算管理
  let privacy_budget = azimuth::DifferentialPrivacyManager::get_privacy_budget(differential_privacy_manager)
  assert_eq(privacy_budget.remaining, 1.0)
  
  # 消耗隐私预算
  azimuth::DifferentialPrivacyManager::consume_budget(differential_privacy_manager, 0.3)
  let remaining_budget = azimuth::DifferentialPrivacyManager::get_privacy_budget(differential_privacy_manager).remaining
  assert_eq(remaining_budget, 0.7)
  
  # 预算不足时应该拒绝查询
  let query_result = azimuth::DifferentialPrivacyManager::query_with_privacy(differential_privacy_manager, sensitive_metrics, 0.8)
  assert_false(query_result.allowed)  # 预算不足
  
  # 预算充足时应该允许查询
  let allowed_query = azimuth::DifferentialPrivacyManager::query_with_privacy(differential_privacy_manager, sensitive_metrics, 0.5)
  assert_true(allowed_query.allowed)
}

pub test "审计日志和合规性测试" {
  let security_manager = azimuth::SecurityManager::new()
  
  // 创建审计管理器
  let audit_manager = azimuth::SecurityManager::create_audit_manager(security_manager)
  
  // 配置审计策略
  let audit_policy = {
    log_data_access: true,
    log_data_modification: true,
    log_permission_changes: true,
    log_authentication: true,
    log_encryption_operations: true,
    retention_days: 365,
    require_signature: true
  }
  
  azimuth::AuditManager::set_policy(audit_manager, audit_policy)
  
  // 模拟审计事件
  let audit_events = [
    {
      event_type: "data_access",
      user_id: "user-002",
      resource_id: "trace-001",
      action: "read",
      timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
      result: "success",
      details: "Accessed telemetry trace data"
    },
    {
      event_type: "data_modification",
      user_id: "user-001",
      resource_id: "metrics-001",
      action: "update",
      timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
      result: "success",
      details: "Updated metrics configuration"
    },
    {
      event_type: "permission_change",
      user_id: "user-001",
      resource_id: "user-003",
      action: "grant_permission",
      timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
      result: "success",
      details: "Granted write:telemetry permission"
    },
    {
      event_type: "authentication",
      user_id: "user-002",
      resource_id: "system",
      action: "login",
      timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
      result: "success",
      details: "User login successful"
    },
    {
      event_type: "encryption_operation",
      user_id: "system",
      resource_id: "trace-002",
      action: "encrypt",
      timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
      result: "success",
      details: "Encrypted sensitive trace data"
    }
  ]
  
  // 记录审计事件
  for event in audit_events {
    azimuth::AuditManager::log_event(audit_manager, event)
  }
  
  // 验证审计日志记录
  let all_events = azimuth::AuditManager::get_all_events(audit_manager)
  assert_eq(all_events.length(), 5)
  
  // 测试审计日志查询
  let data_access_events = azimuth::AuditManager::query_events(audit_manager, {
    event_type: "data_access",
    start_time: 0L,
    end_time: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 1000000000L
  })
  
  assert_eq(data_access_events.length(), 1)
  assert_eq(data_access_events[0].user_id, "user-002")
  assert_eq(data_access_events[0].action, "read")
  
  let user_events = azimuth::AuditManager::query_events_by_user(audit_manager, "user-001")
  assert_eq(user_events.length(), 2)  # data_modification 和 permission_change
  
  // 测试审计日志完整性验证
  let integrity_result = azimuth::AuditManager::verify_integrity(audit_manager)
  assert_true(integrity_result.valid)
  
  // 模拟日志篡改
  azimuth::AuditManager::tamper_with_log(audit_manager, 1)  # 篡改第二个事件
  
  let tampered_integrity = azimuth::AuditManager::verify_integrity(audit_manager)
  assert_false(tampered_integrity.valid)
  assert_true(tampered_integrity.tampered_indices.contains(1))
  
  // 恢复日志完整性
  azimuth::AuditManager::restore_integrity(audit_manager)
  let restored_integrity = azimuth::AuditManager::verify_integrity(audit_manager)
  assert_true(restored_integrity.valid)
  
  // 测试合规性报告生成
  let compliance_report = azimuth::AuditManager::generate_compliance_report(audit_manager, {
    start_date: "2025-01-01",
    end_date: "2025-12-31",
    regulations: ["GDPR", "CCPA", "HIPAA"],
    include_details: true
  })
  
  assert_true(compliance_report.generated_at > 0L)
  assert_true(compliance_report.total_events >= 5)
  assert_true(compliance_report.compliance_score >= 0.0 && compliance_report.compliance_score <= 1.0)
  
  // 验证GDPR合规性
  let gdpr_compliance = compliance_report.regulation_compliance["GDPR"]
  assert_true(gdpr_compliance.data_access_logged)
  assert_true(gdpr_compliance.data_processing_recorded)
  assert_true(gdpr_compliance.consent_tracking_available)
  
  // 验证CCPA合规性
  let ccpa_compliance = compliance_report.regulation_compliance["CCPA"]
  assert_true(ccpa_compliance.data_access_logged)
  assert_true(ccpa_compliance.data_deletion_available)
  assert_true(ccpa_compliance.opt_out_respected)
  
  // 测试审计日志归档
  let archive_result = azimuth::AuditManager::archive_logs(audit_manager, {
    older_than_days: 30,
    compression: true,
    encryption: true
  })
  
  assert_true(archive_result.success)
  assert_true(archive_result.archived_count >= 0)
  assert_true(archive_result.archive_size > 0)
  
  // 测试审计日志检索
  let archived_events = azimuth::AuditManager::search_archived_logs(audit_manager, {
    query: "user_id:user-002",
    date_range: { start: "2025-01-01", end: "2025-12-31" }
  })
  
  assert_true(archived_events.length() >= 1)
  
  // 测试实时审计监控
  let alert_manager = azimuth::AuditManager::create_alert_manager(audit_manager)
  
  // 设置安全警报规则
  azimuth::AlertManager::add_rule(alert_manager, {
    name: "multiple_failed_logins",
    condition: "event_type:authentication AND result:failure",
    threshold: 3,
    time_window: 300,  # 5分钟
    severity: "high"
  })
  
  azimuth::AlertManager::add_rule(alert_manager, {
    name: "privilege_escalation",
    condition: "event_type:permission_change AND action:grant_permission",
    threshold: 1,
    time_window: 60,   # 1分钟
    severity: "critical"
  })
  
  // 模拟触发警报的事件
  let failed_login_events = [
    { event_type: "authentication", user_id: "user-999", action: "login", result: "failure" },
    { event_type: "authentication", user_id: "user-999", action: "login", result: "failure" },
    { event_type: "authentication", user_id: "user-999", action: "login", result: "failure" }
  ]
  
  for event in failed_login_events {
    let full_event = {
      ..event,
      resource_id: "system",
      timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
      details: "Failed login attempt"
    }
    azimuth::AuditManager::log_event(audit_manager, full_event)
  }
  
  // 检查是否触发警报
  let active_alerts = azimuth::AlertManager::get_active_alerts(alert_manager)
  let multiple_failed_logins_alert = active_alerts.find(fn(alert) { alert.rule_name == "multiple_failed_logins" })
  
  assert_true(multiple_failed_logins_alert != null)
  assert_eq(multiple_failed_logins_alert.severity, "high")
  
  // 测试警报响应
  azimuth::AlertManager::acknowledge_alert(alert_manager, multiple_failed_logins_alert.id, "Security team notified")
  let acknowledged_alert = azimuth::AlertManager::get_alert(alert_manager, multiple_failed_logins_alert.id)
  
  assert_true(acknowledged_alert.acknowledged)
  assert_true(acknowledged_alert.acknowledged_by.contains("security_system"))
}
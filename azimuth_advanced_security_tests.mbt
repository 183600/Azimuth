// Azimuth Advanced Security and Privacy Test Suite
// 高级安全和隐私保护测试套件

// Test 1: 零信任架构和身份验证
test "zero trust architecture and authentication" {
  let zero_trust_manager = @azimuth.ZeroTrustManager::new()
  
  // 配置零信任策略
  let trust_policy = @azimuth.TrustPolicy::new()
  @azimuth.TrustPolicy::require_mutual_authentication(trust_policy, true)
  @azimuth.TrustPolicy::require_device_compliance(trust_policy, true)
  @azimuth.TrustPolicy::require_location_validation(trust_policy, true)
  @azimuth.TrustPolicy::set_session_timeout(trust_policy, 300000) // 5分钟
  @azimuth.TrustPolicy::enable_continuous_validation(trust_policy, true)
  
  @azimuth.ZeroTrustManager::set_policy(zero_trust_manager, trust_policy)
  
  // 创建身份提供者
  let identity_provider = @azimuth.IdentityProvider::new()
  @azimuth.IdentityProvider::configure_mfa(identity_provider, true) // 多因素认证
  @azimuth.IdentityProvider::configure_password_policy(identity_provider, @azimuth.PasswordPolicy::new(12, true, true, true))
  @azimuth.IdentityProvider::configure_biometric(identity_provider, true) // 生物识别
  @azimuth.IdentityProvider::configure_hardware_token(identity_provider, true) // 硬件令牌
  
  // 创建用户身份
  let user_identity = @azimuth.UserIdentity::new("user-001", "security-admin@company.com")
  @azimuth.UserIdentity::set_roles(user_identity, ["admin", "security-auditor"])
  @azimuth.UserIdentity::set_permissions(user_identity, [
    "telemetry.read", "telemetry.write", "security.audit", "policy.manage"
  ])
  @azimuth.UserIdentity::set_security_context(user_identity, @azimuth.SecurityContext::new(
    "192.168.1.100",
    "device-001",
    "secure-terminal",
    @azimuth.Time::now_unix_nanos()
  ))
  
  // 创建设备身份
  let device_identity = @azimuth.DeviceIdentity::new("edge-node-001", "IoT-Sensor-Array")
  @azimuth.DeviceIdentity::set_certificate(device_identity, "device-cert-001")
  @azimuth.DeviceIdentity::set_trust_level(device_identity, @azimuth.TrustLevel::High)
  @azimuth.DeviceIdentity::set_compliance_status(device_identity, @azimuth.ComplianceStatus::Compliant)
  
  // 测试身份验证流程
  let auth_request = @azimuth.AuthenticationRequest::new(user_identity, device_identity)
  auth_request.mfa_token = "123456"
  auth_request.biometric_signature = "biometric-hash-001"
  auth_request.hardware_token_challenge = "challenge-response-001"
  
  let auth_result = @azimuth.ZeroTrustManager::authenticate(zero_trust_manager, auth_request)
  
  // 验证认证结果
  assert_true(auth_result.success)
  assert_true(auth_result.mfa_verified)
  assert_true(auth_result.biometric_verified)
  assert_true(auth_result.hardware_verified)
  assert_true(auth_result.device_compliant)
  assert_true(auth_result.location_valid)
  assert_true(auth_result.session_token.length() > 0)
  
  // 测试持续验证
  let session_token = auth_result.session_token
  let validation_result = @azimuth.ZeroTrustManager::validate_session(zero_trust_manager, session_token)
  
  assert_true(validation_result.valid)
  assert_true(validation_result.trust_score > 0.8)
  
  // 模拟安全事件（异常位置）
  let suspicious_context = @azimuth.SecurityContext::new(
    "10.0.0.1", // 异常IP
    "device-001",
    "secure-terminal",
    @azimuth.Time::now_unix_nanos()
  )
  
  @azimuth.UserIdentity::update_security_context(user_identity, suspicious_context)
  
  let revalidation_result = @azimuth.ZeroTrustManager::validate_session(zero_trust_manager, session_token)
  
  // 验证异常检测
  assert_false(revalidation_result.valid)
  assert_true(revalidation_result.trust_score < 0.5)
  assert_true(revalidation_result.risk_factors.length() > 0)
  assert_true(revalidation_result.risk_factors.some(fn(factor) { factor == @azimuth.RiskFactor::AnomalousLocation }))
  
  // 测试自适应认证
  let adaptive_auth_result = @azimuth.ZeroTrustManager::adaptive_authenticate(zero_trust_manager, auth_request)
  
  // 验证自适应认证要求额外验证
  assert_true(adaptive_auth_result.requires_additional_verification)
  assert_true(adaptive_auth_result.required_factors.length() > 0)
}

// Test 2: 端到端加密和密钥管理
test "end-to-end encryption and key management" {
  let key_manager = @azimuth.KeyManager::new()
  
  // 配置密钥管理策略
  let key_policy = @azimuth.KeyPolicy::new()
  @azimuth.KeyPolicy::set_algorithm(key_policy, @azimuth.EncryptionAlgorithm::AES256_GCM)
  @azimuth.KeyPolicy::set_key_size(key_policy, 256)
  @azimuth.KeyPolicy::set_rotation_interval(key_policy, 86400000) // 24小时
  @azimuth.KeyPolicy::set_retention_period(key_policy, 7776000000) // 90天
  @azimuth.KeyPolicy::enable_hsm(key_policy, true) // 硬件安全模块
  
  @azimuth.KeyManager::set_policy(key_manager, key_policy)
  
  // 创建主密钥
  let master_key = @azimuth.KeyManager::create_master_key(key_manager, "telemetry-master-key")
  
  // 创建数据加密密钥
  let data_encryption_key = @azimuth.KeyManager::create_data_key(key_manager, master_key, "telemetry-data-key")
  
  // 创建敏感遥测数据
  let sensitive_telemetry = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("user.email", @azimuth.StringValue("john.doe@company.com")),
      ("user.ssn", @azimuth.StringValue("123-45-6789")),
      ("credit.card.number", @azimuth.StringValue("4111-1111-1111-1111")),
      ("api.key", @azimuth.StringValue("sk-1234567890abcdef")),
      ("medical.record.id", @azimuth.StringValue("MR-001234567")),
      ("transaction.amount", @azimuth.FloatValue(1500.0)),
      ("account.balance", @azimuth.FloatValue(25000.0)),
      ("location.coordinates", @azimuth.StringValue("37.7749,-122.4194"))
    ]
  )
  
  // 测试端到端加密
  let encryption_context = @azimuth.EncryptionContext::new()
  @azimuth.EncryptionContext::add_key(encryption_context, "data-encryption", data_encryption_key)
  @azimuth.EncryptionContext::set_purpose(encryption_context, @azimuth.EncryptionPurpose::DataProtection)
  @azimuth.EncryptionContext::set_access_level(encryption_context, @azimuth.AccessLevel::Restricted)
  
  let encrypted_data = @azimuth.KeyManager::encrypt_data(key_manager, sensitive_telemetry, encryption_context)
  
  // 验证加密效果
  assert_true(encrypted_data.ciphertext.length() > 0)
  assert_true(encrypted_data.iv.length() > 0)
  assert_true(encrypted_data.tag.length() > 0)
  assert_true(encrypted_data.key_id == data_encryption_key.key_id)
  assert_true(encrypted_data.encryption_timestamp > 0)
  
  // 验证数据不可读性
  let encrypted_string = @azimuth.String::from_bytes(encrypted_data.ciphertext)
  assert_false(encrypted_string.contains("john.doe@company.com"))
  assert_false(encrypted_string.contains("123-45-6789"))
  assert_false(encrypted_string.contains("4111-1111-1111-1111"))
  
  // 测试解密
  let decryption_context = @azimuth.DecryptionContext::new()
  @azimuth.DecryptionContext::add_key(decryption_context, "data-encryption", data_encryption_key)
  @azimuth.DecryptionContext::set_purpose(decryption_context, @azimuth.EncryptionPurpose::DataProtection)
  
  let decrypted_data = @azimuth.KeyManager::decrypt_data(key_manager, encrypted_data, decryption_context)
  
  // 验证解密正确性
  let decrypted_attrs = @azimuth.TelemetryData::attributes(decrypted_data)
  let original_attrs = @azimuth.TelemetryData::attributes(sensitive_telemetry)
  assert_eq(decrypted_attrs.length(), original_attrs.length())
  
  // 验证敏感数据完整性
  let decrypted_email = @azimuth.TelemetryData::get_attribute(decrypted_data, "user.email")
  match decrypted_email {
    Some(@azimuth.StringValue(email)) => assert_eq(email, "john.doe@company.com")
    _ => assert_true(false)
  }
  
  // 测试密钥轮换
  let key_rotation_result = @azimuth.KeyManager::rotate_key(key_manager, data_encryption_key.key_id)
  
  assert_true(key_rotation_result.success)
  assert_true(key_rotation_result.new_key_id != data_encryption_key.key_id)
  assert_true(key_rotation_result.rotation_timestamp > 0)
  
  // 验证旧密钥仍然可以解密旧数据
  let old_key_decryption = @azimuth.KeyManager::decrypt_data_with_key(
    key_manager, 
    encrypted_data, 
    data_encryption_key
  )
  assert_eq(@azimuth.TelemetryData::attributes(old_key_decryption).length(), original_attrs.length())
  
  // 测试密钥撤销
  let revocation_result = @azimuth.KeyManager::revoke_key(key_manager, data_encryption_key.key_id)
  assert_true(revocation_result.success)
  
  // 验证撤销后无法使用密钥
  let revoked_encryption_attempt = @azimuth.KeyManager::try_encrypt_data(
    key_manager, 
    sensitive_telemetry, 
    encryption_context
  )
  match revoked_encryption_attempt {
    @azimuth.Result::Err(msg) => assert_true(msg.contains("revoked") || msg.contains("inactive"))
    _ => assert_true(false)
  }
}

// Test 3: 差分隐私和数据匿名化
test "differential privacy and data anonymization" {
  let privacy_manager = @azimuth.PrivacyManager::new()
  
  // 配置差分隐私参数
  let dp_config = @azimuth.DifferentialPrivacyConfig::new()
  @azimuth.DifferentialPrivacyConfig::set_epsilon(dp_config, 1.0) // 隐私预算
  @azimuth.DifferentialPrivacyConfig::set_delta(dp_config, 0.00001) // 失败概率
  @azimuth.DifferentialPrivacyConfig::set_sensitivity(dp_config, 1.0) // 敏感度
  
  @azimuth.PrivacyManager::configure_differential_privacy(privacy_manager, dp_config)
  
  // 创建包含敏感信息的用户数据集
  let user_dataset = []
  for i in 0..=1000 {
    let user_record = @azimuth.UserRecord::new(
      "user-#{i}",
      "user#{i}@example.com",
      @azimuth.IntValue(25 + (i % 50)), // 年龄
      @azimuth.FloatValue(50000.0 + (i % 100) * 1000.0), // 收入
      @azimuth.StringValue(["New York", "Los Angeles", "Chicago", "Houston", "Phoenix"][i % 5]), // 城市
      @azimuth.StringValue(["Engineer", "Manager", "Analyst", "Developer", "Designer"][i % 5]) // 职业
    )
    user_dataset = user_dataset.push(user_record)
  }
  
  // 测试差分隐私查询
  let age_query = @azimuth.DPQuery::new(@azimuth.QueryType::Average, "age")
  let dp_age_result = @azimuth.PrivacyManager::execute_dp_query(privacy_manager, user_dataset, age_query)
  
  // 验证差分隐私结果
  assert_true(dp_age_result.value > 0.0)
  assert_true(dp_age_result.noise_added > 0.0)
  assert_true(dp_age_result.epsilon_used <= dp_config.epsilon)
  assert_true(dp_age_result.confidence_interval.lower < dp_age_result.value)
  assert_true(dp_age_result.confidence_interval.upper > dp_age_result.value)
  
  // 计算真实平均值用于比较
  let real_average_age = @azimuth.PrivacyManager::calculate_real_average(privacy_manager, user_dataset, "age")
  
  // 验证差分隐私结果的准确性
  let error_margin = @azimuth.Math::abs(dp_age_result.value - real_average_age) / real_average_age
  assert_true(error_margin < 0.1) // 误差应在10%以内
  
  // 测试数据匿名化
  let anonymization_config = @azimuth.AnonymizationConfig::new()
  @azimuth.AnonymizationConfig::set_method(anonymization_config, @azimuth.AnonymizationMethod::KAnonymity)
  @azimuth.AnonymizationConfig::set_k_value(anonymization_config, 5) // k=5匿名性
  @azimuth.AnonymizationConfig::set_quasi_identifiers(anonymization_config, ["age", "city", "occupation"])
  
  let anonymized_dataset = @azimuth.PrivacyManager::anonymize_dataset(privacy_manager, user_dataset, anonymization_config)
  
  // 验证匿名化效果
  assert_eq(anonymized_dataset.length(), user_dataset.length())
  
  // 检查每个等价类的大小
  let equivalence_classes = @azimuth.PrivacyManager::find_equivalence_classes(privacy_manager, anonymized_dataset, ["age", "city", "occupation"])
  
  for class in equivalence_classes {
    assert_true(class.size >= 5) // 每个等价类至少有5个记录
  }
  
  // 验证匿名化后无法识别个人
  let anonymized_record = anonymized_dataset[0]
  assert_false(@azimuth.UserRecord::has_original_email(anonymized_record))
  assert_false(@azimuth.UserRecord::has_original_id(anonymized_record))
  
  // 测试局部差分隐私
  let ldp_config = @azimuth.LocalDifferentialPrivacyConfig::new()
  @azimuth.LocalDifferentialPrivacyConfig::set_epsilon(ldp_config, 2.0) // 局部隐私预算通常更高
  
  let local_private_data = @azimuth.PrivacyManager::apply_local_dp(privacy_manager, user_dataset, ldp_config)
  
  // 验证局部差分隐私
  assert_eq(local_private_data.length(), user_dataset.length())
  
  // 测试隐私预算管理
  let budget_manager = @azimuth.PrivacyBudgetManager::new()
  @azimuth.PrivacyBudgetManager::set_total_budget(budget_manager, 10.0) // 总隐私预算
  @azimuth.PrivacyBudgetManager::set_reset_period(budget_manager, 86400000) // 24小时重置
  
  // 执行多个查询消耗隐私预算
  let query1_budget = @azimuth.PrivacyBudgetManager::consume_budget(budget_manager, 1.0)
  let query2_budget = @azimuth.PrivacyBudgetManager::consume_budget(budget_manager, 2.0)
  let query3_budget = @azimuth.PrivacyBudgetManager::consume_budget(budget_manager, 3.0)
  
  assert_true(query1_budget)
  assert_true(query2_budget)
  assert_true(query3_budget)
  
  // 超出预算的查询应该被拒绝
  let over_budget_query = @azimuth.PrivacyBudgetManager::consume_budget(budget_manager, 5.0)
  assert_false(over_budget_query)
  
  // 验证剩余预算
  let remaining_budget = @azimuth.PrivacyBudgetManager::get_remaining_budget(budget_manager)
  assert_eq(remaining_budget, 4.0) // 10.0 - 1.0 - 2.0 - 3.0 = 4.0
}

// Test 4: 审计日志和合规性监控
test "audit logging and compliance monitoring" {
  let audit_manager = @azimuth.AuditManager::new()
  
  // 配置审计策略
  let audit_policy = @azimuth.AuditPolicy::new()
  @azimuth.AuditPolicy::log_all_data_access(audit_policy, true)
  @azimuth.AuditPolicy::log_all_config_changes(audit_policy, true)
  @azimuth.AuditPolicy::log_all_auth_events(audit_policy, true)
  @azimuth.AuditPolicy::log_data_modifications(audit_policy, true)
  @azimuth.AuditPolicy::set_retention_days(audit_policy, 2555) // 7年保留期
  @azimuth.AuditPolicy::enable_tamper_protection(audit_policy, true)
  
  @azimuth.AuditManager::set_policy(audit_manager, audit_policy)
  
  // 创建审计事件
  let data_access_event = @azimuth.AuditEvent::new(
    @azimuth.EventType::DataAccess,
    "user-001",
    "telemetry-data-001",
    "READ",
    @azimuth.Time::now_unix_nanos()
  )
  @azimuth.AuditEvent::add_detail(data_access_event, "data.type", "user-metrics")
  @azimuth.AuditEvent::add_detail(data_access_event, "access.reason", "business-analysis")
  @azimuth.AuditEvent::add_detail(data_access_event, "ip.address", "192.168.1.100")
  
  let config_change_event = @azimuth.AuditEvent::new(
    @azimuth.EventType::ConfigurationChange,
    "admin-001",
    "sampling-config",
    "UPDATE",
    @azimuth.Time::now_unix_nanos()
  )
  @azimuth.AuditEvent::add_detail(config_change_event, "previous.value", "0.1")
  @azimuth.AuditEvent::add_detail(config_change_event, "new.value", "0.05")
  @azimuth.AuditEvent::add_detail(config_change_event, "change.reason", "performance-optimization")
  
  let auth_event = @azimuth.AuditEvent::new(
    @azimuth.EventType::Authentication,
    "user-002",
    "auth-system",
    "LOGIN",
    @azimuth.Time::now_unix_nanos()
  )
  @azimuth.AuditEvent::add_detail(auth_event, "auth.method", "mfa")
  @azimuth.AuditEvent::add_detail(auth_event, "login.status", "success")
  @azimuth.AuditEvent::add_detail(auth_event, "device.id", "device-001")
  
  // 记录审计事件
  @azimuth.AuditManager::log_event(audit_manager, data_access_event)
  @azimuth.AuditManager::log_event(audit_manager, config_change_event)
  @azimuth.AuditManager::log_event(audit_manager, auth_event)
  
  // 测试审计日志查询
  let access_logs = @azimuth.AuditManager::query_events(audit_manager, @azimuth.EventQuery::new()
    .with_event_type(@azimuth.EventType::DataAccess)
    .with_time_range(@azimuth.Time::now_unix_nanos() - 1000000, @azimuth.Time::now_unix_nanos())
  )
  
  assert_eq(access_logs.length(), 1)
  assert_eq(@azimuth.AuditEvent::event_type(access_logs[0]), @azimuth.EventType::DataAccess)
  
  // 测试合规性监控
  let compliance_monitor = @azimuth.ComplianceMonitor::new()
  
  // 配置合规性规则
  let gdpr_rule = @azimuth.ComplianceRule::new("GDPR-DataAccess", @azimuth.ComplianceFramework::GDPR)
  @azimuth.ComplianceRule::add_requirement(gdpr_rule, @azimuth.Requirement::LawfulBasisForProcessing)
  @azimuth.ComplianceRule::add_requirement(gdpr_rule, @azimuth.Requirement::DataMinimization)
  @azimuth.ComplianceRule::add_requirement(gdpr_rule, @azimuth.Requirement::AuditTrail)
  
  let hipaa_rule = @azimuth.ComplianceRule::new("HIPAA-DataProtection", @azimuth.ComplianceFramework::HIPAA)
  @azimuth.ComplianceRule::add_requirement(hipaa_rule, @azimuth.Requirement::AccessControls)
  @azimuth.ComplianceRule::add_requirement(hipaa_rule, @azimuth.Requirement::AuditControls)
  @azimuth.ComplianceRule::add_requirement(hipaa_rule, @azimuth.Requirement::IntegrityControls)
  
  @azimuth.ComplianceMonitor::add_rule(compliance_monitor, gdpr_rule)
  @azimuth.ComplianceMonitor::add_rule(compliance_monitor, hipaa_rule)
  
  // 执行合规性检查
  let compliance_report = @azimuth.ComplianceMonitor::evaluate_compliance(compliance_monitor, audit_manager)
  
  // 验证合规性报告
  assert_eq(compliance_report.frameworks.length(), 2)
  assert_true(compliance_report.frameworks.some(fn(f) { f.framework == @azimuth.ComplianceFramework::GDPR }))
  assert_true(compliance_report.frameworks.some(fn(f) { f.framework == @azimuth.ComplianceFramework::HIPAA }))
  
  // 检查GDPR合规性
  let gdpr_framework = compliance_report.frameworks.filter(fn(f) { f.framework == @azimuth.ComplianceFramework::GDPR })[0]
  assert_true(gdpr_framework.overall_compliant)
  
  // 检查HIPAA合规性
  let hipaa_framework = compliance_report.frameworks.filter(fn(f) { f.framework == @azimuth.ComplianceFramework::HIPAA })[0]
  assert_true(hipaa_framework.overall_compliant)
  
  // 测试审计日志完整性
  let integrity_check = @azimuth.AuditManager::verify_log_integrity(audit_manager)
  assert_true(integrity_check.valid)
  assert_eq(integrity_check.total_events, 3)
  assert_eq(integrity_check.tampered_events, 0)
  
  // 模拟日志篡改
  @azimuth.AuditManager::simulate_tampering(audit_manager, 1) // 篡改第一个事件
  
  let tampered_integrity_check = @azimuth.AuditManager::verify_log_integrity(audit_manager)
  assert_false(tampered_integrity_check.valid)
  assert_eq(tampered_integrity_check.tampered_events, 1)
  
  // 测试审计日志备份和恢复
  let backup_result = @azimuth.AuditManager::backup_logs(audit_manager, "/backup/audit-logs")
  assert_true(backup_result.success)
  assert_true(backup_result.backed_up_events > 0)
  
  // 清除当前日志
  @azimuth.AuditManager::clear_logs(audit_manager)
  
  // 恢复日志
  let restore_result = @azimuth.AuditManager::restore_logs(audit_manager, "/backup/audit-logs")
  assert_true(restore_result.success)
  assert_eq(restore_result.restored_events, backup_result.backed_up_events)
}

// Test 5: 威胁检测和异常行为分析
test "threat detection and anomaly behavior analysis" {
  let threat_detector = @azimuth.ThreatDetector::new()
  
  // 配置威胁检测规则
  let detection_rules = [
    @azimuth.ThreatRule::new("BruteForceAttack", @azimuth.ThreatType::AuthenticationAbuse)
      .with_condition("failed.login.attempts", ">", 5)
      .with_time_window(300000) // 5分钟内
      .with_severity(@azimuth.ThreatSeverity::High),
    
    @azimuth.ThreatRule::new("DataExfiltration", @azimuth.ThreatType::DataTheft)
      .with_condition("data.export.volume", ">", "1GB")
      .with_time_window(3600000) // 1小时内
      .with_severity(@azimuth.ThreatSeverity::Critical),
    
    @azimuth.ThreatRule::new("UnauthorizedAccess", @azimuth.ThreatType::UnauthorizedAccess)
      .with_condition("access.denied.count", ">", 10)
      .with_time_window(600000) // 10分钟内
      .with_severity(@azimuth.ThreatSeverity::Medium),
    
    @azimuth.ThreatRule::new("AnomalousDataAccess", @azimuth.ThreatType::AnomalousActivity)
      .with_condition("access.pattern.deviation", ">", 0.8)
      .with_time_window(86400000) // 24小时内
      .with_severity(@azimuth.ThreatSeverity::Medium)
  ]
  
  for rule in detection_rules {
    @azimuth.ThreatDetector::add_rule(threat_detector, rule)
  }
  
  // 模拟正常访问模式
  let normal_behavior = @azimuth.BehaviorProfile::new("user-001")
  for day in 0..=30 {
    @azimuth.BehaviorProfile::add_daily_activity(normal_behavior, @azimuth.DailyActivity::new(
      @azimuth.Time::now_unix_nanos() - (day * 86400000),
      50, // 平均访问次数
      100, // 平均数据量(MB)
      ["09:00", "10:30", "14:00", "16:30"], // 访问时间
      ["192.168.1.100", "192.168.1.101"] // 常用IP
    ))
  }
  
  @azimuth.ThreatDetector::add_behavior_profile(threat_detector, normal_behavior)
  
  // 模拟暴力破解攻击
  let brute_force_events = []
  for i in 0..=8 {
    let auth_event = @azimuth.SecurityEvent::new(
      @azimuth.EventType::Authentication,
      "attacker-external",
      @azimuth.Time::now_unix_nanos() - (8 - i) * 30000 // 30秒间隔
    )
    @azimuth.SecurityEvent::add_detail(auth_event, "login.status", "failed")
    @azimuth.SecurityEvent::add_detail(auth_event, "source.ip", "10.0.0.50")
    @azimuth.SecurityEvent::add_detail(auth_event, "target.user", "admin-user")
    
    brute_force_events = brute_force_events.push(auth_event)
  }
  
  // 模拟数据渗漏
  let exfiltration_events = []
  for i in 0..=5 {
    let data_access_event = @azimuth.SecurityEvent::new(
      @azimuth.EventType::DataAccess,
      "insider-threat",
      @azimuth.Time::now_unix_nanos() - (5 - i) * 600000 // 10分钟间隔
    )
    @azimuth.SecurityEvent::add_detail(data_access_event, "access.type", "export")
    @azimuth.SecurityEvent::add_detail(data_access_event, "data.volume", @azimuth.StringValue("#{200 + i * 100}MB"))
    @azimuth.SecurityEvent::add_detail(data_access_event, "data.sensitivity", "high")
    
    exfiltration_events = exfiltration_events.push(data_access_event)
  }
  
  // 模拟异常访问模式
  let anomalous_access_event = @azimuth.SecurityEvent::new(
    @azimuth.EventType::DataAccess,
    "user-001",
    @azimuth.Time::now_unix_nanos()
  )
  @azimuth.SecurityEvent::add_detail(anomalous_access_event, "access.time", "03:00") // 异常时间
  @azimuth.SecurityEvent::add_detail(anomalous_access_event, "source.ip", "10.0.0.100") // 异常IP
  @azimuth.SecurityEvent::add_detail(anomalous_access_event, "data.volume", @azimuth.StringValue("500MB")) // 异常数据量
  @azimuth.SecurityEvent::add_detail(anomalous_access_event, "access.pattern.deviation", @azimuth.StringValue("0.9"))
  
  // 处理安全事件
  for event in brute_force_events {
    @azimuth.ThreatDetector::process_event(threat_detector, event)
  }
  
  for event in exfiltration_events {
    @azimuth.ThreatDetector::process_event(threat_detector, event)
  }
  
  @azimuth.ThreatDetector::process_event(threat_detector, anomalous_access_event)
  
  // 获取威胁检测结果
  let threat_report = @azimuth.ThreatDetector::generate_threat_report(threat_detector)
  
  // 验证威胁检测
  assert_true(threat_report.detected_threats.length() >= 3)
  
  // 验证暴力破解攻击检测
  let brute_force_threat = threat_report.detected_threats.filter(fn(t) { 
    @azimuth.Threat::threat_type(t) == @azimuth.ThreatType::AuthenticationAbuse 
  })
  assert_true(brute_force_threat.length() > 0)
  
  let bf_threat = brute_force_threat[0]
  assert_eq(@azimuth.Threat::rule_name(bf_threat), "BruteForceAttack")
  assert_eq(@azimuth.Threat::severity(bf_threat), @azimuth.ThreatSeverity::High)
  assert_true(@azimuth.Threat::confidence_score(bf_threat) > 0.8)
  
  // 验证数据渗漏检测
  let exfiltration_threat = threat_report.detected_threats.filter(fn(t) { 
    @azimuth.Threat::threat_type(t) == @azimuth.ThreatType::DataTheft 
  })
  assert_true(exfiltration_threat.length() > 0)
  
  let ex_threat = exfiltration_threat[0]
  assert_eq(@azimuth.Threat::rule_name(ex_threat), "DataExfiltration")
  assert_eq(@azimuth.Threat::severity(ex_threat), @azimuth.ThreatSeverity::Critical)
  
  // 验证异常行为检测
  let anomaly_threat = threat_report.detected_threats.filter(fn(t) { 
    @azimuth.Threat::threat_type(t) == @azimuth.ThreatType::AnomalousActivity 
  })
  assert_true(anomaly_threat.length() > 0)
  
  // 测试自动响应
  let auto_response = @azimuth.AutoResponse::new()
  @azimuth.AutoResponse::configure_action(auto_response, "BruteForceAttack", @azimuth.ResponseAction::BlockIP)
  @azimuth.AutoResponse::configure_action(auto_response, "DataExfiltration", @azimuth.ResponseAction::SuspendAccount)
  @azimuth.AutoResponse::configure_action(auto_response, "AnomalousDataAccess", @azimuth.ResponseAction::RequireReauth)
  
  for threat in threat_report.detected_threats {
    let response_result = @azimuth.AutoResponse::execute_response(auto_response, threat)
    assert_true(response_result.success)
  }
  
  // 验证响应执行
  let response_report = @azimuth.AutoResponse::get_response_report(auto_response)
  assert_true(response_report.blocked_ips.length() > 0)
  assert_true(response_report.suspended_accounts.length() > 0)
  assert_true(response_report.reauth_required.length() > 0)
}

// Test 6: 安全数据传输和通道保护
test "secure data transmission and channel protection" {
  let secure_channel = @azimuth.SecureChannel::new()
  
  // 配置安全通道
  let channel_config = @azimuth.ChannelConfig::new()
  @azimuth.ChannelConfig::set_protocol(channel_config, @azimuth.SecurityProtocol::TLS1_3)
  @azimuth.ChannelConfig::set_cipher_suite(channel_config, "TLS_AES_256_GCM_SHA384")
  @azimuth.ChannelConfig::enable_certificate_pinning(channel_config, true)
  @azimuth.ChannelConfig::enable_perfect_forward_secrecy(channel_config, true)
  @azimuth.ChannelConfig::set_certificate_validation(channel_config, @azimuth.ValidationMode::Strict)
  
  @azimuth.SecureChannel::configure(secure_channel, channel_config)
  
  // 创建客户端和服务端证书
  let client_cert = @azimuth.Certificate::new(
    "client.telemetry.example.com",
    @azimuth.CertificateType::Client,
    "2024-01-01",
    "2025-01-01",
    ["client-auth"]
  )
  
  let server_cert = @azimuth.Certificate::new(
    "server.telemetry.example.com",
    @azimuth.CertificateType::Server,
    "2024-01-01",
    "2025-01-01",
    ["server-auth"]
  )
  
  // 配置证书固定
  @azimuth.SecureChannel::pin_certificate(secure_channel, server_cert)
  
  // 建立安全连接
  let connection_result = @azimuth.SecureChannel::establish_connection(
    secure_channel,
    "client.telemetry.example.com",
    "server.telemetry.example.com:4317",
    client_cert
  )
  
  // 验证连接建立
  assert_true(connection_result.success)
  assert_true(connection_result.session_id.length() > 0)
  assert_true(connection_result.cipher_suite == "TLS_AES_256_GCM_SHA384")
  assert_true(connection_result.protocol_version == "TLSv1.3")
  
  // 创建敏感遥测数据
  let sensitive_payload = @azimuth.TelemetryPayload::new()
  for i in 0..=50 {
    let data_point = @azimuth.TelemetryData::new(
      @azimuth.TraceId::generate(),
      @azimuth.SpanId::generate(),
      [
        ("user.id", @azimuth.StringValue("user-#{i % 10}")),
        ("transaction.id", @azimuth.StringValue("txn-#{i}")),
        ("amount", @azimuth.FloatValue(100.0 + i * 10.0)),
        ("currency", @azimuth.StringValue("USD")),
        ("payment.method", @azimuth.StringValue("credit-card")),
        ("card.type", @azimuth.StringValue("visa")),
        ("merchant.id", @azimuth.StringValue("merchant-#{i % 5}")),
        ("location", @azimuth.StringValue("US-#{i % 50}"))
      ]
    )
    @azimuth.TelemetryPayload::add_data(sensitive_payload, data_point)
  }
  
  // 测试安全传输
  let transmission_start = @azimuth.Time::now_unix_nanos()
  let transmission_result = @azimuth.SecureChannel::transmit_data(secure_channel, sensitive_payload)
  let transmission_end = @azimuth.Time::now_unix_nanos()
  
  // 验证传输结果
  assert_true(transmission_result.success)
  assert_true(transmission_result.bytes_transferred > 0)
  assert_true(transmission_result.encrypted)
  assert_true(transmission_result.compressed)
  assert_true(transmission_result.transmission_time < 5000000) // 5秒内完成
  
  // 验证传输过程中的数据保护
  assert_true(transmission_result.encryption_algorithm.length() > 0)
  assert_true(transmission_result.key_exchange_method.length() > 0)
  assert_true(transmission_result.signature_algorithm.length() > 0)
  
  // 测试连接重用和会话恢复
  let session_resume_result = @azimuth.SecureChannel::resume_session(secure_channel, connection_result.session_id)
  
  assert_true(session_resume_result.success)
  assert_true(session_resume_result.session_resumed)
  assert_true(session_resume_result.handshake_time < connection_result.handshake_time) // 会话恢复应该更快
  
  // 测试中间人攻击防护
  let mitm_simulator = @azimuth.MITMSimulator::new()
  let mitm_result = @azimuth.MITMSimulator::attempt_interception(mitm_simulator, secure_channel)
  
  // 验证中间人攻击防护
  assert_false(mitm_result.success)
  assert_true(mitm_result.blocked_by_certificate_pinning || mitm_result.blocked_by_validation)
  
  // 测试重放攻击防护
  let replay_simulator = @azimuth.ReplaySimulator::new()
  let captured_packet = @azimuth.SecureChannel::capture_packet(secure_channel, 1)
  let replay_result = @azimuth.ReplaySimulator::attempt_replay(replay_simulator, secure_channel, captured_packet)
  
  // 验证重放攻击防护
  assert_false(replay_result.success)
  assert_true(replay_result.replay_detected)
  
  // 测试连接断开和清理
  let disconnect_result = @azimuth.SecureChannel::disconnect(secure_channel)
  
  assert_true(disconnect_result.success)
  assert_true(disconnect_result.session_terminated)
  assert_true(disconnect_result.keys_destroyed)
  
  // 验证连接状态
  let connection_status = @azimuth.SecureChannel::get_connection_status(secure_channel)
  assert_eq(connection_status.state, @azimuth.ConnectionState::Disconnected)
  assert_true(connection_status.session_id == "")
}
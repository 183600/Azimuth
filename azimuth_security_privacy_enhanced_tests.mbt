// Azimuth 安全隐私增强测试
// 测试敏感数据处理和隐私保护功能

test "个人身份信息(PII)检测和脱敏" {
  // 测试个人身份信息的检测和脱敏
  
  // 创建PII检测器
  let pii_detector = PIIDetector::new()
  
  // 测试邮箱地址检测和脱敏
  let email = "user@example.com"
  let is_email_pii = PIIDetector::is_email(pii_detector, email)
  assert_true(is_email_pii)
  
  let masked_email = PIIDetector::mask_email(pii_detector, email)
  assert_eq(masked_email, "u***@example.com")
  
  // 测试电话号码检测和脱敏
  let phone = "+1-555-123-4567"
  let is_phone_pii = PIIDetector::is_phone(pii_detector, phone)
  assert_true(is_phone_pii)
  
  let masked_phone = PIIDetector::mask_phone(pii_detector, phone)
  assert_eq(masked_phone, "+1-***-***-4567")
  
  // 测试信用卡号检测和脱敏
  let credit_card = "4111-1111-1111-1111"
  let is_credit_card_pii = PIIDetector::is_credit_card(pii_detector, credit_card)
  assert_true(is_credit_card_pii)
  
  let masked_credit_card = PIIDetector::mask_credit_card(pii_detector, credit_card)
  assert_eq(masked_credit_card, "****-****-****-1111")
  
  // 测试社会安全号码检测和脱敏
  let ssn = "123-45-6789"
  let is_ssn_pii = PIIDetector::is_ssn(pii_detector, ssn)
  assert_true(is_ssn_pii)
  
  let masked_ssn = PIIDetector::mask_ssn(pii_detector, ssn)
  assert_eq(masked_ssn, "***-**-6789")
  
  // 测试IP地址检测和脱敏
  let ip_address = "192.168.1.100"
  let is_ip_pii = PIIDetector::is_ip_address(pii_detector, ip_address)
  assert_true(is_ip_pii)
  
  let masked_ip = PIIDetector::mask_ip(pii_detector, ip_address)
  assert_eq(masked_ip, "192.168.1.***")
}

test "敏感数据过滤和清理" {
  // 测试敏感数据的过滤和清理
  
  // 创建数据过滤器
  let data_filter = DataFilter::new()
  
  // 配置过滤规则
  DataFilter::add_pattern(data_filter, "password", "password")
  DataFilter::add_pattern(data_filter, "api_key", "api[_-]?key")
  DataFilter::add_pattern(data_filter, "token", "token")
  DataFilter::add_pattern(data_filter, "secret", "secret")
  
  // 测试数据过滤
  let sensitive_data = [
    ("username", "john.doe"),
    ("password", "supersecret123"),
    ("email", "john@example.com"),
    ("api_key", "sk-1234567890abcdef"),
    ("session_id", "sess_abc123"),
    ("credit_card", "4111111111111111"),
    ("ssn", "123-45-6789")
  ]
  
  let filtered_data = DataFilter::filter(data_filter, sensitive_data)
  
  // 验证过滤结果
  for (key, value) in filtered_data {
    match key {
      "password" => assert_eq(value, "***")
      "api_key" => assert_eq(value, "***")
      "credit_card" => assert_eq(value, "************1111")
      "ssn" => assert_eq(value, "***-**-6789")
      _ => ()  // 非敏感数据保持原样
    }
  }
  
  // 测试日志消息过滤
  let log_message = "User john.doe logged in with password supersecret123 from IP 192.168.1.100"
  let filtered_log = DataFilter::filter_message(data_filter, log_message)
  
  assert_true(filtered_log.contains("john.doe"))
  assert_false(filtered_log.contains("supersecret123"))
  assert_true(filtered_log.contains("***"))
  assert_false(filtered_log.contains("192.168.1.100"))  // IP也应该被过滤
}

test "数据加密和解密" {
  // 测试敏感数据的加密和解密
  
  // 创建加密器
  let encryptor = Encryptor::new("encryption_key_12345")
  
  // 测试字符串加密
  let plaintext = "sensitive_user_data_12345"
  let encrypted = Encryptor::encrypt(encryptor, plaintext)
  
  assert_true(encrypted != plaintext)
  assert_true(encrypted.length() > 0)
  
  // 测试字符串解密
  let decrypted = Encryptor::decrypt(encryptor, encrypted)
  assert_eq(decrypted, plaintext)
  
  // 测试数值加密
  let number_value = 123456789
  let encrypted_number = Encryptor::encrypt_number(encryptor, number_value)
  let decrypted_number = Encryptor::decrypt_number(encryptor, encrypted_number)
  assert_eq(decrypted_number, number_value)
  
  // 测试批量加密
  let sensitive_fields = [
    ("user_id", "user123"),
    ("email", "user@example.com"),
    ("phone", "+1-555-123-4567"),
    ("ssn", "123-45-6789")
  ]
  
  let encrypted_fields = Encryptor::encrypt_batch(encryptor, sensitive_fields)
  let decrypted_fields = Encryptor::decrypt_batch(encryptor, encrypted_fields)
  
  // 验证批量加密和解密
  for i in 0..=sensitive_fields.length() - 1 {
    let original_key = sensitive_fields[i].0
    let original_value = sensitive_fields[i].1
    let decrypted_key = decrypted_fields[i].0
    let decrypted_value = decrypted_fields[i].1
    
    assert_eq(original_key, decrypted_key)
    assert_eq(original_value, decrypted_value)
  }
  
  // 测试加密密钥轮换
  let new_encryptor = Encryptor::new("new_encryption_key_67890")
  let re_encrypted = Encryptor::re_encrypt(encryptor, new_encryptor, encrypted)
  let re_decrypted = Encryptor::decrypt(new_encryptor, re_encrypted)
  assert_eq(re_decrypted, plaintext)
}

test "访问控制和权限管理" {
  // 测试访问控制和权限管理
  
  // 创建访问控制管理器
  let access_manager = AccessManager::new()
  
  // 定义角色和权限
  AccessManager::define_role(access_manager, "admin", ["read", "write", "delete", "export"])
  AccessManager::define_role(access_manager, "analyst", ["read", "export"])
  AccessManager::define_role(access_manager, "viewer", ["read"])
  
  // 创建用户并分配角色
  AccessManager::create_user(access_manager, "admin_user", "admin")
  AccessManager::create_user(access_manager, "analyst_user", "analyst")
  AccessManager::create_user(access_manager, "viewer_user", "viewer")
  
  // 测试权限检查
  let admin_can_read = AccessManager::check_permission(access_manager, "admin_user", "read")
  let admin_can_delete = AccessManager::check_permission(access_manager, "admin_user", "delete")
  let analyst_can_write = AccessManager::check_permission(access_manager, "analyst_user", "write")
  let analyst_can_export = AccessManager::check_permission(access_manager, "analyst_user", "export")
  let viewer_can_read = AccessManager::check_permission(access_manager, "viewer_user", "read")
  let viewer_can_delete = AccessManager::check_permission(access_manager, "viewer_user", "delete")
  
  assert_true(admin_can_read)
  assert_true(admin_can_delete)
  assert_false(analyst_can_write)
  assert_true(analyst_can_export)
  assert_true(viewer_can_read)
  assert_false(viewer_can_delete)
  
  // 测试资源级访问控制
  let telemetry_data = TelemetryData::new("sensitive_operation")
  TelemetryData::set_access_level(telemetry_data, "confidential")
  
  let admin_access = AccessManager::can_access_resource(access_manager, "admin_user", telemetry_data)
  let viewer_access = AccessManager::can_access_resource(access_manager, "viewer_user", telemetry_data)
  
  assert_true(admin_access)
  assert_false(viewer_access)
  
  // 测试数据脱敏策略
  let data_masking_policy = DataMaskingPolicy::new()
  DataMaskingPolicy::add_rule(data_masking_policy, "viewer", ["email", "phone", "ssn"])
  DataMaskingPolicy::add_rule(data_masking_policy, "analyst", ["ssn"])
  
  let user_data = [
    ("name", "John Doe"),
    ("email", "john@example.com"),
    ("phone", "+1-555-123-4567"),
    ("ssn", "123-45-6789"),
    ("account_id", "acc123456")
  ]
  
  let viewer_masked_data = DataMaskingPolicy::apply(data_masking_policy, "viewer", user_data)
  let analyst_masked_data = DataMaskingPolicy::apply(data_masking_policy, "analyst", user_data)
  
  // 验证viewer只能看到非敏感信息
  assert_true(viewer_masked_data.contains(("name", "John Doe")))
  assert_false(viewer_masked_data.contains(("email", "john@example.com")))
  assert_false(viewer_masked_data.contains(("phone", "+1-555-123-4567")))
  assert_false(viewer_masked_data.contains(("ssn", "123-45-6789")))
  
  // 验证analyst可以看到更多但不是全部
  assert_true(analyst_masked_data.contains(("email", "john@example.com")))
  assert_true(analyst_masked_data.contains(("phone", "+1-555-123-4567")))
  assert_false(analyst_masked_data.contains(("ssn", "123-45-6789")))
}

test "审计日志和合规性" {
  // 测试审计日志和合规性功能
  
  // 创建审计日志记录器
  let audit_logger = AuditLogger::new()
  
  // 记录用户访问事件
  let access_event = AuditEvent::new("user_access", "admin_user", "2025-01-02T10:00:00Z")
  AuditEvent::add_detail(access_event, "resource", "telemetry_dashboard")
  AuditEvent::add_detail(access_event, "action", "view")
  AuditEvent::add_detail(access_event, "ip_address", "192.168.1.100")
  AuditEvent::add_detail(access_event, "user_agent", "Mozilla/5.0")
  
  AuditLogger::log_event(audit_logger, access_event)
  
  // 记录数据导出事件
  let export_event = AuditEvent::new("data_export", "analyst_user", "2025-01-02T10:05:00Z")
  AuditEvent::add_detail(export_event, "dataset", "user_analytics")
  AuditEvent::add_detail(export_event, "format", "csv")
  AuditEvent::add_detail(export_event, "record_count", "1000")
  AuditEvent::add_detail(export_event, "filter", "date_range:2025-01-01 to 2025-01-02")
  
  AuditLogger::log_event(audit_logger, export_event)
  
  // 记录权限变更事件
  let permission_event = AuditEvent::new("permission_change", "admin_user", "2025-01-02T10:10:00Z")
  AuditEvent::add_detail(permission_event, "target_user", "viewer_user")
  AuditEvent::add_detail(permission_event, "old_role", "viewer")
  AuditEvent::add_detail(permission_event, "new_role", "analyst")
  AuditEvent::add_detail(permission_event, "reason", "project requirement")
  
  AuditLogger::log_event(audit_logger, permission_event)
  
  // 测试审计日志查询
  let access_logs = AuditLogger::query_events(audit_logger, "user_access", "2025-01-02T00:00:00Z", "2025-01-02T23:59:59Z")
  assert_eq(access_logs.length(), 1)
  
  let all_logs = AuditLogger::query_events(audit_logger, "", "2025-01-02T00:00:00Z", "2025-01-02T23:59:59Z")
  assert_eq(all_logs.length(), 3)
  
  // 测试合规性检查
  let compliance_checker = ComplianceChecker::new()
  
  // 配置GDPR合规规则
  ComplianceChecker::add_gdpr_rule(compliance_checker, "data_retention", 365)  // 365天
  ComplianceChecker::add_gdpr_rule(compliance_checker, "right_to_be_forgotten", true)
  ComplianceChecker::add_gdpr_rule(compliance_checker, "data_portability", true)
  
  // 配置HIPAA合规规则
  ComplianceChecker::add_hipaa_rule(compliance_checker, "phi_protection", true)
  ComplianceChecker::add_hipaa_rule(compliance_checker, "audit_trail", true)
  ComplianceChecker::add_hipaa_rule(compliance_checker, "access_control", true)
  
  // 检查遥测数据的合规性
  let telemetry_data = TelemetryData::new("user_health_metrics")
  TelemetryData::add_tag(telemetry_data, "contains_phi", "true")
  TelemetryData::set_retention_days(telemetry_data, 400)
  
  let gdpr_compliance = ComplianceChecker::check_gdpr(compliance_checker, telemetry_data)
  let hipaa_compliance = ComplianceChecker::check_hipaa(compliance_checker, telemetry_data)
  
  assert_false(gdpr_compliance.data_retention)  // 超过365天
  assert_true(hipaa_compliance.phi_protection)  # PHI保护启用
  assert_true(hipaa_compliance.audit_trail)     # 审计跟踪启用
}

test "数据最小化和匿名化" {
  // 测试数据最小化和匿名化功能
  
  // 创建数据最小化器
  let minimizer = DataMinimizer::new()
  
  // 配置最小化规则
  Minimizer::add_rule(minimizer, "user_analytics", [
    ("name", "remove"),
    ("email", "hash"),
    ("phone", "remove"),
    ("age", "bucketize", [0, 18, 25, 35, 45, 55, 65, 100]),
    ("location", "generalize", "city"),
    ("user_id", "hash")
  ])
  
  // 测试数据最小化
  let raw_data = [
    ("name", "John Doe"),
    ("email", "john@example.com"),
    ("phone", "+1-555-123-4567"),
    ("age", "32"),
    ("location", "123 Main St, New York, NY 10001"),
    ("user_id", "user123456"),
    ("last_login", "2025-01-02T10:00:00Z"),
    ("session_count", "25")
  ]
  
  let minimized_data = Minimizer::apply(minimizer, "user_analytics", raw_data)
  
  // 验证最小化结果
  assert_false(minimized_data.contains_key("name"))  # 应该被移除
  assert_false(minimized_data.contains_key("phone"))  # 应该被移除
  
  // 验证哈希处理
  let hashed_email = minimized_data.get("email")
  match hashed_email {
    Some(value) => {
      assert_false(value == "john@example.com")  # 应该被哈希
      assert_true(value.length() == 64)          # SHA256哈希长度
    }
    None => assert_true(false)
  }
  
  // 验证年龄分桶
  let bucketed_age = minimized_data.get("age")
  match bucketed_age {
    Some(value) => assert_eq(value, "25-35")
    None => assert_true(false)
  }
  
  // 验证位置泛化
  let generalized_location = minimized_data.get("location")
  match generalized_location {
    Some(value) => assert_eq(value, "New York")
    None => assert_true(false)
  }
  
  // 测试完全匿名化
  let anonymizer = DataAnonymizer::new()
  
  let anonymized_data = Anonymizer::anonymize(anonymizer, raw_data)
  
  // 验证匿名化结果
  assert_false(anonymized_data.contains_key("name"))
  assert_false(anonymized_data.contains_key("email"))
  assert_false(anonymized_data.contains_key("phone"))
  assert_false(anonymized_data.contains_key("location"))
  assert_false(anonymized_data.contains_key("user_id"))
  
  // 验证保留非敏感字段
  assert_true(anonymized_data.contains_key("age"))
  assert_true(anonymized_data.contains_key("last_login"))
  assert_true(anonymized_data.contains_key("session_count"))
  
  // 测试差分隐私
  let dp_analyzer = DifferentialPrivacy::new(epsilon = 1.0)
  
  let sensitive_values = [10, 15, 20, 25, 30, 35, 40, 45, 50]
  let dp_result = DifferentialPrivacy::add_noise(dp_analyzer, sensitive_values)
  
  // 验证差分隐私结果
  assert_eq(dp_result.length(), sensitive_values.length())
  
  // 验证噪声添加（值应该不同）
  let mut all_same = true
  for i in 0..=sensitive_values.length() - 1 {
    if sensitive_values[i] != dp_result[i] {
      all_same = false
      break
    }
  }
  assert_false(all_same)  # 至少有一些值应该因噪声而改变
}

test "安全遥测数据传输" {
  // 测试安全遥测数据传输
  
  // 创建安全传输管理器
  let secure_transport = SecureTransport::new()
  
  // 配置TLS证书
  SecureTransport::set_certificate(secure_transport, "server.crt")
  SecureTransport::set_private_key(secure_transport, "server.key")
  SecureTransport::set_ca_certificate(secure_transport, "ca.crt")
  
  // 测试遥测数据签名
  let telemetry_data = TelemetryData::new("secure_operation")
  TelemetryData::add_metric(telemetry_data, "cpu_usage", 75.5)
  TelemetryData::add_metric(telemetry_data, "memory_usage", 60.2)
  TelemetryData::add_tag(telemetry_data, "service", "auth-service")
  TelemetryData::add_tag(telemetry_data, "environment", "production")
  
  let signature = SecureTransport::sign_data(secure_transport, telemetry_data)
  assert_true(signature.length() > 0)
  
  // 验证签名
  let is_valid = SecureTransport::verify_signature(secure_transport, telemetry_data, signature)
  assert_true(is_valid)
  
  // 测试数据加密传输
  let encrypted_data = SecureTransport::encrypt_for_transmission(secure_transport, telemetry_data)
  assert_true(encrypted_data != telemetry_data)
  
  let decrypted_data = SecureTransport::decrypt_from_transmission(secure_transport, encrypted_data)
  assert_eq(decrypted_data, telemetry_data)
  
  // 测试安全传输协议
  let transport_config = TransportConfig::new()
  TransportConfig::set_protocol(transport_config, "https")
  TransportConfig::set_port(transport_config, 443)
  TransportConfig::set_verify_peer(transport_config, true)
  TransportConfig::set_verify_host(transport_config, true)
  
  // 模拟安全传输
  let transmission_result = SecureTransport::transmit(secure_transport, telemetry_data, transport_config)
  assert_true(transmission_result.success)
  
  // 测试传输日志
  let transmission_logs = SecureTransport::get_transmission_logs(secure_transport)
  assert_true(transmission_logs.length() > 0)
  
  // 验证传输日志不包含敏感数据
  for log in transmission_logs {
    assert_false(log.contains("cpu_usage"))
    assert_false(log.contains("memory_usage"))
    assert_true(log.contains("transmitted") || log.contains("received"))
  }
}
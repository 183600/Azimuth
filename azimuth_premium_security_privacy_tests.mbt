// Azimuth 高级安全性和隐私保护测试用例
// 专注于验证遥测系统的安全性和隐私保护功能

// 测试1: 敏感数据脱敏和加密
test "敏感数据脱敏和加密测试" {
  // 创建安全管理器
  let security_manager = SecurityManager::new()
  SecurityManager::set_encryption_key(security_manager, "test-encryption-key-12345")
  
  let provider = MeterProvider::default()
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "security.test")
  
  // 创建包含敏感数据的span
  let sensitive_span = Tracer::start_span(tracer, "sensitive.data.operation")
  
  // 添加敏感属性
  Span::set_attribute(sensitive_span, "user.email", StringValue("user@example.com"))
  Span::set_attribute(sensitive_span, "user.phone", StringValue("+1234567890"))
  Span::set_attribute(sensitive_span, "credit.card", StringValue("4111-1111-1111-1111"))
  Span::set_attribute(sensitive_span, "social.security", StringValue("123-45-6789"))
  Span::set_attribute(sensitive_span, "api.key", StringValue("sk-1234567890abcdef"))
  Span::set_attribute(sensitive_span, "password", StringValue("secret123"))
  Span::set_attribute(sensitive_span, "personal.address", StringValue("123 Main St, City, State"))
  
  // 添加包含敏感数据的事件
  Span::add_event_with_attributes(sensitive_span, "user.login", [
    ("username", StringValue("john.doe")),
    ("email", StringValue("john.doe@example.com")),
    ("ip.address", StringValue("192.168.1.100"))
  ])
  
  // 应用数据脱敏
  let data_masker = DataMasker::new()
  DataMasker::add_pattern(data_masker, "email", @"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}")
  DataMasker::add_pattern(data_masker, "phone", @"\\+?\\d{10,15}")
  DataMasker::add_pattern(data_masker, "credit.card", @"\\d{4}[-\\s]?\\d{4}[-\\s]?\\d{4}[-\\s]?\\d{4}")
  DataMasker::add_pattern(data_masker, "ssn", @"\\d{3}-\\d{2}-\\d{4}")
  DataMasker::add_pattern(data_masker, "api.key", @"[a-zA-Z0-9]{20,}")
  DataMasker::add_pattern(data_masker, "ip.address", @"\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}")
  
  // 脱敏处理
  let masked_span = DataMasker::mask_span(data_masker, sensitive_span)
  
  // 验证脱敏效果
  let masked_attributes = Span::get_all_attributes(masked_span)
  
  // 验证邮箱脱敏
  assert_eq(masked_attributes.get("user.email"), "u***@e******.com")
  
  // 验证电话脱敏
  assert_eq(masked_attributes.get("user.phone"), "+*******90")
  
  // 验证信用卡脱敏
  assert_eq(masked_attributes.get("credit.card"), "4111-****-****-1111")
  
  // 验证SSN脱敏
  assert_eq(masked_attributes.get("social.security"), "***-**-6789")
  
  // 验证API密钥脱敏
  assert_eq(masked_attributes.get("api.key"), "sk-****************")
  
  // 验证IP地址脱敏
  let masked_events = Span::get_events(masked_span)
  let login_event = masked_events[0]
  assert_eq(login_event.get_attribute("ip.address"), "192.168.1.***")
  
  // 测试数据加密
  let encryption_service = EncryptionService::new(security_manager)
  let original_data = "sensitive user information"
  
  let encrypted_data = EncryptionService::encrypt(encryption_service, original_data)
  let decrypted_data = EncryptionService::decrypt(encryption_service, encrypted_data)
  
  assert_not_eq(encrypted_data, original_data) // 加密后数据应该不同
  assert_eq(decrypted_data, original_data) // 解密后数据应该相同
  
  // 测试字段级加密
  let field_encryption = FieldLevelEncryption::new(security_manager)
  FieldLevelEncryption::add_encrypted_field(field_encryption, "personal.address")
  
  let encrypted_span = FieldLevelEncryption::encrypt_fields(field_encryption, sensitive_span)
  let encrypted_address = Span::get_attribute(encrypted_span, "personal.address")
  
  assert_not_eq(encrypted_address, "123 Main St, City, State") // 应该被加密
  
  // 解密验证
  let decrypted_span = FieldLevelEncryption::decrypt_fields(field_encryption, encrypted_span)
  let decrypted_address = Span::get_attribute(decrypted_span, "personal.address")
  
  assert_eq(decrypted_address, "123 Main St, City, State") // 解密后应该相同
  
  Span::end(sensitive_span)
  Span::end(masked_span)
  Span::end(encrypted_span)
  Span::end(decrypted_span)
}

// 测试2: 访问控制和权限验证
test "访问控制和权限验证测试" {
  // 创建访问控制管理器
  let access_manager = AccessControlManager::new()
  
  // 定义角色和权限
  AccessControlManager::add_role(access_manager, "admin")
  AccessControlManager::add_role(access_manager, "operator")
  AccessControlManager::add_role(access_manager, "viewer")
  
  AccessControlManager::add_permission(access_manager, "telemetry.read")
  AccessControlManager::add_permission(access_manager, "telemetry.write")
  AccessControlManager::add_permission(access_manager, "telemetry.delete")
  AccessControlManager::add_permission(access_manager, "user.read")
  AccessControlManager::add_permission(access_manager, "system.configure")
  
  // 分配角色权限
  AccessControlManager::grant_permission(access_manager, "admin", "telemetry.read")
  AccessControlManager::grant_permission(access_manager, "admin", "telemetry.write")
  AccessControlManager::grant_permission(access_manager, "admin", "telemetry.delete")
  AccessControlManager::grant_permission(access_manager, "admin", "user.read")
  AccessControlManager::grant_permission(access_manager, "admin", "system.configure")
  
  AccessControlManager::grant_permission(access_manager, "operator", "telemetry.read")
  AccessControlManager::grant_permission(access_manager, "operator", "telemetry.write")
  
  AccessControlManager::grant_permission(access_manager, "viewer", "telemetry.read")
  
  // 创建用户
  let admin_user = User::new("admin-001", "admin")
  let operator_user = User::new("operator-001", "operator")
  let viewer_user = User::new("viewer-001", "viewer")
  
  // 创建测试遥测数据
  let provider = MeterProvider::default()
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "access.control.test")
  
  let test_span = Tracer::start_span(tracer, "access.controlled.operation")
  Span::set_attribute(test_span, "data.sensitivity", StringValue("confidential"))
  Span::set_attribute(test_span, "access.level", StringValue("restricted"))
  Span::end(test_span)
  
  // 测试访问控制
  let access_validator = AccessValidator::new(access_manager)
  
  // 管理员访问测试
  let admin_read_access = AccessValidator::can_read(access_validator, admin_user, test_span)
  let admin_write_access = AccessValidator::can_write(access_validator, admin_user, test_span)
  let admin_delete_access = AccessValidator::can_delete(access_validator, admin_user, test_span)
  
  assert_true(admin_read_access) // 管理员应该能读取
  assert_true(admin_write_access) // 管理员应该能写入
  assert_true(admin_delete_access) // 管理员应该能删除
  
  // 操作员访问测试
  let operator_read_access = AccessValidator::can_read(access_validator, operator_user, test_span)
  let operator_write_access = AccessValidator::can_write(access_validator, operator_user, test_span)
  let operator_delete_access = AccessValidator::can_delete(access_validator, operator_user, test_span)
  
  assert_true(operator_read_access) // 操作员应该能读取
  assert_true(operator_write_access) // 操作员应该能写入
  assert_false(operator_delete_access) // 操作员不应该能删除
  
  // 查看者访问测试
  let viewer_read_access = AccessValidator::can_read(access_validator, viewer_user, test_span)
  let viewer_write_access = AccessValidator::can_write(access_validator, viewer_user, test_span)
  let viewer_delete_access = AccessValidator::can_delete(access_validator, viewer_user, test_span)
  
  assert_true(viewer_read_access) // 查看者应该能读取
  assert_false(viewer_write_access) // 查看者不应该能写入
  assert_false(viewer_delete_access) // 查看者不应该能删除
  
  // 测试基于属性的访问控制 (ABAC)
  let abac_validator = ABACValidator::new()
  
  // 定义访问策略
  let policy = AccessPolicy::new("data.access.policy")
  AccessPolicy::add_rule(policy, fn(user, resource) {
    // 只有相同部门的用户才能访问部门数据
    let user_department = user.get_attribute("department")
    let resource_department = resource.get_attribute("department")
    
    user_department == resource_department
  })
  
  AccessPolicy::add_rule(policy, fn(user, resource) {
    // 管理员可以访问所有数据
    user.has_role("admin")
  })
  
  ABACValidator::add_policy(abac_validator, policy)
  
  // 创建部门用户
  let finance_user = User::new("finance-001", "viewer")
  finance_user.set_attribute("department", "finance")
  
  let hr_user = User::new("hr-001", "viewer")
  hr_user.set_attribute("department", "hr")
  
  // 创建部门数据
  let finance_span = Tracer::start_span(tracer, "finance.operation")
  Span::set_attribute(finance_span, "department", StringValue("finance"))
  Span::set_attribute(finance_span, "data.type", StringValue("financial"))
  Span::end(finance_span)
  
  let hr_span = Tracer::start_span(tracer, "hr.operation")
  Span::set_attribute(hr_span, "department", StringValue("hr"))
  Span::set_attribute(hr_span, "data.type", StringValue("personnel"))
  Span::end(hr_span)
  
  // 测试ABAC访问控制
  let finance_access_finance_user = ABACValidator::can_access(abac_validator, finance_user, finance_span)
  let finance_access_hr_user = ABACValidator::can_access(abac_validator, hr_user, finance_span)
  let hr_access_hr_user = ABACValidator::can_access(abac_validator, hr_user, hr_span)
  let hr_access_finance_user = ABACValidator::can_access(abac_validator, finance_user, hr_span)
  
  assert_true(finance_access_finance_user) // 财务用户应该能访问财务数据
  assert_false(finance_access_hr_user) // HR用户不应该能访问财务数据
  assert_true(hr_access_hr_user) // HR用户应该能访问HR数据
  assert_false(hr_access_finance_user) // 财务用户不应该能访问HR数据
  
  // 测试临时访问权限
  let temporary_access = TemporaryAccessManager::new()
  
  // 授予临时访问权限
  let expiry_time = Timestamp::now() + Duration::from_hours(1)
  TemporaryAccessManager::grant_temporary_access(temporary_access, viewer_user, "telemetry.write", expiry_time)
  
  // 验证临时访问
  let temp_write_access = TemporaryAccessManager::has_temporary_access(temporary_access, viewer_user, "telemetry.write")
  assert_true(temp_write_access) // 应该有临时写入权限
  
  // 测试过期权限
  let past_expiry = Timestamp::now() - Duration::from_hours(1)
  TemporaryAccessManager::grant_temporary_access(temporary_access, viewer_user, "telemetry.delete", past_expiry)
  
  let expired_access = TemporaryAccessManager::has_temporary_access(temporary_access, viewer_user, "telemetry.delete")
  assert_false(expired_access) // 过期的权限应该无效
}

// 测试3: 审计日志和合规性
test "审计日志和合规性测试" {
  // 创建审计管理器
  let audit_manager = AuditManager::new()
  AuditManager::set_retention_period(audit_manager, Duration::from_days(90)) // 90天保留期
  
  let provider = MeterProvider::default()
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "audit.test")
  
  // 创建审计事件记录器
  let audit_logger = AuditLogger::new(audit_manager)
  
  // 测试用户操作审计
  let test_user = User::new("audit-test-001", "operator")
  
  // 记录用户登录
  let login_event = AuditLogger::log_event(audit_logger, test_user, "user.login", [
    ("ip.address", StringValue("192.168.1.100")),
    ("user.agent", StringValue("Mozilla/5.0...")),
    ("login.timestamp", StringValue(Timestamp::now().to_string()))
  ])
  
  // 记录数据访问
  let access_event = AuditLogger::log_event(audit_logger, test_user, "data.access", [
    ("resource.type", StringValue("telemetry.span")),
    ("resource.id", StringValue("span-12345")),
    ("access.type", StringValue("read")),
    ("access.timestamp", StringValue(Timestamp::now().to_string()))
  ])
  
  // 记录敏感数据访问
  let sensitive_access_event = AuditLogger::log_event(audit_logger, test_user, "sensitive.data.access", [
    ("resource.type", StringValue("user.profile")),
    ("resource.id", StringValue("user-67890")),
    ("data.fields", StringValue("email,phone,address")),
    ("access.reason", StringValue("customer.support")),
    ("access.timestamp", StringValue(Timestamp::now().to_string()))
  ])
  
  // 记录配置更改
  let config_change_event = AuditLogger::log_event(audit_logger, test_user, "system.config.change", [
    ("config.section", StringValue("telemetry.sampling")),
    ("old.value", StringValue("0.1")),
    ("new.value", StringValue("0.2")),
    ("change.reason", StringValue("performance.optimization")),
    ("change.timestamp", StringValue(Timestamp::now().to_string()))
  ])
  
  // 创建测试span并记录审计
  let test_span = Tracer::start_span(tracer, "audited.operation")
  Span::set_attribute(test_span, "user.id", StringValue("audit-test-001"))
  Span::set_attribute(test_span, "operation.type", StringValue("data.query"))
  Span::end(test_span)
  
  // 记录span创建审计
  let span_create_event = AuditLogger::log_span_event(audit_logger, test_user, test_span, "span.created")
  
  // 验证审计日志完整性
  let audit_trail = AuditManager::get_user_audit_trail(audit_manager, test_user)
  
  assert_eq(audit_trail.events.length(), 5) // 应该有5个审计事件
  assert_true(audit_trail.events.contains(login_event))
  assert_true(audit_trail.events.contains(access_event))
  assert_true(audit_trail.events.contains(sensitive_access_event))
  assert_true(audit_trail.events.contains(config_change_event))
  assert_true(audit_trail.events.contains(span_create_event))
  
  // 验证审计事件属性
  for event in audit_trail.events {
    assert_true(event.has_attribute("user.id"))
    assert_true(event.has_attribute("event.type"))
    assert_true(event.has_attribute("timestamp"))
    assert_true(event.has_attribute("session.id"))
  }
  
  // 测试合规性检查
  let compliance_checker = ComplianceChecker::new()
  
  // 添加GDPR合规规则
  ComplianceChecker::add_rule(compliance_checker, "gdpr.data.access", fn(event) {
    if event.event_type == "sensitive.data.access" {
      return event.has_attribute("access.reason") && 
             event.has_attribute("data.fields") &&
             event.has_attribute("retention.period")
    }
    return true
  })
  
  // 添加SOX合规规则
  ComplianceChecker::add_rule(compliance_checker, "sox.financial.data", fn(event) {
    if event.resource_type == "financial.data" {
      return event.has_attribute("approval.id") &&
             event.has_attribute("reviewer.id")
    }
    return true
  })
  
  // 添加HIPAA合规规则
  ComplianceChecker::add_rule(compliance_checker, "hipaa.protected.health", fn(event) {
    if event.resource_type == "protected.health.info" {
      return event.has_attribute("patient.consent") &&
             event.has_attribute("minimum.necessary")
    }
    return true
  })
  
  // 执行合规性检查
  let compliance_report = ComplianceChecker::check_compliance(compliance_checker, audit_trail)
  
  assert_true(compliance_report.overall_compliance_score >= 0.8) // 合规评分应该大于80%
  assert_true(compliance_report.violations.length() == 0) // 应该没有违规
  
  // 测试审计日志不可变性
  let immutability_tester = AuditLogImmutabilityTester::new(audit_manager)
  
  // 尝试修改审计日志
  let modification_attempt = immutability_tester.attempt_modification(login_event, [
    ("ip.address", StringValue("malicious.ip.address"))
  ])
  
  assert_false(modification_attempt.success) // 修改应该失败
  assert_true(modification_attempt.reason.contains("immutable")) // 应该有不可变原因
  
  // 测试审计日志加密
  let encryption_tester = AuditLogEncryptionTester::new(audit_manager)
  
  // 加密敏感审计日志
  let encrypted_event = encryption_tester.encrypt_sensitive_event(sensitive_access_event)
  
  assert_not_eq(encrypted_event.raw_data, sensitive_access_event.raw_data) // 加密后数据应该不同
  
  // 解密验证
  let decrypted_event = encryption_tester.decrypt_event(encrypted_event)
  assert_eq(decrypted_event.raw_data, sensitive_access_event.raw_data) // 解密后应该相同
  
  // 测试审计日志保留策略
  let retention_tester = AuditLogRetentionTester::new(audit_manager)
  
  // 创建过期审计事件
  let old_timestamp = Timestamp::now() - Duration::from_days(100) // 100天前
  let old_event = AuditLogger::log_event_with_timestamp(audit_logger, test_user, "old.event", [], old_timestamp)
  
  // 执行保留策略清理
  let cleanup_result = retention_tester.apply_retention_policy()
  
  assert_true(cleanup_result.cleaned_up_events > 0) // 应该清理了一些过期事件
  assert_false(cleanup_result.remaining_events.contains(old_event)) // 过期事件应该被清理
}

// 测试4: 数据传输安全
test "数据传输安全测试" {
  // 创建传输安全管理器
  let transport_security = TransportSecurityManager::new()
  
  // 配置TLS设置
  let tls_config = TLSConfiguration::new()
  TLSConfiguration::set_min_version(tls_config, "1.2")
  TLSConfiguration::set_cipher_suites(tls_config, ["TLS_AES_256_GCM_SHA384", "TLS_CHACHA20_POLY1305_SHA256"])
  TLSConfiguration::set_certificate_validation(tls_config, true)
  
  TransportSecurityManager::set_tls_config(transport_security, tls_config)
  
  // 创建测试数据
  let provider = MeterProvider::default()
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "transport.security.test")
  
  let test_span = Tracer::start_span(tracer, "secure.transmission")
  Span::set_attribute(test_span, "data.classification", StringValue("confidential"))
  Span::set_attribute(test_span, "transmission.required", StringValue("encrypted"))
  
  // 添加敏感事件
  Span::add_event_with_attributes(test_span, "sensitive.operation", [
    ("user.id", StringValue("user-12345")),
    ("personal.info", StringValue("sensitive data")),
    ("financial.data", StringValue("account details"))
  ])
  
  Span::end(test_span)
  
  // 测试数据序列化安全
  let secure_serializer = SecureSerializer::new(transport_security)
  
  // 使用安全序列化
  let serialized_data = SecureSerializer::serialize(secure_serializer, test_span)
  
  // 验证序列化数据不包含明文敏感信息
  assert_false(serialized_data.contains("user-12345"))
  assert_false(serialized_data.contains("sensitive data"))
  assert_false(serialized_data.contains("account details"))
  
  // 测试安全反序列化
  let deserialized_span = SecureSerializer::deserialize(secure_serializer, serialized_data)
  
  // 验证反序列化后数据完整性
  assert_eq(Span::get_attribute(deserialized_span, "data.classification"), "confidential")
  assert_eq(Span::get_attribute(deserialized_span, "transmission.required"), "encrypted")
  
  // 测试传输加密
  let transmission_encryptor = TransmissionEncryptor::new(transport_security)
  
  // 模拟网络传输
  let original_data = serialized_data
  let encrypted_transmission = TransmissionEncryptor::encrypt_for_transmission(transmission_encryptor, original_data)
  
  // 验证传输加密
  assert_not_eq(encrypted_transmission.data, original_data)
  assert_true(encrypted_transmission.encrypted)
  assert_true(encrypted_transmission.integrity_protected)
  
  // 模拟接收端解密
  let decrypted_transmission = TransmissionEncryptor::decrypt_from_transmission(transmission_encryptor, encrypted_transmission)
  
  // 验证解密完整性
  assert_eq(decrypted_transmission, original_data)
  
  // 测试传输完整性验证
  let integrity_verifier = TransmissionIntegrityVerifier::new(transport_security)
  
  // 添加完整性签名
  let signed_data = integrity_verifier.sign_data(original_data)
  
  // 验证完整性签名
  let integrity_check = integrity_verifier.verify_signature(signed_data)
  assert_true(integrity_check.valid)
  assert_true(integrity_check.integrity_intact)
  
  // 测试篡改检测
  let tampered_data = signed_data.data.substring(0, signed_data.data.length() - 1) + "X" // 篡改最后一个字符
  let tampered_check = integrity_verifier.verify_signature({ data: tampered_data, signature: signed_data.signature })
  
  assert_false(tampered_check.valid)
  assert_false(tampered_check.integrity_intact)
  assert_true(tampered_check.tampering_detected)
  
  // 测试安全通道建立
  let channel_manager = SecureChannelManager::new(transport_security)
  
  // 模拟客户端和服务端
  let client_handshake = SecureChannelManager::initiate_handshake(channel_manager, "client")
  let server_handshake = SecureChannelManager::respond_to_handshake(channel_manager, "server", client_handshake.handshake_data)
  
  let client_completion = SecureChannelManager::complete_handshake(channel_manager, client_handshake.session_id, server_handshake.handshake_data)
  
  // 验证安全通道建立
  assert_true(client_completion.success)
  assert_true(client_completion.channel.established)
  assert_true(client_completion.channel.encrypted)
  assert_true(client_completion.channel.authenticated)
  
  // 通过安全通道传输数据
  let secure_transmission = SecureChannelManager::transmit_data(channel_manager, client_completion.channel.id, original_data)
  
  assert_true(secure_transmission.success)
  assert_true(secure_transmission.data_encrypted)
  assert_true(secure_transmission.integrity_protected)
  
  // 测试重放攻击防护
  let replay_protection = ReplayProtectionManager::new(transport_security)
  
  // 记录传输
  let transmission_id = replay_protection.record_transmission(original_data, Timestamp::now())
  
  // 尝试重放
  let replay_attempt = replay_protection.detect_replay(transmission_id, original_data)
  
  assert_true(replay_attempt.is_replay) // 应该检测到重放
  assert_true(replay_attempt.original_timestamp > 0) // 应该有原始时间戳
  
  // 测试新传输
  let new_data = original_data + " (updated)"
  let new_transmission_id = replay_protection.record_transmission(new_data, Timestamp::now())
  let new_replay_check = replay_protection.detect_replay(new_transmission_id, new_data)
  
  assert_false(new_replay_check.is_replay) // 新传输不应该被检测为重放
}

// 测试5: 身份认证和授权
test "身份认证和授权测试" {
  // 创建身份认证管理器
  let auth_manager = AuthenticationManager::new()
  
  // 配置认证方法
  AuthenticationManager::enable_password_auth(auth_manager)
  AuthenticationManager::enable_token_auth(auth_manager)
  AuthenticationManager::enable_certificate_auth(auth_manager)
  AuthenticationManager::enable_multi_factor_auth(auth_manager)
  
  // 创建测试用户
  let test_user = User::new("auth-test-001", "operator")
  test_user.set_password("SecurePassword123!")
  test_user.set_email("auth-test@example.com")
  
  // 添加用户到认证系统
  AuthenticationManager::add_user(auth_manager, test_user)
  
  // 测试密码认证
  let password_auth = PasswordAuthenticator::new(auth_manager)
  
  let correct_password_result = PasswordAuthenticator::authenticate(password_auth, "auth-test-001", "SecurePassword123!")
  assert_true(correct_password_result.success)
  assert_true(correct_password_result.authenticated)
  assert_eq(correct_password_result.user.id, "auth-test-001")
  
  let incorrect_password_result = PasswordAuthenticator::authenticate(password_auth, "auth-test-001", "WrongPassword")
  assert_false(incorrect_password_result.success)
  assert_false(incorrect_password_result.authenticated)
  assert_true(incorrect_password_result.error.contains("invalid credentials"))
  
  // 测试令牌认证
  let token_auth = TokenAuthenticator::new(auth_manager)
  
  // 生成访问令牌
  let token_generator = TokenGenerator::new(auth_manager)
  let access_token = TokenGenerator::generate_access_token(token_generator, test_user, Duration::from_hours(1))
  
  assert_true(access_token.value.length() > 0)
  assert_true(access_token.expires_at > Timestamp::now())
  
  // 验证令牌
  let token_auth_result = TokenAuthenticator::authenticate(token_auth, access_token.value)
  assert_true(token_auth_result.success)
  assert_true(token_auth_result.authenticated)
  assert_eq(token_auth_result.user.id, "auth-test-001")
  
  // 测试令牌过期
  let expired_token = TokenGenerator::generate_access_token(token_generator, test_user, Duration::from_seconds(-1)) // 已过期
  let expired_token_result = TokenAuthenticator::authenticate(token_auth, expired_token.value)
  
  assert_false(expired_token_result.success)
  assert_false(expired_token_result.authenticated)
  assert_true(expired_token_result.error.contains("token expired"))
  
  // 测试多因素认证
  let mfa_auth = MFAAuthenticator::new(auth_manager)
  
  // 设置MFA
  let mfa_setup = MFAAuthenticator::setup_mfa(mfa_auth, test_user, "totp")
  assert_true(mfa_setup.success)
  assert_true(mfa_setup.secret_key.length() > 0)
  assert_true(mfa_setup.backup_codes.length() > 0)
  
  // 模拟MFA验证
  let mfa_code = MFAAuthenticator::generate_test_code(mfa_auth, mfa_setup.secret_key)
  let mfa_auth_result = MFAAuthenticator::authenticate(mfa_auth, test_user, mfa_code)
  
  assert_true(mfa_auth_result.success)
  assert_true(mfa_auth_result.authenticated)
  
  // 测试证书认证
  let cert_auth = CertificateAuthenticator::new(auth_manager)
  
  // 生成测试证书
  let cert_generator = CertificateGenerator::new(auth_manager)
  let test_certificate = cert_generator.generate_user_certificate(test_user, Duration::from_days(365))
  
  assert_true(test_certificate.certificate.length() > 0)
  assert_true(test_certificate.private_key.length() > 0)
  assert_true(test_certificate.expires_at > Timestamp::now())
  
  // 验证证书
  let cert_auth_result = CertificateAuthenticator::authenticate(cert_auth, test_certificate.certificate)
  assert_true(cert_auth_result.success)
  assert_true(cert_auth_result.authenticated)
  assert_eq(cert_auth_result.user.id, "auth-test-001")
  
  // 测试会话管理
  let session_manager = SessionManager::new(auth_manager)
  
  // 创建会话
  let session = SessionManager::create_session(session_manager, test_user, Duration::from_hours(8))
  
  assert_true(session.id.length() > 0)
  assert_true(session.user_id == "auth-test-001")
  assert_true(session.expires_at > Timestamp::now())
  
  // 验证会话
  let session_validation = SessionManager::validate_session(session_manager, session.id)
  assert_true(session_validation.valid)
  assert_eq(session_validation.session.user_id, "auth-test-001")
  
  // 测试会话过期
  let expired_session = SessionManager::create_session(session_manager, test_user, Duration::from_seconds(-1))
  let expired_session_validation = SessionManager::validate_session(session_manager, expired_session.id)
  
  assert_false(expired_session_validation.valid)
  assert_true(expired_session_validation.reason.contains("expired"))
  
  // 测试会话撤销
  SessionManager::revoke_session(session_manager, session.id)
  let revoked_session_validation = SessionManager::validate_session(session_manager, session.id)
  
  assert_false(revoked_session_validation.valid)
  assert_true(revoked_session_validation.reason.contains("revoked"))
  
  // 测试权限提升检测
  let privilege_escalation_detector = PrivilegeEscalationDetector::new(auth_manager)
  
  // 模拟正常权限使用
  let normal_access = privilege_escalation_detector.check_access(test_user, "telemetry.read")
  assert_true(normal_access.allowed)
  assert_false(normal_access.privilege_escalation_detected)
  
  // 模拟权限提升尝试
  let escalation_attempt = privilege_escalation_detector.check_access(test_user, "system.admin")
  assert_false(escalation_attempt.allowed)
  assert_true(escalation_attempt.privilege_escalation_detected)
  assert_true(escalation_attempt.alert_generated)
  
  // 测试异常登录检测
  let anomaly_detector = LoginAnomalyDetector::new(auth_manager)
  
  // 记录正常登录模式
  for i in 1..=5 {
    let login_record = LoginRecord::new(test_user.id, "192.168.1.100", "Mozilla/5.0...", Timestamp::now() - Duration::from_hours(i.to_int()))
    anomaly_detector.record_login(login_record)
  }
  
  // 正常登录检测
  let normal_login = LoginRecord::new(test_user.id, "192.168.1.100", "Mozilla/5.0...", Timestamp::now())
  let normal_login_check = anomaly_detector.analyze_login(normal_login)
  
  assert_false(normal_login_check.is_anomalous)
  assert_true(normal_login_check.risk_score < 0.3)
  
  // 异常登录检测（不同IP）
  let anomalous_login = LoginRecord::new(test_user.id, "10.0.0.1", "Mozilla/5.0...", Timestamp::now())
  let anomalous_login_check = anomaly_detector.analyze_login(anomalous_login)
  
  assert_true(anomalous_login_check.is_anomalous)
  assert_true(anomalous_login_check.risk_score > 0.7)
  assert_true(anomalous_login_check.alert_required)
}
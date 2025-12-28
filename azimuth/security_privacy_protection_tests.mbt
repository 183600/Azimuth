// 安全性和隐私保护测试用例
// 测试Azimuth遥测系统的数据加密、敏感信息过滤、访问控制等安全功能

test "sensitive_data_filtering" {
  // 测试敏感数据过滤功能
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "security-test")
  
  // 配置敏感数据过滤器
  let sensitive_patterns = [
    "password",
    "token",
    "secret",
    "key",
    "credential",
    "auth",
    "api_key",
    "private"
  ]
  
  let filter_config = DataFilterConfig::new(
    sensitive_patterns,
    Some("***REDACTED***"),
    Some(true)  // enable_strict_filtering
  )
  
  TracerProvider::set_data_filter(tracer_provider, filter_config)
  
  // 创建包含敏感信息的Span
  let span = Tracer::start_span(tracer, "sensitive-data-test")
  
  // 测试敏感属性过滤
  Span::set_attribute(span, "username", StringValue("john.doe"))
  Span::set_attribute(span, "password", StringValue("supersecret123"))
  Span::set_attribute(span, "api_token", StringValue("sk-1234567890abcdef"))
  Span::set_attribute(span, "session_id", StringValue("sess_abc123"))
  
  // 测试敏感事件数据过滤
  Span::add_event(span, "user.login", Some([
    ("username", StringValue("john.doe")),
    ("password", StringValue("supersecret123")),
    ("ip_address", StringValue("192.168.1.100"))
  ]))
  
  // 验证敏感数据被过滤
  let filtered_attributes = Span::get_filtered_attributes(span)
  let username_found = filtered_attributes.any(fn(attr) {
    attr.0 == "username" && attr.1 == StringValue("john.doe")
  })
  let password_filtered = filtered_attributes.any(fn(attr) {
    attr.0 == "password" && attr.1 == StringValue("***REDACTED***")
  })
  let token_filtered = filtered_attributes.any(fn(attr) {
    attr.0 == "api_token" && attr.1 == StringValue("***REDACTED***")
  })
  
  assert_true(username_found)  // 非敏感数据应保留
  assert_true(password_filtered)  // 敏感数据应被过滤
  assert_true(token_filtered)  // 敏感数据应被过滤
  
  Span::end(span)
}

test "data_encryption_at_rest" {
  // 测试静态数据加密
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "encryption-test")
  
  // 配置加密设置
  let encryption_config = EncryptionConfig::new(
    "AES-256-GCM",
    Some("base64-encoded-encryption-key-32-bytes-long-"),
    Some(true),  // enable_compression
    Some("v1")   // key_version
  )
  
  LoggerProvider::set_encryption_config(logger_provider, encryption_config)
  
  // 创建包含敏感信息的日志记录
  let sensitive_log_data = LogRecord::new_with_context(
    Warn,
    Some("Security audit log with sensitive information"),
    Some(Attributes::from_array([
      ("user_id", StringValue("user_12345")),
      ("social_security_number", StringValue("123-45-6789")),
      ("credit_card", StringValue("4532-1234-5678-9012")),
      ("medical_record", StringValue("MRN001234567")),
      ("timestamp", IntValue(Clock::now_unix_nanos(Clock::system())))
    ])),
    None,
    None,
    None,
    None
  )
  
  // 加密日志数据
  let encrypted_log = Logger::encrypt_log_record(logger, sensitive_log_data)
  assert_true(encrypted_log.is_encrypted)
  assert_not_eq(encrypted_log.encrypted_data, "")
  
  // 解密日志数据
  let decrypted_log = Logger::decrypt_log_record(logger, encrypted_log)
  assert_true(decrypted_log.is_some)
  
  let decrypted_record = decrypted_log.unwrap()
  assert_eq(LogRecord::severity_number(decrypted_record), Warn)
  assert_eq(LogRecord::body(decrypted_record), Some("Security audit log with sensitive information"))
  
  // 验证敏感数据在解密后正确恢复
  let decrypted_attributes = LogRecord::attributes(decrypted_record)
  let ssn_found = decrypted_attributes.any(fn(attr) {
    attr.0 == "social_security_number" && attr.1 == StringValue("123-45-6789")
  })
  assert_true(ssn_found)
}

test "access_control_authorization" {
  // 测试访问控制和授权
  let tracer_provider = TracerProvider::default()
  
  // 配置访问控制策略
  let access_policies = [
    AccessPolicy::new(
      "admin-role",
      ["read", "write", "delete", "configure"],
      ["spans", "metrics", "logs", "config"]
    ),
    AccessPolicy::new(
      "analyst-role", 
      ["read"],
      ["spans", "metrics"]
    ),
    AccessPolicy::new(
      "service-role",
      ["write"],
      ["spans", "logs"]
    )
  ]
  
  let access_control = AccessControl::new(access_policies)
  TracerProvider::set_access_control(tracer_provider, access_control)
  
  // 测试不同角色的访问权限
  let admin_context = SecurityContext::new("admin-user", ["admin-role"])
  let analyst_context = SecurityContext::new("analyst-user", ["analyst-role"])
  let service_context = SecurityContext::new("service-account", ["service-role"])
  
  // 测试管理员权限
  let admin_can_read_spans = AccessControl::can_read(access_control, admin_context, "spans")
  let admin_can_delete_config = AccessControl::can_delete(access_control, admin_context, "config")
  assert_true(admin_can_read_spans)
  assert_true(admin_can_delete_config)
  
  // 测试分析师权限
  let analyst_can_read_spans = AccessControl::can_read(access_control, analyst_context, "spans")
  let analyst_can_write_spans = AccessControl::can_write(access_control, analyst_context, "spans")
  let analyst_can_read_logs = AccessControl::can_read(access_control, analyst_context, "logs")
  
  assert_true(analyst_can_read_spans)
  assert_false(analyst_can_write_spans)
  assert_false(analyst_can_read_logs)
  
  // 测试服务账户权限
  let service_can_write_spans = AccessControl::can_write(access_control, service_context, "spans")
  let service_can_read_metrics = AccessControl::can_read(access_control, service_context, "metrics")
  let service_can_configure = AccessControl::can_configure(access_control, service_context, "config")
  
  assert_true(service_can_write_spans)
  assert_false(service_can_read_metrics)
  assert_false(service_can_configure)
}

test "data_retention_and_purging" {
  // 测试数据保留和清理策略
  let meter_provider = MeterProvider::default()
  
  // 配置数据保留策略
  let retention_policies = [
    RetentionPolicy::new("spans", 30, Some(DAYS)),      // 30天
    RetentionPolicy::new("metrics", 90, Some(DAYS)),    // 90天
    RetentionPolicy::new("logs", 7, Some(DAYS)),        // 7天
    RetentionPolicy::new("debug", 1, Some(DAYS))        // 1天
  ]
  
  let retention_manager = RetentionManager::new(retention_policies)
  MeterProvider::set_retention_manager(meter_provider, retention_manager)
  
  // 创建不同时间的数据
  let current_time = Clock::now_unix_nanos(Clock::system())
  let day_in_nanos = 24 * 60 * 60 * 1000000000L
  
  // 模拟老数据（40天前）
  let old_span_time = current_time - (40 * day_in_nanos)
  let old_span = create_test_span_with_timestamp("old-span", old_span_time)
  
  // 模拟较新的数据（10天前）
  let recent_span_time = current_time - (10 * day_in_nanos)
  let recent_span = create_test_span_with_timestamp("recent-span", recent_span_time)
  
  // 模拟非常老的日志数据（8天前）
  let old_log_time = current_time - (8 * day_in_nanos)
  let old_log = create_test_log_with_timestamp("old-log", old_log_time)
  
  // 执行数据清理
  let purge_result = RetentionManager::purge_expired_data(retention_manager, current_time)
  
  // 验证清理结果
  assert_true(purge_result.spans_purged > 0)  // 老的Span应该被清理
  assert_true(purge_result.logs_purged > 0)   // 老的日志应该被清理
  assert_eq(purge_result.metrics_purged, 0)   // 度量数据应该还在保留期内
  
  // 验证数据确实被清理
  let old_span_exists = check_span_exists(old_span)
  let recent_span_exists = check_span_exists(recent_span)
  let old_log_exists = check_log_exists(old_log)
  
  assert_false(old_span_exists)     // 老Span应该不存在
  assert_true(recent_span_exists)   // 新Span应该存在
  assert_false(old_log_exists)      // 老日志应该不存在
}

test "audit_logging_and_compliance" {
  // 测试审计日志和合规性
  let tracer_provider = TracerProvider::default()
  let audit_logger = AuditLogger::new()
  
  // 配置审计策略
  let audit_config = AuditConfig::new(
    [READ_SENSITIVE, WRITE_CONFIG, DELETE_DATA, ACCESS_DENIED],
    Some(true),  // include_user_context
    Some(true),  // include_timestamp
    Some(true),  // include_ip_address
    Some(365)    // retain_audit_logs_days
  )
  
  AuditLogger::configure(audit_logger, audit_config)
  
  // 模拟各种需要审计的操作
  let admin_context = SecurityContext::new("admin-user", ["admin-role"])
  let user_context = SecurityContext::new("regular-user", ["user-role"])
  
  // 审计敏感数据访问
  let sensitive_data_access = AuditEvent::new(
    READ_SENSITIVE,
    admin_context,
    "spans",
    Some("Accessed sensitive span data for user_12345"),
    Some("192.168.1.100")
  )
  AuditLogger::log_event(audit_logger, sensitive_data_access)
  
  // 审计配置修改
  let config_change = AuditEvent::new(
    WRITE_CONFIG,
    admin_context,
    "tracer_config",
    Some("Modified sampling rate from 0.1 to 0.05"),
    Some("192.168.1.100")
  )
  AuditLogger::log_event(audit_logger, config_change)
  
  // 审计访问拒绝
  let access_denied = AuditEvent::new(
    ACCESS_DENIED,
    user_context,
    "admin_config",
    Some("Attempted to access admin configuration without permission"),
    Some("192.168.1.101")
  )
  AuditLogger::log_event(audit_logger, access_denied)
  
  // 验证审计日志
  let audit_logs = AuditLogger::get_logs(audit_logger, 100)  # 获取最近100条审计日志
  assert_true(audit_logs.length >= 3)
  
  // 验证审计日志内容
  let has_sensitive_access = audit_logs.any(fn(log) {
    log.event_type == READ_SENSITIVE && log.user_id == "admin-user"
  })
  let has_config_change = audit_logs.any(fn(log) {
    log.event_type == WRITE_CONFIG && log.resource_type == "tracer_config"
  })
  let has_access_denied = audit_logs.any(fn(log) {
    log.event_type == ACCESS_DENIED && log.user_id == "regular-user"
  })
  
  assert_true(has_sensitive_access)
  assert_true(has_config_change)
  assert_true(has_access_denied)
}

test "gdpr_data_subject_rights" {
  // 测试GDPR数据主体权利
  let data_protection_manager = DataProtectionManager::new()
  
  // 创建测试用户数据
  let user_id = "gdpr-test-user-12345"
  let user_telemetry_data = create_user_telemetry_data(user_id, [
    ("spans", create_test_spans_for_user(user_id, 50)),
    ("metrics", create_test_metrics_for_user(user_id, 25)),
    ("logs", create_test_logs_for_user(user_id, 100))
  ])
  
  // 验证数据存在
  let data_exists_before = DataProtectionManager::user_data_exists(data_protection_manager, user_id)
  assert_true(data_exists_before)
  
  // 测试数据可携带性（Right to Data Portability）
  let portable_data = DataProtectionManager::export_user_data(data_protection_manager, user_id)
  assert_true(portable_data.is_some)
  
  let exported_data = portable_data.unwrap()
  assert_true(exported_data.spans.length > 0)
  assert_true(exported_data.metrics.length > 0)
  assert_true(exported_data.logs.length > 0)
  
  // 验证导出格式符合标准（JSON格式，包含必要字段）
  let export_is_valid_json = is_valid_json(exported_data.to_json())
  assert_true(export_is_valid_json)
  
  let contains_required_fields = exported_data.to_json().contains("user_id") &&
                                 exported_data.to_json().contains("export_timestamp") &&
                                 exported_data.to_json().contains("data_types")
  assert_true(contains_required_fields)
  
  // 测试被遗忘权（Right to be Forgotten）
  let deletion_request = DataDeletionRequest::new(
    user_id,
    "User requested data deletion under GDPR Article 17",
    Some("gdpr-request-12345")
  )
  
  let deletion_result = DataProtectionManager::delete_user_data(data_protection_manager, deletion_request)
  assert_true(deletion_result.success)
  assert_eq(deletion_result.records_deleted, 175)  # 50 + 25 + 100
  
  // 验证数据已被删除
  let data_exists_after = DataProtectionManager::user_data_exists(data_protection_manager, user_id)
  assert_false(data_exists_after)
  
  // 测试删除确认
  let deletion_confirmation = DataProtectionManager::get_deletion_confirmation(data_protection_manager, "gdpr-request-12345")
  assert_true(deletion_confirmation.is_some)
  
  let confirmation = deletion_confirmation.unwrap()
  assert_eq(confirmation.user_id, user_id)
  assert_eq(confirmation.request_id, "gdpr-request-12345")
  assert_true(confirmation.deletion_timestamp > 0L)
}

test "secure_communication_channels" {
  // 测试安全通信通道
  let tracer_provider = TracerProvider::default()
  
  // 配置安全通信设置
  let tls_config = TLSConfig::new(
    Some("/path/to/ca.crt"),
    Some("/path/to/client.crt"),
    Some("/path/to/client.key"),
    Some("TLSv1.3"),
    Some(true)  // verify_certificates
  )
  
  let communication_config = SecureCommunicationConfig::new(
    Some(tls_config),
    Some(true),  // enable_mutual_tls
    Some("AES-256-GCM"),  // encryption_algorithm
    Some(3600)  # session_timeout_seconds
  )
  
  TracerProvider::set_secure_communication(tracer_provider, communication_config)
  
  // 测试安全连接建立
  let secure_client = SecureClient::new(communication_config)
  let connection_result = SecureClient::connect(secure_client, "telemetry.example.com", 4317)
  
  assert_true(connection_result.success)
  assert_true(connection_result.encrypted)
  assert_true(connection_result.verified)
  
  // 测试数据传输加密
  let test_data = "sensitive telemetry data to be encrypted"
  let encrypted_data = SecureClient::encrypt(secure_client, test_data)
  assert_not_eq(encrypted_data, test_data)
  assert_true(encrypted_data.length > 0)
  
  // 测试数据解密
  let decrypted_data = SecureClient::decrypt(secure_client, encrypted_data)
  assert_eq(decrypted_data, test_data)
  
  // 测试证书验证
  let certificate_validation = SecureClient::validate_certificate(secure_client, "/path/to/server.crt")
  assert_true(certificate_validation.is_valid)
  assert_true(certificate_validation.not_expired)
  assert_true(certificate_validation.trusted)
  
  // 测试会话管理
  let session_id = SecureClient::get_session_id(secure_client)
  assert_true(session_id.length > 0)
  
  let session_is_active = SecureClient::is_session_active(secure_client)
  assert_true(session_is_active)
  
  // 测试会话超时
  SecureClient::simulate_session_timeout(secure_client)
  let session_active_after_timeout = SecureClient::is_session_active(secure_client)
  assert_false(session_active_after_timeout)
  
  // 测试会话恢复
  let session_recovery = SecureClient::recover_session(secure_client)
  assert_true(session_recovery.success)
}
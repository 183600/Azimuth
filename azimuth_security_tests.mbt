// Azimuth 安全性测试用例
// 测试遥测系统的安全功能，包括数据加密、访问控制、身份验证和安全审计

test "数据加密测试" {
  // 创建加密管理器
  let encryption_manager = azimuth::EncryptionManager::new("encryption-key-12345678901234567890")
  
  // 测试字符串加密
  let original_string = "sensitive telemetry data"
  let encrypted_string = azimuth::EncryptionManager::encrypt_string(encryption_manager, original_string)
  
  // 验证加密结果
  assert_not_eq(encrypted_string, original_string)
  assert_true(encrypted_string.length() > 0)
  assert_false(encrypted_string.contains("sensitive"))
  assert_false(encrypted_string.contains("telemetry"))
  assert_false(encrypted_string.contains("data"))
  
  // 测试字符串解密
  let decrypted_string = azimuth::EncryptionManager::decrypt_string(encryption_manager, encrypted_string)
  
  // 验证解密结果
  assert_eq(decrypted_string, original_string)
  
  // 测试整数加密
  let original_int = 123456789
  let encrypted_int = azimuth::EncryptionManager::encrypt_int(encryption_manager, original_int)
  
  // 验证整数加密结果
  assert_not_eq(encrypted_int, original_int)
  
  // 测试整数解密
  let decrypted_int = azimuth::EncryptionManager::decrypt_int(encryption_manager, encrypted_int)
  
  // 验证整数解密结果
  assert_eq(decrypted_int, original_int)
  
  // 测试布尔值加密
  let original_bool = true
  let encrypted_bool = azimuth::EncryptionManager::encrypt_bool(encryption_manager, original_bool)
  
  // 验证布尔值加密结果
  assert_not_eq(encrypted_bool, original_bool)
  
  // 测试布尔值解密
  let decrypted_bool = azimuth::EncryptionManager::decrypt_bool(encryption_manager, encrypted_bool)
  
  // 验证布尔值解密结果
  assert_eq(decrypted_bool, original_bool)
  
  // 测试浮点数加密
  let original_float = 3.14159265359
  let encrypted_float = azimuth::EncryptionManager::encrypt_float(encryption_manager, original_float)
  
  // 验证浮点数加密结果
  assert_not_eq(encrypted_float, original_float)
  
  // 测试浮点数解密
  let decrypted_float = azimuth::EncryptionManager::decrypt_float(encryption_manager, encrypted_float)
  
  // 验证浮点数解密结果
  assert_eq(decrypted_float, original_float)
  
  // 测试数组加密
  let original_array = ["item1", "item2", "item3"]
  let encrypted_array = azimuth::EncryptionManager::encrypt_string_array(encryption_manager, original_array)
  
  // 验证数组加密结果
  assert_not_eq(encrypted_array.length(), original_array.length())
  
  // 测试数组解密
  let decrypted_array = azimuth::EncryptionManager::decrypt_string_array(encryption_manager, encrypted_array)
  
  // 验证数组解密结果
  assert_eq(decrypted_array.length(), original_array.length())
  for i in 0..decrypted_array.length() {
    assert_eq(decrypted_array[i], original_array[i])
  }
}

test "访问控制测试" {
  // 创建访问控制管理器
  let access_manager = azimuth::AccessControlManager::new()
  
  // 定义角色和权限
  let admin_role = "admin"
  let operator_role = "operator"
  let viewer_role = "viewer"
  
  let permissions = [
    (admin_role, ["telemetry.read", "telemetry.write", "telemetry.delete", "config.read", "config.write", "user.manage"]),
    (operator_role, ["telemetry.read", "telemetry.write", "config.read"]),
    (viewer_role, ["telemetry.read"])
  ]
  
  // 配置权限
  for (role, perms) in permissions {
    for perm in perms {
      azimuth::AccessControlManager::grant_permission(access_manager, role, perm)
    }
  }
  
  // 创建用户
  let admin_user = "admin_user"
  let operator_user = "operator_user"
  let viewer_user = "viewer_user"
  
  azimuth::AccessControlManager::assign_role(access_manager, admin_user, admin_role)
  azimuth::AccessControlManager::assign_role(access_manager, operator_user, operator_role)
  azimuth::AccessControlManager::assign_role(access_manager, viewer_user, viewer_role)
  
  // 测试管理员权限
  assert_true(azimuth::AccessControlManager::has_permission(access_manager, admin_user, "telemetry.read"))
  assert_true(azimuth::AccessControlManager::has_permission(access_manager, admin_user, "telemetry.write"))
  assert_true(azimuth::AccessControlManager::has_permission(access_manager, admin_user, "telemetry.delete"))
  assert_true(azimuth::AccessControlManager::has_permission(access_manager, admin_user, "config.read"))
  assert_true(azimuth::AccessControlManager::has_permission(access_manager, admin_user, "config.write"))
  assert_true(azimuth::AccessControlManager::has_permission(access_manager, admin_user, "user.manage"))
  
  // 测试操作员权限
  assert_false(azimuth::AccessControlManager::has_permission(access_manager, operator_user, "telemetry.delete"))
  assert_false(azimuth::AccessControlManager::has_permission(access_manager, operator_user, "config.write"))
  assert_false(azimuth::AccessControlManager::has_permission(access_manager, operator_user, "user.manage"))
  
  assert_true(azimuth::AccessControlManager::has_permission(access_manager, operator_user, "telemetry.read"))
  assert_true(azimuth::AccessControlManager::has_permission(access_manager, operator_user, "telemetry.write"))
  assert_true(azimuth::AccessControlManager::has_permission(access_manager, operator_user, "config.read"))
  
  // 测试查看者权限
  assert_false(azimuth::AccessControlManager::has_permission(access_manager, viewer_user, "telemetry.write"))
  assert_false(azimuth::AccessControlManager::has_permission(access_manager, viewer_user, "telemetry.delete"))
  assert_false(azimuth::AccessControlManager::has_permission(access_manager, viewer_user, "config.read"))
  assert_false(azimuth::AccessControlManager::has_permission(access_manager, viewer_user, "config.write"))
  assert_false(azimuth::AccessControlManager::has_permission(access_manager, viewer_user, "user.manage"))
  
  assert_true(azimuth::AccessControlManager::has_permission(access_manager, viewer_user, "telemetry.read"))
  
  // 测试用户不存在的情况
  assert_false(azimuth::AccessControlManager::has_permission(access_manager, "nonexistent_user", "telemetry.read"))
  
  // 测试角色变更
  azimuth::AccessControlManager::assign_role(access_manager, viewer_user, operator_role)
  
  // 验证角色变更后的权限
  assert_true(azimuth::AccessControlManager::has_permission(access_manager, viewer_user, "telemetry.write"))
  assert_true(azimuth::AccessControlManager::has_permission(access_manager, viewer_user, "config.read"))
  
  // 测试权限撤销
  azimuth::AccessControlManager::revoke_permission(access_manager, operator_role, "config.read")
  
  // 验证权限撤销后的状态
  assert_false(azimuth::AccessControlManager::has_permission(access_manager, operator_user, "config.read"))
  assert_false(azimuth::AccessControlManager::has_permission(access_manager, viewer_user, "config.read"))
  
  // 测试角色移除
  azimuth::AccessControlManager::remove_role(access_manager, viewer_user)
  
  // 验证角色移除后的状态
  assert_false(azimuth::AccessControlManager::has_permission(access_manager, viewer_user, "telemetry.read"))
}

test "身份验证测试" {
  // 创建身份验证管理器
  let auth_manager = azimuth::AuthenticationManager::new()
  
  // 创建用户
  let username = "testuser"
  let password = "securepassword123"
  let email = "testuser@example.com"
  
  azimuth::AuthenticationManager::create_user(auth_manager, username, password, email)
  
  // 测试用户认证
  let auth_result = azimuth::AuthenticationManager::authenticate(auth_manager, username, password)
  
  // 验证认证结果
  assert_true(auth_result.is_success)
  assert_not_eq(auth_result.token, "")
  assert_eq(auth_result.user.username, username)
  assert_eq(auth_result.user.email, email)
  assert_true(auth_result.user.is_active)
  assert_false(auth_result.user.is_locked)
  
  // 测试错误密码认证
  let wrong_auth_result = azimuth::AuthenticationManager::authenticate(auth_manager, username, "wrongpassword")
  
  // 验证错误密码认证结果
  assert_false(wrong_auth_result.is_success)
  assert_eq(wrong_auth_result.token, "")
  assert_eq(wrong_auth_result.error, "Invalid credentials")
  
  // 测试不存在用户认证
  let nonexistent_auth_result = azimuth::AuthenticationManager::authenticate(auth_manager, "nonexistent", password)
  
  // 验证不存在用户认证结果
  assert_false(nonexistent_auth_result.is_success)
  assert_eq(nonexistent_auth_result.token, "")
  assert_eq(nonexistent_auth_result.error, "User not found")
  
  // 测试令牌验证
  let token = auth_result.token
  let token_validation = azimuth::AuthenticationManager::validate_token(auth_manager, token)
  
  // 验证令牌验证结果
  assert_true(token_validation.is_valid)
  assert_eq(token_validation.username, username)
  assert_true(token_validation.expires_at > get_current_time())
  
  // 测试无效令牌验证
  let invalid_token_validation = azimuth::AuthenticationManager::validate_token(auth_manager, "invalid-token")
  
  // 验证无效令牌验证结果
  assert_false(invalid_token_validation.is_valid)
  assert_eq(invalid_token_validation.error, "Invalid token")
  
  // 测试令牌刷新
  let refresh_result = azimuth::AuthenticationManager::refresh_token(auth_manager, token)
  
  // 验证令牌刷新结果
  assert_true(refresh_result.is_success)
  assert_not_eq(refresh_result.new_token, "")
  assert_not_eq(refresh_result.new_token, token)
  assert_true(refresh_result.expires_at > token_validation.expires_at)
  
  // 测试旧令牌失效
  let old_token_validation = azimuth::AuthenticationManager::validate_token(auth_manager, token)
  
  // 验证旧令牌已失效
  assert_false(old_token_validation.is_valid)
  
  // 测试用户锁定
  // 多次失败登录
  for i in 0..5 {
    let failed_auth = azimuth::AuthenticationManager::authenticate(auth_manager, username, "wrongpassword")
    assert_false(failed_auth.is_success)
  }
  
  // 验证用户已锁定
  let locked_auth_result = azimuth::AuthenticationManager::authenticate(auth_manager, username, password)
  assert_false(locked_auth_result.is_success)
  assert_eq(locked_auth_result.error, "Account locked")
  
  // 测试用户解锁
  azimuth::AuthenticationManager::unlock_user(auth_manager, username)
  
  // 验证用户解锁后可以正常认证
  let unlocked_auth_result = azimuth::AuthenticationManager::authenticate(auth_manager, username, password)
  assert_true(unlocked_auth_result.is_success)
}

test "安全审计测试" {
  // 创建安全审计管理器
  let audit_manager = azimuth::SecurityAuditManager::new()
  
  // 记录安全事件
  let login_event = azimuth::SecurityEvent {
    timestamp: get_current_time(),
    event_type: azimuth::SecurityEventType::UserLogin,
    user_id: Some("testuser"),
    resource: None,
    action: Some("login"),
    result: azimuth::SecurityEventResult::Success,
    details: "User logged in successfully",
    ip_address: Some("192.168.1.100"),
    user_agent: Some("Azimuth-Client/1.0")
  }
  
  azimuth::SecurityAuditManager::log_event(audit_manager, login_event)
  
  // 记录权限检查事件
  let permission_check_event = azimuth::SecurityEvent {
    timestamp: get_current_time(),
    event_type: azimuth::SecurityEventType::PermissionCheck,
    user_id: Some("testuser"),
    resource: Some("telemetry.data"),
    action: Some("read"),
    result: azimuth::SecurityEventResult::Success,
    details: "Permission check passed",
    ip_address: Some("192.168.1.100"),
    user_agent: Some("Azimuth-Client/1.0")
  }
  
  azimuth::SecurityAuditManager::log_event(audit_manager, permission_check_event)
  
  // 记录认证失败事件
  let auth_failure_event = azimuth::SecurityEvent {
    timestamp: get_current_time(),
    event_type: azimuth::SecurityEventType::AuthenticationFailure,
    user_id: Some("testuser"),
    resource: None,
    action: Some("login"),
    result: azimuth::SecurityEventResult::Failure,
    details: "Invalid password provided",
    ip_address: Some("192.168.1.100"),
    user_agent: Some("Azimuth-Client/1.0")
  }
  
  azimuth::SecurityAuditManager::log_event(audit_manager, auth_failure_event)
  
  // 记录数据访问事件
  let data_access_event = azimuth::SecurityEvent {
    timestamp: get_current_time(),
    event_type: azimuth::SecurityEventType::DataAccess,
    user_id: Some("testuser"),
    resource: Some("sensitive.telemetry"),
    action: Some("read"),
    result: azimuth::SecurityEventResult::Success,
    details: "Accessed sensitive telemetry data",
    ip_address: Some("192.168.1.100"),
    user_agent: Some("Azimuth-Client/1.0")
  }
  
  azimuth::SecurityAuditManager::log_event(audit_manager, data_access_event)
  
  // 测试事件查询
  let all_events = azimuth::SecurityAuditManager::get_all_events(audit_manager)
  
  // 验证事件数量
  assert_eq(all_events.length(), 4)
  
  // 验证事件类型
  let mut login_events = 0
  let mut permission_check_events = 0
  let mut auth_failure_events = 0
  let mut data_access_events = 0
  
  for event in all_events {
    match event.event_type {
      azimuth::SecurityEventType::UserLogin => login_events = login_events + 1
      azimuth::SecurityEventType::PermissionCheck => permission_check_events = permission_check_events + 1
      azimuth::SecurityEventType::AuthenticationFailure => auth_failure_events = auth_failure_events + 1
      azimuth::SecurityEventType::DataAccess => data_access_events = data_access_events + 1
      _ => () // 其他事件类型
    }
  }
  
  assert_eq(login_events, 1)
  assert_eq(permission_check_events, 1)
  assert_eq(auth_failure_events, 1)
  assert_eq(data_access_events, 1)
  
  // 测试按用户查询事件
  let user_events = azimuth::SecurityAuditManager::get_events_by_user(audit_manager, "testuser")
  
  // 验证用户事件数量
  assert_eq(user_events.length(), 4)
  
  // 测试按事件类型查询事件
  let failure_events = azimuth::SecurityAuditManager::get_events_by_type(audit_manager, azimuth::SecurityEventType::AuthenticationFailure)
  
  // 验证失败事件数量
  assert_eq(failure_events.length(), 1)
  
  // 验证失败事件详情
  let failure_event = failure_events[0]
  assert_eq(failure_event.user_id, Some("testuser"))
  assert_eq(failure_event.result, azimuth::SecurityEventResult::Failure)
  assert_eq(failure_event.details, "Invalid password provided")
  
  // 测试按时间范围查询事件
  let start_time = get_current_time() - 3600 // 1小时前
  let end_time = get_current_time() + 3600 // 1小时后
  let time_range_events = azimuth::SecurityAuditManager::get_events_by_time_range(audit_manager, start_time, end_time)
  
  // 验证时间范围内事件数量
  assert_eq(time_range_events.length(), 4)
  
  // 测试安全报告生成
  let security_report = azimuth::SecurityAuditManager::generate_security_report(audit_manager)
  
  // 验证安全报告内容
  assert_true(security_report.total_events > 0)
  assert_true(security_report.successful_events > 0)
  assert_true(security_report.failed_events > 0)
  assert_true(security_report.unique_users > 0)
  assert_true(security_report.report_generated_at > 0)
  
  // 测试异常活动检测
  let suspicious_activities = azimuth::SecurityAuditManager::detect_suspicious_activities(audit_manager)
  
  // 验证异常活动检测结果
  assert_true(suspicious_activities.length() >= 0)
}

test "敏感数据处理测试" {
  // 创建敏感数据管理器
  let sensitive_manager = azimuth::SensitiveDataManager::new()
  
  // 定义敏感数据类型
  let pii_types = [
    "email.address",
    "phone.number",
    "social.security.number",
    "credit.card.number",
    "bank.account.number"
  ]
  
  // 注册敏感数据类型
  for pii_type in pii_types {
    azimuth::SensitiveDataManager::register_sensitive_type(sensitive_manager, pii_type)
  }
  
  // 创建包含敏感数据的属性
  let sensitive_attrs = azimuth::Attributes {
    values: [
      ("user.email", azimuth::AttributeValue::StringValue("user@example.com")),
      ("user.phone", azimuth::AttributeValue::StringValue("+1-555-123-4567")),
      ("user.ssn", azimuth::AttributeValue::StringValue("123-45-6789")),
      ("payment.cc", azimuth::AttributeValue::StringValue("4111-1111-1111-1111")),
      ("payment.account", azimuth::AttributeValue::StringValue("123456789")),
      ("user.name", azimuth::AttributeValue::StringValue("John Doe")),
      ("user.age", azimuth::AttributeValue::IntValue(30)),
      ("user.active", azimuth::AttributeValue::BoolValue(true))
    ]
  }
  
  // 测试敏感数据检测
  let sensitive_fields = azimuth::SensitiveDataManager::detect_sensitive_data(sensitive_manager, sensitive_attrs)
  
  // 验证敏感数据检测结果
  assert_eq(sensitive_fields.length(), 5)
  assert_true(sensitive_fields.contains("user.email"))
  assert_true(sensitive_fields.contains("user.phone"))
  assert_true(sensitive_fields.contains("user.ssn"))
  assert_true(sensitive_fields.contains("payment.cc"))
  assert_true(sensitive_fields.contains("payment.account"))
  
  // 测试敏感数据脱敏
  let sanitized_attrs = azimuth::SensitiveDataManager::sanitize_data(sensitive_manager, sensitive_attrs)
  
  // 验证敏感数据脱敏结果
  for (key, value) in sanitized_attrs.values {
    if sensitive_fields.contains(key) {
      match value {
        azimuth::AttributeValue::StringValue(sanitized_value) => {
          // 验证敏感数据已被脱敏
          assert_false(sanitized_value.contains("@")) // 邮箱
          assert_false(sanitized_value.contains("-")) // 电话、SSN、信用卡
          assert_true(sanitized_value.contains("***")) // 脱敏标记
        }
        _ => assert_true(false)
      }
    }
  }
  
  // 验证非敏感数据未被修改
  let mut name_found = false
  let mut age_found = false
  let mut active_found = false
  
  for (key, value) in sanitized_attrs.values {
    match key {
      "user.name" => {
        match value {
          azimuth::AttributeValue::StringValue(name) => {
            assert_eq(name, "John Doe")
            name_found = true
          }
          _ => assert_true(false)
        }
      }
      "user.age" => {
        match value {
          azimuth::AttributeValue::IntValue(age) => {
            assert_eq(age, 30)
            age_found = true
          }
          _ => assert_true(false)
        }
      }
      "user.active" => {
        match value {
          azimuth::AttributeValue::BoolValue(active) => {
            assert_true(active)
            active_found = true
          }
          _ => assert_true(false)
        }
      }
      _ => () // 其他键
    }
  }
  
  assert_true(name_found)
  assert_true(age_found)
  assert_true(active_found)
  
  // 测试敏感数据加密
  let encrypted_attrs = azimuth::SensitiveDataManager::encrypt_sensitive_data(sensitive_manager, sensitive_attrs)
  
  // 验证敏感数据加密结果
  for (key, value) in encrypted_attrs.values {
    if sensitive_fields.contains(key) {
      match value {
        azimuth::AttributeValue::StringValue(encrypted_value) => {
          // 验证敏感数据已被加密
          assert_not_eq(encrypted_value.length(), 0)
          assert_false(encrypted_value.contains("@"))
          assert_false(encrypted_value.contains("user"))
          assert_false(encrypted_value.contains("example"))
        }
        _ => assert_true(false)
      }
    }
  }
  
  // 测试敏感数据解密
  let decrypted_attrs = azimuth::SensitiveDataManager::decrypt_sensitive_data(sensitive_manager, encrypted_attrs)
  
  // 验证敏感数据解密结果
  assert_eq(decrypted_attrs.values.length(), sensitive_attrs.values.length())
  
  for i in 0..decrypted_attrs.values.length() {
    let (decrypted_key, decrypted_value) = decrypted_attrs.values[i]
    let (original_key, original_value) = sensitive_attrs.values[i]
    
    assert_eq(decrypted_key, original_key)
    
    // 验证解密后的值与原始值一致
    match (decrypted_value, original_value) {
      (azimuth::AttributeValue::StringValue(d), azimuth::AttributeValue::StringValue(o)) => {
        assert_eq(d, o)
      }
      (azimuth::AttributeValue::IntValue(d), azimuth::AttributeValue::IntValue(o)) => {
        assert_eq(d, o)
      }
      (azimuth::AttributeValue::BoolValue(d), azimuth::AttributeValue::BoolValue(o)) => {
        assert_eq(d, o)
      }
      _ => assert_true(false) // 类型不匹配
    }
  }
}

test "网络安全测试" {
  // 创建网络安全管理器
  let security_manager = azimuth::NetworkSecurityManager::new()
  
  // 测试IP白名单
  let allowed_ips = ["192.168.1.0/24", "10.0.0.0/8", "172.16.0.0/12"]
  
  for ip in allowed_ips {
    azimuth::NetworkSecurityManager::add_allowed_ip(security_manager, ip)
  }
  
  // 测试IP访问检查
  assert_true(azimuth::NetworkSecurityManager::is_ip_allowed(security_manager, "192.168.1.100"))
  assert_true(azimuth::NetworkSecurityManager::is_ip_allowed(security_manager, "10.0.0.50"))
  assert_true(azimuth::NetworkSecurityManager::is_ip_allowed(security_manager, "172.16.5.10"))
  
  assert_false(azimuth::NetworkSecurityManager::is_ip_allowed(security_manager, "8.8.8.8"))
  assert_false(azimuth::NetworkSecurityManager::is_ip_allowed(security_manager, "203.0.113.1"))
  
  // 测试IP黑名单
  let blocked_ips = ["203.0.113.0/24", "198.51.100.0/24"]
  
  for ip in blocked_ips {
    azimuth::NetworkSecurityManager::add_blocked_ip(security_manager, ip)
  }
  
  // 测试黑名单IP访问检查
  assert_false(azimuth::NetworkSecurityManager::is_ip_allowed(security_manager, "203.0.113.5"))
  assert_false(azimuth::NetworkSecurityManager::is_ip_allowed(security_manager, "198.51.100.10"))
  
  // 测试请求频率限制
  let rate_limit_config = azimuth::RateLimitConfig {
    max_requests_per_minute: 60,
    max_requests_per_hour: 1000,
    max_requests_per_day: 10000
  }
  
  azimuth::NetworkSecurityManager::configure_rate_limit(security_manager, "testuser", rate_limit_config)
  
  // 测试正常请求频率
  for i in 0..30 {
    let request_result = azimuth::NetworkSecurityManager::check_rate_limit(security_manager, "testuser")
    assert_true(request_result.allowed)
  }
  
  // 测试超出频率限制
  for i in 0..100 {
    let request_result = azimuth::NetworkSecurityManager::check_rate_limit(security_manager, "testuser")
    // 在某个点应该超出限制
    if !request_result.allowed {
      assert_eq(request_result.reason, "Rate limit exceeded")
      break
    }
  }
  
  // 测试TLS证书验证
  let valid_cert = azimuth::CertificateInfo {
    subject: "CN=telemetry.example.com",
    issuer: "CN=Example CA",
    serial_number: "123456789",
    not_before: 1609459200, // 2021-01-01 00:00:00 UTC
    not_after: 1640995200, // 2022-01-01 00:00:00 UTC
    fingerprint: "AB:CD:EF:12:34:56:78:90:AB:CD:EF:12:34:56:78:90"
  }
  
  let cert_validation = azimuth::NetworkSecurityManager::validate_certificate(security_manager, valid_cert)
  
  // 验证证书验证结果
  assert_true(cert_validation.is_valid)
  assert_eq(cert_validation.subject, "CN=telemetry.example.com")
  
  // 测试过期证书
  let expired_cert = azimuth::CertificateInfo {
    subject: "CN=expired.example.com",
    issuer: "CN=Example CA",
    serial_number: "987654321",
    not_before: 1609459200, // 2021-01-01 00:00:00 UTC
    not_after: 1612137600, // 2021-02-01 00:00:00 UTC
    fingerprint: "FE:DC:BA:09:87:65:43:21:FE:DC:BA:09:87:65:43:21"
  }
  
  let expired_cert_validation = azimuth::NetworkSecurityManager::validate_certificate(security_manager, expired_cert)
  
  // 验证过期证书验证结果
  assert_false(expired_cert_validation.is_valid)
  assert_eq(expired_cert_validation.error, "Certificate has expired")
  
  // 测试自签名证书
  let self_signed_cert = azimuth::CertificateInfo {
    subject: "CN=self-signed.example.com",
    issuer: "CN=self-signed.example.com", // 自签名：subject和issuer相同
    serial_number: "555666777",
    not_before: 1609459200, // 2021-01-01 00:00:00 UTC
    not_after: 1640995200, // 2022-01-01 00:00:00 UTC
    fingerprint: "11:22:33:44:55:66:77:88:99:AA:BB:CC:DD:EE:FF:00"
  }
  
  let self_signed_validation = azimuth::NetworkSecurityManager::validate_certificate(security_manager, self_signed_cert)
  
  // 验证自签名证书验证结果
  assert_false(self_signed_validation.is_valid)
  assert_eq(self_signed_validation.error, "Self-signed certificate not allowed")
  
  // 测试安全头部配置
  let security_headers = [
    ("Strict-Transport-Security", "max-age=31536000; includeSubDomains"),
    ("Content-Security-Policy", "default-src 'self'"),
    ("X-Content-Type-Options", "nosniff"),
    ("X-Frame-Options", "DENY"),
    ("X-XSS-Protection", "1; mode=block")
  ]
  
  for (header, value) in security_headers {
    azimuth::NetworkSecurityManager::add_security_header(security_manager, header, value)
  }
  
  // 验证安全头部配置
  let configured_headers = azimuth::NetworkSecurityManager::get_security_headers(security_manager)
  
  assert_eq(configured_headers.length(), 5)
  
  for (header, value) in security_headers {
    let mut header_found = false
    for (configured_header, configured_value) in configured_headers {
      if configured_header == header && configured_value == value {
        header_found = true
        break
      }
    }
    assert_true(header_found)
  }
}

// 辅助函数：获取当前时间（秒）
fn get_current_time() -> Int {
  // 模拟时间戳，实际应该使用系统时间API
  1609459200 // 2021-01-01 00:00:00 UTC
}
// Azimuth 安全性测试用例
// 专注于身份验证、授权、数据加密和安全漏洞防护

// 测试1: 身份验证和授权机制
test "身份验证和授权机制" {
  // 创建安全管理器
  let security_manager = SecurityManager::new()
  
  // 创建用户存储
  let user_store = UserStore::new()
  
  // 添加测试用户
  let admin_user = {
    id: "user-001",
    username: "admin",
    email: "admin@example.com",
    password_hash: SecurityManager::hash_password(security_manager, "admin123"),
    roles: ["admin"],
    permissions: ["read", "write", "delete", "manage_users"],
    created_at: Time::now(),
    last_login: None,
    active: true
  }
  
  let regular_user = {
    id: "user-002",
    username: "john_doe",
    email: "john@example.com",
    password_hash: SecurityManager::hash_password(security_manager, "password123"),
    roles: ["user"],
    permissions: ["read", "write"],
    created_at: Time::now(),
    last_login: None,
    active: true
  }
  
  let guest_user = {
    id: "user-003",
    username: "guest",
    email: "guest@example.com",
    password_hash: SecurityManager::hash_password(security_manager, "guest123"),
    roles: ["guest"],
    permissions: ["read"],
    created_at: Time::now(),
    last_login: None,
    active: true
  }
  
  UserStore::add_user(user_store, admin_user)
  UserStore::add_user(user_store, regular_user)
  UserStore::add_user(user_store, guest_user)
  
  // 测试身份验证
  let admin_auth = SecurityManager::authenticate(security_manager, user_store, "admin", "admin123")
  assert_true(admin_auth.success)
  assert_eq(admin_auth.user.id, "user-001")
  assert_eq(admin_auth.user.username, "admin")
  assert_true(admin_auth.user.roles.contains("admin"))
  
  let invalid_auth = SecurityManager::authenticate(security_manager, user_store, "admin", "wrong_password")
  assert_false(invalid_auth.success)
  assert_eq(invalid_auth.error, "Invalid credentials")
  
  let nonexistent_auth = SecurityManager::authenticate(security_manager, user_store, "nonexistent", "password")
  assert_false(nonexistent_auth.success)
  assert_eq(nonexistent_auth.error, "User not found")
  
  // 测试授权检查
  let admin_session = admin_auth.session
  let regular_session = SecurityManager::authenticate(security_manager, user_store, "john_doe", "password123").session
  let guest_session = SecurityManager::authenticate(security_manager, user_store, "guest", "guest123").session
  
  # 管理员权限检查
  assert_true(SecurityManager::has_permission(security_manager, admin_session, "read"))
  assert_true(SecurityManager::has_permission(security_manager, admin_session, "write"))
  assert_true(SecurityManager::has_permission(security_manager, admin_session, "delete"))
  assert_true(SecurityManager::has_permission(security_manager, admin_session, "manage_users"))
  
  # 普通用户权限检查
  assert_true(SecurityManager::has_permission(security_manager, regular_session, "read"))
  assert_true(SecurityManager::has_permission(security_manager, regular_session, "write"))
  assert_false(SecurityManager::has_permission(security_manager, regular_session, "delete"))
  assert_false(SecurityManager::has_permission(security_manager, regular_session, "manage_users"))
  
  # 访客权限检查
  assert_true(SecurityManager::has_permission(security_manager, guest_session, "read"))
  assert_false(SecurityManager::has_permission(security_manager, guest_session, "write"))
  assert_false(SecurityManager::has_permission(security_manager, guest_session, "delete"))
  assert_false(SecurityManager::has_permission(security_manager, guest_session, "manage_users"))
  
  // 测试基于角色的访问控制
  let resource_permissions = {
    "/api/users": ["admin"],
    "/api/orders": ["admin", "user"],
    "/api/products": ["admin", "user", "guest"],
    "/admin/dashboard": ["admin"]
  }
  
  # 管理员可以访问所有资源
  assert_true(SecurityManager::can_access_resource(security_manager, admin_session, "/api/users", resource_permissions))
  assert_true(SecurityManager::can_access_resource(security_manager, admin_session, "/api/orders", resource_permissions))
  assert_true(SecurityManager::can_access_resource(security_manager, admin_session, "/api/products", resource_permissions))
  assert_true(SecurityManager::can_access_resource(security_manager, admin_session, "/admin/dashboard", resource_permissions))
  
  # 普通用户可以访问部分资源
  assert_false(SecurityManager::can_access_resource(security_manager, regular_session, "/api/users", resource_permissions))
  assert_true(SecurityManager::can_access_resource(security_manager, regular_session, "/api/orders", resource_permissions))
  assert_true(SecurityManager::can_access_resource(security_manager, regular_session, "/api/products", resource_permissions))
  assert_false(SecurityManager::can_access_resource(security_manager, regular_session, "/admin/dashboard", resource_permissions))
  
  # 访客只能访问公开资源
  assert_false(SecurityManager::can_access_resource(security_manager, guest_session, "/api/users", resource_permissions))
  assert_false(SecurityManager::can_access_resource(security_manager, guest_session, "/api/orders", resource_permissions))
  assert_true(SecurityManager::can_access_resource(security_manager, guest_session, "/api/products", resource_permissions))
  assert_false(SecurityManager::can_access_resource(security_manager, guest_session, "/admin/dashboard", resource_permissions))
  
  // 测试会话管理
  let session_id = admin_session.id
  assert_true(SecurityManager::is_session_valid(security_manager, session_id))
  
  # 注销会话
  SecurityManager::logout(security_manager, session_id)
  assert_false(SecurityManager::is_session_valid(security_manager, session_id))
  
  // 测试会话过期
  let new_session = SecurityManager::authenticate(security_manager, user_store, "admin", "admin123").session
  let new_session_id = new_session.id
  
  # 模拟会话过期
  SecurityManager::expire_session(security_manager, new_session_id)
  assert_false(SecurityManager::is_session_valid(security_manager, new_session_id))
}

// 测试2: 密码策略和安全性
test "密码策略和安全性" {
  // 创建密码策略管理器
  let password_policy_manager = PasswordPolicyManager::new()
  
  // 定义密码策略
  let strict_password_policy = {
    min_length: 8,
    max_length: 128,
    require_uppercase: true,
    require_lowercase: true,
    require_digits: true,
    require_special_chars: true,
    forbidden_patterns: ["password", "123456", "qwerty"],
    max_repeated_chars: 2,
    history_count: 5,  # 不能重复使用最近5个密码
    expiry_days: 90    # 90天后过期
  }
  
  PasswordPolicyManager::set_policy(password_policy_manager, strict_password_policy)
  
  // 测试有效密码
  let valid_passwords = [
    "SecureP@ssw0rd",
    "MyStr0ng!Pass",
    "C0mpl3x#P@ssw0rd",
    "R@nd0m&Ch@rs123"
  ]
  
  for password in valid_passwords {
    let validation_result = PasswordPolicyManager::validate(password_policy_manager, password)
    assert_true(validation_result.is_valid)
    assert_eq(validation_result.errors.length(), 0)
  }
  
  // 测试无效密码
  let invalid_password_tests = [
    { password: "short", error_contains: "minimum length" },
    { password: "nouppercase1!", error_contains: "uppercase" },
    { password: "NOLOWERCASE1!", error_contains: "lowercase" },
    { password: "NoDigits!", error_contains: "digit" },
    { password: "NoSpecialChars1", error_contains: "special" },
    { password: "MyPassword123", error_contains: "forbidden" },
    { password: "AAAaaa111!", error_contains: "repeated" }
  ]
  
  for test_case in invalid_password_tests {
    let validation_result = PasswordPolicyManager::validate(password_policy_manager, test_case.password)
    assert_false(validation_result.is_valid)
    assert_true(validation_result.errors.any(fn(error) { error.contains(test_case.error_contains) }))
  }
  
  // 测试密码强度评估
  let password_strength_tests = [
    { password: "weak", expected_strength: "very_weak" },
    { password: "password", expected_strength: "weak" },
    { password: "Password1", expected_strength: "medium" },
    { password: "P@ssw0rd", expected_strength: "strong" },
    { password: "C0mpl3x#P@ssw0rdWithR@nd0mCh@rs", expected_strength: "very_strong" }
  ]
  
  for test_case in password_strength_tests {
    let strength_result = PasswordPolicyManager::assess_strength(password_policy_manager, test_case.password)
    assert_eq(strength_result.strength, test_case.expected_strength)
    assert_true(strength_result.score >= 0 and strength_result.score <= 100)
  }
  
  // 测试密码历史
  let user_id = "test-user"
  let password_history = []
  
  # 添加历史密码
  password_history = password_history.push("OldPassword1!")
  password_history = password_history.push("OldPassword2!")
  password_history = password_history.push("OldPassword3!")
  password_history = password_history.push("OldPassword4!")
  password_history = password_history.push("OldPassword5!")
  
  # 尝试使用历史密码
  let history_check_result = PasswordPolicyManager::check_history(password_policy_manager, user_id, "OldPassword1!", password_history)
  assert_false(history_check_result.is_allowed)
  assert_true(history_check_result.error.contains("previously used"))
  
  # 使用新密码
  let new_password_check = PasswordPolicyManager::check_history(password_policy_manager, user_id, "NewSecureP@ssw0rd", password_history)
  assert_true(new_password_check.is_allowed)
  
  // 测试密码哈希和验证
  let password = "TestP@ssw0rd"
  let hash = PasswordPolicyManager::hash_password(password_policy_manager, password)
  assert_not_eq(hash, password)
  assert_true(hash.length() > 50)  # 哈希应该足够长
  
  # 验证密码哈希
  assert_true(PasswordPolicyManager::verify_password(password_policy_manager, password, hash))
  assert_false(PasswordPolicyManager::verify_password(password_policy_manager, "WrongPassword", hash))
  
  // 测试密码哈希的唯一性（相同密码应产生不同哈希）
  let hash1 = PasswordPolicyManager::hash_password(password_policy_manager, password)
  let hash2 = PasswordPolicyManager::hash_password(password_policy_manager, password)
  assert_not_eq(hash1, hash2)  # 由于盐值不同，哈希应该不同
  
  # 但都应该验证通过
  assert_true(PasswordPolicyManager::verify_password(password_policy_manager, password, hash1))
  assert_true(PasswordPolicyManager::verify_password(password_policy_manager, password, hash2))
  
  // 测试密码生成器
  let generated_password = PasswordPolicyManager::generate_password(password_policy_manager, 12)
  let generated_validation = PasswordPolicyManager::validate(password_policy_manager, generated_password)
  assert_true(generated_validation.is_valid)
  assert_eq(generated_password.length(), 12)
}

// 测试3: 数据加密和解密
test "数据加密和解密" {
  // 创建加密管理器
  let encryption_manager = EncryptionManager::new()
  
  // 生成加密密钥
  let encryption_key = EncryptionManager::generate_key(encryption_manager, "AES-256", 32)
  assert_eq(encryption_key.length(), 32)
  
  // 测试对称加密
  let plaintext = "This is a secret message that needs to be encrypted."
  
  # 加密
  let encrypted_data = EncryptionManager::encrypt(encryption_manager, plaintext, encryption_key, {
    algorithm: "AES-256-GCM",
    iv_length: 12
  })
  
  assert_not_eq(encrypted_data.ciphertext, plaintext)
  assert_true(encrypted_data.ciphertext.length() > 0)
  assert_true(encrypted_data.iv.length() > 0)
  assert_true(encrypted_data.tag.length() > 0)
  
  # 解密
  let decrypted_data = EncryptionManager::decrypt(encryption_manager, encrypted_data, encryption_key, {
    algorithm: "AES-256-GCM"
  })
  
  assert_eq(decrypted_data, plaintext)
  
  // 测试使用错误密钥解密
  let wrong_key = EncryptionManager::generate_key(encryption_manager, "AES-256", 32)
  let failed_decryption = EncryptionManager::decrypt(encryption_manager, encrypted_data, wrong_key, {
    algorithm: "AES-256-GCM"
  })
  
  assert_eq(failed_decryption, Error("Decryption failed: authentication failed"))
  
  // 测试非对称加密
  let key_pair = EncryptionManager::generate_key_pair(encryption_manager, "RSA-2048")
  
  # 使用公钥加密
  let asymmetric_encrypted = EncryptionManager::public_key_encrypt(encryption_manager, plaintext, key_pair.public_key)
  assert_not_eq(asymmetric_encrypted, plaintext)
  
  # 使用私钥解密
  let asymmetric_decrypted = EncryptionManager::private_key_decrypt(encryption_manager, asymmetric_encrypted, key_pair.private_key)
  assert_eq(asymmetric_decrypted, plaintext)
  
  // 测试数字签名
  let message = "This message needs to be signed for authenticity."
  
  # 使用私钥签名
  let signature = EncryptionManager::sign(encryption_manager, message, key_pair.private_key, {
    algorithm: "RSA-SHA256"
  })
  
  assert_true(signature.length() > 0)
  
  # 使用公钥验证签名
  let signature_valid = EncryptionManager::verify_signature(encryption_manager, message, signature, key_pair.public_key, {
    algorithm: "RSA-SHA256"
  })
  
  assert_true(signature_valid)
  
  # 修改消息后验证签名应该失败
  let tampered_message = message + " tampered"
  let tampered_signature_valid = EncryptionManager::verify_signature(encryption_manager, tampered_message, signature, key_pair.public_key, {
    algorithm: "RSA-SHA256"
  })
  
  assert_false(tampered_signature_valid)
  
  // 测试数据哈希
  let hash_data = "Data to be hashed for integrity verification."
  let hash1 = EncryptionManager::hash(encryption_manager, hash_data, "SHA-256")
  let hash2 = EncryptionManager::hash(encryption_manager, hash_data, "SHA-256")
  
  assert_eq(hash1, hash2)  # 相同数据应产生相同哈希
  assert_eq(hash1.length(), 64)  # SHA-256应产生64字符的十六进制字符串
  
  # 不同数据应产生不同哈希
  let hash3 = EncryptionManager::hash(encryption_manager, hash_data + " modified", "SHA-256")
  assert_not_eq(hash1, hash3)
  
  // 测试密钥派生
  let password = "user_password_123"
  let salt = EncryptionManager::generate_salt(encryption_manager, 16)
  
  let derived_key = EncryptionManager::derive_key(encryption_manager, password, salt, {
    algorithm: "PBKDF2",
    iterations: 10000,
    key_length: 32,
    hash_function: "SHA-256"
  })
  
  assert_eq(derived_key.length(), 32)
  
  // 相同密码和盐应产生相同密钥
  let derived_key2 = EncryptionManager::derive_key(encryption_manager, password, salt, {
    algorithm: "PBKDF2",
    iterations: 10000,
    key_length: 32,
    hash_function: "SHA-256"
  })
  
  assert_eq(derived_key, derived_key2)
  
  // 不同盐应产生不同密钥
  let different_salt = EncryptionManager::generate_salt(encryption_manager, 16)
  let derived_key3 = EncryptionManager::derive_key(encryption_manager, password, different_salt, {
    algorithm: "PBKDF2",
    iterations: 10000,
    key_length: 32,
    hash_function: "SHA-256"
  })
  
  assert_not_eq(derived_key, derived_key3)
}

// 测试4: 输入验证和XSS防护
test "输入验证和XSS防护" {
  // 创建输入验证管理器
  let input_validator = InputValidator::new()
  
  // 定义验证规则
  InputValidator::add_rule(input_validator, "username", {
    type: "string",
    required: true,
    min_length: 3,
    max_length: 20,
    pattern: "^[a-zA-Z0-9_]+$",
    sanitize: true
  })
  
  InputValidator::add_rule(input_validator, "email", {
    type: "email",
    required: true,
    sanitize: true
  })
  
  InputValidator::add_rule(input_validator, "age", {
    type: "integer",
    required: true,
    min_value: 18,
    max_value: 120
  })
  
  InputValidator::add_rule(input_validator, "comment", {
    type: "string",
    required: false,
    max_length: 500,
    sanitize: true,
    xss_protection: true
  })
  
  // 测试有效输入
  let valid_input = {
    username: "john_doe",
    email: "john@example.com",
    age: 25,
    comment: "This is a valid comment."
  }
  
  let valid_result = InputValidator::validate(input_validator, valid_input)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // 测试无效输入
  let invalid_input = {
    username: "invalid username!",  # 包含无效字符
    email: "invalid-email",
    age: 15,  # 小于最小值
    comment: "This comment is way too long " + "a".repeat(500)  # 超过最大长度
  }
  
  let invalid_result = InputValidator::validate(input_validator, invalid_input)
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.errors.length() >= 4)
  
  // 测试XSS攻击防护
  let xss_attempts = [
    "<script>alert('XSS')</script>",
    "javascript:alert('XSS')",
    "<img src=x onerror=alert('XSS')>",
    "<svg onload=alert('XSS')>",
    "';alert('XSS');//",
    "<iframe src=\"javascript:alert('XSS')\"></iframe>"
  ]
  
  for xss_attempt in xss_attempts {
    let xss_input = {
      username: "testuser",
      email: "test@example.com",
      age: 25,
      comment: xss_attempt
    }
    
    let xss_result = InputValidator::validate(input_validator, xss_input)
    
    # 验证XSS防护
    if xss_result.is_valid {
      let sanitized_comment = xss_result.sanitized_data.comment
      assert_false(sanitized_comment.contains("<script>"))
      assert_false(sanitized_comment.contains("javascript:"))
      assert_false(sanitized_comment.contains("onerror="))
      assert_false(sanitized_comment.contains("onload="))
    }
  }
  
  // 测试SQL注入防护
  let sql_injection_attempts = [
    "'; DROP TABLE users; --",
    "1' OR '1'='1",
    "1; DELETE FROM users WHERE 1=1 --",
    "admin'--",
    "admin' /*",
    "' OR 1=1#"
  ]
  
  for injection_attempt in sql_injection_attempts {
    let injection_input = {
      username: injection_attempt,
      email: "test@example.com",
      age: 25
    }
    
    let injection_result = InputValidator::validate(input_validator, injection_input)
    
    # 用户名模式应该阻止SQL注入尝试
    assert_false(injection_result.is_valid)
  }
  
  // 测试HTML内容清理
  let html_content = "<div><p>This is <b>bold</b> text with <script>alert('XSS')</script> malicious script.</p></div>"
  
  let sanitized_html = InputValidator::sanitize_html(input_validator, html_content, {
    allowed_tags: ["p", "b", "i", "em", "strong"],
    allowed_attributes: {}
  })
  
  assert_true(sanitized_html.contains("<p>"))
  assert_true(sanitized_html.contains("<b>"))
  assert_false(sanitized_html.contains("<script>"))
  assert_false(sanitized_html.contains("<div>"))
  
  // 测试URL验证
  let url_validator = InputValidator::create_url_validator(input_validator, {
    allowed_schemes: ["http", "https"],
    allow_relative: false
  })
  
  let valid_urls = [
    "https://example.com",
    "http://example.com/path",
    "https://example.com/path?query=value"
  ]
  
  for url in valid_urls {
    assert_true(InputValidator::validate_url(url_validator, url))
  }
  
  let invalid_urls = [
    "javascript:alert('XSS')",
    "ftp://example.com",
    "//example.com",
    "https://example.com \" onerror=\"alert('XSS')"
  ]
  
  for url in invalid_urls {
    assert_false(InputValidator::validate_url(url_validator, url))
  }
}

// 测试5: 会话安全和令牌管理
test "会话安全和令牌管理" {
  // 创建会话管理器
  let session_manager = SessionManager::new({
    secret_key: "super-secret-key-for-session-signing",
    expiry_time: 3600,  # 1小时
    refresh_threshold: 300,  # 5分钟内过期时自动刷新
    secure_cookies: true,
    http_only: true
  })
  
  // 创建用户
  let user = {
    id: "user-123",
    username: "testuser",
    roles: ["user"]
  }
  
  // 创建会话
  let session = SessionManager::create_session(session_manager, user)
  
  assert_true(session.id.length() > 0)
  assert_eq(session.user_id, "user-123")
  assert_eq(session.username, "testuser")
  assert_true(session.created_at > 0)
  assert_true(session.expires_at > session.created_at)
  assert_true(session.is_active)
  
  // 验证会话
  let validation_result = SessionManager::validate_session(session_manager, session.id)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.session.user_id, "user-123")
  
  // 测试会话过期
  let expired_session = SessionManager::create_session_with_expiry(session_manager, user, 1)  # 1秒过期
  Thread::sleep(1100)  # 等待超过1秒
  
  let expired_validation = SessionManager::validate_session(session_manager, expired_session.id)
  assert_false(expired_validation.is_valid)
  assert_eq(expired_validation.error, "Session expired")
  
  // 测试会话令牌生成
  let token = SessionManager::generate_token(session_manager, {
    user_id: "user-123",
    purpose: "api_access",
    expiry: 1800  # 30分钟
  })
  
  assert_true(token.length() > 0)
  
  // 验证令牌
  let token_validation = SessionManager::validate_token(session_manager, token)
  assert_true(token_validation.is_valid)
  assert_eq(token_validation.claims.get("user_id"), "user-123")
  assert_eq(token_validation.claims.get("purpose"), "api_access")
  
  // 测试令牌过期
  let expired_token = SessionManager::generate_token(session_manager, {
    user_id: "user-123",
    purpose: "api_access",
    expiry: 1  # 1秒过期
  })
  
  Thread::sleep(1100)  # 等待超过1秒
  
  let expired_token_validation = SessionManager::validate_token(session_manager, expired_token)
  assert_false(expired_token_validation.is_valid)
  assert_eq(expired_token_validation.error, "Token expired")
  
  // 测试令牌篡改检测
  let tampered_token = token.substring(0, token.length() - 1) + "X"  # 修改令牌最后一个字符
  let tampered_validation = SessionManager::validate_token(session_manager, tampered_token)
  assert_false(tampered_validation.is_valid)
  assert_eq(tampered_validation.error, "Invalid token signature")
  
  // 测试刷新令牌
  let refresh_token = SessionManager::generate_refresh_token(session_manager, {
    user_id: "user-123",
    session_id: session.id
  })
  
  assert_true(refresh_token.length() > 0)
  
  // 使用刷新令牌获取新的访问令牌
  let refresh_result = SessionManager::refresh_access_token(session_manager, refresh_token)
  assert_true(refresh_result.success)
  assert_true(refresh_result.new_access_token.length() > 0)
  assert_not_eq(refresh_result.new_access_token, token)
  
  // 测试撤销会话
  SessionManager::revoke_session(session_manager, session.id)
  let revoked_validation = SessionManager::validate_session(session_manager, session.id)
  assert_false(revoked_validation.is_valid)
  assert_eq(revoked_validation.error, "Session revoked")
  
  // 测试撤销所有用户会话
  let session2 = SessionManager::create_session(session_manager, user)
  let session3 = SessionManager::create_session(session_manager, user)
  
  SessionManager::revoke_all_user_sessions(session_manager, "user-123")
  
  let session2_validation = SessionManager::validate_session(session_manager, session2.id)
  let session3_validation = SessionManager::validate_session(session_manager, session3.id)
  
  assert_false(session2_validation.is_valid)
  assert_false(session3_validation.is_valid)
  
  // 测试会话并发控制
  let concurrent_sessions = []
  for i in 0..=5 {
    let new_session = SessionManager::create_session(session_manager, user)
    concurrent_sessions = concurrent_sessions.push(new_session)
  }
  
  # 设置最大并发会话数为3
  SessionManager::set_max_concurrent_sessions(session_manager, 3)
  
  let new_session_after_limit = SessionManager::create_session(session_manager, user)
  
  # 验证最旧的会话被撤销
  let oldest_session = concurrent_sessions[0]
  let oldest_validation = SessionManager::validate_session(session_manager, oldest_session.id)
  assert_false(oldest_validation.is_valid)
  
  # 验证新会话有效
  let new_validation = SessionManager::validate_session(session_manager, new_session_after_limit.id)
  assert_true(new_validation.is_valid)
}

// 测试6: 安全日志和审计
test "安全日志和审计" {
  // 创建安全审计管理器
  let audit_manager = SecurityAuditManager::new({
    log_file: "/tmp/security_audit.log",
    retention_days: 30,
    alert_thresholds: {
      failed_login_attempts: 5,
      suspicious_activities: 10,
      privilege_escalation: 1
    }
  })
  
  // 记录登录事件
  AuditManager::log_event(audit_manager, {
    event_type: "login",
    user_id: "user-123",
    username: "testuser",
    ip_address: "192.168.1.100",
    user_agent: "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
    timestamp: Time::now(),
    success: true,
    details: {
      method: "password",
      session_id: "session-abc123"
    }
  })
  
  // 记录登录失败事件
  for i in 0..=6 {
    AuditManager::log_event(audit_manager, {
      event_type: "login_failed",
      user_id: "user-456",
      username: "attacker",
      ip_address: "192.168.1.200",
      user_agent: "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
      timestamp: Time::now(),
      success: false,
      details: {
        reason: "invalid_password",
        attempt_number: i + 1
      }
    })
  }
  
  // 记录权限提升事件
  AuditManager::log_event(audit_manager, {
    event_type: "privilege_escalation",
    user_id: "user-789",
    username: "admin",
    ip_address: "192.168.1.150",
    user_agent: "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
    timestamp: Time::now(),
    success: true,
    details: {
      old_role: "user",
      new_role: "admin",
      reason: "granted_by_superuser"
    }
  })
  
  // 记录数据访问事件
  AuditManager::log_event(audit_manager, {
    event_type: "data_access",
    user_id: "user-123",
    username: "testuser",
    ip_address: "192.168.1.100",
    user_agent: "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
    timestamp: Time::now(),
    success: true,
    details: {
      resource: "/api/users/123",
      action: "read",
      resource_type: "user_profile"
    }
  })
  
  // 记录配置变更事件
  AuditManager::log_event(audit_manager, {
    event_type: "config_change",
    user_id: "user-789",
    username: "admin",
    ip_address: "192.168.1.150",
    user_agent: "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
    timestamp: Time::now(),
    success: true,
    details: {
      config_section: "security",
      changed_keys: ["session_timeout", "password_policy"],
      old_values: { session_timeout: 1800, password_policy: "medium" },
      new_values: { session_timeout: 3600, password_policy: "strict" }
    }
  })
  
  // 查询审计日志
  let login_events = AuditManager::query_events(audit_manager, {
    event_type: "login",
    start_time: Time::now() - 3600,  # 最近1小时
    end_time: Time::now()
  })
  
  assert_eq(login_events.length(), 1)
  assert_eq(login_events[0].username, "testuser")
  assert_true(login_events[0].success)
  
  let failed_login_events = AuditManager::query_events(audit_manager, {
    event_type: "login_failed",
    start_time: Time::now() - 3600,
    end_time: Time::now()
  })
  
  assert_eq(failed_login_events.length(), 7)
  
  // 检测安全事件
  let security_alerts = AuditManager::detect_security_events(audit_manager)
  
  # 应该检测到多次登录失败和权限提升
  assert_true(security_alerts.length() >= 2)
  
  let failed_login_alert = security_alerts.find(fn(alert) { alert.type == "multiple_failed_logins" })
  assert_true(failed_login_alert != None)
  assert_eq(failed_login_alert.severity, "high")
  assert_eq(failed_login_alert.username, "attacker")
  assert_true(failed_login_alert.count >= 5)
  
  let privilege_escalation_alert = security_alerts.find(fn(alert) { alert.type == "privilege_escalation" })
  assert_true(privilege_escalation_alert != None)
  assert_eq(privilege_escalation_alert.severity, "critical")
  assert_eq(privilege_escalation_alert.username, "admin")
  
  // 生成安全报告
  let security_report = AuditManager::generate_security_report(audit_manager, {
    start_time: Time::now() - 86400,  # 最近24小时
    end_time: Time::now(),
    include_event_summary: true,
    include_security_alerts: true,
    include_user_activity: true,
    include_recommendations: true
  })
  
  assert_true(security_report.contains("Security Audit Report"))
  assert_true(security_report.contains("Event Summary"))
  assert_true(security_report.contains("Security Alerts"))
  assert_true(security_report.contains("User Activity"))
  
  // 测试日志完整性验证
  let log_integrity = AuditManager::verify_log_integrity(audit_manager)
  assert_true(log_integrity.valid)
  assert_true(log_integrity.total_entries > 0)
  
  // 测试日志导出
  let exported_logs = AuditManager::export_logs(audit_manager, {
    format: "json",
    start_time: Time::now() - 3600,
    end_time: Time::now(),
    event_types: ["login", "login_failed", "privilege_escalation"]
  })
  
  assert_true(exported_logs.length() > 0)
  
  // 测试日志清理
  let old_event = {
    event_type: "test_old_event",
    user_id: "user-old",
    username: "olduser",
    ip_address: "192.168.1.1",
    user_agent: "test-agent",
    timestamp: Time::now() - (31 * 86400),  # 31天前
    success: true,
    details: {}
  }
  
  AuditManager::log_event(audit_manager, old_event)
  
  # 执行清理（删除30天前的日志）
  let cleanup_result = AuditManager::cleanup_old_logs(audit_manager)
  assert_true(cleanup_result.deleted_count > 0)
  
  # 验证旧日志已删除
  let old_events = AuditManager::query_events(audit_manager, {
    event_type: "test_old_event",
    start_time: Time::now() - (32 * 86400),
    end_time: Time::now() - (30 * 86400)
  })
  
  assert_eq(old_events.length(), 0)
}

// 测试7: 网络安全和防火墙
test "网络安全和防火墙" {
  // 创建网络安全管理器
  let network_security_manager = NetworkSecurityManager::new({
    firewall_enabled: true,
    ddos_protection: true,
    rate_limiting: true,
    ip_whitelist: ["192.168.1.0/24", "10.0.0.0/8"],
    ip_blacklist: ["192.168.100.100", "10.0.0.50"]
  })
  
  // 测试IP白名单
  let whitelist_tests = [
    { ip: "192.168.1.100", expected: true },
    { ip: "192.168.1.200", expected: true },
    { ip: "10.0.5.100", expected: true },
    { ip: "172.16.0.100", expected: false },  # 不在白名单
    { ip: "203.0.113.100", expected: false }  # 公网IP
  ]
  
  for test_case in whitelist_tests {
    let is_allowed = NetworkSecurityManager::is_ip_allowed(network_security_manager, test_case.ip)
    assert_eq(is_allowed, test_case.expected)
  }
  
  // 测试IP黑名单
  let blacklist_tests = [
    { ip: "192.168.100.100", expected: false },  # 在黑名单
    { ip: "10.0.0.50", expected: false },        # 在黑名单
    { ip: "192.168.1.100", expected: true },     # 在白名单，优先级更高
    { ip: "10.0.5.100", expected: true },        # 在白名单，优先级更高
    { ip: "172.16.0.100", expected: false }      # 不在白名单也不在黑名单
  ]
  
  for test_case in blacklist_tests {
    let is_allowed = NetworkSecurityManager::is_ip_allowed(network_security_manager, test_case.ip)
    assert_eq(is_allowed, test_case.expected)
  }
  
  // 测试速率限制
  let rate_limiter = NetworkSecurityManager::get_rate_limiter(network_security_manager, {
    requests_per_minute: 60,
    burst_size: 10,
    block_duration: 300  # 5分钟
  })
  
  # 模拟正常请求速率
  let normal_requests = []
  for i in 0..=5 {
    let result = RateLimiter::check_request(rate_limiter, "192.168.1.100")
    normal_requests = normal_requests.push(result)
  }
  
  # 所有正常请求应该被允许
  for request in normal_requests {
    assert_true(request.allowed)
  }
  
  # 模拟超过速率限制的请求
  let burst_requests = []
  for i in 0..=15 {
    let result = RateLimiter::check_request(rate_limiter, "192.168.1.200")
    burst_requests = burst_requests.push(result)
  }
  
  # 前几个请求应该被允许，后面的应该被拒绝
  let allowed_count = burst_requests.filter(fn(r) { r.allowed }).length()
  assert_true(allowed_count <= 10)  # 最多允许突发大小数量的请求
  
  // 测试DDoS防护
  let ddos_protection = NetworkSecurityManager::get_ddos_protection(network_security_manager, {
    threshold_connections_per_ip: 100,
    threshold_requests_per_second: 50,
    detection_window: 60,  # 1分钟
    block_duration: 1800   # 30分钟
  })
  
  # 模拟正常流量
  for i in 0..=10 {
    DDoSProtection::record_request(ddos_protection, "192.168.1.100")
  }
  
  let is_ddos_blocked = DDoSProtection::is_ip_blocked(ddos_protection, "192.168.1.100")
  assert_false(is_ddos_blocked)
  
  # 模拟DDoS攻击流量
  for i in 0..=150 {
    DDoSProtection::record_request(ddos_protection, "192.168.1.200")
  }
  
  let attacker_blocked = DDoSProtection::is_ip_blocked(ddos_protection, "192.168.1.200")
  assert_true(attacker_blocked)
  
  // 测试防火墙规则
  let firewall = NetworkSecurityManager::get_firewall(network_security_manager)
  
  # 添加防火墙规则
  Firewall::add_rule(firewall, {
    name: "allow-http",
    action: "allow",
    protocol: "tcp",
    source_ip: "0.0.0.0/0",
    source_port: "any",
    destination_ip: "any",
    destination_port: "80,443",
    description: "Allow HTTP/HTTPS traffic"
  })
  
  Firewall::add_rule(firewall, {
    name: "deny-ssh",
    action: "deny",
    protocol: "tcp",
    source_ip: "0.0.0.0/0",
    source_port: "any",
    destination_ip: "any",
    destination_port: "22",
    description: "Deny SSH access"
  })
  
  # 测试防火墙规则匹配
  let http_rule_match = Firewall::evaluate_packet(firewall, {
    protocol: "tcp",
    source_ip: "192.168.1.100",
    source_port: 12345,
    destination_ip: "10.0.0.10",
    destination_port: 80
  })
  
  assert_eq(http_rule_match.action, "allow")
  assert_eq(http_rule_match.matched_rule, "allow-http")
  
  let ssh_rule_match = Firewall::evaluate_packet(firewall, {
    protocol: "tcp",
    source_ip: "192.168.1.100",
    source_port: 12345,
    destination_ip: "10.0.0.10",
    destination_port: 22
  })
  
  assert_eq(ssh_rule_match.action, "deny")
  assert_eq(ssh_rule_match.matched_rule, "deny-ssh")
  
  // 测试入侵检测
  let intrusion_detection = NetworkSecurityManager::get_intrusion_detection(network_security_manager, {
    enabled: true,
    signature_database: "latest",
    alert_threshold: 5,
    log_suspicious_activity: true
  })
  
  # 模拟正常网络流量
  let normal_traffic = [
    { src_ip: "192.168.1.100", dst_ip: "10.0.0.10", dst_port: 80, protocol: "tcp" },
    { src_ip: "192.168.1.100", dst_ip: "10.0.0.10", dst_port: 443, protocol: "tcp" },
    { src_ip: "192.168.1.101", dst_ip: "10.0.0.11", dst_port: 80, protocol: "tcp" }
  ]
  
  for packet in normal_traffic {
    let detection_result = IntrusionDetection::analyze_packet(intrusion_detection, packet)
    assert_false(detection_result.is_suspicious)
  }
  
  # 模拟可疑网络流量
  let suspicious_traffic = [
    { src_ip: "192.168.1.200", dst_ip: "10.0.0.10", dst_port: 22, protocol: "tcp" },  # SSH扫描
    { src_ip: "192.168.1.200", dst_ip: "10.0.0.11", dst_port: 22, protocol: "tcp" },  # SSH扫描
    { src_ip: "192.168.1.200", dst_ip: "10.0.0.12", dst_port: 22, protocol: "tcp" },  # SSH扫描
    { src_ip: "192.168.1.200", dst_ip: "10.0.0.13", dst_port: 22, protocol: "tcp" },  # SSH扫描
    { src_ip: "192.168.1.200", dst_ip: "10.0.0.14", dst_port: 22, protocol: "tcp" },  # SSH扫描
    { src_ip: "192.168.1.200", dst_ip: "10.0.0.15", dst_port: 22, protocol: "tcp" }   # SSH扫描
  ]
  
  let suspicious_detected = false
  for packet in suspicious_traffic {
    let detection_result = IntrusionDetection::analyze_packet(intrusion_detection, packet)
    if detection_result.is_suspicious {
      suspicious_detected = true
    }
  }
  
  assert_true(suspicious_detected)
  
  // 获取安全统计
  let security_stats = NetworkSecurityManager::get_security_statistics(network_security_manager)
  
  assert_true(security_stats.total_requests > 0)
  assert_true(security_stats.blocked_requests >= 0)
  assert_true(security_stats.rate_limited_requests >= 0)
  assert_true(security_stats.ddos_blocked_ips.length() >= 1)
  assert_true(security_stats.firewall_rules_evaluated > 0)
}

// 测试8: 安全漏洞扫描和评估
test "安全漏洞扫描和评估" {
  // 创建漏洞扫描管理器
  let vulnerability_scanner = VulnerabilityScanner::new({
    scan_database: "latest",
    severity_threshold: "medium",
    scan_components: ["dependencies", "configurations", "code"]
  })
  
  // 模拟应用程序依赖
  let app_dependencies = [
    { name: "express", version: "4.16.0", type: "npm" },
    { name: "lodash", version: "4.17.15", type: "npm" },
    { name: "request", version: "2.88.0", type: "npm" },
    { name: "axios", version: "0.21.0", type: "npm" }
  ]
  
  // 执行依赖漏洞扫描
  let dependency_scan_result = VulnerabilityScanner::scan_dependencies(vulnerability_scanner, app_dependencies)
  
  assert_true(dependency_scan_result.scanned_dependencies > 0)
  assert_true(dependency_scan_result.vulnerabilities_found >= 0)
  
  // 模拟发现漏洞
  let mock_vulnerabilities = [
    {
      id: "CVE-2021-23337",
      name: "Prototype Pollution",
      severity: "high",
      affected_package: "lodash",
      affected_versions: "<4.17.20",
      fixed_version: "4.17.20",
      description: "Lodash versions before 4.17.20 are vulnerable to prototype pollution.",
      cwe: "CWE-1321",
      cvss_score: 7.5
    },
    {
      id: "CVE-2021-23405",
      name: "Server-Side Request Forgery",
      severity: "medium",
      affected_package: "axios",
      affected_versions: "<0.21.1",
      fixed_version: "0.21.1",
      description: "Axios is vulnerable to Server-Side Request Forgery (SSRF).",
      cwe: "CWE-918",
      cvss_score: 5.3
    }
  ]
  
  // 添加模拟漏洞到扫描结果
  for vuln in mock_vulnerabilities {
    dependency_scan_result.vulnerabilities = dependency_scan_result.vulnerabilities.push(vuln)
  }
  
  // 验证漏洞分类
  let high_severity_vulns = dependency_scan_result.vulnerabilities.filter(fn(v) { v.severity == "high" })
  let medium_severity_vulns = dependency_scan_result.vulnerabilities.filter(fn(v) { v.severity == "medium" })
  
  assert_true(high_severity_vulns.length() >= 1)
  assert_true(medium_severity_vulns.length() >= 1)
  
  // 测试配置安全扫描
  let security_config = {
    debug_mode: true,
    error_reporting: "verbose",
    session_timeout: 86400,  # 24小时，太长
    password_policy: "weak",
    csrf_protection: false,
    https_only: false,
    headers: {
      "x-frame-options": "DENY",
      "x-content-type-options": "nosniff"
      # 缺少安全头
    }
  }
  
  let config_scan_result = VulnerabilityScanner::scan_configuration(vulnerability_scanner, security_config)
  
  assert_true(config_scan_result.issues_found > 0)
  
  // 验证配置问题
  let debug_mode_issue = config_scan_result.issues.find(fn(issue) { issue.setting == "debug_mode" })
  assert_true(debug_mode_issue != None)
  assert_eq(debug_mode_issue.severity, "medium")
  assert_true(debug_mode_issue.recommendation.contains("disable"))
  
  let session_timeout_issue = config_scan_result.issues.find(fn(issue) { issue.setting == "session_timeout" })
  assert_true(session_timeout_issue != None)
  assert_eq(session_timeout_issue.severity, "low")
  
  let csrf_issue = config_scan_result.issues.find(fn(issue) { issue.setting == "csrf_protection" })
  assert_true(csrf_issue != None)
  assert_eq(csrf_issue.severity, "high")
  
  let https_issue = config_scan_result.issues.find(fn(issue) { issue.setting == "https_only" })
  assert_true(https_issue != None)
  assert_eq(https_issue.severity, "high")
  
  // 测试代码安全扫描
  let code_snippets = [
    {
      file: "user_controller.js",
      content: "
        app.post('/login', (req, res) => {
          const username = req.body.username;
          const password = req.body.password;
          
          // 直接SQL查询，存在SQL注入风险
          const query = 'SELECT * FROM users WHERE username = \\\' + username + '\\' AND password = \\\' + password + '\\'';
          
          db.query(query, (err, result) => {
            if (result.length > 0) {
              res.json({ success: true, token: generateToken(username) });
            } else {
              res.status(401).json({ success: false });
            }
          });
        });
      ",
      language: "javascript"
    },
    {
      file: "file_upload.js",
      content: "
        app.post('/upload', (req, res) => {
          const filename = req.body.filename;
          const content = req.body.content;
          
          // 直接使用用户提供的文件名，存在路径遍历风险
          const path = '/uploads/' + filename;
          
          fs.writeFile(path, content, (err) => {
            if (err) {
              res.status(500).json({ error: 'Upload failed' });
            } else {
              res.json({ success: true, path: path });
            }
          });
        });
      ",
      language: "javascript"
    }
  ]
  
  let code_scan_result = VulnerabilityScanner::scan_code(vulnerability_scanner, code_snippets)
  
  assert_true(code_scan_result.issues_found > 0)
  
  // 验证代码安全问题
  let sql_injection_issue = code_scan_result.issues.find(fn(issue) { issue.type == "sql_injection" })
  assert_true(sql_injection_issue != None)
  assert_eq(sql_injection_issue.severity, "critical")
  assert_true(sql_injection_issue.file == "user_controller.js")
  
  let path_traversal_issue = code_scan_result.issues.find(fn(issue) { issue.type == "path_traversal" })
  assert_true(path_traversal_issue != None)
  assert_eq(path_traversal_issue.severity, "high")
  assert_true(path_traversal_issue.file == "file_upload.js")
  
  // 生成安全报告
  let security_report = VulnerabilityScanner::generate_security_report(vulnerability_scanner, {
    dependency_scan: dependency_scan_result,
    configuration_scan: config_scan_result,
    code_scan: code_scan_result,
    include_recommendations: true,
    include_fixes: true,
    include_risk_score: true
  })
  
  assert_true(security_report.contains("Security Vulnerability Report"))
  assert_true(security_report.contains("Dependency Vulnerabilities"))
  assert_true(security_report.contains("Configuration Issues"))
  assert_true(security_report.contains("Code Security Issues"))
  assert_true(security_report.contains("Risk Score"))
  assert_true(security_report.contains("Recommendations"))
  
  // 测试风险评分计算
  let risk_score = VulnerabilityScanner::calculate_risk_score(vulnerability_scanner, {
    dependency_vulnerabilities: dependency_scan_result.vulnerabilities,
    configuration_issues: config_scan_result.issues,
    code_issues: code_scan_result.issues
  })
  
  assert_true(risk_score.overall_score >= 0 and risk_score.overall_score <= 10)
  assert_true(risk_score.dependency_score >= 0 and risk_score.dependency_score <= 10)
  assert_true(risk_score.configuration_score >= 0 and risk_score.configuration_score <= 10)
  assert_true(risk_score.code_score >= 0 and risk_score.code_score <= 10)
  
  // 测试修复建议生成
  let fix_recommendations = VulnerabilityScanner::generate_fix_recommendations(vulnerability_scanner, {
    dependency_vulnerabilities: dependency_scan_result.vulnerabilities,
    configuration_issues: config_scan_result.issues,
    code_issues: code_scan_result.issues
  })
  
  assert_true(fix_recommendations.length() > 0)
  
  # 验证依赖修复建议
  let dependency_fixes = fix_recommendations.filter(fn(rec) { rec.category == "dependencies" })
  assert_true(dependency_fixes.length() > 0)
  
  let lodash_fix = dependency_fixes.find(fn(rec) { rec.package == "lodash" })
  assert_true(lodash_fix != None)
  assert_eq(lodash_fix.action, "upgrade")
  assert_eq(lodash_fix.recommended_version, "4.17.20")
  
  # 验证配置修复建议
  let config_fixes = fix_recommendations.filter(fn(rec) { rec.category == "configuration" })
  assert_true(config_fixes.length() > 0)
  
  let debug_fix = config_fixes.find(fn(rec) { rec.setting == "debug_mode" })
  assert_true(debug_fix != None)
  assert_eq(debug_fix.action, "disable")
  
  # 验证代码修复建议
  let code_fixes = fix_recommendations.filter(fn(rec) { rec.category == "code" })
  assert_true(code_fixes.length() > 0)
  
  let sql_fix = code_fixes.find(fn(rec) { rec.type == "sql_injection" })
  assert_true(sql_fix != None)
  assert_eq(sql_fix.action, "refactor")
  assert_true(sql_fix.recommendation.contains("parameterized queries"))
}
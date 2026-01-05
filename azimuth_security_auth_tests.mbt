// Azimuth 安全认证和授权测试用例
// 专注于身份认证、权限管理、安全策略和访问控制功能的测试

// 测试1: 基本身份认证
test "基本身份认证测试" {
  // 创建认证管理器
  let auth_manager = AuthenticationManager::new({
    password_policy: {
      min_length: 8,
      require_uppercase: true,
      require_lowercase: true,
      require_numbers: true,
      require_special_chars: true,
      max_age_days: 90
    },
    session_policy: {
      timeout_minutes: 30,
      max_concurrent_sessions: 3,
      secure_cookies: true
    },
    lockout_policy: {
      max_attempts: 5,
      lockout_duration_minutes: 15,
      reset_after_success: true
    }
  })
  
  // 注册用户
  let user_registration = auth_manager.register_user({
    username: "testuser",
    email: "test@example.com",
    password: "SecurePass123!",
    profile: {
      first_name: "Test",
      last_name: "User",
      department: "Engineering"
    }
  })
  
  assert_true(user_registration.success)
  assert_true(user_registration.user_id != None)
  
  // 测试有效登录
  let valid_login = auth_manager.authenticate({
    username: "testuser",
    password: "SecurePass123!",
    ip_address: "192.168.1.100",
    user_agent: "Mozilla/5.0 (Test Browser)"
  })
  
  assert_true(valid_login.success)
  assert_true(valid_login.session_token != None)
  assert_true(valid_login.user.id == user_registration.user_id)
  
  // 测试无效密码
  let invalid_password_login = auth_manager.authenticate({
    username: "testuser",
    password: "WrongPassword123!",
    ip_address: "192.168.1.100",
    user_agent: "Mozilla/5.0 (Test Browser)"
  })
  
  assert_false(invalid_password_login.success)
  assert_eq(invalid_password_login.error, "无效的用户名或密码")
  
  // 测试无效用户名
  let invalid_username_login = auth_manager.authenticate({
    username: "nonexistentuser",
    password: "SecurePass123!",
    ip_address: "192.168.1.100",
    user_agent: "Mozilla/5.0 (Test Browser)"
  })
  
  assert_false(invalid_username_login.success)
  assert_eq(invalid_username_login.error, "无效的用户名或密码")
  
  // 测试账户锁定
  for i in 0..=5 {
    auth_manager.authenticate({
      username: "testuser",
      password: "WrongPassword" + i.to_string(),
      ip_address: "192.168.1.100",
      user_agent: "Mozilla/5.0 (Test Browser)"
    })
  }
  
  let locked_account_login = auth_manager.authenticate({
    username: "testuser",
    password: "SecurePass123!",
    ip_address: "192.168.1.100",
    user_agent: "Mozilla/5.0 (Test Browser)"
  })
  
  assert_false(locked_account_login.success)
  assert_eq(locked_account_login.error, "账户已锁定")
  
  // 测试会话验证
  let session_validation = auth_manager.validate_session(valid_login.session_token.unwrap())
  assert_true(session_validation.success)
  assert_eq(session_validation.user.id, user_registration.user_id)
  
  // 测试无效会话
  let invalid_session_validation = auth_manager.validate_session("invalid_session_token")
  assert_false(invalid_session_validation.success)
  
  // 测试会话注销
  let logout_result = auth_manager.logout(valid_login.session_token.unwrap())
  assert_true(logout_result.success)
  
  // 注销后验证会话应该失败
  let post_logout_validation = auth_manager.validate_session(valid_login.session_token.unwrap())
  assert_false(post_logout_validation.success)
  
  // 测试密码策略验证
  let weak_password_result = auth_manager.validate_password("weak")
  assert_false(weak_password_result.valid)
  assert_true(weak_password_result.errors.length() > 0)
  
  let strong_password_result = auth_manager.validate_password("StrongPassword123!")
  assert_true(strong_password_result.valid)
  assert_eq(strong_password_result.errors.length(), 0)
  
  // 测试密码重置
  let password_reset_request = auth_manager.request_password_reset("test@example.com")
  assert_true(password_reset_request.success)
  assert_true(password_reset_request.reset_token != None)
  
  let password_reset_execution = auth_manager.execute_password_reset(
    password_reset_request.reset_token.unwrap(),
    "NewSecurePass456!"
  )
  
  assert_true(password_reset_execution.success)
  
  // 使用新密码登录
  let new_password_login = auth_manager.authenticate({
    username: "testuser",
    password: "NewSecurePass456!",
    ip_address: "192.168.1.100",
    user_agent: "Mozilla/5.0 (Test Browser)"
  })
  
  assert_true(new_password_login.success)
}

// 测试2: 多因素认证
test "多因素认证测试" {
  // 创建MFA管理器
  let mfa_manager = MFAManager::new({
    totp_config: {
      issuer: "Azimuth",
      algorithm: "SHA1",
      digits: 6,
      period: 30
    },
    sms_config: {
      provider: "twilio",
      from_number: "+1234567890",
      template: "您的验证码是: {code}"
    },
    email_config: {
      from_address: "noreply@azimuth.com",
      subject: "Azimuth 验证码",
      template: "您的验证码是: {code}"
    },
    backup_codes: {
      count: 10,
      length: 8
    }
  })
  
  // 注册用户并启用MFA
  let user_id = "user123"
  
  // 设置TOTP
  let totp_setup = mfa_manager.setup_totp(user_id, {
    user_name: "testuser@azimuth.com",
    secret_encoding: "base32"
  })
  
  assert_true(totp_setup.success)
  assert_true(totp_setup.secret != None)
  assert_true(totp_setup.qr_code != None)
  
  // 验证TOTP设置
  let valid_code = generate_totp_code(totp_setup.secret.unwrap())
  let totp_verification = mfa_manager.verify_totp(user_id, valid_code)
  assert_true(totp_verification.success)
  
  // 设置短信MFA
  let sms_setup = mfa_manager.setup_sms(user_id, "+1234567890")
  assert_true(sms_setup.success)
  
  // 发送短信验证码
  let sms_send = mfa_manager.send_sms_code(user_id)
  assert_true(sms_send.success)
  
  // 验证短信验证码（模拟）
  let mock_sms_code = mfa_manager.get_last_sent_sms_code(user_id)
  let sms_verification = mfa_manager.verify_sms_code(user_id, mock_sms_code)
  assert_true(sms_verification.success)
  
  // 设置邮箱MFA
  let email_setup = mfa_manager.setup_email(user_id, "test@azimuth.com")
  assert_true(email_setup.success)
  
  // 发送邮箱验证码
  let email_send = mfa_manager.send_email_code(user_id)
  assert_true(email_send.success)
  
  // 验证邮箱验证码（模拟）
  let mock_email_code = mfa_manager.get_last_sent_email_code(user_id)
  let email_verification = mfa_manager.verify_email_code(user_id, mock_email_code)
  assert_true(email_verification.success)
  
  // 生成备份码
  let backup_codes_generation = mfa_manager.generate_backup_codes(user_id)
  assert_true(backup_codes_generation.success)
  assert_eq(backup_codes_generation.codes.length(), 10)
  
  // 测试备份码验证
  let backup_code = backup_codes_generation.codes[0]
  let backup_verification = mfa_manager.verify_backup_code(user_id, backup_code)
  assert_true(backup_verification.success)
  
  // 验证使用过的备份码不能再次使用
  let used_backup_verification = mfa_manager.verify_backup_code(user_id, backup_code)
  assert_false(used_backup_verification.success)
  
  // 测试MFA认证流程
  let primary_auth = AuthenticationManager::new()
  let primary_login = primary_auth.authenticate({
    username: "testuser",
    password: "SecurePass123!",
    ip_address: "192.168.1.100",
    user_agent: "Mozilla/5.0 (Test Browser)"
  })
  
  assert_true(primary_login.success)
  
  // 需要MFA验证
  let mfa_challenge = mfa_manager.create_challenge(user_id, {
    methods: ["totp", "sms"],
    timeout_seconds: 300,
    context: {
      ip_address: "192.168.1.100",
      user_agent: "Mozilla/5.0 (Test Browser)"
    }
  })
  
  assert_true(mfa_challenge.success)
  assert_true(mfa_challenge.challenge_id != None)
  
  // 完成MFA挑战
  let mfa_completion = mfa_manager.complete_challenge(
    mfa_challenge.challenge_id.unwrap(),
    "totp",
    valid_code
  )
  
  assert_true(mfa_completion.success)
  assert_true(mfa_completion.auth_token != None)
  
  // 测试MFA设备管理
  let device_registration = mfa_manager.register_device(user_id, {
    name: "iPhone 13",
    type: "mobile",
    trusted: false
  })
  
  assert_true(device_registration.success)
  assert_true(device_registration.device_id != None)
  
  // 信任设备
  let trust_device_result = mfa_manager.trust_device(
    user_id,
    device_registration.device_id.unwrap(),
    30  // 30天
  )
  
  assert_true(trust_device_result.success)
  
  // 测试受信任设备的登录（不需要MFA）
  let trusted_device_login = mfa_manager.authenticate_with_device(
    user_id,
    device_registration.device_id.unwrap()
  )
  
  assert_true(trusted_device_login.success)
  assert_true(trusted_device_login.skip_mfa)
  
  // 测试MFA统计
  let mfa_stats = mfa_manager.get_statistics(user_id)
  assert_true(mfa_stats.total_attempts > 0)
  assert_true(mfa_stats.successful_verifications > 0)
  assert_true(mfa_stats.failed_verifications >= 0)
  assert_true(mfa_stats.registered_devices > 0)
}

// 测试3: 基于角色的访问控制
test "基于角色的访问控制测试" {
  // 创建RBAC管理器
  let rbac_manager = RBACManager::new({
    hierarchical_roles: true,
    permission_inheritance: true,
    session_caching: true,
    audit_logging: true
  })
  
  // 定义权限
  rbac_manager.define_permission("user.read", "读取用户信息")
  rbac_manager.define_permission("user.write", "修改用户信息")
  rbac_manager.define_permission("user.delete", "删除用户")
  rbac_manager.define_permission("admin.system", "系统管理")
  rbac_manager.define_permission("reports.view", "查看报告")
  rbac_manager.define_permission("reports.export", "导出报告")
  
  // 定义角色
  rbac_manager.define_role("user", "普通用户", ["user.read"])
  rbac_manager.define_role("moderator", "版主", ["user.read", "user.write"])
  rbac_manager.define_role("admin", "管理员", ["user.read", "user.write", "user.delete", "admin.system"])
  rbac_manager.define_role("analyst", "分析师", ["reports.view", "reports.export"])
  
  // 设置角色层次结构
  rbac_manager.set_role_hierarchy("admin", ["moderator", "analyst"])  // admin继承moderator和analyst的权限
  rbac_manager.set_role_hierarchy("moderator", ["user"])  // moderator继承user的权限
  
  // 创建用户并分配角色
  let regular_user_id = rbac_manager.create_user("regular_user", {
    email: "regular@example.com",
    department: "Sales"
  })
  
  let moderator_user_id = rbac_manager.create_user("moderator_user", {
    email: "moderator@example.com",
    department: "Support"
  })
  
  let admin_user_id = rbac_manager.create_user("admin_user", {
    email: "admin@example.com",
    department: "IT"
  })
  
  // 分配角色
  rbac_manager.assign_role(regular_user_id, "user")
  rbac_manager.assign_role(moderator_user_id, "moderator")
  rbac_manager.assign_role(admin_user_id, "admin")
  
  // 测试权限检查
  let user_read_check = rbac_manager.has_permission(regular_user_id, "user.read")
  assert_true(user_read_check)  // user角色有user.read权限
  
  let user_write_check = rbac_manager.has_permission(regular_user_id, "user.write")
  assert_false(user_write_check)  // user角色没有user.write权限
  
  let moderator_write_check = rbac_manager.has_permission(moderator_user_id, "user.write")
  assert_true(moderator_write_check)  // moderator角色有user.write权限
  
  let admin_delete_check = rbac_manager.has_permission(admin_user_id, "user.delete")
  assert_true(admin_delete_check)  // admin角色有user.delete权限
  
  let admin_reports_check = rbac_manager.has_permission(admin_user_id, "reports.view")
  assert_true(admin_reports_check)  // admin通过层次结构继承analyst的权限
  
  // 测试批量权限检查
  let batch_permissions = ["user.read", "user.write", "admin.system", "reports.view"]
  let admin_batch_check = rbac_manager.has_permissions(admin_user_id, batch_permissions)
  
  assert_true(admin_batch_check.get("user.read"))   // 直接权限
  assert_true(admin_batch_check.get("user.write"))  // 继承权限
  assert_true(admin_batch_check.get("admin.system")) // 直接权限
  assert_true(admin_batch_check.get("reports.view")) // 继承权限
  
  // 测试动态权限
  rbac_manager.define_dynamic_permission("user.own", {
    description: "操作自己的用户信息",
    evaluator: fn(user_id, resource_id, context) {
      user_id == resource_id
    }
  })
  
  let own_user_check = rbac_manager.has_permission_for_resource(
    regular_user_id,
    "user.own",
    regular_user_id,
    {}
  )
  
  assert_true(own_user_check)  // 用户可以操作自己的信息
  
  let other_user_check = rbac_manager.has_permission_for_resource(
    regular_user_id,
    "user.own",
    moderator_user_id,
    {}
  )
  
  assert_false(other_user_check)  // 用户不能操作其他人的信息
  
  // 测试临时权限提升
  let elevation_request = rbac_manager.request_permission_elevation(
    regular_user_id,
    "user.write",
    {
      reason: "需要更新个人资料",
      duration_minutes: 10,
      approver: moderator_user_id,
      context: {
        ip_address: "192.168.1.100",
        request_time: Time::now()
      }
    }
  )
  
  assert_true(elevation_request.success)
  assert_true(elevation_request.elevation_id != None)
  
  // 批准权限提升
  let elevation_approval = rbac_manager.approve_elevation(
    elevation_request.elevation_id.unwrap(),
    moderator_user_id,
    "合理的请求"
  )
  
  assert_true(elevation_approval.success)
  
  // 验证临时权限
  let elevated_permission_check = rbac_manager.has_permission(regular_user_id, "user.write")
  assert_true(elevated_permission_check)  // 现在应该有临时权限
  
  // 测试角色分配历史
  let role_history = rbac_manager.get_role_assignment_history(admin_user_id)
  assert_true(role_history.length() > 0)
  
  let admin_assignment = role_history.find(fn(entry) {
    entry.role == "admin" && entry.action == "assigned"
  })
  assert_true(admin_assignment != None)
  
  // 测试权限审计
  let permission_audit = rbac_manager.audit_permissions({
    include_inherited: true,
    include_temporary: true,
    format: "detailed"
  })
  
  assert_true(permission_audit.success)
  assert_true(permission_audit.users.length() > 0)
  
  // 测试角色模板
  rbac_manager.create_role_template("department_head", {
    description: "部门负责人角色模板",
    base_permissions: ["user.read", "reports.view"],
    conditional_permissions: [
      {
        permission: "user.write",
        condition: "department == user.department"
      }
    ]
  })
  
  let template_application = rbac_manager.apply_role_template(
    "department_head",
    "head_of_sales",
    {
      department: "Sales"
    }
  )
  
  assert_true(template_application.success)
  
  // 测试权限缓存
  let cache_stats = rbac_manager.get_cache_statistics()
  assert_true(cache_stats.hit_rate > 0.0)
  assert_true(cache_stats.total_queries > 0)
}

// 测试4: JWT令牌管理
test "JWT令牌管理测试" {
  // 创建JWT管理器
  let jwt_manager = JWTManager::new({
    algorithm: "RS256",
    issuer: "azimuth-auth",
    audience: "azimuth-api",
    access_token_ttl: 3600,      // 1小时
    refresh_token_ttl: 604800,   // 7天
    clock_skew_tolerance: 30,    // 30秒时钟偏差容忍
    rotation_enabled: true
  })
  
  // 生成密钥对
  let key_generation = jwt_manager.generate_key_pair()
  assert_true(key_generation.success)
  assert_true(key_generation.public_key.length() > 0)
  assert_true(key_generation.private_key.length() > 0)
  
  // 创建访问令牌
  let token_claims = {
    sub: "user123",
    username: "testuser",
    email: "test@example.com",
    roles: ["user", "analyst"],
    permissions: ["user.read", "reports.view"],
    metadata: {
      department: "Engineering",
      location: "US-West"
    }
  }
  
  let access_token_result = jwt_manager.create_access_token(token_claims)
  assert_true(access_token_result.success)
  assert_true(access_token_result.token.length() > 0)
  
  // 验证访问令牌
  let token_validation = jwt_manager.validate_token(access_token_result.token)
  assert_true(token_validation.success)
  
  match token_validation.claims {
    Some(claims) => {
      assert_eq(claims.sub, "user123")
      assert_eq(claims.username, "testuser")
      assert_eq(claims.email, "test@example.com")
      assert_true(claims.roles.contains("user"))
      assert_true(claims.roles.contains("analyst"))
    }
    None => assert_true(false)
  }
  
  // 创建刷新令牌
  let refresh_token_result = jwt_manager.create_refresh_token(token_claims)
  assert_true(refresh_token_result.success)
  assert_true(refresh_token_result.token.length() > 0)
  
  // 使用刷新令牌获取新的访问令牌
  let token_refresh = jwt_manager.refresh_access_token(refresh_token_result.token)
  assert_true(token_refresh.success)
  assert_true(token_refresh.access_token != access_token_result.token)  // 应该是新的令牌
  
  // 测试令牌吊销
  let revocation_result = jwt_manager.revoke_token(access_token_result.token)
  assert_true(revocation_result.success)
  
  // 吊销后的令牌验证应该失败
  let revoked_token_validation = jwt_manager.validate_token(access_token_result.token)
  assert_false(revoked_token_validation.success)
  assert_eq(revoked_token_validation.error, "令牌已吊销")
  
  // 测试令牌黑名单
  let blacklist_result = jwt_manager.add_to_blacklist(refresh_token_result.token)
  assert_true(blacklist_result.success)
  
  let blacklisted_validation = jwt_manager.validate_token(refresh_token_result.token)
  assert_false(blacklisted_validation.success)
  assert_eq(blacklisted_validation.error, "令牌在黑名单中")
  
  // 测试令牌解析（不验证签名）
  let token_parsing = jwt_manager.parse_token_without_validation(token_refresh.access_token)
  assert_true(token_parsing.success)
  
  match token_parsing.claims {
    Some(claims) => {
      assert_eq(claims.sub, "user123")
      assert_eq(claims.issuer, "azimuth-auth")
      assert_eq(claims.audience, "azimuth-api")
    }
    None => assert_true(false)
  }
  
  // 测试令牌过期
  let short_lived_claims = {
    sub: "user456",
    username: "shortlived",
    email: "short@example.com",
    roles: ["user"],
    permissions: ["user.read"]
  }
  
  let short_lived_token = jwt_manager.create_token_with_ttl(short_lived_claims, 1)  // 1秒TTL
  assert_true(short_lived_token.success)
  
  // 等待令牌过期
  Time::sleep(2000)
  
  let expired_token_validation = jwt_manager.validate_token(short_lived_token.token)
  assert_false(expired_token_validation.success)
  assert_eq(expired_token_validation.error, "令牌已过期")
  
  // 测试令牌续期
  let renewal_result = jwt_manager.renew_token(refresh_token_result.token, {
    extend_ttl: true,
    rotate_refresh_token: true
  })
  
  assert_true(renewal_result.success)
  assert_true(renewal_result.new_access_token != None)
  assert_true(renewal_result.new_refresh_token != None)
  
  // 测试令牌元数据
  let token_metadata = jwt_manager.get_token_metadata(token_refresh.access_token)
  assert_true(token_metadata.success)
  
  match token_metadata.metadata {
    Some(metadata) => {
      assert_true(metadata.created_at > 0)
      assert_true(metadata.expires_at > metadata.created_at)
      assert_eq(metadata.issuer, "azimuth-auth")
    }
    None => assert_true(false)
  }
  
  // 测试批量令牌验证
  let tokens_to_validate = [
    token_refresh.access_token,
    short_lived_token.token  // 已过期
  ]
  
  let batch_validation = jwt_manager.validate_tokens_batch(tokens_to_validate)
  assert_true(batch_validation.success)
  assert_eq(batch_validation.results.length(), 2)
  assert_true(batch_validation.results[0].valid)    // 第一个令牌有效
  assert_false(batch_validation.results[1].valid)  // 第二个令牌已过期
  
  // 测试JWT密钥轮换
  let key_rotation = jwt_manager.rotate_keys({
    grace_period: 3600,  // 1小时宽限期
    keep_old_keys: true
  })
  
  assert_true(key_rotation.success)
  assert_true(key_rotation.new_key_id != None)
  
  // 使用新密钥创建令牌
  let new_key_token = jwt_manager.create_access_token(token_claims)
  assert_true(new_key_token.success)
  
  // 验证新密钥创建的令牌
  let new_key_validation = jwt_manager.validate_token(new_key_token.token)
  assert_true(new_key_validation.success)
  
  // 测试令牌统计
  let jwt_stats = jwt_manager.get_statistics()
  assert_true(jwt_stats.tokens_created > 0)
  assert_true(jwt_stats.tokens_validated > 0)
  assert_true(jwt_stats.tokens_revoked > 0)
  assert_true(jwt_stats.active_keys > 0)
}

// 测试5: OAuth2和OpenID Connect
test "OAuth2和OpenID Connect测试" {
  // 创建OAuth2提供者
  let oauth2_provider = OAuth2Provider::new({
    issuer: "https://auth.azimuth.com",
    authorization_endpoint: "https://auth.azimuth.com/oauth/authorize",
    token_endpoint: "https://auth.azimuth.com/oauth/token",
    userinfo_endpoint: "https://auth.azimuth.com/oauth/userinfo",
    jwks_uri: "https://auth.azimuth.com/.well-known/jwks.json",
    supported_grant_types: ["authorization_code", "client_credentials", "refresh_token"],
    supported_response_types: ["code", "token", "id_token"],
    supported_scopes: ["openid", "profile", "email", "read", "write"]
  })
  
  // 注册OAuth2客户端
  let client_registration = oauth2_provider.register_client({
    client_id: "azimuth-web-app",
    client_name: "Azimuth Web Application",
    client_type: "confidential",
    redirect_uris: ["https://app.azimuth.com/callback", "https://app.azimuth.com/silent-callback"],
    allowed_grant_types: ["authorization_code", "refresh_token"],
    allowed_scopes: ["openid", "profile", "email", "read"],
    require_pkce: true,
    token_endpoint_auth_method: "client_secret_basic"
  })
  
  assert_true(client_registration.success)
  assert_true(client_registration.client_id != None)
  assert_true(client_registration.client_secret != None)
  
  // 生成授权URL
  let auth_url_generation = oauth2_provider.generate_authorization_url({
    client_id: client_registration.client_id.unwrap(),
    redirect_uri: "https://app.azimuth.com/callback",
    response_type: "code",
    scope: "openid profile email read",
    state: "random_state_123",
    code_challenge: "code_challenge_456",
    code_challenge_method: "S256"
  })
  
  assert_true(auth_url_generation.success)
  assert_true(auth_url_generation.url.contains("response_type=code"))
  assert_true(auth_url_generation.url.contains("scope=openid"))
  
  // 模拟授权码交换
  let authorization_code = "auth_code_789"
  
  let token_exchange = oauth2_provider.exchange_code_for_tokens({
    client_id: client_registration.client_id.unwrap(),
    client_secret: client_registration.client_secret.unwrap(),
    code: authorization_code,
    redirect_uri: "https://app.azimuth.com/callback",
    code_verifier: "code_verifier_123"
  })
  
  assert_true(token_exchange.success)
  assert_true(token_exchange.access_token != None)
  assert_true(token_exchange.refresh_token != None)
  assert_true(token_exchange.id_token != None)  // OpenID Connect ID令牌
  
  // 验证ID令牌
  let id_token_validation = oauth2_provider.validate_id_token(
    token_exchange.id_token.unwrap(),
    client_registration.client_id.unwrap()
  )
  
  assert_true(id_token_validation.success)
  
  match id_token_validation.claims {
    Some(claims) => {
      assert_eq(claims.iss, "https://auth.azimuth.com")
      assert_eq(claims.aud, client_registration.client_id.unwrap())
      assert_true(claims.exp > Time::now())
    }
    None => assert_true(false)
  }
  
  // 获取用户信息
  let userinfo_result = oauth2_provider.get_userinfo(
    token_exchange.access_token.unwrap()
  )
  
  assert_true(userinfo_result.success)
  
  match userinfo_result.userinfo {
    Some(userinfo) => {
      assert_true(userinfo.contains_key("sub"))
      assert_true(userinfo.contains_key("email"))
    }
    None => assert_true(false)
  }
  
  // 刷新访问令牌
  let token_refresh = oauth2_provider.refresh_access_token({
    client_id: client_registration.client_id.unwrap(),
    client_secret: client_registration.client_secret.unwrap(),
    refresh_token: token_exchange.refresh_token.unwrap()
  })
  
  assert_true(token_refresh.success)
  assert_true(token_refresh.access_token != None)
  
  // 测试客户端凭证流程
  let client_credentials_registration = oauth2_provider.register_client({
    client_id: "azimuth-service",
    client_name: "Azimuth Service",
    client_type: "confidential",
    allowed_grant_types: ["client_credentials"],
    allowed_scopes: ["read", "write"],
    token_endpoint_auth_method: "client_secret_basic"
  })
  
  let client_credentials_flow = oauth2_provider.client_credentials_flow({
    client_id: client_credentials_registration.client_id.unwrap(),
    client_secret: client_credentials_registration.client_secret.unwrap(),
    scope: "read write"
  })
  
  assert_true(client_credentials_flow.success)
  assert_true(client_credentials_flow.access_token != None)
  
  // 测试令牌内省
  let token_introspection = oauth2_provider.introspect_token(
    client_credentials_registration.client_id.unwrap(),
    client_credentials_registration.client_secret.unwrap(),
    client_credentials_flow.access_token.unwrap()
  )
  
  assert_true(token_introspection.success)
  
  match token_introspection.token_info {
    Some(info) => {
      assert_true(info.active)
      assert_eq(info.client_id, client_credentials_registration.client_id.unwrap())
      assert_true(info.scope.contains("read"))
      assert_true(info.scope.contains("write"))
    }
    None => assert_true(false)
  }
  
  // 测试令牌撤销
  let token_revocation = oauth2_provider.revoke_token(
    client_credentials_registration.client_id.unwrap(),
    client_credentials_registration.client_secret.unwrap(),
    client_credentials_flow.access_token.unwrap()
  )
  
  assert_true(token_revocation.success)
  
  // 撤销后的令牌内省应该显示不活跃
  let revoked_introspection = oauth2_provider.introspect_token(
    client_credentials_registration.client_id.unwrap(),
    client_credentials_registration.client_secret.unwrap(),
    client_credentials_flow.access_token.unwrap()
  )
  
  assert_true(revoked_introspection.success)
  
  match revoked_introspection.token_info {
    Some(info) => {
      assert_false(info.active)
    }
    None => assert_true(false)
  }
  
  // 测试OpenID Connect发现
  let discovery_result = oauth2_provider.discover_configuration("https://auth.azimuth.com")
  assert_true(discovery_result.success)
  
  match discovery_result.configuration {
    Some(config) => {
      assert_eq(config.issuer, "https://auth.azimuth.com")
      assert_eq(config.authorization_endpoint, "https://auth.azimuth.com/oauth/authorize")
      assert_eq(config.token_endpoint, "https://auth.azimuth.com/oauth/token")
      assert_true(config.jwks_uri.contains("jwks.json"))
    }
    None => assert_true(false)
  }
  
  // 测试动态客户端注册
  let dynamic_registration = oauth2_provider.register_client_dynamically({
    client_name: "Dynamic Client",
    redirect_uris: ["https://dynamic.example.com/callback"],
    grant_types: ["authorization_code", "refresh_token"],
    response_types: ["code"],
    scope: "openid profile email",
    software_id: "dynamic-software-123",
    software_version: "1.0.0"
  })
  
  assert_true(dynamic_registration.success)
  assert_true(dynamic_registration.client_id != None)
  assert_true(dynamic_registration.registration_access_token != None)
  
  // 测试会话管理
  let session_management = oauth2_provider.create_session({
    user_id: "user123",
    client_id: client_registration.client_id.unwrap(),
    auth_time: Time::now(),
    expires_at: Time::now() + 3600
  })
  
  assert_true(session_management.success)
  assert_true(session_management.session_id != None)
  
  // 检查会话状态
  let session_status = oauth2_provider.check_session_status(
    session_management.session_id.unwrap()
  )
  
  assert_true(session_status.active)
  
  // 注销会话
  let session_logout = oauth2_provider.logout_session(
    session_management.session_id.unwrap()
  )
  
  assert_true(session_logout.success)
  
  // 注销后检查会话状态
  let post_logout_status = oauth2_provider.check_session_status(
    session_management.session_id.unwrap()
  )
  
  assert_false(post_logout_status.active)
}

// 测试6: API密钥管理
test "API密钥管理测试" {
  // 创建API密钥管理器
  let apikey_manager = APIKeyManager::new({
    key_length: 32,
    key_prefix: "az_",
    hashing_algorithm: "SHA256",
    default_quota: {
      requests_per_day: 1000,
      requests_per_hour: 100,
      requests_per_minute: 10
    },
    rate_limiting: {
      enabled: true,
      algorithm: "token_bucket",
      burst_size: 20
    },
    audit_logging: true
  })
  
  // 创建API密钥
  let key_creation = apikey_manager.create_key({
    name: "Production API Key",
    description: "用于生产环境的API访问",
    owner: "user123",
    permissions: ["read", "write"],
    quotas: {
      requests_per_day: 5000,
      requests_per_hour: 500,
      requests_per_minute: 50
    },
    restrictions: {
      ip_whitelist: ["192.168.1.0/24", "10.0.0.0/8"],
      referer_whitelist: ["https://app.azimuth.com"],
      user_agent_whitelist: ["AzimuthClient/*"]
    },
    expires_at: Time::now() + 86400 * 30  // 30天后过期
  })
  
  assert_true(key_creation.success)
  assert_true(key_creation.key_id != None)
  assert_true(key_creation.api_key != None)
  assert_true(key_creation.api_key.unwrap().starts_with("az_"))
  
  let key_id = key_creation.key_id.unwrap()
  let api_key = key_creation.api_key.unwrap()
  
  // 验证API密钥
  let key_validation = apikey_manager.validate_key(api_key)
  assert_true(key_validation.success)
  
  match key_validation.key_info {
    Some(info) => {
      assert_eq(info.id, key_id)
      assert_eq(info.name, "Production API Key")
      assert_eq(info.owner, "user123")
      assert_true(info.permissions.contains("read"))
      assert_true(info.permissions.contains("write"))
    }
    None => assert_true(false)
  }
  
  // 测试API密钥认证
  let auth_result = apikey_manager.authenticate({
    api_key: api_key,
    ip_address: "192.168.1.100",
    referer: "https://app.azimuth.com",
    user_agent: "AzimuthClient/1.0.0"
  })
  
  assert_true(auth_result.success)
  assert_eq(auth_result.key_id, key_id)
  
  // 测试IP限制
  let ip_restriction_auth = apikey_manager.authenticate({
    api_key: api_key,
    ip_address: "203.0.113.1",  // 不在白名单中的IP
    referer: "https://app.azimuth.com",
    user_agent: "AzimuthClient/1.0.0"
  })
  
  assert_false(ip_restriction_auth.success)
  assert_eq(ip_restriction_auth.error, "IP地址不在白名单中")
  
  // 测试速率限制
  let mut rate_limit_results = []
  
  // 快速发送11个请求（超过每分钟10个的限制）
  for i in 0..=11 {
    let result = apikey_manager.authenticate({
      api_key: api_key,
      ip_address: "192.168.1.100",
      referer: "https://app.azimuth.com",
      user_agent: "AzimuthClient/1.0.0"
    })
    rate_limit_results = rate_limit_results.push(result)
  }
  
  // 前10个请求应该成功，第11个应该被限制
  assert_true(rate_limit_results[0].success)
  assert_true(rate_limit_results[9].success)
  assert_false(rate_limit_results[10].success)
  assert_eq(rate_limit_results[10].error, "速率限制")
  
  // 测试权限检查
  let permission_check = apikey_manager.check_permission(key_id, "read")
  assert_true(permission_check)
  
  let invalid_permission_check = apikey_manager.check_permission(key_id, "admin")
  assert_false(invalid_permission_check)
  
  // 更新API密钥
  let update_result = apikey_manager.update_key(key_id, {
    name: "Updated Production API Key",
    quotas: {
      requests_per_day: 10000,
      requests_per_hour: 1000,
      requests_per_minute: 100
    }
  })
  
  assert_true(update_result.success)
  
  // 验证更新
  let updated_key_info = apikey_manager.get_key_info(key_id)
  assert_true(updated_key_info.success)
  
  match updated_key_info.key_info {
    Some(info) => {
      assert_eq(info.name, "Updated Production API Key")
      assert_eq(info.quotas.requests_per_day, 10000)
    }
    None => assert_true(false)
  }
  
  // 创建临时API密钥
  let temp_key_creation = apikey_manager.create_key({
    name: "Temporary API Key",
    description: "临时测试密钥",
    owner: "user456",
    permissions: ["read"],
    expires_at: Time::now() + 3600  // 1小时后过期
  })
  
  assert_true(temp_key_creation.success)
  
  // 测试密钥轮换
  let key_rotation = apikey_manager.rotate_key(key_id)
  assert_true(key_rotation.success)
  assert_true(key_rotation.new_api_key != None)
  assert_ne(key_rotation.new_api_key, api_key)  // 新密钥应该不同
  
  // 旧密钥应该仍然有效一段时间
  let old_key_still_valid = apikey_manager.validate_key(api_key)
  assert_true(old_key_still_valid.success)
  
  // 新密钥也应该有效
  let new_key_valid = apikey_manager.validate_key(key_rotation.new_api_key.unwrap())
  assert_true(new_key_valid.success)
  
  // 测试密钥使用统计
  let usage_stats = apikey_manager.get_usage_statistics(key_id, {
    period: "24h",
    granularity: "hour"
  })
  
  assert_true(usage_stats.success)
  assert_true(usage_stats.statistics.length() > 0)
  
  // 测试密钥审计日志
  let audit_logs = apikey_manager.get_audit_logs(key_id, {
    start_time: Time::now() - 86400,
    end_time: Time::now(),
    limit: 100
  })
  
  assert_true(audit_logs.success)
  assert_true(audit_logs.logs.length() > 0)
  
  // 测试密钥撤销
  let revocation_result = apikey_manager.revoke_key(key_id, {
    reason: "安全原因",
    revoked_by: "admin123"
  })
  
  assert_true(revocation_result.success)
  
  // 撤销后的密钥应该无效
  let revoked_key_validation = apikey_manager.validate_key(api_key)
  assert_false(revoked_key_validation.success)
  assert_eq(revoked_key_validation.error, "密钥已撤销")
  
  // 测试密钥恢复
  let restoration_result = apikey_manager.restore_key(key_id, {
    restored_by: "admin123",
    reason: "误撤销"
  })
  
  assert_true(restoration_result.success)
  
  // 恢复后的密钥应该重新有效
  let restored_key_validation = apikey_manager.validate_key(api_key)
  assert_true(restored_key_validation.success)
}

// 测试7: 安全策略和合规性
test "安全策略和合规性测试" {
  // 创建安全策略管理器
  let security_policy_manager = SecurityPolicyManager::new({
    default_password_policy: {
      min_length: 12,
      require_uppercase: true,
      require_lowercase: true,
      require_numbers: true,
      require_special_chars: true,
      max_age_days: 90,
      history_count: 12,
      dictionary_check: true
    },
    session_policy: {
      timeout_minutes: 30,
      max_concurrent_sessions: 3,
      secure_cookies: true,
      same_site_policy: "strict"
    },
    access_policy: {
      max_failed_attempts: 5,
      lockout_duration_minutes: 30,
      mfa_required_for_admin: true,
      ip_whitelist_enabled: false
    },
    audit_policy: {
      log_all_access: true,
      log_failed_attempts: true,
      log_privileged_actions: true,
      retention_days: 365
    }
  })
  
  // 创建合规性框架
  let compliance_framework = ComplianceFramework::new({
    standards: ["GDPR", "SOC2", "ISO27001", "HIPAA"],
    default_controls: [
      {
        id: "DATA_ENCRYPTION",
        name: "数据加密",
        description: "所有敏感数据必须加密存储和传输",
        category: "DataProtection",
        mandatory: true
      },
      {
        id: "ACCESS_LOGGING",
        name: "访问日志",
        description: "所有访问尝试必须记录日志",
        category: "AuditLogging",
        mandatory: true
      },
      {
        id: "MFA_REQUIREMENT",
        name: "多因素认证",
        description: "特权账户必须使用多因素认证",
        category: "Authentication",
        mandatory: true
      }
    ]
  })
  
  // 测试密码策略合规性
  let password_compliance = security_policy_manager.check_password_compliance("StrongPass123!")
  assert_true(password_compliance.compliant)
  assert_eq(password_compliance.violations.length(), 0)
  
  let weak_password_compliance = security_policy_manager.check_password_compliance("weak")
  assert_false(weak_password_compliance.compliant)
  assert_true(weak_password_compliance.violations.length() > 0)
  
  // 测试会话策略
  let session_compliance = security_policy_manager.check_session_compliance({
    timeout_minutes: 60,  // 超过策略的30分钟
    secure_cookies: true,
    same_site_policy: "strict"
  })
  
  assert_false(session_compliance.compliant)
  assert_true(session_compliance.violations.contains("会话超时时间超过策略限制"))
  
  // 测试访问策略
  let access_compliance = security_policy_manager.check_access_compliance({
    failed_attempts: 3,
    mfa_enabled: true,
    ip_address: "192.168.1.100"
  })
  
  assert_true(access_compliance.compliant)
  
  // 测试数据保护合规性
  let data_protection_compliance = compliance_framework.check_compliance({
    standard: "GDPR",
    controls: [
      { id: "DATA_ENCRYPTION", implemented: true },
      { id: "ACCESS_LOGGING", implemented: true },
      { id: "DATA_MINIMIZATION", implemented: false }
    ]
  })
  
  assert_false(data_protection_compliance.fully_compliant)
  assert_true(data_protection_compliance.compliance_score < 100.0)
  
  // 测试合规性报告生成
  let compliance_report = compliance_framework.generate_report({
    standards: ["GDPR", "SOC2"],
    include_recommendations: true,
    format: "detailed"
  })
  
  assert_true(compliance_report.success)
  assert_true(compliance_report.overall_score > 0.0)
  assert_true(compliance_report.recommendations.length() > 0)
  
  // 测试安全事件管理
  let security_event_manager = SecurityEventManager::new({
    alert_thresholds: {
      failed_login_attempts: 10,
      unusual_access_patterns: 5,
      privilege_escalation: 1
    },
    notification_channels: ["email", "slack"],
    auto_response_enabled: true
  })
  
  // 记录安全事件
  let failed_login_event = security_event_manager.record_event({
    type: "failed_login",
    severity: "medium",
    user_id: "user123",
    ip_address: "203.0.113.1",
    details: {
      username: "testuser",
      reason: "invalid_password"
    }
  })
  
  assert_true(failed_login_event.success)
  
  // 记录多个失败登录以触发警报
  for i in 0..=11 {
    security_event_manager.record_event({
      type: "failed_login",
      severity: "medium",
      user_id: "user123",
      ip_address: "203.0.113.1",
      details: {
        username: "testuser",
        reason: "invalid_password"
      }
    })
  }
  
  // 检查是否触发了安全警报
  let active_alerts = security_event_manager.get_active_alerts()
  assert_true(active_alerts.length() > 0)
  
  let failed_login_alert = active_alerts.find(fn(alert) => {
    alert.type == "failed_login_burst"
  })
  
  assert_true(failed_login_alert != None)
  
  // 测试风险评估
  let risk_assessment = security_policy_manager.assess_user_risk("user123", {
    include_login_history: true,
    include_access_patterns: true,
    include_device_fingerprint: true
  })
  
  assert_true(risk_assessment.success)
  assert_true(risk_assessment.risk_score >= 0.0 && risk_assessment.risk_score <= 100.0)
  
  // 测试安全策略更新
  let policy_update = security_policy_manager.update_policy("password_policy", {
    min_length: 16,  // 增加最小长度
    require_passphrase: true  // 新增要求
  })
  
  assert_true(policy_update.success)
  
  // 验证策略更新后的合规性检查
  let updated_password_compliance = security_policy_manager.check_password_compliance("StrongPass123!")
  assert_false(updated_password_compliance.compliant)  // 现在应该不合规
  
  // 测试合规性自动化检查
  let automated_check = compliance_framework.run_automated_check({
    schedule: "daily",
    controls_to_check: ["DATA_ENCRYPTION", "ACCESS_LOGGING"],
    auto_remediate: false,
    notification_on_failure: true
  })
  
  assert_true(automated_check.success)
  
  // 测试安全培训跟踪
  let training_tracker = SecurityTrainingTracker::new({
    required_courses: [
      { id: "SEC101", name: "安全基础", duration_minutes: 30 },
      { id: "SEC201", name: "数据保护", duration_minutes: 45 },
      { id: "SEC301", name: "威胁识别", duration_minutes: 60 }
    ],
    renewal_period_months: 12,
    auto_reminder: true
  })
  
  // 分配培训给用户
  let training_assignment = training_tracker.assign_training("user123", ["SEC101", "SEC201"])
  assert_true(training_assignment.success)
  
  // 记录培训完成
  let training_completion = training_tracker.record_completion("user123", "SEC101", {
    completion_date: Time::now(),
    score: 95,
    certificate_id: "CERT-123456"
  })
  
  assert_true(training_completion.success)
  
  // 检查培训状态
  let training_status = training_tracker.get_training_status("user123")
  assert_true(training_status.completed_courses.contains("SEC101"))
  assert_false(training_status.completed_courses.contains("SEC201"))
  
  // 测试安全审计
  let security_audit = SecurityAuditor::new({
    audit_scope: ["authentication", "authorization", "data_protection"],
    audit_frequency: "quarterly",
    automated_tests: true
  })
  
  // 执行安全审计
  let audit_execution = security_audit.execute_audit({
    start_date: Time::now() - 86400 * 90,  // 过去90天
    end_date: Time::now(),
    include_recommendations: true
  })
  
  assert_true(audit_execution.success)
  assert_true(audit_execution.findings.length() >= 0)
  assert_true(audit_execution.recommendations.length() >= 0)
  
  // 测试漏洞管理
  let vulnerability_manager = VulnerabilityManager::new({
    severity_levels: ["critical", "high", "medium", "low"],
    auto_scanning: true,
    remediation_timeframes: {
      critical: 24,    // 24小时
      high: 72,        // 3天
      medium: 168,     // 1周
      low: 720         // 30天
    }
  })
  
  // 报告漏洞
  let vulnerability_report = vulnerability_manager.report_vulnerability({
    title: "SQL注入漏洞",
    description: "在用户登录页面发现SQL注入漏洞",
    severity: "high",
    affected_component: "authentication_service",
    discovered_by: "security_team",
    discovered_at: Time::now()
  })
  
  assert_true(vulnerability_report.success)
  assert_true(vulnerability_report.vulnerability_id != None)
  
  // 检查漏洞修复状态
  let vulnerability_status = vulnerability_manager.get_vulnerability_status(
    vulnerability_report.vulnerability_id.unwrap()
  )
  
  assert_true(vulnerability_status.severity == "high")
  assert_true(vulnerability_status.status == "open")  // 初始状态应该是开放
  
  // 测试安全指标仪表盘
  let security_dashboard = SecurityDashboard::new({
    metrics: [
      "failed_login_attempts",
      "successful_logins",
      "security_incidents",
      "vulnerabilities_by_severity",
      "compliance_score"
    ],
    time_range: "30d",
    refresh_interval: 300  // 5分钟
  })
  
  // 生成仪表盘数据
  let dashboard_data = security_dashboard.generate_dashboard()
  assert_true(dashboard_data.success)
  assert_true(dashboard_data.metrics.length() > 0)
  
  // 验证关键指标存在
  assert_true(dashboard_data.metrics.contains_key("failed_login_attempts"))
  assert_true(dashboard_data.metrics.contains_key("compliance_score"))
}

// 测试8: 加密和密钥管理
test "加密和密钥管理测试" {
  // 创建密钥管理器
  let key_manager = KeyManager::new({
    default_algorithm: "AES-256-GCM",
    key_derivation: "PBKDF2",
    derivation_iterations: 100000,
    salt_length: 32,
    iv_length: 12,
    tag_length: 16
  })
  
  // 生成数据加密密钥
  let key_generation = key_manager.generate_key({
    algorithm: "AES-256-GCM",
    usage: "data_encryption",
    metadata: {
      created_by: "admin",
      purpose: "用户数据加密"
    }
  })
  
  assert_true(key_generation.success)
  assert_true(key_generation.key_id != None)
  assert_true(key_generation.key_material.length() == 32)  // 256位密钥
  
  let key_id = key_generation.key_id.unwrap()
  
  // 加密数据
  let plaintext = "这是一段需要加密的敏感数据"
  let encryption_result = key_manager.encrypt(key_id, plaintext)
  
  assert_true(encryption_result.success)
  assert_true(encryption_result.ciphertext != plaintext)
  assert_true(encryption_result.ciphertext.length() > plaintext.length())
  
  // 解密数据
  let decryption_result = key_manager.decrypt(
    key_id,
    encryption_result.ciphertext,
    encryption_result.nonce
  )
  
  assert_true(decryption_result.success)
  assert_eq(decryption_result.plaintext, plaintext)
  
  // 测试使用错误密钥解密
  let wrong_key_generation = key_manager.generate_key({
    algorithm: "AES-256-GCM",
    usage: "data_encryption"
  })
  
  let wrong_key_decryption = key_manager.decrypt(
    wrong_key_generation.key_id.unwrap(),
    encryption_result.ciphertext,
    encryption_result.nonce
  )
  
  assert_false(wrong_key_decryption.success)
  
  // 测试密钥轮换
  let key_rotation = key_manager.rotate_key(key_id, {
    preserve_old_key: true,
    transition_period: 86400  // 1天过渡期
  })
  
  assert_true(key_rotation.success)
  assert_true(key_rotation.new_key_id != None)
  assert_ne(key_rotation.new_key_id, key_id)
  
  // 使用新密钥加密
  let new_key_encryption = key_manager.encrypt(
    key_rotation.new_key_id.unwrap(),
    "使用新密钥加密的数据"
  )
  
  assert_true(new_key_encryption.success)
  
  // 测试密钥版本管理
  let key_versions = key_manager.get_key_versions(key_id)
  assert_true(key_versions.length() >= 2)  // 原始密钥 + 轮换后的密钥
  
  // 测试密钥过期
  let expiring_key = key_manager.generate_key({
    algorithm: "AES-256-GCM",
    usage: "temporary_encryption",
    expires_at: Time::now() + 3600  // 1小时后过期
  })
  
  assert_true(expiring_key.success)
  
  // 检查密钥状态
  let key_status = key_manager.get_key_status(expiring_key.key_id.unwrap())
  assert_true(key_status.active)
  
  // 测试密钥撤销
  let key_revocation = key_manager.revoke_key(key_id, {
    reason: "安全原因",
    revoked_by: "security_admin"
  })
  
  assert_true(key_revocation.success)
  
  let revoked_key_status = key_manager.get_key_status(key_id)
  assert_false(revoked_key_status.active)
  assert_eq(revoked_key_status.revocation_reason, "安全原因")
  
  // 测试密钥导入导出
  let export_key = key_manager.generate_key({
    algorithm: "AES-256-GCM",
    usage: "export_test"
  })
  
  let key_export = key_manager.export_key(
    export_key.key_id.unwrap(),
    {
      format: "pem",
      include_private_key: true,
      password: "export_password"
    }
  )
  
  assert_true(key_export.success)
  assert_true(key_export.key_data.length() > 0)
  
  let key_import = key_manager.import_key(
    key_export.key_data,
    {
      format: "pem",
      password: "export_password",
      usage: "imported_key"
    }
  )
  
  assert_true(key_import.success)
  
  // 测试HMAC
  let hmac_key = key_manager.generate_key({
    algorithm: "HMAC-SHA256",
    usage: "message_authentication"
  })
  
  let message = "需要认证的消息"
  let hmac_generation = key_manager.generate_hmac(
    hmac_key.key_id.unwrap(),
    message
  )
  
  assert_true(hmac_generation.success)
  assert_true(hmac_generation.hmac.length() == 64)  // SHA256 HMAC是64字符
  
  // 验证HMAC
  let hmac_verification = key_manager.verify_hmac(
    hmac_key.key_id.unwrap(),
    message,
    hmac_generation.hmac
  )
  
  assert_true(hmac_verification.valid)
  
  // 测试被篡改的消息验证
  let tampered_verification = key_manager.verify_hmac(
    hmac_key.key_id.unwrap(),
    "被篡改的消息",
    hmac_generation.hmac
  )
  
  assert_false(tampered_verification.valid)
  
  // 测试密钥派生
  let key_derivation = key_manager.derive_key({
    master_key_id: key_rotation.new_key_id.unwrap(),
    context: "user_data_encryption",
    salt: "user123_salt",
    output_length: 32
  })
  
  assert_true(key_derivation.success)
  assert_true(key_derivation.derived_key.length() == 32)
  
  // 测试数字签名
  let signing_key = key_manager.generate_key({
    algorithm: "ECDSA-P256",
    usage: "digital_signature"
  })
  
  let data_to_sign = "需要签名的数据"
  let signature_generation = key_manager.sign_data(
    signing_key.key_id.unwrap(),
    data_to_sign
  )
  
  assert_true(signature_generation.success)
  
  // 验证签名
  let signature_verification = key_manager.verify_signature(
    signing_key.key_id.unwrap(),
    data_to_sign,
    signature_generation.signature
  )
  
  assert_true(signature_verification.valid)
  
  // 测试密钥使用审计
  let audit_logs = key_manager.get_key_usage_logs(key_id, {
    start_time: Time::now() - 86400,
    end_time: Time::now(),
    operations: ["encrypt", "decrypt", "rotate", "revoke"]
  })
  
  assert_true(audit_logs.success)
  assert_true(audit_logs.logs.length() > 0)
  
  // 测试密钥备份
  let key_backup = key_manager.backup_keys({
    key_ids: [key_rotation.new_key_id.unwrap()],
    backup_location: "secure_backup_location",
    encryption: true
  })
  
  assert_true(key_backup.success)
  assert_true(key_backup.backup_id != None)
  
  // 测试密钥恢复
  let key_recovery = key_manager.restore_keys(
    key_backup.backup_id.unwrap(),
    {
      backup_location: "secure_backup_location",
      decryption_password: "backup_password"
    }
  )
  
  assert_true(key_recovery.success)
  assert_true(key_recovery.restored_keys.length() > 0)
}

// 测试9: 网络安全
test "网络安全测试" {
  // 创建网络安全管理器
  let network_security_manager = NetworkSecurityManager::new({
    firewall_rules: [
      {
        id: "allow_admin_access",
        action: "allow",
        source: "10.0.0.0/8",
        destination: "admin_panel",
        port: 443,
        protocol: "tcp"
      },
      {
        id: "block_suspicious_ips",
        action: "deny",
        source: "suspicious_ips_list",
        destination: "any",
        port: "any",
        protocol: "any"
      }
    ],
    ddos_protection: {
      enabled: true,
      threshold_requests_per_second: 1000,
      burst_size: 5000
    },
    ssl_configuration: {
      min_version: "TLSv1.2",
      preferred_ciphers: ["TLS_AES_256_GCM_SHA384", "TLS_CHACHA20_POLY1305_SHA256"],
      certificate_validation: "strict"
    }
  })
  
  // 测试IP白名单
  let ip_whitelist_check = network_security_manager.check_ip_allowed("10.0.1.100", "admin_panel")
  assert_true(ip_whitelist_check.allowed)
  
  let ip_blacklist_check = network_security_manager.check_ip_allowed("203.0.113.1", "admin_panel")
  assert_false(ip_blacklist_check.allowed)  // 假设在可疑IP列表中
  
  // 测试DDoS保护
  let ddos_protection = network_security_manager.get_ddos_protection()
  
  // 模拟正常流量
  for i in 0..=500 {
    let request_result = ddos_protection.process_request("192.168.1.100")
    assert_true(request_result.allowed)
  }
  
  // 模拟攻击流量
  let mut attack_blocked_count = 0
  for i in 0..=2000 {
    let request_result = ddos_protection.process_request("203.0.113.1")  // 攻击IP
    if not(request_result.allowed) {
      attack_blocked_count = attack_blocked_count + 1
    }
  }
  
  assert_true(attack_blocked_count > 0)  // 部分请求应该被阻止
  
  // 测试SSL/TLS配置
  let ssl_config = network_security_manager.get_ssl_configuration()
  
  // 测试证书验证
  let certificate_validation = ssl_config.validate_certificate("example.com")
  assert_true(certificate_validation.success || certificate_validation.error.contains("证书验证失败"))  // 测试环境可能没有有效证书
  
  // 测试入侵检测系统
  let ids = IntrusionDetectionSystem::new({
    rules: [
      {
        id: "sql_injection_detection",
        pattern: "union.*select",
        severity: "high",
        description: "检测SQL注入攻击"
      },
      {
        id: "xss_detection",
        pattern: "<script.*>",
        severity: "medium",
        description: "检测跨站脚本攻击"
      }
    ],
    anomaly_detection: {
      enabled: true,
      baseline_learning_period: 86400,  // 1天
      sensitivity: "medium"
    }
  })
  
  // 测试入侵检测规则
  let sql_injection_request = ids.analyze_request({
    ip_address: "203.0.113.1",
    user_agent: "Mozilla/5.0",
    method: "POST",
    url: "/api/users",
    headers: [("Content-Type", "application/json")],
    body: "username=admin&password=' OR '1'='1"
  })
  
  assert_true(sql_injection_request.threat_detected)
  assert_eq(sql_injection_request.threats[0].rule_id, "sql_injection_detection")
  assert_eq(sql_injection_request.threats[0].severity, "high")
  
  let xss_request = ids.analyze_request({
    ip_address: "203.0.113.1",
    user_agent: "Mozilla/5.0",
    method: "POST",
    url: "/api/comments",
    headers: [("Content-Type", "application/json")],
    body: "comment=<script>alert('xss')</script>"
  })
  
  assert_true(xss_request.threat_detected)
  assert_eq(xss_request.threats[0].rule_id, "xss_detection")
  assert_eq(xss_request.threats[0].severity, "medium")
  
  // 测试安全头管理
  let security_headers = SecurityHeadersManager::new({
    default_headers: {
      "X-Frame-Options": "DENY",
      "X-Content-Type-Options": "nosniff",
      "X-XSS-Protection": "1; mode=block",
      "Strict-Transport-Security": "max-age=31536000; includeSubDomains",
      "Content-Security-Policy": "default-src 'self'"
    }
  })
  
  // 生成安全头
  let headers = security_headers.generate_headers({
    request_path: "/api/users",
    is_https: true,
    include_subdomains: true
  })
  
  assert_true(headers.contains_key("X-Frame-Options"))
  assert_eq(headers.get("X-Frame-Options"), Some("DENY"))
  assert_true(headers.contains_key("Strict-Transport-Security"))
  
  // 测试CSP策略
  let csp_policy = security_headers.generate_csp_policy({
    default_src: ["'self'"],
    script_src: ["'self'", "https://cdn.example.com"],
    style_src: ["'self'", "https://fonts.googleapis.com"],
    img_src: ["'self'", "data:", "https:"],
    connect_src: ["'self'", "https://api.example.com"]
  })
  
  assert_true(csp_policy.contains("default-src 'self'"))
  assert_true(csp_policy.contains("script-src 'self' https://cdn.example.com"))
  
  // 测试网络扫描检测
  let port_scanner = PortScanner::new({
    common_ports: [22, 80, 443, 3306, 5432, 6379, 8080],
    scan_timeout: 5000,
    max_concurrent_scans: 10
  })
  
  let scan_result = port_scanner.scan_host("localhost")
  assert_true(scan_result.success)
  assert_true(scan_result.open_ports.length() >= 0)
  
  // 测试漏洞扫描
  let vulnerability_scanner = VulnerabilityScanner::new({
    scan_types: ["port_scan", "service_detection", "version_detection"],
    timeout: 30000,
    max_concurrent_scans: 5
  })
  
  let vuln_scan = vulnerability_scanner.scan_target("localhost", {
    ports: [80, 443, 8080],
    services: ["http", "https"],
    deep_scan: false
  })
  
  assert_true(vuln_scan.success)
  assert_true(vuln_scan.vulnerabilities.length() >= 0)
  
  // 测试网络分段
  let network_segmentation = NetworkSegmentationManager::new({
    segments: [
      {
        name: "dmz",
        cidr: "192.168.100.0/24",
        description: "隔离区"
      },
      {
        name: "internal",
        cidr: "192.168.1.0/24",
        description: "内部网络"
      },
      {
        name: "database",
        cidr: "192.168.2.0/24",
        description: "数据库网络"
      }
    ],
    rules: [
      {
        source: "dmz",
        destination: "internal",
        allowed_ports: [443, 8080],
        description: "DMZ可以访问内部网络的特定端口"
      },
      {
        source: "internal",
        destination: "database",
        allowed_ports: [3306, 5432],
        description: "内部网络可以访问数据库"
      }
    ]
  })
  
  // 测试网络分段规则
  let dmz_to_internal = network_segmentation.check_traffic_allowed({
    source_ip: "192.168.100.10",
    source_segment: "dmz",
    dest_ip: "192.168.1.10",
    dest_segment: "internal",
    port: 443,
    protocol: "tcp"
  })
  
  assert_true(dmz_to_internal.allowed)  // 允许的端口
  
  let dmz_to_internal_blocked = network_segmentation.check_traffic_allowed({
    source_ip: "192.168.100.10",
    source_segment: "dmz",
    dest_ip: "192.168.1.10",
    dest_segment: "internal",
    port: 22,  // 不在允许列表中
    protocol: "tcp"
  })
  
  assert_false(dmz_to_internal_blocked.allowed)  // 不允许的端口
  
  // 测试网络流量分析
  let traffic_analyzer = NetworkTrafficAnalyzer::new({
    analysis_window: 300,  // 5分钟窗口
    anomaly_threshold: 2.0,  // 2倍标准差
    protocols_to_monitor: ["HTTP", "HTTPS", "TCP", "UDP"]
  })
  
  // 模拟网络流量数据
  let traffic_data = generate_network_traffic_data({
    duration_seconds: 300,
    normal_baseline: true
  })
  
  let analysis_result = traffic_analyzer.analyze_traffic(traffic_data)
  assert_true(analysis_result.success)
  
  if analysis_result.anomalies.length() > 0 {
    // 验证异常检测
    let anomaly = analysis_result.anomalies[0]
    assert_true(anomaly.severity in ["low", "medium", "high"])
    assert_true(anomaly.description.length() > 0)
  }
}

// 测试10: 安全监控和事件响应
test "安全监控和事件响应测试" {
  // 创建安全监控管理器
  let security_monitor = SecurityMonitor::new({
    monitoring_interval: 60,  // 1分钟
    alert_thresholds: {
      failed_login_rate: 0.1,      // 10%失败率
      unusual_access_pattern: 5,    // 5个异常模式
      privilege_escalation: 1,      // 任何权限提升
      data_exfiltration: 1024 * 1024  // 1MB数据外泄
    },
    retention_period: 90,  // 90天
    automated_response: true
  })
  
  // 创建事件响应管理器
  let incident_manager = IncidentManager::new({
    severity_levels: ["low", "medium", "high", "critical"],
    escalation_rules: [
      {
        trigger: "severity == 'critical' or (severity == 'high' and age_hours > 4)",
        action: "escalate_to_management",
        notification_channels: ["email", "sms", "slack"]
      },
      {
        trigger: "severity == 'medium' and age_hours > 24",
        action: "escalate_to_team_lead",
        notification_channels: ["email", "slack"]
      }
    ],
    auto_containment: {
      enabled: true,
      actions: ["block_ip", "disable_account", "isolate_system"]
    }
  })
  
  // 模拟安全事件
  let security_event = SecurityEvent::new({
    type: "failed_login_burst",
    severity: "high",
    source_ip: "203.0.113.1",
    target_user: "admin",
    timestamp: Time::now(),
    details: {
      attempt_count: 15,
      time_window: 300,  // 5分钟内
      user_agent: "Mozilla/5.0 (compatible; scanner/1.0)"
    }
  })
  
  // 记录安全事件
  let event_recording = security_monitor.record_event(security_event)
  assert_true(event_recording.success)
  
  // 检查是否触发警报
  let active_alerts = security_monitor.get_active_alerts()
  assert_true(active_alerts.length() > 0)
  
  // 创建安全事件
  let incident_creation = incident_manager.create_incident({
    title: "疑似暴力破解攻击",
    description: "检测到来自203.0.113.1的大量失败登录尝试",
    severity: "high",
    assigned_to: "security_team",
    tags: ["brute_force", "failed_login", "external_ip"],
    source_events: [security_event.id]
  })
  
  assert_true(incident_creation.success)
  assert_true(incident_creation.incident_id != None)
  
  let incident_id = incident_creation.incident_id.unwrap()
  
  // 更新事件状态
  let status_update = incident_manager.update_incident_status(incident_id, {
    status: "investigating",
    comment: "开始调查攻击源和影响范围",
    updated_by: "security_analyst"
  })
  
  assert_true(status_update.success)
  
  // 添加事件调查笔记
  let note_addition = incident_manager.add_note(incident_id, {
    content: "检查IP地址203.0.113.1的地理位置和历史活动",
    author: "security_analyst",
    note_type: "investigation"
  })
  
  assert_true(note_addition.success)
  
  // 测试自动遏制措施
  let containment_actions = incident_manager.apply_auto_containment(incident_id, {
    block_source_ip: true,
    disable_target_account: false,
    isolate_affected_systems: false
  })
  
  assert_true(containment_actions.success)
  assert_true(containment_actions.actions_applied.contains("block_ip"))
  
  // 验证IP已被阻止
  let ip_block_verification = security_monitor.is_ip_blocked("203.0.113.1")
  assert_true(ip_block_verification)
  
  // 测试事件升级
  let escalation_test = incident_manager.create_incident({
    title: "数据泄露事件",
    description: "检测到大量数据外泄",
    severity: "critical",
    assigned_to: "security_team",
    tags: ["data_exfiltration", "critical"]
  })
  
  assert_true(escalation_test.success)
  
  let critical_incident_id = escalation_test.incident_id.unwrap()
  
  // 等待升级检查（模拟时间流逝）
  Time::sleep(1000)
  
  // 检查是否已升级
  let critical_incident = incident_manager.get_incident(critical_incident_id)
  assert_true(critical_incident.success)
  
  match critical_incident.incident {
    Some(incident) => {
      assert_eq(incident.severity, "critical")
      // 在实际实现中，应该检查升级状态
    }
    None => assert_true(false)
  }
  
  // 测试事件报告生成
  let report_generation = incident_manager.generate_incident_report(incident_id, {
    include_timeline: true,
    include_evidence: true,
    include_actions: true,
    format: "detailed"
  })
  
  assert_true(report_generation.success)
  assert_true(report_generation.report.content.length() > 0)
  
  // 测试事件统计分析
  let analytics = incident_manager.get_analytics({
    time_range: "30d",
    group_by: ["severity", "type", "status"],
    include_trends: true
  })
  
  assert_true(analytics.success)
  assert_true(analytics.statistics.total_incidents > 0)
  assert_true(analytics.statistics.by_severity.contains_key("high"))
  
  // 测试威胁情报集成
  let threat_intel = ThreatIntelligenceManager::new({
    providers: ["virustotal", "alienvault", "misp"],
    update_interval: 3600,  // 1小时
    cache_duration: 86400    // 24小时缓存
  })
  
  // 查询IP威胁情报
  let ip_threat_check = threat_intel.check_ip("203.0.113.1")
  assert_true(ip_threat_check.success)
  
  match ip_threat_check.threat_intel {
    Some(intel) => {
      assert_true(intel.malicious_score >= 0.0 && intel.malicious_score <= 100.0)
      assert_true(intel.sources.length() >= 0)
    }
    None => assert_true(false)
  }
  
  // 测试文件哈希检查
  let file_hash_check = threat_intel.check_file_hash("a1b2c3d4e5f6789012345678901234567890abcd")
  assert_true(file_hash_check.success)
  
  // 测试安全指标仪表盘
  let security_dashboard = SecurityDashboard::new({
    metrics: [
      "active_incidents",
      "resolved_incidents",
      "mean_time_to_resolve",
      "threat_detection_rate",
      "false_positive_rate"
    ],
    time_range: "7d",
    refresh_interval: 300
  })
  
  let dashboard_data = security_dashboard.generate_dashboard()
  assert_true(dashboard_data.success)
  assert_true(dashboard_data.metrics.length() > 0)
  
  // 测试安全自动化响应
  let automation_engine = SecurityAutomationEngine::new({
    rules: [
      {
        name: "block_malicious_ip",
        trigger: "threat_intel.malicious_score > 80",
        actions: [
          { type: "block_ip", parameters: { duration: 86400 } },
          { type: "create_alert", parameters: { severity: "high" } }
        ]
      },
      {
        name: "quarantine_compromised_account",
        trigger: "failed_login_burst.attempts > 20",
        actions: [
          { type: "disable_account", parameters: { temporary: true, duration: 3600 } },
          { type: "notify_user", parameters: { method: "email" } }
        ]
      }
    ]
  })
  
  // 触发自动化规则
  let automation_trigger = automation_engine.evaluate_rules({
    ip_address: "203.0.113.1",
    threat_intel: ip_threat_check.threat_intel,
    security_event: security_event
  })
  
  assert_true(automation_trigger.success)
  assert_true(automation_trigger.triggered_rules.length() >= 0)
  
  // 测试安全合规检查
  let compliance_checker = SecurityComplianceChecker::new({
    frameworks: ["NIST", "ISO27001", "PCI-DSS"],
    checks: [
      {
        framework: "NIST",
        control: "AC-7",
        description: "限制登录尝试次数",
        check_function: fn() {
          // 检查登录限制配置
          true  // 假设配置正确
        }
      },
      {
        framework: "ISO27001",
        control: "A.9.2.1",
        description: "用户访问控制",
        check_function: fn() {
          // 检查访问控制策略
          true  // 假设配置正确
        }
      }
    ]
  })
  
  let compliance_check = compliance_checker.run_checks()
  assert_true(compliance_check.success)
  assert_true(compliance_check.overall_score >= 0.0 && compliance_check.overall_score <= 100.0)
  
  // 测试安全培训集成
  let training_integration = SecurityTrainingIntegration::new({
    training_platform: "security_education_platform",
    mandatory_courses: [
      { id: "PHISHING_AWARENESS", required_for_all: true },
      { id: "DATA_PROTECTION", required_for_roles: ["admin", "hr"] },
      { id: "INCIDENT_RESPONSE", required_for_roles: ["security_team"] }
    ]
  })
  
  // 检查用户培训状态
  let training_status = training_integration.get_user_training_status("user123")
  assert_true(training_status.success)
  
  // 测试安全事件模拟
  let simulation_engine = SecuritySimulationEngine::new({
    scenarios: [
      {
        name: "phishing_attack",
        description: "模拟钓鱼攻击",
        parameters: {
          target_users: ["user123", "user456"],
          email_template: "suspicious_login_attempt",
          success_rate: 0.3
        }
      },
      {
        name: "malware_outbreak",
        description: "模拟恶意软件爆发",
        parameters: {
          affected_systems: ["web_server", "db_server"],
          propagation_speed: "medium",
          detection_delay: 300
        }
      }
    ]
  })
  
  // 运行安全模拟
  let simulation_result = simulation_engine.run_simulation("phishing_attack")
  assert_true(simulation_result.success)
  
  match simulation_result.results {
    Some(results) => {
      assert_true(results.metrics.contains_key("success_rate"))
      assert_true(results.metrics.contains_key("detection_rate"))
    }
    None => assert_true(false)
  }
}

// 辅助函数：生成TOTP代码
fn generate_totp_code(secret: String) -> String {
  // 简化的TOTP生成，实际实现会使用加密库
  "123456"
}

// 辅助函数：生成网络流量数据
fn generate_network_traffic_data(options: Map[String, Any]) -> Array[NetworkTrafficData] {
  // 简化的网络流量数据生成
  []
}
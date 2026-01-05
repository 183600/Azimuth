// Azimuth 遥测数据安全性和隐私测试
// 测试遥测数据的安全性和隐私保护功能

// 测试1: 数据加密和解密
test "数据加密和解密测试" {
  // 创建安全管理器
  let security_manager = SecurityManager::new()
  
  // 配置加密算法
  let encryption_config = {
    algorithm: "AES-256-GCM",
    key_derivation: "PBKDF2",
    key_iterations: 100000,
    salt_length: 32,
    iv_length: 12,
    tag_length: 16
  }
  
  // 创建加密密钥
  let master_key = SecurityManager::generate_key(security_manager, {
    algorithm: "AES-256",
    source: "random"
  })
  
  assert_true(master_key.length() == 32)  // 256位密钥
  
  // 创建测试数据
  let sensitive_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    operation_name: "user.authentication",
    service_name: "auth-service",
    user_id: "user-98765",
    email: "user@example.com",
    credit_card: "4532-1234-5678-9012",
    ssn: "123-45-6789",
    attributes: [
      ("user.id", "user-98765"),
      ("user.email", "user@example.com"),
      ("payment.method", "credit_card"),
      ("payment.last4", "9012"),
      ("session.token", "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...")
    ]
  }
  
  // 测试字段级加密
  let field_encryption_config = {
    fields: ["user_id", "email", "credit_card", "ssn"],
    encryption: encryption_config,
    key: master_key
  }
  
  let encrypted_data = SecurityManager::encrypt_fields(security_manager, sensitive_data, field_encryption_config)
  
  // 验证字段加密结果
  assert_eq(encrypted_data.trace_id, "trace-12345")  // 未加密字段保持不变
  assert_eq(encrypted_data.span_id, "span-67890")    // 未加密字段保持不变
  assert_eq(encrypted_data.operation_name, "user.authentication")  // 未加密字段保持不变
  assert_eq(encrypted_data.service_name, "auth-service")  // 未加密字段保持不变
  
  // 加密字段应该不再是原始值
  assert_true(encrypted_data.user_id != "user-98765")
  assert_true(encrypted_data.email != "user@example.com")
  assert_true(encrypted_data.credit_card != "4532-1234-5678-9012")
  assert_true(encrypted_data.ssn != "123-45-6789")
  
  // 加密字段应该有加密标记
  assert_true(encrypted_data.user_id.starts_with("enc:"))
  assert_true(encrypted_data.email.starts_with("enc:"))
  assert_true(encrypted_data.credit_card.starts_with("enc:"))
  assert_true(encrypted_data.ssn.starts_with("enc:"))
  
  // 测试字段解密
  let decrypted_data = SecurityManager::decrypt_fields(security_manager, encrypted_data, field_encryption_config)
  
  // 验证解密结果
  assert_eq(decrypted_data.user_id, "user-98765")
  assert_eq(decrypted_data.email, "user@example.com")
  assert_eq(decrypted_data.credit_card, "4532-1234-5678-9012")
  assert_eq(decrypted_data.ssn, "123-45-6789")
  
  // 测试属性加密
  let attribute_encryption_config = {
    patterns: [
      {
        pattern: "user.*",
        encryption: encryption_config,
        key: master_key
      },
      {
        pattern: "payment.*",
        encryption: encryption_config,
        key: master_key
      },
      {
        pattern: "session.*",
        encryption: encryption_config,
        key: master_key
      }
    ]
  }
  
  let encrypted_attributes = SecurityManager::encrypt_attributes(security_manager, sensitive_data.attributes, attribute_encryption_config)
  
  // 验证属性加密结果
  for attr in encrypted_attributes {
    let (key, value) = attr
    
    if key.starts_with("user.") or key.starts_with("payment.") or key.starts_with("session.") {
      assert_true(value.starts_with("enc:"))
    } else {
      assert_false(value.starts_with("enc:"))
    }
  }
  
  // 测试属性解密
  let decrypted_attributes = SecurityManager::decrypt_attributes(security_manager, encrypted_attributes, attribute_encryption_config)
  
  // 验证属性解密结果
  assert_eq(decrypted_attributes.length(), sensitive_data.attributes.length())
  
  for i in 0..decrypted_attributes.length() {
    let (dec_key, dec_value) = decrypted_attributes[i]
    let (orig_key, orig_value) = sensitive_data.attributes[i]
    
    assert_eq(dec_key, orig_key)
    assert_eq(dec_value, orig_value)
  }
  
  // 测试完整数据加密
  let full_encryption_config = {
    encryption: encryption_config,
    key: master_key,
    compress: true
  }
  
  let fully_encrypted = SecurityManager::encrypt_data(security_manager, sensitive_data, full_encryption_config)
  
  // 验证完整加密结果
  assert_true(fully_encrypted.starts_with("enc:"))
  assert_true(fully_encrypted.length() > 0)
  
  // 测试完整数据解密
  let fully_decrypted = SecurityManager::decrypt_data(security_manager, fully_encrypted, full_encryption_config)
  
  // 验证完整解密结果
  assert_eq(fully_decrypted.trace_id, sensitive_data.trace_id)
  assert_eq(fully_decrypted.span_id, sensitive_data.span_id)
  assert_eq(fully_decrypted.user_id, sensitive_data.user_id)
  assert_eq(fully_decrypted.email, sensitive_data.email)
  assert_eq(fully_decrypted.credit_card, sensitive_data.credit_card)
  assert_eq(fully_decrypted.ssn, sensitive_data.ssn)
  
  // 测试加密性能
  let performance_test_data = []
  
  for i in 0..1000 {
    let data = {
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      user_id: "user-" + i.to_string(),
      email: "user" + i.to_string() + "@example.com",
      credit_card: "4532-" + (1000 + i).to_string() + "-5678-9012"
    }
    
    performance_test_data = performance_test_data.push(data)
  }
  
  let encryption_start_time = Time::now()
  let encrypted_performance_data = SecurityManager::encrypt_batch(security_manager, performance_test_data, field_encryption_config)
  let encryption_time = Time::now() - encryption_start_time
  
  let decryption_start_time = Time::now()
  let decrypted_performance_data = SecurityManager::decrypt_batch(security_manager, encrypted_performance_data, field_encryption_config)
  let decryption_time = Time::now() - decryption_start_time
  
  // 验证性能
  assert_true(encryption_time < 10000)  // 加密应在10秒内完成
  assert_true(decryption_time < 10000)  // 解密应在10秒内完成
  
  // 验证批量处理结果
  assert_eq(encrypted_performance_data.length(), performance_test_data.length())
  assert_eq(decrypted_performance_data.length(), performance_test_data.length())
  
  // 验证批量解密结果正确性
  for i in 0..decrypted_performance_data.length() {
    let decrypted = decrypted_performance_data[i]
    let original = performance_test_data[i]
    
    assert_eq(decrypted.user_id, original.user_id)
    assert_eq(decrypted.email, original.email)
    assert_eq(decrypted.credit_card, original.credit_card)
  }
}

// 测试2: 数据脱敏和匿名化
test "数据脱敏和匿名化测试" {
  // 创建隐私管理器
  let privacy_manager = PrivacyManager::new()
  
  // 配置脱敏规则
  let masking_rules = [
    {
      field: "email",
      type: "email",
      strategy: "partial",
      preserve_domain: true
    },
    {
      field: "credit_card",
      type: "credit_card",
      strategy: "keep_last4"
    },
    {
      field: "ssn",
      type: "ssn",
      strategy: "full"
    },
    {
      field: "phone",
      type: "phone",
      strategy: "partial",
      preserve_area_code: true
    },
    {
      field: "ip_address",
      type: "ip_address",
      strategy: "hash"
    }
  ]
  
  // 创建测试数据
  let sensitive_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    operation_name: "user.profile.update",
    service_name: "user-service",
    user_id: "user-98765",
    email: "john.doe@example.com",
    credit_card: "4532123456789012",
    ssn: "123456789",
    phone: "+1-555-123-4567",
    ip_address: "192.168.1.100",
    attributes: [
      ("user.email", "jane.smith@company.org"),
      ("payment.card", "6011111111111117"),
      ("user.phone", "+44-20-7946-0958"),
      ("client.ip", "10.0.0.50")
    ]
  }
  
  // 应用脱敏规则
  let masked_data = PrivacyManager::apply_masking(privacy_manager, sensitive_data, masking_rules)
  
  // 验证脱敏结果
  assert_eq(masked_data.trace_id, "trace-12345")  // 非敏感字段保持不变
  assert_eq(masked_data.span_id, "span-67890")    // 非敏感字段保持不变
  assert_eq(masked_data.operation_name, "user.profile.update")  // 非敏感字段保持不变
  assert_eq(masked_data.service_name, "user-service")  // 非敏感字段保持不变
  assert_eq(masked_data.user_id, "user-98765")  // 非敏感字段保持不变
  
  // 验证邮箱脱敏
  assert_eq(masked_data.email, "j***.***@example.com")  // 部分脱敏，保留域名
  
  // 验证信用卡脱敏
  assert_eq(masked_data.credit_card, "************9012")  // 只保留最后4位
  
  // 验证SSN脱敏
  assert_eq(masked_data.ssn, "***-**-****")  // 完全脱敏
  
  // 验证电话号码脱敏
  assert_eq(masked_data.phone, "+1-***-***-4567")  // 部分脱敏，保留区号
  
  // 验证IP地址脱敏
  assert_true(masked_data.ip_address != "192.168.1.100")  // 应该被哈希
  assert_true(masked_data.ip_address.length() == 64)  // SHA-256哈希长度
  
  // 验证属性脱敏
  let masked_attributes = masked_data.attributes
  
  for attr in masked_attributes {
    let (key, value) = attr
    
    if key == "user.email" {
      assert_eq(value, "j***.*****@company.org")  // 部分脱敏，保留域名
    } else if key == "payment.card" {
      assert_eq(value, "************1117")  // 只保留最后4位
    } else if key == "user.phone" {
      assert_eq(value, "+44-**-****-0958")  // 部分脱敏，保留区号
    } else if key == "client.ip" {
      assert_true(value != "10.0.0.50")  // 应该被哈希
    }
  }
  
  // 测试匿名化
  let anonymization_config = {
    strategy: "pseudonymization",
    fields: ["user_id", "trace_id"],
    salt: "random_salt_value"
  }
  
  let anonymized_data = PrivacyManager::anonymize(privacy_manager, masked_data, anonymization_config)
  
  // 验证匿名化结果
  assert_true(anonymized_data.user_id != "user-98765")  // 应该被假名化
  assert_true(anonymized_data.trace_id != "trace-12345")  // 应该被假名化
  
  // 假名化应该是一致的（相同输入产生相同输出）
  let re_anonymized_data = PrivacyManager::anonymize(privacy_manager, masked_data, anonymization_config)
  assert_eq(anonymized_data.user_id, re_anonymized_data.user_id)
  assert_eq(anonymized_data.trace_id, re_anonymized_data.trace_id)
  
  // 但不同的盐应该产生不同的结果
  let different_salt_config = { anonymization_config | salt: "different_salt_value" }
  let different_anonymized_data = PrivacyManager::anonymize(privacy_manager, masked_data, different_salt_config)
  assert_true(anonymized_data.user_id != different_anonymized_data.user_id)
  
  // 测试差分隐私
  let differential_privacy_config = {
    epsilon: 1.0,  // 隐私预算
    mechanism: "laplace",
    fields: ["duration", "response_size"]
  }
  
  let telemetry_data = {
    trace_id: "trace-12345",
    operation_name: "api.request",
    duration: 250,  // 毫秒
    response_size: 1024,  // 字节
    attributes: []
  }
  
  let private_data = PrivacyManager::apply_differential_privacy(privacy_manager, telemetry_data, differential_privacy_config)
  
  // 验证差分隐私结果
  assert_true(private_data.duration != 250)  // 应该添加噪声
  assert_true(private_data.response_size != 1024)  // 应该添加噪声
  
  // 但噪声不应该太大（应该在原始值的合理范围内）
  assert_true(private_data.duration > 0 and private_data.duration < 500)
  assert_true(private_data.response_size > 0 and private_data.response_size < 2000)
  
  // 相同输入可能产生不同输出（由于随机噪声）
  let private_data2 = PrivacyManager::apply_differential_privacy(privacy_manager, telemetry_data, differential_privacy_config)
  assert_true(private_data.duration != private_data2.duration or private_data.response_size != private_data2.response_size)
  
  // 测试k-匿名化
  let k_anonymity_config = {
    k: 5,  // 至少5个相同的记录
    quasi_identifiers: ["age", "zip_code", "gender"],
    sensitive_fields: ["diagnosis"]
  }
  
  let medical_records = [
    {
      age: 25,
      zip_code: "12345",
      gender: "F",
      diagnosis: "Hypertension"
    },
    {
      age: 26,
      zip_code: "12345",
      gender: "F",
      diagnosis: "Diabetes"
    },
    {
      age: 27,
      zip_code: "12345",
      gender: "F",
      diagnosis: "Asthma"
    },
    {
      age: 28,
      zip_code: "12345",
      gender: "F",
      diagnosis: "Hypertension"
    },
    {
      age: 29,
      zip_code: "12345",
      gender: "F",
      diagnosis: "Diabetes"
    },
    {
      age: 35,
      zip_code: "67890",
      gender: "M",
      diagnosis: "Hypertension"
    }
  ]
  
  let k_anonymized_records = PrivacyManager::apply_k_anonymity(privacy_manager, medical_records, k_anonymity_config)
  
  // 验证k-匿名化结果
  for record in k_anonymized_records {
    // 每个等价类应该至少有k个记录
    assert_true(record.equivalence_class_size >= 5)
    
    // 准标识符应该被泛化
    if record.age == 25 {
      assert_eq(record.age_range, "25-29")  // 年龄被泛化为范围
      assert_eq(record.zip_code, "12***")    // 邮编被泛化
    }
    
    // 敏感字段应该保持不变
    assert_true(record.diagnosis == "Hypertension" or 
                record.diagnosis == "Diabetes" or 
                record.diagnosis == "Asthma")
  }
}

// 测试3: 访问控制和权限管理
test "访问控制和权限管理测试" {
  // 创建访问控制管理器
  let access_manager = AccessManager::new()
  
  // 创建角色
  let admin_role = AccessManager::create_role(access_manager, {
    name: "admin",
    description: "Full system access",
    permissions: [
      "telemetry:read",
      "telemetry:write",
      "telemetry:delete",
      "telemetry:export",
      "telemetry:manage",
      "user:manage",
      "config:manage"
    ]
  })
  
  let analyst_role = AccessManager::create_role(access_manager, {
    name: "analyst",
    description: "Read-only access to telemetry data",
    permissions: [
      "telemetry:read",
      "telemetry:export"
    ]
  })
  
  let service_role = AccessManager::create_role(access_manager, {
    name: "service",
    description: "Service account for telemetry ingestion",
    permissions: [
      "telemetry:write"
    ]
  })
  
  // 创建用户
  let admin_user = AccessManager::create_user(access_manager, {
    username: "admin",
    email: "admin@example.com",
    roles: [admin_role.id]
  })
  
  let analyst_user = AccessManager::create_user(access_manager, {
    username: "analyst",
    email: "analyst@example.com",
    roles: [analyst_role.id]
  })
  
  let service_user = AccessManager::create_user(access_manager, {
    username: "telemetry-service",
    email: "service@example.com",
    roles: [service_role.id]
  })
  
  // 测试权限检查
  let admin_permissions = AccessManager::get_user_permissions(access_manager, admin_user.id)
  
  // 管理员应该有所有权限
  assert_true(admin_permissions.contains("telemetry:read"))
  assert_true(admin_permissions.contains("telemetry:write"))
  assert_true(admin_permissions.contains("telemetry:delete"))
  assert_true(admin_permissions.contains("telemetry:export"))
  assert_true(admin_permissions.contains("telemetry:manage"))
  assert_true(admin_permissions.contains("user:manage"))
  assert_true(admin_permissions.contains("config:manage"))
  
  let analyst_permissions = AccessManager::get_user_permissions(access_manager, analyst_user.id)
  
  // 分析师应该只有读取和导出权限
  assert_true(analyst_permissions.contains("telemetry:read"))
  assert_true(analyst_permissions.contains("telemetry:export"))
  assert_false(analyst_permissions.contains("telemetry:write"))
  assert_false(analyst_permissions.contains("telemetry:delete"))
  assert_false(analyst_permissions.contains("telemetry:manage"))
  
  let service_permissions = AccessManager::get_user_permissions(access_manager, service_user.id)
  
  // 服务账户应该只有写入权限
  assert_true(service_permissions.contains("telemetry:write"))
  assert_false(service_permissions.contains("telemetry:read"))
  assert_false(service_permissions.contains("telemetry:export"))
  assert_false(service_permissions.contains("telemetry:delete"))
  
  // 测试资源级访问控制
  let telemetry_data = {
    id: "telemetry-12345",
    type: "span",
    service_name: "payment-service",
    owner: "admin",
    sensitivity: "high",
    tags: ["pii", "financial"]
  }
  
  // 创建访问策略
  let owner_policy = AccessManager::create_policy(access_manager, {
    name: "owner_access",
    effect: "allow",
    principal: { type: "user", id: admin_user.id },
    action: "*",
    resource: { type: "telemetry", id: "*" },
    condition: { field: "owner", operator: "equals", value: admin_user.username }
  })
  
  let service_policy = AccessManager::create_policy(access_manager, {
    name: "service_access",
    effect: "allow",
    principal: { type: "role", id: service_role.id },
    action: "write",
    resource: { type: "telemetry", id: "*" },
    condition: { field: "sensitivity", operator: "not_equals", value: "high" }
  })
  
  let analyst_policy = AccessManager::create_policy(access_manager, {
    name: "analyst_access",
    effect: "allow",
    principal: { type: "role", id: analyst_role.id },
    action: "read",
    resource: { type: "telemetry", id: "*" },
    condition: { field: "tags", operator: "not_contains", value: "pii" }
  })
  
  // 测试访问决策
  let admin_access = AccessManager::check_access(access_manager, {
    principal: { type: "user", id: admin_user.id },
    action: "delete",
    resource: { type: "telemetry", id: telemetry_data.id },
    context: telemetry_data
  })
  
  assert_true(admin_access.allowed)  // 管理员应该能够删除自己的数据
  
  let analyst_access = AccessManager::check_access(access_manager, {
    principal: { type: "user", id: analyst_user.id },
    action: "read",
    resource: { type: "telemetry", id: telemetry_data.id },
    context: telemetry_data
  })
  
  assert_false(analyst_access.allowed)  // 分析师不应该能读取包含PII的数据
  
  let service_access = AccessManager::check_access(access_manager, {
    principal: { type: "user", id: service_user.id },
    action: "write",
    resource: { type: "telemetry", id: telemetry_data.id },
    context: telemetry_data
  })
  
  assert_false(service_access.allowed)  // 服务账户不应该能写入高敏感度数据
  
  // 测试非敏感数据的服务访问
  let non_sensitive_data = { telemetry_data | sensitivity: "normal", tags: ["performance"] }
  
  let service_access_normal = AccessManager::check_access(access_manager, {
    principal: { type: "user", id: service_user.id },
    action: "write",
    resource: { type: "telemetry", id: non_sensitive_data.id },
    context: non_sensitive_data
  })
  
  assert_true(service_access_normal.allowed)  // 服务账户应该能写入非敏感数据
  
  // 测试基于时间的访问控制
  let time_based_policy = AccessManager::create_policy(access_manager, {
    name: "business_hours_access",
    effect: "allow",
    principal: { type: "role", id: analyst_role.id },
    action: "export",
    resource: { type: "telemetry", id: "*" },
    condition: {
      type: "time_range",
      start_hour: 9,
      end_hour: 17,
      timezone: "UTC",
      weekdays_only: true
    }
  })
  
  // 模拟工作时间内的访问
  let business_hours_context = {
    current_time: "2022-01-01T14:00:00Z",  // 工作日下午2点
    current_day: "saturday"  // 周末
  }
  
  let business_hours_access = AccessManager::check_access(access_manager, {
    principal: { type: "user", id: analyst_user.id },
    action: "export",
    resource: { type: "telemetry", id: "*" },
    context: business_hours_context
  })
  
  assert_false(business_hours_access.allowed)  // 周末即使在工作时间也不应该允许访问
  
  // 模拟工作日的访问
  let weekday_context = { business_hours_context | current_day: "monday" }
  
  let weekday_access = AccessManager::check_access(access_manager, {
    principal: { type: "user", id: analyst_user.id },
    action: "export",
    resource: { type: "telemetry", id: "*" },
    context: weekday_context
  })
  
  assert_true(weekday_access.allowed)  // 工作日工作时间应该允许访问
  
  // 测试访问审计
  let audit_log = AccessManager::get_audit_log(access_manager, {
    start_time: "2022-01-01T00:00:00Z",
    end_time: "2022-01-01T23:59:59Z",
    principal_id: analyst_user.id
  })
  
  // 验证审计日志
  assert_true(audit_log.length() > 0)
  
  let denied_access = audit_log.find(fn(entry) { 
    entry.action == "read" and 
    entry.resource_id == telemetry_data.id and 
    entry.result == "denied" 
  })
  assert_true(denied_access != None)
  
  let allowed_access = audit_log.find(fn(entry) { 
    entry.action == "export" and 
    entry.result == "allowed" 
  })
  assert_true(allowed_access != None)
}

// 测试4: 数据完整性和签名
test "数据完整性和签名测试" {
  // 创建完整性管理器
  let integrity_manager = IntegrityManager::new()
  
  // 配置签名算法
  let signature_config = {
    algorithm: "ECDSA",
    curve: "P-256",
    hash_function: "SHA-256"
  }
  
  // 生成密钥对
  let key_pair = IntegrityManager::generate_key_pair(integrity_manager, signature_config)
  
  assert_true(key_pair.private_key.length() > 0)
  assert_true(key_pair.public_key.length() > 0)
  
  // 创建测试数据
  let telemetry_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    operation_name: "payment.process",
    service_name: "payment-service",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    attributes: [
      ("payment.amount", "99.99"),
      ("payment.currency", "USD"),
      ("payment.method", "credit_card"),
      ("merchant.id", "merchant-12345")
    ]
  }
  
  // 计算数据哈希
  let data_hash = IntegrityManager::calculate_hash(integrity_manager, telemetry_data, {
    algorithm: "SHA-256",
    exclude_fields: ["signature"]  // 排除签名字段本身
  })
  
  assert_true(data_hash.length() == 64)  // SHA-256哈希长度
  
  // 创建数字签名
  let signature = IntegrityManager::sign_data(integrity_manager, data_hash, key_pair.private_key, signature_config)
  
  assert_true(signature.length() > 0)
  
  // 验证签名
  let is_valid = IntegrityManager::verify_signature(integrity_manager, data_hash, signature, key_pair.public_key, signature_config)
  assert_true(is_valid)
  
  // 测试篡改检测
  let tampered_data = { telemetry_data | attributes: [("payment.amount", "199.99")] }
  
  let tampered_hash = IntegrityManager::calculate_hash(integrity_manager, tampered_data, {
    algorithm: "SHA-256",
    exclude_fields: ["signature"]
  })
  
  let is_tampered_valid = IntegrityManager::verify_signature(integrity_manager, tampered_hash, signature, key_pair.public_key, signature_config)
  assert_false(is_tampered_valid)  // 篡改后的数据签名验证应该失败
  
  // 测试数据链完整性
  let data_chain = []
  let previous_hash = "0"  // 创世区块哈希
  
  for i in 0..5 {
    let block_data = {
      index: i,
      timestamp: 1640995200 + i * 1000,
      data: "Block " + i.to_string() + " data",
      previous_hash: previous_hash
    }
    
    let block_hash = IntegrityManager::calculate_hash(integrity_manager, block_data, {
      algorithm: "SHA-256"
    })
    
    let block_signature = IntegrityManager::sign_data(integrity_manager, block_hash, key_pair.private_key, signature_config)
    
    let block = {
      data: block_data,
      hash: block_hash,
      signature: block_signature
    }
    
    data_chain = data_chain.push(block)
    previous_hash = block_hash
  }
  
  // 验证数据链完整性
  let is_chain_valid = IntegrityManager::verify_chain(integrity_manager, data_chain, key_pair.public_key, signature_config)
  assert_true(is_chain_valid)
  
  // 测试篡改数据链
  let tampered_chain = data_chain.map(fn(block, index) {
    if index == 2 {
      // 篡改第3个区块
      let tampered_block_data = { block.data | data: "Tampered Block 3 data" }
      let tampered_block_hash = IntegrityManager::calculate_hash(integrity_manager, tampered_block_data, {
        algorithm: "SHA-256"
      })
      
      {
        data: tampered_block_data,
        hash: tampered_block_hash,
        signature: block.signature
      }
    } else {
      block
    }
  })
  
  let is_tampered_chain_valid = IntegrityManager::verify_chain(integrity_manager, tampered_chain, key_pair.public_key, signature_config)
  assert_false(is_tampered_chain_valid)  // 篡改后的数据链验证应该失败
  
  // 测试Merkle树完整性
  let merkle_tree_data = []
  
  for i in 0..100 {
    let data = "Transaction " + i.to_string()
    merkle_tree_data = merkle_tree_data.push(data)
  }
  
  let merkle_tree = IntegrityManager::create_merkle_tree(integrity_manager, merkle_tree_data, {
    hash_algorithm: "SHA-256"
  })
  
  // 验证Merkle树
  let is_merkle_valid = IntegrityManager::verify_merkle_tree(integrity_manager, merkle_tree)
  assert_true(is_merkle_valid)
  
  // 验证特定数据项
  let item_index = 42
  let item_data = merkle_tree_data[item_index]
  let proof = IntegrityManager::get_merkle_proof(integrity_manager, merkle_tree, item_index)
  
  let is_item_valid = IntegrityManager::verify_merkle_proof(integrity_manager, item_data, proof, merkle_tree.root)
  assert_true(is_item_valid)
  
  // 测试篡改Merkle树中的数据项
  let tampered_item = "Tampered Transaction " + item_index.to_string()
  let is_tampered_item_valid = IntegrityManager::verify_merkle_proof(integrity_manager, tampered_item, proof, merkle_tree.root)
  assert_false(is_tampered_item_valid)  // 篡改后的数据项验证应该失败
  
  // 测试批量签名验证
  let batch_data = []
  
  for i in 0..10 {
    let data = {
      id: "data-" + i.to_string(),
      content: "Content " + i.to_string(),
      timestamp: 1640995200 + i * 1000
    }
    
    let data_hash = IntegrityManager::calculate_hash(integrity_manager, data, {
      algorithm: "SHA-256"
    })
    
    let signature = IntegrityManager::sign_data(integrity_manager, data_hash, key_pair.private_key, signature_config)
    
    batch_data = batch_data.push({
      data: data,
      hash: data_hash,
      signature: signature
    })
  }
  
  let batch_verification = IntegrityManager::verify_batch(integrity_manager, batch_data, key_pair.public_key, signature_config)
  
  // 验证批量验证结果
  assert_eq(batch_verification.total_items, 10)
  assert_eq(batch_verification.valid_items, 10)
  assert_eq(batch_verification.invalid_items, 0)
  
  // 测试批量验证性能
  let batch_start_time = Time::now()
  let large_batch_data = []
  
  for i in 0..1000 {
    let data = {
      id: "data-" + i.to_string(),
      content: "Content " + i.to_string(),
      timestamp: 1640995200 + i * 1000
    }
    
    let data_hash = IntegrityManager::calculate_hash(integrity_manager, data, {
      algorithm: "SHA-256"
    })
    
    let signature = IntegrityManager::sign_data(integrity_manager, data_hash, key_pair.private_key, signature_config)
    
    large_batch_data = large_batch_data.push({
      data: data,
      hash: data_hash,
      signature: signature
    })
  }
  
  let large_batch_verification = IntegrityManager::verify_batch(integrity_manager, large_batch_data, key_pair.public_key, signature_config)
  let batch_verification_time = Time::now() - batch_start_time
  
  // 验证批量验证性能
  assert_true(batch_verification_time < 5000)  // 批量验证应在5秒内完成
  assert_eq(large_batch_verification.total_items, 1000)
  assert_eq(large_batch_verification.valid_items, 1000)
  assert_eq(large_batch_verification.invalid_items, 0)
}

// 测试5: 安全审计和合规性
test "安全审计和合规性测试" {
  // 创建审计管理器
  let audit_manager = AuditManager::new()
  
  // 配置审计策略
  let audit_policy = {
    log_all_access: true,
    log_data_changes: true,
    log_authentication: true,
    log_authorization: true,
    log_sensitive_operations: true,
    retention_days: 2555  // 7年
  }
  
  // 模拟安全事件
  let security_events = [
    {
      timestamp: "2022-01-01T10:00:00Z",
      event_type: "authentication",
      user_id: "admin",
      action: "login",
      result: "success",
      source_ip: "192.168.1.100",
      user_agent: "Mozilla/5.0..."
    },
    {
      timestamp: "2022-01-01T10:05:00Z",
      event_type: "authorization",
      user_id: "admin",
      action: "telemetry:read",
      resource: "trace-12345",
      result: "allowed",
      source_ip: "192.168.1.100"
    },
    {
      timestamp: "2022-01-01T10:10:00Z",
      event_type: "data_access",
      user_id: "analyst",
      action: "export",
      resource: "telemetry-data",
      result: "denied",
      reason: "insufficient_permissions",
      source_ip: "192.168.1.101"
    },
    {
      timestamp: "2022-01-01T10:15:00Z",
      event_type: "data_modification",
      user_id: "service",
      action: "telemetry:write",
      resource: "span-67890",
      result: "success",
      source_ip: "10.0.0.50"
    },
    {
      timestamp: "2022-01-01T10:20:00Z",
      event_type: "authentication",
      user_id: "unknown",
      action: "login",
      result: "failure",
      reason: "invalid_credentials",
      source_ip: "203.0.113.1"
    }
  ]
  
  // 记录安全事件
  for event in security_events {
    AuditManager::log_event(audit_manager, event, audit_policy)
  }
  
  // 测试审计日志查询
  let all_events = AuditManager::query_events(audit_manager, {
    start_time: "2022-01-01T00:00:00Z",
    end_time: "2022-01-01T23:59:59Z"
  })
  
  assert_eq(all_events.length(), 5)
  
  // 测试按用户查询
  let admin_events = AuditManager::query_events(audit_manager, {
    user_id: "admin",
    start_time: "2022-01-01T00:00:00Z",
    end_time: "2022-01-01T23:59:59Z"
  })
  
  assert_eq(admin_events.length(), 2)
  
  // 测试按事件类型查询
  let auth_events = AuditManager::query_events(audit_manager, {
    event_type: "authentication",
    start_time: "2022-01-01T00:00:00Z",
    end_time: "2022-01-01T23:59:59Z"
  })
  
  assert_eq(auth_events.length(), 2)
  
  // 测试按结果查询
  let failure_events = AuditManager::query_events(audit_manager, {
    result: "failure",
    start_time: "2022-01-01T00:00:00Z",
    end_time: "2022-01-01T23:59:59Z"
  })
  
  assert_eq(failure_events.length(), 2)
  
  // 测试安全分析
  let security_analysis = AuditManager::analyze_security(audit_manager, {
    start_time: "2022-01-01T00:00:00Z",
    end_time: "2022-01-01T23:59:59Z"
  })
  
  // 验证安全分析结果
  assert_eq(security_analysis.total_events, 5)
  assert_eq(security_analysis.successful_events, 3)
  assert_eq(security_analysis.failed_events, 2)
  
  assert_eq(security_analysis.authentication_events, 2)
  assert_eq(security_analysis.authorization_events, 1)
  assert_eq(security_analysis.data_access_events, 1)
  assert_eq(security_analysis.data_modification_events, 1)
  
  // 验证用户活动统计
  let admin_activity = security_analysis.user_activity.find(fn(a) { a.user_id == "admin" })
  assert_true(admin_activity != None)
  
  match admin_activity {
    Some(activity) => {
      assert_eq(activity.total_events, 2)
      assert_eq(activity.successful_events, 2)
      assert_eq(activity.failed_events, 0)
    }
    None => assert_true(false)
  }
  
  // 验证IP地址统计
  let internal_ips = security_analysis.ip_address_activity.filter(fn(a) { 
    a.ip_address.starts_with("192.168.") or a.ip_address.starts_with("10.")
  })
  
  assert_eq(internal_ips.length(), 3)  // 3个内部IP地址
  
  let external_ips = security_analysis.ip_address_activity.filter(fn(a) { 
    not(a.ip_address.starts_with("192.168.")) and not(a.ip_address.starts_with("10."))
  })
  
  assert_eq(external_ips.length(), 1)  // 1个外部IP地址
  
  // 测试异常检测
  let anomaly_detection = AuditManager::detect_anomalies(audit_manager, {
    start_time: "2022-01-01T00:00:00Z",
    end_time: "2022-01-01T23:59:59Z",
    anomaly_types: ["failed_logins", "unusual_access_patterns", "privilege_escalation"]
  })
  
  // 验证异常检测结果
  assert_true(anomaly_detection.anomalies.length() > 0)
  
  let failed_login_anomaly = anomaly_detection.anomalies.find(fn(a) { a.type == "failed_logins" })
  assert_true(failed_login_anomaly != None)
  
  match failed_login_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.severity, "medium")
      assert_true(anomaly.description.contains("invalid_credentials"))
    }
    None => assert_true(false)
  }
  
  // 测试合规性检查
  let compliance_checks = [
    {
      name: "gdpr_data_access",
      description: "GDPR要求记录所有个人数据访问",
      check: fn(events) {
        let pii_access = events.filter(fn(e) { 
          e.event_type == "data_access" and 
          e.resource.contains("pii") 
        })
        
        pii_access.length() > 0 and 
        pii_access.all(fn(e) { e.user_id != "" and e.timestamp != "" })
      }
    },
    {
      name: "sox_audit_trail",
      description: "SOX要求完整的审计追踪",
      check: fn(events) {
        events.all(fn(e) { 
          e.timestamp != "" and 
          e.user_id != "" and 
          e.action != "" and 
          e.result != "" 
        })
      }
    },
    {
      name: "pci_access_control",
      description: "PCI DSS要求严格的访问控制",
      check: fn(events) {
        let pci_access = events.filter(fn(e) { 
          e.resource.contains("payment") or e.resource.contains("card")
        })
        
        pci_access.all(fn(e) { 
          e.result == "allowed" or 
          (e.result == "denied" and e.reason != "") 
        })
      }
    }
  ]
  
  let compliance_results = []
  
  for check in compliance_checks {
    let result = AuditManager::check_compliance(audit_manager, check, {
      start_time: "2022-01-01T00:00:00Z",
      end_time: "2022-01-01T23:59:59Z"
    })
    
    compliance_results = compliance_results.push(result)
  }
  
  // 验证合规性检查结果
  assert_eq(compliance_results.length(), 3)
  
  // GDPR检查应该通过（假设有PII访问记录）
  let gdpr_result = compliance_results.find(fn(r) { r.name == "gdpr_data_access" })
  assert_true(gdpr_result != None)
  
  match gdpr_result {
    Some(result) => assert_true(result.compliant),
    None => assert_true(false)
  }
  
  // SOX检查应该通过
  let sox_result = compliance_results.find(fn(r) { r.name == "sox_audit_trail" })
  assert_true(sox_result != None)
  
  match sox_result {
    Some(result) => assert_true(result.compliant),
    None => assert_true(false)
  }
  
  // PCI检查应该通过
  let pci_result = compliance_results.find(fn(r) { r.name == "pci_access_control" })
  assert_true(pci_result != None)
  
  match pci_result {
    Some(result) => assert_true(result.compliant),
    None => assert_true(false)
  }
  
  // 生成合规性报告
  let compliance_report = AuditManager::generate_compliance_report(audit_manager, compliance_results, {
    format: "json",
    include_recommendations: true
  })
  
  // 验证合规性报告
  assert_true(compliance_report.length() > 0)
  assert_true(compliance_report.contains("compliance_summary"))
  assert_true(compliance_report.contains("recommendations"))
}
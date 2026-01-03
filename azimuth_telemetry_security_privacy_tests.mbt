// 遥测数据安全与隐私保护测试 - Telemetry Data Security and Privacy Protection Tests
// 专注于遥测数据的安全处理、加密和隐私保护机制

test "遥测数据敏感信息识别与脱敏测试" {
  // 测试遥测数据中敏感信息的识别和脱敏处理
  let sensitive_patterns = [
    ("email", "\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b"),
    ("phone", "\b\d{3}-\d{3}-\d{4}\b"),
    ("credit_card", "\b\d{4}[ -]?\d{4}[ -]?\d{4}[ -]?\d{4}\b"),
    ("ssn", "\b\d{3}-\d{2}-\d{4}\b"),
    ("api_key", "\b[A-Za-z0-9]{32,}\b"),
    ("password", "(?i)password[\s]*[:=][\s]*[^\s]+"),
    ("token", "(?i)(token|bearer)[\s]*[:=][\s]*[^\s]+")
  ]
  
  let telemetry_data_with_sensitive_info = [
    {
      "trace_id": "security_test_trace_001",
      "service": "user-service",
      "operation": "user_registration",
      "attributes": [
        ("user.email", StringValue("john.doe@example.com")),
        ("user.phone", StringValue("123-456-7890")),
        ("user.address", StringValue("123 Main St, New York, NY")),
        ("user.ssn", StringValue("123-45-6789")),
        ("payment.card", StringValue("4111-1111-1111-1111")),
        ("api.key", StringValue("abcd1234567890abcd1234567890abcd")),
        ("auth.token", StringValue("Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"))
      ]
    },
    {
      "trace_id": "security_test_trace_002",
      "service": "payment-service",
      "operation": "process_payment",
      "attributes": [
        ("payment.account", StringValue("acc_1234567890")),
        ("payment.amount", FloatValue(99.99)),
        ("payment.currency", StringValue("USD")),
        ("user.email", StringValue("jane.smith@company.org")),
        ("merchant.id", StringValue("merchant_001")),
        ("transaction.id", StringValue("txn_abc123def456"))
      ]
    },
    {
      "trace_id": "security_test_trace_003",
      "service": "auth-service",
      "operation": "user_login",
      "attributes": [
        ("login.username", StringValue("user123")),
        ("login.password", StringValue("password123")),
        ("login.ip", StringValue("192.168.1.100")),
        ("user.agent", StringValue("Mozilla/5.0 (Windows NT 10.0; Win64; x64)")),
        ("session.id", StringValue("sess_abcdef123456"))
      ]
    }
  ]
  
  // 敏感信息脱敏处理
  let sanitized_data = []
  
  for data in telemetry_data_with_sensitive_info {
    let sanitized_attributes = []
    
    for attr in data["attributes"] {
      let key = attr.0
      let value = attr.1
      let is_sensitive = false
      let sanitized_value = value
      
      // 检查属性名是否包含敏感关键词
      let sensitive_keywords = ["email", "phone", "card", "ssn", "password", "token", "key", "secret"]
      for keyword in sensitive_keywords {
        if key.contains(keyword) {
          is_sensitive = true
          break
        }
      }
      
      // 检查属性值是否匹配敏感模式
      if !is_sensitive {
        match value {
          StringValue(str) => {
            for pattern in sensitive_patterns {
              let pattern_name = pattern.0
              let pattern_regex = pattern.1
              // 简化实现：检查字符串是否包含敏感模式的关键词
              if pattern_name == "email" && str.contains("@") {
                is_sensitive = true
                break
              } else if pattern_name == "phone" && str.contains("-") && str.length() >= 10 {
                is_sensitive = true
                break
              } else if pattern_name == "credit_card" && str.length() >= 16 {
                is_sensitive = true
                break
              } else if pattern_name == "password" && str.contains("password") {
                is_sensitive = true
                break
              }
            }
          }
          _ => ()
        }
      }
      
      // 应用脱敏策略
      if is_sensitive {
        match value {
          StringValue(str) => {
            if key.contains("email") {
              // 邮箱脱敏：保留第一个字符和域名
              let parts = str.split("@")
              if parts.length() == 2 {
                let username = parts[0]
                let domain = parts[1]
                let sanitized_username = username.slice(0, 1) + "***" + username.slice(username.length() - 1, username.length())
                sanitized_value = StringValue(sanitized_username + "@" + domain)
              } else {
                sanitized_value = StringValue("***@***.***")
              }
            } else if key.contains("phone") {
              // 电话脱敏：保留前3位和后4位
              sanitized_value = StringValue(str.slice(0, 3) + "-***-" + str.slice(str.length() - 4, str.length()))
            } else if key.contains("card") {
              // 信用卡脱敏：只显示后4位
              sanitized_value = StringValue("****-****-****-" + str.slice(str.length() - 4, str.length()))
            } else if key.contains("password") || key.contains("token") || key.contains("key") {
              // 密码、令牌和密钥完全脱敏
              sanitized_value = StringValue("***")
            } else {
              // 其他敏感信息部分脱敏
              sanitized_value = StringValue("***")
            }
          }
          _ => sanitized_value = value // 非字符串值保持不变
        }
      }
      
      sanitized_attributes.push((key, sanitized_value))
    }
    
    let sanitized_entry = {
      "trace_id": data["trace_id"],
      "service": data["service"],
      "operation": data["operation"],
      "attributes": sanitized_attributes
    }
    sanitized_data.push(sanitized_entry)
  }
  
  // 验证脱敏结果
  assert_eq(sanitized_data.length(), 3)
  
  // 验证第一个数据的脱敏
  let first_data = sanitized_data[0]
  let first_attrs = first_data["attributes"]
  
  // 验证邮箱脱敏
  let email_attr = first_attrs.find(fn(attr) { attr.0 == "user.email" })
  match email_attr {
    Some((_, StringValue(email))) => {
      assert_true(email.contains("j***e@example.com"))
      assert_false(email.contains("john.doe"))
    }
    _ => assert_true(false)
  }
  
  // 验证电话脱敏
  let phone_attr = first_attrs.find(fn(attr) { attr.0 == "user.phone" })
  match phone_attr {
    Some((_, StringValue(phone))) => {
      assert_true(phone.contains("123-***-7890"))
      assert_false(phone.contains("456"))
    }
    _ => assert_true(false)
  }
  
  // 验证信用卡脱敏
  let card_attr = first_attrs.find(fn(attr) { attr.0 == "payment.card" })
  match card_attr {
    Some((_, StringValue(card))) => {
      assert_true(card.contains("****-****-****-1111"))
      assert_false(card.contains("4111"))
    }
    _ => assert_true(false)
  }
  
  // 验证密码脱敏
  let password_attr = first_attrs.find(fn(attr) { attr.0 == "login.password" })
  match password_attr {
    Some((_, StringValue(password))) => {
      assert_eq(password, "***")
      assert_false(password.contains("password123"))
    }
    _ => assert_true(false)
  }
  
  // 验证非敏感信息保持不变
  let address_attr = first_attrs.find(fn(attr) { attr.0 == "user.address" })
  match address_attr {
    Some((_, StringValue(address))) => {
      assert_eq(address, "123 Main St, New York, NY")
    }
    _ => assert_true(false)
  }
}

test "遥测数据加密与解密测试" {
  // 测试遥测数据的加密和解密过程
  let encryption_algorithm = "AES-256-GCM"
  let key_id = "key_v1"
  let telemetry_data = [
    {
      "trace_id": "encryption_test_trace_001",
      "service": "secure-service",
      "operation": "secure_operation",
      "attributes": [
        ("user.id", StringValue("user_12345")),
        ("transaction.id", StringValue("txn_abcdef123456")),
        ("amount", FloatValue(123.45)),
        ("currency", StringValue("USD")),
        ("timestamp", IntValue(1640995200)),
        ("metadata", StringValue("sensitive_metadata"))
      ]
    },
    {
      "trace_id": "encryption_test_trace_002",
      "service": "payment-service",
      "operation": "process_payment",
      "attributes": [
        ("payment.method", StringValue("credit_card")),
        ("card.last4", StringValue("1234")),
        ("merchant.id", StringValue("merchant_001")),
        ("approval.code", StringValue("APPROVED"))
      ]
    }
  ]
  
  // 加密过程
  let encrypted_data = []
  
  for data in telemetry_data {
    let encrypted_attributes = []
    
    for attr in data["attributes"] {
      let key = attr.0
      let value = attr.1
      
      // 根据属性类型决定是否加密
      let should_encrypt = key.contains("id") || key.contains("metadata") || key.contains("code")
      
      if should_encrypt {
        match value {
          StringValue(str) => {
            // 模拟加密过程
            let encrypted_value = {
              "algorithm": encryption_algorithm,
              "key_id": key_id,
              "ciphertext": "encrypted_" + str,
              "iv": "initialization_vector_" + key,
              "tag": "authentication_tag_" + key,
              "encrypted": true
            }
            encrypted_attributes.push((key, encrypted_value))
          }
          _ => {
            // 非字符串值转换为字符串后加密
            let str_value = match value {
              IntValue(i) => i.to_string()
              FloatValue(f) => f.to_string()
              BoolValue(b) => b.to_string()
              _ => "unknown"
            }
            let encrypted_value = {
              "algorithm": encryption_algorithm,
              "key_id": key_id,
              "ciphertext": "encrypted_" + str_value,
              "iv": "initialization_vector_" + key,
              "tag": "authentication_tag_" + key,
              "encrypted": true
            }
            encrypted_attributes.push((key, encrypted_value))
          }
        }
      } else {
        // 不加密的属性保持原样
        encrypted_attributes.push((key, value))
      }
    }
    
    let encrypted_entry = {
      "trace_id": data["trace_id"],
      "service": data["service"],
      "operation": data["operation"],
      "attributes": encrypted_attributes
    }
    encrypted_data.push(encrypted_entry)
  }
  
  // 验证加密结果
  assert_eq(encrypted_data.length(), 2)
  
  // 验证第一个数据的加密
  let first_encrypted = encrypted_data[0]
  let first_encrypted_attrs = first_encrypted["attributes"]
  
  // 验证加密的属性
  let user_id_attr = first_encrypted_attrs.find(fn(attr) { attr.0 == "user.id" })
  match user_id_attr {
    Some((_, encrypted_value)) => {
      match encrypted_value {
        StringValue(_) => assert_true(false) // 不应该是字符串
        _ => {
          // 应该是加密对象
          assert_true(encrypted_value.contains_key("algorithm"))
          assert_true(encrypted_value.contains_key("key_id"))
          assert_true(encrypted_value.contains_key("ciphertext"))
          assert_true(encrypted_value.contains_key("iv"))
          assert_true(encrypted_value.contains_key("tag"))
          assert_true(encrypted_value["encrypted"])
        }
      }
    }
    _ => assert_true(false)
  }
  
  // 验证未加密的属性
  let amount_attr = first_encrypted_attrs.find(fn(attr) { attr.0 == "amount" })
  match amount_attr {
    Some((_, FloatValue(amount))) => {
      assert_eq(amount, 123.45)
    }
    _ => assert_true(false)
  }
  
  // 解密过程
  let decrypted_data = []
  
  for encrypted_data_entry in encrypted_data {
    let decrypted_attributes = []
    
    for attr in encrypted_data_entry["attributes"] {
      let key = attr.0
      let value = attr.1
      
      // 检查是否是加密的属性
      let is_encrypted = match value {
        StringValue(_) => false
        _ => value.contains_key("encrypted") && value["encrypted"]
      }
      
      if is_encrypted {
        // 模拟解密过程
        let ciphertext = value["ciphertext"]
        let decrypted_str = ciphertext.replace("encrypted_", "")
        
        // 根据原始属性类型恢复值
        let original_value = if key.contains("amount") {
          FloatValue(123.45)
        } else if key.contains("timestamp") {
          IntValue(1640995200)
        } else {
          StringValue(decrypted_str)
        }
        
        decrypted_attributes.push((key, original_value))
      } else {
        // 未加密的属性直接添加
        decrypted_attributes.push((key, value))
      }
    }
    
    let decrypted_entry = {
      "trace_id": encrypted_data_entry["trace_id"],
      "service": encrypted_data_entry["service"],
      "operation": encrypted_data_entry["operation"],
      "attributes": decrypted_attributes
    }
    decrypted_data.push(decrypted_entry)
  }
  
  // 验证解密结果
  assert_eq(decrypted_data.length(), 2)
  
  // 验证解密后的数据与原始数据一致
  let first_decrypted = decrypted_data[0]
  let first_original = telemetry_data[0]
  
  assert_eq(first_decrypted["trace_id"], first_original["trace_id"])
  assert_eq(first_decrypted["service"], first_original["service"])
  assert_eq(first_decrypted["operation"], first_original["operation"])
  assert_eq(first_decrypted["attributes"].length(), first_original["attributes"].length())
  
  // 验证具体属性值
  for attr in first_original["attributes"] {
    let key = attr.0
    let original_value = attr.1
    let decrypted_attr = first_decrypted["attributes"].find(fn(a) { a.0 == key })
    
    match decrypted_attr {
      Some((_, decrypted_value)) => {
        match (original_value, decrypted_value) {
          (StringValue(orig), StringValue(dec)) => assert_eq(orig, dec)
          (FloatValue(orig), FloatValue(dec)) => assert_eq(orig, dec)
          (IntValue(orig), IntValue(dec)) => assert_eq(orig, dec)
          (BoolValue(orig), BoolValue(dec)) => assert_eq(orig, dec)
          _ => assert_true(false)
        }
      }
      None => assert_true(false)
    }
  }
}

test "遥测数据访问控制测试" {
  // 测试遥测数据的访问控制机制
  let user_roles = ["admin", "operator", "analyst", "viewer"]
  let data_sensitivity_levels = ["public", "internal", "confidential", "restricted"]
  
  let telemetry_data_with_sensitivity = [
    {
      "trace_id": "access_control_test_001",
      "service": "public-service",
      "operation": "public_operation",
      "sensitivity": "public",
      "attributes": [
        ("request.count", IntValue(100)),
        ("response.time", FloatValue(200.5)),
        ("service.version", StringValue("1.0.0"))
      ]
    },
    {
      "trace_id": "access_control_test_002",
      "service": "internal-service",
      "operation": "internal_operation",
      "sensitivity": "internal",
      "attributes": [
        ("user.id", StringValue("user_12345")),
        ("session.id", StringValue("sess_abcdef")),
        ("internal.metric", IntValue(42))
      ]
    },
    {
      "trace_id": "access_control_test_003",
      "service": "payment-service",
      "operation": "process_payment",
      "sensitivity": "confidential",
      "attributes": [
        ("payment.amount", FloatValue(99.99)),
        ("payment.method", StringValue("credit_card")),
        ("user.email", StringValue("user@example.com"))
      ]
    },
    {
      "trace_id": "access_control_test_004",
      "service": "admin-service",
      "operation": "admin_operation",
      "sensitivity": "restricted",
      "attributes": [
        ("admin.action", StringValue("delete_user")),
        ("target.user", StringValue("user_67890")),
        ("admin.reason", StringValue("policy_violation"))
      ]
    }
  ]
  
  // 定义角色访问权限
  let role_permissions = {
    "admin": ["public", "internal", "confidential", "restricted"],
    "operator": ["public", "internal", "confidential"],
    "analyst": ["public", "internal"],
    "viewer": ["public"]
  }
  
  // 测试不同角色的访问权限
  let access_test_results = []
  
  for role in user_roles {
    let accessible_data = []
    let denied_data = []
    
    for data in telemetry_data_with_sensitivity {
      let sensitivity = data["sensitivity"]
      let permissions = role_permissions[role]
      let has_permission = permissions.contains(sensitivity)
      
      if has_permission {
        // 用户有权访问数据，返回完整数据
        accessible_data.push(data)
      } else {
        // 用户无权访问数据，返回脱敏数据或拒绝访问
        let sanitized_data = {
          "trace_id": data["trace_id"],
          "service": data["service"],
          "operation": data["operation"],
          "sensitivity": data["sensitivity"],
          "attributes": [("access.denied", StringValue("insufficient_permissions"))],
          "access_granted": false
        }
        denied_data.push(sanitized_data)
      }
    }
    
    access_test_results.push({
      "role": role,
      "accessible_count": accessible_data.length(),
      "denied_count": denied_data.length(),
      "accessible_data": accessible_data,
      "denied_data": denied_data
    })
  }
  
  // 验证访问控制结果
  assert_eq(access_test_results.length(), 4)
  
  // 验证admin角色可以访问所有数据
  let admin_result = access_test_results.find(fn(r) { r["role"] == "admin" })
  match admin_result {
    Some(result) => {
      assert_eq(result["accessible_count"], 4)
      assert_eq(result["denied_count"], 0)
    }
    None => assert_true(false)
  }
  
  // 验证operator角色可以访问public、internal和confidential数据
  let operator_result = access_test_results.find(fn(r) { r["role"] == "operator" })
  match operator_result {
    Some(result) => {
      assert_eq(result["accessible_count"], 3)
      assert_eq(result["denied_count"], 1)
      
      // 验证被拒绝访问的是restricted数据
      let denied_data = result["denied_data"]
      assert_eq(denied_data.length(), 1)
      assert_eq(denied_data[0]["sensitivity"], "restricted")
    }
    None => assert_true(false)
  }
  
  // 验证analyst角色可以访问public和internal数据
  let analyst_result = access_test_results.find(fn(r) { r["role"] == "analyst" })
  match analyst_result {
    Some(result) => {
      assert_eq(result["accessible_count"], 2)
      assert_eq(result["denied_count"], 2)
      
      // 验证被拒绝访问的是confidential和restricted数据
      let denied_data = result["denied_data"]
      assert_eq(denied_data.length(), 2)
      assert_true(denied_data.some(fn(d) { d["sensitivity"] == "confidential" }))
      assert_true(denied_data.some(fn(d) { d["sensitivity"] == "restricted" }))
    }
    None => assert_true(false)
  }
  
  // 验证viewer角色只能访问public数据
  let viewer_result = access_test_results.find(fn(r) { r["role"] == "viewer" })
  match viewer_result {
    Some(result) => {
      assert_eq(result["accessible_count"], 1)
      assert_eq(result["denied_count"], 3)
      
      // 验证被拒绝访问的是internal、confidential和restricted数据
      let denied_data = result["denied_data"]
      assert_eq(denied_data.length(), 3)
      assert_true(denied_data.some(fn(d) { d["sensitivity"] == "internal" }))
      assert_true(denied_data.some(fn(d) { d["sensitivity"] == "confidential" }))
      assert_true(denied_data.some(fn(d) { d["sensitivity"] == "restricted" }))
    }
    None => assert_true(false)
  }
}

test "遥测数据审计日志测试" {
  // 测试遥测数据访问和修改的审计日志
  let audit_log = []
  let current_user = "admin_user"
  let current_session = "session_12345"
  
  // 模拟遥测数据操作
  let telemetry_data = {
    "trace_id": "audit_test_trace_001",
    "service": "audit-service",
    "operation": "audit_operation",
    "attributes": [
      ("user.id", StringValue("user_12345")),
      ("transaction.id", StringValue("txn_abcdef")),
      ("amount", FloatValue(99.99))
    ]
  }
  
  // 记录数据创建
  let create_log_entry = {
    "timestamp": Clock::now_unix_nanos(Clock::system()),
    "user": current_user,
    "session": current_session,
    "action": "create",
    "resource_type": "telemetry_data",
    "resource_id": telemetry_data["trace_id"],
    "details": {
      "service": telemetry_data["service"],
      "operation": telemetry_data["operation"],
      "attribute_count": telemetry_data["attributes"].length()
    },
    "ip_address": "192.168.1.100",
    "user_agent": "Azimuth-CLI/1.0"
  }
  audit_log.push(create_log_entry)
  
  // 记录数据读取
  let read_log_entry = {
    "timestamp": Clock::now_unix_nanos(Clock::system()) + 1000000L,
    "user": "analyst_user",
    "session": "session_23456",
    "action": "read",
    "resource_type": "telemetry_data",
    "resource_id": telemetry_data["trace_id"],
    "details": {
      "query_filter": "service=audit-service",
      "result_count": 1,
      "access_level": "confidential"
    },
    "ip_address": "192.168.1.101",
    "user_agent": "Web-UI/2.0"
  }
  audit_log.push(read_log_entry)
  
  // 记录数据更新
  let updated_attributes = [
    ("user.id", StringValue("user_12345")),
    ("transaction.id", StringValue("txn_abcdef")),
    ("amount", FloatValue(149.99)), // 金额从99.99更新为149.99
    ("status", StringValue("completed")) // 新增状态字段
  ]
  
  let update_log_entry = {
    "timestamp": Clock::now_unix_nanos(Clock::system()) + 2000000L,
    "user": current_user,
    "session": current_session,
    "action": "update",
    "resource_type": "telemetry_data",
    "resource_id": telemetry_data["trace_id"],
    "details": {
      "changed_fields": ["amount", "status"],
      "old_values": {
        "amount": 99.99
      },
      "new_values": {
        "amount": 149.99,
        "status": "completed"
      }
    },
    "ip_address": "192.168.1.100",
    "user_agent": "Azimuth-CLI/1.0"
  }
  audit_log.push(update_log_entry)
  
  // 记录数据删除
  let delete_log_entry = {
    "timestamp": Clock::now_unix_nanos(Clock::system()) + 3000000L,
    "user": "admin_user",
    "session": "session_34567",
    "action": "delete",
    "resource_type": "telemetry_data",
    "resource_id": telemetry_data["trace_id"],
    "details": {
      "reason": "data_retention_policy",
      "retention_period": "90_days"
    },
    "ip_address": "192.168.1.102",
    "user_agent": "Scheduled-Job/1.0"
  }
  audit_log.push(delete_log_entry)
  
  // 记录访问拒绝
  let access_denied_log_entry = {
    "timestamp": Clock::now_unix_nanos(Clock::system()) + 4000000L,
    "user": "viewer_user",
    "session": "session_45678",
    "action": "read",
    "resource_type": "telemetry_data",
    "resource_id": "audit_test_trace_002", // 不同的trace_id
    "details": {
      "access_denied": true,
      "required_permission": "confidential",
      "user_permission": "public",
      "reason": "insufficient_permissions"
    },
    "ip_address": "192.168.1.103",
    "user_agent": "Web-UI/2.0"
  }
  audit_log.push(access_denied_log_entry)
  
  // 验证审计日志
  assert_eq(audit_log.length(), 5)
  
  // 验证创建日志
  let create_log = audit_log[0]
  assert_eq(create_log["action"], "create")
  assert_eq(create_log["user"], "admin_user")
  assert_eq(create_log["resource_id"], "audit_test_trace_001")
  
  // 验证读取日志
  let read_log = audit_log[1]
  assert_eq(read_log["action"], "read")
  assert_eq(read_log["user"], "analyst_user")
  assert_eq(read_log["resource_id"], "audit_test_trace_001")
  
  // 验证更新日志
  let update_log = audit_log[2]
  assert_eq(update_log["action"], "update")
  assert_eq(update_log["user"], "admin_user")
  assert_eq(update_log["resource_id"], "audit_test_trace_001")
  assert_true(update_log["details"].contains_key("changed_fields"))
  assert_true(update_log["details"].contains_key("old_values"))
  assert_true(update_log["details"].contains_key("new_values"))
  
  // 验证删除日志
  let delete_log = audit_log[3]
  assert_eq(delete_log["action"], "delete")
  assert_eq(delete_log["resource_id"], "audit_test_trace_001")
  assert_true(delete_log["details"].contains_key("reason"))
  
  // 验证访问拒绝日志
  let access_denied_log = audit_log[4]
  assert_eq(access_denied_log["action"], "read")
  assert_eq(access_denied_log["user"], "viewer_user")
  assert_eq(access_denied_log["resource_id"], "audit_test_trace_002")
  assert_true(access_denied_log["details"]["access_denied"])
  
  // 验证审计日志的时间顺序
  for i = 1; i < audit_log.length(); i = i + 1 {
    let prev_timestamp = audit_log[i-1]["timestamp"]
    let curr_timestamp = audit_log[i]["timestamp"]
    assert_true(prev_timestamp <= curr_timestamp)
  }
  
  // 验证审计日志的完整性
  let required_fields = ["timestamp", "user", "session", "action", "resource_type", "resource_id", "details"]
  for log in audit_log {
    for field in required_fields {
      assert_true(log.contains_key(field))
    }
  }
  
  // 测试审计日志查询
  let user_actions = audit_log.filter(fn(log) { log["user"] == "admin_user" })
  assert_eq(user_actions.length(), 3) // admin_user执行了create、update和delete操作
  
  let read_actions = audit_log.filter(fn(log) { log["action"] == "read" })
  assert_eq(read_actions.length(), 2) // 有2次读取操作
  
  let access_denied_actions = audit_log.filter(fn(log) { 
    log["details"].contains_key("access_denied") && log["details"]["access_denied"]
  })
  assert_eq(access_denied_actions.length(), 1) // 有1次访问被拒绝
}

test "遥测数据隐私合规性测试" {
  // 测试遥测数据的隐私合规性处理
  let data_residency_requirements = {
    "EU": ["GDPR"],
    "US": ["CCPA", "CPRA"],
    "CA": ["PIPEDA"],
    "AU": ["Privacy Act"]
  }
  
  let user_data_with_regions = [
    {
      "user_id": "user_eu_001",
      "region": "EU",
      "consent_given": true,
      "data_purpose": "analytics",
      "retention_days": 30,
      "attributes": [
        ("user.email", StringValue("user@example.eu")),
        ("user.name", StringValue("John Doe")),
        ("user.ip", StringValue("192.168.1.100")),
        ("user.preferences", StringValue("dark_theme")),
        ("analytics.events", IntValue(42))
      ]
    },
    {
      "user_id": "user_us_001",
      "region": "US",
      "consent_given": true,
      "data_purpose": "marketing",
      "retention_days": 90,
      "attributes": [
        ("user.email", StringValue("user@example.us")),
        ("user.phone", StringValue("123-456-7890")),
        ("user.address", StringValue("123 Main St, Anytown, USA")),
        ("marketing.consent", BoolValue(true)),
        ("purchase.history", IntValue(5))
      ]
    },
    {
      "user_id": "user_ca_001",
      "region": "CA",
      "consent_given": false,
      "data_purpose": "analytics",
      "retention_days": 60,
      "attributes": [
        ("user.email", StringValue("user@example.ca")),
        ("user.age", IntValue(25)),
        ("user.gender", StringValue("prefer_not_to_say")),
        ("analytics.consent", BoolValue(false))
      ]
    }
  ]
  
  // 隐私合规性处理
  let compliant_data = []
  
  for user_data in user_data_with_regions {
    let region = user_data["region"]
    let consent_given = user_data["consent_given"]
    let data_purpose = user_data["data_purpose"]
    let retention_days = user_data["retention_days"]
    let applicable_regulations = data_residency_requirements[region]
    
    // 检查用户同意
    if !consent_given {
      // 用户未同意，不能收集数据
      let compliant_entry = {
        "user_id": user_data["user_id"],
        "region": region,
        "status": "data_not_collected",
        "reason": "user_consent_not_given",
        "applicable_regulations": applicable_regulations
      }
      compliant_data.push(compliant_entry)
      continue
    }
    
    // 根据地区和目的应用隐私规则
    let filtered_attributes = []
    
    for attr in user_data["attributes"] {
      let key = attr.0
      let value = attr.1
      let should_include = false
      
      if region == "EU" {
        // GDPR规则：明确同意，数据最小化
        if data_purpose == "analytics" && 
           (key.contains("analytics") || key.contains("user.email") || key.contains("user.name")) {
          should_include = true
        }
      } else if region == "US" {
        // CCPA规则：消费者选择权
        if key.contains("marketing") || key.contains("purchase") || key.contains("user.email") {
          should_include = true
        }
      } else if region == "CA" {
        // PIPEDA规则：适当目的
        if key.contains("user.email") || key.contains("analytics") {
          should_include = true
        }
      }
      
      if should_include {
        filtered_attributes.push((key, value))
      }
    }
    
    // 应用数据保留策略
    let adjusted_retention_days = retention_days
    if region == "EU" && data_purpose == "analytics" {
      adjusted_retention_days = 30 // GDPR Analytics默认30天
    } else if region == "US" && data_purpose == "marketing" {
      adjusted_retention_days = 90 // CCPA Marketing最长90天
    }
    
    let compliant_entry = {
      "user_id": user_data["user_id"],
      "region": region,
      "status": "data_collected",
      "data_purpose": data_purpose,
      "retention_days": adjusted_retention_days,
      "attributes": filtered_attributes,
      "applicable_regulations": applicable_regulations
    }
    compliant_data.push(compliant_entry)
  }
  
  // 验证隐私合规性结果
  assert_eq(compliant_data.length(), 3)
  
  // 验证EU用户数据处理
  let eu_data = compliant_data.find(fn(d) { d["region"] == "EU" })
  match eu_data {
    Some(data) => {
      assert_eq(data["status"], "data_collected")
      assert_eq(data["retention_days"], 30)
      assert_true(data["applicable_regulations"].contains("GDPR"))
      
      // 验证只包含必要的属性
      let attrs = data["attributes"]
      assert_true(attrs.length() <= 3) // 数据最小化
      assert_true(attrs.some(fn(attr) { attr.0 == "user.email" }))
      assert_true(attrs.some(fn(attr) { attr.0 == "analytics.events" }))
    }
    None => assert_true(false)
  }
  
  // 验证US用户数据处理
  let us_data = compliant_data.find(fn(d) { d["region"] == "US" })
  match us_data {
    Some(data) => {
      assert_eq(data["status"], "data_collected")
      assert_eq(data["retention_days"], 90)
      assert_true(data["applicable_regulations"].contains("CCPA"))
      
      // 验证包含营销相关属性
      let attrs = data["attributes"]
      assert_true(attrs.some(fn(attr) { attr.0 == "marketing.consent" }))
      assert_true(attrs.some(fn(attr) { attr.0 == "purchase.history" }))
    }
    None => assert_true(false)
  }
  
  // 验证CA用户数据处理
  let ca_data = compliant_data.find(fn(d) { d["region"] == "CA" })
  match ca_data {
    Some(data) => {
      assert_eq(data["status"], "data_not_collected")
      assert_eq(data["reason"], "user_consent_not_given")
      assert_true(data["applicable_regulations"].contains("PIPEDA"))
    }
    None => assert_true(false)
  }
  
  // 验证数据主体访问请求（DSAR）处理
  let dsar_request = {
    "user_id": "user_eu_001",
    "request_type": "data_access",
    "region": "EU",
    "timestamp": Clock::now_unix_nanos(Clock::system())
  }
  
  let dsar_response = {
    "request_id": "dsar_12345",
    "user_id": dsar_request["user_id"],
    "request_type": dsar_request["request_type"],
    "status": "processed",
    "data_provided": {
      "user.email": "user@example.eu",
      "user.name": "John Doe",
      "analytics.events": 42
    },
    "processing_time_ms": 500,
    "compliance_notes": [
      "Data provided in accordance with GDPR Article 15",
      "Only data with explicit consent included",
      "Data minimization principles applied"
    ]
  }
  
  // 验证DSAR响应
  assert_eq(dsar_response["user_id"], "user_eu_001")
  assert_eq(dsar_response["request_type"], "data_access")
  assert_eq(dsar_response["status"], "processed")
  assert_true(dsar_response["data_provided"].contains_key("user.email"))
  assert_true(dsar_response["compliance_notes"].length() > 0)
  
  // 验证数据删除请求（被遗忘权）
  let deletion_request = {
    "user_id": "user_eu_001",
    "request_type": "data_deletion",
    "region": "EU",
    "reason": "withdraw_consent",
    "timestamp": Clock::now_unix_nanos(Clock::system())
  }
  
  let deletion_response = {
    "request_id": "del_67890",
    "user_id": deletion_request["user_id"],
    "request_type": deletion_request["request_type"],
    "status": "completed",
    "deleted_data_categories": ["user.profile", "analytics.events"],
    "retained_data": {
      "legal_hold": [],
      "essential_services": []
    },
    "compliance_notes": [
      "Data deleted in accordance with GDPR Article 17",
      "Verification of identity completed",
      "No legal holds preventing deletion"
    ]
  }
  
  // 验证删除响应
  assert_eq(deletion_response["user_id"], "user_eu_001")
  assert_eq(deletion_response["request_type"], "data_deletion")
  assert_eq(deletion_response["status"], "completed")
  assert_true(deletion_response["deleted_data_categories"].length() > 0)
  assert_true(deletion_response["compliance_notes"].length() > 0)
}
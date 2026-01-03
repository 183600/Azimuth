// Azimuth 安全和隐私保护测试
// 专注于测试遥测系统的安全性和隐私保护机制

// 测试1: 敏感数据检测和脱敏
test "敏感数据检测和脱敏" {
  // 定义敏感数据类型
  enum SensitiveDataType {
    Email
    PhoneNumber
    CreditCard
    SSN  // 社会安全号码
    Password
    APIKey
    Token
    PersonalInfo
    FinancialInfo
    HealthInfo
  }
  
  // 定义脱敏策略
  enum MaskingStrategy {
    FullMask      // 完全掩码: *****
    PartialMask   // 部分掩码: a***@example.com
    Hash          // 哈希: 5f4dcc3b5aa765d61d8327deb882cf99
    Tokenize      // 令牌化: tok_1234567890abcdef
    Redact        // 删除: ""
    Encrypt       // 加密: encrypted_value
  }
  
  // 定义敏感数据规则
  type SensitiveDataRule = {
    data_type: SensitiveDataType,
    pattern: String,  // 正则表达式模式
    strategy: MaskingStrategy,
    description: String,
    priority: Int     // 优先级，数字越大优先级越高
  }
  
  // 定义脱敏结果
  type MaskingResult = {
    original_value: String,
    masked_value: String,
    data_type: SensitiveDataType,
    strategy: MaskingStrategy,
    confidence: Float  // 0.0 to 1.0
  }
  
  // 创建敏感数据检测器
  let create_sensitive_data_detector = fn() {
    let rules = []
    
    {
      rules,
      
      // 添加敏感数据规则
      add_rule: fn(rule: SensitiveDataRule) {
        rules = rules.push(rule)
      },
      
      // 检测敏感数据
      detect_sensitive_data: fn(text: String) {
        let detections = []
        
        // 按优先级排序规则
        let sorted_rules = rules.sort(fn(a, b) { b.priority - a.priority })
        
        for rule in sorted_rules {
          // 简化的正则匹配（在实际实现中会使用完整的正则表达式）
          let matches = match rule.data_type {
            Email => {
              if text.contains("@") && text.contains(".") {
                let parts = text.split("@")
                if parts.length() == 2 && parts[0].length() > 0 && parts[1].contains(".") {
                  [text]  // 找到匹配
                } else {
                  []
                }
              } else {
                []
              }
            }
            PhoneNumber => {
              if text.length() >= 10 && text.some(fn(c) { c >= '0' && c <= '9' }) {
                [text]
              } else {
                []
              }
            }
            CreditCard => {
              if text.length() >= 13 && text.length() <= 19 && text.every(fn(c) { c >= '0' && c <= '9' }) {
                [text]
              } else {
                []
              }
            }
            Password => {
              if text.to_lowercase().contains("password") || text.to_lowercase().contains("pwd") {
                [text]
              } else {
                []
              }
            }
            APIKey => {
              if (text.contains("key") || text.contains("token")) && text.length() > 10 {
                [text]
              } else {
                []
              }
            }
            _ => []
          }
          
          for match in matches {
            let detection = {
              value: match,
              data_type: rule.data_type,
              strategy: rule.strategy,
              confidence: 0.9  // 简化的置信度
            }
            detections = detections.push(detection)
          }
        }
        
        detections
      },
      
      // 应用脱敏策略
      apply_masking: fn(value: String, data_type: SensitiveDataType, strategy: MaskingStrategy) {
        let masked_value = match strategy {
          FullMask => {
            "*".repeat(value.length())
          }
          PartialMask => {
            match data_type {
              Email => {
                let parts = value.split("@")
                if parts.length() == 2 && parts[0].length() > 2 {
                  let username = parts[0]
                  let domain = parts[1]
                  username.substring(0, 2) + "*".repeat(username.length() - 2) + "@" + domain
                } else {
                  "*".repeat(value.length())
                }
              }
              PhoneNumber => {
                if value.length() >= 4 {
                  value.substring(0, 2) + "*".repeat(value.length() - 4) + value.substring(value.length() - 2)
                } else {
                  "*".repeat(value.length())
                }
              }
              CreditCard => {
                if value.length() >= 4 {
                  "*".repeat(value.length() - 4) + value.substring(value.length() - 4)
                } else {
                  "*".repeat(value.length())
                }
              }
              _ => "*".repeat(value.length())
            }
          }
          Hash => {
            // 简化的哈希实现
            let mut hash = 0
            for char in value.to_char_array() {
              hash = (hash * 31 + char.to_int()) % 1000000007
            }
            hash.to_string()
          }
          Tokenize => {
            "tok_" + value.substring(0, 8) + UUID::v4().substring(0, 8)
          }
          Redact => {
            ""
          }
          Encrypt => {
            // 简化的加密实现
            "enc_" + value.to_base64()
          }
        }
        
        {
          original_value: value,
          masked_value: masked_value,
          data_type: data_type,
          strategy: strategy,
          confidence: 0.9
        }
      }
    }
  }
  
  // 创建敏感数据检测器并添加规则
  let detector = create_sensitive_data_detector()
  
  // 添加各种敏感数据规则
  detector.add_rule({
    data_type: Email,
    pattern: "\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b",
    strategy: PartialMask,
    description: "Email address detection",
    priority: 90
  })
  
  detector.add_rule({
    data_type: PhoneNumber,
    pattern: "\b\d{10,}\b",
    strategy: PartialMask,
    description: "Phone number detection",
    priority: 80
  })
  
  detector.add_rule({
    data_type: CreditCard,
    pattern: "\b\d{13,19}\b",
    strategy: PartialMask,
    description: "Credit card number detection",
    priority: 95
  })
  
  detector.add_rule({
    data_type: Password,
    pattern: "(?i)password|pwd",
    strategy: FullMask,
    description: "Password field detection",
    priority: 100
  })
  
  detector.add_rule({
    data_type: APIKey,
    pattern: "(?i)(api[_-]?key|token|secret)",
    strategy: Tokenize,
    description: "API key detection",
    priority: 85
  })
  
  // 测试敏感数据检测
  let test_data = [
    ("user@example.com", Email),
    ("1234567890", PhoneNumber),
    ("4111111111111111", CreditCard),
    ("mySecretPassword123", Password),
    ("api_key_abc123def456", APIKey),
    ("john.doe@company.com", Email),
    ("555-123-4567", PhoneNumber),
    ("5200000000000001", CreditCard)
  ]
  
  for (test_value, expected_type) in test_data {
    let detections = detector.detect_sensitive_data(test_value)
    assert_true(detections.length() > 0)
    
    let detection = detections[0]
    assert_eq(detection.value, test_value)
    assert_eq(detection.data_type, expected_type)
    assert_true(detection.confidence > 0.0)
  }
  
  // 测试脱敏策略
  let test_masking = [
    ("user@example.com", Email, PartialMask),
    ("1234567890", PhoneNumber, PartialMask),
    ("4111111111111111", CreditCard, PartialMask),
    ("mySecretPassword123", Password, FullMask),
    ("api_key_abc123def456", APIKey, Tokenize)
  ]
  
  for (test_value, data_type, strategy) in test_masking {
    let masking_result = detector.apply_masking(test_value, data_type, strategy)
    assert_eq(masking_result.original_value, test_value)
    assert_eq(masking_result.data_type, data_type)
    assert_eq(masking_result.strategy, strategy)
    assert_true(masking_result.masked_value != test_value)  // 脱敏后的值应该不同
    
    // 验证特定脱敏策略的效果
    match strategy {
      FullMask => {
        assert_true(masking_result.masked_value.every(fn(c) { c == '*' }))
      }
      PartialMask => {
        assert_true(masking_result.masked_value.contains('*'))
        assert_true(masking_result.masked_value.length() == test_value.length())
      }
      Tokenize => {
        assert_true(masking_result.masked_value.starts_with("tok_"))
      }
      Hash => {
        assert_true(masking_result.masked_value.every(fn(c) { c >= '0' && c <= '9' }))
      }
      Redact => {
        assert_eq(masking_result.masked_value, "")
      }
      Encrypt => {
        assert_true(masking_result.masked_value.starts_with("enc_"))
      }
    }
  }
  
  // 测试复杂文本中的敏感数据检测和脱敏
  let complex_text = "User john.doe@company.com with phone 555-123-4567 used credit card 4111111111111111 for purchase. API key is api_key_abc123def456 and password is mySecretPassword123."
  
  let complex_detections = detector.detect_sensitive_data(complex_text)
  assert_true(complex_detections.length() >= 5)  // 应该检测到多个敏感数据
  
  // 验证检测到的敏感数据类型
  let detected_types = complex_detections.map(fn(d) { d.data_type })
  assert_true(detected_types.contains(Email))
  assert_true(detected_types.contains(PhoneNumber))
  assert_true(detected_types.contains(CreditCard))
  assert_true(detected_types.contains(APIKey))
  assert_true(detected_types.contains(Password))
}

// 测试2: 数据传输加密和安全
test "数据传输加密和安全" {
  // 定义加密算法
  enum EncryptionAlgorithm {
    AES256
    ChaCha20
    RSA
    ECDSA
    TLS
  }
  
  // 定义安全协议
  enum SecurityProtocol {
    TLS_1_2
    TLS_1_3
    HTTPS
    WSS
    mTLS
  }
  
  // 定义加密配置
  type EncryptionConfig = {
    algorithm: EncryptionAlgorithm,
    key_size: Int,
    protocol: SecurityProtocol,
    certificate_validation: Bool,
    compression: Bool,
    perfect_forward_secrecy: Bool
  }
  
  // 定义安全传输结果
  type SecureTransmissionResult = {
    original_data: String,
    encrypted_data: String,
    encryption_time_ns: Int,
    decryption_time_ns: Int,
    data_size: Int,
    protocol_used: SecurityProtocol,
    security_level: String
  }
  
  // 创建安全传输管理器
  let create_secure_transmission_manager = fn(config: EncryptionConfig) {
    let mut transmission_logs = []
    
    {
      config,
      transmission_logs,
      
      // 加密数据
      encrypt_data: fn(data: String) {
        let start_time = Time::nano()
        
        let encrypted_data = match config.algorithm {
          AES256 => {
            // 简化的AES256加密（使用base64模拟）
            "aes256:" + data.to_base64()
          }
          ChaCha20 => {
            // 简化的ChaCha20加密
            "chacha20:" + data.to_base64()
          }
          RSA => {
            // 简化的RSA加密
            let chunks = data.split(32)  // 分块
            let encrypted_chunks = chunks.map(fn(chunk) { 
              "rsa:" + chunk.to_base64() 
            })
            encrypted_chunks.join("|")
          }
          ECDSA => {
            // 简化的ECDSA加密
            "ecdsa:" + data.to_base64()
          }
          TLS => {
            // TLS加密（取决于配置的协议版本）
            match config.protocol {
              TLS_1_2 => "tls12:" + data.to_base64()
              TLS_1_3 => "tls13:" + data.to_base64()
              _ => "tls:" + data.to_base64()
            }
          }
        }
        
        let end_time = Time::nano()
        let encryption_time = end_time - start_time
        
        {
          encrypted_data: encrypted_data,
          encryption_time_ns: encryption_time
        }
      },
      
      // 解密数据
      decrypt_data: fn(encrypted_data: String) {
        let start_time = Time::nano()
        
        let decrypted_data = if encrypted_data.starts_with("aes256:") {
          encrypted_data.substring(7).from_base64()
        } else if encrypted_data.starts_with("chacha20:") {
          encrypted_data.substring(9).from_base64()
        } else if encrypted_data.starts_with("ecdsa:") {
          encrypted_data.substring(6).from_base64()
        } else if encrypted_data.starts_with("tls12:") {
          encrypted_data.substring(6).from_base64()
        } else if encrypted_data.starts_with("tls13:") {
          encrypted_data.substring(6).from_base64()
        } else if encrypted_data.starts_with("tls:") {
          encrypted_data.substring(4).from_base64()
        } else if encrypted_data.contains("|") && encrypted_data.contains("rsa:") {
          // RSA分块解密
          let chunks = encrypted_data.split("|")
          let decrypted_chunks = chunks.map(fn(chunk) { 
            if chunk.starts_with("rsa:") {
              chunk.substring(4).from_base64()
            } else {
              chunk
            }
          })
          decrypted_chunks.join("")
        } else {
          encrypted_data  // 无法解密，返回原始数据
        }
        
        let end_time = Time::nano()
        let decryption_time = end_time - start_time
        
        {
          decrypted_data: decrypted_data,
          decryption_time_ns: decryption_time
        }
      },
      
      // 安全传输数据
      secure_transmit: fn(data: String, destination: String) {
        // 加密数据
        let encryption_result = this.encrypt_data(data)
        
        // 模拟网络传输
        let transmission_start = Time::nano()
        Thread::sleep(10)  // 模拟网络延迟
        let transmission_end = Time::nano()
        let transmission_time = transmission_end - transmission_start
        
        // 解密数据
        let decryption_result = this.decrypt_data(encryption_result.encrypted_data)
        
        // 验证数据完整性
        let integrity_check = data == decryption_result.decrypted_data
        
        // 计算安全级别
        let security_level = match config.algorithm {
          AES256 => if config.perfect_forward_secrecy { "high" } else { "medium" }
          ChaCha20 => "high"
          RSA => if config.key_size >= 2048 { "medium" } else { "low" }
          ECDSA => "high"
          TLS => match config.protocol {
            TLS_1_3 => "high"
            TLS_1_2 => "medium"
            _ => "low"
          }
        }
        
        let result = {
          original_data: data,
          encrypted_data: encryption_result.encrypted_data,
          encryption_time_ns: encryption_result.encryption_time_ns,
          decryption_time_ns: decryption_result.decryption_time_ns,
          data_size: data.length(),
          protocol_used: config.protocol,
          security_level: security_level
        }
        
        // 记录传输日志
        let log_entry = {
          timestamp: Time::now(),
          destination: destination,
          data_size: data.length(),
          encryption_time: encryption_result.encryption_time_ns,
          transmission_time: transmission_time,
          decryption_time: decryption_result.decryption_time_ns,
          integrity_check: integrity_check,
          protocol: config.protocol,
          algorithm: config.algorithm
        }
        
        transmission_logs = transmission_logs.push(log_entry)
        
        result
      },
      
      // 获取传输日志
      get_transmission_logs: fn() {
        transmission_logs
      },
      
      // 验证安全配置
      validate_security_config: fn() {
        let issues = []
        
        // 检查密钥长度
        match config.algorithm {
          RSA => {
            if config.key_size < 2048 {
              issues = issues.push("RSA key size should be at least 2048 bits")
            }
          }
          _ => {
            if config.key_size < 128 {
              issues = issues.push("Key size should be at least 128 bits")
            }
          }
        }
        
        // 检查协议版本
        match config.protocol {
          TLS_1_2 => {
            if not(config.perfect_forward_secrecy) {
              issues = issues.push("TLS 1.2 should use perfect forward secrecy")
            }
          }
          _ => ()
        }
        
        // 检查证书验证
        if not(config.certificate_validation) {
          issues = issues.push("Certificate validation should be enabled")
        }
        
        issues
      }
    }
  }
  
  // 测试不同加密配置
  let encryption_configs = [
    {
      algorithm: AES256,
      key_size: 256,
      protocol: TLS_1_3,
      certificate_validation: true,
      compression: false,
      perfect_forward_secrecy: true
    },
    {
      algorithm: ChaCha20,
      key_size: 256,
      protocol: TLS_1_3,
      certificate_validation: true,
      compression: false,
      perfect_forward_secrecy: true
    },
    {
      algorithm: RSA,
      key_size: 2048,
      protocol: TLS_1_2,
      certificate_validation: true,
      compression: false,
      perfect_forward_secrecy: true
    },
    {
      algorithm: TLS,
      key_size: 128,
      protocol: HTTPS,
      certificate_validation: true,
      compression: true,
      perfect_forward_secrecy: false
    }
  ]
  
  let test_data = JSON::stringify({
    trace_id: "trace-12345",
    span_id: "span-67890",
    operation_name: "secure_operation",
    start_time: Time::now(),
    end_time: Time::now() + 100,
    attributes: [
      ("service.name", "secure-service"),
      ("user.id", "user-12345"),
      ("sensitive.data", "encrypted-value")
    ]
  })
  
  let transmission_results = []
  
  for config in encryption_configs {
    let secure_manager = create_secure_transmission_manager(config)
    
    // 验证安全配置
    let security_issues = secure_manager.validate_security_config()
    
    // 对于不安全的配置，应该有问题
    if config.algorithm == RSA && config.key_size == 2048 {
      assert_eq(security_issues.length(), 0)  // 2048位RSA是安全的
    } else if config.algorithm == TLS && not(config.perfect_forward_secrecy) {
      assert_true(security_issues.length() > 0)  // 应该有安全问题
    }
    
    // 执行安全传输
    let result = secure_manager.secure_transmit(test_data, "https://api.example.com/telemetry")
    
    // 验证传输结果
    assert_eq(result.original_data, test_data)
    assert_true(result.encrypted_data != test_data)
    assert_true(result.encryption_time_ns > 0)
    assert_true(result.decryption_time_ns > 0)
    assert_eq(result.data_size, test_data.length())
    
    // 验证加密后的数据格式
    match config.algorithm {
      AES256 => assert_true(result.encrypted_data.starts_with("aes256:"))
      ChaCha20 => assert_true(result.encrypted_data.starts_with("chacha20:"))
      RSA => assert_true(result.encrypted_data.contains("rsa:"))
      ECDSA => assert_true(result.encrypted_data.starts_with("ecdsa:"))
      TLS => {
        match config.protocol {
          TLS_1_2 => assert_true(result.encrypted_data.starts_with("tls12:"))
          TLS_1_3 => assert_true(result.encrypted_data.starts_with("tls13:"))
          HTTPS => assert_true(result.encrypted_data.starts_with("tls:"))
          _ => ()
        }
      }
    }
    
    // 验证安全级别
    assert_true(result.security_level == "high" || result.security_level == "medium" || result.security_level == "low")
    
    transmission_results = transmission_results.push(result)
  }
  
  // 比较不同算法的性能
  let aes256_result = transmission_results[0]
  let chacha20_result = transmission_results[1]
  let rsa_result = transmission_results[2]
  
  // AES256和ChaCha20应该有相似的性能
  let aes_total_time = aes256_result.encryption_time_ns + aes256_result.decryption_time_ns
  let chacha_total_time = chacha20_result.encryption_time_ns + chacha20_result.decryption_time_ns
  
  // RSA可能更慢（由于分块处理）
  let rsa_total_time = rsa_result.encryption_time_ns + rsa_result.decryption_time_ns
  
  // 验证传输日志
  let secure_manager = create_secure_transmission_manager(encryption_configs[0])
  secure_manager.secure_transmit(test_data, "https://api.example.com/telemetry")
  
  let logs = secure_manager.get_transmission_logs()
  assert_eq(logs.length(), 1)
  
  let log = logs[0]
  assert_true(log.timestamp > 0)
  assert_eq(log.destination, "https://api.example.com/telemetry")
  assert_eq(log.data_size, test_data.length())
  assert_true(log.encryption_time > 0)
  assert_true(log.transmission_time > 0)
  assert_true(log.decryption_time > 0)
  assert_true(log.integrity_check)
}

// 测试3: 访问控制和权限管理
test "访问控制和权限管理" {
  // 定义权限类型
  enum PermissionType {
    ReadTelemetry
    WriteTelemetry
    DeleteTelemetry
    ManageUsers
    ConfigureSystem
    ViewMetrics
    ExportData
    AdminAccess
  }
  
  // 定义角色
  enum Role {
    Viewer
    Analyst
    Operator
    Admin
    SuperAdmin
  }
  
  // 定义用户
  type User = {
    id: String,
    username: String,
    email: String,
    role: Role,
    permissions: Array[PermissionType],
    active: Bool,
    last_login: Int,
    created_at: Int
  }
  
  // 定义访问控制策略
  type AccessControlPolicy = {
    resource: String,
    required_permissions: Array[PermissionType],
    ip_whitelist: Array[String],
    time_restrictions: Option[TimeRestriction],
    mfa_required: Bool
  }
  
  type TimeRestriction = {
    start_hour: Int,
    end_hour: Int,
    days_of_week: Array[Int]
  }
  
  // 定义访问请求
  type AccessRequest = {
    user_id: String,
    resource: String,
    action: String,
    ip_address: String,
    timestamp: Int,
    user_agent: String
  }
  
  // 定义访问控制结果
  type AccessControlResult = {
    allowed: Bool,
    reason: String,
    user_role: Role,
    required_permissions: Array[PermissionType],
    missing_permissions: Array[PermissionType],
    additional_checks: Array[String]
  }
  
  // 创建访问控制管理器
  let create_access_control_manager = fn() {
    let users = Map::empty()  // user_id -> User
    let policies = Map::empty()  // resource -> AccessControlPolicy
    let access_logs = []
    
    {
      users,
      policies,
      access_logs,
      
      // 添加用户
      add_user: fn(user: User) {
        users = Map::insert(users, user.id, user)
      },
      
      // 添加访问控制策略
      add_policy: fn(policy: AccessControlPolicy) {
        policies = Map::insert(policies, policy.resource, policy)
      },
      
      // 检查权限
      check_permissions: fn(user: User, required_permissions: Array[PermissionType]) {
        let user_permissions = user.permissions
        let mut missing_permissions = []
        
        for permission in required_permissions {
          if not(user_permissions.contains(permission)) {
            missing_permissions = missing_permissions.push(permission)
          }
        }
        
        {
          has_all_permissions: missing_permissions.length() == 0,
          missing_permissions: missing_permissions
        }
      },
      
      // 验证IP白名单
      validate_ip_whitelist: fn(ip_address: String, whitelist: Array[String]) {
        if whitelist.length() == 0 {
          true  // 没有白名单限制
        } else {
          whitelist.some(fn(allowed_ip) { 
            // 简化的IP匹配（实际实现会更复杂）
            if allowed_ip.contains("/") {
              // CIDR表示法
              let network_parts = allowed_ip.split("/")
              let network = network_parts[0]
              let prefix_length = network_parts[1].to_int()
              
              // 简化实现：只检查前缀
              if prefix_length >= 24 {
                ip_address.starts_with(network.substring(0, network.length() - 1))
              } else if prefix_length >= 16 {
                ip_address.starts_with(network.substring(0, network.length() - 2))
              } else {
                ip_address.starts_with(network.substring(0, network.length() - 3))
              }
            } else {
              ip_address == allowed_ip
            }
          })
        }
      },
      
      // 验证时间限制
      validate_time_restrictions: fn(timestamp: Int, time_restriction: TimeRestriction) {
        let request_time = timestamp
        let request_hour = (request_time / (60 * 60 * 1000)) % 24
        let request_day = ((request_time / (24 * 60 * 60 * 1000)) + 4) % 7  // 调整为周日开始
        
        let hour_valid = request_hour >= time_restriction.start_hour && 
                        request_hour <= time_restriction.end_hour
        let day_valid = time_restriction.days_of_week.some(fn(allowed_day) { 
          allowed_day == request_day 
        })
        
        hour_valid && day_valid
      },
      
      // 处理访问请求
      handle_access_request: fn(request: AccessRequest) {
        let mut allowed = false
        let mut reason = "Access denied"
        let mut additional_checks = []
        
        // 获取用户信息
        match Map::get(users, request.user_id) {
          Some(user) => {
            if not(user.active) {
              reason = "User account is inactive"
            } else {
              // 获取资源策略
              match Map::get(policies, request.resource) {
                Some(policy) => {
                  // 检查权限
                  let permission_check = this.check_permissions(user, policy.required_permissions)
                  
                  if not(permission_check.has_all_permissions) {
                    reason = "Insufficient permissions"
                  } else {
                    // 检查IP白名单
                    if not(this.validate_ip_whitelist(request.ip_address, policy.ip_whitelist)) {
                      reason = "IP address not in whitelist"
                      additional_checks = additional_checks.push("ip_check_failed")
                    } else {
                      // 检查时间限制
                      match policy.time_restrictions {
                        Some(time_restriction) => {
                          if not(this.validate_time_restrictions(request.timestamp, time_restriction)) {
                            reason = "Access outside allowed time window"
                            additional_checks = additional_checks.push("time_restriction_failed")
                          } else {
                            allowed = true
                            reason = "Access granted"
                          }
                        }
                        None => {
                          allowed = true
                          reason = "Access granted"
                        }
                      }
                      
                      // 检查MFA要求
                      if allowed && policy.mfa_required {
                        // 简化实现：假设MFA已验证
                        additional_checks = additional_checks.push("mfa_verified")
                      }
                    }
                  }
                  
                  let result = {
                    allowed: allowed,
                    reason: reason,
                    user_role: user.role,
                    required_permissions: policy.required_permissions,
                    missing_permissions: if permission_check.has_all_permissions { [] } else { permission_check.missing_permissions },
                    additional_checks: additional_checks
                  }
                  
                  // 记录访问日志
                  let log_entry = {
                    timestamp: request.timestamp,
                    user_id: request.user_id,
                    resource: request.resource,
                    action: request.action,
                    ip_address: request.ip_address,
                    allowed: allowed,
                    reason: reason,
                    user_role: user.role
                  }
                  
                  access_logs = access_logs.push(log_entry)
                  
                  result
                }
                None => {
                  // 没有策略，默认拒绝
                  let result = {
                    allowed: false,
                    reason: "No access policy found for resource",
                    user_role: user.role,
                    required_permissions: [],
                    missing_permissions: [],
                    additional_checks: []
                  }
                  
                  let log_entry = {
                    timestamp: request.timestamp,
                    user_id: request.user_id,
                    resource: request.resource,
                    action: request.action,
                    ip_address: request.ip_address,
                    allowed: false,
                    reason: result.reason,
                    user_role: user.role
                  }
                  
                  access_logs = access_logs.push(log_entry)
                  
                  result
                }
              }
            }
          }
          None => {
            // 用户不存在
            reason = "User not found"
            
            let result = {
              allowed: false,
              reason: reason,
              user_role: Viewer,  // 默认角色
              required_permissions: [],
              missing_permissions: [],
              additional_checks: []
            }
            
            let log_entry = {
              timestamp: request.timestamp,
              user_id: request.user_id,
              resource: request.resource,
              action: request.action,
              ip_address: request.ip_address,
              allowed: false,
              reason: reason,
              user_role: Viewer
            }
            
            access_logs = access_logs.push(log_entry)
            
            result
          }
        }
      },
      
      // 获取访问日志
      get_access_logs: fn() {
        access_logs
      }
    }
  }
  
  // 创建访问控制管理器
  let acm = create_access_control_manager()
  
  // 创建测试用户
  let viewer_user = {
    id: "user-001",
    username: "viewer",
    email: "viewer@example.com",
    role: Viewer,
    permissions: [ReadTelemetry, ViewMetrics],
    active: true,
    last_login: Time::now() - 86400000,  // 1天前
    created_at: Time::now() - 86400000 * 30  // 30天前创建
  }
  
  let analyst_user = {
    id: "user-002",
    username: "analyst",
    email: "analyst@example.com",
    role: Analyst,
    permissions: [ReadTelemetry, WriteTelemetry, ViewMetrics, ExportData],
    active: true,
    last_login: Time::now() - 3600000,  // 1小时前
    created_at: Time::now() - 86400000 * 15  // 15天前创建
  }
  
  let admin_user = {
    id: "user-003",
    username: "admin",
    email: "admin@example.com",
    role: Admin,
    permissions: [ReadTelemetry, WriteTelemetry, DeleteTelemetry, ManageUsers, ConfigureSystem, ViewMetrics, ExportData, AdminAccess],
    active: true,
    last_login: Time::now() - 1800000,  // 30分钟前
    created_at: Time::now() - 86400000 * 60  // 60天前创建
  }
  
  let inactive_user = {
    id: "user-004",
    username: "inactive",
    email: "inactive@example.com",
    role: Viewer,
    permissions: [ReadTelemetry],
    active: false,  // 非活跃用户
    last_login: Time::now() - 86400000 * 30,  // 30天前
    created_at: Time::now() - 86400000 * 90  // 90天前创建
  }
  
  // 添加用户
  acm.add_user(viewer_user)
  acm.add_user(analyst_user)
  acm.add_user(admin_user)
  acm.add_user(inactive_user)
  
  // 添加访问控制策略
  acm.add_policy({
    resource: "/telemetry/read",
    required_permissions: [ReadTelemetry],
    ip_whitelist: ["192.168.1.0/24", "10.0.0.0/8"],
    time_restrictions: None,
    mfa_required: false
  })
  
  acm.add_policy({
    resource: "/telemetry/write",
    required_permissions: [WriteTelemetry],
    ip_whitelist: ["10.0.0.0/8"],
    time_restrictions: Some({
      start_hour: 9,
      end_hour: 17,
      days_of_week: [1, 2, 3, 4, 5]  // 工作日
    }),
    mfa_required: true
  })
  
  acm.add_policy({
    resource: "/admin/users",
    required_permissions: [ManageUsers],
    ip_whitelist: ["192.168.1.100"],
    time_restrictions: None,
    mfa_required: true
  })
  
  // 测试访问控制
  
  // 测试1: Viewer用户尝试读取遥测数据（应该成功）
  let viewer_read_request = {
    user_id: "user-001",
    resource: "/telemetry/read",
    action: "GET",
    ip_address: "192.168.1.50",
    timestamp: Time::now(),
    user_agent: "Mozilla/5.0"
  }
  
  let viewer_read_result = acm.handle_access_request(viewer_read_request)
  assert_true(viewer_read_result.allowed)
  assert_eq(viewer_read_result.reason, "Access granted")
  assert_eq(viewer_read_result.user_role, Viewer)
  assert_eq(viewer_read_result.required_permissions, [ReadTelemetry])
  assert_eq(viewer_read_result.missing_permissions.length(), 0)
  
  // 测试2: Viewer用户尝试写入遥测数据（应该失败）
  let viewer_write_request = {
    user_id: "user-001",
    resource: "/telemetry/write",
    action: "POST",
    ip_address: "192.168.1.50",
    timestamp: Time::now(),
    user_agent: "Mozilla/5.0"
  }
  
  let viewer_write_result = acm.handle_access_request(viewer_write_request)
  assert_false(viewer_write_result.allowed)
  assert_eq(viewer_write_result.reason, "Insufficient permissions")
  assert_eq(viewer_write_result.user_role, Viewer)
  assert_eq(viewer_write_result.required_permissions, [WriteTelemetry])
  assert_true(viewer_write_result.missing_permissions.contains(WriteTelemetry))
  
  // 测试3: Analyst用户尝试写入遥测数据（应该成功）
  let analyst_write_request = {
    user_id: "user-002",
    resource: "/telemetry/write",
    action: "POST",
    ip_address: "10.0.0.100",
    timestamp: Time::now(),
    user_agent: "Mozilla/5.0"
  }
  
  let analyst_write_result = acm.handle_access_request(analyst_write_request)
  assert_true(analyst_write_result.allowed)
  assert_eq(analyst_write_result.reason, "Access granted")
  assert_eq(analyst_write_result.user_role, Analyst)
  assert_true(analyst_write_result.additional_checks.contains("mfa_verified"))
  
  // 测试4: Analyst用户尝试从非允许IP写入（应该失败）
  let analyst_wrong_ip_request = {
    user_id: "user-002",
    resource: "/telemetry/write",
    action: "POST",
    ip_address: "192.168.1.100",  // 不在白名单中
    timestamp: Time::now(),
    user_agent: "Mozilla/5.0"
  }
  
  let analyst_wrong_ip_result = acm.handle_access_request(analyst_wrong_ip_request)
  assert_false(analyst_wrong_ip_result.allowed)
  assert_eq(analyst_wrong_ip_result.reason, "IP address not in whitelist")
  assert_true(analyst_wrong_ip_result.additional_checks.contains("ip_check_failed"))
  
  // 测试5: 非活跃用户尝试访问（应该失败）
  let inactive_request = {
    user_id: "user-004",
    resource: "/telemetry/read",
    action: "GET",
    ip_address: "192.168.1.50",
    timestamp: Time::now(),
    user_agent: "Mozilla/5.0"
  }
  
  let inactive_result = acm.handle_access_request(inactive_request)
  assert_false(inactive_result.allowed)
  assert_eq(inactive_result.reason, "User account is inactive")
  
  // 测试6: Admin用户尝试管理用户（应该成功）
  let admin_manage_request = {
    user_id: "user-003",
    resource: "/admin/users",
    action: "POST",
    ip_address: "192.168.1.100",
    timestamp: Time::now(),
    user_agent: "Mozilla/5.0"
  }
  
  let admin_manage_result = acm.handle_access_request(admin_manage_request)
  assert_true(admin_manage_result.allowed)
  assert_eq(admin_manage_result.reason, "Access granted")
  assert_eq(admin_manage_result.user_role, Admin)
  assert_true(admin_manage_result.additional_checks.contains("mfa_verified"))
  
  // 测试7: 不存在的用户尝试访问（应该失败）
  let nonexistent_request = {
    user_id: "user-999",
    resource: "/telemetry/read",
    action: "GET",
    ip_address: "192.168.1.50",
    timestamp: Time::now(),
    user_agent: "Mozilla/5.0"
  }
  
  let nonexistent_result = acm.handle_access_request(nonexistent_request)
  assert_false(nonexistent_result.allowed)
  assert_eq(nonexistent_result.reason, "User not found")
  assert_eq(nonexistent_result.user_role, Viewer)
  
  // 验证访问日志
  let access_logs = acm.get_access_logs()
  assert_eq(access_logs.length(), 7)  // 我们进行了7次访问请求
  
  // 验证日志内容
  let successful_logs = access_logs.filter(fn(log) { log.allowed })
  let failed_logs = access_logs.filter(fn(log) { not(log.allowed) })
  
  assert_eq(successful_logs.length(), 3)  // 3次成功访问
  assert_eq(failed_logs.length(), 4)     // 4次失败访问
  
  // 验证成功访问的日志
  for log in successful_logs {
    assert_true(log.timestamp > 0)
    assert_true(log.user_id.length() > 0)
    assert_true(log.resource.length() > 0)
    assert_true(log.reason == "Access granted")
  }
  
  // 验证失败访问的日志
  for log in failed_logs {
    assert_true(log.timestamp > 0)
    assert_true(log.user_id.length() > 0)
    assert_true(log.resource.length() > 0)
    assert_true(log.reason != "Access granted")
  }
}
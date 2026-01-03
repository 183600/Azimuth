// Azimuth 遥测安全加密测试用例
// 专注于测试数据安全特性和加密功能

// 测试1: 数据传输加密
test "数据传输加密" {
  // 定义加密算法
  enum EncryptionAlgorithm {
    AES256_GCM
    ChaCha20_Poly1305
    AES128_CBC
    RSA_OAEP
  }
  
  // 定义加密密钥
  type EncryptionKey = {
    algorithm: EncryptionAlgorithm,
    key_data: Array[Byte>,
    iv: Array[Byte>,
    key_id: String
  }
  
  // 定义加密数据包
  type EncryptedPacket = {
    encrypted_data: Array[Byte>,
    auth_tag: Array[Byte>,
    algorithm: EncryptionAlgorithm,
    key_id: String,
    timestamp: Int
  }
  
  // 创建加密器
  let encryptor = fn(key: EncryptionKey) {
    fn(data: Array[Byte>) -> EncryptedPacket {
      match key.algorithm {
        EncryptionAlgorithm::AES256_GCM => {
          // 模拟AES256-GCM加密
          let encrypted_data = data.map(fn(b) { b ^ 0xAA })  // 简化的XOR加密
          let auth_tag = [0x01, 0x02, 0x03, 0x04]  // 模拟认证标签
          
          EncryptedPacket({
            encrypted_data: encrypted_data,
            auth_tag: auth_tag,
            algorithm: EncryptionAlgorithm::AES256_GCM,
            key_id: key.key_id,
            timestamp: Time::now()
          })
        }
        EncryptionAlgorithm::ChaCha20_Poly1305 => {
          // 模拟ChaCha20-Poly1305加密
          let encrypted_data = data.map(fn(b) { b ^ 0xBB })  // 简化的XOR加密
          let auth_tag = [0x11, 0x12, 0x13, 0x14]  // 模拟认证标签
          
          EncryptedPacket({
            encrypted_data: encrypted_data,
            auth_tag: auth_tag,
            algorithm: EncryptionAlgorithm::ChaCha20_Poly1305,
            key_id: key.key_id,
            timestamp: Time::now()
          })
        }
        EncryptionAlgorithm::AES128_CBC => {
          // 模拟AES128-CBC加密
          let encrypted_data = data.map(fn(b) { b ^ 0xCC })  // 简化的XOR加密
          let auth_tag = []  // CBC模式没有认证标签
          
          EncryptedPacket({
            encrypted_data: encrypted_data,
            auth_tag: auth_tag,
            algorithm: EncryptionAlgorithm::AES128_CBC,
            key_id: key.key_id,
            timestamp: Time::now()
          })
        }
        EncryptionAlgorithm::RSA_OAEP => {
          // 模拟RSA-OAEP加密（仅用于小数据）
          let encrypted_data = data.map(fn(b) { b ^ 0xDD })  // 简化的XOR加密
          let auth_tag = [0x21, 0x22, 0x23, 0x24]  // 模拟认证标签
          
          EncryptedPacket({
            encrypted_data: encrypted_data,
            auth_tag: auth_tag,
            algorithm: EncryptionAlgorithm::RSA_OAEP,
            key_id: key.key_id,
            timestamp: Time::now()
          })
        }
      }
    }
  }
  
  // 创建解密器
  let decryptor = fn(key: EncryptionKey) {
    fn(packet: EncryptedPacket) -> Array[Byte] {
      // 验证算法匹配
      if packet.algorithm != key.algorithm {
        return []  // 算法不匹配，解密失败
      }
      
      // 验证密钥ID
      if packet.key_id != key.key_id {
        return []  // 密钥ID不匹配，解密失败
      }
      
      match packet.algorithm {
        EncryptionAlgorithm::AES256_GCM => {
          // 验证认证标签
          if packet.auth_tag.length() != 4 {
            return []  // 认证标签无效
          }
          
          // 模拟AES256-GCM解密
          packet.encrypted_data.map(fn(b) { b ^ 0xAA })  // 简化的XOR解密
        }
        EncryptionAlgorithm::ChaCha20_Poly1305 => {
          // 验证认证标签
          if packet.auth_tag.length() != 4 {
            return []  // 认证标签无效
          }
          
          // 模拟ChaCha20-Poly1305解密
          packet.encrypted_data.map(fn(b) { b ^ 0xBB })  // 简化的XOR解密
        }
        EncryptionAlgorithm::AES128_CBC => {
          // CBC模式没有认证标签验证
          packet.encrypted_data.map(fn(b) { b ^ 0xCC })  // 简化的XOR解密
        }
        EncryptionAlgorithm::RSA_OAEP => {
          // 验证认证标签
          if packet.auth_tag.length() != 4 {
            return []  // 认证标签无效
          }
          
          // 模拟RSA-OAEP解密
          packet.encrypted_data.map(fn(b) { b ^ 0xDD })  // 简化的XOR解密
        }
      }
    }
  }
  
  // 创建不同算法的密钥
  let aes256_key = EncryptionKey({
    algorithm: EncryptionAlgorithm::AES256_GCM,
    key_data: Array::new(32, 0x42),  // 256位密钥
    iv: Array::new(12, 0x24),        // 96位IV
    key_id: "aes256-key-001"
  })
  
  let chacha20_key = EncryptionKey({
    algorithm: EncryptionAlgorithm::ChaCha20_Poly1305,
    key_data: Array::new(32, 0x43),  // 256位密钥
    iv: Array::new(12, 0x34),        // 96位IV
    key_id: "chacha20-key-001"
  })
  
  let aes128_key = EncryptionKey({
    algorithm: EncryptionAlgorithm::AES128_CBC,
    key_data: Array::new(16, 0x44),  // 128位密钥
    iv: Array::new(16, 0x44),        // 128位IV
    key_id: "aes128-key-001"
  })
  
  // 创建测试数据
  let telemetry_data = "cpu_usage:45.2,memory_usage:78.5,timestamp:1735689600000".to_bytes()
  
  // 测试AES256-GCM加密
  let aes256_encrypt = encryptor(aes256_key)
  let aes256_encrypted = aes256_encrypt(telemetry_data)
  
  assert_eq(aes256_encrypted.algorithm, EncryptionAlgorithm::AES256_GCM)
  assert_eq(aes256_encrypted.key_id, "aes256-key-001")
  assert_eq(aes256_encrypted.auth_tag.length(), 4)
  assert_ne(aes256_encrypted.encrypted_data, telemetry_data)  // 加密后数据应该不同
  
  // 测试AES256-GCM解密
  let aes256_decrypt = decryptor(aes256_key)
  let aes256_decrypted = aes256_decrypt(aes256_encrypted)
  
  assert_eq(aes256_decrypted, telemetry_data)  // 解密后应该恢复原始数据
  
  // 测试ChaCha20-Poly1305加密
  let chacha20_encrypt = encryptor(chacha20_key)
  let chacha20_encrypted = chacha20_encrypt(telemetry_data)
  
  assert_eq(chacha20_encrypted.algorithm, EncryptionAlgorithm::ChaCha20_Poly1305)
  assert_eq(chacha20_encrypted.key_id, "chacha20-key-001")
  assert_eq(chacha20_encrypted.auth_tag.length(), 4)
  
  // 测试ChaCha20-Poly1305解密
  let chacha20_decrypt = decryptor(chacha20_key)
  let chacha20_decrypted = chacha20_decrypt(chacha20_encrypted)
  
  assert_eq(chacha20_decrypted, telemetry_data)  // 解密后应该恢复原始数据
  
  // 测试错误处理：错误的密钥
  let wrong_key = EncryptionKey({
    algorithm: EncryptionAlgorithm::AES256_GCM,
    key_data: Array::new(32, 0x99),  // 不同的密钥
    iv: Array::new(12, 0x99),
    key_id: "wrong-key"
  })
  
  let wrong_decrypt = decryptor(wrong_key)
  let wrong_decrypted = wrong_decrypt(aes256_encrypted)
  
  assert_eq(wrong_decrypted.length(), 0)  // 使用错误的密钥应该解密失败
  
  // 测试错误处理：错误的算法
  let wrong_algorithm_key = EncryptionKey({
    algorithm: EncryptionAlgorithm::ChaCha20_Poly1305,  // 不同的算法
    key_data: Array::new(32, 0x42),
    iv: Array::new(12, 0x24),
    key_id: "aes256-key-001"  // 相同的密钥ID
  })
  
  let wrong_algorithm_decrypt = decryptor(wrong_algorithm_key)
  let wrong_algorithm_decrypted = wrong_algorithm_decrypt(aes256_encrypted)
  
  assert_eq(wrong_algorithm_decrypted.length(), 0)  // 使用错误的算法应该解密失败
}

// 测试2: 密钥管理和轮换
test "密钥管理和轮换" {
  // 定义密钥状态
  enum KeyStatus {
    Active
    Inactive
    Deprecated
    Compromised
  }
  
  // 定义密钥元数据
  type KeyMetadata = {
    key_id: String,
    algorithm: EncryptionAlgorithm,
    status: KeyStatus,
    creation_time: Int,
    expiration_time: Int,
    last_rotation: Int,
    rotation_interval_days: Int
  }
  
  // 创建密钥管理器
  let key_manager = fn() {
    let mut keys = Array::empty()
    
    fn(key_metadata: KeyMetadata) -> KeyMetadata {
      // 添加新密钥
      keys = keys.push(key_metadata)
      key_metadata
    }
    
    fn rotate_key(key_id: String) -> Option[KeyMetadata> {
      // 查找密钥
      let key_index = keys.find_index(fn(k) { k.key_id == key_id })
      
      match key_index {
        Some(index) => {
          let key = keys[index]
          let current_time = Time::now()
          
          // 创建新密钥
          let new_key = {
            ..key,
            key_id: key.key_id + "-v" + ((key.last_rotation / 86400000) + 1).to_string(),
            creation_time: current_time,
            expiration_time: current_time + (key.rotation_interval_days * 86400000),
            last_rotation: current_time,
            status: KeyStatus::Active
          }
          
          // 更新旧密钥状态
          let mut updated_keys = keys.copy()
          let mut old_key = updated_keys[index]
          old_key = { ..old_key, status: KeyStatus::Inactive }
          updated_keys[index] = old_key
          
          // 添加新密钥
          updated_keys = updated_keys.push(new_key)
          keys = updated_keys
          
          Some(new_key)
        }
        None => None
      }
    }
    
    fn expire_keys() -> Array[KeyMetadata> {
      let current_time = Time::now()
      let mut expired_keys = Array::empty()
      
      // 查找过期密钥
      for i in 0..keys.length() {
        let key = keys[i]
        if key.status == KeyStatus::Active && key.expiration_time <= current_time {
          let mut updated_keys = keys.copy()
          let mut expired_key = updated_keys[i]
          expired_key = { ..expired_key, status: KeyStatus::Deprecated }
          updated_keys[i] = expired_key
          keys = updated_keys
          
          expired_keys = expired_keys.push(expired_key)
        }
      }
      
      expired_keys
    }
    
    fn get_active_keys() -> Array<KeyMetadata> {
      keys.filter(fn(k) { k.status == KeyStatus::Active })
    }
    
    fn get_key_by_id(key_id: String) -> Option<KeyMetadata> {
      keys.find(fn(k) { k.key_id == key_id })
    }
  }
  
  let manager = key_manager()
  
  // 创建初始密钥
  let key1 = manager(KeyMetadata({
    key_id: "telemetry-key-001",
    algorithm: EncryptionAlgorithm::AES256_GCM,
    status: KeyStatus::Active,
    creation_time: 1735689600000,  // 2025-01-01
    expiration_time: 1735689600000 + (30 * 86400000),  // 30天后过期
    last_rotation: 1735689600000,
    rotation_interval_days: 30
  }))
  
  let key2 = manager(KeyMetadata({
    key_id: "telemetry-key-002",
    algorithm: EncryptionAlgorithm::ChaCha20_Poly1305,
    status: KeyStatus::Active,
    creation_time: 1735689600000,
    expiration_time: 1735689600000 + (60 * 86400000),  // 60天后过期
    last_rotation: 1735689600000,
    rotation_interval_days: 60
  }))
  
  // 验证密钥创建
  assert_eq(key1.key_id, "telemetry-key-001")
  assert_eq(key1.status, KeyStatus::Active)
  assert_eq(key2.key_id, "telemetry-key-002")
  assert_eq(key2.status, KeyStatus::Active)
  
  // 验证活跃密钥
  let active_keys = manager.get_active_keys()
  assert_eq(active_keys.length(), 2)
  
  // 测试密钥轮换
  let rotated_key = manager.rotate_key("telemetry-key-001")
  assert_true(rotated_key.is_some())
  
  let new_key = rotated_key.unwrap()
  assert_eq(new_key.key_id, "telemetry-key-001-v1")
  assert_eq(new_key.status, KeyStatus::Active)
  assert_true(new_key.creation_time > key1.creation_time)
  
  // 验证旧密钥状态
  let old_key = manager.get_key_by_id("telemetry-key-001")
  assert_true(old_key.is_some())
  assert_eq(old_key.unwrap().status, KeyStatus::Inactive)
  
  // 验证活跃密钥数量
  let updated_active_keys = manager.get_active_keys()
  assert_eq(updated_active_keys.length(), 2)  // 仍然是2个，但一个是新的
  
  // 测试密钥过期
  let expired_keys = manager.expire_keys()
  assert_eq(expired_keys.length(), 0)  // 当前时间下没有密钥过期
  
  // 模拟时间流逝（31天后）
  let future_time = 1735689600000 + (31 * 86400000)
  // 注意：这里无法真正修改系统时间，只是模拟逻辑
  
  // 测试无效密钥轮换
  let invalid_rotation = manager.rotate_key("non-existent-key")
  assert_true(invalid_rotation.is_none())
}

// 测试3: 数据完整性验证
test "数据完整性验证" {
  // 定义哈希算法
  enum HashAlgorithm {
    SHA256
    SHA384
    SHA512
    Blake3
  }
  
  // 定义数字签名
  type DigitalSignature = {
    signature_data: Array[Byte>,
    hash_algorithm: HashAlgorithm,
    signer_id: String,
    timestamp: Int
  }
  
  // 定义完整性验证结果
  type IntegrityResult = {
    is_valid: Bool,
    hash_matches: Bool,
    signature_valid: Bool,
    timestamp_valid: Bool,
    details: Array[String>
  }
  
  // 创建哈希计算器
  let hash_calculator = fn(algorithm: HashAlgorithm) {
    fn(data: Array[Byte>) -> Array[Byte> {
      match algorithm {
        HashAlgorithm::SHA256 => {
          // 模拟SHA256哈希计算
          let hash_length = 32
          let mut hash = Array::empty()
          for i in 0..hash_length {
            hash = hash.push((data.reduce(fn(acc, b) { acc + b }, 0) + i) as Byte)
          }
          hash
        }
        HashAlgorithm::SHA384 => {
          // 模拟SHA384哈希计算
          let hash_length = 48
          let mut hash = Array::empty()
          for i in 0..hash_length {
            hash = hash.push((data.reduce(fn(acc, b) { acc + b }, 0) + i * 2) as Byte)
          }
          hash
        }
        HashAlgorithm::SHA512 => {
          // 模拟SHA512哈希计算
          let hash_length = 64
          let mut hash = Array::empty()
          for i in 0..hash_length {
            hash = hash.push((data.reduce(fn(acc, b) { acc + b }, 0) + i * 3) as Byte)
          }
          hash
        }
        HashAlgorithm::Blake3 => {
          // 模拟Blake3哈希计算
          let hash_length = 32
          let mut hash = Array::empty()
          for i in 0..hash_length {
            hash = hash.push((data.reduce(fn(acc, b) { acc + b }, 0) + i * 4) as Byte)
          }
          hash
        }
      }
    }
  }
  
  // 创建数字签名器
  let digital_signer = fn(signer_id: String, hash_algorithm: HashAlgorithm) {
    fn(data: Array[Byte>) -> DigitalSignature {
      let hash_calculator_fn = hash_calculator(hash_algorithm)
      let data_hash = hash_calculator_fn(data)
      
      // 模拟数字签名（使用哈希值的简单变换）
      let signature_data = data_hash.map(fn(b) { b ^ 0x55 })
      
      DigitalSignature({
        signature_data: signature_data,
        hash_algorithm: hash_algorithm,
        signer_id: signer_id,
        timestamp: Time::now()
      })
    }
  }
  
  // 创建完整性验证器
  let integrity_verifier = fn() {
    fn(data: Array[Byte>, signature: DigitalSignature, max_age_ms: Int) -> IntegrityResult {
      let current_time = Time::now()
      let mut details = Array::empty()
      
      // 验证时间戳
      let timestamp_valid = (current_time - signature.timestamp) <= max_age_ms
      if not timestamp_valid {
        details = details.push("Signature timestamp is too old")
      }
      
      // 计算数据哈希
      let hash_calculator_fn = hash_calculator(signature.hash_algorithm)
      let calculated_hash = hash_calculator_fn(data)
      
      // 验证哈希（模拟验证）
      let expected_signature = calculated_hash.map(fn(b) { b ^ 0x55 })
      let hash_matches = expected_signature == signature.signature_data
      if not hash_matches {
        details = details.push("Data hash does not match signature")
      }
      
      // 验证签名（模拟验证）
      let signature_valid = hash_matches && signature.signer_id.length() > 0
      if not signature_valid {
        details = details.push("Digital signature is invalid")
      }
      
      // 总体验证结果
      let is_valid = timestamp_valid && hash_matches && signature_valid
      
      IntegrityResult({
        is_valid: is_valid,
        hash_matches: hash_matches,
        signature_valid: signature_valid,
        timestamp_valid: timestamp_valid,
        details: details
      })
    }
  }
  
  let verifier = integrity_verifier()
  
  // 创建测试数据
  let telemetry_data = "metric:cpu_usage,value:45.2,timestamp:1735689600000".to_bytes()
  
  // 测试SHA256签名和验证
  let sha256_signer = digital_signer("telemetry-service", HashAlgorithm::SHA256)
  let sha256_signature = sha256_signer(telemetry_data)
  
  assert_eq(sha256_signature.hash_algorithm, HashAlgorithm::SHA256)
  assert_eq(sha256_signature.signer_id, "telemetry-service")
  assert_eq(sha256_signature.signature_data.length(), 32)  // SHA256输出长度
  
  // 验证SHA256签名
  let sha256_result = verifier(telemetry_data, sha256_signature, 60000)  // 60秒有效期
  assert_true(sha256_result.is_valid)
  assert_true(sha256_result.hash_matches)
  assert_true(sha256_result.signature_valid)
  assert_true(sha256_result.timestamp_valid)
  assert_eq(sha256_result.details.length(), 0)
  
  // 测试SHA512签名和验证
  let sha512_signer = digital_signer("telemetry-service", HashAlgorithm::SHA512)
  let sha512_signature = sha512_signer(telemetry_data)
  
  assert_eq(sha512_signature.hash_algorithm, HashAlgorithm::SHA512)
  assert_eq(sha512_signature.signature_data.length(), 64)  // SHA512输出长度
  
  // 验证SHA512签名
  let sha512_result = verifier(telemetry_data, sha512_signature, 60000)
  assert_true(sha512_result.is_valid)
  assert_true(sha512_result.hash_matches)
  assert_true(sha512_result.signature_valid)
  
  // 测试数据篡改检测
  let tampered_data = "metric:cpu_usage,value:99.9,timestamp:1735689600000".to_bytes()
  let tampered_result = verifier(tampered_data, sha256_signature, 60000)
  
  assert_false(tampered_result.is_valid)
  assert_false(tampered_result.hash_matches)
  assert_false(tampered_result.signature_valid)
  assert_true(tampered_result.timestamp_valid)  // 时间戳仍然有效
  assert_true(tampered_result.details.length() > 0)
  
  // 测试过期签名
  let expired_result = verifier(telemetry_data, sha256_signature, 0)  // 0秒有效期，立即过期
  assert_false(expired_result.is_valid)
  assert_true(expired_result.hash_matches)  // 哈希仍然匹配
  assert_false(expired_result.signature_valid)  // 由于时间戳无效，整体签名无效
  assert_false(expired_result.timestamp_valid)
  assert_true(expired_result.details.length() > 0)
  
  // 测试不同签名者
  let different_signer = digital_signer("different-service", HashAlgorithm::SHA256)
  let different_signature = different_signer(telemetry_data)
  
  // 验证不同签名者的签名（应该仍然有效，因为签名者ID不参与签名计算）
  let different_signer_result = verifier(telemetry_data, different_signature, 60000)
  assert_true(different_signer_result.is_valid)
  assert_eq(different_signature.signer_id, "different-service")
}

// 测试4: 访问控制和权限管理
test "访问控制和权限管理" {
  // 定义权限类型
  enum Permission {
    ReadTelemetry
    WriteTelemetry
    DeleteTelemetry
    ManageKeys
    ConfigureSystem
  }
  
  // 定义用户角色
  enum UserRole {
    Viewer
    Operator
    Administrator
    SuperAdmin
  }
  
  // 定义用户
  type User = {
    user_id: String,
    username: String,
    role: UserRole,
    permissions: Array<Permission>,
    active: Bool,
    last_login: Int
  }
  
  // 定义访问请求
  type AccessRequest = {
    user_id: String,
    resource: String,
    action: Permission,
    timestamp: Int,
    context: Array[(String, String)>
  }
  
  // 定义访问控制结果
  type AccessResult = {
    granted: Bool,
    reason: String,
    user_role: UserRole,
    required_permission: Permission,
    additional_factors: Array[String>
  }
  
  // 创建访问控制管理器
  let access_control_manager = fn() {
    let mut users = Array::empty()
    let mut access_log = Array::empty()
    
    fn(user: User) -> User {
      users = users.push(user)
      user
    }
    
    fn check_permission(request: AccessRequest) -> AccessResult {
      // 查找用户
      let user_option = users.find(fn(u) { u.user_id == request.user_id && u.active })
      
      match user_option {
        Some(user) => {
          // 检查用户是否有所需权限
          let has_permission = user.permissions.contains(request.action)
          
          // 检查角色权限
          let role_based_permission = match user.role {
            UserRole::Viewer => request.action == Permission::ReadTelemetry
            UserRole::Operator => request.action == Permission::ReadTelemetry || 
                                 request.action == Permission::WriteTelemetry
            UserRole::Administrator => request.action != Permission::ManageKeys
            UserRole::SuperAdmin => true  // SuperAdmin有所有权限
          }
          
          // 检查时间窗口限制（模拟）
          let current_time = Time::now()
          let time_window_valid = (current_time - request.timestamp) < 300000  // 5分钟内
          
          // 检查IP限制（模拟）
          let ip_allowed = match request.context.find(fn(c) { c.0 == "ip_address" }) {
            Some((_, ip)) => ip.starts_with("192.168.") || ip.starts_with("10.")
            None => false
          }
          
          // 综合判断
          let granted = has_permission && role_based_permission && time_window_valid && ip_allowed
          
          // 记录访问日志
          access_log = access_log.push({
            user_id: request.user_id,
            resource: request.resource,
            action: match request.action {
              Permission::ReadTelemetry => "read"
              Permission::WriteTelemetry => "write"
              Permission::DeleteTelemetry => "delete"
              Permission::ManageKeys => "manage_keys"
              Permission::ConfigureSystem => "configure"
            },
            granted: granted,
            timestamp: current_time
          })
          
          let reason = if not granted {
            if not has_permission {
              "User lacks required permission"
            } else if not role_based_permission {
              "User role does not permit this action"
            } else if not time_window_valid {
              "Request timestamp is too old"
            } else if not ip_allowed {
              "IP address not allowed"
            } else {
              "Access denied for unknown reason"
            }
          } else {
            "Access granted"
          }
          
          let additional_factors = Array::concat([
            if time_window_valid { [] } else { ["Invalid timestamp"] },
            if ip_allowed { [] } else { ["Invalid IP address"] },
            if has_permission { [] } else { ["Missing permission"] }
          ])
          
          AccessResult({
            granted: granted,
            reason: reason,
            user_role: user.role,
            required_permission: request.action,
            additional_factors: additional_factors
          })
        }
        None => {
          AccessResult({
            granted: false,
            reason: "User not found or inactive",
            user_role: UserRole::Viewer,  // 默认角色
            required_permission: request.action,
            additional_factors: ["User not found"]
          })
        }
      }
    }
    
    fn get_access_log() -> Array<(String, String, String, Bool, Int)> {
      access_log
    }
  }
  
  let manager = access_control_manager()
  
  // 创建不同角色的用户
  let viewer_user = manager(User({
    user_id: "user-001",
    username: "viewer1",
    role: UserRole::Viewer,
    permissions: [Permission::ReadTelemetry],
    active: true,
    last_login: 1735689600000
  }))
  
  let operator_user = manager(User({
    user_id: "user-002",
    username: "operator1",
    role: UserRole::Operator,
    permissions: [Permission::ReadTelemetry, Permission::WriteTelemetry],
    active: true,
    last_login: 1735689600000
  }))
  
  let admin_user = manager(User({
    user_id: "user-003",
    username: "admin1",
    role: UserRole::Administrator,
    permissions: [Permission::ReadTelemetry, Permission::WriteTelemetry, Permission::DeleteTelemetry, Permission::ConfigureSystem],
    active: true,
    last_login: 1735689600000
  }))
  
  let superadmin_user = manager(User({
    user_id: "user-004",
    username: "superadmin1",
    role: UserRole::SuperAdmin,
    permissions: [Permission::ReadTelemetry, Permission::WriteTelemetry, Permission::DeleteTelemetry, Permission::ManageKeys, Permission::ConfigureSystem],
    active: true,
    last_login: 1735689600000
  }))
  
  let inactive_user = manager(User({
    user_id: "user-005",
    username: "inactive1",
    role: UserRole::Operator,
    permissions: [Permission::ReadTelemetry, Permission::WriteTelemetry],
    active: false,  // 非活跃用户
    last_login: 1635689600000  // 很久以前登录
  }))
  
  // 测试Viewer权限
  let viewer_read_request = AccessRequest({
    user_id: "user-001",
    resource: "/telemetry/metrics",
    action: Permission::ReadTelemetry,
    timestamp: Time::now(),
    context: [("ip_address", "192.168.1.100")]
  })
  
  let viewer_read_result = manager.check_permission(viewer_read_request)
  assert_true(viewer_read_result.granted)
  assert_eq(viewer_read_result.user_role, UserRole::Viewer)
  assert_eq(viewer_read_result.required_permission, Permission::ReadTelemetry)
  
  let viewer_write_request = AccessRequest({
    user_id: "user-001",
    resource: "/telemetry/metrics",
    action: Permission::WriteTelemetry,
    timestamp: Time::now(),
    context: [("ip_address", "192.168.1.100")]
  })
  
  let viewer_write_result = manager.check_permission(viewer_write_request)
  assert_false(viewer_write_result.granted)
  assert_eq(viewer_write_result.reason, "User role does not permit this action")
  
  // 测试Operator权限
  let operator_write_request = AccessRequest({
    user_id: "user-002",
    resource: "/telemetry/metrics",
    action: Permission::WriteTelemetry,
    timestamp: Time::now(),
    context: [("ip_address", "192.168.1.101")]
  })
  
  let operator_write_result = manager.check_permission(operator_write_request)
  assert_true(operator_write_result.granted)
  assert_eq(operator_write_result.user_role, UserRole::Operator)
  
  // 测试Administrator权限
  let admin_configure_request = AccessRequest({
    user_id: "user-003",
    resource: "/system/config",
    action: Permission::ConfigureSystem,
    timestamp: Time::now(),
    context: [("ip_address", "10.0.0.50")]
  })
  
  let admin_configure_result = manager.check_permission(admin_configure_request)
  assert_true(admin_configure_result.granted)
  assert_eq(admin_configure_result.user_role, UserRole::Administrator)
  
  let admin_manage_keys_request = AccessRequest({
    user_id: "user-003",
    resource: "/security/keys",
    action: Permission::ManageKeys,
    timestamp: Time::now(),
    context: [("ip_address", "10.0.0.50")]
  })
  
  let admin_manage_keys_result = manager.check_permission(admin_manage_keys_request)
  assert_false(admin_manage_keys_result.granted)
  assert_eq(admin_manage_keys_result.reason, "User role does not permit this action")
  
  // 测试SuperAdmin权限
  let superadmin_manage_keys_request = AccessRequest({
    user_id: "user-004",
    resource: "/security/keys",
    action: Permission::ManageKeys,
    timestamp: Time::now(),
    context: [("ip_address", "10.0.0.1")]
  })
  
  let superadmin_manage_keys_result = manager.check_permission(superadmin_manage_keys_request)
  assert_true(superadmin_manage_keys_result.granted)
  assert_eq(superadmin_manage_keys_result.user_role, UserRole::SuperAdmin)
  
  // 测试非活跃用户
  let inactive_read_request = AccessRequest({
    user_id: "user-005",
    resource: "/telemetry/metrics",
    action: Permission::ReadTelemetry,
    timestamp: Time::now(),
    context: [("ip_address", "192.168.1.200")]
  })
  
  let inactive_read_result = manager.check_permission(inactive_read_request)
  assert_false(inactive_read_result.granted)
  assert_eq(inactive_read_result.reason, "User not found or inactive")
  
  // 测试IP限制
  let invalid_ip_request = AccessRequest({
    user_id: "user-002",
    resource: "/telemetry/metrics",
    action: Permission::ReadTelemetry,
    timestamp: Time::now(),
    context: [("ip_address", "203.0.113.1")]  // 外部IP
  })
  
  let invalid_ip_result = manager.check_permission(invalid_ip_request)
  assert_false(invalid_ip_result.granted)
  assert_eq(invalid_ip_result.reason, "IP address not allowed")
  
  // 验证访问日志
  let access_log = manager.get_access_log()
  assert_true(access_log.length() >= 8)  // 至少记录了8次访问尝试
  
  // 验证日志内容
  let granted_logs = access_log.filter(fn(log) { log.3 })  // 获取授权的日志
  let denied_logs = access_log.filter(fn(log) { not log.3 })  // 获取拒绝的日志
  
  assert_true(granted_logs.length() >= 4)  // 至少4次授权
  assert_true(denied_logs.length() >= 4)  // 至少4次拒绝
}

// 测试5: 安全审计日志
test "安全审计日志" {
  // 定义审计事件类型
  enum AuditEventType {
    UserLogin
    UserLogout
    DataAccess
    DataModification
    KeyRotation
    PermissionChange
    SecurityViolation
    SystemConfiguration
  }
  
  // 定义审计日志条目
  type AuditLogEntry = {
    timestamp: Int,
    event_type: AuditEventType,
    user_id: String,
    resource: String,
    action: String,
    result: String,
    details: Array[(String, String)],
    source_ip: String,
    user_agent: String
  }
  
  // 定义审计过滤器
  type AuditFilter = {
    start_time: Option<Int>,
    end_time: Option[Int>,
    user_ids: Array<String>,
    event_types: Array<AuditEventType>,
    resources: Array<String>,
    results: Array<String>
  }
  
  // 创建审计日志记录器
  let audit_logger = fn() {
    let mut audit_logs = Array::empty()
    
    fn(entry: AuditLogEntry) {
      audit_logs = audit_logs.push(entry)
    }
    
    fn query_logs(filter: AuditFilter) -> Array<AuditLogEntry> {
      audit_logs.filter(fn(log) {
        // 时间范围过滤
        let time_match = match (filter.start_time, filter.end_time) {
          (Some(start), Some(end)) => log.timestamp >= start && log.timestamp <= end
          (Some(start), None) => log.timestamp >= start
          (None, Some(end)) => log.timestamp <= end
          (None, None) => true
        }
        
        // 用户ID过滤
        let user_match = if filter.user_ids.length() == 0 {
          true
        } else {
          filter.user_ids.contains(log.user_id)
        }
        
        // 事件类型过滤
        let event_type_match = if filter.event_types.length() == 0 {
          true
        } else {
          filter.event_types.contains(log.event_type)
        }
        
        // 资源过滤
        let resource_match = if filter.resources.length() == 0 {
          true
        } else {
          filter.resources.contains(log.resource)
        }
        
        // 结果过滤
        let result_match = if filter.results.length() == 0 {
          true
        } else {
          filter.results.contains(log.result)
        }
        
        time_match && user_match && event_type_match && resource_match && result_match
      })
    }
    
    fn get_security_violations(since: Int) -> Array<AuditLogEntry> {
      audit_logs.filter(fn(log) {
        log.event_type == AuditEventType::SecurityViolation && log.timestamp >= since
      })
    }
    
    fn generate_report(start_time: Int, end_time: Int) {
      let filtered_logs = audit_logs.filter(fn(log) {
        log.timestamp >= start_time && log.timestamp <= end_time
      })
      
      // 统计不同事件类型的数量
      let event_counts = Array::empty()
      let event_types = [
        AuditEventType::UserLogin,
        AuditEventType::UserLogout,
        AuditEventType::DataAccess,
        AuditEventType::DataModification,
        AuditEventType::KeyRotation,
        AuditEventType::PermissionChange,
        AuditEventType::SecurityViolation,
        AuditEventType::SystemConfiguration
      ]
      
      for event_type in event_types {
        let count = filtered_logs.filter(fn(log) { log.event_type == event_type }).length()
        event_counts = event_counts.push((event_type, count))
      }
      
      // 统计用户活动
      let user_activities = filtered_logs.group_by(fn(log) { log.user_id }).map(fn((user_id, logs)) {
        (user_id, logs.length())
      })
      
      // 统计资源访问
      let resource_access = filtered_logs.group_by(fn(log) { log.resource }).map(fn((resource, logs)) {
        (resource, logs.length())
      })
      
      // 统计成功/失败率
      let success_count = filtered_logs.filter(fn(log) { log.result == "success" }).length()
      let failure_count = filtered_logs.filter(fn(log) { log.result == "failure" }).length()
      let total_count = success_count + failure_count
      let success_rate = if total_count > 0 {
        (success_count as Float) / (total_count as Float) * 100.0
      } else {
        0.0
      }
      
      {
        report_period: (start_time, end_time),
        total_events: filtered_logs.length(),
        event_type_counts: event_counts,
        user_activities: user_activities,
        resource_access: resource_access,
        success_rate: success_rate,
        security_violations: filtered_logs.filter(fn(log) { log.event_type == AuditEventType::SecurityViolation }).length()
      }
    }
  }
  
  let logger = audit_logger()
  
  // 记录审计事件
  let base_time = 1735689600000  // 2025-01-01 00:00:00
  
  // 用户登录事件
  logger(AuditLogEntry({
    timestamp: base_time,
    event_type: AuditEventType::UserLogin,
    user_id: "user-001",
    resource: "/auth/login",
    action: "login",
    result: "success",
    details: [("method", "password"), ("mfa", "enabled")],
    source_ip: "192.168.1.100",
    user_agent: "Azimuth-Client/1.0"
  }))
  
  // 数据访问事件
  logger(AuditLogEntry({
    timestamp: base_time + 60000,  // 1分钟后
    event_type: AuditEventType::DataAccess,
    user_id: "user-001",
    resource: "/telemetry/metrics",
    action: "read",
    result: "success",
    details: [("query", "cpu_usage"), ("time_range", "1h")],
    source_ip: "192.168.1.100",
    user_agent: "Azimuth-Client/1.0"
  }))
  
  // 数据修改事件
  logger(AuditLogEntry({
    timestamp: base_time + 120000,  // 2分钟后
    event_type: AuditEventType::DataModification,
    user_id: "user-002",
    resource: "/telemetry/metrics",
    action: "write",
    result: "success",
    details: [("metric", "memory_usage"), ("value", "78.5")],
    source_ip: "192.168.1.101",
    user_agent: "Azimuth-Client/1.0"
  }))
  
  // 安全违规事件
  logger(AuditLogEntry({
    timestamp: base_time + 180000,  // 3分钟后
    event_type: AuditEventType::SecurityViolation,
    user_id: "user-003",
    resource: "/admin/users",
    action: "unauthorized_access",
    result: "failure",
    details: [("violation_type", "privilege_escalation"), ("blocked", "true")],
    source_ip: "203.0.113.50",
    user_agent: "Malicious-Scanner/1.0"
  }))
  
  // 密钥轮换事件
  logger(AuditLogEntry({
    timestamp: base_time + 240000,  // 4分钟后
    event_type: AuditEventType::KeyRotation,
    user_id: "system",
    resource: "/security/keys",
    action: "rotate",
    result: "success",
    details: [("key_id", "telemetry-key-001"), ("algorithm", "AES256-GCM")],
    source_ip: "127.0.0.1",
    user_agent: "Azimuth-System/1.0"
  }))
  
  // 用户登出事件
  logger(AuditLogEntry({
    timestamp: base_time + 300000,  // 5分钟后
    event_type: AuditEventType::UserLogout,
    user_id: "user-001",
    resource: "/auth/logout",
    action: "logout",
    result: "success",
    details: [("session_duration", "300s")],
    source_ip: "192.168.1.100",
    user_agent: "Azimuth-Client/1.0"
  }))
  
  // 测试日志查询
  let all_logs = logger.query_logs(AuditFilter({
    start_time: None,
    end_time: None,
    user_ids: [],
    event_types: [],
    resources: [],
    results: []
  }))
  
  assert_eq(all_logs.length(), 6)
  
  // 测试时间范围过滤
  let time_filtered_logs = logger.query_logs(AuditFilter({
    start_time: Some(base_time + 60000),
    end_time: Some(base_time + 240000),
    user_ids: [],
    event_types: [],
    resources: [],
    results: []
  }))
  
  assert_eq(time_filtered_logs.length(), 3)  // 排除第一个和最后一个事件
  
  // 测试用户过滤
  let user_filtered_logs = logger.query_logs(AuditFilter({
    start_time: None,
    end_time: None,
    user_ids: ["user-001"],
    event_types: [],
    resources: [],
    results: []
  }))
  
  assert_eq(user_filtered_logs.length(), 3)  // user-001有3个事件
  
  // 测试事件类型过滤
  let event_type_filtered_logs = logger.query_logs(AuditFilter({
    start_time: None,
    end_time: None,
    user_ids: [],
    event_types: [AuditEventType::SecurityViolation],
    resources: [],
    results: []
  }))
  
  assert_eq(event_type_filtered_logs.length(), 1)  // 只有1个安全违规事件
  
  // 测试结果过滤
  let result_filtered_logs = logger.query_logs(AuditFilter({
    start_time: None,
    end_time: None,
    user_ids: [],
    event_types: [],
    resources: [],
    results: ["failure"]
  }))
  
  assert_eq(result_filtered_logs.length(), 1)  // 只有1个失败事件
  
  // 测试安全违规查询
  let security_violations = logger.get_security_violations(base_time)
  assert_eq(security_violations.length(), 1)
  assert_eq(security_violations[0].event_type, AuditEventType::SecurityViolation)
  assert_eq(security_violations[0].user_id, "user-003")
  
  // 测试报告生成
  let report = logger.generate_report(base_time, base_time + 300000)
  
  assert_eq(report.total_events, 6)
  assert_eq(report.event_type_counts.length(), 8)  // 8种事件类型
  assert_eq(report.security_violations, 1)
  
  // 验证用户活动统计
  let user001_activities = report.user_activities.find(fn((user_id, count)) { user_id == "user-001" })
  assert_true(user001_activities.is_some())
  assert_eq(user001_activities.unwrap().1, 3)  // user-001有3个活动
  
  // 验证资源访问统计
  let metrics_access = report.resource_access.find(fn((resource, count)) { resource == "/telemetry/metrics" })
  assert_true(metrics_access.is_some())
  assert_eq(metrics_access.unwrap().1, 2)  // /telemetry/metrics被访问2次
  
  // 验证成功率
  assert_eq(report.success_rate.round(), 83.0)  // 5/6 = 83.33%
}
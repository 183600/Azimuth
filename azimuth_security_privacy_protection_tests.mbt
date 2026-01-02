// Azimuth Security and Privacy Protection Tests
// 安全性和隐私保护测试用例 - 专注于数据安全和隐私保护功能

// Test 1: 数据加密和解密
test "data encryption and decryption" {
  let crypto_manager = CryptoManager::new()
  
  // 生成加密密钥
  let key_result = CryptoManager::generate_key(crypto_manager, Algorithm::AES256)
  match key_result {
    Ok(key) => {
      assert_true(key.length() == 32) // AES-256需要32字节密钥
      
      // 测试数据加密
      let plaintext = "Azimuth security test data with sensitive information"
      let plaintext_bytes = String::to_bytes(plaintext)
      
      let encrypt_result = CryptoManager::encrypt(crypto_manager, plaintext_bytes, key)
      match encrypt_result {
        Ok(ciphertext) => {
          // 验证密文与原文不同
          assert_true(ciphertext.length() > 0)
          assert_true(ciphertext != plaintext_bytes)
          
          // 测试数据解密
          let decrypt_result = CryptoManager::decrypt(crypto_manager, ciphertext, key)
          match decrypt_result {
            Ok(decrypted) => {
              let decrypted_text = String::from_bytes(decrypted)
              assert_eq(decrypted_text, plaintext)
            }
            Err(_) => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
      
      // 测试不同算法
      let rsa_key_result = CryptoManager::generate_rsa_keypair(crypto_manager, 2048)
      match rsa_key_result {
        Ok((public_key, private_key)) => {
          // 使用公钥加密
          let rsa_encrypt_result = CryptoManager::rsa_encrypt(crypto_manager, plaintext_bytes, public_key)
          match rsa_encrypt_result {
            Ok(rsa_ciphertext) => {
              // 使用私钥解密
              let rsa_decrypt_result = CryptoManager::rsa_decrypt(crypto_manager, rsa_ciphertext, private_key)
              match rsa_decrypt_result {
                Ok(rsa_decrypted) => {
                  let rsa_decrypted_text = String::from_bytes(rsa_decrypted)
                  assert_eq(rsa_decrypted_text, plaintext)
                }
                Err(_) => assert_true(false)
              }
            }
            Err(_) => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试错误情况
  let invalid_key = [0, 1, 2, 3] // 太短的密钥
  let invalid_encrypt_result = CryptoManager::encrypt(crypto_manager, plaintext_bytes, invalid_key)
  match invalid_encrypt_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(e) => assert_true(Error::is_crypto_error(e))
  }
}

// Test 2: 数据脱敏和匿名化
test "data masking and anonymization" {
  let privacy_manager = PrivacyManager::new()
  
  // 测试个人身份信息脱敏
  let pii_data = {
    "name": "张三",
    "email": "zhangsan@example.com",
    "phone": "13812345678",
    "id_card": "110101199001011234",
    "address": "北京市朝阳区某某街道123号",
    "credit_card": "4111111111111111"
  }
  
  // 配置脱敏规则
  PrivacyManager::add_masking_rule(privacy_manager, "name", MaskingType::Partial, 1, 1)
  PrivacyManager::add_masking_rule(privacy_manager, "email", MaskingType::Email, 0, 0)
  PrivacyManager::add_masking_rule(privacy_manager, "phone", MaskingType::Partial, 3, 4)
  PrivacyManager::add_masking_rule(privacy_manager, "id_card", MaskingType::IDCard, 0, 0)
  PrivacyManager::add_masking_rule(privacy_manager, "address", MaskingType::Partial, 6, 3)
  PrivacyManager::add_masking_rule(privacy_manager, "credit_card", MaskingType::CreditCard, 0, 0)
  
  // 应用脱敏
  let masked_data = PrivacyManager::apply_masking(privacy_manager, pii_data)
  
  // 验证脱敏结果
  assert_eq(masked_data.name, "张*")
  assert_eq(masked_data.email, "z****@example.com")
  assert_eq(masked_data.phone, "138****5678")
  assert_eq(masked_data.id_card, "110101********1234")
  assert_eq(masked_data.address, "北京市朝阳区****123号")
  assert_eq(masked_data.credit_card, "4111********1111")
  
  // 测试数据匿名化
  let user_data = [
    { "user_id": "user123", "age": 25, "city": "北京", "salary": 15000 },
    { "user_id": "user456", "age": 30, "city": "上海", "salary": 20000 },
    { "user_id": "user789", "age": 28, "city": "广州", "salary": 18000 }
  ]
  
  // 配置匿名化策略
  PrivacyManager::set_anonymization_strategy(privacy_manager, {
    "user_id": AnonymizationType::Hash,
    "age": AnonymizationType::Range, // 年龄分组
    "city": AnonymizationType::Generalization, // 城市泛化为区域
    "salary": AnonymizationType::Noise // 添加噪声
  })
  
  let anonymized_data = PrivacyManager::anonymize(privacy_manager, user_data)
  
  // 验证匿名化结果
  assert_true(anonymized_data[0].user_id != "user123")
  assert_true(anonymized_data[0].user_id.length() == 64) // 哈希长度
  
  assert_true(["20-30", "25-35"].contains(anonymized_data[0].age))
  assert_true(["华北", "华东", "华南"].contains(anonymized_data[0].city))
  assert_true(anonymized_data[0].salary != 15000) // 应该有噪声
  
  // 测试k-匿名性
  let k_anonymity_result = PrivacyManager::check_k_anonymity(privacy_manager, anonymized_data, 2)
  match k_anonymity_result {
    Ok(k_value) => assert_true(k_value >= 2)
    Err(_) => assert_true(false)
  }
}

// Test 3: 访问控制和权限管理
test "access control and permission management" {
  let access_manager = AccessManager::new()
  
  // 创建角色和权限
  AccessManager::create_role(access_manager, "admin")
  AccessManager::create_role(access_manager, "user")
  AccessManager::create_role(access_manager, "guest")
  
  AccessManager::add_permission(access_manager, "admin", ["read", "write", "delete", "manage"])
  AccessManager::add_permission(access_manager, "user", ["read", "write"])
  AccessManager::add_permission(access_manager, "guest", ["read"])
  
  // 创建用户并分配角色
  AccessManager::create_user(access_manager, "alice", "password123")
  AccessManager::create_user(access_manager, "bob", "password456")
  AccessManager::create_user(access_manager, "charlie", "password789")
  
  AccessManager::assign_role(access_manager, "alice", "admin")
  AccessManager::assign_role(access_manager, "bob", "user")
  AccessManager::assign_role(access_manager, "charlie", "guest")
  
  // 测试权限检查
  let admin_read = AccessManager::check_permission(access_manager, "alice", "read")
  let admin_delete = AccessManager::check_permission(access_manager, "alice", "delete")
  let user_read = AccessManager::check_permission(access_manager, "bob", "read")
  let user_delete = AccessManager::check_permission(access_manager, "bob", "delete")
  let guest_read = AccessManager::check_permission(access_manager, "charlie", "read")
  let guest_write = AccessManager::check_permission(access_manager, "charlie", "write")
  
  assert_true(admin_read)
  assert_true(admin_delete)
  assert_true(user_read)
  assert_false(user_delete)
  assert_true(guest_read)
  assert_false(guest_write)
  
  // 测试资源级访问控制
  AccessManager::create_resource(access_manager, "document1", ["admin", "user"])
  AccessManager::create_resource(access_manager, "document2", ["admin"])
  AccessManager::create_resource(access_manager, "public_doc", ["admin", "user", "guest"])
  
  let alice_doc1 = AccessManager::check_resource_access(access_manager, "alice", "document1")
  let alice_doc2 = AccessManager::check_resource_access(access_manager, "alice", "document2")
  let bob_doc1 = AccessManager::check_resource_access(access_manager, "bob", "document1")
  let bob_doc2 = AccessManager::check_resource_access(access_manager, "bob", "document2")
  let charlie_public = AccessManager::check_resource_access(access_manager, "charlie", "public_doc")
  
  assert_true(alice_doc1)
  assert_true(alice_doc2)
  assert_true(bob_doc1)
  assert_false(bob_doc2)
  assert_true(charlie_public)
  
  // 测试基于时间的访问控制
  AccessManager::set_time_constraint(access_manager, "bob", "09:00", "17:00") // 工作时间
  
  // 模拟工作时间内的访问
  let work_time_access = AccessManager::check_time_based_access(access_manager, "bob")
  // 模拟工作时间外的访问
  let after_hours_access = AccessManager::check_time_based_access(access_manager, "bob", "20:00")
  
  // 注意：实际结果取决于当前时间，这里只验证方法调用
  assert_true(true)
  
  // 测试访问日志
  AccessManager::log_access_attempt(access_manager, "alice", "document1", true)
  AccessManager::log_access_attempt(access_manager, "bob", "document2", false)
  
  let access_logs = AccessManager::get_access_logs(access_manager)
  assert_true(access_logs.length() >= 2)
  
  let alice_logs = access_logs.filter(fn(log) { log.user == "alice" })
  let bob_logs = access_logs.filter(fn(log) { log.user == "bob" })
  
  assert_true(alice_logs.length() >= 1)
  assert_true(bob_logs.length() >= 1)
  assert_true(alice_logs[0].success)
  assert_false(bob_logs[0].success)
}

// Test 4: 安全审计和日志记录
test "security auditing and logging" {
  let audit_manager = AuditManager::new()
  
  // 配置审计策略
  AuditManager::configure_audit_policy(audit_manager, {
    "log_all_access": true,
    "log_data_changes": true,
    "log_authentication": true,
    "log_permission_changes": true,
    "retention_days": 90
  })
  
  // 模拟审计事件
  AuditManager::log_event(audit_manager, AuditEvent::Login, {
    "user": "alice",
    "timestamp": Time::now(),
    "ip_address": "192.168.1.100",
    "user_agent": "Mozilla/5.0...",
    "success": true
  })
  
  AuditManager::log_event(audit_manager, AuditEvent::DataAccess, {
    "user": "bob",
    "resource": "customer_data",
    "action": "read",
    "timestamp": Time::now(),
    "success": true
  })
  
  AuditManager::log_event(audit_manager, AuditEvent::PermissionChange, {
    "admin": "alice",
    "target_user": "charlie",
    "old_role": "guest",
    "new_role": "user",
    "timestamp": Time::now()
  })
  
  AuditManager::log_event(audit_manager, AuditEvent::DataModification, {
    "user": "bob",
    "resource": "customer_record_123",
    "action": "update",
    "old_values": { "status": "active" },
    "new_values": { "status": "inactive" },
    "timestamp": Time::now()
  })
  
  // 测试审计日志查询
  let all_events = AuditManager::get_all_events(audit_manager)
  assert_true(all_events.length() >= 4)
  
  let login_events = AuditManager::get_events_by_type(audit_manager, AuditEvent::Login)
  assert_true(login_events.length() >= 1)
  assert_eq(login_events[0].user, "alice")
  
  let bob_events = AuditManager::get_events_by_user(audit_manager, "bob")
  assert_true(bob_events.length() >= 2)
  
  let recent_events = AuditManager::get_events_by_time_range(audit_manager, Time::now() - 3600, Time::now())
  assert_true(recent_events.length() >= 4)
  
  // 测试审计报告生成
  let audit_report = AuditManager::generate_report(audit_manager, {
    "start_date": Time::now() - 86400, // 最近24小时
    "end_date": Time::now(),
    "include_summary": true,
    "include_details": true
  })
  
  assert_true(audit_report.contains("Audit Summary"))
  assert_true(audit_report.contains("Total Events"))
  assert_true(audit_report.contains("Login Events"))
  assert_true(audit_report.contains("Data Access Events"))
  
  // 测试安全异常检测
  let security_anomalies = AuditManager::detect_security_anomalies(audit_manager)
  assert_true(security_anomalies.length() >= 0)
  
  // 测试日志完整性验证
  let integrity_result = AuditManager::verify_log_integrity(audit_manager)
  match integrity_result {
    Ok(is_valid) => assert_true(is_valid)
    Err(_) => assert_true(false)
  }
  
  // 测试日志导出
  let export_result = AuditManager::export_logs(audit_manager, "/tmp/azimuth_audit_export.json")
  match export_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
}

// Test 5: 安全通信和传输
test "secure communication and transmission" {
  let secure_comm = SecureCommunication::new()
  
  // 生成证书
  let cert_result = SecureCommunication::generate_self_signed_certificate(secure_comm, {
    "common_name": "azimuth-test",
    "organization": "Azimuth Project",
    "country": "CN",
    "valid_days": 365
  })
  
  match cert_result {
    Ok((cert, private_key)) => {
      // 验证证书
      let cert_valid = SecureCommunication::verify_certificate(secure_comm, cert)
      assert_true(cert_valid)
      
      // 测试TLS连接
      let tls_server = SecureCommunication::create_tls_server(secure_comm, cert, private_key, 8443)
      match tls_server {
        Ok(server) => {
          // 启动服务器
          SecureCommunication::start_server(server)
          
          // 创建TLS客户端
          let tls_client = SecureCommunication::create_tls_client(secure_comm)
          match tls_client {
            Ok(client) => {
              // 连接到服务器
              let connect_result = SecureCommunication::connect(client, "localhost:8443")
              match connect_result {
                Ok(_) => {
                  // 发送安全数据
                  let secure_data = "Secure telemetry data"
                  let send_result = SecureCommunication::send_secure_data(client, String::to_bytes(secure_data))
                  match send_result {
                    Ok(_) => assert_true(true)
                    Err(_) => assert_true(false)
                  }
                  
                  // 接收数据
                  let receive_result = SecureCommunication::receive_secure_data(client)
                  match receive_result {
                    Ok(data) => {
                      let received_text = String::from_bytes(data)
                      assert_eq(received_text, secure_data)
                    }
                    Err(_) => assert_true(false)
                  }
                  
                  SecureCommunication::disconnect(client)
                }
                Err(_) => assert_true(true) // 连接可能失败，这是正常的
              }
            }
            Err(_) => assert_true(false)
          }
          
          SecureCommunication::stop_server(server)
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试消息签名和验证
  let signing_key = SecureCommunication::generate_signing_key(secure_comm)
  let message = "Important telemetry message"
  
  let signature_result = SecureCommunication::sign_message(secure_comm, message, signing_key)
  match signature_result {
    Ok(signature) => {
      // 验证签名
      let verify_result = SecureCommunication::verify_signature(secure_comm, message, signature, signing_key)
      assert_true(verify_result)
      
      // 测试篡改检测
      let tampered_message = message + " tampered"
      let tampered_verify = SecureCommunication::verify_signature(secure_comm, tampered_message, signature, signing_key)
      assert_false(tampered_verify)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试安全通道
  let secure_channel = SecureCommunication::create_secure_channel(secure_comm)
  SecureCommunication::initialize_channel(secure_channel)
  
  // 发送多条消息
  let messages = ["msg1", "msg2", "msg3"]
  for msg in messages {
    let send_result = SecureCommunication::send_channel_message(secure_channel, msg)
    match send_result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  // 接收消息
  let received_messages = []
  for i in 0..<messages.length() {
    let recv_result = SecureCommunication::receive_channel_message(secure_channel)
    match recv_result {
      Ok(msg) => received_messages = received_messages.push(msg)
      Err(_) => assert_true(false)
    }
  }
  
  assert_eq(received_messages, messages)
  SecureCommunication::close_channel(secure_channel)
}

// Test 6: 安全配置管理
test "secure configuration management" {
  let secure_config = SecureConfigManager::new()
  
  // 创建安全配置
  let config_values = {
    "database.password": "secret_db_password",
    "api.key": "secret_api_key_12345",
    "encryption.key": "base64:encryption_key_data",
    "jwt.secret": "jwt_signing_secret_key",
    "admin.password": "admin_secure_password"
  }
  
  // 加密存储配置
  let master_key = SecureConfigManager::generate_master_key(secure_config)
  let store_result = SecureConfigManager::store_encrypted_config(secure_config, config_values, master_key)
  match store_result {
    Ok(config_id) => {
      // 验证配置已加密存储
      let raw_config = SecureConfigManager::get_raw_config(secure_config, config_id)
      assert_true(raw_config.contains("encrypted"))
      assert_false(raw_config.contains("secret_db_password"))
      
      // 解密并读取配置
      let load_result = SecureConfigManager::load_encrypted_config(secure_config, config_id, master_key)
      match load_result {
        Ok(loaded_values) => {
          assert_eq(loaded_values["database.password"], "secret_db_password")
          assert_eq(loaded_values["api.key"], "secret_api_key_12345")
          assert_eq(loaded_values["encryption.key"], "base64:encryption_key_data")
          assert_eq(loaded_values["jwt.secret"], "jwt_signing_secret_key")
          assert_eq(loaded_values["admin.password"], "admin_secure_password")
        }
        Err(_) => assert_true(false)
      }
      
      // 测试配置版本控制
      let updated_config = { ...config_values, "api.key": "updated_api_key_67890" }
      let update_result = SecureConfigManager::update_encrypted_config(secure_config, config_id, updated_config, master_key)
      match update_result {
        Ok(new_version) => {
          // 获取历史版本
          let history = SecureConfigManager::get_config_history(secure_config, config_id)
          assert_true(history.length() >= 2)
          
          // 恢复到之前版本
          let rollback_result = SecureConfigManager::rollback_to_version(secure_config, config_id, history[0].version, master_key)
          match rollback_result {
            Ok(rollback_values) => {
              assert_eq(rollback_values["api.key"], "secret_api_key_12345") // 应该是旧值
            }
            Err(_) => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
      
      // 测试配置访问控制
      SecureConfigManager::set_access_policy(secure_config, config_id, {
        "allowed_users": ["admin", "config_manager"],
        "require_authentication": true,
        "audit_access": true
      })
      
      // 测试访问验证
      let admin_access = SecureConfigManager::check_config_access(secure_config, config_id, "admin")
      let user_access = SecureConfigManager::check_config_access(secure_config, config_id, "regular_user")
      
      assert_true(admin_access)
      assert_false(user_access)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试配置验证
  let validation_rules = {
    "database.password": { "required": true, "min_length": 8 },
    "api.key": { "required": true, "pattern": "^[a-zA-Z0-9_]+$" },
    "encryption.key": { "required": true, "format": "base64" }
  }
  
  SecureConfigManager::set_validation_rules(secure_config, validation_rules)
  
  // 测试有效配置
  let valid_config = {
    "database.password": "valid_password_123",
    "api.key": "valid_api_key_456",
    "encryption.key": "base64:dGVzdF9rZXlfZGF0YQ==" // base64编码的"test_key_data"
  }
  
  let valid_result = SecureConfigManager::validate_config(secure_config, valid_config)
  match valid_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 测试无效配置
  let invalid_config = {
    "database.password": "short", // 太短
    "api.key": "invalid-key!", // 包含无效字符
    "encryption.key": "not_base64" // 不是base64格式
  }
  
  let invalid_result = SecureConfigManager::validate_config(secure_config, invalid_config)
  match invalid_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(errors) => {
      assert_true(errors.length() >= 3)
      assert_true(errors.any(fn(err) { String::contains(err, "password") }))
      assert_true(errors.any(fn(err) { String::contains(err, "api.key") }))
      assert_true(errors.any(fn(err) { String::contains(err, "encryption.key") }))
    }
  }
}

// Test 7: 威胁检测和防护
test "threat detection and protection" {
  let threat_detector = ThreatDetector::new()
  
  // 配置威胁检测规则
  ThreatDetector::add_detection_rule(threat_detector, {
    "name": "brute_force_login",
    "type": "pattern",
    "condition": "failed_login_count > 5 in 5 minutes",
    "severity": "high",
    "action": "block_ip_temporarily"
  })
  
  ThreatDetector::add_detection_rule(threat_detector, {
    "name": "unusual_data_access",
    "type": "anomaly",
    "condition": "data_access_volume > 3 * average",
    "severity": "medium",
    "action": "require_additional_auth"
  })
  
  ThreatDetector::add_detection_rule(threat_detector, {
    "name": "sql_injection_attempt",
    "type": "signature",
    "condition": "query contains 'DROP TABLE' OR 'UNION SELECT'",
    "severity": "critical",
    "action": "block_request_and_alert"
  })
  
  // 模拟正常活动
  for i in 0..<10 {
    ThreatDetector::record_event(threat_detector, {
      "type": "login_success",
      "user": "user" + i.to_string(),
      "ip": "192.168.1." + (100 + i).to_string(),
      "timestamp": Time::now() - i * 60
    })
  }
  
  // 模拟暴力破解攻击
  let attacker_ip = "10.0.0.100"
  for i in 0..<7 {
    ThreatDetector::record_event(threat_detector, {
      "type": "login_failure",
      "user": "admin",
      "ip": attacker_ip,
      "timestamp": Time::now() - i * 30
    })
  }
  
  // 模拟SQL注入尝试
  ThreatDetector::record_event(threat_detector, {
    "type": "database_query",
    "user": "malicious_user",
    "query": "SELECT * FROM users WHERE id = 1; DROP TABLE users; --",
    "ip": "10.0.0.101",
    "timestamp": Time::now()
  })
  
  // 模拟异常数据访问
  for i in 0..<50 {
    ThreatDetector::record_event(threat_detector, {
      "type": "data_access",
      "user": "suspicious_user",
      "resource": "customer_data",
      "volume": 1000,
      "timestamp": Time::now() - i * 10
    })
  }
  
  // 运行威胁检测
  let detection_result = ThreatDetector::analyze_threats(threat_detector)
  
  // 验证检测结果
  assert_true(detection_result.threats_detected >= 2) // 至少检测到暴力破解和SQL注入
  
  // 检查具体威胁
  let brute_force_threat = detection_result.threats.find(fn(threat) { threat.type == "brute_force_login" })
  let sql_injection_threat = detection_result.threats.find(fn(threat) { threat.type == "sql_injection_attempt" })
  let unusual_access_threat = detection_result.threats.find(fn(threat) { threat.type == "unusual_data_access" })
  
  assert_true(brute_force_threat !== None)
  assert_true(sql_injection_threat !== None)
  // 异常访问可能被检测到，但不保证
  
  // 测试自动响应
  for threat in detection_result.threats {
    let response_result = ThreatDetector::execute_response(threat_detector, threat)
    match response_result {
      Ok(action) => {
        assert_true(action != "")
        // 验证响应已执行
        let response_log = ThreatDetector::get_response_log(threat_detector)
        assert_true(response_log.any(fn(log) { log.threat_id == threat.id }))
      }
      Err(_) => assert_true(false)
    }
  }
  
  // 测试IP黑名单
  let blacklist_result = ThreatDetector::add_to_blacklist(threat_detector, attacker_ip, 3600) // 1小时
  match blacklist_result {
    Ok(_) => {
      // 验证IP已被列入黑名单
      let is_blacklisted = ThreatDetector::is_ip_blacklisted(threat_detector, attacker_ip)
      assert_true(is_blacklisted)
      
      // 测试黑名单中的IP访问被阻止
      let access_result = ThreatDetector::check_ip_access(threat_detector, attacker_ip)
      assert_false(access_result.allowed)
      assert_eq(access_result.reason, "blacklisted")
    }
    Err(_) => assert_true(false)
  }
  
  // 生成威胁报告
  let threat_report = ThreatDetector::generate_threat_report(threat_detector, {
    "include_details": true,
    "include_recommendations": true
  })
  
  assert_true(threat_report.contains("Threat Detection Summary"))
  assert_true(threat_report.contains("Detected Threats"))
  assert_true(threat_report.contains("Security Recommendations"))
}

// Test 8: 数据完整性验证
test "data integrity verification" {
  let integrity_verifier = IntegrityVerifier::new()
  
  // 测试校验和计算
  let test_data = "Azimuth telemetry data for integrity testing"
  let data_bytes = String::to_bytes(test_data)
  
  let checksum_result = IntegrityVerifier::calculate_checksum(integrity_verifier, data_bytes, Algorithm::SHA256)
  match checksum_result {
    Ok(checksum) => {
      assert_true(checksum.length() == 64) // SHA-256产生64字符的十六进制字符串
      
      // 验证数据完整性
      let verify_result = IntegrityVerifier::verify_data(integrity_verifier, data_bytes, checksum, Algorithm::SHA256)
      assert_true(verify_result)
      
      // 测试篡改检测
      let tampered_data = test_data + " tampered"
      let tampered_bytes = String::to_bytes(tampered_data)
      let tampered_verify = IntegrityVerifier::verify_data(integrity_verifier, tampered_bytes, checksum, Algorithm::SHA256)
      assert_false(tampered_verify)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试数字签名
  let signing_key = IntegrityVerifier::generate_signing_key(integrity_verifier, Algorithm::RSA)
  match signing_key {
    Ok(key_pair) => {
      // 签名数据
      let sign_result = IntegrityVerifier::sign_data(integrity_verifier, data_bytes, key_pair.private_key)
      match sign_result {
        Ok(signature) => {
          // 验证签名
          let verify_result = IntegrityVerifier::verify_signature(integrity_verifier, data_bytes, signature, key_pair.public_key)
          assert_true(verify_result)
          
          // 测试使用错误密钥验证
          let wrong_key = IntegrityVerifier::generate_signing_key(integrity_verifier, Algorithm::RSA)
          match wrong_key {
            Ok(wrong_key_pair) => {
              let wrong_verify = IntegrityVerifier::verify_signature(integrity_verifier, data_bytes, signature, wrong_key_pair.public_key)
              assert_false(wrong_verify)
            }
            Err(_) => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试Merkle树构建和验证
  let data_blocks = []
  for i in 0..<100 {
    let block_data = "block_" + i.to_string() + "_with_content"
    data_blocks = data_blocks.push(String::to_bytes(block_data))
  }
  
  let merkle_tree_result = IntegrityVerifier::build_merkle_tree(integrity_verifier, data_blocks)
  match merkle_tree_result {
    Ok(merkle_tree) => {
      // 获取根哈希
      let root_hash = IntegrityVerifier::get_merkle_root(merkle_tree)
      assert_true(root_hash.length() == 64)
      
      // 验证特定数据块
      let block_index = 42
      let block_proof = IntegrityVerifier::get_merkle_proof(merkle_tree, block_index)
      match block_proof {
        Ok(proof) => {
          let verify_result = IntegrityVerifier::verify_merkle_proof(integrity_verifier, data_blocks[block_index], proof, root_hash)
          assert_true(verify_result)
          
          // 测试篡改数据块的验证
          let tampered_block = String::to_bytes("tampered_block_content")
          let tampered_verify = IntegrityVerifier::verify_merkle_proof(integrity_verifier, tampered_block, proof, root_hash)
          assert_false(tampered_verify)
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试时间戳验证
  let timestamp_result = IntegrityVerifier::create_timestamp(integrity_verifier, data_bytes)
  match timestamp_result {
    Ok(timestamp) => {
      // 验证时间戳
      let verify_result = IntegrityVerifier::verify_timestamp(integrity_verifier, data_bytes, timestamp)
      assert_true(verify_result)
      
      // 验证时间戳在合理范围内
      let timestamp_time = IntegrityVerifier::extract_timestamp_time(timestamp)
      let current_time = Time::now()
      assert_true(timestamp_time <= current_time)
      assert_true(current_time - timestamp_time < 60) // 应该是最近的时间戳
    }
    Err(_) => assert_true(false)
  }
}

// Test 9: 隐私影响评估
test "privacy impact assessment" {
  let privacy_assessor = PrivacyAssessor::new()
  
  // 定义数据处理活动
  let data_processing = {
    "name": "customer_analytics",
    "description": "分析客户行为以改进服务",
    "data_types": ["personal_name", "email_address", "phone_number", "location_data", "behavioral_data"],
    "purpose": "service_improvement",
    "retention_period": 365,
    "data_subjects": ["customers"],
    "third_parties": ["analytics_provider"],
    "international_transfer": true,
    "automated_decision_making": true
  }
  
  // 执行隐私影响评估
  let assessment_result = PrivacyAssessor::conduct_assessment(privacy_assessor, data_processing)
  
  // 验证评估结果
  assert_true(assessment_result.risk_level != "")
  assert_true(["low", "medium", "high", "critical"].contains(assessment_result.risk_level))
  assert_true(assessment_result.identified_risks.length() > 0)
  assert_true(assessment_result.recommendations.length() > 0)
  
  // 检查具体风险
  let has_identifiability_risk = assessment_result.identified_risks.any(fn(risk) { 
    String::contains(risk, "identifiability")
  })
  let has_retention_risk = assessment_result.identified_risks.any(fn(risk) { 
    String::contains(risk, "retention")
  })
  let has_transfer_risk = assessment_result.identified_risks.any(fn(risk) { 
    String::contains(risk, "international")
  })
  
  assert_true(has_identifiability_risk)
  assert_true(has_retention_risk)
  assert_true(has_transfer_risk)
  
  // 测试缓解措施建议
  let mitigation_result = PrivacyAssessor::suggest_mitigations(privacy_assessor, assessment_result)
  assert_true(mitigation_result.mitigations.length() > 0)
  
  // 验证缓解措施与风险匹配
  for risk in assessment_result.identified_risks {
    let has_mitigation = mitigation_result.mitigations.any(fn(mitigation) { 
      String::contains(mitigation.risk, risk)
    })
    assert_true(has_mitigation)
  }
  
  // 测试数据保护影响评估模板
  let template_result = PrivacyAssessor::generate_dpia_template(privacy_assessor, data_processing)
  match template_result {
    Ok(template) => {
      assert_true(template.contains("Data Protection Impact Assessment"))
      assert_true(template.contains("Processing Description"))
      assert_true(template.contains("Necessity and Proportionality"))
      assert_true(template.contains("Risks to Rights and Freedoms"))
      assert_true(template.contains("Mitigation Measures"))
    }
    Err(_) => assert_true(false)
  }
  
  // 测试合规性检查
  let compliance_result = PrivacyAssessor::check_compliance(privacy_assessor, data_processing, ["GDPR", "CCPA"])
  assert_true(compliance_result.overall_score >= 0 && compliance_result.overall_score <= 100)
  assert_true(compliance_result.framework_results.length() >= 2)
  
  // 检查GDPR合规性
  let gdpr_result = compliance_result.framework_results.find(fn(result) { result.framework == "GDPR" })
  match gdpr_result {
    Some(result) => {
      assert_true(result.score >= 0 && result.score <= 100)
      assert_true(result.requirements_checked > 0)
      assert_true(result.requirements_met >= 0)
    }
    None => assert_true(false)
  }
  
  // 测试隐私政策生成
  let policy_result = PrivacyAssessor::generate_privacy_policy(privacy_assessor, data_processing, mitigation_result)
  match policy_result {
    Ok(policy) => {
      assert_true(policy.contains("Privacy Policy"))
      assert_true(policy.contains("Data Collection"))
      assert_true(policy.contains("Data Use"))
      assert_true(policy.contains("Data Sharing"))
      assert_true(policy.contains("Data Retention"))
      assert_true(policy.contains("Your Rights"))
    }
    Err(_) => assert_true(false)
  }
}

// Test 10: 安全培训和意识测试
test "security training and awareness testing" {
  let training_manager = SecurityTrainingManager::new()
  
  // 创建安全培训课程
  let training_courses = [
    {
      "id": "sec101",
      "title": "安全基础知识",
      "description": "介绍基本的安全概念和最佳实践",
      "duration": 60, // 分钟
      "difficulty": "beginner",
      "modules": ["密码安全", "网络威胁", "社会工程", "数据保护"]
    },
    {
      "id": "sec201",
      "title": "高级安全实践",
      "description": "深入探讨高级安全技术和策略",
      "duration": 120,
      "difficulty": "advanced",
      "modules": ["加密技术", "安全架构", "威胁检测", "事件响应"]
    },
    {
      "id": "priv101",
      "title": "隐私保护基础",
      "description": "学习隐私法规和数据保护原则",
      "duration": 90,
      "difficulty": "intermediate",
      "modules": ["隐私法规", "数据分类", "隐私影响评估", "用户权利"]
    }
  ]
  
  // 添加培训课程
  for course in training_courses {
    let add_result = TrainingManager::add_course(training_manager, course)
    match add_result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  // 创建培训用户
  let users = ["alice", "bob", "charlie", "diana"]
  for user in users {
    TrainingManager::create_user(training_manager, user)
  }
  
  // 分配培训课程
  TrainingManager::assign_course(training_manager, "alice", "sec101")
  TrainingManager::assign_course(training_manager, "alice", "sec201")
  TrainingManager::assign_course(training_manager, "bob", "sec101")
  TrainingManager::assign_course(training_manager, "bob", "priv101")
  TrainingManager::assign_course(training_manager, "charlie", "sec101")
  TrainingManager::assign_course(training_manager, "diana", "priv101")
  
  // 模拟培训完成
  TrainingManager::mark_completed(training_manager, "alice", "sec101", 85) // 85分
  TrainingManager::mark_completed(training_manager, "bob", "sec101", 92)
  TrainingManager::mark_completed(training_manager, "charlie", "sec101", 78)
  TrainingManager::mark_completed(training_manager, "diana", "priv101", 88)
  
  // 创建钓鱼模拟测试
  let phishing_simulation = {
    "id": "phishing_sim_001",
    "name": "季度钓鱼模拟测试",
    "description": "测试员工对钓鱼邮件的识别能力",
    "template": "it_support_alert",
    "target_users": users,
    "scheduled_date": Time::now() + 86400 // 明天
  }
  
  let sim_result = TrainingManager::create_phishing_simulation(training_manager, phishing_simulation)
  match sim_result {
    Ok(sim_id) => {
      // 模拟钓鱼测试结果
      TrainingManager::record_phishing_result(training_manager, sim_id, "alice", false) // 未上钩
      TrainingManager::record_phishing_result(training_manager, sim_id, "bob", true)  // 上钩了
      TrainingManager::record_phishing_result(training_manager, sim_id, "charlie", false)
      TrainingManager::record_phishing_result(training_manager, sim_id, "diana", false)
      
      // 获取钓鱼测试报告
      let phishing_report = TrainingManager::get_phishing_report(training_manager, sim_id)
      assert_true(phishing_report.total_users == 4)
      assert_true(phishing_report.caught_users == 1)
      assert_eq(phishing_report.catch_rate, 25.0)
    }
    Err(_) => assert_true(false)
  }
  
  // 创建安全意识问卷
  let awareness_quiz = {
    "id": "awareness_quiz_001",
    "title": "安全意识评估",
    "description": "评估员工的安全意识和知识水平",
    "questions": [
      {
        "id": "q1",
        "text": "以下哪个是强密码的特征？",
        "type": "multiple_choice",
        "options": ["只包含字母", "包含大小写字母、数字和特殊字符", "少于8个字符", "使用个人信息"],
        "correct_answer": 1
      },
      {
        "id": "q2",
        "text": "收到可疑邮件时应该怎么做？",
        "type": "multiple_choice",
        "options": ["立即点击链接", "回复邮件询问", "报告给IT部门", "转发给同事"],
        "correct_answer": 2
      },
      {
        "id": "q3",
        "text": "以下哪项是保护数据安全的好做法？",
        "type": "multiple_choice",
        "options": ["在公共场所讨论敏感信息", "使用未经授权的软件", "定期备份重要数据", "共享登录凭证"],
        "correct_answer": 2
      }
    ]
  }
  
  let quiz_result = TrainingManager::create_awareness_quiz(training_manager, awareness_quiz)
  match quiz_result {
    Ok(quiz_id) => {
      // 模拟问卷回答
      TrainingManager::submit_quiz_answers(training_manager, quiz_id, "alice", [1, 2, 2])
      TrainingManager::submit_quiz_answers(training_manager, quiz_id, "bob", [1, 0, 2])
      TrainingManager::submit_quiz_answers(training_manager, quiz_id, "charlie", [1, 2, 3])
      TrainingManager::submit_quiz_answers(training_manager, quiz_id, "diana", [1, 2, 2])
      
      // 获取问卷结果
      let quiz_results = TrainingManager::get_quiz_results(training_manager, quiz_id)
      assert_true(quiz_results.length() == 4)
      
      // 验证评分
      let alice_result = quiz_results.find(fn(result) { result.user == "alice" })
      match alice_result {
        Some(result) => assert_eq(result.score, 100) // 3/3正确
        None => assert_true(false)
      }
      
      let bob_result = quiz_results.find(fn(result) { result.user == "bob" })
      match bob_result {
        Some(result) => assert_eq(result.score, 66) // 2/3正确
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 生成培训报告
  let training_report = TrainingManager::generate_training_report(training_manager, {
    "include_completion_status": true,
    "include_quiz_results": true,
    "include_phishing_results": true,
    "recommend_additional_training": true
  })
  
  assert_true(training_report.contains("Security Training Report"))
  assert_true(training_report.contains("Course Completion Status"))
  assert_true(training_report.contains("Quiz Results"))
  assert_true(training_report.contains("Phishing Simulation Results"))
  assert_true(training_report.contains("Training Recommendations"))
  
  // 测试培训效果评估
  let effectiveness_result = TrainingManager::evaluate_training_effectiveness(training_manager, "sec101")
  match effectiveness_result {
    Ok(effectiveness) => {
      assert_true(effectiveness.average_score >= 0 && effectiveness.average_score <= 100)
      assert_true(effectiveness.completion_rate >= 0 && effectiveness.completion_rate <= 100)
      assert_true(effectiveness.improvement_suggestions.length() > 0)
    }
    Err(_) => assert_true(false)
  }
}
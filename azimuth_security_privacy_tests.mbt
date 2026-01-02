// Azimuth Security and Privacy Tests
// 测试安全性、隐私保护和数据加密功能

test "数据加密和解密" {
  // 测试数据加密和解密功能
  let encryption_key = EncryptionKey::generate(256)  // AES-256
  let sensitive_data = """
  {
    "user_id": "user-12345",
    "personal_info": {
      "name": "John Doe",
      "email": "john.doe@example.com",
      "phone": "+1-555-123-4567",
      "ssn": "123-45-6789"
    },
    "payment_info": {
      "credit_card": "4532-1234-5678-9012",
      "expiry": "12/25",
      "cvv": "123"
    },
    "health_info": {
      "conditions": ["Hypertension", "Diabetes Type 2"],
      "medications": ["Metformin", "Lisinopril"],
      "allergies": ["Penicillin"]
    }
  }
  """
  
  // 测试对称加密
  let encrypted_data = SymmetricEncryption::encrypt(encryption_key, sensitive_data, "AES-GCM")
  assert_not_eq(encrypted_data, sensitive_data)
  assert_false(encrypted_data.contains("John Doe"))
  assert_false(encrypted_data.contains("john.doe@example.com"))
  assert_false(encrypted_data.contains("4532-1234-5678-9012"))
  
  // 测试对称解密
  let decrypted_data = SymmetricEncryption::decrypt(encryption_key, encrypted_data, "AES-GCM")
  assert_eq(decrypted_data, sensitive_data)
  
  // 测试非对称加密
  let rsa_key_pair = RSAKeyPair::generate(2048)
  let public_key = RSAKeyPair::public_key(rsa_key_pair)
  let private_key = RSAKeyPair::private_key(rsa_key_pair)
  
  let encrypted_with_public = RSAEncryption::encrypt(public_key, "sensitive-user-data")
  assert_not_eq(encrypted_with_public, "sensitive-user-data")
  
  let decrypted_with_private = RSAEncryption::decrypt(private_key, encrypted_with_public)
  assert_eq(decrypted_with_private, "sensitive-user-data")
  
  // 测试混合加密（对称+非对称）
  let data_key = EncryptionKey::generate(256)
  let encrypted_data_key = RSAEncryption::encrypt(public_key, data_key.to_base64())
  let encrypted_payload = SymmetricEncryption::encrypt(data_key, sensitive_data, "AES-GCM")
  
  // 解密数据密钥
  let decrypted_data_key_base64 = RSAEncryption::decrypt(private_key, encrypted_data_key)
  let decrypted_data_key = EncryptionKey::from_base64(decrypted_data_key_base64)
  
  // 解密载荷
  let decrypted_payload = SymmetricEncryption::decrypt(decrypted_data_key, encrypted_payload, "AES-GCM")
  assert_eq(decrypted_payload, sensitive_data)
}

test "个人身份信息(PII)检测和屏蔽" {
  // 测试个人身份信息检测和屏蔽
  let telemetry_data = """
  {
    "trace_id": "trace-12345",
    "span_id": "span-67890",
    "operation_name": "user.registration",
    "attributes": {
      "user.id": "user-12345",
      "user.email": "alice.smith@example.com",
      "user.phone": "+1-555-987-6543",
      "user.name": "Alice Smith",
      "user.address": "123 Main St, New York, NY 10001",
      "user.ssn": "111-22-3333",
      "user.credit_card": "5555-4444-3333-2222",
      "user.ip_address": "192.168.1.100",
      "user.browser": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
      "session.id": "sess-abcdef123456",
      "request.id": "req-789012345678"
    },
    "events": [
      {
        "name": "form.submitted",
        "attributes": {
          "form.email": "bob.johnson@company.com",
          "form.phone": "212-555-1234"
        }
      }
    ]
  }
  """
  
  // 检测PII字段
  let pii_fields = PIIDetector::detect_fields(telemetry_data)
  assert_true(pii_fields.contains("user.email"))
  assert_true(pii_fields.contains("user.phone"))
  assert_true(pii_fields.contains("user.name"))
  assert_true(pii_fields.contains("user.address"))
  assert_true(pii_fields.contains("user.ssn"))
  assert_true(pii_fields.contains("user.credit_card"))
  assert_true(pii_fields.contains("user.ip_address"))
  assert_true(pii_fields.contains("form.email"))
  assert_true(pii_fields.contains("form.phone"))
  assert_false(pii_fields.contains("trace_id"))
  assert_false(pii_fields.contains("span_id"))
  assert_false(pii_fields.contains("operation_name"))
  
  // 屏蔽PII数据
  let masked_config = {
    "email": {
      "method": "partial_mask",
      "keep_chars": 2,
      "mask_char": "*",
      "keep_domain": true
    },
    "phone": {
      "method": "partial_mask",
      "keep_chars": 3,
      "mask_char": "*"
    },
    "name": {
      "method": "hash",
      "algorithm": "SHA-256"
    },
    "ssn": {
      "method": "full_mask",
      "mask_char": "*"
    },
    "credit_card": {
      "method": "tokenize",
      "format": "last4"
    },
    "ip_address": {
      "method": "generalize",
      "precision": "subnet"
    }
  }
  
  let masked_data = PIIMasker::mask_data(telemetry_data, pii_fields, masked_config)
  
  // 验证屏蔽效果
  assert_true(masked_data.contains("al***@example.com"))  // 邮箱部分屏蔽
  assert_true(masked_data.contains("+1-555-***"))        // 电话部分屏蔽
  assert_false(masked_data.contains("Alice Smith"))      // 姓名应该被哈希
  assert_true(masked_data.contains("***-**-****"))       // SSN完全屏蔽
  assert_true(masked_data.contains("****-****-****-2222")) // 信用卡只显示最后4位
  assert_true(masked_data.contains("192.168.1.0/24"))     // IP地址泛化到子网
}

test "访问控制和权限验证" {
  // 测试访问控制和权限验证
  let access_manager = AccessManager::new()
  
  // 定义角色和权限
  let roles = {
    "admin": [
      "telemetry.read",
      "telemetry.write",
      "telemetry.delete",
      "config.read",
      "config.write",
      "user.manage"
    ],
    "operator": [
      "telemetry.read",
      "telemetry.write",
      "config.read"
    ],
    "viewer": [
      "telemetry.read"
    ],
    "auditor": [
      "telemetry.read",
      "audit.read"
    ]
  }
  
  // 创建用户
  let users = [
    {
      "id": "user-001",
      "username": "admin_user",
      "roles": ["admin"]
    },
    {
      "id": "user-002",
      "username": "ops_user",
      "roles": ["operator"]
    },
    {
      "id": "user-003",
      "username": "view_user",
      "roles": ["viewer"]
    },
    {
      "id": "user-004",
      "username": "audit_user",
      "roles": ["auditor"]
    }
  ]
  
  // 初始化访问管理器
  AccessManager::setup_roles(access_manager, roles)
  AccessManager::setup_users(access_manager, users)
  
  // 测试权限验证
  assert_true(AccessManager::has_permission(access_manager, "user-001", "telemetry.delete"))
  assert_true(AccessManager::has_permission(access_manager, "user-001", "user.manage"))
  assert_true(AccessManager::has_permission(access_manager, "user-002", "telemetry.write"))
  assert_false(AccessManager::has_permission(access_manager, "user-002", "telemetry.delete"))
  assert_false(AccessManager::has_permission(access_manager, "user-002", "user.manage"))
  assert_true(AccessManager::has_permission(access_manager, "user-003", "telemetry.read"))
  assert_false(AccessManager::has_permission(access_manager, "user-003", "telemetry.write"))
  assert_true(AccessManager::has_permission(access_manager, "user-004", "audit.read"))
  assert_false(AccessManager::has_permission(access_manager, "user-004", "config.write"))
  
  // 测试资源级权限控制
  let resource_policies = [
    {
      "resource": "telemetry.traces",
      "conditions": {
        "service": ["payment-service", "auth-service"],
        "environment": ["production"]
      },
      "permissions": ["telemetry.read", "telemetry.write"]
    },
    {
      "resource": "telemetry.metrics",
      "conditions": {
        "service": ["*"],
        "environment": ["production", "staging"]
      },
      "permissions": ["telemetry.read"]
    }
  ]
  
  AccessManager::setup_resource_policies(access_manager, resource_policies)
  
  // 测试资源访问权限
  let payment_trace_access = AccessManager::check_resource_access(
    access_manager,
    "user-002",
    "telemetry.traces",
    {
      "service": "payment-service",
      "environment": "production"
    }
  )
  assert_true(payment_trace_access)
  
  let dev_trace_access = AccessManager::check_resource_access(
    access_manager,
    "user-002",
    "telemetry.traces",
    {
      "service": "dev-service",
      "environment": "development"
    }
  )
  assert_false(dev_trace_access)
  
  let staging_metrics_access = AccessManager::check_resource_access(
    access_manager,
    "user-003",
    "telemetry.metrics",
    {
      "service": "any-service",
      "environment": "staging"
    }
  )
  assert_true(staging_metrics_access)
}

test "审计日志和合规性" {
  // 测试审计日志和合规性功能
  let audit_logger = AuditLogger::new({
    "log_format": "JSON",
    "retention_days": 2555,  // 7年
    "encryption_enabled": true,
    "signing_enabled": true
  })
  
  // 记录审计事件
  let audit_events = [
    {
      "event_type": "user_login",
      "user_id": "user-001",
      "timestamp": 1735689600000000000L,
      "ip_address": "192.168.1.100",
      "user_agent": "Mozilla/5.0...",
      "success": true,
      "details": {
        "login_method": "password",
        "mfa_verified": true
      }
    },
    {
      "event_type": "data_access",
      "user_id": "user-002",
      "timestamp": 1735689660000000000L,
      "resource": "telemetry.traces",
      "action": "read",
      "success": true,
      "details": {
        "query_filter": "service=payment-service",
        "result_count": 150
      }
    },
    {
      "event_type": "config_change",
      "user_id": "user-001",
      "timestamp": 1735689720000000000L,
      "resource": "telemetry.config",
      "action": "update",
      "success": true,
      "details": {
        "changed_fields": ["sampling_rate", "batch_size"],
        "old_values": {"sampling_rate": 0.1, "batch_size": 50},
        "new_values": {"sampling_rate": 0.2, "batch_size": 100}
      }
    },
    {
      "event_type": "data_export",
      "user_id": "user-003",
      "timestamp": 1735689780000000000L,
      "resource": "telemetry.metrics",
      "action": "export",
      "success": true,
      "details": {
        "export_format": "CSV",
        "date_range": "2025-01-01 to 2025-01-02",
        "record_count": 5000
      }
    },
    {
      "event_type": "permission_denied",
      "user_id": "user-003",
      "timestamp": 1735689840000000000L,
      "resource": "telemetry.config",
      "action": "delete",
      "success": false,
      "details": {
        "reason": "insufficient_permissions",
        "required_permission": "telemetry.delete"
      }
    }
  ]
  
  // 记录审计事件
  for event in audit_events {
    AuditLogger::log_event(audit_logger, event)
  }
  
  // 测试审计日志查询
  let user_activity = AuditLogger::query_events(audit_logger, {
    "user_id": "user-001",
    "time_range": {
      "start": 1735689500000000000L,
      "end": 1735689900000000000L
    }
  })
  assert_eq(user_activity.length(), 2)
  assert_true(user_activity.all(fn(e) { e.user_id == "user-001" }))
  
  let failed_attempts = AuditLogger::query_events(audit_logger, {
    "success": false,
    "event_type": "permission_denied"
  })
  assert_eq(failed_attempts.length(), 1)
  assert_eq(failed_attempts[0].user_id, "user-003")
  
  // 测试合规性报告生成
  let compliance_report = AuditLogger::generate_compliance_report(audit_logger, {
    "period": "daily",
    "date": "2025-01-02",
    "requirements": ["GDPR", "SOX", "HIPAA"]
  })
  
  assert_true(compliance_report.contains("GDPR"))
  assert_true(compliance_report.contains("data_access"))
  assert_true(compliance_report.contains("data_export"))
  assert_true(compliance_report.contains("user_activity"))
  
  // 测试审计日志完整性验证
  let integrity_check = AuditLogger::verify_integrity(audit_logger)
  assert_true(integrity_check.valid)
  assert_eq(integrity_check.total_events, 5)
  assert_eq(integrity_check.tampered_events, 0)
}

test "数据脱敏和匿名化" {
  // 测试数据脱敏和匿名化功能
  let sensitive_telemetry = """
  {
    "trace_id": "trace-12345",
    "user_journey": [
      {
        "step": "login",
        "timestamp": 1735689600000000000L,
        "user_data": {
          "email": "john.doe@company.com",
          "ip": "203.0.113.1",
          "device_id": "device-abc123",
          "session_id": "sess-def456"
        }
      },
      {
        "step": "search",
        "timestamp": 1735689660000000000L,
        "search_data": {
          "query": "diabetes medication",
          "location": "New York, NY",
          "user_profile": {
            "age": 45,
            "gender": "male",
            "medical_conditions": ["diabetes", "hypertension"]
          }
        }
      },
      {
        "step": "purchase",
        "timestamp": 1735689720000000000L,
        "payment_data": {
          "card_type": "Visa",
          "last4": "1234",
          "billing_address": "123 Main St, New York, NY 10001",
          "amount": 29.99
        }
      }
    ]
  }
  """
  
  // 配置脱敏规则
  let anonymization_rules = {
    "email": {
      "method": "pseudonymization",
      "salt": "fixed-salt-value",
      "preserve_domain": false
    },
    "ip": {
      "method": "generalization",
      "precision": "class_b"  // 203.0.113.1 -> 203.0.0.0/16
    },
    "device_id": {
      "method": "tokenization",
      "vault": "secure-token-vault"
    },
    "session_id": {
      "method": "hash",
      "algorithm": "SHA-256",
      "salt": "session-salt"
    },
    "query": {
      "method": "suppression",
      "conditions": ["contains_medical_terms"]
    },
    "location": {
      "method": "generalization",
      "precision": "city"  // New York, NY -> New York
    },
    "age": {
      "method": "bucketing",
      "buckets": [18, 25, 35, 45, 55, 65, 100]
    },
    "gender": {
      "method": "suppression"
    },
    "medical_conditions": {
      "method": "suppression"
    },
    "billing_address": {
      "method": "tokenization",
      "vault": "secure-token-vault"
    }
  }
  
  // 应用脱敏
  let anonymized_data = DataAnonymizer::anonymize(sensitive_telemetry, anonymization_rules)
  
  // 验证脱敏效果
  assert_false(anonymized_data.contains("john.doe@company.com"))
  assert_false(anonymized_data.contains("203.0.113.1"))
  assert_false(anonymized_data.contains("device-abc123"))
  assert_false(anonymized_data.contains("sess-def456"))
  assert_false(anonymized_data.contains("diabetes medication"))
  assert_false(anonymized_data.contains("New York, NY"))
  assert_false(anonymized_data.contains("45"))
  assert_false(anonymized_data.contains("male"))
  assert_false(anonymized_data.contains("diabetes"))
  assert_false(anonymized_data.contains("123 Main St, New York, NY 10001"))
  
  // 验证数据结构保持完整
  assert_true(anonymized_data.contains("trace_id"))
  assert_true(anonymized_data.contains("user_journey"))
  assert_true(anonymized_data.contains("login"))
  assert_true(anonymized_data.contains("search"))
  assert_true(anonymized_data.contains("purchase"))
  assert_true(anonymized_data.contains("29.99"))  // 非敏感金额应保留
  
  // 测试匿名化质量评估
  let quality_metrics = DataAnonymizer::assess_quality(sensitive_telemetry, anonymized_data)
  assert_true(quality_metrics.privacy_score > 0.8)  // 高隐私保护
  assert_true(quality_metrics.utility_score > 0.6)   // 保持合理的数据效用
  assert_true(quality_metrics.re_identification_risk < 0.1)  // 低重新识别风险
}
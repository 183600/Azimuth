// Azimuth Security and Privacy Tests
// 安全和隐私测试用例

test "sensitive data masking and redaction" {
  // 测试敏感数据掩码和编辑
  let masking_rules = [
    @azimuth.MaskingRule {
      rule_id : "rule-credit-card",
      name : "Credit Card Number Masking",
      pattern : "\\b(\\d{4})[-\\s]?(\\d{4})[-\\s]?(\\d{4})[-\\s]?(\\d{4})\\b",
      replacement : "$1-****-****-$4",
      enabled : true,
      priority : 1,
      applies_to_attributes : ["payment.card.number", "credit.card"]
    },
    @azimuth.MaskingRule {
      rule_id : "rule-email",
      name : "Email Address Masking",
      pattern : "\\b([a-zA-Z0-9._%+-]+)@([a-zA-Z0-9.-]+\\.[a-zA-Z]{2,})\\b",
      replacement : "$1***@$2",
      enabled : true,
      priority : 2,
      applies_to_attributes : ["user.email", "email.address"]
    },
    @azimuth.MaskingRule {
      rule_id : "rule-ssn",
      name : "Social Security Number Masking",
      pattern : "\\b(\\d{3})[-\\s]?(\\d{2})[-\\s]?(\\d{4})\\b",
      replacement : "***-**-$3",
      enabled : true,
      priority : 1,
      applies_to_attributes : ["user.ssn", "social.security.number"]
    }
  ]
  
  let sensitive_data = @azimuth.TelemetryData {
    trace_id : "trace-1234567890",
    span_id : "span-1234567890",
    attributes : [
      ("user.email", @azimuth.StringValue("john.doe@example.com")),
      ("payment.card.number", @azimuth.StringValue("4111-1111-1111-1111")),
      ("user.ssn", @azimuth.StringValue("123-45-6789")),
      ("user.name", @azimuth.StringValue("John Doe")),
      ("transaction.amount", @azimuth.FloatValue(99.99))
    ]
  }
  
  // 验证掩码规则
  assert_eq(masking_rules.length(), 3)
  
  // 验证信用卡掩码规则
  let cc_rule = masking_rules[0]
  assert_eq(cc_rule.rule_id, "rule-credit-card")
  assert_eq(cc_rule.name, "Credit Card Number Masking")
  assert_eq(cc_rule.replacement, "$1-****-****-$4")
  assert_true(cc_rule.enabled)
  assert_eq(cc_rule.priority, 1)
  assert_eq(cc_rule.applies_to_attributes.length(), 2)
  
  // 验证邮箱掩码规则
  let email_rule = masking_rules[1]
  assert_eq(email_rule.rule_id, "rule-email")
  assert_eq(email_rule.name, "Email Address Masking")
  assert_eq(email_rule.replacement, "$1***@$2")
  assert_true(email_rule.enabled)
  assert_eq(email_rule.priority, 2)
  
  // 验证SSN掩码规则
  let ssn_rule = masking_rules[2]
  assert_eq(ssn_rule.rule_id, "rule-ssn")
  assert_eq(ssn_rule.name, "Social Security Number Masking")
  assert_eq(ssn_rule.replacement, "***-**-$3")
  assert_true(ssn_rule.enabled)
  assert_eq(ssn_rule.priority, 1)
  
  // 验证敏感数据
  assert_eq(sensitive_data.attributes.length(), 5)
  let email_attr = sensitive_data.attributes.filter(fn(attr) { attr.0 == "user.email" })
  assert_eq(email_attr.length(), 1)
  match email_attr[0].1 {
    @azimuth.StringValue(v) => assert_eq(v, "john.doe@example.com")
    _ => assert_true(false)
  }
}

test "data encryption and secure transmission" {
  // 测试数据加密和安全传输
  let encryption_config = @azimuth.EncryptionConfig {
    encryption_algorithm : @azimuth.EncryptionAlgorithm::AES256_GCM,
    key_derivation_function : @azimuth.KeyDerivationFunction::PBKDF2,
    key_rotation_interval_hours : 24,
    encryption_at_rest : true,
    encryption_in_transit : true,
    certificate_validation : true,
    tls_version : @azimuth.TLSVersion::TLS1_3,
    cipher_suites : [
      "TLS_AES_256_GCM_SHA384",
      "TLS_CHACHA20_POLY1305_SHA256",
      "TLS_AES_128_GCM_SHA256"
    ]
  }
  
  let secure_transmission = @azimuth.SecureTransmission {
    transmission_id : "trans-secure-123",
    endpoint : "https://otel-collector.example.com:4317",
    authentication_method : @azimuth.AuthMethod::MutualTLS,
    client_certificate : "-----BEGIN CERTIFICATE-----\n...\n-----END CERTIFICATE-----",
    client_private_key : "-----BEGIN PRIVATE KEY-----\n...\n-----END PRIVATE KEY-----",
    ca_certificate : "-----BEGIN CERTIFICATE-----\n...\n-----END CERTIFICATE-----",
    headers : [
      ("Authorization", "Bearer token123"),
      ("X-Telemetry-Source", "payment-service")
    ],
    timeout_ms : 5000,
    retry_policy : @azimuth.RetryPolicy {
      max_retries : 3,
      initial_backoff_ms : 1000,
      max_backoff_ms : 10000,
      backoff_multiplier : 2.0
    }
  }
  
  // 验证加密配置
  match encryption_config.encryption_algorithm {
    @azimuth.EncryptionAlgorithm::AES256_GCM => assert_true(true)
    _ => assert_true(false)
  }
  match encryption_config.key_derivation_function {
    @azimuth.KeyDerivationFunction::PBKDF2 => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(encryption_config.key_rotation_interval_hours, 24)
  assert_true(encryption_config.encryption_at_rest)
  assert_true(encryption_config.encryption_in_transit)
  assert_true(encryption_config.certificate_validation)
  match encryption_config.tls_version {
    @azimuth.TLSVersion::TLS1_3 => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(encryption_config.cipher_suites.length(), 3)
  
  // 验证安全传输
  assert_eq(secure_transmission.transmission_id, "trans-secure-123")
  assert_eq(secure_transmission.endpoint, "https://otel-collector.example.com:4317")
  match secure_transmission.authentication_method {
    @azimuth.AuthMethod::MutualTLS => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(secure_transmission.headers.length(), 2)
  assert_eq(secure_transmission.timeout_ms, 5000)
  assert_eq(secure_transmission.retry_policy.max_retries, 3)
  assert_eq(secure_transmission.retry_policy.initial_backoff_ms, 1000)
  assert_eq(secure_transmission.retry_policy.max_backoff_ms, 10000)
  assert_eq(secure_transmission.retry_policy.backoff_multiplier, 2.0)
}

test "access control and authorization" {
  // 测试访问控制和授权
  let access_policies = [
    @azimuth.AccessPolicy {
      policy_id : "policy-admin",
      name : "Administrator Access Policy",
      description : "Full access to all telemetry data",
      principals : ["admin@company.com", "service-account-admin"],
      resources : ["*"],
      actions : ["read", "write", "delete", "admin"],
      effect : @azimuth.PolicyEffect::Allow,
      conditions : [
        @azimuth.Condition {
          key : "source.ip",
          operator : @azimuth.ConditionOperator::IpAddress,
          values : ["10.0.0.0/8", "192.168.0.0/16"]
        }
      ]
    },
    @azimuth.AccessPolicy {
      policy_id : "policy-readonly",
      name : "Read-Only Access Policy",
      description : "Read-only access to production telemetry data",
      principals : ["analyst@company.com", "service-account-readonly"],
      resources : ["telemetry.production.*"],
      actions : ["read"],
      effect : @azimuth.PolicyEffect::Allow,
      conditions : [
        @azimuth.Condition {
          key : "time.of.day",
          operator : @azimuth.ConditionOperator::StringEquals,
          values : ["09:00-17:00"]
        }
      ]
    },
    @azimuth.AccessPolicy {
      policy_id : "policy-deny-pii",
      name : "PII Access Denial Policy",
      description : "Deny access to personally identifiable information",
      principals : ["*"],
      resources : ["telemetry.*.pii.*"],
      actions : ["*"],
      effect : @azimuth.PolicyEffect::Deny,
      conditions : []
    }
  ]
  
  // 验证访问策略
  assert_eq(access_policies.length(), 3)
  
  // 验证管理员策略
  let admin_policy = access_policies[0]
  assert_eq(admin_policy.policy_id, "policy-admin")
  assert_eq(admin_policy.name, "Administrator Access Policy")
  assert_eq(admin_policy.principals.length(), 2)
  assert_eq(admin_policy.resources.length(), 1)
  assert_eq(admin_policy.actions.length(), 4)
  match admin_policy.effect {
    @azimuth.PolicyEffect::Allow => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(admin_policy.conditions.length(), 1)
  
  // 验证只读策略
  let readonly_policy = access_policies[1]
  assert_eq(readonly_policy.policy_id, "policy-readonly")
  assert_eq(readonly_policy.name, "Read-Only Access Policy")
  assert_eq(readonly_policy.principals.length(), 2)
  assert_eq(readonly_policy.resources.length(), 1)
  assert_eq(readonly_policy.actions.length(), 1)
  match readonly_policy.effect {
    @azimuth.PolicyEffect::Allow => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证拒绝策略
  let deny_policy = access_policies[2]
  assert_eq(deny_policy.policy_id, "policy-deny-pii")
  assert_eq(deny_policy.name, "PII Access Denial Policy")
  assert_eq(deny_policy.principals.length(), 1)
  assert_eq(deny_policy.resources.length(), 1)
  assert_eq(deny_policy.actions.length(), 1)
  match deny_policy.effect {
    @azimuth.PolicyEffect::Deny => assert_true(true)
    _ => assert_true(false)
  }
}

test "audit logging and compliance tracking" {
  // 测试审计日志和合规性跟踪
  let audit_log = @azimuth.AuditLog {
    log_id : "audit-1234567890",
    timestamp : 1640995200000L,
    actor : {
      type : @azimuth.ActorType::User,
      id : "user-123456",
      name : "john.doe@company.com",
      ip_address : "192.168.1.100",
      user_agent : "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
    },
    action : @azimuth.AuditAction::TelemetryDataAccess,
    resource : {
      type : @azimuth.ResourceType::TelemetryData,
      id : "telemetry-batch-789",
      name : "Production Telemetry Batch"
    },
    result : @azimuth.AuditResult::Success,
    details : [
      ("query.filter", @azimuth.StringValue("service.name=payment-service")),
      ("records.accessed", @azimuth.IntValue(1000)),
      ("access.duration.ms", @azimuth.IntValue(250))
    ],
    compliance_tags : ["GDPR", "SOX", "HIPAA"]
  }
  
  let compliance_report = @azimuth.ComplianceReport {
    report_id : "compliance-2022-01",
    period_start : 1640995200000L, // 2022-01-01 00:00:00
    period_end : 1643587200000L,   // 2022-02-01 00:00:00
    frameworks : [
      @azimuth.ComplianceFramework {
        name : "GDPR",
        requirements : [
          @azimuth.ComplianceRequirement {
            requirement_id : "GDPR-Art-32",
            name : "Security of processing",
            status : @azimuth.ComplianceStatus::Compliant,
            evidence : ["encryption-configured", "access-controls-implemented", "audit-logging-enabled"]
          },
          @azimuth.ComplianceRequirement {
            requirement_id : "GDPR-Art-25",
            name : "Data protection by design and by default",
            status : @azimuth.ComplianceStatus::Compliant,
            evidence : ["data-minimization-applied", "pseudonymization-enabled"]
          }
        ]
      },
      @azimuth.ComplianceFramework {
        name : "SOX",
        requirements : [
          @azimuth.ComplianceRequirement {
            requirement_id : "SOX-404",
            name : "Internal control over financial reporting",
            status : @azimuth.ComplianceStatus::Compliant,
            evidence : ["audit-trail-intact", "data-retention-policy-enforced"]
          }
        ]
      }
    ]
  }
  
  // 验证审计日志
  assert_eq(audit_log.log_id, "audit-1234567890")
  assert_eq(audit_log.timestamp, 1640995200000L)
  match audit_log.actor.type {
    @azimuth.ActorType::User => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(audit_log.actor.id, "user-123456")
  assert_eq(audit_log.actor.name, "john.doe@company.com")
  match audit_log.action {
    @azimuth.AuditAction::TelemetryDataAccess => assert_true(true)
    _ => assert_true(false)
  }
  match audit_log.resource.type {
    @azimuth.ResourceType::TelemetryData => assert_true(true)
    _ => assert_true(false)
  }
  match audit_log.result {
    @azimuth.AuditResult::Success => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(audit_log.details.length(), 3)
  assert_eq(audit_log.compliance_tags.length(), 3)
  
  // 验证合规报告
  assert_eq(compliance_report.report_id, "compliance-2022-01")
  assert_eq(compliance_report.period_start, 1640995200000L)
  assert_eq(compliance_report.period_end, 1643587200000L)
  assert_eq(compliance_report.frameworks.length(), 2)
  
  // 验证GDPR框架
  let gdpr_framework = compliance_report.frameworks[0]
  assert_eq(gdpr_framework.name, "GDPR")
  assert_eq(gdpr_framework.requirements.length(), 2)
  let gdpr_req = gdpr_framework.requirements[0]
  assert_eq(gdpr_req.requirement_id, "GDPR-Art-32")
  assert_eq(gdpr_req.name, "Security of processing")
  match gdpr_req.status {
    @azimuth.ComplianceStatus::Compliant => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(gdpr_req.evidence.length(), 3)
}

test "data retention and privacy policies" {
  // 测试数据保留和隐私策略
  let retention_policies = [
    @azimuth.RetentionPolicy {
      policy_id : "policy-production-telemetry",
      name : "Production Telemetry Retention",
      data_type : @azimuth.DataType::TelemetryData,
      retention_period_days : 90,
      archival_enabled : true,
      archival_storage_tier : @azimuth.StorageTier::Cold,
      deletion_method : @azimuth.DeletionMethod::Secure,
      privacy_controls : [
        @azimuth.PrivacyControl {
          control_type : @azimuth.ControlType::Anonymization,
          enabled : true,
          parameters : [
            ("anonymization.method", @azimuth.StringValue("hashing")),
            ("hash.algorithm", @azimuth.StringValue("SHA-256"))
          ]
        },
        @azimuth.PrivacyControl {
          control_type : @azimuth.ControlType::DataMinimization,
          enabled : true,
          parameters : [
            ("minimize.attributes", @azimuth.ArrayValue([
              @azimuth.StringValue("user.email"),
              @azimuth.StringValue("user.phone")
            ]))
          ]
        }
      ]
    },
    @azimuth.RetentionPolicy {
      policy_id : "policy-security-events",
      name : "Security Events Retention",
      data_type : @azimuth.DataType::SecurityEvents,
      retention_period_days : 365,
      archival_enabled : true,
      archival_storage_tier : @azimuth.StorageTier::Cold,
      deletion_method : @azimuth.DeletionMethod::Secure,
      privacy_controls : [
        @azimuth.PrivacyControl {
          control_type : @azimuth.ControlType::AccessLogging,
          enabled : true,
          parameters : [
            ("log.retention.days", @azimuth.IntValue(730))
          ]
        }
      ]
    }
  ]
  
  let privacy_settings = @azimuth.PrivacySettings {
    settings_id : "privacy-global",
    data_subject_rights : [
      @azimuth.DataSubjectRight {
        right : @azimuth.RightType::Access,
        enabled : true,
        fulfillment_time_days : 30,
        verification_required : true
      },
      @azimuth.DataSubjectRight {
        right : @azimuth.RightType::Rectification,
        enabled : true,
        fulfillment_time_days : 30,
        verification_required : true
      },
      @azimuth.DataSubjectRight {
        right : @azimuth.RightType::Erasure,
        enabled : true,
        fulfillment_time_days : 30,
        verification_required : true
      },
      @azimuth.DataSubjectRight {
        right : @azimuth.RightType::Portability,
        enabled : true,
        fulfillment_time_days : 30,
        verification_required : true
      }
    ],
    consent_management : @azimuth.ConsentManagement {
      consent_required : true,
      consent_storage_days : 2555, // 7 years
      consent_withdrawal_allowed : true,
      granular_consent : true
    },
    cross_border_transfers : @azimuth.CrossBorderTransfers {
      allowed : false,
      adequacy_decisions : ["EU-US", "Swiss-US"],
      standard_contractual_clauses : true,
      binding_corporate_rules : false
    }
  }
  
  // 验证保留策略
  assert_eq(retention_policies.length(), 2)
  
  // 验证生产遥测保留策略
  let prod_policy = retention_policies[0]
  assert_eq(prod_policy.policy_id, "policy-production-telemetry")
  assert_eq(prod_policy.name, "Production Telemetry Retention")
  match prod_policy.data_type {
    @azimuth.DataType::TelemetryData => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(prod_policy.retention_period_days, 90)
  assert_true(prod_policy.archival_enabled)
  match prod_policy.archival_storage_tier {
    @azimuth.StorageTier::Cold => assert_true(true)
    _ => assert_true(false)
  }
  match prod_policy.deletion_method {
    @azimuth.DeletionMethod::Secure => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(prod_policy.privacy_controls.length(), 2)
  
  // 验证安全事件保留策略
  let security_policy = retention_policies[1]
  assert_eq(security_policy.policy_id, "policy-security-events")
  assert_eq(security_policy.name, "Security Events Retention")
  match security_policy.data_type {
    @azimuth.DataType::SecurityEvents => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(security_policy.retention_period_days, 365)
  
  // 验证隐私设置
  assert_eq(privacy_settings.settings_id, "privacy-global")
  assert_eq(privacy_settings.data_subject_rights.length(), 4)
  
  // 验证访问权
  let access_right = privacy_settings.data_subject_rights[0]
  match access_right.right {
    @azimuth.RightType::Access => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(access_right.enabled)
  assert_eq(access_right.fulfillment_time_days, 30)
  assert_true(access_right.verification_required)
  
  // 验证同意管理
  assert_true(privacy_settings.consent_management.consent_required)
  assert_eq(privacy_settings.consent_management.consent_storage_days, 2555)
  assert_true(privacy_settings.consent_management.consent_withdrawal_allowed)
  assert_true(privacy_settings.consent_management.granular_consent)
  
  // 验证跨境传输
  assert_false(privacy_settings.cross_border_transfers.allowed)
  assert_eq(privacy_settings.cross_border_transfers.adequacy_decisions.length(), 2)
  assert_true(privacy_settings.cross_border_transfers.standard_contractual_clauses)
  assert_false(privacy_settings.cross_border_transfers.binding_corporate_rules)
}
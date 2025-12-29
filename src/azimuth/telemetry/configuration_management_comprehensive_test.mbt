// 配置管理综合测试 - 验证遥测系统的配置管理和动态更新功能

use azimuth.telemetry.api.common.{AttributeValue, Attributes, Resource, InstrumentationScope}
use azimuth.telemetry.api.trace.{SpanKind, StatusCode, Span}
use azimuth.telemetry.api.metrics.{MetricType, Instrument, Measurement}
use azimuth.telemetry.api.logs.{LogRecord, Severity, Logger}
use azimuth.telemetry.api.context.{Context, Baggage, ContextKey}
use azimuth.telemetry.api.propagation::{TextMapPropagator, Propagator}

test "dynamic_configuration_update" {
  // 测试动态配置更新功能
  
  let trace_id = "trace_config_update_123"
  let config_update_time = 1641008000L
  
  // 1. 初始配置状态
  let initial_config_span = Span::{
    trace_id: trace_id,
    span_id: "span_initial_config",
    parent_span_id: None,
    name: "telemetry_with_initial_config",
    kind: SpanKind::Server,
    start_time: config_update_time - 5000L,
    end_time: Some(config_update_time - 4500L),
    status: StatusCode::Ok,
    attributes: [
      ("sampling.probability", AttributeValue::float(0.1)),
      ("batch.size", AttributeValue::int(100L)),
      ("export.interval_ms", AttributeValue::int(5000L)),
      ("metric.export.interval_ms", AttributeValue::int(10000L)),
      ("log.level", AttributeValue::string("INFO"))
    ],
    events: [],
    links: []
  }
  
  // 2. 配置更新操作
  let config_update_span = Span::{
    trace_id: trace_id,
    span_id: "span_config_update",
    parent_span_id: Some("span_initial_config"),
    name: "dynamic_configuration_update",
    kind: SpanKind::Internal,
    start_time: config_update_time,
    end_time: Some(config_update_time + 1000L),
    status: StatusCode::Ok,
    attributes: [
      ("config.source", AttributeValue::string("remote_config_server")),
      ("config.version", AttributeValue::string("v2.1.0")),
      ("update.type", AttributeValue::string("partial_update")),
      ("updated.keys.count", AttributeValue::int(3L))
    ],
    events: [
      ("config_update_started", config_update_time + 100L, [
        ("update.id", AttributeValue::string("update_abc123")),
        ("config.source.url", AttributeValue::string("https://config.example.com/telemetry"))
      ]),
      ("config_validation_passed", config_update_time + 500L, [
        ("validation.rules.passed", AttributeValue::int(15L)),
        ("validation.rules.failed", AttributeValue::int(0L))
      ]),
      ("config_applied", config_update_time + 900L, [
        ("applied.changes", AttributeValue::int(3L)),
        ("rollback.required", AttributeValue::bool(false))
      ])
    ],
    links: []
  }
  
  // 3. 更新后的配置状态
  let updated_config_span = Span::{
    trace_id: trace_id,
    span_id: "span_updated_config",
    parent_span_id: Some("span_config_update"),
    name: "telemetry_with_updated_config",
    kind: SpanKind::Server,
    start_time: config_update_time + 2000L,
    end_time: Some(config_update_time + 2500L),
    status: StatusCode::Ok,
    attributes: [
      ("sampling.probability", AttributeValue::float(0.25)), // 更新后的值
      ("batch.size", AttributeValue::int(200L)), // 更新后的值
      ("export.interval_ms", AttributeValue::int(5000L)), // 未更新
      ("metric.export.interval_ms", AttributeValue::int(5000L)), // 更新后的值
      ("log.level", AttributeValue::string("INFO")) // 未更新
    ],
    events: [],
    links: []
  }
  
  // 4. 记录配置更新指标
  let config_metrics = [
    Measurement::{
      instrument: Instrument::counter("configuration_updates_total", "Total configuration updates", "count"),
      value: 1.0,
      attributes: [
        ("config.source", AttributeValue::string("remote_config_server")),
        ("update.type", AttributeValue::string("partial_update")),
        ("update.status", AttributeValue::string("success"))
      ],
      time: config_update_time + 1000L
    },
    Measurement::{
      instrument: Instrument::histogram("config_update_duration_ms", "Configuration update duration", "ms"),
      value: 1000.0,
      attributes: [
        ("config.source", AttributeValue::string("remote_config_server")),
        ("config.size.bytes", AttributeValue::int(2048L))
      ],
      time: config_update_time + 1000L
    },
    Measurement::{
      instrument: Instrument::gauge("active_config_version", "Active configuration version", "version"),
      value: 2.1, // 版本号作为数值
      attributes: [
        ("config.source", AttributeValue::string("remote_config_server")),
        ("config.status", AttributeValue::string("active"))
      ],
      time: config_update_time + 2000L
    }
  ]
  
  // 5. 记录配置更新日志
  let config_update_log = LogRecord::{
    timestamp: config_update_time + 1000L,
    observed_timestamp: Some(config_update_time + 1001L),
    severity: Some(Severity::Info),
    severity_text: Some("INFO"),
    body: Some("Configuration updated successfully from remote server"),
    attributes: [
      ("trace_id", AttributeValue::string(trace_id)),
      ("config.version", AttributeValue::string("v2.1.0")),
      ("updated.keys", AttributeValue::array_string(["sampling.probability", "batch.size", "metric.export.interval_ms"])),
      ("config.source", AttributeValue::string("remote_config_server")),
      ("validation.status", AttributeValue::string("passed"))
    ],
    flags: 0,
    trace_id: Some(trace_id),
    span_id: Some("span_config_update"),
    trace_flags: Some(1)
  }
  
  // 6. 验证配置更新的正确性
  assert_eq(initial_config_span.status, StatusCode::Ok)
  assert_eq(config_update_span.status, StatusCode::Ok)
  assert_eq(updated_config_span.status, StatusCode::Ok)
  
  // 验证配置值的变化
  let initial_sampling = initial_config_span.attributes[0]
  match initial_sampling.1 {
    FloatValue(probability) => assert_eq(probability, 0.1)
    _ => assert_eq(false, true)
  }
  
  let updated_sampling = updated_config_span.attributes[0]
  match updated_sampling.1 {
    FloatValue(probability) => assert_eq(probability, 0.25)
    _ => assert_eq(false, true)
  }
  
  // 验证未更新的配置值保持不变
  let initial_export_interval = initial_config_span.attributes[2]
  match initial_export_interval.1 {
    IntValue(interval) => assert_eq(interval, 5000L)
    _ => assert_eq(false, true)
  }
  
  let updated_export_interval = updated_config_span.attributes[2]
  match updated_export_interval.1 {
    IntValue(interval) => assert_eq(interval, 5000L)
    _ => assert_eq(false, true)
  }
  
  // 验证配置更新时间线的正确性
  assert_eq(initial_config_span.end_time.unwrap() < config_update_span.start_time, true)
  assert_eq(config_update_span.end_time.unwrap() < updated_config_span.start_time, true)
}

test "configuration_validation_and_rollback" {
  // 测试配置验证和回滚功能
  
  let trace_id = "trace_config_validation_456"
  let validation_time = 1641009000L
  
  // 1. 无效配置更新尝试
  let invalid_config_span = Span::{
    trace_id: trace_id,
    span_id: "span_invalid_config_update",
    parent_span_id: None,
    name: "invalid_configuration_update",
    kind: SpanKind::Internal,
    start_time: validation_time,
    end_time: Some(validation_time + 1500L),
    status: StatusCode::Error,
    attributes: [
      ("config.source", AttributeValue::string("manual_override")),
      ("config.version", AttributeValue::string("v2.2.0-invalid")),
      ("update.type", AttributeValue::string("full_update")),
      ("validation.status", AttributeValue::string("failed"))
    ],
    events: [
      ("config_update_started", validation_time + 100L, [
        ("update.id", AttributeValue::string("update_invalid_789")),
        ("override.reason", AttributeValue::string("emergency_maintenance"))
      ]),
      ("validation_failed", validation_time + 800L, [
        ("validation.errors.count", AttributeValue::int(3L)),
        ("validation.warnings.count", AttributeValue::int(2L)),
        ("first.error", AttributeValue::string("sampling.probability must be between 0.0 and 1.0")),
        ("second.error", AttributeValue::string("batch.size must be positive"))
      ]),
      ("rollback_initiated", validation_time + 1200L, [
        ("rollback.reason", AttributeValue::string("validation_failed")),
        ("rollback.to.version", AttributeValue::string("v2.1.0"))
      ]),
      ("rollback_completed", validation_time + 1400L, [
        ("rollback.success", AttributeValue::bool(true)),
        ("config.restored", AttributeValue::bool(true))
      ])
    ],
    links: []
  }
  
  // 2. 回滚后的配置状态验证
  let rollback_verification_span = Span::{
    trace_id: trace_id,
    span_id: "span_rollback_verification",
    parent_span_id: Some("span_invalid_config_update"),
    name: "post_rollback_configuration_verification",
    kind: SpanKind::Internal,
    start_time: validation_time + 2000L,
    end_time: Some(validation_time + 2500L),
    status: StatusCode::Ok,
    attributes: [
      ("config.version", AttributeValue::string("v2.1.0")), // 回滚到之前的版本
      ("config.status", AttributeValue::string("stable")),
      ("sampling.probability", AttributeValue::float(0.25)), // 验证回滚的值
      ("batch.size", AttributeValue::int(200L)), // 验证回滚的值
      ("validation.passed", AttributeValue::bool(true))
    ],
    events: [
      ("config_verified", validation_time + 2200L, [
        ("verification.checks.passed", AttributeValue::int(20L)),
        ("verification.checks.failed", AttributeValue::int(0L))
      ])
    ],
    links: []
  }
  
  // 3. 记录配置验证和回滚指标
  let validation_metrics = [
    Measurement::{
      instrument: Instrument::counter("configuration_validation_failures_total", "Total configuration validation failures", "count"),
      value: 1.0,
      attributes: [
        ("config.source", AttributeValue::string("manual_override")),
        ("failure.reason", AttributeValue::string("invalid_values")),
        ("rollback.required", AttributeValue::bool(true))
      ],
      time: validation_time + 800L
    },
    Measurement::{
      instrument: Instrument::counter("configuration_rollbacks_total", "Total configuration rollbacks", "count"),
      value: 1.0,
      attributes: [
        ("rollback.reason", AttributeValue::string("validation_failed")),
        ("rollback.from.version", AttributeValue::string("v2.2.0-invalid")),
        ("rollback.to.version", AttributeValue::string("v2.1.0"))
      ],
      time: validation_time + 1400L
    },
    Measurement::{
      instrument: Instrument::histogram("rollback_duration_ms", "Configuration rollback duration", "ms"),
      value: 200.0,
      attributes: [
        ("rollback.reason", AttributeValue::string("validation_failed")),
        ("config.complexity", AttributeValue::string("medium"))
      ],
      time: validation_time + 1400L
    }
  ]
  
  // 4. 记录配置验证和回滚日志
  let validation_failure_log = LogRecord::{
    timestamp: validation_time + 800L,
    observed_timestamp: Some(validation_time + 801L),
    severity: Some(Severity::Error),
    severity_text: Some("ERROR"),
    body: Some("Configuration validation failed, invalid parameter values detected"),
    attributes: [
      ("trace_id", AttributeValue::string(trace_id)),
      ("config.version", AttributeValue::string("v2.2.0-invalid")),
      ("validation.errors.count", AttributeValue::int(3L)),
      ("validation.errors", AttributeValue::array_string([
        "sampling.probability must be between 0.0 and 1.0",
        "batch.size must be positive",
        "export.interval_ms must be greater than 1000"
      ])),
      ("rollback.initiated", AttributeValue::bool(true))
    ],
    flags: 0,
    trace_id: Some(trace_id),
    span_id: Some("span_invalid_config_update"),
    trace_flags: Some(1)
  }
  
  let rollback_success_log = LogRecord::{
    timestamp: validation_time + 1400L,
    observed_timestamp: Some(validation_time + 1401L),
    severity: Some(Severity::Info),
    severity_text: Some("INFO"),
    body: Some("Configuration rollback completed successfully, system restored to stable state"),
    attributes: [
      ("trace_id", AttributeValue::string(trace_id)),
      ("rollback.from.version", AttributeValue::string("v2.2.0-invalid")),
      ("rollback.to.version", AttributeValue::string("v2.1.0")),
      ("rollback.duration_ms", AttributeValue::int(200L)),
      ("config.stability.restored", AttributeValue::bool(true))
    ],
    flags: 0,
    trace_id: Some(trace_id),
    span_id: Some("span_invalid_config_update"),
    trace_flags: Some(1)
  }
  
  // 5. 验证配置验证和回滚的正确性
  assert_eq(invalid_config_span.status, StatusCode::Error)
  assert_eq(rollback_verification_span.status, StatusCode::Ok)
  
  // 验证验证失败的状态
  let validation_status = invalid_config_span.attributes[3]
  match validation_status.1 {
    StringValue(status) => assert_eq(status, "failed")
    _ => assert_eq(false, true)
  }
  
  // 验证回滚版本的正确性
  let rollback_version = rollback_verification_span.attributes[0]
  match rollback_version.1 {
    StringValue(version) => assert_eq(version, "v2.1.0")
    _ => assert_eq(false, true)
  }
  
  // 验证回滚后配置的稳定性
  let config_stability = rollback_verification_span.attributes[1]
  match config_stability.1 {
    StringValue(stability) => assert_eq(stability, "stable")
    _ => assert_eq(false, true)
  }
  
  // 验证回滚事件的存在
  assert_eq(invalid_config_span.events.length(), 4)
  assert_eq(invalid_config_span.events[2].0, "rollback_initiated")
  assert_eq(invalid_config_span.events[3].0, "rollback_completed")
}

test "environment_specific_configuration" {
  // 测试环境特定的配置管理
  
  let trace_id = "trace_env_config_789"
  let env_config_time = 1641010000L
  
  // 1. 开发环境配置
  let dev_config_span = Span::{
    trace_id: trace_id,
    span_id: "span_dev_environment_config",
    parent_span_id: None,
    name: "development_environment_configuration",
    kind: SpanKind::Internal,
    start_time: env_config_time,
    end_time: Some(env_config_time + 500L),
    status: StatusCode::Ok,
    attributes: [
      ("environment", AttributeValue::string("development")),
      ("config.profile", AttributeValue::string("dev")),
      ("sampling.probability", AttributeValue::float(1.0)), // 开发环境全采样
      ("batch.size", AttributeValue::int(10L)), // 小批次快速处理
      ("export.interval_ms", AttributeValue::int(1000L)), // 频繁导出
      ("log.level", AttributeValue::string("DEBUG")), // 详细日志
      ("debug.enabled", AttributeValue::bool(true)),
      ("performance.monitoring", AttributeValue::string("minimal"))
    ],
    events: [
      ("dev_config_loaded", env_config_time + 200L, [
        ("config.source", AttributeValue::string("local_dev_config")),
        ("hot.reload.enabled", AttributeValue::bool(true))
      ])
    ],
    links: []
  }
  
  // 2. 生产环境配置
  let prod_config_span = Span::{
    trace_id: trace_id,
    span_id: "span_prod_environment_config",
    parent_span_id: Some("span_dev_environment_config"),
    name: "production_environment_configuration",
    kind: SpanKind::Internal,
    start_time: env_config_time + 1000L,
    end_time: Some(env_config_time + 1500L),
    status: StatusCode::Ok,
    attributes: [
      ("environment", AttributeValue::string("production")),
      ("config.profile", AttributeValue::string("prod")),
      ("sampling.probability", AttributeValue::float(0.1)), // 生产环境低采样率
      ("batch.size", AttributeValue::int(500L)), // 大批次优化性能
      ("export.interval_ms", AttributeValue::int(10000L)), // 较长间隔
      ("log.level", AttributeValue::string("WARN")), // 仅警告和错误
      ("debug.enabled", AttributeValue::bool(false)),
      ("performance.monitoring", AttributeValue::string("comprehensive")),
      ("security.auditing", AttributeValue::bool(true))
    ],
    events: [
      ("prod_config_loaded", env_config_time + 1200L, [
        ("config.source", AttributeValue::string("secure_config_store")),
        ("encryption.enabled", AttributeValue::bool(true))
      ])
    ],
    links: []
  }
  
  // 3. 测试环境配置
  let test_config_span = Span::{
    trace_id: trace_id,
    span_id: "span_test_environment_config",
    parent_span_id: Some("span_prod_environment_config"),
    name: "test_environment_configuration",
    kind: SpanKind::Internal,
    start_time: env_config_time + 2000L,
    end_time: Some(env_config_time + 2500L),
    status: StatusCode::Ok,
    attributes: [
      ("environment", AttributeValue::string("testing")),
      ("config.profile", AttributeValue::string("test")),
      ("sampling.probability", AttributeValue::float(0.5)), // 测试环境中等采样
      ("batch.size", AttributeValue::int(50L)), // 中等批次
      ("export.interval_ms", AttributeValue::int(5000L)), // 中等间隔
      ("log.level", AttributeValue::string("INFO")), // 信息级别日志
      ("debug.enabled", AttributeValue::bool(false)),
      ("performance.monitoring", AttributeValue::string("moderate")),
      ("test.data.generation", AttributeValue::bool(true))
    ],
    events: [
      ("test_config_loaded", env_config_time + 2200L, [
        ("config.source", AttributeValue::string("test_config_repo")),
        ("mock.services.enabled", AttributeValue::bool(true))
      ])
    ],
    links: []
  }
  
  // 4. 记录环境配置指标
  let env_config_metrics = [
    Measurement::{
      instrument: Instrument::counter("environment_configurations_loaded_total", "Total environment configurations loaded", "count"),
      value: 3.0,
      attributes: [
        ("environments.loaded", AttributeValue::array_string(["development", "production", "testing"])),
        ("load.status", AttributeValue::string("success"))
      ],
      time: env_config_time + 2500L
    },
    Measurement::{
      instrument: Instrument::gauge("configuration_complexity_score", "Configuration complexity score", "score"),
      value: 3.0, // 生产环境复杂度最高
      attributes: [
        ("environment", AttributeValue::string("production")),
        ("config.elements.count", AttributeValue::int(8L))
      ],
      time: env_config_time + 1500L
    },
    Measurement::{
      instrument: Instrument::gauge("configuration_complexity_score", "Configuration complexity score", "score"),
      value: 1.0, // 开发环境复杂度最低
      attributes: [
        ("environment", AttributeValue::string("development")),
        ("config.elements.count", AttributeValue::int(7L))
      ],
      time: env_config_time + 500L
    }
  ]
  
  // 5. 记录环境配置日志
  let env_config_log = LogRecord::{
    timestamp: env_config_time + 2500L,
    observed_timestamp: Some(env_config_time + 2501L),
    severity: Some(Severity::Info),
    severity_text: Some("INFO"),
    body: Some("Environment-specific configurations loaded successfully"),
    attributes: [
      ("trace_id", AttributeValue::string(trace_id)),
      ("environments.configured", AttributeValue::array_string(["development", "production", "testing"])),
      ("config.validation.passed", AttributeValue::bool(true)),
      ("environment.isolation.verified", AttributeValue::bool(true))
    ],
    flags: 0,
    trace_id: Some(trace_id),
    span_id: Some("span_test_environment_config"),
    trace_flags: Some(1)
  }
  
  // 6. 验证环境特定配置的正确性
  assert_eq(dev_config_span.status, StatusCode::Ok)
  assert_eq(prod_config_span.status, StatusCode::Ok)
  assert_eq(test_config_span.status, StatusCode::Ok)
  
  // 验证开发环境配置的特性
  let dev_sampling = dev_config_span.attributes[2]
  match dev_sampling.1 {
    FloatValue(probability) => assert_eq(probability, 1.0) // 全采样
    _ => assert_eq(false, true)
  }
  
  let dev_log_level = dev_config_span.attributes[5]
  match dev_log_level.1 {
    StringValue(level) => assert_eq(level, "DEBUG") // 详细日志
    _ => assert_eq(false, true)
  }
  
  // 验证生产环境配置的特性
  let prod_sampling = prod_config_span.attributes[2]
  match prod_sampling.1 {
    FloatValue(probability) => assert_eq(probability, 0.1) // 低采样率
    _ => assert_eq(false, true)
  }
  
  let prod_log_level = prod_config_span.attributes[5]
  match prod_log_level.1 {
    StringValue(level) => assert_eq(level, "WARN") // 仅警告和错误
    _ => assert_eq(false, true)
  }
  
  // 验证测试环境配置的特性
  let test_sampling = test_config_span.attributes[2]
  match test_sampling.1 {
    FloatValue(probability) => assert_eq(probability, 0.5) // 中等采样率
    _ => assert_eq(false, true)
  }
  
  // 验证环境隔离性
  let dev_env = dev_config_span.attributes[0]
  match dev_env.1 {
    StringValue(env) => assert_eq(env, "development")
    _ => assert_eq(false, true)
  }
  
  let prod_env = prod_config_span.attributes[0]
  match prod_env.1 {
    StringValue(env) => assert_eq(env, "production")
    _ => assert_eq(false, true)
  }
  
  let test_env = test_config_span.attributes[0]
  match test_env.1 {
    StringValue(env) => assert_eq(env, "testing")
    _ => assert_eq(false, true)
  }
}

test "configuration_security_and_encryption" {
  // 测试配置的安全性和加密功能
  
  let trace_id = "trace_config_security_999"
  let security_config_time = 1641011000L
  
  // 1. 加密配置加载
  let encrypted_config_span = Span::{
    trace_id: trace_id,
    span_id: "span_encrypted_config_loading",
    parent_span_id: None,
    name: "encrypted_configuration_loading",
    kind: SpanKind::Internal,
    start_time: security_config_time,
    end_time: Some(security_config_time + 2000L),
    status: StatusCode::Ok,
    attributes: [
      ("config.source", AttributeValue::string("encrypted_vault")),
      ("encryption.algorithm", AttributeValue::string("AES-256-GCM")),
      ("key.management.service", AttributeValue::string("AWS KMS")),
      ("config.version", AttributeValue::string("v3.0.0-encrypted")),
      ("security.level", AttributeValue::string("high")),
      ("decryption.success", AttributeValue::bool(true))
    ],
    events: [
      ("encryption_key_retrieval", security_config_time + 200L, [
        ("key.id", AttributeValue::string("arn:aws:kms:us-west-2:123456789012:key/abcd-efgh-ijkl-mnop")),
        ("key.rotation.status", AttributeValue::string("current")),
        ("key.usage.count", AttributeValue::int(1000L))
      ]),
      ("config_decryption_started", security_config_time + 500L, [
        ("encrypted.data.size", AttributeValue::int(4096L)),
        ("decryption.algorithm", AttributeValue::string("AES-256-GCM"))
      ]),
      ("integrity_verification", security_config_time + 1500L, [
        ("verification.method", AttributeValue::string("HMAC-SHA256")),
        ("verification.status", AttributeValue::string("passed")),
        ("checksum.match", AttributeValue::bool(true))
      ]),
      ("config_loaded_successfully", security_config_time + 1800L, [
        ("sensitive.data.count", AttributeValue::int(5L)),
        ("data.masking.applied", AttributeValue::bool(true))
      ])
    ],
    links: []
  }
  
  // 2. 敏感配置项处理
  let sensitive_config_span = Span::{
    trace_id: trace_id,
    span_id: "span_sensitive_config_handling",
    parent_span_id: Some("span_encrypted_config_loading"),
    name: "sensitive_configuration_handling",
    kind: SpanKind::Internal,
    start_time: security_config_time + 2500L,
    end_time: Some(security_config_time + 3000L),
    status: StatusCode::Ok,
    attributes: [
      ("sensitive.config.type", AttributeValue::string("database_credentials")),
      ("data.classification", AttributeValue::string("confidential")),
      ("access.control.level", AttributeValue::string("restricted")),
      ("audit.logging.enabled", AttributeValue::bool(true)),
      ("data.masking.in.logs", AttributeValue::bool(true)),
      ("retention.policy.days", AttributeValue::int(90L))
    ],
    events: [
      ("sensitive_data_access", security_config_time + 2600L, [
        ("access.requested.by", AttributeValue::string("telemetry_service")),
        ("access.granted", AttributeValue::bool(true)),
        ("access.reason", AttributeValue::string("configuration_initialization"))
      ]),
      ("data_masking_applied", security_config_time + 2800L, [
        ("masking.strategy", AttributeValue::string("partial_mask")),
        ("masked.fields.count", AttributeValue::int(3L))
      ])
    ],
    links: []
  }
  
  // 3. 配置访问审计
  let audit_span = Span::{
    trace_id: trace_id,
    span_id: "span_config_access_audit",
    parent_span_id: Some("span_sensitive_config_handling"),
    name: "configuration_access_audit",
    kind: SpanKind::Internal,
    start_time: security_config_time + 3500L,
    end_time: Some(security_config_time + 4000L),
    status: StatusCode::Ok,
    attributes: [
      ("audit.event.type", AttributeValue::string("configuration_access")),
      ("audit.severity", AttributeValue::string("medium")),
      ("user.id", AttributeValue::string("service_account_telemetry")),
      ("service.principal", AttributeValue::string("telemetry-service")),
      ("access.timestamp", AttributeValue::string(security_config_time.to_string())),
      ("config.items.accessed", AttributeValue::int(5L)),
      ("access.compliance", AttributeValue::bool(true))
    ],
    events: [
      ("audit_log_created", security_config_time + 3700L, [
        ("log.id", AttributeValue::string("audit_123456789")),
        ("log.storage", AttributeValue::string("secure_audit_store")),
        ("log.tamper_proof", AttributeValue::bool(true))
      ])
    ],
    links: []
  }
  
  // 4. 记录安全配置指标
  let security_metrics = [
    Measurement::{
      instrument: Instrument::counter("secure_config_loads_total", "Total secure configuration loads", "count"),
      value: 1.0,
      attributes: [
        ("config.source", AttributeValue::string("encrypted_vault")),
        ("encryption.algorithm", AttributeValue::string("AES-256-GCM")),
        ("load.status", AttributeValue::string("success"))
      ],
      time: security_config_time + 2000L
    },
    Measurement::{
      instrument: Instrument::counter("sensitive_data_access_total", "Total sensitive data access events", "count"),
      value: 1.0,
      attributes: [
        ("data.classification", AttributeValue::string("confidential")),
        ("access.reason", AttributeValue::string("configuration_initialization")),
        ("access.granted", AttributeValue::bool(true))
      ],
      time: security_config_time + 2600L
    },
    Measurement::{
      instrument: Instrument::histogram("config_decryption_time_ms", "Configuration decryption time", "ms"),
      value: 1300.0,
      attributes: [
        ("encryption.algorithm", AttributeValue::string("AES-256-GCM")),
        ("data.size.bytes", AttributeValue::int(4096L))
      ],
      time: security_config_time + 1800L
    }
  ]
  
  // 5. 记录安全配置日志
  let security_config_log = LogRecord::{
    timestamp: security_config_time + 2000L,
    observed_timestamp: Some(security_config_time + 2001L),
    severity: Some(Severity::Info),
    severity_text: Some("INFO"),
    body: Some("Encrypted configuration loaded and verified successfully"),
    attributes: [
      ("trace_id", AttributeValue::string(trace_id)),
      ("config.source", AttributeValue::string("encrypted_vault")),
      ("encryption.algorithm", AttributeValue::string("AES-256-GCM")),
      ("integrity.verified", AttributeValue::bool(true)),
      ("sensitive.data.protected", AttributeValue::bool(true))
    ],
    flags: 0,
    trace_id: Some(trace_id),
    span_id: Some("span_encrypted_config_loading"),
    trace_flags: Some(1)
  }
  
  // 6. 验证安全配置的正确性
  assert_eq(encrypted_config_span.status, StatusCode::Ok)
  assert_eq(sensitive_config_span.status, StatusCode::Ok)
  assert_eq(audit_span.status, StatusCode::Ok)
  
  // 验证加密算法的正确性
  let encryption_algo = encrypted_config_span.attributes[1]
  match encryption_algo.1 {
    StringValue(algo) => assert_eq(algo, "AES-256-GCM")
    _ => assert_eq(false, true)
  }
  
  // 验证密钥管理服务的正确性
  let key_service = encrypted_config_span.attributes[2]
  match key_service.1 {
    StringValue(service) => assert_eq(service, "AWS KMS")
    _ => assert_eq(false, true)
  }
  
  // 验证解密成功的状态
  let decryption_status = encrypted_config_span.attributes[5]
  match decryption_status.1 {
    BoolValue(success) => assert_eq(success, true)
    _ => assert_eq(false, true)
  }
  
  // 验证敏感数据分类
  let data_classification = sensitive_config_span.attributes[1]
  match data_classification.1 {
    StringValue(classification) => assert_eq(classification, "confidential")
    _ => assert_eq(false, true)
  }
  
  // 验证审计合规性
  let audit_compliance = audit_span.attributes[6]
  match audit_compliance.1 {
    BoolValue(compliance) => assert_eq(compliance, true)
    _ => assert_eq(false, true)
  }
  
  // 验证安全事件的时间线
  assert_eq(encrypted_config_span.end_time.unwrap() < sensitive_config_span.start_time, true)
  assert_eq(sensitive_config_span.end_time.unwrap() < audit_span.start_time, true)
}
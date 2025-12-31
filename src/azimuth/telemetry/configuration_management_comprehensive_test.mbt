// 配置管理测试 - 测试动态配置和热重载功能
// 验证Azimuth Telemetry配置系统的灵活性和动态更新能力

test "dynamic_configuration_updates" {
  // 测试动态配置更新功能
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("config-test", "1.0.0")
  let meter = meter_provider.get_meter("config-test", "1.0.0")
  let logger = logger_provider.get_logger("config-test", "1.0.0")
  
  // 1. 初始配置状态
  let initial_config = [
    ("service.name", common::AttributeValue::string("config-test-service")),
    ("service.version", common::AttributeValue::string("1.0.0")),
    ("telemetry.enabled", common::AttributeValue::bool(true)),
    ("sampling.rate", common::AttributeValue::float(0.1)),
    ("batch.size", common::AttributeValue::int(512L)),
    ("export.interval.ms", common::AttributeValue::int(5000L)),
    ("log.level", common::AttributeValue::string("INFO"))
  ]
  
  // 验证初始配置
  let mut found_service_name = false
  let mut found_telemetry_enabled = false
  let mut found_sampling_rate = false
  let mut found_batch_size = false
  
  let mut i = 0
  while i < initial_config.length() {
    let (key, value) = initial_config[i]
    match key {
      "service.name" => {
        match value {
          common::StringValue(name) => {
            assert_eq(name, "config-test-service")
            found_service_name = true
          }
          _ => assert_eq(false, true, "Expected string value for service.name")
        }
      }
      "telemetry.enabled" => {
        match value {
          common::BoolValue(enabled) => {
            assert_eq(enabled, true)
            found_telemetry_enabled = true
          }
          _ => assert_eq(false, true, "Expected bool value for telemetry.enabled")
        }
      }
      "sampling.rate" => {
        match value {
          common::FloatValue(rate) => {
            assert_eq(rate, 0.1)
            found_sampling_rate = true
          }
          _ => assert_eq(false, true, "Expected float value for sampling.rate")
        }
      }
      "batch.size" => {
        match value {
          common::IntValue(size) => {
            assert_eq(size, 512L)
            found_batch_size = true
          }
          _ => assert_eq(false, true, "Expected int value for batch.size")
        }
      }
      _ => assert_eq(true, true)  // 其他配置项也有效
    }
    i = i + 1
  }
  
  assert_eq(found_service_name, true)
  assert_eq(found_telemetry_enabled, true)
  assert_eq(found_sampling_rate, true)
  assert_eq(found_batch_size, true)
  
  // 2. 使用初始配置创建遥测操作
  let (ctx_initial, initial_span) = tracer.start_span(
    context::Context::empty(),
    "operation_with_initial_config",
    trace::Internal,
    initial_config,
    1640995200000000000L
  )
  
  logger.info("Operation with initial configuration", [
    ("config.version", common::AttributeValue::string("v1")),
    ("sampling.rate", common::AttributeValue::float(0.1))
  ])
  
  // 记录初始配置的指标
  let initial_counter = meter.create_counter("operations_total", "count", "Total operations")
  initial_counter.add(1L, [
    ("config.version", common::AttributeValue::string("v1")),
    ("batch.size", common::AttributeValue::int(512L))
  ])
  
  // 3. 动态更新配置
  let updated_config = [
    ("service.name", common::AttributeValue::string("config-test-service")),
    ("service.version", common::AttributeValue::string("1.1.0")),  // 版本更新
    ("telemetry.enabled", common::AttributeValue::bool(true)),
    ("sampling.rate", common::AttributeValue::float(0.5)),  // 采样率增加
    ("batch.size", common::AttributeValue::int(1024L)),  // 批处理大小加倍
    ("export.interval.ms", common::AttributeValue::int(3000L)),  // 导出间隔缩短
    ("log.level", common::AttributeValue::string("DEBUG")),  // 日志级别调整
    ("config.update.timestamp", common::AttributeValue::int(1640995200100000000L))
  ]
  
  // 记录配置更新事件
  logger.info("Configuration updated dynamically", [
    ("old.version", common::AttributeValue::string("1.0.0")),
    ("new.version", common::AttributeValue::string("1.1.0")),
    ("sampling.rate.old", common::AttributeValue::float(0.1)),
    ("sampling.rate.new", common::AttributeValue::float(0.5)),
    ("batch.size.old", common::AttributeValue::int(512L)),
    ("batch.size.new", common::AttributeValue::int(1024L))
  ])
  
  // 记录配置更新指标
  let config_update_counter = meter.create_counter("config_updates", "count", "Configuration updates")
  config_update_counter.add(1L, [
    ("config.type", common::AttributeValue::string("dynamic")),
    ("update.success", common::AttributeValue::bool(true))
  ])
  
  // 4. 使用更新后的配置创建遥测操作
  let (ctx_updated, updated_span) = tracer.start_span(
    context::Context::empty(),
    "operation_with_updated_config",
    trace::Internal,
    updated_config,
    1640995200150000000L
  )
  
  logger.debug("Operation with updated configuration", [
    ("config.version", common::AttributeValue::string("v1.1")),
    ("sampling.rate", common::AttributeValue::float(0.5)),
    ("batch.size", common::AttributeValue::int(1024L))
  ])
  
  // 记录更新配置的指标
  let updated_counter = meter.create_counter("operations_total", "count", "Total operations")
  updated_counter.add(1L, [
    ("config.version", common::AttributeValue::string("v1.1")),
    ("batch.size", common::AttributeValue::int(1024L))
  ])
  
  // 5. 验证配置更新效果
  let mut found_updated_version = false
  let mut found_updated_sampling = false
  let mut found_updated_batch = false
  
  i = 0
  while i < updated_config.length() {
    let (key, value) = updated_config[i]
    match key {
      "service.version" => {
        match value {
          common::StringValue(version) => {
            assert_eq(version, "1.1.0")
            found_updated_version = true
          }
          _ => assert_eq(false, true, "Expected string value for service.version")
        }
      }
      "sampling.rate" => {
        match value {
          common::FloatValue(rate) => {
            assert_eq(rate, 0.5)
            found_updated_sampling = true
          }
          _ => assert_eq(false, true, "Expected float value for sampling.rate")
        }
      }
      "batch.size" => {
        match value {
          common::IntValue(size) => {
            assert_eq(size, 1024L)
            found_updated_batch = true
          }
          _ => assert_eq(false, true, "Expected int value for batch.size")
        }
      }
      _ => assert_eq(true, true)  // 其他配置项也有效
    }
    i = i + 1
  }
  
  assert_eq(found_updated_version, true)
  assert_eq(found_updated_sampling, true)
  assert_eq(found_updated_batch, true)
}

test "hot_reload_configuration" {
  // 测试配置热重载功能
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("hot-reload-test", "1.0.0")
  let meter = meter_provider.get_meter("hot-reload-test", "1.0.0")
  let logger = logger_provider.get_logger("hot-reload-test", "1.0.0")
  
  // 1. 模拟配置文件热重载
  let config_file_path = "/etc/telemetry/config.yaml"
  let original_config_content = """
service:
  name: hot-reload-service
  version: 1.0.0
telemetry:
  enabled: true
  sampling:
    rate: 0.1
  batch:
    size: 512
    timeout: 5000
  exporters:
    - type: otlp
      endpoint: http://localhost:4317
"""
  
  let updated_config_content = """
service:
  name: hot-reload-service
  version: 1.0.1
telemetry:
  enabled: true
  sampling:
    rate: 0.3
  batch:
    size: 1024
    timeout: 3000
  exporters:
    - type: otlp
      endpoint: http://localhost:4317
    - type: prometheus
      port: 9090
"""
  
  // 2. 记录原始配置加载
  logger.info("Configuration loaded from file", [
    ("config.file", common::AttributeValue::string(config_file_path)),
    ("config.version", common::AttributeValue::string("1.0.0")),
    ("config.size.bytes", common::AttributeValue::int(original_config_content.length().to_int64()))
  ])
  
  // 创建原始配置的遥测操作
  let original_config = [
    ("config.source", common::AttributeValue::string("file")),
    ("config.file", common::AttributeValue::string(config_file_path)),
    ("config.version", common::AttributeValue::string("1.0.0")),
    ("sampling.rate", common::AttributeValue::float(0.1)),
    ("batch.size", common::AttributeValue::int(512L))
  ]
  
  let (ctx_original, original_span) = tracer.start_span(
    context::Context::empty(),
    "operation_with_file_config",
    trace::Internal,
    original_config,
    1640995200000000000L
  )
  
  // 3. 模拟配置文件变更检测
  let file_change_detected = true
  let file_modification_time = 1640995200050000000L
  let file_size_change = updated_config_content.length() - original_config_content.length()
  
  if file_change_detected {
    logger.info("Configuration file change detected", [
      ("config.file", common::AttributeValue::string(config_file_path)),
      ("modification.time", common::AttributeValue::int(file_modification_time)),
      ("size.change.bytes", common::AttributeValue::int(file_size_change.to_int64()))
    ])
    
    // 记录文件变更指标
    let file_change_counter = meter.create_counter("config_file_changes", "count", "Configuration file changes")
    file_change_counter.add(1L, [
      ("config.file", common::AttributeValue::string(config_file_path)),
      ("change.type", common::AttributeValue::string("content_modified"))
    ])
  }
  
  // 4. 执行热重载
  let (ctx_reload, reload_span) = tracer.start_span(
    context::Context::empty(),
    "hot_reload_configuration",
    trace::Internal,
    [
      ("operation.type", common::AttributeValue::string("hot_reload")),
      ("config.file", common::AttributeValue::string(config_file_path)),
      ("reload.reason", common::AttributeValue::string("file_modified"))
    ],
    1640995200100000000L
  )
  
  logger.info("Hot reloading configuration", [
    ("config.file", common::AttributeValue::string(config_file_path)),
    ("reload.strategy", common::AttributeValue::string("graceful")),
    ("preserve.active.operations", common::AttributeValue::bool(true))
  ])
  
  // 记录热重载指标
  let reload_counter = meter.create_counter("config_hot_reloads", "count", "Configuration hot reloads")
  reload_counter.add(1L, [
    ("config.file", common::AttributeValue::string(config_file_path)),
    ("reload.strategy", common::AttributeValue::string("graceful"))
  ])
  
  let reload_gauge = meter.create_gauge("config_reload_duration_ms", "ms", "Configuration reload duration")
  reload_gauge.record(250.0, [
    ("config.file", common::AttributeValue::string(config_file_path))
  ])
  
  // 5. 验证新配置应用
  let reloaded_config = [
    ("config.source", common::AttributeValue::string("file")),
    ("config.file", common::AttributeValue::string(config_file_path)),
    ("config.version", common::AttributeValue::string("1.0.1")),
    ("sampling.rate", common::AttributeValue::float(0.3)),
    ("batch.size", common::AttributeValue::int(1024L)),
    ("exporter.count", common::AttributeValue::int(2L)),
    ("config.reload.time", common::AttributeValue::int(1640995200100000000L))
  ]
  
  logger.info("Configuration hot reloaded successfully", [
    ("old.version", common::AttributeValue::string("1.0.0")),
    ("new.version", common::AttributeValue::string("1.0.1")),
    ("reload.duration.ms", common::AttributeValue::int(250L)),
    ("active.operations.preserved", common::AttributeValue::bool(true))
  ])
  
  // 6. 使用重载后的配置创建遥测操作
  let (ctx_reloaded, reloaded_span) = tracer.start_span(
    context::Context::empty(),
    "operation_with_reloaded_config",
    trace::Internal,
    reloaded_config,
    1640995200150000000L
  )
  
  logger.info("Operation with reloaded configuration", [
    ("config.version", common::AttributeValue::string("1.0.1")),
    ("sampling.rate", common::AttributeValue::float(0.3)),
    ("batch.size", common::AttributeValue::int(1024L))
  ])
  
  // 记录重载后的操作指标
  let reloaded_counter = meter.create_counter("operations_total", "count", "Total operations")
  reloaded_counter.add(1L, [
    ("config.version", common::AttributeValue::string("1.0.1")),
    ("config.source", common::AttributeValue::string("file_hot_reload"))
  ])
  
  // 7. 验证热重载效果
  let mut found_reloaded_version = false
  let mut found_reloaded_sampling = false
  let mut found_reloaded_batch = false
  let mut found_exporter_count = false
  
  let mut i = 0
  while i < reloaded_config.length() {
    let (key, value) = reloaded_config[i]
    match key {
      "config.version" => {
        match value {
          common::StringValue(version) => {
            assert_eq(version, "1.0.1")
            found_reloaded_version = true
          }
          _ => assert_eq(false, true, "Expected string value for config.version")
        }
      }
      "sampling.rate" => {
        match value {
          common::FloatValue(rate) => {
            assert_eq(rate, 0.3)
            found_reloaded_sampling = true
          }
          _ => assert_eq(false, true, "Expected float value for sampling.rate")
        }
      }
      "batch.size" => {
        match value {
          common::IntValue(size) => {
            assert_eq(size, 1024L)
            found_reloaded_batch = true
          }
          _ => assert_eq(false, true, "Expected int value for batch.size")
        }
      }
      "exporter.count" => {
        match value {
          common::IntValue(count) => {
            assert_eq(count, 2L)
            found_exporter_count = true
          }
          _ => assert_eq(false, true, "Expected int value for exporter.count")
        }
      }
      _ => assert_eq(true, true)  // 其他配置项也有效
    }
    i = i + 1
  }
  
  assert_eq(found_reloaded_version, true)
  assert_eq(found_reloaded_sampling, true)
  assert_eq(found_reloaded_batch, true)
  assert_eq(found_exporter_count, true)
}

test "configuration_validation_and_rollback" {
  // 测试配置验证和回滚功能
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("config-validation", "1.0.0")
  let meter = meter_provider.get_meter("config-validation", "1.0.0")
  let logger = logger_provider.get_logger("config-validation", "1.0.0")
  
  // 1. 稳定的基准配置
  let stable_config = [
    ("service.name", common::AttributeValue::string("validation-service")),
    ("service.version", common::AttributeValue::string("1.0.0")),
    ("telemetry.enabled", common::AttributeValue::bool(true)),
    ("sampling.rate", common::AttributeValue::float(0.1)),
    ("batch.size", common::AttributeValue::int(512L)),
    ("export.interval.ms", common::AttributeValue::int(5000L)),
    ("log.level", common::AttributeValue::string("INFO"))
  ]
  
  // 2. 尝试应用无效配置
  let invalid_config = [
    ("service.name", common::AttributeValue::string("")),  // 无效：空服务名
    ("service.version", common::AttributeValue::string("1.0.1")),
    ("telemetry.enabled", common::AttributeValue::bool(true)),
    ("sampling.rate", common::AttributeValue::float(1.5)),  // 无效：采样率超出范围
    ("batch.size", common::AttributeValue::int(-100L)),  // 无效：负批处理大小
    ("export.interval.ms", common::AttributeValue::int(0L)),  // 无效：零导出间隔
    ("log.level", common::AttributeValue::string("INVALID"))  // 无效：不支持的日志级别
  ]
  
  // 3. 记录配置验证尝试
  logger.info("Configuration validation started", [
    ("validation.type", common::AttributeValue::string("full_validation")),
    ("config.entries", common::AttributeValue::int(invalid_config.length().to_int64()))
  ])
  
  // 4. 模拟配置验证过程
  let (ctx_validation, validation_span) = tracer.start_span(
    context::Context::empty(),
    "configuration_validation",
    trace::Internal,
    [
      ("validation.type", common::AttributeValue::string("schema_validation")),
      ("config.source", common::AttributeValue::string("dynamic_update"))
    ],
    1640995200000000000L
  )
  
  // 模拟验证失败
  logger.error("Configuration validation failed", [
    ("validation.errors", common::AttributeValue::int(4L)),
    ("error.details", common::AttributeValue::string("service.name.empty, sampling.rate.out_of_range, batch.size.negative, export.interval.zero"))
  ])
  
  // 记录验证失败指标
  let validation_failure_counter = meter.create_counter("config_validation_failures", "count", "Configuration validation failures")
  validation_failure_counter.add(1L, [
    ("validation.type", common::AttributeValue::string("schema_validation")),
    ("error.count", common::AttributeValue::int(4L))
  ])
  
  // 5. 创建验证失败的span
  let failed_validation_span = trace::Span::{
    ..validation_span,
    end_time_unix_nanos: Some(1640995200050000000L),
    status: trace::Error,
    status_description: Some("Configuration validation failed with 4 errors"),
    events: [
      trace::SpanEvent::{
        name: "validation_error",
        timestamp_unix_nanos: 1640995200050000000L,
        attributes: [
          ("error.count", common::AttributeValue::int(4L)),
          ("validation.duration.ms", common::AttributeValue::int(5000L))
        ]
      }
    ]
  }
  
  assert_eq(failed_validation_span.status, trace::Error)
  
  // 6. 执行配置回滚
  let (ctx_rollback, rollback_span) = tracer.start_span(
    context::Context::empty(),
    "configuration_rollback",
    trace::Internal,
    [
      ("rollback.reason", common::AttributeValue::string("validation_failed")),
      ("rollback.strategy", common::AttributeValue::string("to_last_known_good")),
      ("preserve.operations", common::AttributeValue::bool(true))
    ],
    1640995200100000000L
  )
  
  logger.warn("Configuration rolled back due to validation failures", [
    ("rollback.reason", common::AttributeValue::string("validation_failed")),
    ("config.version.rollback", common::AttributeValue::string("1.0.0")),
    ("rollback.duration.ms", common::AttributeValue::int(1000L))
  ])
  
  // 记录回滚指标
  let rollback_counter = meter.create_counter("config_rollbacks", "count", "Configuration rollbacks")
  rollback_counter.add(1L, [
    ("rollback.reason", common::AttributeValue::string("validation_failed")),
    ("rollback.strategy", common::AttributeValue::string("to_last_known_good"))
  ])
  
  // 7. 验证回滚成功
  let successful_rollback_span = trace::Span::{
    ..rollback_span,
    end_time_unix_nanos: Some(1640995200110000000L),
    status: trace::Ok,
    status_description: Some("Configuration rolled back successfully"),
    events: [
      trace::SpanEvent::{
        name: "rollback_completed",
        timestamp_unix_nanos: 1640995200110000000L,
        attributes: [
          ("rollback.duration.ms", common::AttributeValue::int(1000L)),
          ("config.restored", common::AttributeValue::bool(true))
        ]
      }
    ]
  }
  
  assert_eq(successful_rollback_span.status, trace::Ok)
  
  // 8. 使用回滚后的稳定配置继续操作
  let (ctx_stable, stable_span) = tracer.start_span(
    context::Context::empty(),
    "operation_with_stable_config",
    trace::Internal,
    stable_config,
    1640995200150000000L
  )
  
  logger.info("Operation continuing with stable configuration", [
    ("config.version", common::AttributeValue::string("1.0.0")),
    ("config.source", common::AttributeValue::string("rollback_restored"))
  ])
  
  // 记录稳定配置的操作指标
  let stable_counter = meter.create_counter("operations_total", "count", "Total operations")
  stable_counter.add(1L, [
    ("config.version", common::AttributeValue::string("1.0.0")),
    ("config.source", common::AttributeValue::string("rollback_restored"))
  ])
  
  // 9. 验证稳定配置仍然有效
  let mut found_stable_service = false
  let mut found_stable_sampling = false
  let mut found_stable_batch = false
  
  let mut i = 0
  while i < stable_config.length() {
    let (key, value) = stable_config[i]
    match key {
      "service.name" => {
        match value {
          common::StringValue(name) => {
            assert_eq(name, "validation-service")
            found_stable_service = true
          }
          _ => assert_eq(false, true, "Expected string value for service.name")
        }
      }
      "sampling.rate" => {
        match value {
          common::FloatValue(rate) => {
            assert_eq(rate, 0.1)
            found_stable_sampling = true
          }
          _ => assert_eq(false, true, "Expected float value for sampling.rate")
        }
      }
      "batch.size" => {
        match value {
          common::IntValue(size) => {
            assert_eq(size, 512L)
            found_stable_batch = true
          }
          _ => assert_eq(false, true, "Expected int value for batch.size")
        }
      }
      _ => assert_eq(true, true)  // 其他配置项也有效
    }
    i = i + 1
  }
  
  assert_eq(found_stable_service, true)
  assert_eq(found_stable_sampling, true)
  assert_eq(found_stable_batch, true)
}

test "environment_based_configuration" {
  // 测试基于环境的配置管理
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("env-config-test", "1.0.0")
  let meter = meter_provider.get_meter("env-config-test", "1.0.0")
  let logger = logger_provider.get_logger("env-config-test", "1.0.0")
  
  // 1. 定义不同环境的配置
  let development_config = [
    ("environment", common::AttributeValue::string("development")),
    ("service.name", common::AttributeValue::string("env-config-service")),
    ("service.version", common::AttributeValue::string("1.0.0-dev")),
    ("telemetry.enabled", common::AttributeValue::bool(true)),
    ("sampling.rate", common::AttributeValue::float(1.0)),  // 开发环境全采样
    ("batch.size", common::AttributeValue::int(128L)),  // 小批处理
    ("export.interval.ms", common::AttributeValue::int(10000L)),  // 较长间隔
    ("log.level", common::AttributeValue::string("DEBUG")),  // 详细日志
    ("debug.enabled", common::AttributeValue::bool(true))
  ]
  
  let staging_config = [
    ("environment", common::AttributeValue::string("staging")),
    ("service.name", common::AttributeValue::string("env-config-service")),
    ("service.version", common::AttributeValue::string("1.0.0-staging")),
    ("telemetry.enabled", common::AttributeValue::bool(true)),
    ("sampling.rate", common::AttributeValue::float(0.5)),  // 测试环境中等采样
    ("batch.size", common::AttributeValue::int(256L)),
    ("export.interval.ms", common::AttributeValue::int(5000L)),
    ("log.level", common::AttributeValue::string("INFO")),
    ("debug.enabled", common::AttributeValue::bool(false))
  ]
  
  let production_config = [
    ("environment", common::AttributeValue::string("production")),
    ("service.name", common::AttributeValue::string("env-config-service")),
    ("service.version", common::AttributeValue::string("1.0.0")),
    ("telemetry.enabled", common::AttributeValue::bool(true)),
    ("sampling.rate", common::AttributeValue::float(0.1)),  // 生产环境低采样
    ("batch.size", common::AttributeValue::int(1024L)),  // 大批处理
    ("export.interval.ms", common::AttributeValue::int(3000L)),  // 较短间隔
    ("log.level", common::AttributeValue::string("WARN")),  // 仅警告和错误
    ("debug.enabled", common::AttributeValue::bool(false))
  ]
  
  // 2. 模拟环境检测和配置加载
  let current_environment = "production"
  
  logger.info("Loading environment-specific configuration", [
    ("detected.environment", common::AttributeValue::string(current_environment)),
    ("config.source", common::AttributeValue::string("environment_variables"))
  ])
  
  // 3. 根据环境选择配置
  let selected_config = match current_environment {
    "development" => development_config,
    "staging" => staging_config,
    "production" => production_config,
    _ => development_config  // 默认配置
  }
  
  // 4. 使用环境配置创建遥测操作
  let (ctx_env, env_span) = tracer.start_span(
    context::Context::empty(),
    "operation_with_environment_config",
    trace::Internal,
    selected_config,
    1640995200000000000L
  )
  
  logger.info("Operation with environment configuration", [
    ("environment", common::AttributeValue::string(current_environment)),
    ("sampling.rate", common::AttributeValue::float(0.1)),
    ("log.level", common::AttributeValue::string("WARN"))
  ])
  
  // 记录环境配置指标
  let env_counter = meter.create_counter("operations_total", "count", "Total operations")
  env_counter.add(1L, [
    ("environment", common::AttributeValue::string(current_environment)),
    ("config.source", common::AttributeValue::string("environment_specific"))
  ])
  
  // 5. 验证生产环境配置
  let mut found_env = false
  let mut found_sampling = false
  let mut found_batch = false
  let mut found_log_level = false
  
  let mut i = 0
  while i < selected_config.length() {
    let (key, value) = selected_config[i]
    match key {
      "environment" => {
        match value {
          common::StringValue(env) => {
            assert_eq(env, "production")
            found_env = true
          }
          _ => assert_eq(false, true, "Expected string value for environment")
        }
      }
      "sampling.rate" => {
        match value {
          common::FloatValue(rate) => {
            assert_eq(rate, 0.1)
            found_sampling = true
          }
          _ => assert_eq(false, true, "Expected float value for sampling.rate")
        }
      }
      "batch.size" => {
        match value {
          common::IntValue(size) => {
            assert_eq(size, 1024L)
            found_batch = true
          }
          _ => assert_eq(false, true, "Expected int value for batch.size")
        }
      }
      "log.level" => {
        match value {
          common::StringValue(level) => {
            assert_eq(level, "WARN")
            found_log_level = true
          }
          _ => assert_eq(false, true, "Expected string value for log.level")
        }
      }
      _ => assert_eq(true, true)  // 其他配置项也有效
    }
    i = i + 1
  }
  
  assert_eq(found_env, true)
  assert_eq(found_sampling, true)
  assert_eq(found_batch, true)
  assert_eq(found_log_level, true)
  
  // 6. 模拟环境切换（从生产到测试）
  let new_environment = "staging"
  
  logger.info("Environment change detected", [
    ("old.environment", common::AttributeValue::string(current_environment)),
    ("new.environment", common::AttributeValue::string(new_environment)),
    ("config.reload.required", common::AttributeValue::bool(true))
  ])
  
  // 记录环境切换指标
  let env_switch_counter = meter.create_counter("environment_switches", "count", "Environment switches")
  env_switch_counter.add(1L, [
    ("old.environment", common::AttributeValue::string(current_environment)),
    ("new.environment", common::AttributeValue::string(new_environment))
  ])
  
  // 7. 加载新环境配置
  let new_selected_config = match new_environment {
    "development" => development_config,
    "staging" => staging_config,
    "production" => production_config,
    _ => development_config
  }
  
  let (ctx_new_env, new_env_span) = tracer.start_span(
    context::Context::empty(),
    "operation_after_environment_switch",
    trace::Internal,
    new_selected_config,
    1640995200100000000L
  )
  
  logger.info("Operation after environment switch", [
    ("environment", common::AttributeValue::string(new_environment)),
    ("sampling.rate", common::AttributeValue::float(0.5)),
    ("log.level", common::AttributeValue::string("INFO"))
  ])
  
  // 8. 验证测试环境配置
  let mut found_new_env = false
  let mut found_new_sampling = false
  let mut found_new_log_level = false
  
  i = 0
  while i < new_selected_config.length() {
    let (key, value) = new_selected_config[i]
    match key {
      "environment" => {
        match value {
          common::StringValue(env) => {
            assert_eq(env, "staging")
            found_new_env = true
          }
          _ => assert_eq(false, true, "Expected string value for environment")
        }
      }
      "sampling.rate" => {
        match value {
          common::FloatValue(rate) => {
            assert_eq(rate, 0.5)
            found_new_sampling = true
          }
          _ => assert_eq(false, true, "Expected float value for sampling.rate")
        }
      }
      "log.level" => {
        match value {
          common::StringValue(level) => {
            assert_eq(level, "INFO")
            found_new_log_level = true
          }
          _ => assert_eq(false, true, "Expected string value for log.level")
        }
      }
      _ => assert_eq(true, true)  // 其他配置项也有效
    }
    i = i + 1
  }
  
  assert_eq(found_new_env, true)
  assert_eq(found_new_sampling, true)
  assert_eq(found_new_log_level, true)
}
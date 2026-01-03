// Azimuth Telemetry Configuration Management Tests
// 专注于遥测系统配置管理、动态更新和验证的高级测试用例

test "遥测配置创建和验证测试" {
  // 创建完整的遥测配置
  let telemetry_config = azimuth::TelemetryConfig {
    service_name: "user-service",
    service_version: "1.2.3",
    environment: "production",
    
    tracing: azimuth::TracingConfig {
      enabled: true,
      sampling_rate: 0.1,
      exporter: "jaeger",
      endpoint: "http://jaeger:14268/api/traces",
      batch_size: 100,
      max_export_batch_size: 512,
      export_timeout_ms: 30000,
      max_queue_size: 2048
    },
    
    metrics: azimuth::MetricsConfig {
      enabled: true,
      exporter: "prometheus",
      endpoint: "http://prometheus:9090/metrics",
      port: 9090,
      path: "/metrics",
      collection_interval_ms: 10000,
      default_histogram_boundaries: [10.0, 25.0, 50.0, 100.0, 250.0, 500.0, 1000.0, 2500.0, 5000.0, 10000.0]
    },
    
    logging: azimuth::LoggingConfig {
      enabled: true,
      level: "info",
      format: "json",
      output: "stdout",
      include_trace_context: true,
      include_span_context: true,
      include_resource_attributes: true
    },
    
    resource: azimuth::ResourceConfig {
      attributes: [
        ("service.instance.id", "instance-12345"),
        ("host.name", "server-01"),
        ("host.ip", "192.168.1.10"),
        ("cloud.provider", "aws"),
        ("cloud.region", "us-east-1"),
        ("cloud.availability_zone", "us-east-1a")
      ]
    }
  }
  
  // 验证配置创建
  assert_eq(telemetry_config.service_name, "user-service")
  assert_eq(telemetry_config.service_version, "1.2.3")
  assert_eq(telemetry_config.environment, "production")
  
  // 验证追踪配置
  assert_true(telemetry_config.tracing.enabled)
  assert_eq(telemetry_config.tracing.sampling_rate, 0.1)
  assert_eq(telemetry_config.tracing.exporter, "jaeger")
  assert_eq(telemetry_config.tracing.endpoint, "http://jaeger:14268/api/traces")
  
  // 验证指标配置
  assert_true(telemetry_config.metrics.enabled)
  assert_eq(telemetry_config.metrics.exporter, "prometheus")
  assert_eq(telemetry_config.metrics.port, 9090)
  assert_eq(telemetry_config.metrics.path, "/metrics")
  
  // 验证日志配置
  assert_true(telemetry_config.logging.enabled)
  assert_eq(telemetry_config.logging.level, "info")
  assert_eq(telemetry_config.logging.format, "json")
  assert_true(telemetry_config.logging.include_trace_context)
  
  // 验证资源配置
  assert_eq(telemetry_config.resource.attributes.length(), 6)
  
  // 验证配置有效性
  let validation_result = azimuth::validate_config(telemetry_config)
  match validation_result {
    azimuth::ValidationResult::Valid => assert_true(true)
    azimuth::ValidationResult::Invalid(errors) => assert_true(false, "Config should be valid, but got errors: " + errors.join(", "))
  }
}

test "遥测配置动态更新测试" {
  // 创建初始配置
  let initial_config = azimuth::TelemetryConfig {
    service_name: "order-service",
    service_version: "1.0.0",
    environment: "staging",
    
    tracing: azimuth::TracingConfig {
      enabled: true,
      sampling_rate: 0.5,
      exporter: "zipkin",
      endpoint: "http://zipkin:9411/api/v2/spans",
      batch_size: 50,
      max_export_batch_size: 256,
      export_timeout_ms: 15000,
      max_queue_size: 1024
    },
    
    metrics: azimuth::MetricsConfig {
      enabled: true,
      exporter: "prometheus",
      endpoint: "http://prometheus:9090/metrics",
      port: 9090,
      path: "/metrics",
      collection_interval_ms: 15000,
      default_histogram_boundaries: [5.0, 10.0, 25.0, 50.0, 100.0, 250.0, 500.0]
    },
    
    logging: azimuth::LoggingConfig {
      enabled: true,
      level: "debug",
      format: "text",
      output: "stdout",
      include_trace_context: true,
      include_span_context: false,
      include_resource_attributes: true
    },
    
    resource: azimuth::ResourceConfig {
      attributes: [
        ("service.instance.id", "instance-67890"),
        ("host.name", "server-02")
      ]
    }
  }
  
  // 应用初始配置
  let config_id = azimuth::apply_config(initial_config)
  assert_true(config_id.length() > 0)
  
  // 验证配置已应用
  let current_config = azimuth::get_current_config()
  assert_eq(current_config.service_name, "order-service")
  assert_eq(current_config.tracing.sampling_rate, 0.5)
  assert_eq(current_config.logging.level, "debug")
  
  // 创建配置更新
  let config_update = azimuth::ConfigUpdate {
    config_id: config_id,
    updates: [
      ("tracing.sampling_rate", "0.1"),
      ("tracing.batch_size", "100"),
      ("metrics.collection_interval_ms", "10000"),
      ("logging.level", "info"),
      ("logging.format", "json"),
      ("resource.attributes", "[(\"deployment.version\", \"v1.1.0\")]"))
    ]
  }
  
  // 应用配置更新
  let update_result = azimuth::update_config(config_update)
  match update_result {
    azimuth::UpdateResult::Success => assert_true(true)
    azimuth::UpdateResult::Failure(error) => assert_true(false, "Update failed: " + error)
    azimuth::UpdateResult::Invalid(errors) => assert_true(false, "Invalid update: " + errors.join(", "))
  }
  
  // 验证配置已更新
  let updated_config = azimuth::get_current_config()
  assert_eq(updated_config.tracing.sampling_rate, 0.1)
  assert_eq(updated_config.tracing.batch_size, 100)
  assert_eq(updated_config.metrics.collection_interval_ms, 10000)
  assert_eq(updated_config.logging.level, "info")
  assert_eq(updated_config.logging.format, "json")
}

test "遥测配置版本管理测试" {
  // 创建配置版本1
  let config_v1 = azimuth::TelemetryConfig {
    service_name: "payment-service",
    service_version: "1.0.0",
    environment: "production",
    
    tracing: azimuth::TracingConfig {
      enabled: true,
      sampling_rate: 0.01,
      exporter: "jaeger",
      endpoint: "http://jaeger:14268/api/traces",
      batch_size: 100,
      max_export_batch_size: 512,
      export_timeout_ms: 30000,
      max_queue_size: 2048
    },
    
    metrics: azimuth::MetricsConfig {
      enabled: true,
      exporter: "prometheus",
      endpoint: "http://prometheus:9090/metrics",
      port: 9090,
      path: "/metrics",
      collection_interval_ms: 10000,
      default_histogram_boundaries: [10.0, 25.0, 50.0, 100.0, 250.0, 500.0, 1000.0]
    },
    
    logging: azimuth::LoggingConfig {
      enabled: true,
      level: "warn",
      format: "json",
      output: "stdout",
      include_trace_context: true,
      include_span_context: true,
      include_resource_attributes: true
    },
    
    resource: azimuth::ResourceConfig {
      attributes: [
        ("service.instance.id", "instance-pmt-01"),
        ("host.name", "payment-server-01")
      ]
    }
  }
  
  // 保存配置版本1
  let version_v1 = azimuth::save_config_version(config_v1, "Initial production configuration")
  assert_eq(version_v1, "v1.0.0")
  
  // 创建配置版本2
  let mut config_v2 = config_v1
  config_v2.service_version = "1.1.0"
  config_v2.tracing.sampling_rate = 0.05
  config_v2.logging.level = "info"
  
  // 保存配置版本2
  let version_v2 = azimuth::save_config_version(config_v2, "Updated sampling rate and log level")
  assert_eq(version_v2, "v1.1.0")
  
  // 验证配置版本历史
  let version_history = azimuth::get_config_version_history()
  assert_eq(version_history.length(), 2)
  assert_eq(version_history[0].version, "v1.0.0")
  assert_eq(version_history[1].version, "v1.1.0")
  assert_eq(version_history[0].description, "Initial production configuration")
  assert_eq(version_history[1].description, "Updated sampling rate and log level")
  
  // 恢复到版本1
  let restore_result = azimuth::restore_config_version("v1.0.0")
  match restore_result {
    azimuth::RestoreResult::Success => assert_true(true)
    azimuth::RestoreResult::Failure(error) => assert_true(false, "Restore failed: " + error)
    azimuth::RestoreResult::VersionNotFound => assert_true(false, "Version v1.0.0 should exist")
  }
  
  // 验证配置已恢复
  let restored_config = azimuth::get_current_config()
  assert_eq(restored_config.service_version, "1.0.0")
  assert_eq(restored_config.tracing.sampling_rate, 0.01)
  assert_eq(restored_config.logging.level, "warn")
}

test "遥测配置模板和继承测试" {
  // 创建基础配置模板
  let base_template = azimuth::ConfigTemplate {
    name: "base-production-template",
    description: "基础生产环境配置模板",
    template: azimuth::TelemetryConfig {
      service_name: "${SERVICE_NAME}",
      service_version: "${SERVICE_VERSION}",
      environment: "production",
      
      tracing: azimuth::TracingConfig {
        enabled: true,
        sampling_rate: 0.1,
        exporter: "jaeger",
        endpoint: "${JAEGER_ENDPOINT}",
        batch_size: 100,
        max_export_batch_size: 512,
        export_timeout_ms: 30000,
        max_queue_size: 2048
      },
      
      metrics: azimuth::MetricsConfig {
        enabled: true,
        exporter: "prometheus",
        endpoint: "${PROMETHEUS_ENDPOINT}",
        port: 9090,
        path: "/metrics",
        collection_interval_ms: 10000,
        default_histogram_boundaries: [10.0, 25.0, 50.0, 100.0, 250.0, 500.0, 1000.0]
      },
      
      logging: azimuth::LoggingConfig {
        enabled: true,
        level: "info",
        format: "json",
        output: "stdout",
        include_trace_context: true,
        include_span_context: true,
        include_resource_attributes: true
      },
      
      resource: azimuth::ResourceConfig {
        attributes: [
          ("service.instance.id", "${INSTANCE_ID}"),
          ("host.name", "${HOSTNAME}")
        ]
      }
    }
  }
  
  // 保存配置模板
  let template_id = azimuth::save_config_template(base_template)
  assert_true(template_id.length() > 0)
  
  // 创建服务特定模板（继承基础模板）
  let service_template = azimuth::ConfigTemplate {
    name: "web-service-template",
    description: "Web服务特定配置模板",
    template: azimuth::TelemetryConfig {
      service_name: "${SERVICE_NAME}",
      service_version: "${SERVICE_VERSION}",
      environment: "production",
      
      tracing: azimuth::TracingConfig {
        enabled: true,
        sampling_rate: 0.05,  // 覆盖基础模板的采样率
        exporter: "jaeger",
        endpoint: "${JAEGER_ENDPOINT}",
        batch_size: 200,  // 覆盖基础模板的批处理大小
        max_export_batch_size: 512,
        export_timeout_ms: 30000,
        max_queue_size: 2048
      },
      
      metrics: azimuth::MetricsConfig {
        enabled: true,
        exporter: "prometheus",
        endpoint: "${PROMETHEUS_ENDPOINT}",
        port: 9090,
        path: "/metrics",
        collection_interval_ms: 5000,  // 覆盖基础模板的收集间隔
        default_histogram_boundaries: [10.0, 25.0, 50.0, 100.0, 250.0, 500.0, 1000.0]
      },
      
      logging: azimuth::LoggingConfig {
        enabled: true,
        level: "debug",  // 覆盖基础模板的日志级别
        format: "json",
        output: "stdout",
        include_trace_context: true,
        include_span_context: true,
        include_resource_attributes: true
      },
      
      resource: azimuth::ResourceConfig {
        attributes: [
          ("service.instance.id", "${INSTANCE_ID}"),
          ("host.name", "${HOSTNAME}"),
          ("service.type", "web")  // 新增属性
        ]
      }
    },
    parent_template: Some(template_id)
  }
  
  // 保存服务特定模板
  let service_template_id = azimuth::save_config_template(service_template)
  
  // 从模板实例化配置
  let variables = [
    ("SERVICE_NAME", "web-frontend"),
    ("SERVICE_VERSION", "2.1.0"),
    ("JAEGER_ENDPOINT", "http://jaeger:14268/api/traces"),
    ("PROMETHEUS_ENDPOINT", "http://prometheus:9090/metrics"),
    ("INSTANCE_ID", "instance-web-01"),
    ("HOSTNAME", "web-01")
  ]
  
  let instantiated_config = azimuth::instantiate_config_from_template(service_template_id, variables)
  
  // 验证实例化配置
  assert_eq(instantiated_config.service_name, "web-frontend")
  assert_eq(instantiated_config.service_version, "2.1.0")
  assert_eq(instantiated_config.tracing.sampling_rate, 0.05)  // 使用服务模板的值
  assert_eq(instantiated_config.tracing.batch_size, 200)  // 使用服务模板的值
  assert_eq(instantiated_config.metrics.collection_interval_ms, 5000)  // 使用服务模板的值
  assert_eq(instantiated_config.logging.level, "debug")  // 使用服务模板的值
  
  // 验证新增的资源属性
  let service_type_attr = instantiated_config.resource.attributes.find(fn(attr) { attr.0 == "service.type" })
  match service_type_attr {
    Some((_, value)) => assert_eq(value, "web")
    None => assert_true(false)
  }
}

test "遥测配置验证和错误处理测试" {
  // 测试无效配置
  let invalid_config = azimuth::TelemetryConfig {
    service_name: "",  // 无效：空服务名
    service_version: "1.0.0",
    environment: "production",
    
    tracing: azimuth::TracingConfig {
      enabled: true,
      sampling_rate: 1.5,  // 无效：采样率超出范围
      exporter: "invalid-exporter",  // 无效：不支持的导出器
      endpoint: "invalid-url",  // 无效：URL格式错误
      batch_size: 0,  // 无效：批处理大小为0
      max_export_batch_size: 512,
      export_timeout_ms: -1000,  // 无效：负数超时
      max_queue_size: 2048
    },
    
    metrics: azimuth::MetricsConfig {
      enabled: true,
      exporter: "prometheus",
      endpoint: "http://prometheus:9090/metrics",
      port: 70000,  // 无效：端口号超出范围
      path: "",  // 无效：空路径
      collection_interval_ms: 0,  // 无效：收集间隔为0
      default_histogram_boundaries: []  // 无效：空边界
    },
    
    logging: azimuth::LoggingConfig {
      enabled: true,
      level: "invalid-level",  // 无效：不支持的日志级别
      format: "xml",  // 无效：不支持的格式
      output: "invalid-output",  // 无效：不支持的输出
      include_trace_context: true,
      include_span_context: true,
      include_resource_attributes: true
    },
    
    resource: azimuth::ResourceConfig {
      attributes: [
        ("", "value"),  // 无效：空键名
        ("key", "")  // 无效：空值
      ]
    }
  }
  
  // 验证无效配置
  let validation_result = azimuth::validate_config(invalid_config)
  match validation_result {
    azimuth::ValidationResult::Valid => assert_true(false, "Config should be invalid")
    azimuth::ValidationResult::Invalid(errors) => {
      assert_true(errors.length() > 0)
      assert_true(errors.some(fn(error) { error.contains("service_name") }))
      assert_true(errors.some(fn(error) { error.contains("sampling_rate") }))
      assert_true(errors.some(fn(error) { error.contains("exporter") }))
      assert_true(errors.some(fn(error) { error.contains("batch_size") }))
      assert_true(errors.some(fn(error) { error.contains("export_timeout_ms") }))
      assert_true(errors.some(fn(error) { error.contains("port") }))
      assert_true(errors.some(fn(error) { error.contains("path") }))
      assert_true(errors.some(fn(error) { error.contains("collection_interval_ms") }))
      assert_true(errors.some(fn(error) { error.contains("level") }))
      assert_true(errors.some(fn(error) { error.contains("format") }))
    }
  }
  
  // 测试部分无效配置
  let partially_invalid_config = azimuth::TelemetryConfig {
    service_name: "valid-service",
    service_version: "1.0.0",
    environment: "production",
    
    tracing: azimuth::TracingConfig {
      enabled: true,
      sampling_rate: 0.1,  // 有效
      exporter: "jaeger",  // 有效
      endpoint: "http://jaeger:14268/api/traces",  // 有效
      batch_size: 100,  // 有效
      max_export_batch_size: 512,
      export_timeout_ms: 30000,  // 有效
      max_queue_size: 2048
    },
    
    metrics: azimuth::MetricsConfig {
      enabled: true,
      exporter: "prometheus",
      endpoint: "http://prometheus:9090/metrics",
      port: 9090,  // 有效
      path: "/metrics",  // 有效
      collection_interval_ms: 10000,  // 有效
      default_histogram_boundaries: [10.0, 25.0, 50.0, 100.0, 250.0, 500.0, 1000.0]  // 有效
    },
    
    logging: azimuth::LoggingConfig {
      enabled: true,
      level: "invalid-level",  // 无效
      format: "json",  // 有效
      output: "stdout",  // 有效
      include_trace_context: true,
      include_span_context: true,
      include_resource_attributes: true
    },
    
    resource: azimuth::ResourceConfig {
      attributes: [
        ("service.instance.id", "instance-123"),
        ("host.name", "server-01")
      ]
    }
  }
  
  // 验证部分无效配置
  let partial_validation_result = azimuth::validate_config(partially_invalid_config)
  match partial_validation_result {
    azimuth::ValidationResult::Valid => assert_true(false, "Config should be invalid")
    azimuth::ValidationResult::Invalid(errors) => {
      assert_eq(errors.length(), 1)  // 只有一个错误
      assert_true(errors[0].contains("level"))
    }
  }
}
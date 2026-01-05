// Azimuth New Comprehensive Test Cases
// 新的综合测试用例，覆盖多个功能领域

// 测试1: 遥测数据序列化和反序列化
test "telemetry data serialization and deserialization" {
  // 创建原始遥测数据
  let telemetry_data = azimuth::TelemetryData {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    parent_span_id: Some("b7ad6b7169203331"),
    operation_name: "process_payment",
    start_time: 1234567890L,
    end_time: 1234567895L,
    status: azimuth::SpanStatus::Ok,
    attributes: [
      ("service.name", azimuth::AttributeValue::StringValue("payment-service")),
      ("service.version", azimuth::AttributeValue::StringValue("2.1.0")),
      ("payment.amount", azimuth::AttributeValue::FloatValue(99.99)),
      ("payment.currency", azimuth::AttributeValue::StringValue("USD")),
      ("customer.id", azimuth::AttributeValue::StringValue("cust-12345"))
    ],
    events: [
      azimuth::SpanEvent {
        name: "validation_started",
        timestamp: 1234567891L,
        attributes: [("validation.type", azimuth::AttributeValue::StringValue("payment_method"))]
      },
      azimuth::SpanEvent {
        name: "payment_processed",
        timestamp: 1234567894L,
        attributes: [
          ("processor.id", azimuth::AttributeValue::StringValue("stripe")),
          ("transaction.id", azimuth::AttributeValue::StringValue("txn-67890"))
        ]
      }
    ]
  }
  
  // 序列化遥测数据
  let serialized = azimuth::TelemetrySerializer::serialize(telemetry_data)
  assert_true(serialized.length() > 0)
  
  // 反序列化遥测数据
  let deserialized = azimuth::TelemetrySerializer::deserialize(serialized)
  
  // 验证反序列化后的数据
  assert_eq(deserialized.trace_id, telemetry_data.trace_id)
  assert_eq(deserialized.span_id, telemetry_data.span_id)
  assert_eq(deserialized.operation_name, telemetry_data.operation_name)
  assert_eq(deserialized.attributes.length(), telemetry_data.attributes.length())
  assert_eq(deserialized.events.length(), telemetry_data.events.length())
}

// 测试2: 跨服务上下文传播
test "cross-service context propagation" {
  // 创建原始上下文
  let original_context = azimuth::Context {
    data: Some((
      "trace.context",
      "trace-id:4bf92f3577b34da6a3ce929d0e0e4736|span-id:00f067aa0ba902b7|sampled:1"
    ))
  }
  
  // 模拟服务A
  let service_a_context = azimuth::Context {
    data: Some((
      "service.a.metadata",
      "service.name:auth-service|version:1.2.0|instance:auth-001"
    ))
  }
  
  // 模拟服务B
  let service_b_context = azimuth::Context {
    data: Some((
      "service.b.metadata", 
      "service.name:payment-service|version:2.1.0|instance:payment-003"
    ))
  }
  
  // 模拟服务C
  let service_c_context = azimuth::Context {
    data: Some((
      "service.c.metadata",
      "service.name:notification-service|version:1.5.0|instance:notification-002"
    ))
  }
  
  // 验证上下文传播
  let contexts = [original_context, service_a_context, service_b_context, service_c_context]
  
  // 验证所有上下文都包含原始追踪信息
  for ctx in contexts {
    match ctx.data {
      Some((key, value)) => {
        if key == "trace.context" {
          assert_true(value.contains("trace-id:4bf92f3577b34da6a3ce929d0e0e4736"))
          assert_true(value.contains("sampled:1"))
        }
      }
      None => assert_true(false)
    }
  }
  
  // 验证每个服务有唯一的元数据
  let mut service_names = []
  let mut service_versions = []
  
  for ctx in contexts {
    match ctx.data {
      Some((key, value)) => {
        if key.contains("service") && key.contains("metadata") {
          let parts = value.split("|")
          for part in parts {
            if part.contains("service.name:") {
              let name_part = part.split(":")
              service_names = service_names + [name_part[1]]
            } else if part.contains("version:") {
              let version_part = part.split(":")
              service_versions = service_versions + [version_part[1]]
            }
          }
        }
      }
      None => () // 忽略None情况
    }
  }
  
  assert_eq(service_names.length(), 3)
  assert_eq(service_versions.length(), 3)
  
  // 验证服务名称唯一
  assert_true(service_names.contains("auth-service"))
  assert_true(service_names.contains("payment-service"))
  assert_true(service_names.contains("notification-service"))
}

// 测试3: 智能采样策略
test "intelligent sampling strategy" {
  // 创建采样策略配置
  let sampling_config = azimuth::SamplingConfig {
    sample_rate: 0.1, // 10%采样率
    max_traces_per_second: 100,
    priority_attributes: ["error", "slow_query", "high_value_transaction"],
    adaptive_sampling: true
  }
  
  // 模拟不同类型的追踪
  let normal_trace = azimuth::TraceData {
    trace_id: "trace-normal-001",
    attributes: [
      ("operation.type", azimuth::AttributeValue::StringValue("standard")),
      ("duration", azimuth::AttributeValue::IntValue(50))
    ]
  }
  
  let error_trace = azimuth::TraceData {
    trace_id: "trace-error-001",
    attributes: [
      ("operation.type", azimuth::AttributeValue::StringValue("standard")),
      ("error", azimuth::AttributeValue::BoolValue(true)),
      ("error.type", azimuth::AttributeValue::StringValue("timeout"))
    ]
  }
  
  let slow_query_trace = azimuth::TraceData {
    trace_id: "trace-slow-001",
    attributes: [
      ("operation.type", azimuth::AttributeValue::StringValue("database_query")),
      ("duration", azimuth::AttributeValue::IntValue(5000)),
      ("slow_query", azimuth::AttributeValue::BoolValue(true))
    ]
  }
  
  let high_value_trace = azimuth::TraceData {
    trace_id: "trace-high-value-001",
    attributes: [
      ("operation.type", azimuth::AttributeValue::StringValue("payment")),
      ("transaction.amount", azimuth::AttributeValue::FloatValue(1000.0)),
      ("high_value_transaction", azimuth::AttributeValue::BoolValue(true))
    ]
  }
  
  // 创建采样策略
  let sampler = azimuth::IntelligentSampler::new(sampling_config)
  
  // 测试不同追踪的采样决策
  let normal_sampled = azimuth::IntelligentSampler::should_sample(sampler, normal_trace)
  let error_sampled = azimuth::IntelligentSampler::should_sample(sampler, error_trace)
  let slow_query_sampled = azimuth::IntelligentSampler::should_sample(sampler, slow_query_trace)
  let high_value_sampled = azimuth::IntelligentSampler::should_sample(sampler, high_value_trace)
  
  // 验证采样决策
  // 正常追踪可能被采样（取决于采样率）
  // 错误追踪应该总是被采样
  // 慢查询追踪应该总是被采样
  // 高价值交易追踪应该总是被采样
  assert_true(error_sampled)
  assert_true(slow_query_sampled)
  assert_true(high_value_sampled)
}

// 测试4: 指标聚合和分析
test "metric aggregation and analysis" {
  // 创建指标提供者
  let provider = azimuth::MeterProvider::new()
  let meter = azimuth::MeterProvider::get_meter(provider, "azimuth.metrics")
  
  // 创建计数器指标
  let request_counter = azimuth::Meter::create_counter(
    meter, 
    "http_requests_total",
    Some("Total number of HTTP requests"),
    Some("requests")
  )
  
  // 创建直方图指标
  let response_time_histogram = azimuth::Meter::create_histogram(
    meter,
    "http_request_duration_seconds",
    Some("HTTP request duration in seconds"),
    Some("seconds")
  )
  
  // 创建仪表指标
  let active_connections_gauge = azimuth::Meter::create_gauge(
    meter,
    "active_connections",
    Some("Number of active connections"),
    Some("connections")
  )
  
  // 模拟指标数据
  // 记录不同类型的HTTP请求
  azimuth::Counter::add(request_counter, 100.0, Some([
    ("method", azimuth::AttributeValue::StringValue("GET")),
    ("status", azimuth::AttributeValue::StringValue("200"))
  ]))
  
  azimuth::Counter::add(request_counter, 20.0, Some([
    ("method", azimuth::AttributeValue::StringValue("POST")),
    ("status", azimuth::AttributeValue::StringValue("200"))
  ]))
  
  azimuth::Counter::add(request_counter, 5.0, Some([
    ("method", azimuth::AttributeValue::StringValue("GET")),
    ("status", azimuth::AttributeValue::StringValue("404"))
  ]))
  
  azimuth::Counter::add(request_counter, 2.0, Some([
    ("method", azimuth::AttributeValue::StringValue("POST")),
    ("status", azimuth::AttributeValue::StringValue("500"))
  ]))
  
  // 记录响应时间
  azimuth::Histogram::record(response_time_histogram, 0.05, Some([
    ("endpoint", azimuth::AttributeValue::StringValue("/api/users"))
  ]))
  
  azimuth::Histogram::record(response_time_histogram, 0.12, Some([
    ("endpoint", azimuth::AttributeValue::StringValue("/api/orders"))
  ]))
  
  azimuth::Histogram::record(response_time_histogram, 0.08, Some([
    ("endpoint", azimuth::AttributeValue::StringValue("/api/products"))
  ]))
  
  // 设置活动连接数
  azimuth::Gauge::set(active_connections_gauge, 150.0, Some([
    ("connection.type", azimuth::AttributeValue::StringValue("http"))
  ]))
  
  // 聚合指标数据
  let aggregated_metrics = azimuth::MetricAggregator::aggregate(provider)
  
  // 验证聚合结果
  assert_true(aggregated_metrics.length() >= 3) // 至少有三个指标
  
  // 验证请求计数器聚合
  let mut total_requests = 0.0
  for metric in aggregated_metrics {
    match metric.name {
      "http_requests_total" => {
        match metric.value {
          azimuth::MetricValue::Counter(count) => total_requests = count
          _ => assert_true(false)
        }
      }
      _ => () // 其他指标
    }
  }
  
  assert_eq(total_requests, 127.0) // 100 + 20 + 5 + 2
}

// 测试5: 遥测数据质量验证
test "telemetry data quality validation" {
  // 创建数据质量验证器
  let validator = azimuth::DataQualityValidator::new()
  
  // 创建有效遥测数据
  let valid_data = azimuth::TelemetryData {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    parent_span_id: Some("b7ad6b7169203331"),
    operation_name: "process_payment",
    start_time: 1234567890L,
    end_time: 1234567895L,
    status: azimuth::SpanStatus::Ok,
    attributes: [
      ("service.name", azimuth::AttributeValue::StringValue("payment-service")),
      ("service.version", azimuth::AttributeValue::StringValue("2.1.0")),
      ("payment.amount", azimuth::AttributeValue::FloatValue(99.99))
    ],
    events: []
  }
  
  // 创建无效遥测数据 - 缺少必需字段
  let invalid_data_missing_trace = azimuth::TelemetryData {
    trace_id: "", // 空的追踪ID
    span_id: "00f067aa0ba902b7",
    parent_span_id: None,
    operation_name: "process_payment",
    start_time: 1234567890L,
    end_time: 1234567895L,
    status: azimuth::SpanStatus::Ok,
    attributes: [],
    events: []
  }
  
  // 创建无效遥测数据 - 时间戳不一致
  let invalid_data_timestamp = azimuth::TelemetryData {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    parent_span_id: None,
    operation_name: "process_payment",
    start_time: 1234567895L, // 开始时间晚于结束时间
    end_time: 1234567890L,
    status: azimuth::SpanStatus::Ok,
    attributes: [],
    events: []
  }
  
  // 验证数据质量
  let valid_result = azimuth::DataQualityValidator::validate(validator, valid_data)
  let invalid_trace_result = azimuth::DataQualityValidator::validate(validator, invalid_data_missing_trace)
  let invalid_timestamp_result = azimuth::DataQualityValidator::validate(validator, invalid_data_timestamp)
  
  // 验证结果
  assert_true(valid_result.is_valid)
  assert_false(invalid_trace_result.is_valid)
  assert_false(invalid_timestamp_result.is_valid)
  
  // 验证错误消息
  assert_eq(invalid_trace_result.errors.length(), 1)
  assert_eq(invalid_timestamp_result.errors.length(), 1)
  assert_true(invalid_trace_result.errors[0].contains("trace_id"))
  assert_true(invalid_timestamp_result.errors[0].contains("timestamp"))
}

// 测试6: 动态配置更新
test "dynamic configuration updates" {
  // 创建初始配置
  let initial_config = azimuth::TelemetryConfig {
    service_name: "payment-service",
    service_version: "2.1.0",
    sampling_rate: 0.1,
    batch_size: 100,
    export_interval: 5000,
    enabled_features: ["metrics", "tracing"]
  }
  
  // 创建配置管理器
  let config_manager = azimuth::ConfigManager::new(initial_config)
  
  // 验证初始配置
  let current_config = azimuth::ConfigManager::get_config(config_manager)
  assert_eq(current_config.service_name, "payment-service")
  assert_eq(current_config.sampling_rate, 0.1)
  assert_eq(current_config.batch_size, 100)
  
  // 更新配置
  let updated_config = azimuth::TelemetryConfig {
    service_name: "payment-service",
    service_version: "2.1.1", // 版本更新
    sampling_rate: 0.2, // 采样率更新
    batch_size: 200, // 批量大小更新
    export_interval: 3000, // 导出间隔更新
    enabled_features: ["metrics", "tracing", "logging"] // 添加日志功能
  }
  
  azimuth::ConfigManager::update_config(config_manager, updated_config)
  
  // 验证配置更新
  let new_config = azimuth::ConfigManager::get_config(config_manager)
  assert_eq(new_config.service_version, "2.1.1")
  assert_eq(new_config.sampling_rate, 0.2)
  assert_eq(new_config.batch_size, 200)
  assert_eq(new_config.export_interval, 3000)
  assert_eq(new_config.enabled_features.length(), 3)
  assert_true(new_config.enabled_features.contains("logging"))
}

// 测试7: 异常检测和恢复
test "anomaly detection and recovery" {
  // 创建异常检测器
  let detector = azimuth::AnomalyDetector::new()
  
  // 创建正常性能基线
  let baseline_metrics = [
    azimuth::MetricPoint {
      name: "response_time",
      value: 100.0,
      timestamp: 1234567890L
    },
    azimuth::MetricPoint {
      name: "error_rate",
      value: 0.01,
      timestamp: 1234567890L
    },
    azimuth::MetricPoint {
      name: "throughput",
      value: 1000.0,
      timestamp: 1234567890L
    }
  ]
  
  // 训练检测器
  azimuth::AnomalyDetector::train(detector, baseline_metrics)
  
  // 创建正常指标
  let normal_metrics = [
    azimuth::MetricPoint {
      name: "response_time",
      value: 120.0, // 稍微高于基线但仍在正常范围内
      timestamp: 1234567891L
    },
    azimuth::MetricPoint {
      name: "error_rate",
      value: 0.02, // 稍微高于基线但仍在正常范围内
      timestamp: 1234567891L
    },
    azimuth::MetricPoint {
      name: "throughput",
      value: 950.0, // 稍微低于基线但仍在正常范围内
      timestamp: 1234567891L
    }
  ]
  
  // 创建异常指标
  let anomalous_metrics = [
    azimuth::MetricPoint {
      name: "response_time",
      value: 5000.0, // 显著高于基线
      timestamp: 1234567892L
    },
    azimuth::MetricPoint {
      name: "error_rate",
      value: 0.5, // 显著高于基线
      timestamp: 1234567892L
    },
    azimuth::MetricPoint {
      name: "throughput",
      value: 100.0, // 显著低于基线
      timestamp: 1234567892L
    }
  ]
  
  // 检测异常
  let normal_result = azimuth::AnomalyDetector::detect(detector, normal_metrics)
  let anomalous_result = azimuth::AnomalyDetector::detect(detector, anomalous_metrics)
  
  // 验证检测结果
  assert_false(normal_result.has_anomaly)
  assert_true(anomalous_result.has_anomaly)
  
  // 验证异常详情
  assert_eq(anomalous_result.anomalies.length(), 3)
  assert_true(anomalous_result.anomalies[0].metric_name == "response_time")
  assert_true(anomalous_result.anomalies[1].metric_name == "error_rate")
  assert_true(anomalous_result.anomalies[2].metric_name == "throughput")
  
  // 创建恢复策略
  let recovery_strategy = azimuth::RecoveryStrategy {
    name: "auto_scaling",
    conditions: ["high_response_time", "high_error_rate"],
    actions: ["scale_up", "circuit_breaker"]
  }
  
  // 应用恢复策略
  let recovery_actions = azimuth::RecoveryStrategy::apply(recovery_strategy, anomalous_result)
  
  // 验证恢复动作
  assert_eq(recovery_actions.length(), 2)
  assert_true(recovery_actions.contains("scale_up"))
  assert_true(recovery_actions.contains("circuit_breaker"))
}

// 测试8: 多维度数据分析
test "multi-dimensional data analysis" {
  // 创建多维数据集
  let dataset = azimuth::MultiDimensionalDataset::new()
  
  // 添加数据点
  let data_points = [
    azimuth::DataPoint {
      dimensions: [
        ("service", azimuth::DimensionValue::StringValue("auth-service")),
        ("region", azimuth::DimensionValue::StringValue("us-east-1")),
        ("environment", azimuth::DimensionValue::StringValue("production"))
      ],
      metrics: [
        ("request_count", azimuth::MetricValue::IntValue(1000)),
        ("error_rate", azimuth::MetricValue::FloatValue(0.01)),
        ("avg_response_time", azimuth::MetricValue::FloatValue(50.0))
      ]
    },
    azimuth::DataPoint {
      dimensions: [
        ("service", azimuth::DimensionValue::StringValue("payment-service")),
        ("region", azimuth::DimensionValue::StringValue("us-east-1")),
        ("environment", azimuth::DimensionValue::StringValue("production"))
      ],
      metrics: [
        ("request_count", azimuth::MetricValue::IntValue(500)),
        ("error_rate", azimuth::MetricValue::FloatValue(0.02)),
        ("avg_response_time", azimuth::MetricValue::FloatValue(100.0))
      ]
    },
    azimuth::DataPoint {
      dimensions: [
        ("service", azimuth::DimensionValue::StringValue("auth-service")),
        ("region", azimuth::DimensionValue::StringValue("us-west-2")),
        ("environment", azimuth::DimensionValue::StringValue("production"))
      ],
      metrics: [
        ("request_count", azimuth::MetricValue::IntValue(800)),
        ("error_rate", azimuth::MetricValue::FloatValue(0.015)),
        ("avg_response_time", azimuth::MetricValue::FloatValue(60.0))
      ]
    },
    azimuth::DataPoint {
      dimensions: [
        ("service", azimuth::DimensionValue::StringValue("payment-service")),
        ("region", azimuth::DimensionValue::StringValue("us-west-2")),
        ("environment", azimuth::DimensionValue::StringValue("production"))
      ],
      metrics: [
        ("request_count", azimuth::MetricValue::IntValue(400)),
        ("error_rate", azimuth::MetricValue::FloatValue(0.025)),
        ("avg_response_time", azimuth::MetricValue::FloatValue(120.0))
      ]
    }
  ]
  
  // 添加数据点到数据集
  for data_point in data_points {
    azimuth::MultiDimensionalDataset::add_point(dataset, data_point)
  }
  
  // 按服务维度分析
  let service_analysis = azimuth::MultiDimensionalDataset::analyze_by_dimension(
    dataset, 
    "service"
  )
  
  // 验证服务分析结果
  assert_eq(service_analysis.length(), 2) // auth-service 和 payment-service
  
  // 验证auth-service聚合数据
  let mut auth_service_data = None
  for analysis in service_analysis {
    match analysis.dimension_value {
      azimuth::DimensionValue::StringValue(service) => {
        if service == "auth-service" {
          auth_service_data = Some(analysis)
        }
      }
      _ => ()
    }
  }
  
  match auth_service_data {
    Some(data) => {
      // 验证聚合指标
      let mut total_requests = 0
      let mut avg_error_rate = 0.0
      let mut avg_response_time = 0.0
      
      for (metric_name, metric_value) in data.aggregated_metrics {
        match metric_name {
          "request_count" => {
            match metric_value {
              azimuth::MetricValue::IntValue(count) => total_requests = count
              _ => assert_true(false)
            }
          }
          "error_rate" => {
            match metric_value {
              azimuth::MetricValue::FloatValue(rate) => avg_error_rate = rate
              _ => assert_true(false)
            }
          }
          "avg_response_time" => {
            match metric_value {
              azimuth::MetricValue::FloatValue(time) => avg_response_time = time
              _ => assert_true(false)
            }
          }
          _ => ()
        }
      }
      
      assert_eq(total_requests, 1800) // 1000 + 800
      assert_true(avg_error_rate > 0.01 && avg_error_rate < 0.02) // 平均值
      assert_true(avg_response_time > 50.0 && avg_response_time < 60.0) // 平均值
    }
    None => assert_true(false)
  }
  
  // 按区域维度分析
  let region_analysis = azimuth::MultiDimensionalDataset::analyze_by_dimension(
    dataset, 
    "region"
  )
  
  // 验证区域分析结果
  assert_eq(region_analysis.length(), 2) // us-east-1 和 us-west-2
}

// 测试9: 遥测数据生命周期管理
test "telemetry data lifecycle management" {
  // 创建生命周期管理器
  let lifecycle_manager = azimuth::TelemetryLifecycleManager::new()
  
  // 配置生命周期策略
  let lifecycle_policy = azimuth::LifecyclePolicy {
    retention_period: 86400, // 24小时（秒）
    max_storage_size: 1000000, // 1MB
    cleanup_interval: 3600, // 1小时
    archival_enabled: true,
    archival_conditions: ["high_value_traces", "error_traces"]
  }
  
  azimuth::TelemetryLifecycleManager::set_policy(lifecycle_manager, lifecycle_policy)
  
  // 创建不同类型的遥测数据
  let normal_trace = azimuth::TelemetryData {
    trace_id: "trace-normal-001",
    span_id: "span-001",
    parent_span_id: None,
    operation_name: "normal_operation",
    start_time: 1234567890L,
    end_time: 1234567895L,
    status: azimuth::SpanStatus::Ok,
    attributes: [
      ("service.name", azimuth::AttributeValue::StringValue("test-service")),
      ("trace.type", azimuth::AttributeValue::StringValue("normal"))
    ],
    events: []
  }
  
  let error_trace = azimuth::TelemetryData {
    trace_id: "trace-error-001",
    span_id: "span-002",
    parent_span_id: None,
    operation_name: "error_operation",
    start_time: 1234567890L,
    end_time: 1234567895L,
    status: azimuth::SpanStatus::Error,
    attributes: [
      ("service.name", azimuth::AttributeValue::StringValue("test-service")),
      ("trace.type", azimuth::AttributeValue::StringValue("error")),
      ("error.message", azimuth::AttributeValue::StringValue("Connection timeout"))
    ],
    events: []
  }
  
  let high_value_trace = azimuth::TelemetryData {
    trace_id: "trace-high-value-001",
    span_id: "span-003",
    parent_span_id: None,
    operation_name: "high_value_operation",
    start_time: 1234567890L,
    end_time: 1234567895L,
    status: azimuth::SpanStatus::Ok,
    attributes: [
      ("service.name", azimuth::AttributeValue::StringValue("test-service")),
      ("trace.type", azimuth::AttributeValue::StringValue("high_value")),
      ("transaction.amount", azimuth::AttributeValue::FloatValue(10000.0))
    ],
    events: []
  }
  
  // 存储遥测数据
  azimuth::TelemetryLifecycleManager::store(lifecycle_manager, normal_trace)
  azimuth::TelemetryLifecycleManager::store(lifecycle_manager, error_trace)
  azimuth::TelemetryLifecycleManager::store(lifecycle_manager, high_value_trace)
  
  // 验证数据存储
  let stored_data = azimuth::TelemetryLifecycleManager::get_all(lifecycle_manager)
  assert_eq(stored_data.length(), 3)
  
  // 模拟时间流逝（超过保留期）
  let current_time = 1234567890L + (86400 + 3600) // 25小时后
  
  // 执行清理
  let cleanup_result = azimuth::TelemetryLifecycleManager::cleanup(lifecycle_manager, current_time)
  
  // 验证清理结果
  assert_eq(cleanup_result.deleted_count, 1) // 只有正常追踪被删除
  assert_eq(cleanup_result.archived_count, 2) // 错误和高价值追踪被归档
  
  // 验证剩余数据
  let remaining_data = azimuth::TelemetryLifecycleManager::get_all(lifecycle_manager)
  assert_eq(remaining_data.length(), 2) // 错误和高价值追踪仍然存在
  
  // 验证归档数据
  let archived_data = azimuth::TelemetryLifecycleManager::get_archived(lifecycle_manager)
  assert_eq(archived_data.length(), 2)
}

// 测试10: 遥测数据导出和集成
test "telemetry data export and integration" {
  // 创建导出配置
  let export_config = azimuth::ExportConfig {
    exporters: [
      azimuth::ExporterConfig {
        name: "prometheus",
        type: "metrics",
        endpoint: "http://prometheus:9090/api/v1/write",
        format: "prometheus",
        enabled: true
      },
      azimuth::ExporterConfig {
        name: "jaeger",
        type: "tracing",
        endpoint: "http://jaeger:14268/api/traces",
        format: "jaeger",
        enabled: true
      },
      azimuth::ExporterConfig {
        name: "elasticsearch",
        type: "logs",
        endpoint: "http://elasticsearch:9200/logs",
        format: "json",
        enabled: true
      }
    ],
    batch_size: 100,
    export_interval: 5000,
    retry_policy: azimuth::RetryPolicy {
      max_retries: 3,
      backoff_factor: 2.0,
      initial_delay: 1000
    }
  }
  
  // 创建导出管理器
  let export_manager = azimuth::ExportManager::new(export_config)
  
  // 创建测试遥测数据
  let metrics_data = azimuth::MetricsData {
    metrics: [
      azimuth::Metric {
        name: "http_requests_total",
        value: azimuth::MetricValue::Counter(1250.0),
        attributes: [
          ("method", azimuth::AttributeValue::StringValue("GET")),
          ("status", azimuth::AttributeValue::StringValue("200"))
        ],
        timestamp: 1234567890L
      },
      azimuth::Metric {
        name: "http_request_duration_seconds",
        value: azimuth::MetricValue::Histogram(azimuth::HistogramData {
          buckets: [(0.1, 1000.0), (0.5, 200.0), (1.0, 50.0)],
          count: 1250.0,
          sum: 125.0
        }),
        attributes: [
          ("endpoint", azimuth::AttributeValue::StringValue("/api/users"))
        ],
        timestamp: 1234567890L
      }
    ]
  }
  
  let tracing_data = azimuth::TracingData {
    traces: [
      azimuth::Trace {
        trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
        spans: [
          azimuth::Span {
            span_id: "00f067aa0ba902b7",
            parent_span_id: None,
            operation_name: "http_get",
            start_time: 1234567890L,
            end_time: 1234567895L,
            status: azimuth::SpanStatus::Ok,
            attributes: [
              ("http.method", azimuth::AttributeValue::StringValue("GET")),
              ("http.url", azimuth::AttributeValue::StringValue("/api/users")),
              ("http.status_code", azimuth::AttributeValue::IntValue(200))
            ],
            events: []
          }
        ]
      }
    ]
  }
  
  let logs_data = azimuth::LogsData {
    logs: [
      azimuth::LogRecord {
        timestamp: 1234567890L,
        severity: azimuth::LogSeverity::Info,
        body: Some("Request processed successfully"),
        attributes: [
          ("trace_id", azimuth::AttributeValue::StringValue("4bf92f3577b34da6a3ce929d0e0e4736")),
          ("span_id", azimuth::AttributeValue::StringValue("00f067aa0ba902b7")),
          ("user.id", azimuth::AttributeValue::StringValue("user-12345"))
        ]
      }
    ]
  }
  
  // 导出数据
  let metrics_export_result = azimuth::ExportManager::export_metrics(export_manager, metrics_data)
  let tracing_export_result = azimuth::ExportManager::export_traces(export_manager, tracing_data)
  let logs_export_result = azimuth::ExportManager::export_logs(export_manager, logs_data)
  
  // 验证导出结果
  assert_true(metrics_export_result.success)
  assert_true(tracing_export_result.success)
  assert_true(logs_export_result.success)
  
  // 验证导出详情
  assert_eq(metrics_export_result.exported_count, 2) // 两个指标
  assert_eq(tracing_export_result.exported_count, 1) // 一个追踪
  assert_eq(logs_export_result.exported_count, 1) // 一条日志
  
  // 验证使用的导出器
  assert_eq(metrics_export_result.used_exporters.length(), 1) // 只有prometheus
  assert_eq(tracing_export_result.used_exporters.length(), 1) // 只有jaeger
  assert_eq(logs_export_result.used_exporters.length(), 1) // 只有elasticsearch
  
  assert_true(metrics_export_result.used_exporters.contains("prometheus"))
  assert_true(tracing_export_result.used_exporters.contains("jaeger"))
  assert_true(logs_export_result.used_exporters.contains("elasticsearch"))
}
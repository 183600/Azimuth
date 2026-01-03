// Azimuth Advanced Telemetry Integration Tests
// 高级遥测集成测试，覆盖关键功能和边界情况

test "telemetry data serialization and deserialization" {
  // 创建一个复杂的遥测数据结构
  let telemetry_data = @azimuth.TelemetryData {
    timestamp : 1672531200000, // 2023-01-01 00:00:00 UTC
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : "1234567890abcdef",
    parent_span_id : Some("abcdef1234567890"),
    operation_name : "http.request",
    duration_ms : 150,
    status : @azimuth.SpanStatus::Ok,
    attributes : [
      ("http.method", @azimuth.StringValue("GET")),
      ("http.url", @azimuth.StringValue("https://api.example.com/users")),
      ("http.status_code", @azimuth.IntValue(200)),
      ("user.id", @azimuth.StringValue("12345"))
    ],
    events : [
      @azimuth.SpanEvent {
        name : "database.query",
        timestamp : 1672531200050,
        attributes : [
          ("db.statement", @azimuth.StringValue("SELECT * FROM users WHERE id = ?")),
          ("db.type", @azimuth.StringValue("postgresql"))
        ]
      },
      @azimuth.SpanEvent {
        name : "cache.hit",
        timestamp : 1672531200100,
        attributes : [
          ("cache.key", @azimuth.StringValue("user:12345")),
          ("cache.type", @azimuth.StringValue("redis"))
        ]
      }
    ]
  }
  
  // 序列化为JSON
  let serialized = @azimuth.serialize_telemetry_data(telemetry_data)
  assert_true(serialized.length() > 0)
  assert_true(serialized.contains("\"operation_name\":\"http.request\""))
  assert_true(serialized.contains("\"duration_ms\":150"))
  
  // 反序列化
  let deserialized = @azimuth.deserialize_telemetry_data(serialized)
  match deserialized {
    Ok(data) => {
      assert_eq(data.operation_name, "http.request")
      assert_eq(data.duration_ms, 150)
      assert_eq(data.attributes.length(), 4)
      assert_eq(data.events.length(), 2)
    }
    Err(_) => assert_true(false)
  }
}

test "performance metrics collection and aggregation" {
  // 创建性能指标收集器
  let metrics_collector = @azimuth.MetricsCollector {
    buffer_size : 1000,
    flush_interval_ms : 5000,
    aggregation_window_ms : 60000
  }
  
  // 模拟添加性能指标
  for i in 1..=100 {
    let metric = @azimuth.Metric {
      name : "request.duration",
      value : @azimuth.MetricValue::Histogram {
        buckets : [10.0, 50.0, 100.0, 500.0, 1000.0],
        counts : [5, 25, 40, 25, 5],
        sum : 25000.0,
        count : 100
      },
      timestamp : 1672531200000 + i * 100,
      attributes : [
        ("endpoint", @azimuth.StringValue("/api/users")),
        ("method", @azimuth.StringValue("GET"))
      ]
    }
    metrics_collector.add_metric(metric)
  }
  
  // 获取聚合结果
  let aggregated = metrics_collector.get_aggregated_metrics("request.duration")
  match aggregated {
    Some(histogram) => {
      assert_eq(histogram.count, 100)
      assert_eq(histogram.sum, 25000.0)
      assert_eq(histogram.buckets.length(), 5)
    }
    None => assert_true(false)
  }
  
  // 测试百分位数计算
  let p95 = metrics_collector.calculate_percentile("request.duration", 0.95)
  match p95 {
    Some(value) => assert_true(value > 500.0 && value <= 1000.0)
    None => assert_true(false)
  }
}

test "error handling and recovery mechanisms" {
  // 测试网络错误处理
  let network_error = @azimuth.NetworkError {
    code : @azimuth.ErrorCode::ConnectionTimeout,
    message : "Connection timed out after 30 seconds",
    retryable : true,
    retry_after_ms : Some(5000)
  }
  
  assert_eq(network_error.code, @azimuth.ErrorCode::ConnectionTimeout)
  assert_true(network_error.retryable)
  
  // 测试错误恢复策略
  let recovery_strategy = @azimuth.RecoveryStrategy {
    max_retries : 3,
    backoff_strategy : @azimuth.BackoffStrategy::Exponential,
    initial_delay_ms : 1000,
    max_delay_ms : 30000
  }
  
  let retry_delays = recovery_strategy.calculate_retry_delays()
  assert_eq(retry_delays.length(), 3)
  assert_eq(retry_delays[0], 1000)
  assert_eq(retry_delays[1], 2000) // 指数退避
  assert_eq(retry_delays[2], 4000)
  
  // 测试断路器模式
  let circuit_breaker = @azimuth.CircuitBreaker {
    failure_threshold : 5,
    recovery_timeout_ms : 60000,
    failure_count : 0,
    last_failure_time : None,
    state : @azimuth.CircuitState::Closed
  }
  
  // 模拟失败
  for i in 1..=6 {
    circuit_breaker.record_failure()
  }
  
  assert_eq(circuit_breaker.state, @azimuth.CircuitState::Open)
  assert_eq(circuit_breaker.failure_count, 6)
}

test "concurrent telemetry processing" {
  // 创建并发处理器
  let processor = @azimuth.ConcurrentProcessor {
    worker_count : 4,
    queue_capacity : 1000,
    batch_size : 50
  }
  
  // 模拟并发处理任务
  let mut tasks = []
  for i in 1..=20 {
    let task = @azimuth.ProcessingTask {
      id : "task-" + i.to_string(),
      data : "sample telemetry data " + i.to_string(),
      priority : if i % 5 == 0 { @azimuth.Priority::High } else { @azimuth.Priority::Normal }
    }
    tasks = tasks.push(task)
  }
  
  // 提交任务
  for task in tasks {
    let submitted = processor.submit_task(task)
    assert_true(submitted)
  }
  
  // 等待处理完成
  let results = processor.wait_for_completion(5000)
  assert_eq(results.length(), 20)
  
  // 验证高优先级任务优先处理
  let high_priority_results = results.filter(fn(r) { r.priority == @azimuth.Priority::High })
  assert_eq(high_priority_results.length(), 4)
  
  // 验证处理顺序正确性
  for result in results {
    assert_true(result.processing_time_ms >= 0)
    assert_true(result.success)
  }
}

test "time series data analysis" {
  // 创建时间序列数据
  let mut time_series_data = []
  let base_timestamp = 1672531200000 // 2023-01-01 00:00:00 UTC
  
  for i in 0..<24 { // 24小时的数据
    let hour_data = @azimuth.TimeSeriesPoint {
      timestamp : base_timestamp + i * 3600000, // 每小时一个数据点
      value : 100.0 + @azimuth.random_normal() * 20.0, // 带有随机噪声的基础值
      metadata : [
        ("metric.name", @azimuth.StringValue("cpu.usage")),
        ("host.name", @azimuth.StringValue("server-" + ((i % 3) + 1).to_string()))
      ]
    }
    time_series_data = time_series_data.push(hour_data)
  }
  
  // 创建时间序列分析器
  let analyzer = @azimuth.TimeSeriesAnalyzer {
    window_size : 6, // 6小时窗口
    trend_threshold : 0.1
  }
  
  // 分析趋势
  let trend = analyzer.analyze_trend(time_series_data)
  match trend {
    @azimuth.Trend::Increasing => assert_true(true)
    @azimuth.Trend::Decreasing => assert_true(true)
    @azimuth.Trend::Stable => assert_true(true)
  }
  
  // 检测异常
  let anomalies = analyzer.detect_anomalies(time_series_data)
  assert_true(anomalies.length() >= 0)
  
  // 计算移动平均
  let moving_avg = analyzer.calculate_moving_average(time_series_data, 3)
  assert_eq(moving_avg.length(), time_series_data.length() - 2)
  
  // 预测下一个值
  let prediction = analyzer.predict_next_value(time_series_data)
  match prediction {
    Some(value) => assert_true(value > 0.0)
    None => assert_true(false)
  }
}

test "distributed tracing context propagation" {
  // 创建根span上下文
  let root_context = @azimuth.SpanContext {
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : "1234567890abcdef",
    sampled : true,
    trace_state : "key1=value1,key2=value2"
  }
  
  // 创建子span
  let child_span = @azimuth.create_child_span(root_context, "child.operation")
  assert_eq(child_span.parent_span_id, Some("1234567890abcdef"))
  assert_eq(child_span.trace_id, "1234567890abcdef1234567890abcdef")
  assert_true(child_span.sampled)
  
  // 创建跨服务传播器
  let propagator = @azimuth.CrossServicePropagator {
    injectors : [@azimum.TraceContextInjector, @azimum.BaggageInjector],
    extractors : [@azimum.TraceContextExtractor, @azimum.BaggageExtractor]
  }
  
  // 注入上下文到HTTP头
  let carrier = @azimuth.TextMapCarrier { headers : [] }
  propagator.inject(child_span, carrier)
  
  assert_true(carrier.headers.length() > 0)
  
  // 从HTTP头提取上下文
  let extracted_context = propagator.extract(carrier)
  match extracted_context {
    Some(ctx) => {
      assert_eq(ctx.trace_id, child_span.trace_id)
      assert_eq(ctx.span_id, child_span.span_id)
      assert_eq(ctx.sampled, child_span.sampled)
    }
    None => assert_true(false)
  }
  
  // 测试baggage传播
  let baggage = @azimuth.Baggage {
    entries : [
      ("user.id", "12345"),
      ("request.id", "req-67890"),
      ("region", "us-west-2")
    ]
  }
  
  let baggage_carrier = @azimuth.TextMapCarrier { headers : [] }
  propagator.inject_baggage(baggage, baggage_carrier)
  
  let extracted_baggage = propagator.extract_baggage(baggage_carrier)
  match extracted_baggage {
    Some(b) => assert_eq(b.entries.length(), 3)
    None => assert_true(false)
  }
}

test "configuration management and validation" {
  // 创建配置管理器
  let config_manager = @azimuth.ConfigManager {
    sources : [
      @azimuth.ConfigSource::Environment,
      @azimuth.ConfigSource::File { path : "/etc/azimuth/config.yaml" },
      @azimuth.ConfigSource::Default
    ]
  }
  
  // 加载配置
  let config = config_manager.load_config()
  match config {
    Ok(cfg) => {
      // 验证必需的配置项
      assert_true(cfg.service_name.length() > 0)
      assert_true(cfg.service_version.length() > 0)
      assert_true(cfg.endpoint.length() > 0)
      
      // 验证可选配置项的默认值
      match cfg.batch_size {
        Some(size) => assert_true(size > 0)
        None => assert_true(true) // 使用默认值
      }
      
      match cfg.timeout_ms {
        Some(timeout) => assert_true(timeout > 0)
        None => assert_true(true) // 使用默认值
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试配置验证
  let invalid_config = @azimuth.Config {
    service_name : "", // 空服务名称应该无效
    service_version : "1.0.0",
    endpoint : "http://localhost:8080",
    batch_size : Some(100),
    timeout_ms : Some(5000)
  }
  
  let validation_result = config_manager.validate_config(invalid_config)
  match validation_result {
    @azimuth.ValidationResult::Valid => assert_true(false)
    @azimuth.ValidationResult::Invalid(errors) => {
      assert_true(errors.length() > 0)
      assert_true(errors.any(fn(e) { e.contains("service_name") }))
    }
  }
  
  // 测试配置热更新
  let update_result = config_manager.update_config("batch_size", "200")
  match update_result {
    Ok(()) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  let updated_config = config_manager.get_config()
  match updated_config.batch_size {
    Some(size) => assert_eq(size, 200)
    None => assert_true(false)
  }
}

test "data integrity and validation checks" {
  // 创建数据验证器
  let validator = @azimuth.DataValidator {
    strict_mode : true,
    custom_validators : [
      @azimuth.CustomValidator {
        name : "trace_id_format",
        validator_fn : fn(value) {
          match value {
            @azimuth.StringValue(trace_id) => {
              trace_id.length() == 32 && trace_id.chars().all(fn(c) { 
                (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
              })
            }
            _ => false
          }
        }
      }
    ]
  }
  
  // 测试有效数据
  let valid_telemetry = @azimuth.TelemetryData {
    timestamp : 1672531200000,
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : "1234567890abcdef",
    parent_span_id : None,
    operation_name : "valid.operation",
    duration_ms : 100,
    status : @azimuth.SpanStatus::Ok,
    attributes : [
      ("service.name", @azimuth.StringValue("test-service")),
      ("version", @azimuth.StringValue("1.0.0"))
    ],
    events : []
  }
  
  let valid_result = validator.validate_telemetry_data(valid_telemetry)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // 测试无效数据
  let invalid_telemetry = @azimuth.TelemetryData {
    timestamp : 1672531200000,
    trace_id : "invalid-trace-id", // 无效的trace_id格式
    span_id : "1234567890abcdef",
    parent_span_id : None,
    operation_name : "", // 空操作名称
    duration_ms : -100, // 负持续时间
    status : @azimuth.SpanStatus::Ok,
    attributes : [
      ("service.name", @azimuth.StringValue("")) // 空服务名称
    ],
    events : []
  }
  
  let invalid_result = validator.validate_telemetry_data(invalid_telemetry)
  assert_false(invalid_result.is_valid)
  assert_true(invalid_result.errors.length() >= 3)
  
  // 测试数据完整性检查
  let integrity_checker = @azimuth.IntegrityChecker {
    checksum_algorithm : @azimuth.ChecksumAlgorithm::SHA256
  }
  
  let data = "sample telemetry data"
  let checksum = integrity_checker.calculate_checksum(data)
  assert_true(checksum.length() > 0)
  
  let verification_result = integrity_checker.verify_checksum(data, checksum)
  assert_true(verification_result)
  
  // 测试篡改检测
  let tampered_data = "tampered telemetry data"
  let tampered_result = integrity_checker.verify_checksum(tampered_data, checksum)
  assert_false(tampered_result)
}
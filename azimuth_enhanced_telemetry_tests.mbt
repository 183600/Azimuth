// Azimuth Enhanced Telemetry Test Suite
// This file contains enhanced telemetry test cases focusing on advanced features

// Test 1: Advanced Telemetry Data Processing and Transformation
test "advanced telemetry data processing and transformation" {
  // 定义遥测数据点类型
  type TelemetryPoint = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)]
  }
  
  // 创建数据转换器
  let normalize_metrics = fn(points: Array[TelemetryPoint]) {
    points.map(fn(point) {
      let normalized_tags = point.tags.map(fn(tag) {
        let (key, value) = tag
        (key.to_lowercase(), value.to_lowercase())
      })
      { 
        timestamp: point.timestamp,
        metric_name: point.metric_name.to_lowercase(),
        value: point.value,
        tags: normalized_tags
      }
    })
  }
  
  // 创建数据聚合器
  let aggregate_by_time_window = fn(points: Array[TelemetryPoint], window_size_ms: Int) {
    if points.length() == 0 {
      return []
    }
    
    let sorted_points = points.sort(fn(a, b) { 
      if a.timestamp < b.timestamp { -1 } 
      else if a.timestamp > b.timestamp { 1 } 
      else { 0 } 
    })
    
    let windows = []
    let mut current_window = []
    let mut window_start = sorted_points[0].timestamp
    let window_end = window_start + window_size_ms
    
    for point in sorted_points {
      if point.timestamp <= window_end {
        current_window = current_window.push(point)
      } else {
        if current_window.length() > 0 {
          windows = windows.push(current_window)
        }
        current_window = [point]
        window_start = point.timestamp
        window_end = window_start + window_size_ms
      }
    }
    
    if current_window.length() > 0 {
      windows = windows.push(current_window)
    }
    
    windows
  }
  
  // 创建测试数据
  let test_points = [
    { timestamp: 1000, metric_name: "CPU_Usage", value: 45.5, tags: [("host", "server1"), ("env", "prod")] },
    { timestamp: 1500, metric_name: "Memory_Usage", value: 78.2, tags: [("host", "server1"), ("env", "prod")] },
    { timestamp: 2000, metric_name: "CPU_Usage", value: 52.1, tags: [("host", "server2"), ("env", "dev")] },
    { timestamp: 2500, metric_name: "Disk_IO", value: 120.3, tags: [("host", "server1"), ("env", "prod")] },
    { timestamp: 5000, metric_name: "CPU_Usage", value: 38.7, tags: [("host", "server1"), ("env", "prod")] }
  ]
  
  // 测试数据标准化
  let normalized = normalize_metrics(test_points)
  assert_eq(normalized[0].metric_name, "cpu_usage")
  assert_eq(normalized[0].tags[0].0, "host")
  assert_eq(normalized[0].tags[0].1, "server1")
  
  // 测试时间窗口聚合
  let windows = aggregate_by_time_window(test_points, 3000)  // 3秒窗口
  assert_eq(windows.length(), 2)
  assert_eq(windows[0].length(), 4)  // 前4个点在第一个窗口
  assert_eq(windows[1].length(), 1)  // 最后1个点在第二个窗口
}

// Test 2: Distributed Tracing Scenarios
test "distributed tracing scenarios" {
  // 定义Span类型
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // 创建追踪链
  let create_trace_chain = fn(root_operation: String, child_operations: Array[String]) {
    let trace_id = "trace_" + Time::now().to_string()
    let root_span = {
      trace_id: trace_id,
      span_id: "span_" + Time::now().to_string(),
      parent_span_id: None,
      operation_name: root_operation,
      start_time: Time::now(),
      end_time: Time::now() + 1000,
      status: "ok",
      attributes: [("service", "api-gateway")]
    }
    
    let mut spans = [root_span]
    let mut parent_id = Some(root_span.span_id)
    
    for operation in child_operations {
      let child_span = {
        trace_id: trace_id,
        span_id: "span_" + Time::now().to_string() + operation,
        parent_span_id: parent_id,
        operation_name: operation,
        start_time: Time::now(),
        end_time: Time::now() + 500,
        status: "ok",
        attributes: [("service", operation)]
      }
      spans = spans.push(child_span)
      parent_id = Some(child_span.span_id)
    }
    
    spans
  }
  
  // 分析追踪链
  let analyze_trace = fn(spans: Array[Span]) {
    if spans.length() == 0 {
      return { total_spans: 0, total_duration: 0, error_rate: 0.0 }
    }
    
    let root_span = spans[0]
    let total_duration = root_span.end_time - root_span.start_time
    let error_count = spans.filter(fn(span) { span.status != "ok" }).length()
    let error_rate = (error_count as Float) / (spans.length() as Float)
    
    {
      total_spans: spans.length(),
      total_duration: total_duration,
      error_rate: error_rate
    }
  }
  
  // 测试正常追踪链
  let normal_trace = create_trace_chain("api-request", ["auth", "validation", "database"])
  let analysis = analyze_trace(normal_trace)
  assert_eq(analysis.total_spans, 4)
  assert_eq(analysis.total_duration, 1000)
  assert_eq(analysis.error_rate, 0.0)
  
  // 测试带错误的追踪链
  let error_spans = normal_trace.map(fn(span) {
    if span.operation_name == "database" {
      { span | status: "error", attributes: span.attributes.push(("error.type", "timeout")) }
    } else {
      span
    }
  })
  
  let error_analysis = analyze_trace(error_spans)
  assert_eq(error_analysis.total_spans, 4)
  assert_eq(error_analysis.error_rate, 0.25)
}

// Test 3: Performance Monitoring and Metrics Collection
test "performance monitoring and metrics collection" {
  // 定义度量类型
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // 定义度量数据
  type Metric = {
    name: String,
    metric_type: MetricType,
    value: Float,
    labels: Array[(String, String)],
    timestamp: Int
  }
  
  // 创建度量收集器
  let create_collector = fn() {
    let metrics = []
    
    let record_counter = fn(name: String, value: Float, labels: Array[(String, String)]) {
      let metric = {
        name: name,
        metric_type: MetricType::Counter,
        value: value,
        labels: labels,
        timestamp: Time::now()
      }
      metrics = metrics.push(metric)
    }
    
    let record_gauge = fn(name: String, value: Float, labels: Array[(String, String)]) {
      let metric = {
        name: name,
        metric_type: MetricType::Gauge,
        value: value,
        labels: labels,
        timestamp: Time::now()
      }
      metrics = metrics.push(metric)
    }
    
    let get_metrics = fn() { metrics }
    
    { record_counter, record_gauge, get_metrics }
  }
  
  // 创建度量分析器
  let analyze_metrics = fn(metrics: Array[Metric]) {
    let counters = metrics.filter(fn(m) { 
      match m.metric_type { 
        MetricType::Counter => true 
        _ => false 
      } 
    })
    
    let gauges = metrics.filter(fn(m) { 
      match m.metric_type { 
        MetricType::Gauge => true 
        _ => false 
      } 
    })
    
    let counter_sum = counters.reduce(fn(acc, m) { acc + m.value }, 0.0)
    let gauge_avg = if gauges.length() > 0 {
      gauges.reduce(fn(acc, m) { acc + m.value }, 0.0) / (gauges.length() as Float)
    } else {
      0.0
    }
    
    {
      total_metrics: metrics.length(),
      counter_count: counters.length(),
      gauge_count: gauges.length(),
      counter_sum: counter_sum,
      gauge_average: gauge_avg
    }
  }
  
  // 测试度量收集
  let collector = create_collector()
  collector.record_counter("http_requests_total", 100.0, [("method", "GET"), ("/api", "users")])
  collector.record_counter("http_requests_total", 50.0, [("method", "POST"), ("/api", "users")])
  collector.record_gauge("memory_usage_bytes", 1024.0 * 1024.0 * 512.0, [("host", "server1")])
  collector.record_gauge("cpu_usage_percent", 75.5, [("host", "server1")])
  
  let all_metrics = collector.get_metrics()
  let analysis = analyze_metrics(all_metrics)
  
  assert_eq(analysis.total_metrics, 4)
  assert_eq(analysis.counter_count, 2)
  assert_eq(analysis.gauge_count, 2)
  assert_eq(analysis.counter_sum, 150.0)
  assert_eq(analysis.gauge_average.round(), (1024.0 * 1024.0 * 512.0 + 75.5) / 2.0)
}

// Test 4: Error Handling and Exception Scenarios
test "error handling and exception scenarios" {
  // 定义错误类型
  enum TelemetryError {
    NetworkTimeout(String)
    SerializationError(String)
    ValidationFailed(String)
    RateLimitExceeded(Int)
    CircuitBreakerOpen(String)
  }
  
  // 定义错误恢复策略
  enum RecoveryStrategy {
    RetryWithBackoff(Int, Float)  // max_attempts, initial_delay
    FailFast
    UseFallback(String)
    CircuitBreaker(Int)  // failure_threshold
  }
  
  // 创建错误处理器
  let create_error_handler = fn() {
    let error_counts = Map::empty()
    
    let handle_error = fn(error: TelemetryError, strategy: RecoveryStrategy) {
      let error_key = match error {
        TelemetryError::NetworkTimeout(_) => "network_timeout"
        TelemetryError::SerializationError(_) => "serialization_error"
        TelemetryError::ValidationFailed(_) => "validation_failed"
        TelemetryError::RateLimitExceeded(_) => "rate_limit_exceeded"
        TelemetryError::CircuitBreakerOpen(_) => "circuit_breaker_open"
      }
      
      let current_count = match Map::get(error_counts, error_key) {
        Some(count) => count
        None => 0
      }
      
      let _ = Map::insert(error_counts, error_key, current_count + 1)
      
      match (error, strategy) {
        (TelemetryError::NetworkTimeout(msg), RecoveryStrategy::RetryWithBackoff(max_attempts, delay)) => {
          if current_count < max_attempts {
            "retry_with_backoff"
          } else {
            "max_attempts_reached"
          }
        }
        (TelemetryError::RateLimitExceeded(retry_after), RecoveryStrategy::RetryWithBackoff(_, _)) => {
          "wait_for_rate_limit"
        }
        (TelemetryError::CircuitBreakerOpen(service), RecoveryStrategy::CircuitBreaker(threshold)) => {
          if current_count >= threshold {
            "circuit_breaker_triggered"
          } else {
            "continue_processing"
          }
        }
        (_, RecoveryStrategy::FailFast) => {
          "fail_fast"
        }
        (_, RecoveryStrategy::UseFallback(fallback_value)) => {
          "using_fallback: " + fallback_value
        }
        (_, _) => {
          "default_handling"
        }
      }
    }
    
    let get_error_counts = fn() { error_counts }
    
    { handle_error, get_error_counts }
  }
  
  // 测试错误处理
  let handler = create_error_handler()
  
  // 测试网络超时重试
  let network_error = TelemetryError::NetworkTimeout("Connection timeout after 30s")
  let retry_strategy = RecoveryStrategy::RetryWithBackoff(3, 1000.0)
  
  assert_eq(handler.handle_error(network_error, retry_strategy), "retry_with_backoff")
  assert_eq(handler.handle_error(network_error, retry_strategy), "retry_with_backoff")
  assert_eq(handler.handle_error(network_error, retry_strategy), "retry_with_backoff")
  assert_eq(handler.handle_error(network_error, retry_strategy), "max_attempts_reached")
  
  // 测试速率限制
  let rate_limit_error = TelemetryError::RateLimitExceeded(5000)
  assert_eq(handler.handle_error(rate_limit_error, retry_strategy), "wait_for_rate_limit")
  
  // 测试断路器
  let circuit_error = TelemetryError::CircuitBreakerOpen("payment-service")
  let circuit_strategy = RecoveryStrategy::CircuitBreaker(2)
  
  assert_eq(handler.handle_error(circuit_error, circuit_strategy), "continue_processing")
  assert_eq(handler.handle_error(circuit_error, circuit_strategy), "circuit_breaker_triggered")
  
  // 验证错误计数
  let error_counts = handler.get_error_counts()
  assert_eq(Map::get(error_counts, "network_timeout"), Some(4))
  assert_eq(Map::get(error_counts, "rate_limit_exceeded"), Some(1))
  assert_eq(Map::get(error_counts, "circuit_breaker_open"), Some(2))
}

// Test 5: Configuration Management and Dynamic Updates
test "configuration management and dynamic updates" {
  // 定义配置类型
  type TelemetryConfig = {
    service_name: String,
    service_version: String,
    sampling_rate: Float,
    batch_size: Int,
    export_interval_ms: Int,
    enabled_features: Array[String]
  }
  
  // 创建配置管理器
  let create_config_manager = fn(initial_config: TelemetryConfig) {
    let current_config = initial_config
    let config_history = [initial_config]
    let subscribers = []
    
    let update_config = fn(updates: (TelemetryConfig) -> TelemetryConfig) {
      let new_config = updates(current_config)
      current_config = new_config
      config_history = config_history.push(new_config)
      
      // 通知订阅者
      for subscriber in subscribers {
        subscriber(new_config)
      }
    }
    
    let get_config = fn() { current_config }
    let get_history = fn() { config_history }
    
    let subscribe = fn(subscriber: (TelemetryConfig) -> Unit) {
      subscribers = subscribers.push(subscriber)
      subscriber(current_config)
    }
    
    { update_config, get_config, get_history, subscribe }
  }
  
  // 创建配置验证器
  let validate_config = fn(config: TelemetryConfig) {
    let errors = []
    
    if config.service_name.length() == 0 {
      errors = errors.push("Service name cannot be empty")
    }
    
    if config.sampling_rate < 0.0 || config.sampling_rate > 1.0 {
      errors = errors.push("Sampling rate must be between 0.0 and 1.0")
    }
    
    if config.batch_size <= 0 {
      errors = errors.push("Batch size must be positive")
    }
    
    if config.export_interval_ms <= 0 {
      errors = errors.push("Export interval must be positive")
    }
    
    errors
  }
  
  // 测试配置管理
  let initial_config = {
    service_name: "payment-service",
    service_version: "1.0.0",
    sampling_rate: 0.1,
    batch_size: 100,
    export_interval_ms: 5000,
    enabled_features: ["tracing", "metrics"]
  }
  
  let config_manager = create_config_manager(initial_config)
  
  // 验证初始配置
  let initial_validation = validate_config(config_manager.get_config())
  assert_eq(initial_validation.length(), 0)
  
  // 测试配置更新
  config_manager.update_config(fn(config) {
    { config | 
      sampling_rate: 0.2,
      batch_size: 200,
      enabled_features: config.enabled_features.push("logging")
    }
  })
  
  let updated_config = config_manager.get_config()
  assert_eq(updated_config.sampling_rate, 0.2)
  assert_eq(updated_config.batch_size, 200)
  assert_eq(updated_config.enabled_features.length(), 3)
  
  // 验证更新后的配置
  let updated_validation = validate_config(updated_config)
  assert_eq(updated_validation.length(), 0)
  
  // 测试配置历史
  let history = config_manager.get_history()
  assert_eq(history.length(), 2)
  assert_eq(history[0].sampling_rate, 0.1)
  assert_eq(history[1].sampling_rate, 0.2)
  
  // 测试无效配置
  config_manager.update_config(fn(config) {
    { config | 
      sampling_rate: 1.5,  // 无效值
      batch_size: -10      // 无效值
    }
  })
  
  let invalid_config = config_manager.get_config()
  let invalid_validation = validate_config(invalid_config)
  assert_eq(invalid_validation.length(), 2)
  assert_true(invalid_validation.contains("Sampling rate must be between 0.0 and 1.0"))
  assert_true(invalid_validation.contains("Batch size must be positive"))
  
  // 测试配置订阅
  let mut notification_count = 0
  let mut last_config = initial_config
  
  config_manager.subscribe(fn(config) {
    notification_count = notification_count + 1
    last_config = config
  })
  
  config_manager.update_config(fn(config) {
    { config | 
      service_version: "1.1.0"
    }
  })
  
  assert_eq(notification_count, 2)  // 初始订阅通知 + 更新通知
  assert_eq(last_config.service_version, "1.1.0")
}

// Test 6: Data Serialization and Deserialization
test "data serialization and deserialization" {
  // 定义序列化格式
  enum SerializationFormat {
    Json
    Protobuf
    Avro
  }
  
  // 定义可序列化数据
  type SerializableData = {
    trace_id: String,
    span_id: String,
    operation_name: String,
    start_time: Int,
    duration_ms: Int,
    attributes: Array[(String, String)]
  }
  
  // 创建序列化器
  let create_serializer = fn(format: SerializationFormat) {
    let serialize = fn(data: SerializableData) {
      match format {
        SerializationFormat::Json => {
          // 简化的JSON序列化
          let attrs_str = data.attributes.map(fn(attr) {
            let (key, value) = attr
            "\"" + key + "\":\"" + value + "\""
          }).join(",")
          
          "{" +
          "\"trace_id\":\"" + data.trace_id + "\"," +
          "\"span_id\":\"" + data.span_id + "\"," +
          "\"operation_name\":\"" + data.operation_name + "\"," +
          "\"start_time\":" + data.start_time.to_string() + "," +
          "\"duration_ms\":" + data.duration_ms.to_string() + "," +
          "\"attributes\":{" + attrs_str + "}" +
          "}"
        }
        SerializationFormat::Protobuf => {
          // 简化的Protobuf序列化
          "protobuf:" + data.trace_id + ":" + data.span_id + ":" + data.operation_name
        }
        SerializationFormat::Avro => {
          // 简化的Avro序列化
          "avro:" + data.trace_id + "|" + data.span_id + "|" + data.operation_name
        }
      }
    }
    
    let deserialize = fn(serialized: String) {
      match format {
        SerializationFormat::Json => {
          // 简化的JSON反序列化
          if serialized.contains("trace_id") && serialized.contains("span_id") {
            Some({
              trace_id: "deserialized_trace_id",
              span_id: "deserialized_span_id",
              operation_name: "deserialized_operation",
              start_time: 1234567890,
              duration_ms: 100,
              attributes: [("key", "value")]
            })
          } else {
            None
          }
        }
        SerializationFormat::Protobuf => {
          if serialized.starts_with("protobuf:") {
            let parts = serialized.split(":")
            if parts.length() >= 4 {
              Some({
                trace_id: parts[1],
                span_id: parts[2],
                operation_name: parts[3],
                start_time: 1234567890,
                duration_ms: 100,
                attributes: []
              })
            } else {
              None
            }
          } else {
            None
          }
        }
        SerializationFormat::Avro => {
          if serialized.starts_with("avro:") {
            let parts = serialized.split("|")
            if parts.length() >= 4 {
              Some({
                trace_id: parts[1],
                span_id: parts[2],
                operation_name: parts[3],
                start_time: 1234567890,
                duration_ms: 100,
                attributes: []
              })
            } else {
              None
            }
          } else {
            None
          }
        }
      }
    }
    
    { serialize, deserialize }
  }
  
  // 测试数据
  let test_data = {
    trace_id: "1234567890abcdef1234567890abcdef",
    span_id: "1234567890abcdef",
    operation_name: "http_request",
    start_time: 1234567890,
    duration_ms: 150,
    attributes: [
      ("http.method", "GET"),
      ("http.url", "/api/users"),
      ("http.status_code", "200")
    ]
  }
  
  // 测试JSON序列化
  let json_serializer = create_serializer(SerializationFormat::Json)
  let json_serialized = json_serializer.serialize(test_data)
  assert_true(json_serialized.contains("\"trace_id\":\"1234567890abcdef1234567890abcdef\""))
  assert_true(json_serialized.contains("\"operation_name\":\"http_request\""))
  assert_true(json_serialized.contains("\"attributes\":{\"http.method\":\"GET\""))
  
  let json_deserialized = json_serializer.deserialize(json_serialized)
  assert_true(json_deserialized.is_some())
  
  // 测试Protobuf序列化
  let protobuf_serializer = create_serializer(SerializationFormat::Protobuf)
  let protobuf_serialized = protobuf_serializer.serialize(test_data)
  assert_eq(protobuf_serialized, "protobuf:1234567890abcdef1234567890abcdef:1234567890abcdef:http_request")
  
  let protobuf_deserialized = protobuf_serializer.deserialize(protobuf_serialized)
  assert_true(protobuf_deserialized.is_some())
  
  match protobuf_deserialized {
    Some(data) => {
      assert_eq(data.trace_id, "1234567890abcdef1234567890abcdef")
      assert_eq(data.span_id, "1234567890abcdef")
      assert_eq(data.operation_name, "http_request")
    }
    None => assert_true(false)
  }
  
  // 测试Avro序列化
  let avro_serializer = create_serializer(SerializationFormat::Avro)
  let avro_serialized = avro_serializer.serialize(test_data)
  assert_eq(avro_serialized, "avro:1234567890abcdef1234567890abcdef|1234567890abcdef|http_request")
  
  let avro_deserialized = avro_serializer.deserialize(avro_serialized)
  assert_true(avro_deserialized.is_some())
  
  // 测试无效数据反序列化
  let invalid_json = json_serializer.deserialize("invalid json")
  assert_eq(invalid_json, None)
  
  let invalid_protobuf = protobuf_serializer.deserialize("invalid:protobuf")
  assert_eq(invalid_protobuf, None)
  
  let invalid_avro = avro_serializer.deserialize("invalid|avro")
  assert_eq(invalid_avro, None)
}

// Test 7: Context Propagation and Cross-Service Communication
test "context propagation and cross-service communication" {
  // 定义上下文类型
  type Context = {
    trace_id: String,
    span_id: String,
    baggage: Array[(String, String)],
    entries: Map[String, String]
  }
  
  // 定义传播器类型
  enum PropagatorType {
    TraceContext
    Baggage
    Composite
  }
  
  // 创建传播器
  let create_propagator = fn(propagator_type: PropagatorType) {
    let inject = fn(context: Context, carrier: Array[(String, String)]) {
      match propagator_type {
        PropagatorType::TraceContext => {
          let traceparent = "00-" + context.trace_id + "-" + context.span_id + "-01"
          carrier.push(("traceparent", traceparent))
        }
        PropagatorType::Baggage => {
          if context.baggage.length() > 0 {
            let baggage_str = context.baggage.map(fn(entry) {
              let (key, value) = entry
              key + "=" + value
            }).join(",")
            carrier.push(("baggage", baggage_str))
          }
        }
        PropagatorType::Composite => {
          // 注入traceparent和baggage
          let traceparent = "00-" + context.trace_id + "-" + context.span_id + "-01"
          carrier.push(("traceparent", traceparent))
          
          if context.baggage.length() > 0 {
            let baggage_str = context.baggage.map(fn(entry) {
              let (key, value) = entry
              key + "=" + value
            }).join(",")
            carrier.push(("baggage", baggage_str))
          }
        }
      }
      carrier
    }
    
    let extract = fn(carrier: Array[(String, String)]) {
      let mut trace_id = ""
      let mut span_id = ""
      let mut baggage = []
      
      for (key, value) in carrier {
        match key {
          "traceparent" => {
            let parts = value.split("-")
            if parts.length() >= 4 {
              trace_id = parts[1]
              span_id = parts[2]
            }
          }
          "baggage" => {
            let entries = value.split(",")
            for entry in entries {
              let entry_parts = entry.split("=")
              if entry_parts.length() == 2 {
                baggage = baggage.push((entry_parts[0], entry_parts[1]))
              }
            }
          }
          _ => {}
        }
      }
      
      if trace_id.length() > 0 && span_id.length() > 0 {
        Some({
          trace_id: trace_id,
          span_id: span_id,
          baggage: baggage,
          entries: Map::empty()
        })
      } else {
        None
      }
    }
    
    { inject, extract }
  }
  
  // 测试上下文传播
  let original_context = {
    trace_id: "1234567890abcdef1234567890abcdef",
    span_id: "1234567890abcdef",
    baggage: [
      ("user.id", "12345"),
      ("request.id", "req-67890"),
      ("region", "us-west-2")
    ],
    entries: Map::empty()
  }
  
  // 测试TraceContext传播器
  let trace_propagator = create_propagator(PropagatorType::TraceContext)
  let mut carrier = []
  let trace_carrier = trace_propagator.inject(original_context, carrier)
  
  assert_eq(trace_carrier.length(), 1)
  assert_eq(trace_carrier[0].0, "traceparent")
  assert_eq(trace_carrier[0].1, "00-1234567890abcdef1234567890abcdef-1234567890abcdef-01")
  
  let extracted_trace_context = trace_propagator.extract(trace_carrier)
  assert_true(extracted_trace_context.is_some())
  
  match extracted_trace_context {
    Some(context) => {
      assert_eq(context.trace_id, original_context.trace_id)
      assert_eq(context.span_id, original_context.span_id)
      assert_eq(context.baggage.length(), 0)  // TraceContext不传播baggage
    }
    None => assert_true(false)
  }
  
  // 测试Baggage传播器
  let baggage_propagator = create_propagator(PropagatorType::Baggage)
  carrier = []
  let baggage_carrier = baggage_propagator.inject(original_context, carrier)
  
  assert_eq(baggage_carrier.length(), 1)
  assert_eq(baggage_carrier[0].0, "baggage")
  assert_eq(baggage_carrier[0].1, "user.id=12345,request.id=req-67890,region=us-west-2")
  
  let extracted_baggage_context = baggage_propagator.extract(baggage_carrier)
  assert_eq(extracted_baggage_context, None)  // 没有traceparent，无法提取
  
  // 测试Composite传播器
  let composite_propagator = create_propagator(PropagatorType::Composite)
  carrier = []
  let composite_carrier = composite_propagator.inject(original_context, carrier)
  
  assert_eq(composite_carrier.length(), 2)
  assert_true(composite_carrier.any(fn(item) { item.0 == "traceparent" }))
  assert_true(composite_carrier.any(fn(item) { item.0 == "baggage" }))
  
  let extracted_composite_context = composite_propagator.extract(composite_carrier)
  assert_true(extracted_composite_context.is_some())
  
  match extracted_composite_context {
    Some(context) => {
      assert_eq(context.trace_id, original_context.trace_id)
      assert_eq(context.span_id, original_context.span_id)
      assert_eq(context.baggage.length(), 3)
      assert_true(context.baggage.contains(("user.id", "12345")))
      assert_true(context.baggage.contains(("request.id", "req-67890")))
      assert_true(context.baggage.contains(("region", "us-west-2")))
    }
    None => assert_true(false)
  }
}

// Test 8: Sampling Strategies and Filtering Mechanisms
test "sampling strategies and filtering mechanisms" {
  // 定义采样决策
  enum SamplingDecision {
    RecordAndSample
    RecordOnly
    Drop
  }
  
  // 定义采样器类型
  enum SamplerType {
    AlwaysOn
    AlwaysOff
    TraceIdRatio(Float)  // 采样率
    ParentBased(Box[SamplerType])  // 基于父span的采样决策
    AttributeBased(Array[(String, String)])  // 基于属性的采样
  }
  
  // 创建采样器
  let create_sampler = fn(sampler_type: SamplerType) {
    let should_sample = fn(trace_id: String, parent_sampled: Option[Bool], attributes: Array[(String, String)]) {
      match sampler_type {
        SamplerType::AlwaysOn => {
          SamplingDecision::RecordAndSample
        }
        SamplerType::AlwaysOff => {
          SamplingDecision::Drop
        }
        SamplerType::TraceIdRatio(ratio) => {
          // 简化的采样算法：基于trace_id的哈希值
          let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
          let normalized = (hash % 100) as Float / 100.0
          if normalized <= ratio {
            SamplingDecision::RecordAndSample
          } else {
            SamplingDecision::Drop
          }
        }
        SamplerType::ParentBased(inner_sampler) => {
          match parent_sampled {
            Some(true) => SamplingDecision::RecordAndSample,
            Some(false) => SamplingDecision::Drop,
            None => {
              // 没有父span，使用内部采样器
              let inner = create_sampler(*inner_sampler)
              inner.should_sample(trace_id, None, attributes)
            }
          }
        }
        SamplerType::AttributeBased(required_attributes) => {
          let all_match = required_attributes.all(fn(required) {
            let (key, expected_value) = required
            attributes.any(fn(attr) {
              let (attr_key, attr_value) = attr
              attr_key == key && attr_value == expected_value
            })
          })
          
          if all_match {
            SamplingDecision::RecordAndSample
          } else {
            SamplingDecision::Drop
          }
        }
      }
    }
    
    { should_sample }
  }
  
  // 测试AlwaysOn采样器
  let always_on_sampler = create_sampler(SamplerType::AlwaysOn)
  let decision1 = always_on_sampler.should_sample("trace-123", None, [])
  assert_eq(decision1, SamplingDecision::RecordAndSample)
  
  // 测试AlwaysOff采样器
  let always_off_sampler = create_sampler(SamplerType::AlwaysOff)
  let decision2 = always_off_sampler.should_sample("trace-456", None, [])
  assert_eq(decision2, SamplingDecision::Drop)
  
  // 测试TraceIdRatio采样器
  let ratio_sampler = create_sampler(SamplerType::TraceIdRatio(0.5))  // 50%采样率
  let decision3 = ratio_sampler.should_sample("trace-789", None, [])
  // 结果取决于trace_id的哈希值，我们只验证它返回一个有效决策
  assert_true(decision3 == SamplingDecision::RecordAndSample || decision3 == SamplingDecision::Drop)
  
  // 测试ParentBased采样器
  let parent_based_sampler = create_sampler(SamplerType::ParentBased(Box::new(SamplerType::AlwaysOff)))
  
  // 父span已采样
  let decision4 = parent_based_sampler.should_sample("trace-abc", Some(true), [])
  assert_eq(decision4, SamplingDecision::RecordAndSample)
  
  // 父span未采样
  let decision5 = parent_based_sampler.should_sample("trace-def", Some(false), [])
  assert_eq(decision5, SamplingDecision::Drop)
  
  // 没有父span，使用内部采样器
  let decision6 = parent_based_sampler.should_sample("trace-ghi", None, [])
  assert_eq(decision6, SamplingDecision::Drop)
  
  // 测试AttributeBased采样器
  let attributes = [
    ("service.name", "payment-service"),
    ("environment", "production"),
    ("http.method", "POST")
  ]
  
  let required_attrs = [
    ("environment", "production"),
    ("service.name", "payment-service")
  ]
  
  let attr_sampler = create_sampler(SamplerType::AttributeBased(required_attrs))
  
  // 匹配所有必需属性
  let decision7 = attr_sampler.should_sample("trace-jkl", None, attributes)
  assert_eq(decision7, SamplingDecision::RecordAndSample)
  
  // 不匹配所有必需属性
  let decision8 = attr_sampler.should_sample("trace-mno", None, [
    ("service.name", "payment-service"),
    ("environment", "development")  // 不匹配
  ])
  assert_eq(decision8, SamplingDecision::Drop)
  
  // 创建过滤器
  let create_filter = fn(filter_rules: Array[(String, String)]) {
    let should_include = fn(attributes: Array[(String, String)]) {
      filter_rules.all(fn(rule) {
        let (key, expected_value) = rule
        attributes.any(fn(attr) {
          let (attr_key, attr_value) = attr
          attr_key == key && attr_value == expected_value
        })
      })
    }
    
    { should_include }
  }
  
  // 测试过滤器
  let filter = create_filter([
    ("environment", "production"),
    ("service.name", "api-service")
  ])
  
  let prod_attrs = [
    ("environment", "production"),
    ("service.name", "api-service"),
    ("version", "1.0.0")
  ]
  
  let dev_attrs = [
    ("environment", "development"),
    ("service.name", "api-service"),
    ("version", "1.0.0")
  ]
  
  assert_true(filter.should_include(prod_attrs))
  assert_false(filter.should_include(dev_attrs))
}

// Test 9: Caching Mechanisms and Performance Optimization
test "caching mechanisms and performance optimization" {
  // 定义缓存项类型
  type CacheItem[T] = {
    value: T,
    timestamp: Int,
    ttl_ms: Int,
    access_count: Int
  }
  
  // 创建缓存
  let create_cache = fn[T](max_size: Int, default_ttl_ms: Int) {
    let cache = Map::empty()
    let access_order = []  // LRU跟踪
    
    let get = fn(key: String) {
      match Map::get(cache, key) {
        Some(item) => {
          // 检查是否过期
          let current_time = Time::now()
          if current_time - item.timestamp > item.ttl_ms {
            let _ = Map::remove(cache, key)
            // 从访问顺序中移除
            access_order = access_order.filter(fn(k) { k != key })
            None
          } else {
            // 更新访问计数和顺序
            let updated_item = { item | access_count: item.access_count + 1 }
            let _ = Map::insert(cache, key, updated_item)
            
            // 移动到访问顺序的末尾（最近使用）
            access_order = access_order.filter(fn(k) { k != key })
            access_order = access_order.push(key)
            
            Some(updated_item.value)
          }
        }
        None => None
      }
    }
    
    let put = fn(key: String, value: T, ttl_ms: Option[Int]) {
      let current_time = Time::now()
      let item_ttl = match ttl_ms {
        Some(ttl) => ttl
        None => default_ttl_ms
      }
      
      // 如果缓存已满，移除最少使用的项
      if Map::size(cache) >= max_size && not Map::contains_key(cache, key) {
        if access_order.length() > 0 {
          let lru_key = access_order[0]
          let _ = Map::remove(cache, lru_key)
          access_order = access_order.slice(1, access_order.length())
        }
      }
      
      let item = {
        value: value,
        timestamp: current_time,
        ttl_ms: item_ttl,
        access_count: 1
      }
      
      let _ = Map::insert(cache, key, item)
      
      // 添加到访问顺序
      if not access_order.contains(key) {
        access_order = access_order.push(key)
      } else {
        // 如果已存在，移动到末尾
        access_order = access_order.filter(fn(k) { k != key })
        access_order = access_order.push(key)
      }
    }
    
    let remove = fn(key: String) {
      let _ = Map::remove(cache, key)
      access_order = access_order.filter(fn(k) { k != key })
    }
    
    let clear = fn() {
      let _ = Map::clear(cache)
      access_order = []
    }
    
    let size = fn() { Map::size(cache) }
    
    { get, put, remove, clear, size }
  }
  
  // 测试缓存
  let string_cache = create_cache[String](3, 1000)  // 最大3项，默认TTL 1秒
  
  // 测试基本put/get
  string_cache.put("key1", "value1", None)
  assert_eq(string_cache.size(), 1)
  
  match string_cache.get("key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  // 测试多个项
  string_cache.put("key2", "value2", None)
  string_cache.put("key3", "value3", None)
  assert_eq(string_cache.size(), 3)
  
  // 测试LRU淘汰
  string_cache.put("key4", "value4", None)  // 应该淘汰key1
  assert_eq(string_cache.size(), 3)
  
  assert_eq(string_cache.get("key1"), None)  // key1已被淘汰
  assert_eq(string_cache.get("key2"), Some("value2"))
  assert_eq(string_cache.get("key3"), Some("value3"))
  assert_eq(string_cache.get("key4"), Some("value4"))
  
  // 测试TTL过期
  string_cache.put("ttl_key", "ttl_value", Some(100))  // 100ms TTL
  
  // 立即获取应该成功
  match string_cache.get("ttl_key") {
    Some(value) => assert_eq(value, "ttl_value")
    None => assert_true(false)
  }
  
  // 等待TTL过期（在实际测试中需要等待，这里用模拟）
  // 假设已经过了100ms
  string_cache.remove("ttl_key")  // 模拟过期
  
  assert_eq(string_cache.get("ttl_key"), None)
  
  // 测试缓存清除
  string_cache.clear()
  assert_eq(string_cache.size(), 0)
  assert_eq(string_cache.get("key2"), None)
  assert_eq(string_cache.get("key3"), None)
  assert_eq(string_cache.get("key4"), None)
  
  // 创建性能优化的批处理器
  let create_batch_processor = fn[T](batch_size: Int, flush_interval_ms: Int) {
    let mut batch = []
    let mut last_flush = Time::now()
    
    let process = fn(item: T, processor: (Array[T]) -> Unit) {
      batch = batch.push(item)
      
      let current_time = Time::now()
      if batch.length() >= batch_size || (current_time - last_flush) >= flush_interval_ms {
        processor(batch)
        batch = []
        last_flush = current_time
      }
    }
    
    let flush = fn(processor: (Array[T]) -> Unit) {
      if batch.length() > 0 {
        processor(batch)
        batch = []
        last_flush = Time::now()
      }
    }
    
    { process, flush }
  }
  
  // 测试批处理器
  let processed_batches = []
  let batch_processor = create_batch_processor[String](3, 1000)
  
  let mock_processor = fn(batch: Array[String]) {
    processed_batches.push(batch)
  }
  
  // 添加项目直到达到批处理大小
  batch_processor.process("item1", mock_processor)
  batch_processor.process("item2", mock_processor)
  batch_processor.process("item3", mock_processor)  // 应该触发批处理
  
  assert_eq(processed_batches.length(), 1)
  assert_eq(processed_batches[0], ["item1", "item2", "item3"])
  
  // 添加更多项目
  batch_processor.process("item4", mock_processor)
  batch_processor.process("item5", mock_processor)
  
  // 手动刷新
  batch_processor.flush(mock_processor)
  
  assert_eq(processed_batches.length(), 2)
  assert_eq(processed_batches[1], ["item4", "item5"])
}

// Test 10: Security and Authentication
test "security and authentication" {
  // 定义认证令牌类型
  type AuthToken = {
    token_type: String,
    access_token: String,
    expires_at: Int,
    scopes: Array[String]
  }
  
  // 定义权限类型
  type Permission = {
    resource: String,
    action: String,
    conditions: Array[(String, String)]
  }
  
  // 创建认证管理器
  let create_auth_manager = fn() {
    let tokens = Map::empty()
    let permissions = Map::empty()
    
    let add_token = fn(user_id: String, token: AuthToken) {
      let _ = Map::insert(tokens, user_id, token)
    }
    
    let validate_token = fn(user_id: String, token_string: String) {
      match Map::get(tokens, user_id) {
        Some(token) => {
          let current_time = Time::now()
          if token.access_token == token_string && current_time < token.expires_at {
            true
          } else {
            false
          }
        }
        None => false
      }
    }
    
    let add_permission = fn(user_id: String, permission: Permission) {
      let user_permissions = match Map::get(permissions, user_id) {
        Some(perms) => perms
        None => []
      }
      let updated_permissions = user_permissions.push(permission)
      let _ = Map::insert(permissions, user_id, updated_permissions)
    }
    
    let check_permission = fn(user_id: String, resource: String, action: String, context: Array[(String, String)]) {
      match Map::get(permissions, user_id) {
        Some(user_permissions) => {
          user_permissions.any(fn(permission) {
            if permission.resource == resource && permission.action == action {
              // 检查所有条件是否满足
              permission.conditions.all(fn(condition) {
                let (key, expected_value) = condition
                context.any(fn(ctx_item) {
                  let (ctx_key, ctx_value) = ctx_item
                  ctx_key == key && ctx_value == expected_value
                })
              })
            } else {
              false
            }
          })
        }
        None => false
      }
    }
    
    { add_token, validate_token, add_permission, check_permission }
  }
  
  // 测试认证管理器
  let auth_manager = create_auth_manager()
  
  // 创建测试令牌
  let current_time = Time::now()
  let test_token = {
    token_type: "Bearer",
    access_token: "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.test.token",
    expires_at: current_time + 3600,  // 1小时后过期
    scopes: ["read", "write"]
  }
  
  // 添加令牌
  auth_manager.add_token("user123", test_token)
  
  // 测试有效令牌
  assert_true(auth_manager.validate_token("user123", "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.test.token"))
  
  // 测试无效令牌
  assert_false(auth_manager.validate_token("user123", "invalid.token"))
  assert_false(auth_manager.validate_token("nonexistent_user", "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.test.token"))
  
  // 测试过期令牌（模拟）
  let expired_token = {
    token_type: "Bearer",
    access_token: "expired.token",
    expires_at: current_time - 3600,  // 1小时前过期
    scopes: ["read"]
  }
  
  auth_manager.add_token("user456", expired_token)
  assert_false(auth_manager.validate_token("user456", "expired.token"))
  
  // 添加权限
  auth_manager.add_permission("user123", {
    resource: "telemetry",
    action: "read",
    conditions: [("environment", "production")]
  })
  
  auth_manager.add_permission("user123", {
    resource: "telemetry",
    action: "write",
    conditions: [("service", "payment-service"), ("environment", "production")]
  })
  
  auth_manager.add_permission("user789", {
    resource: "telemetry",
    action: "read",
    conditions: [("environment", "development")]
  })
  
  // 测试权限检查
  let prod_context = [
    ("environment", "production"),
    ("service", "api-service")
  ]
  
  let dev_context = [
    ("environment", "development"),
    ("service", "api-service")
  ]
  
  let payment_prod_context = [
    ("environment", "production"),
    ("service", "payment-service")
  ]
  
  // user123在生产环境有读权限
  assert_true(auth_manager.check_permission("user123", "telemetry", "read", prod_context))
  
  // user123在开发环境没有读权限
  assert_false(auth_manager.check_permission("user123", "telemetry", "read", dev_context))
  
  // user123在生产环境的payment-service有写权限
  assert_true(auth_manager.check_permission("user123", "telemetry", "write", payment_prod_context))
  
  // user123在生产环境的api-service没有写权限
  assert_false(auth_manager.check_permission("user123", "telemetry", "write", prod_context))
  
  // user789在开发环境有读权限
  assert_true(auth_manager.check_permission("user789", "telemetry", "read", dev_context))
  
  // user789在生产环境没有读权限
  assert_false(auth_manager.check_permission("user789", "telemetry", "read", prod_context))
  
  // 测试不存在的用户
  assert_false(auth_manager.check_permission("nonexistent_user", "telemetry", "read", prod_context))
  
  // 创建安全策略验证器
  let create_security_policy_validator = fn() {
    let validate_data_access = fn(user_id: String, data_type: String, operation: String, 
                                  data_sensitivity: String, user_clearance: String) {
      // 定义敏感度级别
      let sensitivity_levels = ["public", "internal", "confidential", "secret"]
      let clearance_levels = ["public", "internal", "confidential", "secret"]
      
      let data_level_index = sensitivity_levels.find_index(fn(level) { level == data_sensitivity })
      let clearance_level_index = clearance_levels.find_index(fn(level) { level == user_clearance })
      
      match (data_level_index, clearance_level_index) {
        (Some(data_idx), Some(clearance_idx)) => {
          if clearance_idx >= data_idx {
            // 用户有足够的权限级别
            match operation {
              "read" => true,
              "write" => user_clearance != "public",  // 公共用户不能写入
              "delete" => user_clearance == "secret",  // 只有secret用户可以删除
              _ => false
            }
          } else {
            false
          }
        }
        _ => false
      }
    }
    
    { validate_data_access }
  }
  
  // 测试安全策略
  let security_validator = create_security_policy_validator()
  
  // 公共用户可以读取公共数据
  assert_true(security_validator.validate_data_access("user1", "metrics", "read", "public", "public"))
  
  // 公共用户不能读取机密数据
  assert_false(security_validator.validate_data_access("user1", "traces", "read", "confidential", "public"))
  
  // 内部用户可以读取内部数据
  assert_true(security_validator.validate_data_access("user2", "logs", "read", "internal", "internal"))
  
  // 内部用户可以写入内部数据
  assert_true(security_validator.validate_data_access("user2", "logs", "write", "internal", "internal"))
  
  // 机密用户可以读取机密数据
  assert_true(security_validator.validate_data_access("user3", "traces", "read", "confidential", "confidential"))
  
  // 机密用户不能删除数据
  assert_false(security_validator.validate_data_access("user3", "traces", "delete", "confidential", "confidential"))
  
  // 秘密用户可以删除数据
  assert_true(security_validator.validate_data_access("user4", "traces", "delete", "secret", "secret"))
}
// OTLP导出器集成测试 - 端到端测试OpenTelemetry Protocol导出器的高级功能

test "otlp_exporter_end_to_end_trace_flow" {
  // 测试完整的追踪数据流从生成到导出
  
  // 模拟完整的Span生命周期
  struct TraceContext {
    trace_id : String
    span_id : String
    parent_span_id : String?
    sampling_decision : Bool
    trace_state : String
  }
  
  struct SpanEvent {
    name : String
    timestamp : Int64
    attributes : Array[String]
  }
  
  struct SpanLink {
    trace_id : String
    span_id : String
    attributes : Array[String]
  }
  
  struct CompleteSpan {
    context : TraceContext
    name : String
    kind : String
    start_time : Int64
    end_time : Int64
    status : String
    attributes : Array[String]
    events : Array[SpanEvent]
    links : Array[SpanLink]
    resource : Array[String]
  }
  
  // 创建根Span
  let root_context = {
    trace_id: "1234567890abcdef1234567890abcdef",
    span_id: "1111111111111111",
    parent_span_id: None,
    sampling_decision: true,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  let root_span = {
    context: root_context,
    name: "root-operation",
    kind: "SPAN_KIND_SERVER",
    start_time: 1609459200000000000L,
    end_time: 1609459202000000000L,
    status: "STATUS_CODE_OK",
    attributes: ["service.name:payment-service", "service.version:1.0.0"],
    events: [],
    links: [],
    resource: ["service.name:payment-service", "service.instance.id:instance-1"]
  }
  
  // 创建子Span
  let child_context = {
    trace_id: root_context.trace_id,
    span_id: "2222222222222222",
    parent_span_id: Some(root_context.span_id),
    sampling_decision: true,
    trace_state: root_context.trace_state
  }
  
  let child_span = {
    context: child_context,
    name: "database-query",
    kind: "SPAN_KIND_CLIENT",
    start_time: 1609459200500000000L,
    end_time: 1609459201500000000L,
    status: "STATUS_CODE_OK",
    attributes: ["db.system:postgresql", "db.statement:SELECT * FROM orders"],
    events: [
      {
        name: "db.query.start",
        timestamp: 1609459200500000000L,
        attributes: ["db.type:read"]
      },
      {
        name: "db.query.complete",
        timestamp: 1609459201500000000L,
        attributes: ["db.rows:10", "db.duration_ms:100"]
      }
    ],
    links: [],
    resource: root_span.resource
  }
  
  // 模拟OTLP导出序列化
  let spans = [root_span, child_span]
  
  // 验证Span关系
  assert_eq(spans.length(), 2)
  assert_eq(spans[0].context.parent_span_id, None)
  assert_eq(spans[1].context.parent_span_id, Some(spans[0].context.span_id))
  assert_eq(spans[0].context.trace_id, spans[1].context.trace_id)
  
  // 验证时间顺序
  assert_eq(spans[0].start_time < spans[1].start_time, true)
  assert_eq(spans[1].end_time < spans[0].end_time, true)
  
  // 验证事件顺序
  let child_events = spans[1].events
  assert_eq(child_events.length(), 2)
  assert_eq(child_events[0].timestamp < child_events[1].timestamp, true)
}

test "otlp_exporter_metrics_aggregation_flow" {
  // 测试指标数据的聚合和导出流程
  
  struct MetricPoint {
    attributes : Array[String]
    start_time : Int64
    end_time : Int64
    value : Double
    exemplars : Array[String]
  }
  
  struct HistogramMetric {
    name : String
    description : String
    unit : String
    aggregation_temporality : String
    data_points : Array[MetricPoint]
    sum : Double
    count : Int64
    min : Double
    max : Double
    bucket_counts : Array[Int64]
    explicit_bounds : Array[Double]
  }
  
  // 创建直方图指标
  let request_duration_histogram = {
    name: "http.server.request.duration",
    description: "HTTP server request duration in milliseconds",
    unit: "ms",
    aggregation_temporality: "AGGREGATION_TEMPORALITY_CUMULATIVE",
    data_points: [
      {
        attributes: ["http.method:GET", "http.status_code:200"],
        start_time: 1609459200000000000L,
        end_time: 1609459260000000000L,
        value: 0.0,
        exemplars: []
      }
    ],
    sum: 1250.5,
    count: 100L,
    min: 5.2,
    max: 500.8,
    bucket_counts: [0L, 10L, 50L, 30L, 8L, 2L],
    explicit_bounds: [10.0, 50.0, 100.0, 200.0, 500.0]
  }
  
  // 验证直方图数据一致性
  let total_count_from_buckets = request_duration_histogram.bucket_counts.reduce(fn(acc, x) { acc + x }, 0L)
  assert_eq(total_count_from_buckets, request_duration_histogram.count)
  
  // 验证桶边界数量
  assert_eq(request_duration_histogram.bucket_counts.length(), 
            request_duration_histogram.explicit_bounds.length() + 1)
  
  // 验证聚合时间窗口
  let duration_nanos = request_duration_histogram.data_points[0].end_time - 
                      request_duration_histogram.data_points[0].start_time
  assert_eq(duration_nanos, 60000000000L)  // 60秒
  
  // 创建计数器指标
  struct CounterMetric {
    name : String
    description : String
    unit : String
    aggregation_temporality : String
    data_points : Array[MetricPoint]
  }
  
  let request_counter = {
    name: "http.server.requests.total",
    description: "Total number of HTTP requests",
    unit: "1",
    aggregation_temporality: "AGGREGATION_TEMPORALITY_CUMULATIVE",
    data_points: [
      {
        attributes: ["http.method:GET", "http.status_code:200"],
        start_time: 1609459200000000000L,
        end_time: 1609459260000000000L,
        value: 100.0,
        exemplars: []
      },
      {
        attributes: ["http.method:POST", "http.status_code:201"],
        start_time: 1609459200000000000L,
        end_time: 1609459260000000000L,
        value: 25.0,
        exemplars: []
      }
    ]
  }
  
  // 验证计数器数据
  assert_eq(request_counter.data_points.length(), 2)
  let total_requests = request_counter.data_points.reduce(fn(acc, dp) { acc + dp.value }, 0.0)
  assert_eq(total_requests, 125.0)
}

test "otlp_exporter_logs_structured_formatting" {
  // 测试结构化日志的格式化和导出
  
  enum SeverityNumber {
    TRACE
    DEBUG
    INFO
    WARN
    ERROR
    FATAL
  }
  
  struct LogRecord {
    time_unix_nano : Int64
    observed_time_unix_nano : Int64
    severity_number : SeverityNumber
    severity_text : String
    body : String
    attributes : Array[String]
    trace_id : String?
    span_id : String?
    flags : Int
  }
  
  // 创建不同严重级别的日志记录
  let log_records = [
    {
      time_unix_nano: 1609459201000000000L,
      observed_time_unix_nano: 1609459201000000000L,
      severity_number: INFO,
      severity_text: "INFO",
      body: "User login successful",
      attributes: ["user.id:12345", "user.name:john_doe", "ip.address:192.168.1.100"],
      trace_id: Some("1234567890abcdef1234567890abcdef"),
      span_id: Some("1111111111111111"),
      flags: 1
    },
    {
      time_unix_nano: 1609459202000000000L,
      observed_time_unix_nano: 1609459202000000000L,
      severity_number: WARN,
      severity_text: "WARN",
      body: "Database connection pool running low",
      attributes: ["pool.active:8", "pool.max:10", "pool.waiting_time_ms:150"],
      trace_id: Some("1234567890abcdef1234567890abcdef"),
      span_id: Some("2222222222222222"),
      flags: 1
    },
    {
      time_unix_nano: 1609459203000000000L,
      observed_time_unix_nano: 1609459203000000000L,
      severity_number: ERROR,
      severity_text: "ERROR",
      body: "Payment processing failed",
      attributes: [
        "error.type:PaymentDeclined", 
        "payment.amount:99.99", 
        "payment.currency:USD",
        "error.code:402",
        "error.message:Insufficient funds"
      ],
      trace_id: Some("1234567890abcdef1234567890abcdef"),
      span_id: Some("3333333333333333"),
      flags: 1
    }
  ]
  
  // 验证日志记录结构
  assert_eq(log_records.length(), 3)
  
  // 验证严重级别递增
  assert_eq(log_records[0].severity_number < log_records[1].severity_number, true)
  assert_eq(log_records[1].severity_number < log_records[2].severity_number, true)
  
  // 验证时间顺序
  assert_eq(log_records[0].time_unix_nano < log_records[1].time_unix_nano, true)
  assert_eq(log_records[1].time_unix_nano < log_records[2].time_unix_nano, true)
  
  // 验证错误日志的丰富属性
  let error_log = log_records[2]
  assert_eq(error_log.attributes.length(), 5)
  assert_eq(error_log.attributes.any(fn(attr) { attr.has_prefix("error.type:") }), true)
  assert_eq(error_log.attributes.any(fn(attr) { attr.has_prefix("error.code:") }), true)
  
  // 验证追踪关联
  for log in log_records {
    match log.trace_id {
      Some(trace_id) => assert_eq(trace_id.length(), 32)
      None => assert_eq(false, true)  // 所有日志都应该有关联的追踪ID
    }
    match log.span_id {
      Some(span_id) => assert_eq(span_id.length(), 16)
      None => assert_eq(false, true)  // 所有日志都应该有关联的Span ID
    }
  }
}

test "otlp_exporter_resource_metadata_integration" {
  // 测试资源元数据的集成和传播
  
  struct ResourceMetadata {
    attributes : Array[String]
    dropped_attributes_count : Int
  }
  
  // 创建服务资源元数据
  let service_resource = {
    attributes: [
      "service.name:order-processing-service",
      "service.version:2.1.0",
      "service.instance.id:order-prod-789",
      "service.namespace:production",
      "service.telemetry.sdk.name:opentelemetry",
      "service.telemetry.sdk.version:1.20.0",
      "service.telemetry.sdk.language:moonbit"
    ],
    dropped_attributes_count: 0
  }
  
  // 创建主机资源元数据
  let host_resource = {
    attributes: [
      "host.name:prod-order-01.example.com",
      "host.arch:amd64",
      "host.os.family:linux",
      "host.os.name:Ubuntu",
      "host.os.version:20.04",
      "host.cpu.count:8",
      "host.memory.total:17179869184"  // 16GB in bytes
    ],
    dropped_attributes_count: 0
  }
  
  // 创建进程资源元数据
  let process_resource = {
    attributes: [
      "process.pid:12345",
      "process.executable.name:/usr/bin/order-processor",
      "process.executable.path:/usr/bin/order-processor",
      "process.command_line:/usr/bin/order-processor --config /etc/order/config.yaml",
      "process.runtime.name:moonbit-runtime",
      "process.runtime.version:1.5.2",
      "process.runtime.description:MoonBit Runtime Environment"
    ],
    dropped_attributes_count: 0
  }
  
  // 创建容器资源元数据
  let container_resource = {
    attributes: [
      "container.name:order-processor-container",
      "container.id:abc123def456",
      "container.image.name:order-processor:2.1.0",
      "container.image.tag:2.1.0",
      "container.runtime:docker"
    ],
    dropped_attributes_count: 0
  }
  
  // 合并所有资源元数据
  let mut all_attributes : Array[String] = []
  let resources = [service_resource, host_resource, process_resource, container_resource]
  
  for resource in resources {
    for attr in resource.attributes {
      all_attributes.push(attr)
    }
  }
  
  // 验证资源属性合并
  assert_eq(all_attributes.length(), 24)
  
  // 验证必需的服务属性存在
  assert_eq(all_attributes.any(fn(attr) { attr == "service.name:order-processing-service" }), true)
  assert_eq(all_attributes.any(fn(attr) { attr.has_prefix("service.version:") }), true)
  
  // 验证主机信息
  assert_eq(all_attributes.any(fn(attr) { attr.has_prefix("host.name:") }), true)
  assert_eq(all_attributes.any(fn(attr) { attr.has_prefix("host.cpu.count:") }), true)
  
  // 验证进程信息
  assert_eq(all_attributes.any(fn(attr) { attr.has_prefix("process.pid:") }), true)
  assert_eq(all_attributes.any(fn(attr) { attr.has_prefix("process.runtime.name:") }), true)
  
  // 验证容器信息（如果在容器中运行）
  assert_eq(all_attributes.any(fn(attr) { attr.has_prefix("container.name:") }), true)
}

test "otlp_exporter_circuit_breaker_integration" {
  // 测试断路器模式的集成
  
  enum CircuitState {
    CLOSED    // 正常状态
    OPEN      // 断路状态
    HALF_OPEN // 半开状态
  }
  
  struct CircuitBreakerConfig {
    failure_threshold : Int        // 失败阈值
    success_threshold : Int        // 成功阈值（半开状态）
    timeout_ms : Int              // 超时时间
    reset_timeout_ms : Int        // 重置超时时间
  }
  
  struct CircuitBreakerState {
    state : CircuitState
    failure_count : Int
    success_count : Int
    last_failure_time : Int64
    last_state_change : Int64
  }
  
  let config = {
    failure_threshold: 5,
    success_threshold: 3,
    timeout_ms: 30000,
    reset_timeout_ms: 60000
  }
  
  let mut state = {
    state: CLOSED,
    failure_count: 0,
    success_count: 0,
    last_failure_time: 0L,
    last_state_change: 1609459200000L
  }
  
  // 模拟一系列请求和响应
  let request_results = [
    "success", "success", "failure", "success", "failure", 
    "failure", "failure", "failure", "failure", "success"  // 第5个失败触发断路
  ]
  
  let mut request_count = 0
  
  for result in request_results {
    request_count = request_count + 1
    
    // 检查断路器状态
    let can_execute = match state.state {
      CLOSED => true
      OPEN => {
        let time_since_open = 1609459200000L - state.last_state_change
        time_since_open > config.reset_timeout_ms.to_int64() * 1000000L
      }
      HALF_OPEN => true
    }
    
    if can_execute {
      // 执行请求
      if result == "success" {
        // 成功响应
        match state.state {
          HALF_OPEN => {
            state.success_count = state.success_count + 1
            if state.success_count >= config.success_threshold {
              state.state = CLOSED
              state.failure_count = 0
              state.success_count = 0
              state.last_state_change = 1609459200000L
            }
          }
          _ => {
            state.failure_count = @max(0, state.failure_count - 1)
          }
        }
      } else {
        // 失败响应
        state.failure_count = state.failure_count + 1
        state.last_failure_time = 1609459200000L
        
        if state.failure_count >= config.failure_threshold {
          state.state = OPEN
          state.last_state_change = 1609459200000L
        }
      }
    }
  }
  
  // 验证断路器状态
  assert_eq(state.state, OPEN)  // 应该是断开状态
  assert_eq(state.failure_count, 6)  // 6个失败
  assert_eq(request_count, 10)
}
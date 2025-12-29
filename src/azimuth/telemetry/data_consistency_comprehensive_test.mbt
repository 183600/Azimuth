// 数据一致性综合测试 - 验证遥测系统在分布式场景下的数据一致性保证

use azimuth.telemetry.api.common.{AttributeValue, Attributes, Resource, InstrumentationScope}
use azimuth.telemetry.api.trace.{SpanKind, StatusCode, Span}
use azimuth.telemetry.api.metrics.{MetricType, Instrument, Measurement}
use azimuth.telemetry.api.logs.{LogRecord, Severity, Logger}
use azimuth.telemetry.api.context.{Context, Baggage, ContextKey}
use azimuth.telemetry.api.propagation.{TextMapPropagator, Propagator}

test "distributed_trace_consistency" {
  // 测试分布式追踪的一致性
  
  let trace_id = "trace_distributed_consistency_123"
  let consistency_check_time = 1641016000L
  
  // 1. 服务A中的根Span
  let service_a_root_span = Span::{
    trace_id: trace_id,
    span_id: "span_service_a_root",
    parent_span_id: None,
    name: "service_a_root_operation",
    kind: SpanKind::Server,
    start_time: consistency_check_time,
    end_time: Some(consistency_check_time + 2000L),
    status: StatusCode::Ok,
    attributes: [
      ("service.name", AttributeValue::string("service-a")),
      ("service.version", AttributeValue::string("1.2.3")),
      ("operation.name", AttributeValue::string("process_request")),
      ("user.id", AttributeValue::string("user_456")),
      ("request.id", AttributeValue::string("req_abc123")),
      ("trace.state", AttributeValue::string("initialized"))
    ],
    events: [
      ("request_received", consistency_check_time + 100L, [
        ("endpoint", AttributeValue::string("/api/process")),
        ("request.size", AttributeValue::int(1024L))
      ])
    ],
    links: []
  }
  
  // 2. 服务A调用服务B的子Span
  let service_a_to_b_span = Span::{
    trace_id: trace_id,
    span_id: "span_service_a_to_b",
    parent_span_id: Some("span_service_a_root"),
    name: "service_a_calls_service_b",
    kind: SpanKind::Client,
    start_time: consistency_check_time + 500L,
    end_time: Some(consistency_check_time + 1200L),
    status: StatusCode::Ok,
    attributes: [
      ("service.name", AttributeValue::string("service-a")),
      ("peer.service", AttributeValue::string("service-b")),
      ("rpc.system", AttributeValue::string("http")),
      ("rpc.method", AttributeValue::string("POST")),
      ("rpc.url", AttributeValue::string("http://service-b:8080/api/validate")),
      ("trace.propagation.format", AttributeValue::string("w3c-tracecontext")),
      ("trace.consistency.checked", AttributeValue::bool(true))
    ],
    events: [
      ("call_initiated", consistency_check_time + 600L, [
        ("trace.context.injected", AttributeValue::bool(true)),
        ("propagation.headers.count", AttributeValue::int(3L))
      ])
    ],
    links: []
  }
  
  // 3. 服务B中接收的Span
  let service_b_receive_span = Span::{
    trace_id: trace_id,
    span_id: "span_service_b_receive",
    parent_span_id: Some("span_service_a_to_b"),
    name: "service_b_receive_and_process",
    kind: SpanKind::Server,
    start_time: consistency_check_time + 800L,
    end_time: Some(consistency_check_time + 1500L),
    status: StatusCode::Ok,
    attributes: [
      ("service.name", AttributeValue::string("service-b")),
      ("service.version", AttributeValue::string("2.1.0")),
      ("operation.name", AttributeValue::string("validate_data")),
      ("peer.service", AttributeValue::string("service-a")),
      ("trace.context.extracted", AttributeValue::bool(true)),
      ("trace.id.match", AttributeValue::bool(true)),
      ("parent.span.id.match", AttributeValue::bool(true)),
      ("user.id", AttributeValue::string("user_456")), // 一致的用户ID
      ("request.id", AttributeValue::string("req_abc123")) // 一致的请求ID
    ],
    events: [
      ("context_extracted", consistency_check_time + 900L, [
        ("extraction.success", AttributeValue::bool(true)),
        ("trace.state.validated", AttributeValue::bool(true))
      ]),
      ("validation_completed", consistency_check_time + 1400L, [
        ("validation.result", AttributeValue::string("passed")),
        ("data.consistency.verified", AttributeValue::bool(true))
      ])
    ],
    links: []
  }
  
  // 4. 服务B调用服务C的子Span
  let service_b_to_c_span = Span::{
    trace_id: trace_id,
    span_id: "span_service_b_to_c",
    parent_span_id: Some("span_service_b_receive"),
    name: "service_b_calls_service_c",
    kind: SpanKind::Client,
    start_time: consistency_check_time + 1000L,
    end_time: Some(consistency_check_time + 1300L),
    status: StatusCode::Ok,
    attributes: [
      ("service.name", AttributeValue::string("service-b")),
      ("peer.service", AttributeValue::string("service-c")),
      ("rpc.system", AttributeValue::string("grpc")),
      ("rpc.service", AttributeValue::string("DataService")),
      ("rpc.method", AttributeValue::string("Store")),
      ("trace.propagation.format", AttributeValue::string("w3c-tracecontext")),
      ("trace.chain.length", AttributeValue::int(3L))
    ],
    events: [
      ("grpc_call_initiated", consistency_check_time + 1100L, [
        ("trace.context.propagated", AttributeValue::bool(true)),
        ("grpc.metadata.tracing", AttributeValue::bool(true))
      ])
    ],
    links: []
  }
  
  // 5. 服务C中的最终Span
  let service_c_final_span = Span::{
    trace_id: trace_id,
    span_id: "span_service_c_final",
    parent_span_id: Some("span_service_b_to_c"),
    name: "service_c_store_operation",
    kind: SpanKind::Server,
    start_time: consistency_check_time + 1150L,
    end_time: Some(consistency_check_time + 1600L),
    status: StatusCode::Ok,
    attributes: [
      ("service.name", AttributeValue::string("service-c")),
      ("service.version", AttributeValue::string("3.0.1")),
      ("operation.name", AttributeValue::string("store_data")),
      ("peer.service", AttributeValue::string("service-b")),
      ("trace.context.extracted", AttributeValue::bool(true)),
      ("trace.chain.complete", AttributeValue::bool(true)),
      ("user.id", AttributeValue::string("user_456")), // 一致的用户ID
      ("request.id", AttributeValue::string("req_abc123")), // 一致的请求ID
      ("data.stored", AttributeValue::bool(true)),
      ("consistency.checksum", AttributeValue::string("sha256:abc123def456"))
    ],
    events: [
      ("data_storage_completed", consistency_check_time + 1550L, [
        ("storage.location", AttributeValue::string("primary_db")),
        ("data.version", AttributeValue::int(1L)),
        ("consistency.verified", AttributeValue::bool(true))
      ])
    ],
    links: []
  }
  
  // 6. 记录分布式追踪一致性指标
  let consistency_metrics = [
    Measurement::{
      instrument: Instrument::counter("distributed_trace_consistency_checks_total", "Total distributed trace consistency checks", "count"),
      value: 4.0, // 4个一致性检查点
      attributes: [
        ("trace.id", AttributeValue::string(trace_id)),
        ("check.status", AttributeValue::string("passed")),
        ("services.involved", AttributeValue::int(3L))
      ],
      time: consistency_check_time + 1600L
    },
    Measurement::{
      instrument: Instrument::histogram("trace_propagation_latency_ms", "Trace propagation latency", "ms"),
      value: 300.0, // 从A到C的总传播延迟
      attributes: [
        ("source.service", AttributeValue::string("service-a")),
        ("destination.service", AttributeValue::string("service-c")),
        ("propagation.hops", AttributeValue::int(2L))
      ],
      time: consistency_check_time + 1600L
    },
    Measurement::{
      instrument: Instrument::gauge("trace_completeness_ratio", "Trace completeness ratio", "ratio"),
      value: 1.0, // 100%完整
      attributes: [
        ("trace.id", AttributeValue::string(trace_id)),
        ("expected.spans", AttributeValue::int(5L)),
        ("actual.spans", AttributeValue::int(5L))
      ],
      time: consistency_check_time + 1600L
    }
  ]
  
  // 7. 记录分布式追踪一致性日志
  let consistency_log = LogRecord::{
    timestamp: consistency_check_time + 1600L,
    observed_timestamp: Some(consistency_check_time + 1601L),
    severity: Some(Severity::Info),
    severity_text: Some("INFO"),
    body: Some("Distributed trace consistency verified across all services"),
    attributes: [
      ("trace_id", AttributeValue::string(trace_id)),
      ("services.involved", AttributeValue::array_string(["service-a", "service-b", "service-c"])),
      ("total.spans", AttributeValue::int(5L)),
      ("consistency.checks.passed", AttributeValue::int(4L)),
      ("trace.completeness", AttributeValue::string("100%")),
      ("data.integrity.verified", AttributeValue::bool(true))
    ],
    flags: 0,
    trace_id: Some(trace_id),
    span_id: Some("span_service_c_final"),
    trace_flags: Some(1)
  }
  
  // 8. 验证分布式追踪一致性的正确性
  assert_eq(service_a_root_span.status, StatusCode::Ok)
  assert_eq(service_a_to_b_span.status, StatusCode::Ok)
  assert_eq(service_b_receive_span.status, StatusCode::Ok)
  assert_eq(service_b_to_c_span.status, StatusCode::Ok)
  assert_eq(service_c_final_span.status, StatusCode::Ok)
  
  // 验证trace ID的一致性
  assert_eq(service_a_root_span.trace_id, trace_id)
  assert_eq(service_b_receive_span.trace_id, trace_id)
  assert_eq(service_c_final_span.trace_id, trace_id)
  
  // 验证父子关系的正确性
  assert_eq(service_a_to_b_span.parent_span_id, Some("span_service_a_root"))
  assert_eq(service_b_receive_span.parent_span_id, Some("span_service_a_to_b"))
  assert_eq(service_b_to_c_span.parent_span_id, Some("span_service_b_receive"))
  assert_eq(service_c_final_span.parent_span_id, Some("span_service_b_to_c"))
  
  // 验证用户ID在所有服务中的一致性
  let service_a_user = service_a_root_span.attributes[3]
  match service_a_user.1 {
    StringValue(user_id) => assert_eq(user_id, "user_456")
    _ => assert_eq(false, true)
  }
  
  let service_c_user = service_c_final_span.attributes[6]
  match service_c_user.1 {
    StringValue(user_id) => assert_eq(user_id, "user_456")
    _ => assert_eq(false, true)
  }
  
  // 验证请求ID的一致性
  let service_a_request = service_a_root_span.attributes[4]
  match service_a_request.1 {
    StringValue(request_id) => assert_eq(request_id, "req_abc123")
    _ => assert_eq(false, true)
  }
  
  let service_c_request = service_c_final_span.attributes[7]
  match service_c_request.1 {
    StringValue(request_id) => assert_eq(request_id, "req_abc123")
    _ => assert_eq(false, true)
  }
  
  // 验证时间线的正确性
  assert_eq(service_a_root_span.start_time <= service_a_to_b_span.start_time, true)
  assert_eq(service_a_to_b_span.start_time <= service_b_receive_span.start_time, true)
  assert_eq(service_b_receive_span.start_time <= service_b_to_c_span.start_time, true)
  assert_eq(service_b_to_c_span.start_time <= service_c_final_span.start_time, true)
}

test "metric_aggregation_consistency" {
  // 测试指标聚合的一致性
  
  let aggregation_time = 1641017000L
  let aggregation_window = 60000L // 1分钟聚合窗口
  
  // 1. 各服务产生的原始指标
  let service_a_metrics = [
    Measurement::{
      instrument: Instrument::counter("http_requests_total", "Total HTTP requests", "count"),
      value: 100.0,
      attributes: [
        ("service.name", AttributeValue::string("service-a")),
        ("http.method", AttributeValue::string("GET")),
        ("http.status_code", AttributeValue::int(200L)),
        ("user.region", AttributeValue::string("us-west-2"))
      ],
      time: aggregation_time - 50000L
    },
    Measurement::{
      instrument: Instrument::counter("http_requests_total", "Total HTTP requests", "count"),
      value: 50.0,
      attributes: [
        ("service.name", AttributeValue::string("service-a")),
        ("http.method", AttributeValue::string("POST")),
        ("http.status_code", AttributeValue::int(201L)),
        ("user.region", AttributeValue::string("us-west-2"))
      ],
      time: aggregation_time - 40000L
    }
  ]
  
  let service_b_metrics = [
    Measurement::{
      instrument: Instrument::counter("http_requests_total", "Total HTTP requests", "count"),
      value: 75.0,
      attributes: [
        ("service.name", AttributeValue::string("service-b")),
        ("http.method", AttributeValue::string("GET")),
        ("http.status_code", AttributeValue::int(200L)),
        ("user.region", AttributeValue::string("us-west-2"))
      ],
      time: aggregation_time - 45000L
    },
    Measurement::{
      instrument: Instrument::counter("http_requests_total", "Total HTTP requests", "count"),
      value: 25.0,
      attributes: [
        ("service.name", AttributeValue::string("service-b")),
        ("http.method", AttributeValue::string("PUT")),
        ("http.status_code", AttributeValue::int(200L)),
        ("user.region", AttributeValue::string("us-west-2"))
      ],
      time: aggregation_time - 35000L
    }
  ]
  
  let service_c_metrics = [
    Measurement::{
      instrument: Instrument::counter("http_requests_total", "Total HTTP requests", "count"),
      value: 60.0,
      attributes: [
        ("service.name", AttributeValue::string("service-c")),
        ("http.method", AttributeValue::string("GET")),
        ("http.status_code", AttributeValue::int(200L)),
        ("user.region", AttributeValue::string("us-west-2"))
      ],
      time: aggregation_time - 30000L
    }
  ]
  
  // 2. 聚合服务的一致性检查
  let aggregation_span = Span::{
    trace_id: "trace_metric_aggregation_456",
    span_id: "span_metric_aggregation",
    parent_span_id: None,
    name: "cross_service_metric_aggregation",
    kind: SpanKind::Internal,
    start_time: aggregation_time,
    end_time: Some(aggregation_time + 2000L),
    status: StatusCode::Ok,
    attributes: [
      ("aggregation.window.ms", AttributeValue::int(aggregation_window)),
      ("services.involved", AttributeValue::int(3L)),
      ("raw.measurements.count", AttributeValue::int(5L)),
      ("aggregation.status", AttributeValue::string("consistent")),
      ("data.integrity.verified", AttributeValue::bool(true))
    ],
    events: [
      ("metrics_collection_started", aggregation_time + 200L, [
        ("collection.source", AttributeValue::string("all_services")),
        ("expected.measurements", AttributeValue::int(5L))
      ]),
      ("consistency_check_passed", aggregation_time + 1000L, [
        ("checksum.verified", AttributeValue::bool(true)),
        ("duplicate.measurements", AttributeValue::int(0L)),
        ("missing.measurements", AttributeValue::int(0L))
      ]),
      ("aggregation_completed", aggregation_time + 1800L, [
        ("aggregated.metrics.count", AttributeValue::int(4L)),
        ("total.requests", AttributeValue::float(310.0))
      ])
    ],
    links: []
  }
  
  // 3. 聚合后的指标（预期结果）
  let aggregated_metrics = [
    Measurement::{
      instrument: Instrument::counter("http_requests_total", "Total HTTP requests", "count"),
      value: 235.0, // 100 + 75 + 60 (GET请求总和)
      attributes: [
        ("aggregated", AttributeValue::bool(true)),
        ("http.method", AttributeValue::string("GET")),
        ("http.status_code", AttributeValue::int(200L)),
        ("user.region", AttributeValue::string("us-west-2")),
        ("services.count", AttributeValue::int(3L))
      ],
      time: aggregation_time + 2000L
    },
    Measurement::{
      instrument: Instrument::counter("http_requests_total", "Total HTTP requests", "count"),
      value: 50.0, // POST请求
      attributes: [
        ("aggregated", AttributeValue::bool(true)),
        ("http.method", AttributeValue::string("POST")),
        ("http.status_code", AttributeValue::int(201L)),
        ("user.region", AttributeValue::string("us-west-2")),
        ("services.count", AttributeValue::int(1L))
      ],
      time: aggregation_time + 2000L
    },
    Measurement::{
      instrument: Instrument::counter("http_requests_total", "Total HTTP requests", "count"),
      value: 25.0, // PUT请求
      attributes: [
        ("aggregated", AttributeValue::bool(true)),
        ("http.method", AttributeValue::string("PUT")),
        ("http.status_code", AttributeValue::int(200L)),
        ("user.region", AttributeValue::string("us-west-2")),
        ("services.count", AttributeValue::int(1L))
      ],
      time: aggregation_time + 2000L
    },
    Measurement::{
      instrument: Instrument::gauge("request_rate_per_second", "Request rate per second", "rate"),
      value: 5.17, // 310 requests / 60 seconds
      attributes: [
        ("aggregated", AttributeValue::bool(true)),
        ("user.region", AttributeValue::string("us-west-2")),
        ("services.involved", AttributeValue::int(3L))
      ],
      time: aggregation_time + 2000L
    }
  ]
  
  // 4. 记录聚合一致性指标
  let consistency_metrics = [
    Measurement::{
      instrument: Instrument::counter("metric_aggregation_consistency_checks_total", "Total metric aggregation consistency checks", "count"),
      value: 1.0,
      attributes: [
        ("aggregation.status", AttributeValue::string("passed")),
        ("services.involved", AttributeValue::int(3L)),
        ("measurements.processed", AttributeValue::int(5L))
      ],
      time: aggregation_time + 2000L
    },
    Measurement::{
      instrument: Instrument::gauge("aggregation_data_quality_score", "Aggregation data quality score", "score"),
      value: 1.0, // 100%数据质量
      attributes: [
        ("quality.dimension", AttributeValue::string("completeness")),
        ("missing.data.percentage", AttributeValue::float(0.0))
      ],
      time: aggregation_time + 2000L
    }
  ]
  
  // 5. 记录聚合一致性日志
  let aggregation_log = LogRecord::{
    timestamp: aggregation_time + 2000L,
    observed_timestamp: Some(aggregation_time + 2001L),
    severity: Some(Severity::Info),
    severity_text: Some("INFO"),
    body: Some("Cross-service metric aggregation completed with data consistency verified"),
    attributes: [
      ("aggregation.window.ms", AttributeValue::int(aggregation_window)),
      ("services.involved", AttributeValue::array_string(["service-a", "service-b", "service-c"])),
      ("raw.measurements.count", AttributeValue::int(5L)),
      ("aggregated.metrics.count", AttributeValue::int(4L)),
      ("total.requests.aggregated", AttributeValue::float(310.0)),
      ("data.consistency.verified", AttributeValue::bool(true))
    ],
    flags: 0,
    trace_id: Some("trace_metric_aggregation_456"),
    span_id: Some("span_metric_aggregation"),
    trace_flags: Some(1)
  }
  
  // 6. 验证指标聚合一致性的正确性
  assert_eq(aggregation_span.status, StatusCode::Ok)
  
  // 验证聚合状态
  let aggregation_status = aggregation_span.attributes[4]
  match aggregation_status.1 {
    StringValue(status) => assert_eq(status, "consistent")
    _ => assert_eq(false, true)
  }
  
  // 验证数据完整性
  let data_integrity = aggregation_span.attributes[5]
  match data_integrity.1 {
    BoolValue(verified) => assert_eq(verified, true)
    _ => assert_eq(false, true)
  }
  
  // 验证聚合指标的计算正确性
  let get_requests_total = aggregated_metrics[0]
  match get_requests_total.value {
    FloatValue(total) => assert_eq(total, 235.0) // 100 + 75 + 60
    _ => assert_eq(false, true)
  }
  
  let request_rate = aggregated_metrics[3]
  match request_rate.value {
    FloatValue(rate) => assert_eq(rate > 5.0 && rate < 5.2, true) // 310/60 ≈ 5.17
    _ => assert_eq(false, true)
  }
  
  // 验证聚合标记
  let aggregated_flag = aggregated_metrics[0].attributes[0]
  match aggregated_flag.1 {
    BoolValue(aggregated) => assert_eq(aggregated, true)
    _ => assert_eq(false, true)
  }
}

test "log_streaming_consistency" {
  // 测试日志流的一致性
  
  let log_stream_time = 1641018000L
  let stream_id = "stream_consistency_789"
  
  // 1. 服务A产生的日志流
  let service_a_logs = [
    LogRecord::{
      timestamp: log_stream_time - 5000L,
      observed_timestamp: Some(log_stream_time - 4999L),
      severity: Some(Severity::Info),
      severity_text: Some("INFO"),
      body: Some("Request processing started in service A"),
      attributes: [
        ("service.name", AttributeValue::string("service-a")),
        ("request.id", AttributeValue::string("req_xyz789")),
        ("user.id", AttributeValue::string("user_123")),
        ("stream.id", AttributeValue::string(stream_id)),
        ("sequence.number", AttributeValue::int(1L))
      ],
      flags: 0,
      trace_id: Some("trace_log_stream_123"),
      span_id: Some("span_service_a_1"),
      trace_flags: Some(1)
    },
    LogRecord::{
      timestamp: log_stream_time - 3000L,
      observed_timestamp: Some(log_stream_time - 2999L),
      severity: Some(Severity::Info),
      severity_text: Some("INFO"),
      body: Some("Service A calling service B"),
      attributes: [
        ("service.name", AttributeValue::string("service-a")),
        ("request.id", AttributeValue::string("req_xyz789")),
        ("user.id", AttributeValue::string("user_123")),
        ("stream.id", AttributeValue::string(stream_id)),
        ("sequence.number", AttributeValue::int(2L)),
        ("target.service", AttributeValue::string("service-b"))
      ],
      flags: 0,
      trace_id: Some("trace_log_stream_123"),
      span_id: Some("span_service_a_2"),
      trace_flags: Some(1)
    }
  ]
  
  // 2. 服务B产生的日志流
  let service_b_logs = [
    LogRecord::{
      timestamp: log_stream_time - 2500L,
      observed_timestamp: Some(log_stream_time - 2499L),
      severity: Some(Severity::Info),
      severity_text: Some("INFO"),
      body: Some("Request received in service B from service A"),
      attributes: [
        ("service.name", AttributeValue::string("service-b")),
        ("request.id", AttributeValue::string("req_xyz789")),
        ("user.id", AttributeValue::string("user_123")),
        ("stream.id", AttributeValue::string(stream_id)),
        ("sequence.number", AttributeValue::int(3L)),
        ("source.service", AttributeValue::string("service-a"))
      ],
      flags: 0,
      trace_id: Some("trace_log_stream_123"),
      span_id: Some("span_service_b_1"),
      trace_flags: Some(1)
    },
    LogRecord::{
      timestamp: log_stream_time - 1000L,
      observed_timestamp: Some(log_stream_time - 999L),
      severity: Some(Severity::Warn),
      severity_text: Some("WARN"),
      body: Some("Service B processing warning"),
      attributes: [
        ("service.name", AttributeValue::string("service-b")),
        ("request.id", AttributeValue::string("req_xyz789")),
        ("user.id", AttributeValue::string("user_123")),
        ("stream.id", AttributeValue::string(stream_id)),
        ("sequence.number", AttributeValue::int(4L)),
        ("warning.type", AttributeValue::string("performance_degradation"))
      ],
      flags: 0,
      trace_id: Some("trace_log_stream_123"),
      span_id: Some("span_service_b_2"),
      trace_flags: Some(1)
    }
  ]
  
  // 3. 服务C产生的日志流
  let service_c_logs = [
    LogRecord::{
      timestamp: log_stream_time - 500L,
      observed_timestamp: Some(log_stream_time - 499L),
      severity: Some(Severity::Info),
      severity_text: Some("INFO"),
      body: Some("Service C completing the request flow"),
      attributes: [
        ("service.name", AttributeValue::string("service-c")),
        ("request.id", AttributeValue::string("req_xyz789")),
        ("user.id", AttributeValue::string("user_123")),
        ("stream.id", AttributeValue::string(stream_id)),
        ("sequence.number", AttributeValue::int(5L)),
        ("flow.status", AttributeValue::string("completed"))
      ],
      flags: 0,
      trace_id: Some("trace_log_stream_123"),
      span_id: Some("span_service_c_1"),
      trace_flags: Some(1)
    }
  ]
  
  // 4. 日志流一致性检查
  let stream_consistency_span = Span::{
    trace_id: "trace_log_stream_consistency_456",
    span_id: "span_stream_consistency_check",
    parent_span_id: None,
    name: "log_stream_consistency_verification",
    kind: SpanKind::Internal,
    start_time: log_stream_time,
    end_time: Some(log_stream_time + 1500L),
    status: StatusCode::Ok,
    attributes: [
      ("stream.id", AttributeValue::string(stream_id)),
      ("total.logs.count", AttributeValue::int(5L)),
      ("services.involved", AttributeValue::int(3L)),
      ("consistency.status", AttributeValue::string("verified")),
      ("sequence.intact", AttributeValue::bool(true)),
      ("correlation.complete", AttributeValue::bool(true))
    ],
    events: [
      ("stream_analysis_started", log_stream_time + 200L, [
        ("analysis.type", AttributeValue::string("consistency_check")),
        ("expected.sequence", AttributeValue::string("1-5"))
      ]),
      ("correlation_verified", log_stream_time + 800L, [
        ("request.id.consistent", AttributeValue::bool(true)),
        ("user.id.consistent", AttributeValue::bool(true)),
        ("stream.id.consistent", AttributeValue::bool(true))
      ]),
      ("sequence_validated", log_stream_time + 1200L, [
        ("sequence.complete", AttributeValue::bool(true)),
        ("missing.sequence.numbers", AttributeValue::int(0L)),
        ("duplicate.sequence.numbers", AttributeValue::int(0L))
      ])
    ],
    links: []
  }
  
  // 5. 记录日志流一致性指标
  let stream_metrics = [
    Measurement::{
      instrument: Instrument::counter("log_stream_consistency_checks_total", "Total log stream consistency checks", "count"),
      value: 1.0,
      attributes: [
        ("stream.id", AttributeValue::string(stream_id)),
        ("check.status", AttributeValue::string("passed")),
        ("logs.analyzed", AttributeValue::int(5L))
      ],
      time: log_stream_time + 1500L
    },
    Measurement::{
      instrument: Instrument::gauge("log_stream_completeness_ratio", "Log stream completeness ratio", "ratio"),
      value: 1.0, // 100%完整
      attributes: [
        ("stream.id", AttributeValue::string(stream_id)),
        ("expected.logs", AttributeValue::int(5L)),
        ("found.logs", AttributeValue::int(5L))
      ],
      time: log_stream_time + 1500L
    },
    Measurement::{
      instrument: Instrument::gauge("log_stream_correlation_score", "Log stream correlation score", "score"),
      value: 1.0, // 100%关联
      attributes: [
        ("correlation.type", AttributeValue::string("request_id")),
        ("correlation.success", AttributeValue::bool(true))
      ],
      time: log_stream_time + 1500L
    }
  ]
  
  // 6. 记录日志流一致性日志
  let stream_consistency_log = LogRecord::{
    timestamp: log_stream_time + 1500L,
    observed_timestamp: Some(log_stream_time + 1501L),
    severity: Some(Severity::Info),
    severity_text: Some("INFO"),
    body: Some("Log stream consistency verification completed successfully"),
    attributes: [
      ("stream.id", AttributeValue::string(stream_id)),
      ("services.involved", AttributeValue::array_string(["service-a", "service-b", "service-c"])),
      ("total.logs.analyzed", AttributeValue::int(5L)),
      ("sequence.integrity", AttributeValue::string("verified")),
      ("request.correlation", AttributeValue::string("perfect")),
      ("user.context.consistent", AttributeValue::bool(true))
    ],
    flags: 0,
    trace_id: Some("trace_log_stream_consistency_456"),
    span_id: Some("span_stream_consistency_check"),
    trace_flags: Some(1)
  }
  
  // 7. 验证日志流一致性的正确性
  assert_eq(stream_consistency_span.status, StatusCode::Ok)
  
  // 验证一致性状态
  let consistency_status = stream_consistency_span.attributes[3]
  match consistency_status.1 {
    StringValue(status) => assert_eq(status, "verified")
    _ => assert_eq(false, true)
  }
  
  // 验证序列完整性
  let sequence_intact = stream_consistency_span.attributes[4]
  match sequence_intact.1 {
    BoolValue(intact) => assert_eq(intact, true)
    _ => assert_eq(false, true)
  }
  
  // 验证关联完整性
  let correlation_complete = stream_consistency_span.attributes[5]
  match correlation_complete.1 {
    BoolValue(complete) => assert_eq(complete, true)
    _ => assert_eq(false, true)
  }
  
  // 验证所有日志中的stream ID一致性
  let service_a_stream = service_a_logs[0].attributes[3]
  match service_a_stream.1 {
    StringValue(stream) => assert_eq(stream, stream_id)
    _ => assert_eq(false, true)
  }
  
  let service_c_stream = service_c_logs[0].attributes[3]
  match service_c_stream.1 {
    StringValue(stream) => assert_eq(stream, stream_id)
    _ => assert_eq(false, true)
  }
  
  // 验证序列号的连续性
  let service_a_seq1 = service_a_logs[0].attributes[4]
  match service_a_seq1.1 {
    IntValue(seq) => assert_eq(seq, 1L)
    _ => assert_eq(false, true)
  }
  
  let service_c_seq = service_c_logs[0].attributes[4]
  match service_c_seq.1 {
    IntValue(seq) => assert_eq(seq, 5L)
    _ => assert_eq(false, true)
  }
  
  // 验证请求ID在所有日志中的一致性
  let service_a_request = service_a_logs[0].attributes[1]
  match service_a_request.1 {
    StringValue(req_id) => assert_eq(req_id, "req_xyz789")
    _ => assert_eq(false, true)
  }
  
  let service_c_request = service_c_logs[0].attributes[1]
  match service_c_request.1 {
    StringValue(req_id) => assert_eq(req_id, "req_xyz789")
    _ => assert_eq(false, true)
  }
}

test "event_sourcing_consistency" {
  // 测试事件溯源的一致性
  
  let event_sourcing_time = 1641019000L
  let event_stream_id = "event_stream_consistency_999"
  
  // 1. 事件流中的事件序列
  let event_stream = [
    // 事件1: 用户创建
    LogRecord::{
      timestamp: event_sourcing_time - 4000L,
      observed_timestamp: Some(event_sourcing_time - 3999L),
      severity: Some(Severity::Info),
      severity_text: Some("INFO"),
      body: Some("UserCreated event"),
      attributes: [
        ("event.type", AttributeValue::string("UserCreated")),
        ("event.id", AttributeValue::string("event_user_created_123")),
        ("event.stream.id", AttributeValue::string(event_stream_id)),
        ("event.sequence", AttributeValue::int(1L)),
        ("aggregate.id", AttributeValue::string("user_456")),
        ("aggregate.type", AttributeValue::string("User")),
        ("event.version", AttributeValue::int(1L)),
        ("causation.id", AttributeValue::string("command_create_user_123"))
      ],
      flags: 0,
      trace_id: Some("trace_event_sourcing_123"),
      span_id: Some("span_user_creation"),
      trace_flags: Some(1)
    },
    // 事件2: 用户资料更新
    LogRecord::{
      timestamp: event_sourcing_time - 3000L,
      observed_timestamp: Some(event_sourcing_time - 2999L),
      severity: Some(Severity::Info),
      severity_text: Some("INFO"),
      body: Some("UserProfileUpdated event"),
      attributes: [
        ("event.type", AttributeValue::string("UserProfileUpdated")),
        ("event.id", AttributeValue::string("event_profile_updated_456")),
        ("event.stream.id", AttributeValue::string(event_stream_id)),
        ("event.sequence", AttributeValue::int(2L)),
        ("aggregate.id", AttributeValue::string("user_456")),
        ("aggregate.type", AttributeValue::string("User")),
        ("event.version", AttributeValue::int(1L)),
        ("causation.id", AttributeValue::string("command_update_profile_456")),
        ("correlation.id", AttributeValue::string("event_user_created_123"))
      ],
      flags: 0,
      trace_id: Some("trace_event_sourcing_123"),
      span_id: Some("span_profile_update"),
      trace_flags: Some(1)
    },
    // 事件3: 用户订单创建
    LogRecord::{
      timestamp: event_sourcing_time - 2000L,
      observed_timestamp: Some(event_sourcing_time - 1999L),
      severity: Some(Severity::Info),
      severity_text: Some("INFO"),
      body: Some("OrderCreated event"),
      attributes: [
        ("event.type", AttributeValue::string("OrderCreated")),
        ("event.id", AttributeValue::string("event_order_created_789")),
        ("event.stream.id", AttributeValue::string(event_stream_id)),
        ("event.sequence", AttributeValue::int(3L)),
        ("aggregate.id", AttributeValue::string("order_789")),
        ("aggregate.type", AttributeValue::string("Order")),
        ("event.version", AttributeValue::int(1L)),
        ("causation.id", AttributeValue::string("command_create_order_789")),
        ("correlation.id", AttributeValue::string("event_profile_updated_456")),
        ("user.id", AttributeValue::string("user_456"))
      ],
      flags: 0,
      trace_id: Some("trace_event_sourcing_123"),
      span_id: Some("span_order_creation"),
      trace_flags: Some(1)
    },
    // 事件4: 订单支付处理
    LogRecord::{
      timestamp: event_sourcing_time - 1000L,
      observed_timestamp: Some(event_sourcing_time - 999L),
      severity: Some(Severity::Info),
      severity_text: Some("INFO"),
      body: Some("OrderPaymentProcessed event"),
      attributes: [
        ("event.type", AttributeValue::string("OrderPaymentProcessed")),
        ("event.id", AttributeValue::string("event_payment_processed_012")),
        ("event.stream.id", AttributeValue::string(event_stream_id)),
        ("event.sequence", AttributeValue::int(4L)),
        ("aggregate.id", AttributeValue::string("order_789")),
        ("aggregate.type", AttributeValue::string("Order")),
        ("event.version", AttributeValue::int(1L)),
        ("causation.id", AttributeValue::string("command_process_payment_012")),
        ("correlation.id", AttributeValue::string("event_order_created_789")),
        ("payment.id", AttributeValue::string("payment_345"))
      ],
      flags: 0,
      trace_id: Some("trace_event_sourcing_123"),
      span_id: Some("span_payment_processing"),
      trace_flags: Some(1)
    }
  ]
  
  // 2. 事件溯源一致性检查
  let event_consistency_span = Span::{
    trace_id: "trace_event_consistency_456",
    span_id: "span_event_consistency_check",
    parent_span_id: None,
    name: "event_sourcing_consistency_verification",
    kind: SpanKind::Internal,
    start_time: event_sourcing_time,
    end_time: Some(event_sourcing_time + 2000L),
    status: StatusCode::Ok,
    attributes: [
      ("event.stream.id", AttributeValue::string(event_stream_id)),
      ("total.events.count", AttributeValue::int(4L)),
      ("aggregates.involved", AttributeValue::int(2L)), // User and Order aggregates
      ("consistency.status", AttributeValue::string("verified")),
      ("causality.chain.intact", AttributeValue::bool(true)),
      ("event.sequence.valid", AttributeValue::bool(true)),
      ("snapshot.consistent", AttributeValue::bool(true))
    ],
    events: [
      ("event_stream_analysis_started", event_sourcing_time + 200L, [
        ("analysis.scope", AttributeValue::string("full_stream")),
        ("expected.events", AttributeValue::int(4L))
      ]),
      ("causality_chain_verified", event_sourcing_time + 800L, [
        ("chain.length", AttributeValue::int(4L)),
        ("broken.links", AttributeValue::int(0L)),
        ("circular.dependencies", AttributeValue::int(0L))
      ]),
      ("aggregate_state_reconstructed", event_sourcing_time + 1400L, [
        ("user.aggregate.version", AttributeValue::int(2L)),
        ("order.aggregate.version", AttributeValue::int(2L)),
        ("state.reconstruction.success", AttributeValue::bool(true))
      ]),
      ("snapshot_consistency_checked", event_sourcing_time + 1800L, [
        ("snapshot.version", AttributeValue::int(1L)),
        ("snapshot.matches.events", AttributeValue::bool(true))
      ])
    ],
    links: []
  }
  
  // 3. 记录事件溯源一致性指标
  let event_metrics = [
    Measurement::{
      instrument: Instrument::counter("event_sourcing_consistency_checks_total", "Total event sourcing consistency checks", "count"),
      value: 1.0,
      attributes: [
        ("event.stream.id", AttributeValue::string(event_stream_id)),
        ("check.status", AttributeValue::string("passed")),
        ("events.analyzed", AttributeValue::int(4L))
      ],
      time: event_sourcing_time + 2000L
    },
    Measurement::{
      instrument: Instrument::gauge("event_stream_integrity_score", "Event stream integrity score", "score"),
      value: 1.0, // 100%完整性
      attributes: [
        ("event.stream.id", AttributeValue::string(event_stream_id)),
        ("integrity.dimension", AttributeValue::string("sequence")),
        ("missing.events", AttributeValue::int(0L))
      ],
      time: event_sourcing_time + 2000L
    },
    Measurement::{
      instrument: Instrument::gauge("causality_chain_strength", "Causality chain strength", "strength"),
      value: 1.0, // 100%强因果关系
      attributes: [
        ("event.stream.id", AttributeValue::string(event_stream_id)),
        ("chain.length", AttributeValue::int(4L)),
        ("broken.links", AttributeValue::int(0L))
      ],
      time: event_sourcing_time + 2000L
    }
  ]
  
  // 4. 记录事件溯源一致性日志
  let event_consistency_log = LogRecord::{
    timestamp: event_sourcing_time + 2000L,
    observed_timestamp: Some(event_sourcing_time + 2001L),
    severity: Some(Severity::Info),
    severity_text: Some("INFO"),
    body: Some("Event sourcing consistency verification completed successfully"),
    attributes: [
      ("event.stream.id", AttributeValue::string(event_stream_id)),
      ("total.events.analyzed", AttributeValue::int(4L)),
      ("aggregates.involved", AttributeValue::array_string(["User", "Order"])),
      ("causality.chain.verified", AttributeValue::bool(true)),
      ("event.sequence.intact", AttributeValue::bool(true)),
      ("aggregate.state.reconstructed", AttributeValue::bool(true)),
      ("snapshot.consistency.verified", AttributeValue::bool(true))
    ],
    flags: 0,
    trace_id: Some("trace_event_consistency_456"),
    span_id: Some("span_event_consistency_check"),
    trace_flags: Some(1)
  }
  
  // 5. 验证事件溯源一致性的正确性
  assert_eq(event_consistency_span.status, StatusCode::Ok)
  
  // 验证一致性状态
  let consistency_status = event_consistency_span.attributes[3]
  match consistency_status.1 {
    StringValue(status) => assert_eq(status, "verified")
    _ => assert_eq(false, true)
  }
  
  // 验证因果链完整性
  let causality_intact = event_consistency_span.attributes[4]
  match causality_intact.1 {
    BoolValue(intact) => assert_eq(intact, true)
    _ => assert_eq(false, true)
  }
  
  // 验证事件序列有效性
  let sequence_valid = event_consistency_span.attributes[5]
  match sequence_valid.1 {
    BoolValue(valid) => assert_eq(valid, true)
    _ => assert_eq(false, true)
  }
  
  // 验证事件流ID的一致性
  let first_event_stream = event_stream[0].attributes[2]
  match first_event_stream.1 {
    StringValue(stream) => assert_eq(stream, event_stream_id)
    _ => assert_eq(false, true)
  }
  
  let last_event_stream = event_stream[3].attributes[2]
  match last_event_stream.1 {
    StringValue(stream) => assert_eq(stream, event_stream_id)
    _ => assert_eq(false, true)
  }
  
  // 验证事件序列的连续性
  let event1_seq = event_stream[0].attributes[3]
  match event1_seq.1 {
    IntValue(seq) => assert_eq(seq, 1L)
    _ => assert_eq(false, true)
  }
  
  let event4_seq = event_stream[3].attributes[3]
  match event4_seq.1 {
    IntValue(seq) => assert_eq(seq, 4L)
    _ => assert_eq(false, true)
  }
  
  // 验证因果关系的正确性
  let event2_correlation = event_stream[1].attributes[8]
  match event2_correlation.1 {
    StringValue(correlation) => assert_eq(correlation, "event_user_created_123")
    _ => assert_eq(false, true)
  }
  
  let event3_correlation = event_stream[2].attributes[8]
  match event3_correlation.1 {
    StringValue(correlation) => assert_eq(correlation, "event_profile_updated_456")
    _ => assert_eq(false, true)
  }
  
  let event4_correlation = event_stream[3].attributes[8]
  match event4_correlation.1 {
    StringValue(correlation) => assert_eq(correlation, "event_order_created_789")
    _ => assert_eq(false, true)
  }
}
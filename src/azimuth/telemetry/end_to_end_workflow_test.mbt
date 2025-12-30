// 端到端工作流测试
// 测试完整的遥测数据生命周期和工作流

test "complete telemetry lifecycle" {
  // 测试完整的遥测数据生命周期
  let workflow_start_time = 1640995200000000000L
  
  // 1. 初始化阶段
  let resource = common::Resource::{
    service_name: "e2e-test-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", common::AttributeValue::string("test")),
      ("deployment.region", common::AttributeValue::string("us-west-2"))
    ]
  }
  
  let instrumentation_scope = common::InstrumentationScope::{
    name: "e2e-test-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  // 2. 创建根Span
  let root_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(i) { (i + 1) |> to_int64 |> to_byte })
  let root_span_id = [for i = 0; i < 8; i = i + 1].map(fn(i) { (i + 1) |> to_int64 |> to_byte })
  
  let root_span = trace::Span::{
    name: "e2e-workflow-root",
    context: trace::SpanContext::{
      trace_id: root_trace_id,
      span_id: root_span_id,
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: workflow_start_time,
    end_time_unix_nanos: None,  // 活跃状态
    status: trace::Unset,
    status_description: None,
    attributes: [
      ("workflow.type", common::AttributeValue::string("e2e-test")),
      ("service.name", common::AttributeValue::string(resource.service_name))
    ],
    events: [],
    links: []
  }
  
  // 3. 创建子Span - 数据处理
  let processing_start = workflow_start_time + 1000000L
  let child_span_id = [for i = 0; i < 8; i = i + 1].map(fn(i) { (i + 10) |> to_int64 |> to_byte })
  
  let processing_span = trace::Span::{
    name: "data-processing",
    context: trace::SpanContext::{
      trace_id: root_trace_id,
      span_id: child_span_id,
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: Some(root_span_id),
    start_time_unix_nanos: processing_start,
    end_time_unix_nanos: Some(processing_start + 5000000L),
    status: trace::Ok,
    status_description: Some("Data processing completed successfully"),
    attributes: [
      ("processing.type", common::AttributeValue::string("batch")),
      ("record.count", common::AttributeValue::int(1000L))
    ],
    events: [
      trace::SpanEvent::{
        name: "processing-started",
        timestamp_unix_nanos: processing_start + 500000L,
        attributes: [
          ("batch.id", common::AttributeValue::string("batch-001"))
        ]
      },
      trace::SpanEvent::{
        name: "processing-completed",
        timestamp_unix_nanos: processing_start + 4500000L,
        attributes: [
          ("records.processed", common::AttributeValue::int(1000L)),
          ("processing.duration", common::AttributeValue::int(4000000L))
        ]
      }
    ],
    links: []
  }
  
  // 4. 创建关联的Metrics
  let processing_metrics = [
    metrics::Measurement::{ 
      value: 1000.0, 
      attributes: [
        ("operation.type", common::AttributeValue::string("processing")),
        ("status", common::AttributeValue::string("success"))
      ]
    },
    metrics::Measurement::{ 
      value: 4.0, 
      attributes: [
        ("metric.type", common::AttributeValue::string("processing.duration.seconds")),
        ("operation", common::AttributeValue::string("data-processing"))
      ]
    }
  ]
  
  // 5. 创建关联的Logs
  let processing_logs = [
    logs::LogRecord::{
      timestamp_unix_nanos: processing_start + 1000000L,
      observed_timestamp_unix_nanos: Some(processing_start + 1100000L),
      severity_number: logs::Info,
      severity_text: Some("INFO"),
      body: Some("Started processing batch data"),
      attributes: [
        ("batch.size", common::AttributeValue::int(1000L)),
        ("operation.id", common::AttributeValue::string("op-001"))
      ],
      trace_id: Some(root_trace_id),
      span_id: Some(child_span_id),
      trace_flags: Some(1_byte),
      resource: Some(resource),
      instrumentation_scope: Some(instrumentation_scope)
    },
    logs::LogRecord::{
      timestamp_unix_nanos: processing_start + 4000000L,
      observed_timestamp_unix_nanos: Some(processing_start + 4100000L),
      severity_number: logs::Info,
      severity_text: Some("INFO"),
      body: Some("Completed batch processing successfully"),
      attributes: [
        ("records.processed", common::AttributeValue::int(1000L)),
        ("success.rate", common::AttributeValue::float(1.0))
      ],
      trace_id: Some(root_trace_id),
      span_id: Some(child_span_id),
      trace_flags: Some(1_byte),
      resource: Some(resource),
      instrumentation_scope: Some(instrumentation_scope)
    }
  ]
  
  // 6. 完成根Span
  let workflow_end_time = processing_start + 6000000L
  let completed_root_span = trace::Span::{
    ..root_span,
    end_time_unix_nanos: Some(workflow_end_time),
    status: trace::Ok,
    status_description: Some("E2E workflow completed successfully"),
    events: [
      trace::SpanEvent::{
        name: "workflow-completed",
        timestamp_unix_nanos: workflow_end_time - 100000L,
        attributes: [
          ("total.duration", common::AttributeValue::int(workflow_end_time - workflow_start_time)),
          ("child.spans", common::AttributeValue::int(1L))
        ]
      }
    ]
  }
  
  // 验证完整工作流
  assert root_span.context.trace_id == processing_span.context.trace_id
  assert processing_span.parent_span_id? == root_span.context.span_id
  assert processing_logs.length == 2
  assert processing_metrics.length == 2
  assert processing_span.events.length == 2
  assert completed_root_span.end_time_unix_nanos? > root_span.start_time_unix_nanos
}

test "cross-service distributed tracing" {
  // 测试跨服务分布式追踪
  let service_a_start = 1640995200000000000L
  
  // Service A - 入口点
  let service_a_trace_id = [1_byte; 16]
  let service_a_span_id = [1_byte; 8]
  
  let service_a_span = trace::Span::{
    name: "service-a-entrypoint",
    context: trace::SpanContext::{
      trace_id: service_a_trace_id,
      span_id: service_a_span_id,
      trace_flags: 1_byte,
      trace_state: "",
    },
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: service_a_start,
    end_time_unix_nanos: Some(service_a_start + 3000000L),
    status: trace::Ok,
    status_description: Some("Service A processing completed"),
    attributes: [
      ("service.name", common::AttributeValue::string("service-a")),
      ("http.method", common::AttributeValue::string("GET")),
      ("http.route", common::AttributeValue::string("/api/process"))
    ],
    events: [],
    links: []
  }
  
  // Service B - 被调用服务
  let service_b_start = service_a_start + 1000000L
  let service_b_span_id = [2_byte; 8]
  
  let service_b_span = trace::Span::{
    name: "service-b-processing",
    context: trace::SpanContext::{
      trace_id: service_a_trace_id,  // 同一个trace
      span_id: service_b_span_id,
      trace_flags: 1_byte,
      trace_state: "",
    },
    kind: trace::Server,
    parent_span_id: Some(service_a_span_id),  // 父Span是Service A的Span
    start_time_unix_nanos: service_b_start,
    end_time_unix_nanos: Some(service_b_start + 1500000L),
    status: trace::Ok,
    status_description: Some("Service B processing completed"),
    attributes: [
      ("service.name", common::AttributeValue::string("service-b")),
      ("rpc.service", common::AttributeValue::string("service-b.Method")),
      ("rpc.method", common::AttributeValue::string("ProcessData"))
    ],
    events: [
      trace::SpanEvent::{
        name: "database-query",
        timestamp_unix_nanos: service_b_start + 500000L,
        attributes: [
          ("db.statement", common::AttributeValue::string("SELECT * FROM data")),
          ("db.duration", common::AttributeValue::int(800000L))
        ]
      }
    ],
    links: []
  }
  
  // Service C - 另一个被调用服务
  let service_c_start = service_a_start + 2000000L
  let service_c_span_id = [3_byte; 8]
  
  let service_c_span = trace::Span::{
    name: "service-c-validation",
    context: trace::SpanContext::{
      trace_id: service_a_trace_id,  // 同一个trace
      span_id: service_c_span_id,
      trace_flags: 1_byte,
      trace_state: "",
    },
    kind: trace::Client,
    parent_span_id: Some(service_a_span_id),  // 父Span是Service A的Span
    start_time_unix_nanos: service_c_start,
    end_time_unix_nanos: Some(service_c_start + 800000L),
    status: trace::Ok,
    status_description: Some("Service C validation completed"),
    attributes: [
      ("service.name", common::AttributeValue::string("service-c")),
      ("validation.type", common::AttributeValue::string("schema")),
      ("validation.result", common::AttributeValue::string("valid"))
    ],
    events: [],
    links: []
  }
  
  // 验证分布式追踪的完整性
  assert service_a_span.context.trace_id == service_b_span.context.trace_id
  assert service_a_span.context.trace_id == service_c_span.context.trace_id
  assert service_b_span.parent_span_id? == service_a_span.context.span_id
  assert service_c_span.parent_span_id? == service_a_span.context.span_id
  
  // 验证时间顺序
  assert service_a_span.start_time_unix_nanos <= service_b_span.start_time_unix_nanos
  assert service_a_span.start_time_unix_nanos <= service_c_span.start_time_unix_nanos
  assert service_b_span.end_time_unix_nanos? <= service_a_span.end_time_unix_nanos?
  assert service_c_span.end_time_unix_nanos? <= service_a_span.end_time_unix_nanos?
}

test "telemetry data pipeline" {
  // 测试遥测数据管道
  let pipeline_start = 1640995200000000000L
  
  // 1. 数据生成阶段
  let generation_span = trace::Span::{
    name: "telemetry-generation",
    context: trace::SpanContext::{
      trace_id: [10_byte; 16],
      span_id: [10_byte; 8],
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: pipeline_start,
    end_time_unix_nanos: Some(pipeline_start + 2000000L),
    status: trace::Ok,
    status_description: Some("Telemetry data generated"),
    attributes: [
      ("data.type", common::AttributeValue::string("mixed-signals")),
      ("generation.source", common::AttributeValue::string("application"))
    ],
    events: [],
    links: []
  }
  
  // 2. 数据收集阶段
  let collection_start = pipeline_start + 2500000L
  let collection_span = trace::Span::{
    name: "telemetry-collection",
    context: trace::SpanContext::{
      trace_id: [10_byte; 16],
      span_id: [11_byte; 8],
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: Some([10_byte; 8]),
    start_time_unix_nanos: collection_start,
    end_time_unix_nanos: Some(collection_start + 1500000L),
    status: trace::Ok,
    status_description: Some("Telemetry data collected"),
    attributes: [
      ("batch.size", common::AttributeValue::int(500L)),
      ("collection.strategy", common::AttributeValue::string("periodic"))
    ],
    events: [
      trace::SpanEvent::{
        name: "batch-formed",
        timestamp_unix_nanos: collection_start + 500000L,
        attributes: [
          ("spans.count", common::AttributeValue::int(200L)),
          ("metrics.count", common::AttributeValue::int(150L)),
          ("logs.count", common::AttributeValue::int(150L))
        ]
      }
    ],
    links: []
  }
  
  // 3. 数据处理阶段
  let processing_start = collection_start + 2000000L
  let processing_span = trace::Span::{
    name: "telemetry-processing",
    context: trace::SpanContext::{
      trace_id: [10_byte; 16],
      span_id: [12_byte; 8],
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: Some([10_byte; 8]),
    start_time_unix_nanos: processing_start,
    end_time_unix_nanos: Some(processing_start + 3000000L),
    status: trace::Ok,
    status_description: Some("Telemetry data processed"),
    attributes: [
      ("processing.type", common::AttributeValue::string("transformation")),
      ("enrichment.enabled", common::AttributeValue::bool(true))
    ],
    events: [
      trace::SpanEvent::{
        name: "attribute-enrichment",
        timestamp_unix_nanos: processing_start + 1000000L,
        attributes: [
          ("enriched.fields", common::AttributeValue::int(25L)),
          ("enrichment.source", common::AttributeValue::string("resource.metadata"))
        ]
      },
      trace::SpanEvent::{
        name: "data-transformation",
        timestamp_unix_nanos: processing_start + 2000000L,
        attributes: [
          ("transformation.type", common::AttributeValue::string("format-conversion")),
          ("records.transformed", common::AttributeValue::int(500L))
        ]
      }
    ],
    links: []
  }
  
  // 4. 数据导出阶段
  let export_start = processing_start + 3500000L
  let export_span = trace::Span::{
    name: "telemetry-export",
    context: trace::SpanContext::{
      trace_id: [10_byte; 16],
      span_id: [13_byte; 8],
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: Some([10_byte; 8]),
    start_time_unix_nanos: export_start,
    end_time_unix_nanos: Some(export_start + 4000000L),
    status: trace::Ok,
    status_description: Some("Telemetry data exported"),
    attributes: [
      ("export.protocol", common::AttributeValue::string("http")),
      ("export.endpoint", common::AttributeValue::string("http://otel-collector:4317")),
      ("export.format", common::AttributeValue::string("otlp"))
    ],
    events: [
      trace::SpanEvent::{
        name: "connection-established",
        timestamp_unix_nanos: export_start + 500000L,
        attributes: [
          ("connection.type", common::AttributeValue::string("persistent")),
          ("connection.duration", common::AttributeValue::int(200000L))
        ]
      },
      trace::SpanEvent::{
        name: "data-sent",
        timestamp_unix_nanos: export_start + 2000000L,
        attributes: [
          ("bytes.sent", common::AttributeValue::int(1024000L)),
          ("compression.ratio", common::AttributeValue::float(0.7))
        ]
      },
      trace::SpanEvent::{
        name: "acknowledgment-received",
        timestamp_unix_nanos: export_start + 3500000L,
        attributes: [
          ("ack.status", common::AttributeValue::string("success")),
          ("processing.time", common::AttributeValue::int(1500000L))
        ]
      }
    ],
    links: []
  }
  
  // 验证管道完整性
  assert generation_span.context.trace_id == collection_span.context.trace_id
  assert collection_span.context.trace_id == processing_span.context.trace_id
  assert processing_span.context.trace_id == export_span.context.trace_id
  
  // 验证管道阶段顺序
  assert generation_span.end_time_unix_nanos? <= collection_span.start_time_unix_nanos
  assert collection_span.end_time_unix_nanos? <= processing_span.start_time_unix_nanos
  assert processing_span.end_time_unix_nanos? <= export_span.start_time_unix_nanos
  
  // 验证管道总耗时
  let total_pipeline_duration = export_span.end_time_unix_nanos? - generation_span.start_time_unix_nanos
  assert total_pipeline_duration > 0L
  assert total_pipeline_duration == 13000000L  // 13秒
}

test "error handling in workflow" {
  // 测试工作流中的错误处理
  let workflow_start = 1640995200000000000L
  
  // 创建一个包含错误的Span
  let error_span = trace::Span::{
    name: "error-prone-operation",
    context: trace::SpanContext::{
      trace_id: [20_byte; 16],
      span_id: [20_byte; 8],
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: workflow_start,
    end_time_unix_nanos: Some(workflow_start + 5000000L),
    status: trace::Error,
    status_description: Some("Operation failed due to timeout"),
    attributes: [
      ("operation.type", common::AttributeValue::string("external-api-call")),
      ("error.type", common::AttributeValue::string("timeout")),
      ("timeout.duration", common::AttributeValue::int(30000L))
    ],
    events: [
      trace::SpanEvent::{
        name: "operation-started",
        timestamp_unix_nanos: workflow_start + 500000L,
        attributes: [
          ("api.endpoint", common::AttributeValue::string("https://external.service/api/data"))
        ]
      },
      trace::SpanEvent::{
        name: "timeout-occurred",
        timestamp_unix_nanos: workflow_start + 3000000L,
        attributes: [
          ("timeout.reason", common::AttributeValue::string("network-latency")),
          ("elapsed.time", common::AttributeValue::int(2500000L))
        ]
      },
      trace::SpanEvent::{
        name: "retry-attempted",
        timestamp_unix_nanos: workflow_start + 3500000L,
        attributes: [
          ("retry.count", common::AttributeValue::int(1L)),
          ("retry.strategy", common::AttributeValue::string("exponential-backoff"))
        ]
      },
      trace::SpanEvent::{
        name: "operation-failed",
        timestamp_unix_nanos: workflow_start + 4500000L,
        attributes: [
          ("final.error", common::AttributeValue::string("MAX_RETRIES_EXCEEDED")),
          ("total.attempts", common::AttributeValue::int(2L))
        ]
      }
    ],
    links: []
  }
  
  // 创建错误日志
  let error_log = logs::LogRecord::{
    timestamp_unix_nanos: workflow_start + 4000000L,
    observed_timestamp_unix_nanos: Some(workflow_start + 4100000L),
    severity_number: logs::Error,
    severity_text: Some("ERROR"),
    body: Some("External API call failed after retries"),
    attributes: [
      ("error.code", common::AttributeValue::string("TIMEOUT")),
      ("error.message", common::AttributeValue::string("Request timed out after 30 seconds")),
      ("retry.attempts", common::AttributeValue::int(2L)),
      ("fallback.activated", common::AttributeValue::bool(true))
    ],
    trace_id: Some(error_span.context.trace_id),
    span_id: Some(error_span.context.span_id),
    trace_flags: Some(error_span.context.trace_flags),
    resource: None,
    instrumentation_scope: None
  }
  
  // 创建恢复操作Span
  let recovery_start = workflow_start + 5000000L
  let recovery_span = trace::Span::{
    name: "error-recovery",
    context: trace::SpanContext::{
      trace_id: [20_byte; 16],
      span_id: [21_byte; 8],
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: Some(error_span.context.span_id),
    start_time_unix_nanos: recovery_start,
    end_time_unix_nanos: Some(recovery_start + 2000000L),
    status: trace::Ok,
    status_description: Some("Fallback mechanism activated successfully"),
    attributes: [
      ("recovery.strategy", common::AttributeValue::string("cache-fallback")),
      ("fallback.data.age", common::AttributeValue::int(300000L)),  // 5分钟
      ("recovery.success", common::AttributeValue::bool(true))
    ],
    events: [
      trace::SpanEvent::{
        name: "fallback-activated",
        timestamp_unix_nanos: recovery_start + 500000L,
        attributes: [
          ("fallback.source", common::AttributeValue::string("redis-cache")),
          ("data.stale", common::AttributeValue::bool(false))
        ]
      }
    ],
    links: []
  }
  
  // 验证错误处理工作流
  assert error_span.status == trace::Error
  assert error_log.severity_number == logs::Error
  assert recovery_span.status == trace::Ok
  assert recovery_span.parent_span_id? == error_span.context.span_id
  assert error_span.events.length == 4
  assert recovery_span.events.length == 1
}
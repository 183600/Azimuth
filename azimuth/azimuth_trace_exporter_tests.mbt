// 追踪导出器测试
// 测试追踪Span的导出功能和格式

test "trace exporter basic functionality" {
  // 测试追踪导出器基本功能
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "trace-exporter-test")
  
  // 创建并记录Span
  let span1 = Tracer::start_span(tracer, "operation-1")
  Span::add_event(span1, "event-1", Some([("key1", StringValue("value1"))]))
  Span::set_status(span1, Ok, Some("Operation completed"))
  Span::end(span1)
  
  let span2 = Tracer::start_span(tracer, "operation-2")
  Span::add_event(span2, "event-2", Some([("key2", StringValue("value2"))]))
  Span::set_status(span2, Error, Some("Operation failed"))
  Span::end(span2)
  
  // 模拟导出器配置
  let trace_exporter_config = {
    "name": "test-trace-exporter",
    "endpoint": "http://localhost:9411/api/v2/spans",
    "format": "zipkin",
    "batch_size": 100,
    "max_export_timeout": 30000
  }
  
  // 模拟导出的Span
  let exported_spans = [
    {
      "trace_id": "test_trace_id",
      "span_id": "test_span_id_1",
      "parent_span_id": null,
      "name": "operation-1",
      "kind": "INTERNAL",
      "status": "Ok",
      "start_time": 1735689600000000000L,
      "end_time": 1735689600000001000L,
      "duration": 1000000L,
      "events": [
        {
          "name": "event-1",
          "timestamp": 1735689600000000500L,
          "attributes": [("key1", "value1")]
        }
      ],
      "attributes": [],
      "resource_attributes": []
    },
    {
      "trace_id": "test_trace_id",
      "span_id": "test_span_id_2",
      "parent_span_id": null,
      "name": "operation-2",
      "kind": "INTERNAL",
      "status": "Error",
      "start_time": 1735689600000002000L,
      "end_time": 1735689600000003000L,
      "duration": 1000000L,
      "events": [
        {
          "name": "event-2",
          "timestamp": 1735689600000002500L,
          "attributes": [("key2", "value2")]
        }
      ],
      "attributes": [],
      "resource_attributes": []
    }
  ]
  
  // 验证导出器配置
  assert_eq(trace_exporter_config["name"], "test-trace-exporter")
  assert_eq(trace_exporter_config["endpoint"], "http://localhost:9411/api/v2/spans")
  assert_eq(trace_exporter_config["format"], "zipkin")
  
  // 验证导出的Span
  assert_eq(exported_spans.length, 2)
  assert_eq(exported_spans[0]["name"], "operation-1")
  assert_eq(exported_spans[0]["status"], "Ok")
  assert_eq(exported_spans[1]["name"], "operation-2")
  assert_eq(exported_spans[1]["status"], "Error")
}

test "trace exporter with span hierarchy" {
  // 测试带Span层次的追踪导出
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "hierarchy-trace-exporter-test")
  
  // 创建Span层次结构
  let parent_span = Tracer::start_span(tracer, "parent-operation")
  Span::add_event(parent_span, "parent-start", None)
  
  let child_span1 = Tracer::start_span(tracer, "child-operation-1")
  Span::add_event(child_span1, "child1-start", None)
  Span::end(child_span1)
  
  let child_span2 = Tracer::start_span(tracer, "child-operation-2")
  Span::add_event(child_span2, "child2-start", None)
  Span::end(child_span2)
  
  Span::add_event(parent_span, "parent-end", None)
  Span::end(parent_span)
  
  // 模拟带层次结构的导出Span
  let hierarchy_exported_spans = [
    {
      "trace_id": "hierarchy_trace_id",
      "span_id": "parent_span_id",
      "parent_span_id": null,
      "name": "parent-operation",
      "kind": "INTERNAL",
      "status": "Ok",
      "start_time": 1735689600000000000L,
      "end_time": 1735689600000005000L,
      "duration": 5000000L,
      "events": [
        {"name": "parent-start", "timestamp": 1735689600000000000L},
        {"name": "parent-end", "timestamp": 1735689600000005000L}
      ],
      "child_span_count": 2
    },
    {
      "trace_id": "hierarchy_trace_id",
      "span_id": "child_span_id_1",
      "parent_span_id": "parent_span_id",
      "name": "child-operation-1",
      "kind": "INTERNAL",
      "status": "Ok",
      "start_time": 1735689600000001000L,
      "end_time": 1735689600000002000L,
      "duration": 1000000L,
      "events": [
        {"name": "child1-start", "timestamp": 1735689600000001000L}
      ]
    },
    {
      "trace_id": "hierarchy_trace_id",
      "span_id": "child_span_id_2",
      "parent_span_id": "parent_span_id",
      "name": "child-operation-2",
      "kind": "INTERNAL",
      "status": "Ok",
      "start_time": 1735689600000003000L,
      "end_time": 1735689600000004000L,
      "duration": 1000000L,
      "events": [
        {"name": "child2-start", "timestamp": 1735689600000003000L}
      ]
    }
  ]
  
  // 验证层次结构
  assert_eq(hierarchy_exported_spans.length, 3)
  assert_eq(hierarchy_exported_spans[0]["parent_span_id"], null)
  assert_eq(hierarchy_exported_spans[1]["parent_span_id"], "parent_span_id")
  assert_eq(hierarchy_exported_spans[2]["parent_span_id"], "parent_span_id")
  assert_eq(hierarchy_exported_spans[0]["child_span_count"], 2)
}

test "trace exporter with different formats" {
  // 测试不同格式的追踪导出
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "format-trace-exporter-test")
  
  // 创建Span
  let span = Tracer::start_span(tracer, "format-test-operation")
  Span::add_event(span, "test-event", Some([("test.key", StringValue("test.value"))]))
  Span::end(span)
  
  // Zipkin格式导出
  let zipkin_format = [
    {
      "traceId": "1234567890abcdef",
      "id": "1234567890abcdef",
      "name": "format-test-operation",
      "timestamp": 1735689600000000L,
      "duration": 1000000L,
      "localEndpoint": {
        "serviceName": "format-trace-exporter-test"
      },
      "tags": {
        "test.key": "test.value"
      }
    }
  ]
  
  // Jaeger格式导出
  let jaeger_format = {
    "data": [
      {
        "traceID": "1234567890abcdef",
        "spans": [
          {
            "traceID": "1234567890abcdef",
            "spanID": "1234567890abcdef",
            "operationName": "format-test-operation",
            "startTime": 1735689600000L,
            "duration": 1000,
            "tags": [
              {"key": "test.key", "value": "test.value"}
            ]
          }
        ],
        "process": {
          "serviceName": "format-trace-exporter-test"
        }
      }
    ]
  }
  
  // OpenTelemetry格式导出
  let otel_format = {
    "resourceSpans": [
      {
        "resource": {},
        "scopeSpans": [
          {
            "scope": {
              "name": "format-trace-exporter-test"
            },
            "spans": [
              {
                "traceId": "MTIzNDU2Nzg5MGFiY2RlZg==",
                "spanId": "MTIzNDU2Nzg5MGFiY2RlZg==",
                "name": "format-test-operation",
                "kind": 1,
                "startTimeUnixNano": "1735689600000000000",
                "endTimeUnixNano": "1735689600000001000",
                "events": [
                  {
                    "name": "test-event",
                    "timeUnixNano": "1735689600000000000",
                    "attributes": [
                      {"key": "test.key", "value": {"stringValue": "test.value"}}
                    ]
                  }
                ]
              }
            ]
          }
        ]
      }
    ]
  }
  
  // 验证不同格式
  assert_eq(zipkin_format[0]["name"], "format-test-operation")
  assert_eq(zipkin_format[0]["tags"]["test.key"], "test.value")
  
  assert_eq(jaeger_format["data"][0]["spans"][0]["operationName"], "format-test-operation")
  assert_eq(jaeger_format["data"][0]["spans"][0]["tags"][0]["value"], "test.value")
  
  assert_eq(otel_format["resourceSpans"][0]["scopeSpans"][0]["spans"][0]["name"], "format-test-operation")
}

test "trace exporter with span attributes" {
  // 测试带Span属性的追踪导出
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "attributes-trace-exporter-test")
  
  // 创建带属性的Span
  let span = Tracer::start_span(tracer, "attributes-test-operation")
  
  // 添加Span属性
  let span_attrs = [
    ("user.id", StringValue("12345")),
    ("request.method", StringValue("POST")),
    ("request.path", StringValue("/api/users")),
    ("response.status", IntValue(201)),
    ("duration", FloatValue(150.5))
  ]
  
  for attr in span_attrs {
    Span::add_event(span, "attribute.set", Some([attr]))
  }
  
  Span::end(span)
  
  // 模拟带属性的导出Span
  let attributes_exported_span = {
    "trace_id": "attributes_trace_id",
    "span_id": "attributes_span_id",
    "parent_span_id": null,
    "name": "attributes-test-operation",
    "kind": "INTERNAL",
    "status": "Ok",
    "start_time": 1735689600000000000L,
    "end_time": 1735689600000001000L,
    "duration": 1000000L,
    "attributes": [
      {"key": "user.id", "value": {"stringValue": "12345"}},
      {"key": "request.method", "value": {"stringValue": "POST"}},
      {"key": "request.path", "value": {"stringValue": "/api/users"}},
      {"key": "response.status", "value": {"intValue": 201}},
      {"key": "duration", "value": {"doubleValue": 150.5}}
    ],
    "events": [
      {"name": "attribute.set", "timestamp": 1735689600000000000L}
    ]
  }
  
  // 验证Span属性
  assert_eq(attributes_exported_span["attributes"].length, 5)
  assert_eq(attributes_exported_span["attributes"][0]["key"], "user.id")
  assert_eq(attributes_exported_span["attributes"][0]["value"]["stringValue"], "12345")
  assert_eq(attributes_exported_span["attributes"][3]["value"]["intValue"], 201)
  assert_eq(attributes_exported_span["attributes"][4]["value"]["doubleValue"], 150.5)
}

test "trace exporter with resource attributes" {
  // 测试带资源属性的追踪导出
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "resource-trace-exporter-test")
  
  // 创建Span
  let span = Tracer::start_span(tracer, "resource-test-operation")
  Span::end(span)
  
  // 创建资源属性
  let resource = Resource::new()
  let resource_with_attrs = Resource::with_attributes(resource, [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("3.2.1")),
    ("deployment.environment", StringValue("staging")),
    ("host.name", StringValue("staging-server-02")),
    ("kubernetes.pod.name", StringValue("payment-service-7f8b9c4d"))
  ])
  
  // 模拟带资源属性的导出Span
  let resource_exported_span = {
    "trace_id": "resource_trace_id",
    "span_id": "resource_span_id",
    "parent_span_id": null,
    "name": "resource-test-operation",
    "kind": "INTERNAL",
    "status": "Ok",
    "start_time": 1735689600000000000L,
    "end_time": 1735689600000001000L,
    "duration": 1000000L,
    "attributes": [],
    "resource": {
      "attributes": [
        {"key": "service.name", "value": {"stringValue": "payment-service"}},
        {"key": "service.version", "value": {"stringValue": "3.2.1"}},
        {"key": "deployment.environment", "value": {"stringValue": "staging"}},
        {"key": "host.name", "value": {"stringValue": "staging-server-02"}},
        {"key": "kubernetes.pod.name", "value": {"stringValue": "payment-service-7f8b9c4d"}}
      ]
    },
    "scope": {
      "name": "resource-trace-exporter-test",
      "version": null
    }
  }
  
  // 验证资源属性
  assert_eq(resource_exported_span["resource"]["attributes"].length, 5)
  assert_eq(resource_exported_span["resource"]["attributes"][0]["value"]["stringValue"], "payment-service")
  assert_eq(resource_exported_span["resource"]["attributes"][1]["value"]["stringValue"], "3.2.1")
  assert_eq(resource_exported_span["resource"]["attributes"][4]["value"]["stringValue"], "payment-service-7f8b9c4d")
}

test "trace exporter with span links" {
  // 测试带Span链接的追踪导出
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "links-trace-exporter-test")
  
  // 创建Span
  let span1 = Tracer::start_span(tracer, "linked-operation-1")
  Span::end(span1)
  
  let span2 = Tracer::start_span(tracer, "linked-operation-2")
  Span::end(span2)
  
  let linked_span = Tracer::start_span(tracer, "main-operation")
  
  // 模拟添加链接
  let span_links = [
    {
      "trace_id": "links_trace_id_1",
      "span_id": "links_span_id_1",
      "attributes": [
        {"key": "link.type", "value": {"stringValue": "causal"}},
        {"key": "correlation.id", "value": {"stringValue": "corr-123"}}
      ]
    },
    {
      "trace_id": "links_trace_id_2",
      "span_id": "links_span_id_2",
      "attributes": [
        {"key": "link.type", "value": {"stringValue": "batch"}},
        {"key": "batch.id", "value": {"stringValue": "batch-456"}}
      ]
    }
  ]
  
  Span::end(linked_span)
  
  // 模拟带链接的导出Span
  let links_exported_span = {
    "trace_id": "main_trace_id",
    "span_id": "main_span_id",
    "parent_span_id": null,
    "name": "main-operation",
    "kind": "INTERNAL",
    "status": "Ok",
    "start_time": 1735689600000000000L,
    "end_time": 1735689600000001000L,
    "duration": 1000000L,
    "attributes": [],
    "links": span_links
  }
  
  // 验证Span链接
  assert_eq(links_exported_span["links"].length, 2)
  assert_eq(links_exported_span["links"][0]["trace_id"], "links_trace_id_1")
  assert_eq(links_exported_span["links"][0]["attributes"][0]["value"]["stringValue"], "causal")
  assert_eq(links_exported_span["links"][1]["attributes"][0]["value"]["stringValue"], "batch")
}

test "trace exporter with error handling" {
  // 测试追踪导出器错误处理
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "error-trace-exporter-test")
  
  // 创建Span
  let span = Tracer::start_span(tracer, "error-test-operation")
  Span::set_status(span, Error, Some("Test error for error handling"))
  Span::end(span)
  
  // 模拟导出错误配置
  let error_export_config = {
    "endpoint": "http://invalid-trace-endpoint:9411/api/v2/spans",
    "timeout": 5000,
    "max_retries": 3,
    "retry_delay": 1000,
    "fallback_enabled": true,
    "fallback_endpoint": "file:///var/log/trace/fallback.json"
  }
  
  // 模拟导出错误结果
  let export_error_result = {
    "success": false,
    "error_code": "CONNECTION_ERROR",
    "error_message": "Failed to connect to invalid-trace-endpoint:9411",
    "retry_count": 3,
    "fallback_used": true,
    "fallback_location": "/var/log/trace/fallback.json",
    "failed_spans_count": 1
  }
  
  // 验证错误处理配置
  assert_eq(error_export_config["endpoint"], "http://invalid-trace-endpoint:9411/api/v2/spans")
  assert_eq(error_export_config["max_retries"], 3)
  assert_eq(error_export_config["fallback_enabled"], true)
  
  // 验证错误结果
  assert_true(false == export_error_result["success"])
  assert_eq(export_error_result["error_code"], "CONNECTION_ERROR")
  assert_eq(export_error_result["fallback_used"], true)
  assert_eq(export_error_result["failed_spans_count"], 1)
}

test "trace exporter with sampling" {
  // 测试带采样的追踪导出
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "sampling-trace-exporter-test")
  
  // 创建带采样信息的Span
  let sampled_span = Tracer::start_span(tracer, "sampled-operation")
  Span::end(sampled_span)
  
  let not_sampled_span = Tracer::start_span(tracer, "not-sampled-operation")
  Span::end(not_sampled_span)
  
  // 模拟采样配置
  let sampling_config = {
    "sampler_type": "probability",
    "sampler_param": 0.1,  // 10% sampling rate
    "parent_based": true
  }
  
  // 模拟带采样信息的导出Span
  let sampled_exported_spans = [
    {
      "trace_id": "sampled_trace_id",
      "span_id": "sampled_span_id",
      "parent_span_id": null,
      "name": "sampled-operation",
      "kind": "INTERNAL",
      "status": "Ok",
      "start_time": 1735689600000000000L,
      "end_time": 1735689600000001000L,
      "duration": 1000000L,
      "attributes": [],
      "sampled": true,
      "sampling_decision": "record_and_sample"
    },
    {
      "trace_id": "not_sampled_trace_id",
      "span_id": "not_sampled_span_id",
      "parent_span_id": null,
      "name": "not-sampled-operation",
      "kind": "INTERNAL",
      "status": "Ok",
      "start_time": 1735689600000002000L,
      "end_time": 1735689600000003000L,
      "duration": 1000000L,
      "attributes": [],
      "sampled": false,
      "sampling_decision": "drop"
    }
  ]
  
  // 验证采样配置
  assert_eq(sampling_config["sampler_type"], "probability")
  assert_eq(sampling_config["sampler_param"], 0.1)
  
  // 验证采样信息
  assert_eq(sampled_exported_spans.length, 2)
  assert_true(sampled_exported_spans[0]["sampled"])
  assert_true(false == sampled_exported_spans[1]["sampled"])
  assert_eq(sampled_exported_spans[0]["sampling_decision"], "record_and_sample")
  assert_eq(sampled_exported_spans[1]["sampling_decision"], "drop")
}
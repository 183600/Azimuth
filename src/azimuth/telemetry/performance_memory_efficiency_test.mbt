test "Performance and memory efficiency comprehensive test" {
  // 测试大量AttributeValue创建的性能
  let start_time = @sysclock.now()
  
  let mut string_attrs = []
  for i = 0; i < 1000; i = i + 1 {
    string_attrs = string_attrs + [common::AttributeValue::string("value_" + i.to_string())]
  }
  
  let mut int_attrs = []
  for i = 0; i < 1000; i = i + 1 {
    int_attrs = int_attrs + [common::AttributeValue::int(i.to_int64())]
  }
  
  let mut float_attrs = []
  for i = 0; i < 1000; i = i + 1 {
    float_attrs = float_attrs + [common::AttributeValue::float(i.to_double() * 0.5)]
  }
  
  let mut bool_attrs = []
  for i = 0; i < 1000; i = i + 1 {
    bool_attrs = bool_attrs + [common::AttributeValue::bool(i % 2 == 0)]
  }
  
  let attr_creation_time = @sysclock.now() - start_time
  
  // 验证创建的属性数量
  assert_eq(string_attrs.length(), 1000)
  assert_eq(int_attrs.length(), 1000)
  assert_eq(float_attrs.length(), 1000)
  assert_eq(bool_attrs.length(), 1000)
  
  // 测试数组属性的性能
  let array_start_time = @sysclock.now()
  
  let string_arrays = [
    common::AttributeValue::array_string(["a", "b", "c", "d", "e"]),
    common::AttributeValue::array_string([for i = 0; i < 100; i = i + 1].map(fn(i) { "item_" + i.to_string() })),
    common::AttributeValue::array_string([])
  ]
  
  let int_arrays = [
    common::AttributeValue::array_int([1L, 2L, 3L, 4L, 5L]),
    common::AttributeValue::array_int([for i = 0; i < 100; i = i + 1].map(fn(i) { i.to_int64() })),
    common::AttributeValue::array_int([])
  ]
  
  let array_creation_time = @sysclock.now() - array_start_time
  
  // 验证数组属性
  match string_arrays[1] {
    common::ArrayStringValue(arr) => assert_eq(arr.length(), 100)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match int_arrays[1] {
    common::ArrayIntValue(arr) => assert_eq(arr.length(), 100)
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  // 测试Resource创建性能
  let resource_start_time = @sysclock.now()
  
  let mut resources = []
  for i = 0; i < 100; i = i + 1 {
    let mut resource = common::Resource::default("service_" + i.to_string())
    resource.service_version = Some("1." + i.to_string() + ".0")
    resource.attributes = [
      ("instance_id", common::AttributeValue::int(i.to_int64())),
      ("env", common::AttributeValue::string("production")),
      ("region", common::AttributeValue::string("us-east-" + (i % 3).to_string()))
    ]
    resources = resources + [resource]
  }
  
  let resource_creation_time = @sysclock.now() - resource_start_time
  assert_eq(resources.length(), 100)
  
  // 测试LogRecord构建性能
  let log_start_time = @sysclock.now()
  
  let mut log_records = []
  for i = 0; i < 500; i = i + 1 {
    let log = logs::LogRecord::builder()
      .timestamp(1640995200000000000L + i.to_int64())
      .severity(match i % 6 {
        0 => logs::Trace
        1 => logs::Debug
        2 => logs::Info
        3 => logs::Warn
        4 => logs::Error
        _ => logs::Fatal
      })
      .body("Log message " + i.to_string() + " with some additional content to simulate realistic log messages")
      .with_attribute("log_index", common::AttributeValue::int(i.to_int64()))
      .with_attribute("module", common::AttributeValue::string("module_" + (i % 10).to_string()))
      .with_attribute("severity", common::AttributeValue::string(match i % 6 {
        0 => "TRACE"
        1 => "DEBUG"
        2 => "INFO"
        3 => "WARN"
        4 => "ERROR"
        _ => "FATAL"
      }))
      .build()
    log_records = log_records + [log]
  }
  
  let log_creation_time = @sysclock.now() - log_start_time
  assert_eq(log_records.length(), 500)
  
  // 测试Span创建性能
  let span_start_time = @sysclock.now()
  
  let mut spans = []
  for i = 0; i < 200; i = i + 1 {
    let span_context = trace::SpanContext{
      trace_id: [for j = 0; j < 16; j = j + 1].map(fn(j) { ((i + j) % 256).to_byte() }),
      span_id: [for j = 0; j < 8; j = j + 1].map(fn(j) { ((i + j) % 256).to_byte() }),
      trace_flags: 1_byte,
      trace_state: "trace-state-" + i.to_string()
    }
    
    let span = trace::Span{
      name: "operation_" + i.to_string(),
      context: span_context,
      kind: match i % 5 {
        0 => trace::Internal
        1 => trace::Server
        2 => trace::Client
        3 => trace::Producer
        _ => trace::Consumer
      },
      parent_span_id: if i > 0 { Some([for j = 0; j < 8; j = j + 1].map(fn(j) { ((i - 1 + j) % 256).to_byte() })) } else { None },
      start_time_unix_nanos: 1640995200000000000L + i.to_int64() * 1000000L,
      end_time_unix_nanos: Some(1640995200000000000L + i.to_int64() * 1000000L + 500000L),
      status: match i % 3 {
        0 => trace::Unset
        1 => trace::Ok
        _ => trace::Error
      },
      status_description: if i % 3 == 2 { Some("Error occurred") } else { None },
      attributes: [
        ("span_index", common::AttributeValue::int(i.to_int64())),
        ("operation_type", common::AttributeValue::string("type_" + (i % 5).to_string())),
        ("duration_ms", common::AttributeValue::float(0.5))
      ],
      events: if i % 10 == 0 {
        [trace::SpanEvent{
          name: "milestone",
          timestamp_unix_nanos: 1640995200000000000L + i.to_int64() * 1000000L + 250000L,
          attributes: [("milestone_type", common::AttributeValue::string("checkpoint"))]
        }]
      } else { [] },
      links: []
    }
    spans = spans + [span]
  }
  
  let span_creation_time = @sysclock.now() - span_start_time
  assert_eq(spans.length(), 200)
  
  // 测试Metrics操作性能
  let metrics_start_time = @sysclock.now()
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("performance-test-meter", Some("1.0.0"), None)
  
  let counter = meter.create_counter("performance_counter", Some("operations"), Some("Performance test counter"))
  let histogram = meter.create_histogram("performance_histogram", Some("ms"), Some("Performance test histogram"))
  let up_down_counter = meter.create_up_down_counter("performance_up_down", Some("items"), Some("Performance test up-down counter"))
  let gauge = meter.create_gauge("performance_gauge", Some("%"), Some("Performance test gauge"))
  
  // 执行大量指标操作
  for i = 0; i < 1000; i = i + 1 {
    counter.add(1L, [
      ("operation_type", common::AttributeValue::string("type_" + (i % 5).to_string())),
      ("status", common::AttributeValue::string(if i % 10 == 0 { "error" } else { "success" }))
    ])
    
    histogram.record(i.to_double() * 0.1, [
      ("method", common::AttributeValue::string(["GET", "POST", "PUT", "DELETE"][i % 4])),
      ("endpoint", common::AttributeValue::string("/api/endpoint_" + (i % 10).to_string()))
    ])
    
    up_down_counter.add(if i % 3 == 0 { 1L } else { -1L }, [
      ("queue_name", common::AttributeValue::string("queue_" + (i % 3).to_string()))
    ])
    
    gauge.record((i % 100).to_double(), [
      ("metric_name", common::AttributeValue::string("cpu_usage")),
      ("host", common::AttributeValue::string("host_" + (i % 5).to_string()))
    ])
  }
  
  let metrics_operation_time = @sysclock.now() - metrics_start_time
  
  // 测试内存效率 - 大量小对象
  let memory_test_start = @sysclock.now()
  
  let mut small_objects = []
  for i = 0; i < 2000; i = i + 1 {
    let small_resource = common::Resource::default("small-service")
    small_resource.attributes = [("id", common::AttributeValue::int(i.to_int64()))]
    
    let small_log = logs::LogRecord::builder()
      .severity(logs::Info)
      .body("Small log " + i.to_string())
      .build()
    
    let small_span = trace::Span{
      name: "small",
      context: trace::SpanContext{
        trace_id: [for j = 0; j < 16; j = j + 1].map(fn(_) { 0_byte }),
        span_id: [for j = 0; j < 8; j = j + 1].map(fn(_) { 0_byte }),
        trace_flags: 0_byte,
        trace_state: ""
      },
      kind: trace::Internal,
      parent_span_id: None,
      start_time_unix_nanos: i.to_int64(),
      end_time_unix_nanos: Some(i.to_int64()),
      status: trace::Ok,
      status_description: None,
      attributes: [],
      events: [],
      links: []
    }
    
    small_objects = small_objects + [(small_resource, small_log, small_span)]
  }
  
  let memory_test_time = @sysclock.now() - memory_test_start
  assert_eq(small_objects.length(), 2000)
  
  // 测试复杂场景的性能
  let complex_start_time = @sysclock.now()
  
  let shared_resource = common::Resource::default("complex-service")
  shared_resource.attributes = [
    ("env", common::AttributeValue::string("production")),
    ("version", common::AttributeValue::string("2.1.0")),
    ("region", common::AttributeValue::string("us-east")),
    ("instance_count", common::AttributeValue::int(10L))
  ]
  
  let shared_scope = common::InstrumentationScope{
    name: "complex-scope",
    version: Some("1.5.0"),
    schema_url: Some("https://example.com/schema")
  }
  
  let mut complex_logs = []
  let mut complex_spans = []
  
  // 创建复杂的日志和span场景
  for i = 0; i < 100; i = i + 1 {
    // 复杂日志
    let complex_log = logs::LogRecord::builder()
      .timestamp(1640995200000000000L + i.to_int64() * 1000000L)
      .severity(match i % 4 {
        0 => logs::Info
        1 => logs::Warn
        2 => logs::Error
        _ => logs::Debug
      })
      .body("Complex operation " + i.to_string() + " with detailed context information")
      .with_attribute("operation_id", common::AttributeValue::int(i.to_int64()))
      .with_attribute("user_id", common::AttributeValue::int((i * 1000).to_int64()))
      .with_attribute("request_id", common::AttributeValue::string("req_" + i.to_string()))
      .with_attribute("session_id", common::AttributeValue::string("sess_" + (i % 10).to_string()))
      .with_attribute("client_version", common::AttributeValue::string("2.1.0"))
      .with_attribute("ip_address", common::AttributeValue::string("192.168.1." + (i % 255).to_string()))
      .with_attribute("user_agent", common::AttributeValue::string("Azimuth-Client/2.1.0"))
      .with_attribute("request_size", common::AttributeValue::int((1024 + i * 100).to_int64()))
      .with_attribute("response_size", common::AttributeValue::int((2048 + i * 200).to_int64()))
      .build()
    
    let mut complex_log_integrated = complex_log
    complex_log_integrated.resource = shared_resource
    complex_log_integrated.instrumentation_scope = shared_scope
    complex_logs = complex_logs + [complex_log_integrated]
    
    // 复杂span
    let span_context = trace::SpanContext{
      trace_id: [for j = 0; j < 16; j = j + 1].map(fn(j) { ((i * 17 + j) % 256).to_byte() }),
      span_id: [for j = 0; j < 8; j = j + 1].map(fn(j) { ((i * 13 + j) % 256).to_byte() }),
      trace_flags: 1_byte,
      trace_state: "complex-trace-" + i.to_string()
    }
    
    let complex_span = trace::Span{
      name: "complex_operation_" + i.to_string(),
      context: span_context,
      kind: match i % 4 {
        0 => trace::Server
        1 => trace::Client
        2 => trace::Producer
        _ => trace::Consumer
      },
      parent_span_id: if i > 0 { 
        Some([for j = 0; j < 8; j = j + 1].map(fn(j) { (((i - 1) * 13 + j) % 256).to_byte() }))
      } else { None },
      start_time_unix_nanos: 1640995200000000000L + i.to_int64() * 1000000L,
      end_time_unix_nanos: Some(1640995200000000000L + i.to_int64() * 1000000L + (500 + i * 10).to_int64()),
      status: match i % 5 {
        0 => trace::Unset
        1 => trace::Ok
        2 => trace::Ok
        3 => trace::Error
        _ => trace::Error
      },
      status_description: if i % 5 >= 3 { 
        Some("Operation failed with error code " + (500 + i % 10).to_string()) 
      } else { 
        Some("Operation completed successfully") 
      },
      attributes: [
        ("operation_id", common::AttributeValue::int(i.to_int64())),
        ("operation_type", common::AttributeValue::string(["query", "update", "delete", "create"][i % 4])),
        ("table_name", common::AttributeValue::string("table_" + (i % 5).to_string())),
        ("records_affected", common::AttributeValue::int((i + 1).to_int64())),
        ("duration_ms", common::AttributeValue::float((500.0 + i.to_double() * 10.0))),
        ("cpu_usage", common::AttributeValue::float((50.0 + i.to_double() * 2.0))),
        ("memory_usage", common::AttributeValue::float((100.0 + i.to_double() * 5.0))),
        ("cache_hit", common::AttributeValue::bool(i % 3 == 0)),
        ("retry_count", common::AttributeValue::int((i % 3).to_int64()))
      ],
      events: if i % 5 == 0 {
        [
          trace::SpanEvent{
            name: "database.query.start",
            timestamp_unix_nanos: 1640995200000000000L + i.to_int64() * 1000000L + 100000L,
            attributes: [
              ("query", common::AttributeValue::string("SELECT * FROM table_" + (i % 5).to_string())),
              ("timeout", common::AttributeValue::int(30000L))
            ]
          },
          trace::SpanEvent{
            name: "database.query.complete",
            timestamp_unix_nanos: 1640995200000000000L + i.to_int64() * 1000000L + 300000L,
            attributes: [
              ("rows_returned", common::AttributeValue::int((i + 10).to_int64())),
              ("execution_time_ms", common::AttributeValue::float(200.0))
            ]
          }
        ]
      } else { [] },
      links: if i % 7 == 0 && i > 0 {
        [trace::SpanLink{
          context: trace::SpanContext{
            trace_id: [for j = 0; j < 16; j = j + 1].map(fn(j) { (((i - 1) * 17 + j) % 256).to_byte() }),
            span_id: [for j = 0; j < 8; j = j + 1].map(fn(j) { (((i - 1) * 13 + j) % 256).to_byte() }),
            trace_flags: 1_byte,
            trace_state: ""
          },
          attributes: [
            ("link_type", common::AttributeValue::string("related_operation")),
            ("relationship", common::AttributeValue::string("follows_from"))
          ]
        }]
      } else { [] }
    }
    
    complex_spans = complex_spans + [complex_span]
  }
  
  let complex_test_time = @sysclock.now() - complex_start_time
  assert_eq(complex_logs.length(), 100)
  assert_eq(complex_spans.length(), 100)
  
  // 测试并发性能 - 模拟多个同时操作
  let concurrent_start_time = @sysclock.now()
  
  let concurrent_meter = meter_provider.get_meter("concurrent-test", None, None)
  let concurrent_counter = concurrent_meter.create_counter("concurrent_operations", None, None)
  let concurrent_histogram = concurrent_meter.create_histogram("concurrent_duration", None, None)
  
  // 模拟并发操作
  for i = 0; i < 500; i = i + 1 {
    let operation_id = i.to_int64()
    let thread_id = (i % 10).to_int64()
    
    concurrent_counter.add(1L, [
      ("operation_id", common::AttributeValue::int(operation_id)),
      ("thread_id", common::AttributeValue::int(thread_id)),
      ("operation_type", common::AttributeValue::string(["read", "write", "update", "delete"][i % 4]))
    ])
    
    concurrent_histogram.record((i % 100).to_double(), [
      ("operation_id", common::AttributeValue::int(operation_id)),
      ("thread_id", common::AttributeValue::int(thread_id))
    ])
    
    // 创建并发日志
    let concurrent_log = logs::LogRecord::builder()
      .timestamp(1640995200000000000L + operation_id)
      .severity(logs::Info)
      .body("Concurrent operation " + operation_id.to_string())
      .with_attribute("operation_id", common::AttributeValue::int(operation_id))
      .with_attribute("thread_id", common::AttributeValue::int(thread_id))
      .build()
    
    // 创建并发span
    let concurrent_span = trace::Span{
      name: "concurrent_op_" + operation_id.to_string(),
      context: trace::SpanContext{
        trace_id: [for j = 0; j < 16; j = j + 1].map(fn(j) { ((operation_id + j) % 256).to_byte() }),
        span_id: [for j = 0; j < 8; j = j + 1].map(fn(j) { ((operation_id * 3 + j) % 256).to_byte() }),
        trace_flags: 1_byte,
        trace_state: ""
      },
      kind: trace::Internal,
      parent_span_id: None,
      start_time_unix_nanos: 1640995200000000000L + operation_id,
      end_time_unix_nanos: Some(1640995200000000000L + operation_id + 100L),
      status: trace::Ok,
      status_description: None,
      attributes: [
        ("operation_id", common::AttributeValue::int(operation_id)),
        ("thread_id", common::AttributeValue::int(thread_id))
      ],
      events: [],
      links: []
    }
  }
  
  let concurrent_test_time = @sysclock.now() - concurrent_start_time
  
  // 验证性能指标（这些是基本的合理性检查，实际性能会因环境而异）
  // 所有操作都应该在合理时间内完成
  assert_true(attr_creation_time.to_int64() < 10000000000L)  // 10秒
  assert_true(array_creation_time.to_int64() < 10000000000L)  // 10秒
  assert_true(resource_creation_time.to_int64() < 10000000000L)  // 10秒
  assert_true(log_creation_time.to_int64() < 10000000000L)  // 10秒
  assert_true(span_creation_time.to_int64() < 10000000000L)  // 10秒
  assert_true(metrics_operation_time.to_int64() < 10000000000L)  // 10秒
  assert_true(memory_test_time.to_int64() < 10000000000L)  // 10秒
  assert_true(complex_test_time.to_int64() < 10000000000L)  // 10秒
  assert_true(concurrent_test_time.to_int64() < 10000000000L)  // 10秒
  
  // 测试内存泄漏预防 - 大量创建和销毁
  let leak_test_start = @sysclock.now()
  
  for batch = 0; batch < 10; batch = batch + 1 {
    // 创建大量临时对象
    let mut temp_objects = []
    for i = 0; i < 100; i = i + 1 {
      let temp_resource = common::Resource::default("temp_service_" + i.to_string())
      let temp_log = logs::LogRecord::builder()
        .severity(logs::Info)
        .body("Temp log " + i.to_string())
        .build()
      let temp_span = trace::Span{
        name: "temp_span_" + i.to_string(),
        context: trace::SpanContext{
          trace_id: [for j = 0; j < 16; j = j + 1].map(fn(_) { 0_byte }),
          span_id: [for j = 0; j < 8; j = j + 1].map(fn(_) { 0_byte }),
          trace_flags: 0_byte,
          trace_state: ""
        },
        kind: trace::Internal,
        parent_span_id: None,
        start_time_unix_nanos: i.to_int64(),
        end_time_unix_nanos: Some(i.to_int64()),
        status: trace::Ok,
        status_description: None,
        attributes: [],
        events: [],
        links: []
      }
      temp_objects = temp_objects + [(temp_resource, temp_log, temp_span)]
    }
    
    // 临时对象应该在离开作用域时被垃圾回收
    // 这里我们只是验证创建过程不会崩溃
    assert_eq(temp_objects.length(), 100)
  }
  
  let leak_test_time = @sysclock.now() - leak_test_start
  assert_true(leak_test_time.to_int64() < 10000000000L)  // 10秒
  
  // 输出性能统计信息（仅用于验证，不参与测试断言）
  // 在实际环境中，这些时间可以用来监控性能回归
  let total_test_time = @sysclock.now() - start_time
  
  // 验证总测试时间在合理范围内
  assert_true(total_test_time.to_int64() < 60000000000L)  // 60秒
  
  // 验证所有创建的对象都是有效的
  assert_eq(string_attrs.length(), 1000)
  assert_eq(int_attrs.length(), 1000)
  assert_eq(float_attrs.length(), 1000)
  assert_eq(bool_attrs.length(), 1000)
  assert_eq(resources.length(), 100)
  assert_eq(log_records.length(), 500)
  assert_eq(spans.length(), 200)
  assert_eq(small_objects.length(), 2000)
  assert_eq(complex_logs.length(), 100)
  assert_eq(complex_spans.length(), 100)
}
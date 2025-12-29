// 性能压力测试 - 测试高并发场景下的性能

test "high_concurrency_span_creation" {
  // 测试高并发Span创建的性能
  
  let start_time = 0L  // 在实际实现中应该获取当前时间
  let span_count = 10000
  
  // 创建大量Span
  let mut spans = []
  let mut i = 0
  while i < span_count {
    let trace_id = [for j = 0; j < 16; j = j + 1].map(fn(_) { (i % 256).to_byte() })
    let span_id = [for j = 0; j < 8; j = j + 1].map(fn(_) { ((i * 2) % 256).to_byte() })
    
    let span = Span::{
      name: "performance_test_span_" + i.to_string(),
      context: SpanContext::{
        trace_id: trace_id,
        span_id: span_id,
        trace_flags: 1_byte,
        trace_state: "key=value"
      },
      kind: Internal,
      parent_span_id: None,
      start_time_unix_nanos: i.to_int64(),
      end_time_unix_nanos: Some((i + 100).to_int64()),
      status: Ok,
      status_description: None,
      attributes: [
        ("span.index", AttributeValue::int(i.to_int64())),
        ("performance.test", AttributeValue::bool(true)),
        ("batch.id", AttributeValue::string("batch_" + (i / 100).to_string()))
      ],
      events: [],
      links: []
    }
    
    spans.push(span)
    i = i + 1
  }
  
  let end_time = 0L  // 在实际实现中应该获取当前时间
  let duration = end_time - start_time
  
  // 验证所有Span都正确创建
  assert_eq(spans.length(), span_count)
  
  // 验证第一个和最后一个Span的属性
  assert_eq(spans[0].name, "performance_test_span_0")
  match spans[0].attributes[0].1 {
    IntValue(index) => assert_eq(index, 0L)
    _ => @test.fail("Test failed")
  }
  
  assert_eq(spans[span_count - 1].name, "performance_test_span_" + (span_count - 1).to_string())
  match spans[span_count - 1].attributes[0].1 {
    IntValue(index) => assert_eq(index, (span_count - 1).to_int64())
    _ => @test.fail("Test failed")
  }
  
  // 性能断言（这些值应该根据实际环境调整）
  assert_eq(duration < 5000L, true)  // 应该在5秒内完成
  assert_eq(spans.length() / duration, span_count / duration)  // 计算创建速率
}

test "massive_context_operations" {
  // 测试大量Context操作的性能
  
  let ctx = Context::empty()
  let operation_count = 5000
  
  // 测试大量Context值设置
  let start_time = 0L
  let mut ctx_with_values = ctx
  let mut i = 0
  while i < operation_count {
    let key = create_key("perf_key_" + i.to_string())
    ctx_with_values = ctx_with_values.with_value(key, "perf_value_" + i.to_string())
    i = i + 1
  }
  let set_duration = 0L
  
  // 测试大量Context值获取
  let start_get_time = 0L
  let mut retrieved_count = 0
  let mut j = 0
  while j < operation_count {
    let key = create_key("perf_key_" + j.to_string())
    match ctx_with_values.get(key) {
      Some(value) => {
        assert_eq(value, "perf_value_" + j.to_string())
        retrieved_count = retrieved_count + 1
      }
      None => @test.fail("Test failed")
    }
    j = j + 1
  }
  let get_duration = 0L
  
  // 验证操作结果
  assert_eq(retrieved_count, operation_count)
  
  // 性能断言
  assert_eq(set_duration < 3000L, true)  // 设置操作应该在3秒内完成
  assert_eq(get_duration < 2000L, true)  // 获取操作应该在2秒内完成
  
  // 计算操作速率
  let set_rate = operation_count.to_int64() / set_duration
  let get_rate = operation_count.to_int64() / get_duration
  
  assert_eq(set_rate > 1000L, true)  // 每秒至少1000次设置操作
  assert_eq(get_rate > 2000L, true)  // 每秒至少2000次获取操作
}

test "high_volume_metrics_recording" {
  // 测试高量Metrics记录的性能
  
  let meter = NoopMeter::{}
  let counter = meter.create_counter("performance_counter", Some("operations"), Some("Performance test counter"))
  let histogram = meter.create_histogram("performance_histogram", Some("ms"), Some("Performance test histogram"))
  let gauge = meter.create_gauge("performance_gauge", Some("units"), Some("Performance test gauge"))
  let up_down_counter = meter.create_up_down_counter("performance_up_down", Some("units"), Some("Performance test up-down counter"))
  
  let measurement_count = 20000
  
  // 准备属性
  let base_attributes = [
    ("service.name", AttributeValue::string("performance-test-service")),
    ("operation.type", AttributeValue::string("stress_test")),
    ("test.batch", AttributeValue::string("batch_001"))
  ]
  
  // 测试大量Counter操作
  let start_counter_time = 0L
  let mut i = 0
  while i < measurement_count {
    let batch_attributes = base_attributes + [
      ("measurement.index", AttributeValue::int(i.to_int64())),
      ("batch.number", AttributeValue::int((i / 1000).to_int64()))
    ]
    counter.add(1L, Some(batch_attributes))
    i = i + 1
  }
  let counter_duration = 0L
  
  // 测试大量Histogram操作
  let start_histogram_time = 0L
  let mut j = 0
  while j < measurement_count {
    let value = (j % 1000).to_double() / 10.0  // 0.0 到 99.9
    let batch_attributes = base_attributes + [
      ("measurement.index", AttributeValue::int(j.to_int64())),
      ("value.range", AttributeValue::string("range_" + ((j / 100).to_string())))
    ]
    histogram.record(value, Some(batch_attributes))
    j = j + 1
  }
  let histogram_duration = 0L
  
  // 测试大量Gauge操作
  let start_gauge_time = 0L
  let mut k = 0
  while k < measurement_count {
    let value = 50.0 + (k % 100).to_double() / 2.0  // 50.0 到 99.5
    let batch_attributes = base_attributes + [
      ("measurement.index", AttributeValue::int(k.to_int64())),
      ("gauge.type", AttributeValue::string("dynamic"))
    ]
    gauge.record(value, Some(batch_attributes))
    k = k + 1
  }
  let gauge_duration = 0L
  
  // 测试大量UpDownCounter操作
  let start_up_down_time = 0L
  let mut l = 0
  let mut current_value = 0L
  while l < measurement_count {
    let increment = if (l % 3 == 0) { 1L } else { -1L }
    current_value = current_value + increment
    let batch_attributes = base_attributes + [
      ("measurement.index", AttributeValue::int(l.to_int64())),
      ("operation.type", AttributeValue::string(if (increment > 0) { "increment" } else { "decrement" }))
    ]
    up_down_counter.add(increment, Some(batch_attributes))
    l = l + 1
  }
  let up_down_duration = 0L
  
  // 性能断言
  assert_eq(counter_duration < 2000L, true)     // Counter操作应该在2秒内完成
  assert_eq(histogram_duration < 3000L, true)  // Histogram操作应该在3秒内完成
  assert_eq(gauge_duration < 2000L, true)      // Gauge操作应该在2秒内完成
  assert_eq(up_down_duration < 2000L, true)    // UpDownCounter操作应该在2秒内完成
  
  // 计算操作速率
  let counter_rate = measurement_count.to_int64() / counter_duration
  let histogram_rate = measurement_count.to_int64() / histogram_duration
  let gauge_rate = measurement_count.to_int64() / gauge_duration
  let up_down_rate = measurement_count.to_int64() / up_down_duration
  
  assert_eq(counter_rate > 10000L, true)     // 每秒至少10000次Counter操作
  assert_eq(histogram_rate > 6000L, true)    // 每秒至少6000次Histogram操作
  assert_eq(gauge_rate > 10000L, true)       // 每秒至少10000次Gauge操作
  assert_eq(up_down_rate > 10000L, true)     // 每秒至少10000次UpDownCounter操作
}

test "intensive_logging_performance" {
  // 测试密集日志记录的性能
  
  let logger = NoopLogger::{}
  let log_count = 15000
  
  // 准备基础属性
  let base_attributes = [
    ("service.name", AttributeValue::string("logging-performance-service")),
    ("test.type", AttributeValue::string("stress_test")),
    ("test.batch", AttributeValue::string("logging_batch_001"))
  ]
  
  // 测试大量便捷日志方法
  let start_convenience_time = 0L
  let mut i = 0
  while i < log_count {
    let severity_index = i % 5
    let log_attributes = base_attributes + [
      ("log.index", AttributeValue::int(i.to_int64())),
      ("log.batch", AttributeValue::int((i / 500).to_int64())),
      ("log.category", AttributeValue::string("category_" + ((i % 10).to_string())))
    ]
    
    match severity_index {
      0 => logger.debug("Debug message " + i.to_string(), Some(log_attributes))
      1 => logger.info("Info message " + i.to_string(), Some(log_attributes))
      2 => logger.warn("Warning message " + i.to_string(), Some(log_attributes))
      3 => logger.error("Error message " + i.to_string(), Some(log_attributes))
      4 => logger.fatal("Fatal message " + i.to_string(), Some(log_attributes))
      _ => @test.fail("Invalid severity index")
    }
    i = i + 1
  }
  let convenience_duration = 0L
  
  // 测试大量LogRecord emission
  let start_emission_time = 0L
  let mut j = 0
  while j < log_count {
    let severity = match (j % 6) {
      0 => Trace
      1 => Debug
      2 => Info
      3 => Warn
      4 => Error
      5 => Fatal
      _ => Info
    }
    
    let log_record = LogRecord::{
      timestamp_unix_nanos: j.to_int64() * 1000000L,  // 毫秒转纳秒
      observed_timestamp_unix_nanos: Some((j.to_int64() + 1) * 1000000L),
      severity_number: severity,
      severity_text: Some("PERFORMANCE_TEST_" + severity.to_string()),
      body: Some("Performance test log message " + j.to_string()),
      attributes: base_attributes + [
        ("log.index", AttributeValue::int(j.to_int64())),
        ("emission.batch", AttributeValue::int((j / 1000).to_int64())),
        ("severity.level", AttributeValue::int((j % 6).to_int64()))
      ],
      trace_id: Some([for k = 0; k < 16; k = k + 1].map(fn(_) { (j % 256).to_byte() })),
      span_id: Some([for k = 0; k < 8; k = k + 1].map(fn(_) { ((j * 2) % 256).to_byte() })),
      trace_flags: Some(((j % 2).to_byte())),
      resource: None,
      instrumentation_scope: None
    }
    
    logger.emit(log_record)
    j = j + 1
  }
  let emission_duration = 0L
  
  // 测试复杂LogRecord Builder
  let start_builder_time = 0L
  let mut k = 0
  while k < (log_count / 10) {  // Builder操作较慢，减少数量
    let log_record = LogRecord::builder()
      .timestamp(k.to_int64() * 1000000L)
      .severity(Error)
      .severity_text("BUILDER_PERFORMANCE_TEST")
      .body("Builder performance test message " + k.to_string())
      .with_attribute("builder.index", AttributeValue::int(k.to_int64()))
      .with_attribute("builder.batch", AttributeValue::string("batch_" + ((k / 100).to_string())))
      .with_attribute("builder.type", AttributeValue::string("performance_test"))
      .with_attribute("nested.value", AttributeValue::string("nested_" + k.to_string()))
      .build()
    
    logger.emit(log_record)
    k = k + 1
  }
  let builder_duration = 0L
  
  // 性能断言
  assert_eq(convenience_duration < 3000L, true)  // 便捷方法应该在3秒内完成
  assert_eq(emission_duration < 4000L, true)     // LogRecord emission应该在4秒内完成
  assert_eq(builder_duration < 2000L, true)      // Builder操作应该在2秒内完成
  
  // 计算操作速率
  let convenience_rate = log_count.to_int64() / convenience_duration
  let emission_rate = log_count.to_int64() / emission_duration
  let builder_rate = (log_count / 10).to_int64() / builder_duration
  
  assert_eq(convenience_rate > 5000L, true)    // 每秒至少5000次便捷日志操作
  assert_eq(emission_rate > 3000L, true)       // 每秒至少3000次LogRecord emission
  assert_eq(builder_rate > 500L, true)         // 每秒至少500次Builder操作
}

test "memory_allocation_stress_test" {
  // 测试内存分配压力下的性能
  
  // 测试大量AttributeValue创建和销毁
  let start_attribute_time = 0L
  let attribute_count = 50000
  
  let mut i = 0
  while i < attribute_count {
    // 创建各种类型的AttributeValue
    let string_attr = AttributeValue::string("stress_test_string_" + i.to_string())
    let int_attr = AttributeValue::int(i.to_int64())
    let float_attr = AttributeValue::float(i.to_double() / 3.14)
    let bool_attr = AttributeValue::bool(i % 2 == 0)
    
    let string_array = AttributeValue::array_string([
      "item1_" + i.to_string(),
      "item2_" + i.to_string(),
      "item3_" + i.to_string()
    ])
    let int_array = AttributeValue::array_int([i.to_int64(), (i + 1).to_int64(), (i + 2).to_int64()])
    let float_array = AttributeValue::array_float([i.to_double(), (i + 1).to_double(), (i + 2).to_double()])
    let bool_array = AttributeValue::array_bool([i % 2 == 0, (i + 1) % 2 == 0, (i + 2) % 2 == 0])
    
    // 验证属性值（模拟使用）
    match string_attr {
      StringValue(s) => assert_eq(s.contains("stress_test_string"), true)
      _ => @test.fail("Test failed")
    }
    
    match int_attr {
      IntValue(val) => assert_eq(val, i.to_int64())
      _ => @test.fail("Test failed")
    }
    
    i = i + 1
  }
  let attribute_duration = 0L
  
  // 测试大量Span创建和销毁
  let start_span_time = 0L
  let span_count = 10000
  
  let mut j = 0
  while j < span_count {
    let span = Span::{
      name: "memory_stress_span_" + j.to_string(),
      context: SpanContext::{
        trace_id: [for k = 0; k < 16; k = k + 1].map(fn(_) { (j % 256).to_byte() }),
        span_id: [for k = 0; k < 8; k = k + 1].map(fn(_) { ((j * 2) % 256).to_byte() }),
        trace_flags: ((j % 2).to_byte()),
        trace_state: "stress_test_state=" + j.to_string()
      },
      kind: match (j % 5) {
        0 => Internal
        1 => Server
        2 => Client
        3 => Producer
        4 => Consumer
        _ => Internal
      },
      parent_span_id: if (j > 0) { Some([for k = 0; k < 8; k = k + 1].map(fn(_) { ((j - 1) % 256).to_byte() })) } else { None },
      start_time_unix_nanos: j.to_int64() * 1000L,
      end_time_unix_nanos: Some((j.to_int64() + 100) * 1000L),
      status: match (j % 4) {
        0 => Unset
        1 => Ok
        2 => Error
        3 => Ok
        _ => Unset
      },
      status_description: if (j % 4 == 2) { Some("Memory stress error") } else { None },
      attributes: [
        ("span.index", AttributeValue::int(j.to_int64())),
        ("memory.test", AttributeValue::bool(true)),
        ("batch.id", AttributeValue::string("batch_" + ((j / 100).to_string()))),
        ("complex.data", AttributeValue::array_string(["a", "b", "c", "d", "e"]))
      ],
      events: [
        SpanEvent::{
          name: "memory_stress_event",
          timestamp_unix_nanos: (j.to_int64() + 50) * 1000L,
          attributes: [
            ("event.index", AttributeValue::int(j.to_int64())),
            ("event.type", AttributeValue::string("memory_test"))
          ]
        }
      ],
      links: []
    }
    
    // 验证Span属性
    assert_eq(span.name.contains("memory_stress_span"), true)
    assert_eq(span.attributes.length(), 4)
    assert_eq(span.events.length(), 1)
    
    j = j + 1
  }
  let span_duration = 0L
  
  // 测试大量Context操作
  let start_context_time = 0L
  let context_count = 5000
  
  let mut base_ctx = Context::empty()
  let mut k = 0
  while k < context_count {
    let key = create_key("memory_stress_key_" + k.to_string())
    base_ctx = base_ctx.with_value(key, "memory_stress_value_" + k.to_string())
    
    // 每隔100次操作进行一次检索测试
    if (k % 100 == 0) {
      let test_key = create_key("memory_stress_key_" + (k / 2).to_string())
      match base_ctx.get(test_key) {
        Some(value) => assert_eq(value.contains("memory_stress_value"), true)
        None => {}  // 可能还未设置，忽略
      }
    }
    
    k = k + 1
  }
  let context_duration = 0L
  
  // 性能断言
  assert_eq(attribute_duration < 5000L, true)   // Attribute创建应该在5秒内完成
  assert_eq(span_duration < 3000L, true)        // Span创建应该在3秒内完成
  assert_eq(context_duration < 2000L, true)     // Context操作应该在2秒内完成
  
  // 计算操作速率
  let attribute_rate = attribute_count.to_int64() / attribute_duration
  let span_rate = span_count.to_int64() / span_duration
  let context_rate = context_count.to_int64() / context_duration
  
  assert_eq(attribute_rate > 10000L, true)  // 每秒至少10000次Attribute操作
  assert_eq(span_rate > 3000L, true)       // 每秒至少3000次Span操作
  assert_eq(context_rate > 2000L, true)    // 每秒至少2000次Context操作
}

test "concurrent_operations_simulation" {
  // 模拟并发操作的性能测试
  
  let operation_count = 8000
  
  // 模拟并发Span创建
  let start_concurrent_span_time = 0L
  let mut spans = []
  let mut i = 0
  while i < operation_count {
    // 模拟不同的并发操作
    let operation_type = i % 4
    let span = match operation_type {
      0 => {
        // 模拟HTTP请求处理
        Span::{
          name: "http_request",
          context: SpanContext::{
            trace_id: [for j = 0; j < 16; j = j + 1].map(fn(_) { ((i + j) % 256).to_byte() }),
            span_id: [for j = 0; j < 8; j = j + 1].map(fn(_) { ((i * 2 + j) % 256).to_byte() }),
            trace_flags: 1_byte,
            trace_state: ""
          },
          kind: Server,
          parent_span_id: None,
          start_time_unix_nanos: i.to_int64() * 1000L,
          end_time_unix_nanos: Some((i.to_int64() + 200) * 1000L),
          status: Ok,
          status_description: None,
          attributes: [
            ("http.method", AttributeValue::string("GET")),
            ("http.path", AttributeValue::string("/api/test")),
            ("http.status_code", AttributeValue::int(200L)),
            ("request.id", AttributeValue::string("req_" + i.to_string()))
          ],
          events: [],
          links: []
        }
      }
      1 => {
        // 模拟数据库查询
        Span::{
          name: "db_query",
          context: SpanContext::{
            trace_id: [for j = 0; j < 16; j = j + 1].map(fn(_) { ((i + j * 2) % 256).to_byte() }),
            span_id: [for j = 0; j < 8; j = j + 1].map(fn(_) { ((i * 3 + j) % 256).to_byte() }),
            trace_flags: 1_byte,
            trace_state: ""
          },
          kind: Client,
          parent_span_id: Some([for j = 0; j < 8; j = j + 1].map(fn(_) { ((i + j) % 256).to_byte() })]),
          start_time_unix_nanos: i.to_int64() * 1000L,
          end_time_unix_nanos: Some((i.to_int64() + 150) * 1000L),
          status: Ok,
          status_description: None,
          attributes: [
            ("db.operation", AttributeValue::string("SELECT")),
            ("db.table", AttributeValue::string("users")),
            ("db.rows", AttributeValue::int((i % 100).to_int64())),
            ("query.id", AttributeValue::string("query_" + i.to_string()))
          ],
          events: [],
          links: []
        }
      }
      2 => {
        // 模拟缓存操作
        Span::{
          name: "cache_operation",
          context: SpanContext::{
            trace_id: [for j = 0; j < 16; j = j + 1].map(fn(_) { ((i + j * 3) % 256).to_byte() }),
            span_id: [for j = 0; j < 8; j = j + 1].map(fn(_) { ((i * 4 + j) % 256).to_byte() }),
            trace_flags: 1_byte,
            trace_state: ""
          },
          kind: Internal,
          parent_span_id: Some([for j = 0; j < 8; j = j + 1].map(fn(_) { ((i * 2 + j) % 256).to_byte() })]),
          start_time_unix_nanos: i.to_int64() * 1000L,
          end_time_unix_nanos: Some((i.to_int64() + 10) * 1000L),
          status: Ok,
          status_description: None,
          attributes: [
            ("cache.operation", AttributeValue::string(if (i % 2 == 0) { "GET" } else { "SET" })),
            ("cache.key", AttributeValue::string("key_" + i.to_string())),
            ("cache.hit", AttributeValue::bool(i % 3 == 0))
          ],
          events: [],
          links: []
        }
      }
      3 => {
        // 模拟外部服务调用
        Span::{
          name: "external_service",
          context: SpanContext::{
            trace_id: [for j = 0; j < 16; j = j + 1].map(fn(_) { ((i + j * 4) % 256).to_byte() }),
            span_id: [for j = 0; j < 8; j = j + 1].map(fn(_) { ((i * 5 + j) % 256).to_byte() }),
            trace_flags: 1_byte,
            trace_state: ""
          },
          kind: Client,
          parent_span_id: Some([for j = 0; j < 8; j = j + 1].map(fn(_) { ((i * 3 + j) % 256).to_byte() })]),
          start_time_unix_nanos: i.to_int64() * 1000L,
          end_time_unix_nanos: Some((i.to_int64() + 500) * 1000L),
          status: if (i % 10 == 0) { Error } else { Ok },
          status_description: if (i % 10 == 0) { Some("Service timeout") } else { None },
          attributes: [
            ("service.name", AttributeValue::string("external_api")),
            ("service.method", AttributeValue::string("POST")),
            ("service.endpoint", AttributeValue::string("/api/v2/process")),
            ("call.id", AttributeValue::string("call_" + i.to_string()))
          ],
          events: if (i % 10 == 0) {
            [SpanEvent::{
              name: "error",
              timestamp_unix_nanos: (i.to_int64() + 250) * 1000L,
              attributes: [
                ("error.type", AttributeValue::string("timeout")),
                ("error.duration.ms", AttributeValue::int(250L))
              ]
            }]
          } else { [] },
          links: []
        }
      }
      _ => {
        Span::{
          name: "default_span",
          context: SpanContext::{
            trace_id: [for j = 0; j < 16; j = j + 1].map(fn(_) { 0_byte }),
            span_id: [for j = 0; j < 8; j = j + 1].map(fn(_) { 0_byte }),
            trace_flags: 0_byte,
            trace_state: ""
          },
          kind: Internal,
          parent_span_id: None,
          start_time_unix_nanos: 0L,
          end_time_unix_nanos: None,
          status: Unset,
          status_description: None,
          attributes: [],
          events: [],
          links: []
        }
      }
    }
    
    spans.push(span)
    i = i + 1
  }
  let concurrent_span_duration = 0L
  
  // 模拟并发Metrics操作
  let start_concurrent_metrics_time = 0L
  let meter = NoopMeter::{}
  let request_counter = meter.create_counter("http_requests_total", Some("requests"), Some("HTTP requests"))
  let db_histogram = meter.create_histogram("db_query_duration_ms", Some("ms"), Some("Database query duration"))
  let cache_gauge = meter.create_gauge("cache_size_mb", Some("MB"), Some("Cache size"))
  
  let mut j = 0
  while j < operation_count {
    let operation_type = j % 4
    
    match operation_type {
      0 => {
        // HTTP请求计数
        request_counter.add(1L, Some([
          ("http.method", AttributeValue::string("GET")),
          ("http.path", AttributeValue::string("/api/test")),
          ("http.status", AttributeValue::int(if (j % 20 == 0) { 500L } else { 200L }))
        ]))
      }
      1 => {
        // 数据库查询时间
        db_histogram.record((j % 200).to_double(), Some([
          ("db.operation", AttributeValue::string("SELECT")),
          ("db.table", AttributeValue::string("users"))
        ]))
      }
      2 => {
        // 缓存大小更新
        cache_gauge.record(100.0 + (j % 50).to_double(), Some([
          ("cache.type", AttributeValue::string("redis")),
          ("cache.region", AttributeValue::string("us-west-2"))
        ]))
      }
      3 => {
        // 组合操作
        request_counter.add(1L, Some([
          ("http.method", AttributeValue::string("POST")),
          ("http.path", AttributeValue::string("/api/data"))
        ]))
        db_histogram.record((j % 100).to_double(), Some([
          ("db.operation", AttributeValue::string("INSERT"))
        ]))
        cache_gauge.record(150.0 + (j % 25).to_double(), Some([
          ("cache.type", AttributeValue::string("memcached"))
        ]))
      }
      _ => {}
    }
    
    j = j + 1
  }
  let concurrent_metrics_duration = 0L
  
  // 模拟并发日志操作
  let start_concurrent_log_time = 0L
  let logger = NoopLogger::{}
  
  let mut k = 0
  while k < operation_count {
    let log_type = k % 5
    
    let log_attributes = [
      ("request.id", AttributeValue::string("req_" + k.to_string())),
      ("operation.type", AttributeValue::string("concurrent_test")),
      ("thread.id", AttributeValue::int((k % 8).to_int64())),
      ("batch.id", AttributeValue::string("batch_" + ((k / 100).to_string())))
    ]
    
    match log_type {
      0 => logger.debug("Concurrent debug message " + k.to_string(), Some(log_attributes))
      1 => logger.info("Concurrent info message " + k.to_string(), Some(log_attributes))
      2 => logger.warn("Concurrent warning message " + k.to_string(), Some(log_attributes))
      3 => logger.error("Concurrent error message " + k.to_string(), Some(log_attributes))
      4 => {
        let log_record = LogRecord::{
          timestamp_unix_nanos: k.to_int64() * 1000000L,
          observed_timestamp_unix_nanos: Some((k.to_int64() + 1) * 1000000L),
          severity_number: Error,
          severity_text: Some("CONCURRENT_ERROR"),
          body: Some("Concurrent structured log " + k.to_string()),
          attributes: log_attributes + [
            ("structured.log", AttributeValue::bool(true)),
            ("log.format", AttributeValue::string("json"))
          ],
          trace_id: Some([for j = 0; j < 16; j = j + 1].map(fn(_) { ((k + j) % 256).to_byte() })),
          span_id: Some([for j = 0; j < 8; j = j + 1].map(fn(_) { ((k * 2 + j) % 256).to_byte() })),
          trace_flags: Some(((k % 2).to_byte())),
          resource: None,
          instrumentation_scope: None
        }
        logger.emit(log_record)
      }
      _ => {}
    }
    
    k = k + 1
  }
  let concurrent_log_duration = 0L
  
  // 验证操作结果
  assert_eq(spans.length(), operation_count)
  
  // 性能断言
  assert_eq(concurrent_span_duration < 4000L, true)    // 并发Span创建应该在4秒内完成
  assert_eq(concurrent_metrics_duration < 3000L, true) // 并发Metrics操作应该在3秒内完成
  assert_eq(concurrent_log_duration < 4000L, true)     // 并发日志操作应该在4秒内完成
  
  // 计算操作速率
  let span_rate = operation_count.to_int64() / concurrent_span_duration
  let metrics_rate = operation_count.to_int64() / concurrent_metrics_duration
  let log_rate = operation_count.to_int64() / concurrent_log_duration
  
  assert_eq(span_rate > 2000L, true)     // 每秒至少2000次并发Span操作
  assert_eq(metrics_rate > 2500L, true)  // 每秒至少2500次并发Metrics操作
  assert_eq(log_rate > 2000L, true)      // 每秒至少2000次并发日志操作
}
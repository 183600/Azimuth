// 数据一致性测试 - 验证跨模块遥测数据的一致性
test "telemetry_cross_module_consistency" {
  // 测试目标：验证不同模块间遥测数据的一致性
  
  // 1. 创建统一的Trace ID用于跨模块追踪
  let unified_trace_id = [0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10]
  
  // 2. 模拟模块A的遥测数据
  let module_a_span = @azimuth.telemetry.api.trace.Span{
    name: "module-a-operation",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: unified_trace_id,
      span_id: [0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00, 0x11],
      trace_flags: 0x01,
      trace_state: "module=a"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000,
    end_time_unix_nanos: Some(1640995200100000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: [
      ("module.name", @azimuth.telemetry.api.common.AttributeValue::string("module-a")),
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("data-processing")),
      ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user-12345")),
      ("session.id", @azimuth.telemetry.api.common.AttributeValue::string("session-67890"))
    ],
    events: [
      @azimuth.telemetry.api.trace.SpanEvent{
        name: "data-received",
        timestamp_unix_nanos: 1640995200020000000,
        attributes: [
          ("data.size", @azimuth.telemetry.api.common.AttributeValue::int(1024)),
          ("data.type", @azimuth.telemetry.api.common.AttributeValue::string("json"))
        ]
      }
    ],
    links: []
  }
  
  // 3. 模拟模块B的遥测数据
  let module_b_span = @azimuth.telemetry.api.trace.Span{
    name: "module-b-operation",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: unified_trace_id, // 相同的trace_id确保关联性
      span_id: [0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99],
      trace_flags: 0x01,
      trace_state: "module=b"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Client,
    parent_span_id: Some([0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00, 0x11]), // 指向模块A的span
    start_time_unix_nanos: 1640995200050000000,
    end_time_unix_nanos: Some(1640995200080000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: [
      ("module.name", @azimuth.telemetry.api.common.AttributeValue::string("module-b")),
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("data-validation")),
      ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user-12345")), // 相同的用户ID
      ("session.id", @azimuth.telemetry.api.common.AttributeValue::string("session-67890")), // 相同的会话ID
      ("parent.module", @azimuth.telemetry.api.common.AttributeValue::string("module-a"))
    ],
    events: [
      @azimuth.telemetry.api.trace.SpanEvent{
        name: "validation-started",
        timestamp_unix_nanos: 1640995200055000000,
        attributes: [
          ("validation.rules", @azimuth.telemetry.api.common.AttributeValue::int(5))
        ]
      },
      @azimuth.telemetry.api.trace.SpanEvent{
        name: "validation-completed",
        timestamp_unix_nanos: 1640995200075000000,
        attributes: [
          ("validation.result", @azimuth.telemetry.api.common.AttributeValue::bool(true)),
          ("validation.errors", @azimuth.telemetry.api.common.AttributeValue::int(0))
        ]
      }
    ],
    links: []
  }
  
  // 4. 验证跨模块数据一致性
  // 4.1 验证Trace ID一致性
  assert_eq(module_a_span.context.trace_id, module_b_span.context.trace_id, "跨模块Span应该有相同的Trace ID")
  
  // 4.2 验证父子关系
  assert_eq(module_b_span.parent_span_id.unwrap(), module_a_span.context.span_id, "模块B应该是模块A的子Span")
  
  // 4.3 验证时间序列一致性
  assert(module_b_span.start_time_unix_nanos >= module_a_span.start_time_unix_nanos, "子Span开始时间应该晚于或等于父Span开始时间")
  assert(module_b_span.end_time_unix_nanos.unwrap() <= module_a_span.end_time_unix_nanos.unwrap(), "子Span结束时间应该早于或等于父Span结束时间")
  
  // 4.4 验证关键属性一致性
  let module_a_user_id = module_a_span.attributes.find(fn((key, _)) { key == "user.id" })
  let module_b_user_id = module_b_span.attributes.find(fn((key, _)) { key == "user.id" })
  
  match (module_a_user_id, module_b_user_id) {
    (Some((_, @azimuth.telemetry.api.common.AttributeValue::StringValue(a_id))), 
     Some((_, @azimuth.telemetry.api.common.AttributeValue::StringValue(b_id)))) => {
      assert_eq(a_id, b_id, "跨模块的用户ID应该一致")
    }
    _ => assert(false, "用户ID属性应该存在且为字符串类型")
  }
  
  // 5. 创建跨模块的度量数据
  let module_a_metrics = [
    @azimuth.telemetry.api.metrics.Measurement{
      value: 100.0,
      attributes: [
        ("module.name", @azimuth.telemetry.api.common.AttributeValue::string("module-a")),
        ("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("processing.time")),
        ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user-12345")),
        ("trace.id", @azimuth.telemetry.api.common.AttributeValue::array_string(["0123456789abcdeffedcba9876543210"]))
      ]
    }
  ]
  
  let module_b_metrics = [
    @azimuth.telemetry.api.metrics.Measurement{
      value: 3.0,
      attributes: [
        ("module.name", @azimuth.telemetry.api.common.AttributeValue::string("module-b")),
        ("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("validation.time")),
        ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user-12345")),
        ("trace.id", @azimuth.telemetry.api.common.AttributeValue::array_string(["0123456789abcdeffedcba9876543210"]))
      }
    }
  ]
  
  // 6. 验证跨模块度量一致性
  let a_trace_id = module_a_metrics[0].attributes.find(fn((key, _)) { key == "trace.id" })
  let b_trace_id = module_b_metrics[0].attributes.find(fn((key, _)) { key == "trace.id" })
  
  match (a_trace_id, b_trace_id) {
    (Some((_, @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(a_trace))), 
     Some((_, @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(b_trace)))) => {
      assert_eq(a_trace, b_trace, "跨模块度量的Trace ID应该一致")
    }
    _ => assert(false, "Trace ID属性应该存在且为字符串数组类型")
  }
  
  println("✓ 跨模块数据一致性测试通过")
}

test "telemetry_temporal_consistency" {
  // 测试目标：验证遥测数据的时间一致性
  
  // 1. 创建具有明确时间关系的Span序列
  let base_time = 1640995200000000000 // 2022-01-01 00:00:00 UTC
  
  let span_sequence = [
    @azimuth.telemetry.api.trace.Span{
      name: "initial-operation",
      context: @azimuth.telemetry.api.trace.SpanContext{
        trace_id: [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00],
        span_id: [0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01],
        trace_flags: 0x01,
        trace_state: "sequence=1"
      },
      kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
      parent_span_id: None,
      start_time_unix_nanos: base_time,
      end_time_unix_nanos: Some(base_time + 1000000000), // 1秒后
      status: @azimuth.telemetry.api.trace.StatusCode::Ok,
      status_description: None,
      attributes: [("sequence.order", @azimuth.telemetry.api.common.AttributeValue::int(1))],
      events: [],
      links: []
    },
    @azimuth.telemetry.api.trace.Span{
      name: "sub-operation-1",
      context: @azimuth.telemetry.api.trace.SpanContext{
        trace_id: [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00],
        span_id: [0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02],
        trace_flags: 0x01,
        trace_state: "sequence=2"
      },
      kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
      parent_span_id: Some([0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01]),
      start_time_unix_nanos: base_time + 200000000, // 0.2秒后
      end_time_unix_nanos: Some(base_time + 800000000), // 0.8秒后
      status: @azimuth.telemetry.api.trace.StatusCode::Ok,
      status_description: None,
      attributes: [("sequence.order", @azimuth.telemetry.api.common.AttributeValue::int(2))],
      events: [],
      links: []
    },
    @azimuth.telemetry.api.trace.Span{
      name: "final-operation",
      context: @azimuth.telemetry.api.trace.SpanContext{
        trace_id: [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00],
        span_id: [0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03],
        trace_flags: 0x01,
        trace_state: "sequence=3"
      },
      kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
      parent_span_id: None,
      start_time_unix_nanos: base_time + 1200000000, // 1.2秒后
      end_time_unix_nanos: Some(base_time + 2200000000), // 2.2秒后
      status: @azimuth.telemetry.api.trace.StatusCode::Ok,
      status_description: None,
      attributes: [("sequence.order", @azimuth.telemetry.api.common.AttributeValue::int(3))],
      events: [],
      links: []
    }
  ]
  
  // 2. 验证时间序列一致性
  assert_eq(span_sequence.length(), 3)
  
  // 验证Span开始时间的单调性
  for i = 1; i < span_sequence.length(); i = i + 1 {
    let prev_span = span_sequence[i - 1]
    let curr_span = span_sequence[i]
    
    // 检查开始时间
    assert(curr_span.start_time_unix_nanos >= prev_span.start_time_unix_nanos, 
           "Span \{i}的开始时间应该晚于或等于Span \{i-1}的开始时间")
    
    // 检查结束时间存在性
    assert(curr_span.end_time_unix_nanos != None, "Span应该有结束时间")
    
    // 检查持续时间合理性
    let duration = curr_span.end_time_unix_nanos.unwrap() - curr_span.start_time_unix_nanos
    assert(duration > 0, "Span持续时间应该大于0")
  }
  
  // 3. 验证父子时间关系
  let parent_span = span_sequence[0]
  let child_span = span_sequence[1]
  
  assert(child_span.start_time_unix_nanos >= parent_span.start_time_unix_nanos, 
         "子Span开始时间应该晚于或等于父Span开始时间")
  assert(child_span.end_time_unix_nanos.unwrap() <= parent_span.end_time_unix_nanos.unwrap(), 
         "子Span结束时间应该早于或等于父Span结束时间")
  
  // 4. 创建具有时间戳的事件序列
  let time_ordered_events = [
    @azimuth.telemetry.api.trace.SpanEvent{
      name: "operation-started",
      timestamp_unix_nanos: base_time + 100000000, // 0.1秒后
      attributes: [("event.phase", @azimuth.telemetry.api.common.AttributeValue::string("initialization"))]
    },
    @azimuth.telemetry.api.trace.SpanEvent{
      name: "processing-started",
      timestamp_unix_nanos: base_time + 300000000, // 0.3秒后
      attributes: [("event.phase", @azimuth.telemetry.api.common.AttributeValue::string("processing"))]
    },
    @azimuth.telemetry.api.trace.SpanEvent{
      name: "operation-completed",
      timestamp_unix_nanos: base_time + 900000000, // 0.9秒后
      attributes: [("event.phase", @azimuth.telemetry.api.common.AttributeValue::string("completion"))]
    }
  ]
  
  // 5. 验证事件时间序列
  assert_eq(time_ordered_events.length(), 3)
  
  for i = 1; i < time_ordered_events.length(); i = i + 1 {
    let prev_event = time_ordered_events[i - 1]
    let curr_event = time_ordered_events[i]
    
    assert(curr_event.timestamp_unix_nanos > prev_event.timestamp_unix_nanos, 
           "事件 \{i}的时间戳应该晚于事件 \{i-1}的时间戳")
  }
  
  // 6. 验证度量数据的时间一致性
  let time_series_metrics = [
    @azimuth.telemetry.api.metrics.Measurement{
      value: 10.0,
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("counter")),
        ("timestamp", @azimuth.telemetry.api.common.AttributeValue::int((base_time / 1000000).to_int64())), // 转换为毫秒
        ("sequence", @azimuth.telemetry.api.common.AttributeValue::int(1))
      ]
    },
    @azimuth.telemetry.api.metrics.Measurement{
      value: 25.0,
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("counter")),
        ("timestamp", @azimuth.telemetry.api.common.AttributeValue::int(((base_time + 500000000) / 1000000).to_int64())), // 0.5秒后
        ("sequence", @azimuth.telemetry.api.common.AttributeValue::int(2))
      ]
    },
    @azimuth.telemetry.api.metrics.Measurement{
      value: 40.0,
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("counter")),
        ("timestamp", @azimuth.telemetry.api.common.AttributeValue::int(((base_time + 1000000000) / 1000000).to_int64())), // 1秒后
        ("sequence", @azimuth.telemetry.api.common.AttributeValue::int(3))
      ]
    }
  ]
  
  // 7. 验证时间序列度量的单调性
  for i = 1; i < time_series_metrics.length(); i = i + 1 {
    let prev_metric = time_series_metrics[i - 1]
    let curr_metric = time_series_metrics[i]
    
    let prev_timestamp = prev_metric.attributes.find(fn((key, _)) { key == "timestamp" })
    let curr_timestamp = curr_metric.attributes.find(fn((key, _)) { key == "timestamp" })
    
    match (prev_timestamp, curr_timestamp) {
      (Some((_, @azimuth.telemetry.api.common.AttributeValue::IntValue(prev_time))), 
       Some((_, @azimuth.telemetry.api.common.AttributeValue::IntValue(curr_time)))) => {
        assert(curr_time > prev_time, "度量 \{i}的时间戳应该晚于度量 \{i-1}的时间戳")
      }
      _ => assert(false, "时间戳属性应该存在且为整数类型")
    }
  }
  
  println("✓ 时间一致性测试通过")
}

test "telemetry_attribute_consistency" {
  // 测试目标：验证遥测属性的类型和命名一致性
  
  // 1. 定义标准的属性规范
  let standard_attributes = [
    ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("test-service")),
    ("service.version", @azimuth.telemetry.api.common.AttributeValue::string("1.0.0")),
    ("service.instance.id", @azimuth.telemetry.api.common.AttributeValue::string("instance-12345")),
    ("operation.name", @azimuth.telemetry.api.common.AttributeValue::string("test-operation")),
    ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("http")),
    ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user-67890")),
    ("session.id", @azimuth.telemetry.api.common.AttributeValue::string("session-abcde")),
    ("trace.id", @azimuth.telemetry.api.common.AttributeValue::array_string(["0123456789abcdef"])),
    ("span.id", @azimuth.telemetry.api.common.AttributeValue::array_string(["fedcba9876543210"])),
    ("http.method", @azimuth.telemetry.api.common.AttributeValue::string("GET")),
    ("http.status_code", @azimuth.telemetry.api.common.AttributeValue::int(200)),
    ("http.url", @azimuth.telemetry.api.common.AttributeValue::string("https://example.com/api/test")),
    ("http.response_time_ms", @azimuth.telemetry.api.common.AttributeValue::int(150)),
    ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("none")),
    ("error.message", @azimuth.telemetry.api.common.AttributeValue::string("")),
    ("cpu.usage", @azimuth.telemetry.api.common.AttributeValue::float(75.5)),
    ("memory.usage_mb", @azimuth.telemetry.api.common.AttributeValue::int(512)),
    ("disk.usage_percent", @azimuth.telemetry.api.common.AttributeValue::float(45.2)),
    ("network.incoming_bytes", @azimuth.telemetry.api.common.AttributeValue::int(1024000)),
    ("network.outgoing_bytes", @azimuth.telemetry.api.common.AttributeValue::int(512000)),
    ("cache.hit_rate", @azimuth.telemetry.api.common.AttributeValue::float(0.85)),
    ("queue.length", @azimuth.telemetry.api.common.AttributeValue::int(25)),
    ("batch.size", @azimuth.telemetry.api.common.AttributeValue::int(100)),
    ("retry.count", @azimuth.telemetry.api.common.AttributeValue::int(0)),
    ("timeout.enabled", @azimuth.telemetry.api.common.AttributeValue::bool(true)),
    ("compression.enabled", @azimuth.telemetry.api.common.AttributeValue::bool(false))
  ]
  
  // 2. 创建包含标准属性的Span
  let span_with_standard_attrs = @azimuth.telemetry.api.trace.Span{
    name: "attribute-consistency-test",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10],
      span_id: [0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18],
      trace_flags: 0x01,
      trace_state: "attr=test"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000,
    end_time_unix_nanos: Some(1640995200100000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: standard_attributes,
    events: [],
    links: []
  }
  
  // 3. 验证属性类型一致性
  for (key, expected_value) in standard_attributes {
    let found_attr = span_with_standard_attrs.attributes.find(fn((k, _) { k == key })
    assert(found_attr != None, "应该找到属性：\{key}")
    
    match found_attr {
      Some((_, actual_value)) => {
        // 验证类型一致性
        match (expected_value, actual_value) {
          (@azimuth.telemetry.api.common.AttributeValue::StringValue(_), 
           @azimuth.telemetry.api.common.AttributeValue::StringValue(_)) => ()
          (@azimuth.telemetry.api.common.AttributeValue::IntValue(_), 
           @azimuth.telemetry.api.common.AttributeValue::IntValue(_)) => ()
          (@azimuth.telemetry.api.common.AttributeValue::FloatValue(_), 
           @azimuth.telemetry.api.common.AttributeValue::FloatValue(_)) => ()
          (@azimuth.telemetry.api.common.AttributeValue::BoolValue(_), 
           @azimuth.telemetry.api.common.AttributeValue::BoolValue(_)) => ()
          (@azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(_), 
           @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(_)) => ()
          (@azimuth.telemetry.api.common.AttributeValue::ArrayIntValue(_), 
           @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue(_)) => ()
          (@azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue(_), 
           @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue(_)) => ()
          (@azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue(_), 
           @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue(_)) => ()
          _ => assert(false, "属性 \{key} 的类型不一致")
        }
      }
      None => assert(false, "属性查找失败")
    }
  }
  
  // 4. 创建包含相同属性的度量数据
  let metrics_with_standard_attrs = standard_attributes.map(fn((key, value)) {
    @azimuth.telemetry.api.metrics.Measurement{
      value: 100.0,
      attributes: [(key, value)]
    }
  })
  
  // 5. 验证度量属性的一致性
  assert_eq(metrics_with_standard_attrs.length(), standard_attributes.length())
  
  for i = 0; i < metrics_with_standard_attrs.length(); i = i + 1 {
    let metric = metrics_with_standard_attrs[i]
    let (expected_key, expected_value) = standard_attributes[i]
    
    assert_eq(metric.attributes.length(), 1)
    let (actual_key, actual_value) = metric.attributes[0]
    
    assert_eq(actual_key, expected_key)
    
    // 验证类型一致性（同上）
    match (expected_value, actual_value) {
      (@azimuth.telemetry.api.common.AttributeValue::StringValue(_), 
       @azimuth.telemetry.api.common.AttributeValue::StringValue(_)) => ()
      (@azimuth.telemetry.api.common.AttributeValue::IntValue(_), 
       @azimuth.telemetry.api.common.AttributeValue::IntValue(_)) => ()
      (@azimuth.telemetry.api.common.AttributeValue::FloatValue(_), 
       @azimuth.telemetry.api.common.AttributeValue::FloatValue(_)) => ()
      (@azimuth.telemetry.api.common.AttributeValue::BoolValue(_), 
       @azimuth.telemetry.api.common.AttributeValue::BoolValue(_)) => ()
      (@azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(_), 
       @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(_)) => ()
      (@azimuth.telemetry.api.common.AttributeValue::ArrayIntValue(_), 
       @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue(_)) => ()
      (@azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue(_), 
       @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue(_)) => ()
      (@azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue(_), 
       @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue(_)) => ()
      _ => assert(false, "度量属性 \{actual_key} 的类型不一致")
    }
  }
  
  // 6. 验证属性命名规范
  let naming_conventions = [
    "service.name", "service.version", "service.instance.id",
    "operation.name", "operation.type", "user.id", "session.id",
    "trace.id", "span.id", "http.method", "http.status_code",
    "http.url", "http.response_time_ms", "error.type", "error.message",
    "cpu.usage", "memory.usage_mb", "disk.usage_percent",
    "network.incoming_bytes", "network.outgoing_bytes", "cache.hit_rate",
    "queue.length", "batch.size", "retry.count", "timeout.enabled",
    "compression.enabled"
  ]
  
  for convention in naming_conventions {
    let attr_exists = standard_attributes.any(fn((key, _) { key == convention }))
    assert(attr_exists, "应该遵循命名规范：\{convention}")
  }
  
  println("✓ 属性一致性测试通过")
}
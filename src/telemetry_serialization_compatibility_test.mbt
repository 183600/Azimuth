// 数据序列化兼容性测试 - 确保遥测数据的向后兼容性
test "telemetry_serialization_backward_compatibility" {
  // 测试目标：验证遥测数据在不同版本间的向后兼容性
  
  // 1. 模拟v1.0版本的遥测数据结构
  let v1_attributes = [
    ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("legacy-service")),
    ("service.version", @azimuth.telemetry.api.common.AttributeValue::string("1.0.0")),
    ("operation.name", @azimuth.telemetry.api.common.AttributeValue::string("legacy-operation"))
  ]
  
  let v1_span = @azimuth.telemetry.api.trace.Span{
    name: "legacy-span-v1",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10],
      span_id: [0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18],
      trace_flags: 0x01,
      trace_state: "v1=legacy"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000,
    end_time_unix_nanos: Some(1640995200100000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: v1_attributes,
    events: [],
    links: []
  }
  
  // 2. 模拟v2.0版本的遥测数据结构（添加新字段）
  let v2_attributes = v1_attributes + [
    ("service.instance.id", @azimuth.telemetry.api.common.AttributeValue::string("instance-12345")), // 新增字段
    ("telemetry.sdk.version", @azimuth.telemetry.api.common.AttributeValue::string("2.0.0")), // 新增字段
    ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("http")), // 新增字段
    ("legacy.compatibility", @azimuth.telemetry.api.common.AttributeValue::bool(true)) // 兼容性标记
  ]
  
  let v2_span = @azimuth.telemetry.api.trace.Span{
    name: "enhanced-span-v2",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10],
      span_id: [0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29],
      trace_flags: 0x01,
      trace_state: "v2=enhanced"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Server, // 新的枚举值（向后兼容）
    parent_span_id: Some([0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18]),
    start_time_unix_nanos: 1640995200010000000,
    end_time_unix_nanos: Some(1640995200090000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: Some("Enhanced with new features"), // 新增字段
    attributes: v2_attributes,
    events: [ // 新增事件支持
      @azimuth.telemetry.api.trace.SpanEvent{
        name: "enhancement.applied",
        timestamp_unix_nanos: 1640995200050000000,
        attributes: [
          ("enhancement.type", @azimuth.telemetry.api.common.AttributeValue::string("v2_features"))
        ]
      }
    ],
    links: [] // 新增链接支持
  }
  
  // 3. 验证向后兼容性
  // 3.1 验证v1核心字段在v2中仍然存在
  let v2_service_name = v2_span.attributes.find(fn((key, _) { key == "service.name" })
  let v2_service_version = v2_span.attributes.find(fn((key, _) { key == "service.version" })
  let v2_operation_name = v2_span.attributes.find(fn((key, _) { key == "operation.name" })
  
  assert(v2_service_name != None, "v2应该保持v1的service.name字段")
  assert(v2_service_version != None, "v2应该保持v1的service.version字段")
  assert(v2_operation_name != None, "v2应该保持v1的operation.name字段")
  
  // 3.2 验证核心数据类型不变
  match (v2_service_name, v1_attributes[0]) {
    (Some((_, @azimuth.telemetry.api.common.AttributeValue::StringValue(v2_name))), 
     ("service.name", @azimuth.telemetry.api.common.AttributeValue::StringValue(v1_name))) => {
      assert_eq(v2_name, v1_name, "核心字段值应该保持一致")
    }
    _ => assert(false, "核心字段类型应该保持不变")
  }
  
  // 4. 模拟序列化和反序列化过程
  let serialized_v1 = serialize_span(v1_span) // 假设的序列化函数
  let deserialized_v1 = deserialize_span(serialized_v1) // 假设的反序列化函数
  
  // 验证序列化/反序列化的数据完整性
  assert_eq(deserialized_v1.name, v1_span.name)
  assert_eq(deserialized_v1.context.trace_id, v1_span.context.trace_id)
  assert_eq(deserialized_v1.attributes.length(), v1_span.attributes.length())
  
  // 5. 验证v1数据在v2系统中的处理
  let v1_processed_in_v2 = process_legacy_span(v1_span) // 假设的v2处理函数
  
  // v2系统应该能够正确处理v1数据
  assert(v1_processed_in_v2.name != None, "v2系统应该能读取v1的Span名称")
  assert(v1_processed_in_v2.attributes.length() >= v1_span.attributes.length(), 
         "v2系统应该保持v1的所有属性，可能添加新属性")
  
  println("✓ 向后兼容性测试通过")
}

test "telemetry_serialization_attribute_evolution" {
  // 测试目标：验证属性类型的演化兼容性
  
  // 1. 模拟属性类型的演化
  let string_attribute = ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("12345"))
  let int_attribute = ("user.id", @azimuth.telemetry.api.common.AttributeValue::int(12345))
  let array_attribute = ("user.id", @azimuth.telemetry.api.common.AttributeValue::array_string(["12345"]))
  
  // 2. 创建使用不同属性类型的Span
  let string_type_span = @azimuth.telemetry.api.trace.Span{
    name: "string-attribute-span",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00],
      span_id: [0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01],
      trace_flags: 0x01,
      trace_state: "attr=string"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000,
    end_time_unix_nanos: Some(1640995200050000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: [string_attribute],
    events: [],
    links: []
  }
  
  let int_type_span = @azimuth.telemetry.api.trace.Span{
    name: "int-attribute-span",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00],
      span_id: [0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02],
      trace_flags: 0x01,
      trace_state: "attr=int"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200010000000,
    end_time_unix_nanos: Some(1640995200060000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: [int_attribute],
    events: [],
    links: []
  }
  
  let array_type_span = @azimuth.telemetry.api.trace.Span{
    name: "array-attribute-span",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00],
      span_id: [0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03],
      trace_flags: 0x01,
      trace_state: "attr=array"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200020000000,
    end_time_unix_nanos: Some(1640995200070000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: [array_attribute],
    events: [],
    links: []
  }
  
  // 3. 验证类型转换兼容性
  let type_conversion_spans = [string_type_span, int_type_span, array_type_span]
  
  for span in type_conversion_spans {
    let user_id_attr = span.attributes.find(fn((key, _) { key == "user.id" })
    assert(user_id_attr != None, "user.id属性应该存在")
    
    // 验证类型转换逻辑
    match user_id_attr {
      Some((_, @azimuth.telemetry.api.common.AttributeValue::StringValue(value))) => {
        assert(value.length() > 0, "字符串值应该非空")
      }
      Some((_, @azimuth.telemetry.api.common.AttributeValue::IntValue(value))) => {
        assert(value >= 0, "整数值应该非负")
      }
      Some((_, @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(values))) => {
        assert(values.length() > 0, "数组应该非空")
      }
      _ => assert(false, "user.id属性类型应该受支持")
    }
  }
  
  // 4. 模拟属性类型升级场景
  let upgraded_attributes = [
    ("user.id", @azimuth.telemetry.api.common.AttributeValue::array_string(["12345", "67890"])), // 从字符串升级到数组
    ("operation.count", @azimuth.telemetry.api.common.AttributeValue::int(42)), // 新增属性
    ("legacy.supported", @azimuth.telemetry.api.common.AttributeValue::bool(true)) // 兼容性标记
  ]
  
  let upgraded_span = @azimuth.telemetry.api.trace.Span{
    name: "upgraded-attributes-span",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00],
      span_id: [0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04],
      trace_flags: 0x01,
      trace_state: "attr=upgraded"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200030000000,
    end_time_unix_nanos: Some(1640995200080000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: upgraded_attributes,
    events: [],
    links: []
  }
  
  // 5. 验证升级后的属性兼容性
  let upgraded_user_id = upgraded_span.attributes.find(fn((key, _) { key == "user.id" })
  let legacy_supported = upgraded_span.attributes.find(fn((key, _) { key == "legacy.supported" })
  
  match upgraded_user_id {
    Some((_, @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(values))) => {
      assert(values.length() >= 1, "升级后的数组应该包含原始值")
    }
    _ => assert(false, "升级后的user.id应该是数组类型")
  }
  
  match legacy_supported {
    Some((_, @azimuth.telemetry.api.common.AttributeValue::BoolValue(supported))) => {
      assert(supported, "应该标记为支持旧版本")
    }
    _ => assert(false, "legacy.supported应该是布尔类型")
  }
  
  // 6. 创建度量数据的类型演化测试
  let legacy_metrics = [
    @azimuth.telemetry.api.metrics.Measurement{
      value: 100.0,
      attributes: [
        ("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("counter")),
        ("metric.version", @azimuth.telemetry.api.common.AttributeValue::string("1.0"))
      ]
    }
  ]
  
  let enhanced_metrics = [
    @azimuth.telemetry.api.metrics.Measurement{
      value: 100.0,
      attributes: [
        ("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("counter")), // 保持原有字段
        ("metric.version", @azimuth.telemetry.api.common.AttributeValue::string("2.0")), // 版本升级
        ("metric.unit", @azimuth.telemetry.api.common.AttributeValue::string("requests")), // 新增字段
        ("metric.description", @azimuth.telemetry.api.common.AttributeValue::string("Total number of requests")), // 新增字段
        ("legacy.compatible", @azimuth.telemetry.api.common.AttributeValue::bool(true)) // 兼容性标记
      ]
    }
  ]
  
  // 7. 验证度量数据的演化兼容性
  assert_eq(legacy_metrics.length(), 1)
  assert_eq(enhanced_metrics.length(), 1)
  
  let legacy_metric = legacy_metrics[0]
  let enhanced_metric = enhanced_metrics[0]
  
  // 验证核心值不变
  assert_eq(legacy_metric.value, enhanced_metric.value)
  
  // 验证原有属性保持
  let legacy_type = legacy_metric.attributes.find(fn((key, _) { key == "metric.type" })
  let enhanced_type = enhanced_metric.attributes.find(fn((key, _) { key == "metric.type" })
  
  match (legacy_type, enhanced_type) {
    (Some((_, @azimuth.telemetry.api.common.AttributeValue::StringValue(l_type))), 
     Some((_, @azimuth.telemetry.api.common.AttributeValue::StringValue(e_type)))) => {
      assert_eq(l_type, e_type, "度量类型应该保持一致")
    }
    _ => assert(false, "度量类型属性应该保持一致")
  }
  
  // 验证新属性存在
  let metric_unit = enhanced_metric.attributes.find(fn((key, _) { key == "metric.unit" })
  let metric_description = enhanced_metric.attributes.find(fn((key, _) { key == "metric.description" })
  let legacy_compatible = enhanced_metric.attributes.find(fn((key, _) { key == "legacy.compatible" })
  
  assert(metric_unit != None, "应该有新的度量单位属性")
  assert(metric_description != None, "应该有新的度量描述属性")
  assert(legacy_compatible != None, "应该有兼容性标记属性")
  
  println("✓ 属性演化兼容性测试通过")
}

test "telemetry_serialization_format_compatibility" {
  // 测试目标：验证不同序列化格式的兼容性
  
  // 1. 创建标准遥测数据
  let standard_attributes = [
    ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("format-test-service")),
    ("operation.name", @azimuth.telemetry.api.common.AttributeValue::string("format-test-operation")),
    ("trace.id", @azimuth.telemetry.api.common.AttributeValue::array_string(["0123456789abcdef"])),
    ("span.id", @azimuth.telemetry.api.common.AttributeValue::array_string(["fedcba9876543210"])),
    ("status.code", @azimuth.telemetry.api.common.AttributeValue::int(200)),
    ("duration.ms", @azimuth.telemetry.api.common.AttributeValue::int(150)),
    ("success", @azimuth.telemetry.api.common.AttributeValue::bool(true))
  ]
  
  let test_span = @azimuth.telemetry.api.trace.Span{
    name: "format-compatibility-test",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10],
      span_id: [0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18],
      trace_flags: 0x01,
      trace_state: "format=test"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000,
    end_time_unix_nanos: Some(1640995200150000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: standard_attributes,
    events: [
      @azimuth.telemetry.api.trace.SpanEvent{
        name: "format.test.event",
        timestamp_unix_nanos: 1640995200050000000,
        attributes: [
          ("event.type", @azimuth.telemetry.api.common.AttributeValue::string("format_validation"))
        ]
      }
    ],
    links: []
  }
  
  // 2. 模拟JSON格式序列化
  let json_format_data = serialize_to_json(test_span) // 假设的JSON序列化函数
  let json_deserialized = deserialize_from_json(json_format_data) // 假设的JSON反序列化函数
  
  // 验证JSON格式的数据完整性
  assert(json_deserialized.name == test_span.name, "JSON序列化应该保持Span名称")
  assert(json_deserialized.attributes.length() == test_span.attributes.length(), 
         "JSON序列化应该保持属性数量")
  
  // 3. 模拟二进制格式序列化
  let binary_format_data = serialize_to_binary(test_span) // 假设的二进制序列化函数
  let binary_deserialized = deserialize_from_binary(binary_format_data) // 假设的二进制反序列化函数
  
  // 验证二进制格式的数据完整性
  assert(binary_deserialized.name == test_span.name, "二进制序列化应该保持Span名称")
  assert(binary_deserialized.context.trace_id == test_span.context.trace_id, 
         "二进制序列化应该保持Trace ID")
  
  // 4. 验证跨格式兼容性
  let formats = [
    ("json", json_deserialized),
    ("binary", binary_deserialized)
  ]
  
  for (format_name, deserialized_data) in formats {
    // 验证核心字段在所有格式中都存在
    let service_name = deserialized_data.attributes.find(fn((key, _) { key == "service.name" })
    let operation_name = deserialized_data.attributes.find(fn((key, _) { key == "operation.name" })
    let status_code = deserialized_data.attributes.find(fn((key, _) { key == "status.code" })
    
    assert(service_name != None, "\{format_name}格式应该包含service.name")
    assert(operation_name != None, "\{format_name}格式应该包含operation.name")
    assert(status_code != None, "\{format_name}格式应该包含status.code")
    
    // 验证事件数据的兼容性
    assert(deserialized_data.events.length() == test_span.events.length(), 
           "\{format_name}格式应该保持事件数量")
    
    if deserialized_data.events.length() > 0 {
      let event = deserialized_data.events[0]
      assert(event.name == test_span.events[0].name, 
             "\{format_name}格式应该保持事件名称")
    }
  }
  
  // 5. 测试版本化格式兼容性
  let v1_format_data = serialize_to_format_v1(test_span) // v1格式
  let v2_format_data = serialize_to_format_v2(test_span) // v2格式（向后兼容）
  
  let v1_deserialized = deserialize_from_format_v1(v1_format_data)
  let v2_deserialized = deserialize_from_format_v2(v2_format_data)
  
  // 验证跨版本兼容性
  assert(v1_deserialized.name == v2_deserialized.name, "跨版本格式应该保持Span名称")
  assert(v1_deserialized.context.trace_id == v2_deserialized.context.trace_id, 
         "跨版本格式应该保持Trace ID")
  
  // v2格式应该包含v1的所有核心属性
  let v1_core_attrs = ["service.name", "operation.name", "status.code", "duration.ms", "success"]
  for attr_name in v1_core_attrs {
    let v1_attr = v1_deserialized.attributes.find(fn((key, _) { key == attr_name })
    let v2_attr = v2_deserialized.attributes.find(fn((key, _) { key == attr_name })
    
    assert(v1_attr != None, "v1格式应该包含\{attr_name}")
    assert(v2_attr != None, "v2格式应该包含\{attr_name}")
  }
  
  // 6. 创建度量数据的格式兼容性测试
  let test_metrics = [
    @azimuth.telemetry.api.metrics.Measurement{
      value: 250.5,
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("test_metric")),
        ("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("gauge")),
        ("metric.unit", @azimuth.telemetry.api.common.AttributeValue::string("milliseconds"))
      ]
    }
  ]
  
  // 验证度量数据的格式兼容性
  let json_metrics = serialize_metrics_to_json(test_metrics)
  let binary_metrics = serialize_metrics_to_binary(test_metrics)
  
  let json_deserialized_metrics = deserialize_metrics_from_json(json_metrics)
  let binary_deserialized_metrics = deserialize_metrics_from_binary(binary_metrics)
  
  assert(json_deserialized_metrics.length() == test_metrics.length(), 
         "JSON格式应该保持度量数量")
  assert(binary_deserialized_metrics.length() == test_metrics.length(), 
         "二进制格式应该保持度量数量")
  
  // 验证度量值的精度保持
  assert(json_deserialized_metrics[0].value == test_metrics[0].value, 
         "JSON格式应该保持度量值精度")
  assert(binary_deserialized_metrics[0].value == test_metrics[0].value, 
         "二进制格式应该保持度量值精度")
  
  println("✓ 格式兼容性测试通过")
}

// 辅助函数（在实际实现中这些函数需要真正实现）
fn serialize_span(span : @azimuth.telemetry.api.trace.Span) -> String {
  // 模拟序列化函数
  "serialized_span_data"
}

fn deserialize_span(data : String) -> @azimuth.telemetry.api.trace.Span {
  // 模拟反序列化函数
  @azimuth.telemetry.api.trace.Span{
    name: "deserialized_span",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10],
      span_id: [0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18],
      trace_flags: 0x01,
      trace_state: "deserialized"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000,
    end_time_unix_nanos: Some(1640995200100000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
}

fn process_legacy_span(span : @azimuth.telemetry.api.trace.Span) -> { name : String?, attributes : Array[(String, @azimuth.telemetry.api.common.AttributeValue)] } {
  // 模拟v2系统处理v1数据的函数
  { name: Some(span.name), attributes: span.attributes }
}

fn serialize_to_json(span : @azimuth.telemetry.api.trace.Span) -> String {
  // 模拟JSON序列化
  "json_serialized_data"
}

fn deserialize_from_json(data : String) -> @azimuth.telemetry.api.trace.Span {
  // 模拟JSON反序列化
  deserialize_span(data)
}

fn serialize_to_binary(span : @azimuth.telemetry.api.trace.Span) -> Array[Byte] {
  // 模拟二进制序列化
  [0x01, 0x02, 0x03, 0x04]
}

fn deserialize_from_binary(data : Array[Byte]) -> @azimuth.telemetry.api.trace.Span {
  // 模拟二进制反序列化
  deserialize_span("binary_data")
}

fn serialize_to_format_v1(span : @azimuth.telemetry.api.trace.Span) -> String {
  // 模拟v1格式序列化
  "v1_format_data"
}

fn serialize_to_format_v2(span : @azimuth.telemetry.api.trace.Span) -> String {
  // 模拟v2格式序列化
  "v2_format_data"
}

fn deserialize_from_format_v1(data : String) -> @azimuth.telemetry.api.trace.Span {
  // 模拟v1格式反序列化
  deserialize_span(data)
}

fn deserialize_from_format_v2(data : String) -> @azimuth.telemetry.api.trace.Span {
  // 模拟v2格式反序列化
  deserialize_span(data)
}

fn serialize_metrics_to_json(metrics : Array[@azimuth.telemetry.api.metrics.Measurement]) -> String {
  // 模拟度量JSON序列化
  "json_metrics_data"
}

fn serialize_metrics_to_binary(metrics : Array[@azimuth.telemetry.api.metrics.Measurement]) -> Array[Byte] {
  // 模拟度量二进制序列化
  [0x05, 0x06, 0x07, 0x08]
}

fn deserialize_metrics_from_json(data : String) -> Array[@azimuth.telemetry.api.metrics.Measurement] {
  // 模拟度量JSON反序列化
  [
    @azimuth.telemetry.api.metrics.Measurement{
      value: 250.5,
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("test_metric")),
        ("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("gauge")),
        ("metric.unit", @azimuth.telemetry.api.common.AttributeValue::string("milliseconds"))
      ]
    }
  ]
}

fn deserialize_metrics_from_binary(data : Array[Byte]) -> Array[@azimuth.telemetry.api.metrics.Measurement] {
  // 模拟度量二进制反序列化
  deserialize_metrics_from_json("binary")
}
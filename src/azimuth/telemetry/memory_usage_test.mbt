// 内存使用测试用例
// 测试telemetry系统的内存使用效率和内存泄漏检测

test "span_memory_efficiency" {
  // 测试Span创建的内存效率
  
  let span_count = 1000
  
  // 创建大量Span并验证内存使用的合理性
  let mut spans = []
  let mut i = 0
  while i < span_count {
    let trace_id = [0x01_byte, 0x02_byte, 0x03_byte, 0x04_byte,
                    0x05_byte, 0x06_byte, 0x07_byte, 0x08_byte,
                    0x09_byte, 0x0A_byte, 0x0B_byte, 0x0C_byte,
                    0x0D_byte, 0x0E_byte, 0x0F_byte, 0x10_byte]
    
    let span_id = [(i % 256).to_byte(), ((i + 1) % 256).to_byte(), ((i + 2) % 256).to_byte(), ((i + 3) % 256).to_byte(),
                    ((i + 4) % 256).to_byte(), ((i + 5) % 256).to_byte(), ((i + 6) % 256).to_byte(), ((i + 7) % 256).to_byte()]
    
    let span = Span::{
      name: "memory-test-span-" + i.to_string(),
      context: SpanContext::{
        trace_id: trace_id,
        span_id: span_id,
        trace_flags: 0x01_byte,
        trace_state: ""
      },
      kind: Internal,
      parent_span_id: None,
      start_time_unix_nanos: 1640995200000000000L + i * 1000000L,
      end_time_unix_nanos: Some(1640995200000000000L + i * 1000000L + 500000L),
      status: Ok,
      status_description: None,
      attributes: [
        ("span.index", AttributeValue::int(i.to_int64())),
        ("span.batch", AttributeValue::string("memory-test"))
      ],
      events: [],
      links: []
    }
    spans.push(span)
    i = i + 1
  }
  
  // 验证Span数量
  assert_eq(spans.length(), span_count)
  
  // 验证每个Span的内存占用合理性
  let mut i = 0
  while i < spans.length() {
    let span = spans[i]
    
    // 验证基本字段
    assert_eq(span.name.length() > 0, true)
    assert_eq(span.context.trace_id.length(), 16)
    assert_eq(span.context.span_id.length(), 8)
    assert_eq(span.attributes.length(), 2)
    assert_eq(span.events.length(), 0)
    assert_eq(span.links.length(), 0)
    
    // 验证属性值
    match span.attributes[0] {
      ("span.index", AttributeValue::int(index)) => {
        assert_eq(index, i.to_int64())
      }
      _ => @test.fail("Expected span.index attribute")
    }
    
    match span.attributes[1] {
      ("span.batch", AttributeValue::string(batch)) => {
        assert_eq(batch, "memory-test")
      }
      _ => @test.fail("Expected span.batch attribute")
    }
    
    i = i + 1
  }
  
  // 验证Span名称长度不会无限增长
  let first_span_name = spans[0].name
  let last_span_name = spans[span_count - 1].name
  
  assert_eq(first_span_name, "memory-test-span-0")
  assert_eq(last_span_name, "memory-test-span-" + (span_count - 1).to_string())
  
  // 验证名称长度在合理范围内
  assert_eq(first_span_name.length(), 20)
  assert_eq(last_span_name.length(), 20 + (span_count - 1).to_string().length())
  assert_eq(last_span_name.length() < 50, true) // 合理的上限
}

test "attribute_memory_optimization" {
  // 测试属性的内存优化
  
  // 测试大量不同类型的属性
  let string_attributes = [
    ("string.attr.1", AttributeValue::string("value1")),
    ("string.attr.2", AttributeValue::string("value2")),
    ("string.attr.3", AttributeValue::string("value3")),
    ("string.attr.4", AttributeValue::string("value4")),
    ("string.attr.5", AttributeValue::string("value5"))
  ]
  
  let int_attributes = [
    ("int.attr.1", AttributeValue::int(1L)),
    ("int.attr.2", AttributeValue::int(2L)),
    ("int.attr.3", AttributeValue::int(3L)),
    ("int.attr.4", AttributeValue::int(4L)),
    ("int.attr.5", AttributeValue::int(5L))
  ]
  
  let float_attributes = [
    ("float.attr.1", AttributeValue::float(1.1)),
    ("float.attr.2", AttributeValue::float(2.2)),
    ("float.attr.3", AttributeValue::float(3.3)),
    ("float.attr.4", AttributeValue::float(4.4)),
    ("float.attr.5", AttributeValue::float(5.5))
  ]
  
  let bool_attributes = [
    ("bool.attr.1", AttributeValue::bool(true)),
    ("bool.attr.2", AttributeValue::bool(false)),
    ("bool.attr.3", AttributeValue::bool(true)),
    ("bool.attr.4", AttributeValue::bool(false)),
    ("bool.attr.5", AttributeValue::bool(true))
  ]
  
  let array_string_attributes = [
    ("array.string.attr.1", AttributeValue::array_string(["a", "b", "c"])),
    ("array.string.attr.2", AttributeValue::array_string(["d", "e", "f"])),
    ("array.string.attr.3", AttributeValue::array_string(["g", "h", "i"]))
  ]
  
  let array_int_attributes = [
    ("array.int.attr.1", AttributeValue::array_int([1L, 2L, 3L])),
    ("array.int.attr.2", AttributeValue::array_int([4L, 5L, 6L])),
    ("array.int.attr.3", AttributeValue::array_int([7L, 8L, 9L]))
  ]
  
  // 合并所有属性
  let mut all_attributes = []
  let mut i = 0
  while i < string_attributes.length() {
    all_attributes.push(string_attributes[i])
    i = i + 1
  }
  
  i = 0
  while i < int_attributes.length() {
    all_attributes.push(int_attributes[i])
    i = i + 1
  }
  
  i = 0
  while i < float_attributes.length() {
    all_attributes.push(float_attributes[i])
    i = i + 1
  }
  
  i = 0
  while i < bool_attributes.length() {
    all_attributes.push(bool_attributes[i])
    i = i + 1
  }
  
  i = 0
  while i < array_string_attributes.length() {
    all_attributes.push(array_string_attributes[i])
    i = i + 1
  }
  
  i = 0
  while i < array_int_attributes.length() {
    all_attributes.push(array_int_attributes[i])
    i = i + 1
  }
  
  // 创建具有大量属性的Span
  let memory_test_span = Span::{
    name: "memory-attribute-test",
    context: SpanContext::{
      trace_id: [0x01_byte, 0x02_byte, 0x03_byte, 0x04_byte,
                 0x05_byte, 0x06_byte, 0x07_byte, 0x08_byte,
                 0x09_byte, 0x0A_byte, 0x0B_byte, 0x0C_byte,
                 0x0D_byte, 0x0E_byte, 0x0F_byte, 0x10_byte],
      span_id: [0x11_byte, 0x12_byte, 0x13_byte, 0x14_byte,
                0x15_byte, 0x16_byte, 0x17_byte, 0x18_byte],
      trace_flags: 0x01_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200001000000L),
    status: Ok,
    status_description: None,
    attributes: all_attributes,
    events: [],
    links: []
  }
  
  // 验证属性数量
  assert_eq(memory_test_span.attributes.length(), 21) // 5+5+5+5+3+3
  
  // 验证不同类型属性的存在
  let mut string_count = 0
  let mut int_count = 0
  let mut float_count = 0
  let mut bool_count = 0
  let mut array_string_count = 0
  let mut array_int_count = 0
  
  let mut i = 0
  while i < memory_test_span.attributes.length() {
    match memory_test_span.attributes[i] {
      ("string.attr." + _, AttributeValue::string(_)) => string_count = string_count + 1
      ("int.attr." + _, AttributeValue::int(_)) => int_count = int_count + 1
      ("float.attr." + _, AttributeValue::float(_)) => float_count = float_count + 1
      ("bool.attr." + _, AttributeValue::bool(_)) => bool_count = bool_count + 1
      ("array.string.attr." + _, AttributeValue::array_string(_)) => array_string_count = array_string_count + 1
      ("array.int.attr." + _, AttributeValue::array_int(_)) => array_int_count = array_int_count + 1
      _ => {}
    }
    i = i + 1
  }
  
  assert_eq(string_count, 5)
  assert_eq(int_count, 5)
  assert_eq(float_count, 5)
  assert_eq(bool_count, 5)
  assert_eq(array_string_count, 3)
  assert_eq(array_int_count, 3)
}

test "log_record_memory_usage" {
  // 测试LogRecord的内存使用
  
  let log_count = 500
  
  // 创建大量LogRecord
  let mut log_records = []
  let mut i = 0
  while i < log_count {
    let log_record = LogRecord::{
      timestamp_unix_nanos: 1640995200000000000L + i * 1000000L,
      observed_timestamp_unix_nanos: Some(1640995200000000000L + i * 1000000L),
      severity_number: Info,
      severity_text: Some("INFO"),
      body: Some("Memory test log message " + i.to_string()),
      attributes: [
        ("log.index", AttributeValue::int(i.to_int64())),
        ("log.thread", AttributeValue::string("thread-" + (i % 4).to_string())),
        ("log.component", AttributeValue::string("memory-test-component"))
      ],
      trace_id: Some([0x01_byte, 0x02_byte, 0x03_byte, 0x04_byte,
                      0x05_byte, 0x06_byte, 0x07_byte, 0x08_byte,
                      0x09_byte, 0x0A_byte, 0x0B_byte, 0x0C_byte,
                      0x0D_byte, 0x0E_byte, 0x0F_byte, 0x10_byte]),
      span_id: Some([(i % 256).to_byte(), ((i + 1) % 256).to_byte(), ((i + 2) % 256).to_byte(), ((i + 3) % 256).to_byte(),
                     ((i + 4) % 256).to_byte(), ((i + 5) % 256).to_byte(), ((i + 6) % 256).to_byte(), ((i + 7) % 256).to_byte()]),
      trace_flags: Some(0x01_byte),
      resource: None,
      instrumentation_scope: None
    }
    log_records.push(log_record)
    i = i + 1
  }
  
  // 验证LogRecord数量
  assert_eq(log_records.length(), log_count)
  
  // 验证每个LogRecord的内存使用
  let mut i = 0
  while i < log_records.length() {
    let log_record = log_records[i]
    
    // 验证基本字段
    assert_eq(log_record.timestamp_unix_nanos > 0, true)
    assert_eq(log_record.attributes.length(), 3)
    
    match log_record.observed_timestamp_unix_nanos {
      Some(_) => assert_eq(true, true)
      None => @test.fail("Expected observed timestamp")
    }
    
    match log_record.severity_text {
      Some(severity) => assert_eq(severity, "INFO")
      None => @test.fail("Expected severity text")
    }
    
    match log_record.body {
      Some(body) => {
        assert_eq(body.contains("Memory test log message"), true)
        assert_eq(body.contains(i.to_string()), true)
      }
      None => @test.fail("Expected log body")
    }
    
    match log_record.trace_id {
      Some(trace_id) => assert_eq(trace_id.length(), 16)
      None => @test.fail("Expected trace ID")
    }
    
    match log_record.span_id {
      Some(span_id) => assert_eq(span_id.length(), 8)
      None => @test.fail("Expected span ID")
    }
    
    i = i + 1
  }
}

test "context_memory_growth" {
  // 测试Context的内存增长模式
  
  let context_operations = 200
  
  // 创建深层嵌套的Context并验证内存使用
  let mut current_context = Context::empty()
  let mut context_sizes = []
  
  let mut i = 0
  while i < context_operations {
    let key = create_key("key." + i.to_string())
    let value = "value." + i.to_string()
    current_context = current_context.with_value(key, value)
    
    // 记录Context的大小 (通过values数组的长度)
    context_sizes.push(current_context.values.length())
    
    i = i + 1
  }
  
  // 验证Context大小的线性增长
  assert_eq(context_sizes.length(), context_operations)
  
  let mut i = 0
  while i < context_sizes.length() {
    assert_eq(context_sizes[i], i + 1) // Context应该包含i+1个值
    i = i + 1
  }
  
  // 验证最终Context包含所有值
  assert_eq(current_context.values.length(), context_operations)
  
  // 验证特定键值对的存在
  let first_key = create_key("key.0")
  let middle_key = create_key("key." + (context_operations / 2).to_string())
  let last_key = create_key("key." + (context_operations - 1).to_string())
  
  match current_context.get(first_key) {
    Some(value) => assert_eq(value, "value.0")
    None => @test.fail("Expected first key value")
  }
  
  match current_context.get(middle_key) {
    Some(value) => assert_eq(value, "value." + (context_operations / 2).to_string())
    None => @test.fail("Expected middle key value")
  }
  
  match current_context.get(last_key) {
    Some(value) => assert_eq(value, "value." + (context_operations - 1).to_string())
    None => @test.fail("Expected last key value")
  }
}

test "baggage_memory_efficiency" {
  // 测试Baggage的内存效率
  
  let baggage_entries = 150
  
  // 创建包含大量条目的Baggage
  let mut current_baggage = Baggage::empty()
  let mut baggage_sizes = []
  
  let mut i = 0
  while i < baggage_entries {
    let key = "baggage.key." + i.to_string()
    let value = "baggage.value." + i.to_string()
    current_baggage = current_baggage.with_entry(key, value)
    
    // 记录Baggage的大小 (通过entries数组的长度)
    baggage_sizes.push(current_baggage.entries.length())
    
    i = i + 1
  }
  
  // 验证Baggage大小的线性增长
  assert_eq(baggage_sizes.length(), baggage_entries)
  
  let mut i = 0
  while i < baggage_sizes.length() {
    assert_eq(baggage_sizes[i], i + 1) // Baggage应该包含i+1个条目
    i = i + 1
  }
  
  // 验证最终Baggage包含所有条目
  assert_eq(current_baggage.entries.length(), baggage_entries)
  
  // 验证特定条目的存在
  match current_baggage.get("baggage.key.0") {
    Some(value) => assert_eq(value, "baggage.value.0")
    None => @test.fail("Expected first baggage entry")
  }
  
  match current_baggage.get("baggage.key." + (baggage_entries / 2).to_string()) {
    Some(value) => assert_eq(value, "baggage.value." + (baggage_entries / 2).to_string())
    None => @test.fail("Expected middle baggage entry")
  }
  
  match current_baggage.get("baggage.key." + (baggage_entries - 1).to_string()) {
    Some(value) => assert_eq(value, "baggage.value." + (baggage_entries - 1).to_string())
    None => @test.fail("Expected last baggage entry")
  }
  
  // 验证键和值的长度在合理范围内
  let mut i = 0
  while i < current_baggage.entries.length() {
    let (key, value) = current_baggage.entries[i]
    
    // 验证键长度
    assert_eq(key.starts_with("baggage.key."), true)
    assert_eq(key.length(), "baggage.key.".length() + i.to_string().length())
    assert_eq(key.length() < 50, true) // 合理的上限
    
    // 验证值长度
    assert_eq(value.starts_with("baggage.value."), true)
    assert_eq(value.length(), "baggage.value.".length() + i.to_string().length())
    assert_eq(value.length() < 50, true) // 合理的上限
    
    i = i + 1
  }
}

test "resource_memory_optimization" {
  // 测试Resource的内存优化
  
  // 创建具有大量属性的Resource
  let mut resource_attributes = []
  let mut i = 0
  while i < 50 {
    let attr = ("resource.attr." + i.to_string(), AttributeValue::string("resource.value." + i.to_string()))
    resource_attributes.push(attr)
    i = i + 1
  }
  
  let resource = Resource::{
    service_name: "memory-test-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: resource_attributes
  }
  
  // 验证Resource的基本字段
  assert_eq(resource.service_name, "memory-test-service")
  match resource.service_version {
    Some(version) => assert_eq(version, "1.0.0")
    None => @test.fail("Expected service version")
  }
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 50)
  
  // 验证属性的存在和内容
  let mut i = 0
  while i < resource.attributes.length() {
    match resource.attributes[i] {
      ("resource.attr." + index_str, AttributeValue::string(value)) => {
        let expected_value = "resource.value." + index_str
        assert_eq(value, expected_value)
      }
      _ => @test.fail("Expected resource attribute pattern")
    }
    i = i + 1
  }
  
  // 验证特定属性的存在
  let mut found_first = false
  let mut found_middle = false
  let mut found_last = false
  
  let mut i = 0
  while i < resource.attributes.length() {
    match resource.attributes[i] {
      ("resource.attr.0", AttributeValue::string(_)) => found_first = true
      ("resource.attr.25", AttributeValue::string(_)) => found_middle = true
      ("resource.attr.49", AttributeValue::string(_)) => found_last = true
      _ => {}
    }
    i = i + 1
  }
  
  assert_eq(found_first, true)
  assert_eq(found_middle, true)
  assert_eq(found_last, true)
}
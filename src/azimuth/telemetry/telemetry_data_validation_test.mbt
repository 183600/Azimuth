// 数据验证测试 - 测试数据完整性和验证规则
test "data_validation_trace_id_format" {
  // 测试Trace ID格式验证
  
  // 测试有效的Trace ID（16字节，32个十六进制字符）
  let valid_trace_id_1 = [for i = 0; i < 16; i = i + 1].map(fn(i) { 
    if i % 2 == 0 { 0x0a_byte } else { 0xf0_byte }
  })
  let valid_trace_id_2 = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x00_byte })
  let valid_trace_id_3 = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0xff_byte })
  
  // 创建有效的SpanContext
  let valid_context_1 = trace::SpanContext::{
    trace_id: valid_trace_id_1,
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte }),
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let valid_context_2 = trace::SpanContext::{
    trace_id: valid_trace_id_2,
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte }),
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  let valid_context_3 = trace::SpanContext::{
    trace_id: valid_trace_id_3,
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x03_byte }),
    trace_flags: 1_byte,
    trace_state: "key=value"
  }
  
  // 验证有效Trace ID
  assert_eq(valid_context_1.trace_id.length(), 16)
  assert_eq(valid_context_2.trace_id.length(), 16)
  assert_eq(valid_context_3.trace_id.length(), 16)
  
  // 验证Trace ID内容
  assert_eq(valid_context_1.trace_id[0], 0x0a_byte)
  assert_eq(valid_context_1.trace_id[1], 0xf0_byte)
  assert_eq(valid_context_2.trace_id[0], 0x00_byte)
  assert_eq(valid_context_3.trace_id[0], 0xff_byte)
}

test "data_validation_span_id_format" {
  // 测试Span ID格式验证
  
  // 测试有效的Span ID（8字节，16个十六进制字符）
  let valid_span_id_1 = [for i = 0; i < 8; i = i + 1].map(fn(i) { 
    if i % 2 == 0 { 0x0b_byte } else { 0xf1_byte }
  })
  let valid_span_id_2 = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x00_byte })
  let valid_span_id_3 = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0xff_byte })
  
  // 创建有效的SpanContext
  let valid_context_1 = trace::SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
    span_id: valid_span_id_1,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let valid_context_2 = trace::SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x02_byte }),
    span_id: valid_span_id_2,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  let valid_context_3 = trace::SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x03_byte }),
    span_id: valid_span_id_3,
    trace_flags: 1_byte,
    trace_state: "key=value"
  }
  
  // 验证有效Span ID
  assert_eq(valid_context_1.span_id.length(), 8)
  assert_eq(valid_context_2.span_id.length(), 8)
  assert_eq(valid_context_3.span_id.length(), 8)
  
  // 验证Span ID内容
  assert_eq(valid_context_1.span_id[0], 0x0b_byte)
  assert_eq(valid_context_1.span_id[1], 0xf1_byte)
  assert_eq(valid_context_2.span_id[0], 0x00_byte)
  assert_eq(valid_context_3.span_id[0], 0xff_byte)
}

test "data_validation_trace_flags" {
  // 测试Trace Flags验证
  
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte })
  
  // 测试所有可能的Trace Flags值
  let flag_00 = trace::SpanContext::{ trace_id, span_id, trace_flags: 0x00_byte, trace_state: "" }
  let flag_01 = trace::SpanContext::{ trace_id, span_id, trace_flags: 0x01_byte, trace_state: "" }
  let flag_ff = trace::SpanContext::{ trace_id, span_id, trace_flags: 0xff_byte, trace_state: "" }
  
  // 验证Trace Flags
  assert_eq(flag_00.trace_flags, 0x00_byte)
  assert_eq(flag_01.trace_flags, 0x01_byte)
  assert_eq(flag_ff.trace_flags, 0xff_byte)
  
  // 验证采样标志（bit 0）
  let sampled_flag = flag_01.trace_flags & 0x01_byte
  let not_sampled_flag = flag_00.trace_flags & 0x01_byte
  
  assert_eq(sampled_flag, 0x01_byte)  // 已采样
  assert_eq(not_sampled_flag, 0x00_byte)  // 未采样
}

test "data_validation_trace_state" {
  // 测试Trace State验证
  
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte })
  
  // 测试有效的Trace State格式
  let valid_trace_state_1 = ""
  let valid_trace_state_2 = "key=value"
  let valid_trace_state_3 = "key1=value1,key2=value2"
  let valid_trace_state_4 = "vendor1=value1;vendor2=value2,key3=value3"
  let valid_trace_state_5 = "key.with.dots=value.with.dots"
  let valid_trace_state_6 = "key_with_underscores=value_with_underscores"
  
  let context_1 = trace::SpanContext::{ trace_id, span_id, trace_flags: 1_byte, trace_state: valid_trace_state_1 }
  let context_2 = trace::SpanContext::{ trace_id, span_id, trace_flags: 1_byte, trace_state: valid_trace_state_2 }
  let context_3 = trace::SpanContext::{ trace_id, span_id, trace_flags: 1_byte, trace_state: valid_trace_state_3 }
  let context_4 = trace::SpanContext::{ trace_id, span_id, trace_flags: 1_byte, trace_state: valid_trace_state_4 }
  let context_5 = trace::SpanContext::{ trace_id, span_id, trace_flags: 1_byte, trace_state: valid_trace_state_5 }
  let context_6 = trace::SpanContext::{ trace_id, span_id, trace_flags: 1_byte, trace_state: valid_trace_state_6 }
  
  // 验证Trace State
  assert_eq(context_1.trace_state, "")
  assert_eq(context_2.trace_state, "key=value")
  assert_eq(context_3.trace_state, "key1=value1,key2=value2")
  assert_eq(context_4.trace_state, "vendor1=value1;vendor2=value2,key3=value3")
  assert_eq(context_5.trace_state, "key.with.dots=value.with.dots")
  assert_eq(context_6.trace_state, "key_with_underscores=value_with_underscores")
}

test "data_validation_attribute_names" {
  // 测试属性名称验证
  
  // 测试有效的属性名称
  let valid_attribute_names = [
    "service.name",
    "service.version",
    "http.method",
    "http.status_code",
    "db.system",
    "db.statement",
    "user.id",
    "session.id",
    "trace.id",
    "span.kind",
    "error.type",
    "error.message",
    "process.id",
    "process.name",
    "host.name",
    "os.type",
    "cloud.provider",
    "k8s.pod.name",
    "container.id",
    "telemetry.sdk.name"
  ]
  
  // 测试无效的属性名称（在实际实现中应该被拒绝）
  let potentially_invalid_names = [
    "",  // 空字符串
    " ",  // 只有空格
    "key with spaces",  // 包含空格
    "key@with@symbols",  // 包含特殊符号
    "key/with/slashes",  // 包含斜杠
    "key\\with\\backslashes",  // 包含反斜杠
    "a".repeat(300)  // 超长名称
  ]
  
  // 创建有效的属性
  let mut valid_attributes = []
  let mut i = 0
  while i < valid_attribute_names.length() {
    let attr_name = valid_attribute_names[i]
    valid_attributes.push((attr_name, common::AttributeValue::string("test_value")))
    i = i + 1
  }
  
  // 创建可能无效的属性（在当前实现中会被接受）
  let mut potentially_invalid_attributes = []
  i = 0
  while i < potentially_invalid_names.length() {
    let attr_name = potentially_invalid_names[i]
    potentially_invalid_attributes.push((attr_name, common::AttributeValue::string("test_value")))
    i = i + 1
  }
  
  // 验证属性创建
  assert_eq(valid_attributes.length(), 20)
  assert_eq(potentially_invalid_attributes.length(), 7)
  
  // 验证特定属性名称
  assert_eq(valid_attributes[0].0, "service.name")
  assert_eq(valid_attributes[10].0, "error.type")
  assert_eq(valid_attributes[19].0, "telemetry.sdk.name")
}

test "data_validation_attribute_values" {
  // 测试属性值验证
  
  // 测试字符串值
  let valid_string_values = [
    "simple_string",
    "string.with.dots",
    "string_with_underscores",
    "string-with-dashes",
    "StringWithCamelCase",
    "string_with_numbers123",
    "unicode_string_测试",
    "emoji_string_🚀",
    "",  // 空字符串
    " ",  // 只有空格
    "a".repeat(1000)  // 长字符串
  ]
  
  // 测试整数值
  let valid_int_values = [
    0L,
    1L,
    -1L,
    9223372036854775807L,  // Max Int64
    -9223372036854775808L,  // Min Int64
    1234567890L,
    -987654321L
  ]
  
  // 测试浮点数值
  let valid_float_values = [
    0.0,
    1.0,
    -1.0,
    3.14159265359,
    1.7976931348623157e+308,  // Max Double
    -1.7976931348623157e+308,  // Min Double
    1.0/0.0,  // Infinity
    -1.0/0.0,  // Negative Infinity
    0.0/0.0   // NaN
  ]
  
  // 测试布尔值
  let valid_bool_values = [true, false]
  
  // 创建属性并验证
  let mut string_attributes = []
  let mut i = 0
  while i < valid_string_values.length() {
    let value = valid_string_values[i]
    string_attributes.push(("string_test_" + i.to_string(), common::AttributeValue::string(value)))
    i = i + 1
  }
  
  let mut int_attributes = []
  i = 0
  while i < valid_int_values.length() {
    let value = valid_int_values[i]
    int_attributes.push(("int_test_" + i.to_string(), common::AttributeValue::int(value)))
    i = i + 1
  }
  
  let mut float_attributes = []
  i = 0
  while i < valid_float_values.length() {
    let value = valid_float_values[i]
    float_attributes.push(("float_test_" + i.to_string(), common::AttributeValue::float(value)))
    i = i + 1
  }
  
  let mut bool_attributes = []
  i = 0
  while i < valid_bool_values.length() {
    let value = valid_bool_values[i]
    bool_attributes.push(("bool_test_" + i.to_string(), common::AttributeValue::bool(value)))
    i = i + 1
  }
  
  // 验证属性创建
  assert_eq(string_attributes.length(), 11)
  assert_eq(int_attributes.length(), 7)
  assert_eq(float_attributes.length(), 9)
  assert_eq(bool_attributes.length(), 2)
}

test "data_validation_array_values" {
  // 测试数组值验证
  
  // 测试字符串数组
  let string_arrays = [
    [],  // 空数组
    ["single_item"],
    ["item1", "item2", "item3"],
    ["", " ", "test"],  // 包含空字符串
    ["unicode", "测试", "🚀"],  // Unicode和emoji
    ["a".repeat(100)]  // 长字符串数组
  ]
  
  // 测试整数数组
  let int_arrays = [
    [],  // 空数组
    [0L],
    [1L, 2L, 3L],
    [-1L, -2L, -3L],  // 负数
    [9223372036854775807L],  // 最大值
    [-9223372036854775808L]  // 最小值
  ]
  
  // 测试浮点数数组
  let float_arrays = [
    [],  // 空数组
    [0.0],
    [1.1, 2.2, 3.3],
    [-1.1, -2.2, -3.3],  // 负数
    [1.0/0.0, -1.0/0.0, 0.0/0.0],  // Infinity和NaN
    [3.14159265359, 2.71828182846]  // 数学常数
  ]
  
  // 测试布尔数组
  let bool_arrays = [
    [],  // 空数组
    [true],
    [false],
    [true, false, true, false],  // 交替
    [true, true, true],  // 全true
    [false, false, false]  // 全false
  ]
  
  // 创建数组属性
  let mut array_attributes = []
  let mut i = 0
  while i < string_arrays.length() {
    let array = string_arrays[i]
    array_attributes.push(("string_array_" + i.to_string(), common::AttributeValue::array_string(array)))
    i = i + 1
  }
  
  i = 0
  while i < int_arrays.length() {
    let array = int_arrays[i]
    array_attributes.push(("int_array_" + i.to_string(), common::AttributeValue::array_int(array)))
    i = i + 1
  }
  
  i = 0
  while i < float_arrays.length() {
    let array = float_arrays[i]
    array_attributes.push(("float_array_" + i.to_string(), common::AttributeValue::array_float(array)))
    i = i + 1
  }
  
  i = 0
  while i < bool_arrays.length() {
    let array = bool_arrays[i]
    array_attributes.push(("bool_array_" + i.to_string(), common::AttributeValue::array_bool(array)))
    i = i + 1
  }
  
  // 验证数组属性创建
  assert_eq(array_attributes.length(), 22)
  
  // 验证特定数组内容
  match array_attributes[1].1 {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], "single_item")
    }
    _ => @test.fail("Test failed")
  }
  
  match array_attributes[7].1 {
    common::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1L)
      assert_eq(arr[1], 2L)
      assert_eq(arr[2], 3L)
    }
    _ => @test.fail("Test failed")
  }
}

test "data_validation_span_relationships" {
  // 测试Span关系验证
  
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte })
  
  // 创建父子Span关系
  let parent_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte })
  let child_span_id_1 = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x03_byte })
  let child_span_id_2 = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x04_byte })
  
  // 创建根Span（没有父Span）
  let root_span = trace::Span::{
    name: "root_operation",
    context: trace::SpanContext::{
      trace_id: trace_id,
      span_id: parent_span_id,
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(5000L),
    status: trace::Ok,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  // 创建子Span
  let child_span_1 = trace::Span::{
    name: "child_operation_1",
    context: trace::SpanContext::{
      trace_id: trace_id,
      span_id: child_span_id_1,
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: Some(parent_span_id),
    start_time_unix_nanos: 1500L,
    end_time_unix_nanos: Some(3000L),
    status: trace::Ok,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  let child_span_2 = trace::Span::{
    name: "child_operation_2",
    context: trace::SpanContext::{
      trace_id: trace_id,
      span_id: child_span_id_2,
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: Some(parent_span_id),
    start_time_unix_nanos: 2000L,
    end_time_unix_nanos: Some(4000L),
    status: trace::Ok,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  // 验证Span关系
  assert_eq(root_span.context.trace_id, child_span_1.context.trace_id)
  assert_eq(root_span.context.trace_id, child_span_2.context.trace_id)
  
  match root_span.parent_span_id {
    None => assert_eq(true, true)  // 根Span没有父Span
    Some(_) => @test.fail("Test failed")
  }
  
  match child_span_1.parent_span_id {
    Some(parent_id) => {
      assert_eq(parent_id.length(), 8)
      assert_eq(parent_id[0], 0x02_byte)
    }
    None => @test.fail("Test failed")
  }
  
  match child_span_2.parent_span_id {
    Some(parent_id) => {
      assert_eq(parent_id.length(), 8)
      assert_eq(parent_id[0], 0x02_byte)
    }
    None => @test.fail("Test failed")
  }
  
  // 验证时间顺序
  assert_eq(root_span.start_time_unix_nanos, 1000L)
  assert_eq(child_span_1.start_time_unix_nanos, 1500L)
  assert_eq(child_span_2.start_time_unix_nanos, 2000L)
  
  assert_eq(child_span_1.start_time_unix_nanos >= root_span.start_time_unix_nanos, true)
  assert_eq(child_span_2.start_time_unix_nanos >= root_span.start_time_unix_nanos, true)
}

test "data_validation_span_links" {
  // 测试Span链接验证
  
  let trace_id_1 = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte })
  let trace_id_2 = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x02_byte })
  let span_id_1 = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x03_byte })
  let span_id_2 = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x04_byte })
  
  // 创建链接的SpanContext
  let linked_context_1 = trace::SpanContext::{
    trace_id: trace_id_1,
    span_id: span_id_1,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let linked_context_2 = trace::SpanContext::{
    trace_id: trace_id_2,
    span_id: span_id_2,
    trace_flags: 0_byte,
    trace_state: "key=value"
  }
  
  // 创建Span链接
  let span_link_1 = trace::SpanLink::{
    context: linked_context_1,
    attributes: [
      ("link.type", common::AttributeValue::string("causality")),
      ("relationship", common::AttributeValue::string("predecessor"))
    ]
  }
  
  let span_link_2 = trace::SpanLink::{
    context: linked_context_2,
    attributes: [
      ("link.type", common::AttributeValue::string("correlation")),
      ("service", common::AttributeValue::string("related-service"))
    ]
  }
  
  // 创建带有链接的Span
  let span_with_links = trace::Span::{
    name: "operation_with_links",
    context: trace::SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x05_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x06_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(2000L),
    status: trace::Ok,
    status_description: None,
    attributes: [],
    events: [],
    links: [span_link_1, span_link_2]
  }
  
  // 验证Span链接
  assert_eq(span_with_links.links.length(), 2)
  
  // 验证第一个链接
  assert_eq(span_with_links.links[0].context.trace_id.length(), 16)
  assert_eq(span_with_links.links[0].context.span_id.length(), 8)
  assert_eq(span_with_links.links[0].context.trace_flags, 1_byte)
  assert_eq(span_with_links.links[0].attributes.length(), 2)
  
  match span_with_links.links[0].attributes[0].1 {
    common::StringValue(link_type) => assert_eq(link_type, "causality")
    _ => @test.fail("Test failed")
  }
  
  // 验证第二个链接
  assert_eq(span_with_links.links[1].context.trace_id.length(), 16)
  assert_eq(span_with_links.links[1].context.span_id.length(), 8)
  assert_eq(span_with_links.links[1].context.trace_flags, 0_byte)
  assert_eq(span_with_links.links[1].context.trace_state, "key=value")
  assert_eq(span_with_links.links[1].attributes.length(), 2)
}
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
  let valid_context_1 = @azimuth.telemetry.api.trace.SpanContext::{
    trace_id: valid_trace_id_1,
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte }),
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let valid_context_2 = @azimuth.telemetry.api.trace.SpanContext::{
    trace_id: valid_trace_id_2,
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte }),
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  let valid_context_3 = @azimuth.telemetry.api.trace.SpanContext::{
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
  let valid_context_1 = @azimuth.telemetry.api.trace.SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
    span_id: valid_span_id_1,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let valid_context_2 = @azimuth.telemetry.api.trace.SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x02_byte }),
    span_id: valid_span_id_2,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  let valid_context_3 = @azimuth.telemetry.api.trace.SpanContext::{
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
    "a".repeat(100)  // 长字符串
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
    string_attributes.push(("string_test_" + i.to_string(), @azimuth.telemetry.api.common.AttributeValue::string(value)))
    i = i + 1
  }
  
  let mut int_attributes = []
  i = 0
  while i < valid_int_values.length() {
    let value = valid_int_values[i]
    int_attributes.push(("int_test_" + i.to_string(), @azimuth.telemetry.api.common.AttributeValue::int(value)))
    i = i + 1
  }
  
  let mut float_attributes = []
  i = 0
  while i < valid_float_values.length() {
    let value = valid_float_values[i]
    float_attributes.push(("float_test_" + i.to_string(), @azimuth.telemetry.api.common.AttributeValue::float(value)))
    i = i + 1
  }
  
  let mut bool_attributes = []
  i = 0
  while i < valid_bool_values.length() {
    let value = valid_bool_values[i]
    bool_attributes.push(("bool_test_" + i.to_string(), @azimuth.telemetry.api.common.AttributeValue::bool(value)))
    i = i + 1
  }
  
  // 验证属性创建
  assert_eq(string_attributes.length(), 11)
  assert_eq(int_attributes.length(), 7)
  assert_eq(float_attributes.length(), 9)
  assert_eq(bool_attributes.length(), 2)
}
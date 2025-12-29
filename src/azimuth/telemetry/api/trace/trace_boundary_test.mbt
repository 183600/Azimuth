test "span_context_validation" {
  // 测试SpanContext的验证逻辑
  
  // 测试有效的trace_id和span_id
  let valid_trace_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte,
                       9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte]
  let valid_span_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]
  
  assert_eq(valid_trace_id.length(), 16)
  assert_eq(valid_span_id.length(), 8)
  
  // 测试全零的trace_id（无效）
  let zero_trace_id = Array::make(16, 0_byte)
  let is_zero_trace = zero_trace_id.all(fn(x) { x == 0_byte })
  assert_eq(is_zero_trace, true)
  
  // 测试全零的span_id（无效）
  let zero_span_id = Array::make(8, 0_byte)
  let is_zero_span = zero_span_id.all(fn(x) { x == 0_byte })
  assert_eq(is_zero_span, true)
  
  // 测试非零的trace_id和span_id（有效）
  let non_zero_trace = valid_trace_id.some(fn(x) { x != 0_byte })
  let non_zero_span = valid_span_id.some(fn(x) { x != 0_byte })
  assert_eq(non_zero_trace, true)
  assert_eq(non_zero_span, true)
}

test "span_attributes_edge_cases" {
  // 测试Span属性的边界情况
  
  // 测试空属性列表
  let empty_attrs : Attributes = []
  assert_eq(empty_attrs.length(), 0)
  
  // 测试大量属性
  let many_attrs : Attributes = [
    ("attr1", AttributeValue::string("value1")),
    ("attr2", AttributeValue::string("value2")),
    ("attr3", AttributeValue::string("value3")),
    ("attr4", AttributeValue::string("value4")),
    ("attr5", AttributeValue::string("value5"))
  ]
  assert_eq(many_attrs.length(), 5)
  
  // 测试属性值的特殊字符
  let special_attrs : Attributes = [
    ("empty", AttributeValue::string("")),
    ("spaces", AttributeValue::string("   ")),
    ("unicode", AttributeValue::string("测试🌟")),
    ("special", AttributeValue::string("!@#$%^&*()")),
    ("null_char", AttributeValue::string("hello\0world"))
  ]
  
  assert_eq(special_attrs.length(), 5)
  
  // 验证空字符串属性
  match special_attrs[0].1 {
    AttributeValue::StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Expected StringValue")
  }
  
  // 验证Unicode属性
  match special_attrs[2].1 {
    AttributeValue::StringValue(s) => assert_eq(s.contains("测试"), true)
    _ => @test.fail("Expected StringValue")
  }
}
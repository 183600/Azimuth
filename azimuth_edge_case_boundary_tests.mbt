// Azimuth 边缘情况和边界条件测试用例
// 专注于遥测系统在极端条件下的行为验证

// 测试1: 零值和空值处理
test "零值和空值处理测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "edge.case.test")
  
  // 测试零值计数器
  let zero_counter = Meter::create_counter(meter, "zero.counter")
  Counter::add(zero_counter, 0.0)
  assert_eq(Counter::value(zero_counter), 0.0)
  
  // 测试空字符串属性
  let span = Span::new("empty.attrs.test", Client, SpanContext::new("", "", false, ""))
  Span::set_attribute(span, "empty.string", StringValue(""))
  Span::set_attribute(span, "normal.string", StringValue("normal"))
  
  let empty_attr = Span::get_attribute(span, "empty.string")
  let normal_attr = Span::get_attribute(span, "normal.string")
  
  match empty_attr {
    Some(StringValue("")) => assert_true(true)
    _ => assert_true(false)
  }
  
  match normal_attr {
    Some(StringValue("normal")) => assert_true(true)
    _ => assert_true(false)
  }
}

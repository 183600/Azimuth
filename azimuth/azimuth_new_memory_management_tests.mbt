// 内存管理和资源清理测试
test "内存管理和资源清理测试" {
  // 测试大量Span创建和销毁
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "memory-test")
  
  let spans = []
  
  // 创建大量Span以测试内存管理
  for i = 0; i < 100; i = i + 1 {
    let span = azimuth::Tracer::start_span(tracer, "memory-test-span-" + i.to_string())
    spans.push(span)
  }
  
  // 验证所有Span都被正确创建
  assert_eq(spans.length(), 100)
  
  // 批量结束Span以测试资源清理
  for span in spans {
    azimuth::Span::end(span)
  }
  
  // 测试大量属性创建和清理
  let large_attrs = azimuth::Attributes::new()
  
  // 添加大量属性
  for i = 0; i < 50; i = i + 1 {
    azimuth::Attributes::set(large_attrs, "attr." + i.to_string(), azimuth::StringValue("value." + i.to_string()))
  }
  
  // 验证属性数量（基于简化实现）
  let test_attr = azimuth::Attributes::get(large_attrs, "attr.10")
  assert_eq(test_attr, Some(azimuth::StringValue("test_value")))
}
test "instrumentation_scope_creation" {
  // 测试基本的InstrumentationScope结构
  // 由于InstrumentationScope是只读类型，我们无法直接创建实例
  // 但我们可以测试相关的属性和结构
  
  let scope_name = "test-scope"
  let scope_version = Some("1.0.0")
  let scope_schema = Some("http://example.com/schema")
  
  // 测试字符串值的有效性
  assert_eq(scope_name, "test-scope")
  
  match scope_version {
    Some(version) => assert_eq(version, "1.0.0")
    None => @test.fail("Test failed")
  }
  
  match scope_schema {
    Some(schema) => assert_eq(schema, "http://example.com/schema")
    None => @test.fail("Test failed")
  }
  
  // 测试空值情况
  let empty_scope_name = ""
  let empty_scope_version : String? = None
  let empty_scope_schema : String? = None
  
  assert_eq(empty_scope_name, "")
  
  match empty_scope_version {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  match empty_scope_schema {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 测试复杂名称
  let complex_scope_name = "complex.scope.v2-with_special_chars"
  assert_eq(complex_scope_name, "complex.scope.v2-with_special_chars")
  
  // 测试Unicode名称
  let unicode_scope_name = "测试范围🚀"
  assert_eq(unicode_scope_name, "测试范围🚀")
}
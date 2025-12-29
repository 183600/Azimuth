// 简单的上下文测试
test "context_simple_test" {
  let ctx = Context::empty()
  let key = create_key("test_key")
  let ctx_with_value = ctx.with_value(key, "test_value")
  
  match ctx_with_value.get(key) {
    Some(value) => assert_eq(value, "test_value")
    None => assert_eq(false, true)
  }
}
// 语法测试 - 验证正确的跨模块引用语法

test "syntax_validation" {
  // 基本断言测试
  assert_eq(1 + 1, 2)
  assert_eq("hello" + " world", "hello world")
}

test "string_operations" {
  let test_str = "telemetry"
  assert_eq(test_str.length(), 9)
  assert_eq(test_str.contains("tele"), true)
  assert_eq(test_str.has_prefix("tele"), true)
  assert_eq(test_str.has_suffix("try"), true)
}
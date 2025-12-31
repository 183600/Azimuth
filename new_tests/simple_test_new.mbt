// 简单测试验证

test "basic_assertions" {
  assert_eq(1 + 1, 2)
  assert_eq("hello" + " world", "hello world")
  assert_eq(true and true, true)
  assert_eq(false or true, true)
}

test "string_functions" {
  let test_str = "telemetry_test"
  assert_eq(test_str.length(), 14)
  assert_eq(test_str.contains("tele"), true)
  assert_eq(test_str.has_prefix("tele"), true)
  assert_eq(test_str.has_suffix("_test"), true)
}

test "numeric_operations" {
  let x = 10
  let y = 20
  assert_eq(x + y, 30)
  assert_eq(y - x, 10)
  assert_eq(x * y, 200)
  assert_eq(y / x, 2)
  assert_eq(x > 5, true)
  assert_eq(y < 25, true)
}
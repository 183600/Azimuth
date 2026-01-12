// 测试函数和断言函数从 azimuth 包导入

test "unique_test_case_1" {
  assert_eq(10, add(3, 7))
}

test "unique_test_case_2" {
  assert_eq(15, multiply(3, 5))
}

test "unique_test_case_3" {
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}
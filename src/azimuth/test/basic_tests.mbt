// 基本的 MoonBit 测试用例

test "add_test" {
  let result = azimuth::add(2, 3)
  azimuth::assert_eq(5, result)
}

test "multiply_test" {
  let result = azimuth::multiply(4, 5)
  azimuth::assert_eq(20, result)
}

test "greet_test" {
  let result = azimuth::greet("World")
  azimuth::assert_eq_string("Hello, World!", result)
}
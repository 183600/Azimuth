// 基本的 MoonBit 测试用例

test "add_test" {
  let result = azimuth::@azimuth.add(2, 3)
  azimuth::@azimuth.assert_eq(5, result)
}

test "multiply_test" {
  let result = azimuth::@azimuth.multiply(4, 5)
  azimuth::@azimuth.assert_eq(20, result)
}

test "greet_test" {
  let result = azimuth::@azimuth.greet("World")
  azimuth::@azimuth.assert_eq_string("Hello, World!", result)
}
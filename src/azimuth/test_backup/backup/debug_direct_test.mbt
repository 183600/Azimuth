// 测试辅助函数定义
// 从 azimuth 包中复制的函数实现，用于测试
test "azimuth_add_test" {
  // Use direct addition instead of add
  let result = @azimuth.add(2, 3)
  if result != 5 {
    @azimuth.assert_true(false)()
  }
}
test "azimuth_multiply_test" {
  // Use direct multiplication instead of multiply
  let result = @azimuth.multiply(2, 3)
  if result != 6 {
    @azimuth.assert_true(false)()
  }
}
test "azimuth_greet_test" {
  // Use direct string concatenation instead of greet
  let result = @azimuth.greet("World")
  if result != "Hello, World!" {
    @azimuth.assert_true(false)()
  }
}

// 测试辅助函数定义
// 从 azimuth 包中复制的函数实现，用于测试
// 测试文件 - 使用azimuth包中的函数
test "test_2_edge_cases_add_additional" {
  // Test with zero
  if 5 != @azimuth.add(0, 5) {
    @azimuth.assert_true(false)()
  }
  if 5 != @azimuth.add(5, 0) {
    @azimuth.assert_true(false)()
  }
  // Test with large numbers
  if 2147483647 != @azimuth.add(2147483646, 1) {
    @azimuth.assert_true(false)()
  }
  if -2147483648 != @azimuth.add(-2147483647, -1) {
    @azimuth.assert_true(false)()
  }
}
test "test_2_edge_cases_multiply_additional" {
  // Test with zero
  if 0 != @azimuth.multiply(0, 5) {
    @azimuth.assert_true(false)()
  }
  if 0 != @azimuth.multiply(5, 0) {
    @azimuth.assert_true(false)()
  }
  // Test with one
  if 5 != @azimuth.multiply(1, 5) {
    @azimuth.assert_true(false)()
  }
  if 5 != @azimuth.multiply(5, 1) {
    @azimuth.assert_true(false)()
  }
  // Test with negative one
  if -5 != @azimuth.multiply(-1, 5) {
    @azimuth.assert_true(false)()
  }
  if -5 != @azimuth.multiply(5, -1) {
    @azimuth.assert_true(false)()
  }
  if 5 != @azimuth.multiply(-1, -5) {
    @azimuth.assert_true(false)()
  }
}
test "test_2_edge_cases_greet_additional" {
  // Test with special characters
  if "Hello, @#$!" != @azimuth.greet("@#$") {
    @azimuth.assert_true(false)()
  }
  if "Hello, 123!" != @azimuth.greet("123") {
    @azimuth.assert_true(false)()
  }
}

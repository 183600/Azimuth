// 测试辅助函数定义
// 从 azimuth 包中复制的函数实现，用于测试
// 测试文件 - 使用azimuth包中的函数
test "test_1_basic_add_fixed" {
  if 5 != @azimuth.add(2, 3) { @azimuth.assert_true(false)() }
  if 0 != @azimuth.add(5, -5) { @azimuth.assert_true(false)() }
  if -10 != @azimuth.add(-7, -3) { @azimuth.assert_true(false)() }
}
test "test_1_basic_multiply_fixed" {
  if 6 != @azimuth.multiply(2, 3) { @azimuth.assert_true(false)() }
  if -6 != @azimuth.multiply(2, -3) { @azimuth.assert_true(false)() }
  if 21 != @azimuth.multiply(-7, -3) { @azimuth.assert_true(false)() }
}
test "test_1_basic_greet_fixed" {
  if "Hello, World!" != @azimuth.greet("World") { @azimuth.assert_true(false)() }
  if "Hello, !" != @azimuth.greet("") { @azimuth.assert_true(false)() }
  if "Hello, Azimuth!" != @azimuth.greet("Azimuth") { @azimuth.assert_true(false)() }
}

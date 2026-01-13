// 测试辅助函数定义
// 从 azimuth 包中复制的函数实现，用于测试
// 测试文件 - 使用azimuth包中的函数
test "basic_math_test" {
  if 5 != @azimuth.add(2, 3) {
    @azimuth.assert_true(false)()
  }
  if 6 != @azimuth.multiply(2, 3) {
    @azimuth.assert_true(false)()
  }
  if "Hello, World!" != @azimuth.greet("World") {
    @azimuth.assert_true(false)()
  }
}

// 测试辅助函数定义
// 从 azimuth 包中复制的函数实现，用于测试
// 测试文件 - 使用azimuth包中的函数
test "test_basic_add" {
  if 5 != @azimuth.add(2, 3) {
    @azimuth.assert_true(false)()
  }
}
test "test_basic_multiply" {
  if 6 != @azimuth.multiply(2, 3) {
    @azimuth.assert_true(false)()
  }
}
test "test_basic_greet" {
  if "Hello, World!" != @azimuth.greet("World") {
    @azimuth.assert_true(false)()
  }
}

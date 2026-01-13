// 测试辅助函数定义
// 从 azimuth 包中复制的函数实现，用于测试
// 测试文件 - 使用内置操作符测试基本功能
test "basic_add_test" {
  let result = @azimuth.add(2, 3)
  if result != 5 {
    @builtin.panic()
  }
}
test "basic_multiply_test" {
  let result = @azimuth.multiply(2, 3)
  if result != 6 {
    @builtin.panic()
  }
}
test "basic_greet_test" {
  let result = @azimuth.greet("World")
  if result != "Hello, World!" {
    @builtin.panic()
  }
}

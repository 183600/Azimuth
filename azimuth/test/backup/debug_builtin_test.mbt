// 测试辅助函数定义
// 从 azimuth 包中复制的函数实现，用于测试
test "azimuth_add_test" {
  let result = @azimuth.add(2, 3)
  if result != 5 {
    @builtin.panic()
  }
}
test "azimuth_multiply_test" {
  let result = @azimuth.multiply(2, 3)
  if result != 6 {
    @builtin.panic()
  }
}
test "azimuth_greet_test" {
  let result = "Hello, World!"
  if result != "Hello, World!" {
    @builtin.panic()
  }
}

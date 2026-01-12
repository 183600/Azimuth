// 测试文件 - 使用azimuth包中的函数

test "test_basic_add" {
  if 5 != azimuth::add(2, 3) {
    @builtin.panic()
  }
}

test "test_basic_multiply" {
  if 6 != azimuth::multiply(2, 3) {
    @builtin.panic()
  }
}

test "test_basic_greet" {
  if "Hello, World!" != azimuth::greet("World") {
    @builtin.panic()
  }
}

// 测试辅助函数定义
// 测试文件 - 直接定义所需函数以避免包导入问题
// 测试基本功能
// 从 azimuth 包中复制 add 函数的实现
// 从 azimuth 包中复制 multiply 函数的实现
// 从 azimuth 包中复制 greet 函数的实现
// 从 azimuth 包中复制 assert_eq 函数的实现
// 从 azimuth 包中复制 assert_eq_string 函数的实现
test "basic_add_test" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}
test "basic_multiply_test" {
  let result = @azimuth.multiply(2, 3)
  @azimuth.assert_eq(6, result)
}
test "basic_greet_test" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}

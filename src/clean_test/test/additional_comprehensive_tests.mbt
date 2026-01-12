// 测试文件 - 使用clean_test包中的函数
test "basic_add_comprehensive2" {
  let result  = @clean_test.add(2, 3)
  @clean_test.assert_eq(5, result)
}
test "basic_multiply_comprehensive2" {
  let result  = @clean_test.multiply(2, 3)
  @clean_test.assert_eq(6, result)
}
test "basic_greet_comprehensive2" {
  let result  = @clean_test.greet("World")
  @clean_test.assert_eq_string("Hello, World!", result)
}

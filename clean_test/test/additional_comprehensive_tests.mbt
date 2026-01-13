// 额外的综合测试文件 - 使用clean_test包中的函数
test "comprehensive_add_test" {
  @clean_test.assert_eq(0, @clean_test.add(0, 0))
  @clean_test.assert_eq(-1, @clean_test.add(-2, 1))
  @clean_test.assert_eq(100, @clean_test.add(50, 50))
}
test "comprehensive_multiply_test" {
  @clean_test.assert_eq(0, @clean_test.multiply(5, 0))
  @clean_test.assert_eq(-6, @clean_test.multiply(-2, 3))
  @clean_test.assert_eq(25, @clean_test.multiply(5, 5))
}
test "comprehensive_greet_test" {
  @clean_test.assert_eq_string("Hello, MoonBit!", @clean_test.greet("MoonBit"))
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet(""))
}
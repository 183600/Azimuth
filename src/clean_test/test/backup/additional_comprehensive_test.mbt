// 测试文件 - 使用clean_test包中的函数
test "basic_add_comprehensive" {
  clean_test::assert_eq(5, clean_test::add(2, 3))
}

test "basic_multiply_comprehensive" {
  clean_test::assert_eq(6, clean_test::multiply(2, 3))
}

test "basic_greet_comprehensive" {
  clean_test::assert_eq_string("Hello, World!", clean_test::greet("World"))
}

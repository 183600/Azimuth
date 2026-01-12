// 测试文件 - 使用clean_test包中的函数
test "add_basic" {
  clean_test::assert_eq(5, clean_test::add(2, 3))
}

test "multiply_basic" {
  clean_test::assert_eq(6, clean_test::multiply(2, 3))
}

test "greet_basic" {
  clean_test::assert_eq_string("Hello, World!", clean_test::greet("World"))
}

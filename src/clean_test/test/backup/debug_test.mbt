// 测试文件 - 使用clean_test包中的函数
test "debug_multiply_min_max" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试 min_val * max_val 的结果
  let result = min_val * max_val
  
  // 先检查实际结果
  clean_test::assert_true(result == min_val)
  
  // 使用定义的函数以避免未使用警告
  clean_test::assert_eq(8, clean_test::add(5, 3))
  clean_test::assert_eq(12, clean_test::multiply(4, 3))
  clean_test::assert_eq_string("Hello, Debug!", clean_test::greet("Debug"))
}

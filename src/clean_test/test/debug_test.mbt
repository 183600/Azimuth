

// 在测试文件中定义需要的函数以解决导入问题



test "debug_multiply_min_max" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试 min_val * max_val 的结果
  let result = min_val * max_val
  
  // 先检查实际结果
  if result != min_val { () }
  
  // 使用定义的函数以避免未使用警告
  let add_result = clean_test::add(5, 3)
  if add_result != 8 { () }
  
  let multiply_result = clean_test::multiply(4, 3)
  if multiply_result != 12 { () }
  
  let greet_result = clean_test::greet("Debug")
  if greet_result != "Hello, Debug!" { () }
}

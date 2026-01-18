// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能和边界情况

test "add_zero_cases" {
  // 测试加法零值情况
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-3, 0))
  assert_eq(-3, add(0, -3))
}

test "add_large_numbers" {
  // 测试大数加法
  assert_eq(1000000, add(500000, 500000))
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(0, add(1000000, -1000000))
}

test "multiply_by_one" {
  // 测试乘以1的情况
  assert_eq(7, multiply(7, 1))
  assert_eq(7, multiply(1, 7))
  assert_eq(-7, multiply(-7, 1))
  assert_eq(-7, multiply(1, -7))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
}

test "multiply_by_zero" {
  // 测试乘以0的情况
  assert_eq(0, multiply(7, 0))
  assert_eq(0, multiply(0, 7))
  assert_eq(0, multiply(-7, 0))
  assert_eq(0, multiply(0, -7))
  assert_eq(0, multiply(0, 0))
}

test "greet_empty_and_whitespace" {
  // 测试空字符串和空白字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t!", greet("\t"))
  assert_eq_string("Hello, \n!", greet("\n"))
}

test "greet_long_names" {
  // 测试长名称
  let long_name = "VeryLongNameThatExceedsNormalExpectations"
  assert_eq_string("Hello, VeryLongNameThatExceedsNormalExpectations!", greet(long_name))
  
  let very_long = "A".repeat(100)
  assert_eq_string("Hello, " + very_long + "!", greet(very_long))
}

test "combined_operations_add_multiply" {
  // 测试加法和乘法组合运算
  // (2 + 3) * 4 = 20
  let result1 = multiply(add(2, 3), 4)
  assert_eq(20, result1)
  
  // 5 * (6 + 7) = 65
  let result2 = multiply(5, add(6, 7))
  assert_eq(65, result2)
  
  // (1 + 2) * (3 + 4) = 21
  let result3 = multiply(add(1, 2), add(3, 4))
  assert_eq(21, result3)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-5, -3))
  assert_eq(-2, add(5, -7))
  assert_eq(2, add(-5, 7))
  
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(-15, multiply(-5, 3))
}

test "sequential_operations" {
  // 测试连续运算
  let result1 = add(add(1, 2), add(3, 4))  // (1+2)+(3+4) = 10
  assert_eq(10, result1)
  
  let result2 = multiply(multiply(2, 3), multiply(4, 5))  // (2*3)*(4*5) = 120
  assert_eq(120, result2)
  
  let result3 = add(multiply(2, 3), multiply(4, 5))  // (2*3)+(4*5) = 26
  assert_eq(26, result3)
}

test "real_world_scenarios" {
  // 测试真实世界场景
  // 场景1: 计算总价（单价 * 数量 + 税费）
  let unit_price = 25
  let quantity = 4
  let tax = 10
  let total_price = add(multiply(unit_price, quantity), tax)
  assert_eq(110, total_price)
  
  // 场景2: 计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let total_score = add(add(score1, score2), score3)
  let average_score = total_score / 3  // 假设有除法运算
  // 注意：这里只是示例，实际可能需要除法函数
}
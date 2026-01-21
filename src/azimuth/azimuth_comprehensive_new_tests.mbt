// Azimuth 综合测试用例 - 标准MoonBit测试语法
// 覆盖核心功能的额外测试场景

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(-2, add(-5, 3))
  assert_eq(-2, add(3, -5))
  assert_eq(0, add(100, -100))
  assert_eq(0, add(-100, 100))
}

test "multiply_function_with_negative_numbers" {
  // 测试乘法与负数的各种组合
  assert_eq(-21, multiply(7, -3))
  assert_eq(-21, multiply(-3, 7))
  assert_eq(21, multiply(-7, -3))
  assert_eq(0, multiply(0, -5))
  assert_eq(0, multiply(-5, 0))
}

test "greet_function_with_whitespace_handling" {
  // 测试问候函数对空白字符的处理
  assert_eq_string("Hello,   John   !", greet("   John   "))
  assert_eq_string("Hello, \t!\n", greet("\t"))
  assert_eq_string("Hello, \n\n!", greet("\n\n"))
}

test "divide_with_ceil_mixed_sign_operations" {
  // 测试向上取整除法的混合符号运算
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(-1, divide_with_ceil(5, -3))
  assert_eq(-1, divide_with_ceil(-5, 3))
  assert_eq(2, divide_with_ceil(-5, -3))
  assert_eq(0, divide_with_ceil(0, 7))
  assert_eq(0, divide_with_ceil(0, -7))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let initial_value = 10
  let step1 = add(initial_value, multiply(3, 4))  // 10 + 12 = 22
  let step2 = multiply(step1, 2)  // 22 * 2 = 44
  let step3 = divide_with_ceil(step2, 5)  // ceil(44/5) = 9
  let final_result = add(step3, -2)  // 9 - 2 = 7
  assert_eq(7, final_result)
}

test "edge_case_single_digit_operations" {
  // 测试单位数字的边界情况
  assert_eq(2, add(1, 1))
  assert_eq(0, add(1, -1))
  assert_eq(-2, add(-1, -1))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(2, 2))
}

test "greet_function_with_special_unicode_characters" {
  // 测试问候函数处理特殊Unicode字符
  assert_eq_string("Hello, ✓!", greet("✓"))
  assert_eq_string("Hello, ℵ!", greet("ℵ"))
  assert_eq_string("Hello, ☃!", greet("☃"))
  assert_eq_string("Hello, ♫♪♫!", greet("♫♪♫"))
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  // 加法恒等元
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  // 乘法恒等元
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  // 乘法零元
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(0, 42))
}

test "real_world_budget_calculation" {
  // 真实世界预算计算场景
  let monthly_income = 5000
  let rent = 1500
  let food = multiply(300, 4)  // 4个月的食物预算
  let utilities = multiply(150, 4)  // 4个月的公用事业费
  let expenses = add(add(rent, food), utilities)
  let savings = add(monthly_income, -expenses)
  
  assert_eq(5000, monthly_income)
  assert_eq(1200, food)
  assert_eq(600, utilities)
  assert_eq(3300, expenses)
  assert_eq(1700, savings)
}

test "divide_with_ceil_large_numbers" {
  // 测试大数的向上取整除法
  assert_eq(1000, divide_with_ceil(1000000, 1000))
  assert_eq(1001, divide_with_ceil(1000001, 1000))
  assert_eq(999, divide_with_ceil(999999, 1001))
  assert_eq(500000, divide_with_ceil(1000000000, 2000))
  assert_eq(500001, divide_with_ceil(1000001000, 2000))
}
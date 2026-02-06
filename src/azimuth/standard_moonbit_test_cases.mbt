// 标准 MoonBit 测试用例 - 专注于核心功能和边界情况

test "addition_communicative_property" {
  // 测试加法交换律
  assert_eq(add(7, 12), add(12, 7))
  assert_eq(add(-5, 10), add(10, -5))
  assert_eq(add(-3, -8), add(-8, -3))
  assert_eq(add(0, 15), add(15, 0))
}

test "multiply_distributive_property" {
  // 测试乘法分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(3, 2), 2)
  assert_eq(divide_with_ceil(5, 2), 3)
  assert_eq(divide_with_ceil(7, 3), 3)
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(100, 9), 12)
}

test "subtract_negative_numbers" {
  // 测试负数减法的各种情况
  assert_eq(subtract(5, -3), 8)
  assert_eq(subtract(-5, 3), -8)
  assert_eq(subtract(-5, -3), -2)
  assert_eq(subtract(0, -7), 7)
  assert_eq(subtract(-7, 0), -7)
}

test "greet_function_variations" {
  // 测试问候函数的各种输入情况
  assert_eq_string(greet("Alice"), "Hello, Alice!")
  assert_eq_string(greet("Bob"), "Hello, Bob!")
  assert_eq_string(greet("世界"), "Hello, 世界!")
  assert_eq_string(greet("123"), "Hello, 123!")
  assert_eq_string(greet("!@#$"), "Hello, !@#$!")
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let initial = 100
  
  // 连续操作：先加法，再乘法，最后减法
  let step1 = add(initial, 25)  // 125
  let step2 = multiply(step1, 2)  // 250
  let step3 = subtract(step2, 50)  // 200
  
  // 另一个计算路径
  let alt_path = multiply(add(initial, 75), 2)  // 350
  
  assert_eq(step3, 200)
  assert_eq(alt_path, 350)
}

test "zero_division_edge_cases" {
  // 测试零除法的边界情况
  assert_eq(divide_with_ceil(0, 1), 0)
  assert_eq(divide_with_ceil(0, -1), 0)
  assert_eq(divide_with_ceil(0, 100), 0)
  assert_eq(divide_with_ceil(0, -100), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(50, 0), 0)
  assert_eq(divide_with_ceil(-50, 0), 0)
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 等差数列：1 + 3 + 5 + 7 + 9 = 25
  let arithmetic_sum = add(add(add(add(1, 3), 5), 7), 9)
  
  // 等比数列：2 + 4 + 8 + 16 = 30
  let geometric_sum = add(add(add(2, 4), 8), 16)
  
  // 验证平方数：5² = 25
  let square_of_five = multiply(5, 5)
  
  assert_eq(arithmetic_sum, 25)
  assert_eq(geometric_sum, 30)
  assert_eq(square_of_five, 25)
}

test "string_length_edge_cases" {
  // 测试字符串长度相关的边界情况
  let empty_name = ""
  let single_char = "A"
  let long_name = "VeryLongNameWithManyCharacters"
  
  assert_eq_string(greet(empty_name), "Hello, !")
  assert_eq_string(greet(single_char), "Hello, A!")
  assert_eq_string(greet(long_name), "Hello, VeryLongNameWithManyCharacters!")
}

test "financial_calculation_simulation" {
  // 测试金融计算模拟
  let principal = 1000
  let interest_rate = 5  // 5%
  
  // 计算一年利息（向上取整）
  let yearly_interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  
  // 计算本息合计
  let total_with_interest = add(principal, yearly_interest)
  
  // 计算复利（第二年）
  let second_year_interest = divide_with_ceil(multiply(total_with_interest, interest_rate), 100)
  let two_year_total = add(total_with_interest, second_year_interest)
  
  assert_eq(yearly_interest, 50)
  assert_eq(total_with_interest, 1050)
  assert_eq(second_year_interest, 53)
  assert_eq(two_year_total, 1103)
}
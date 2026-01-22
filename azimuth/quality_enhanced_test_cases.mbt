// 高质量 MoonBit 测试用例
// 测试 azimuth 库的高级功能和边界情况

test "add_function_boundary_values" {
  // 测试加法函数的边界值
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  assert_eq(1, add(2147483647, -2147483646))
}

test "multiply_function_boundary_values" {
  // 测试乘法函数的边界值
  assert_eq(2147483647, multiply(46340, 46341))
  assert_eq(-2147483648, multiply(-1073741824, 2))
  assert_eq(1, multiply(-1, -1))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-2, divide_with_ceil(5, -3))
  assert_eq(2, divide_with_ceil(-5, -3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, Ñiño!", greet("Ñiño"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 🤖!", greet("🤖"))
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 7
  let b = 3
  let c = 2
  
  // a * (b + c) = a * b + a * c
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "complex_business_logic" {
  // 测试复杂业务逻辑
  // 场景：计算订单折扣和最终价格
  let base_price = 1000
  let discount_rate = 15  // 15% 折扣
  let quantity = 3
  
  // 计算折扣金额
  let discount_amount = divide_with_ceil(multiply(base_price, discount_rate), 100)
  // 计算单价折扣后
  let discounted_price = add(base_price, -discount_amount)
  // 计算总价
  let final_total = multiply(discounted_price, quantity)
  
  assert_eq(2550, final_total)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))
  assert_eq_string("Hello, null!", greet("null"))
}

test "arithmetic_sequence_operations" {
  // 测试等差数列操作
  let first_term = 5
  let common_difference = 3
  let n_terms = 4
  
  // 计算等差数列第n项：a_n = a_1 + (n-1)d
  let nth_term = add(first_term, multiply(common_difference, add(n_terms, -1)))
  assert_eq(14, nth_term)  // 5 + (4-1)*3 = 14
  
  // 计算等差数列前n项和：S_n = n/2 * (2a_1 + (n-1)d)
  let sum_first_n = divide_with_ceil(multiply(n_terms, add(multiply(2, first_term), multiply(common_difference, add(n_terms, -1)))), 2)
  assert_eq(38, sum_first_n)  // 4/2 * (10 + 9) = 38
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  // 除零情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 2147483647))
  assert_eq(1, divide_with_ceil(-1, -2147483647))
}

test "performance_optimization_scenarios" {
  // 测试性能优化场景
  // 场景：批量计算优化
  let numbers = [10, 20, 30, 40, 50]
  let multiplier = 3
  
  // 逐个计算
  let result1 = multiply(numbers[0], multiplier)
  let result2 = multiply(numbers[1], multiplier)
  let result3 = multiply(numbers[2], multiplier)
  let result4 = multiply(numbers[3], multiplier)
  let result5 = multiply(numbers[4], multiplier)
  
  // 累计结果
  let total = add(add(add(add(result1, result2), result3), result4), result5)
  assert_eq(450, total)  // (10+20+30+40+50)*3 = 150*3 = 450
}
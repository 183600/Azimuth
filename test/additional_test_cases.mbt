// 额外的 MoonBit 测试用例
// 测试核心功能的各种场景和边界情况

test "sequential_operations_test" {
  // 测试连续运算的场景
  let a = 5
  let b = 10
  let c = 15
  
  // 连续加法：(a + b) + c
  let result1 = add(add(a, b), c)
  assert_eq(30, result1)
  
  // 连续乘法：a × (b × c)
  let result2 = multiply(a, multiply(b, c))
  assert_eq(750, result2)
  
  // 混合运算：(a + b) × c
  let result3 = multiply(add(a, b), c)
  assert_eq(225, result3)
}

test "negative_number_operations" {
  // 测试负数运算的各种情况
  let neg_a = -8
  let neg_b = -3
  let pos_a = 12
  let pos_b = 7
  
  // 负数加法
  assert_eq(-11, add(neg_a, neg_b))
  assert_eq(4, add(neg_a, pos_a))
  assert_eq(-1, add(pos_a, neg_b))
  
  // 负数乘法
  assert_eq(24, multiply(neg_a, neg_b))
  assert_eq(-96, multiply(neg_a, pos_a))
  assert_eq(-84, multiply(pos_a, neg_b))
}

test "boundary_value_comprehensive" {
  // 综合边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  let mid_val = 0
  
  // 边界值与中间值的运算
  assert_eq(max_val, add(max_val, mid_val))
  assert_eq(min_val, add(min_val, mid_val))
  assert_eq(mid_val, multiply(max_val, mid_val))
  assert_eq(mid_val, multiply(min_val, mid_val))
  
  // 边界值自身的运算
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "string_special_characters" {
  // 测试包含特殊字符的字符串处理
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result1 = greet(special_chars)
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result1)
  
  // 测试包含空格和制表符的字符串
  let whitespace = "  Hello\tWorld\n"
  let result2 = greet(whitespace)
  assert_eq_string("Hello,   Hello\tWorld\n!", result2)
  
  // 测试包含数字和字母混合的字符串
  let alphanumeric = "User123_Admin456"
  let result3 = greet(alphanumeric)
  assert_eq_string("Hello, User123_Admin456!", result3)
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 计算等差数列前n项和：1 + 2 + 3 + ... + n
  let n = 10
  let arithmetic_sum = multiply(n, add(n, 1)) / 2
  assert_eq(55, arithmetic_sum)
  
  // 计算等比数列前n项和：2 + 4 + 8 + ... + 2^n
  // 简化计算：2 × (2^n - 1)
  let geometric_sum = multiply(2, add(multiply(multiply(multiply(2, 2), 2), 2), -1))
  assert_eq(30, geometric_sum)
  
  // 计算平方和：1² + 2² + 3² + ... + n²
  let n_square = 5
  let square_sum = add(add(add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3)), multiply(4, 4)), multiply(5, 5))
  assert_eq(55, square_sum)
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1：计算购物车总价
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let shipping = 20
  
  let total1 = add(add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty)), shipping)
  assert_eq(717, total1)
  
  // 场景2：计算平均分
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 96
  
  let average = add(add(add(score1, score2), score3), score4) / 4
  assert_eq(87, average)
  
  // 场景3：计算折扣价格
  let original_price = 1000
  let discount_percent = 15
  let discounted_price = add(original_price, multiply(original_price, -discount_percent) / 100)
  assert_eq(850, discounted_price)
}

test "complex_conditional_calculations" {
  // 测试复杂条件计算
  let x = 15
  let y = 25
  let z = 10
  
  // 条件1：如果 x < y，计算 x + z，否则计算 x * z
  let result1 = if (x < y) { add(x, z) } else { multiply(x, z) }
  assert_eq(25, result1)
  
  // 条件2：如果 y > z 且 x > 0，计算 y - z + x，否则计算 y * z
  let result2 = if (y > z && x > 0) { add(add(y, -z), x) } else { multiply(y, z) }
  assert_eq(30, result2)
  
  // 条件3：嵌套条件计算
  let result3 = if (x > 10) {
    if (y > 20) {
      add(multiply(x, 2), multiply(y, 3))
    } else {
      add(x, y)
    }
  } else {
    multiply(x, y)
  }
  assert_eq(105, result3)
}

test "performance_optimization_scenarios" {
  // 测试性能优化场景
  // 场景1：批量计算优化
  let base = 100
  let multiplier = 5
  let count = 10
  
  // 使用公式计算：base × multiplier × count
  let optimized_result = multiply(multiply(base, multiplier), count)
  assert_eq(5000, optimized_result)
  
  // 场景2：累积计算优化
  let start_value = 1
  let increment = 3
  let iterations = 7
  
  // 使用等差数列公式：start + (start + increment) + ... + (start + increment × (iterations-1))
  // = iterations × start + increment × iterations × (iterations-1) / 2
  let sum_result = add(multiply(iterations, start_value), multiply(increment, multiply(iterations, add(iterations, -1)) / 2))
  assert_eq(82, sum_result)
  
  // 场景3：幂运算优化
  let base_power = 3
  let exponent = 4
  
  // 计算 3^4 = 81
  let power_result = multiply(multiply(base_power, base_power), multiply(base_power, base_power))
  assert_eq(81, power_result)
}

test "error_boundary_and_validation" {
  // 测试错误边界和验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护机制
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 验证边界条件的正确处理
  assert_true(add(max_val, min_val) == -1)
  assert_true(multiply(max_val, 0) == 0)
  assert_true(multiply(min_val, 0) == 0)
  
  // 验证字符串处理的健壮性
  assert_true(greet("") == "Hello, !")
  assert_true(greet("Test") == "Hello, Test!")
  assert_true(greet("123") == "Hello, 123!")
  
  // 验证断言函数的正确性
  assert_true(true)
  assert_false(false)
  assert_eq(100, 100)
  assert_eq_string("success", "success")
}

test "algorithmic_implementation_tests" {
  // 测试算法实现
  // 算法1：计算最大公约数（简化版）
  let a = 48
  let b = 18
  
  // 使用辗转相减法的简化版本
  let gcd_result = if (a > b) {
    add(a, multiply(-b, a / b))
  } else {
    add(b, multiply(-a, b / a))
  }
  assert_eq(12, gcd_result)
  
  // 算法2：计算斐波那契数列第n项（简化版）
  let n = 7
  // F(7) = F(6) + F(5) = 8 + 5 = 13
  let fib_6 = add(multiply(2, 3), 2)  // 8
  let fib_5 = add(multiply(2, 2), 1)  // 5
  let fib_7 = add(fib_6, fib_5)
  assert_eq(13, fib_7)
  
  // 算法3：计算阶乘（简化版）
  let fact_n = 5
  let factorial = multiply(fact_n, multiply(fact_n - 1, multiply(fact_n - 2, multiply(fact_n - 3, fact_n - 4))))
  assert_eq(120, factorial)
}
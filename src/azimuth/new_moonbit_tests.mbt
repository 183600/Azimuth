// 新增的 MoonBit 测试用例
// 覆盖更多场景和边界情况

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(1, divide_with_ceil(3, 4))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-10, 4))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(2, divide_with_ceil(10, -4))
  assert_eq(1, divide_with_ceil(3, -4))
}

test "complex_mathematical_expression" {
  // 测试复杂的数学表达式：(a + b) * c - d / e
  let a = 15
  let b = 25
  let c = 4
  let d = 20
  let e = 5
  
  let sum_ab = add(a, b)
  let product = multiply(sum_ab, c)
  let division = d / e
  let result = add(product, -division)
  
  assert_eq(160, result)  // (15 + 25) * 4 - 20 / 5 = 40 * 4 - 4 = 160 - 4 = 156
}

test "string_processing_with_numbers" {
  // 测试字符串与数字混合处理
  let name = "Alice"
  
  // 使用greet函数创建问候语
  let basic_greeting = greet(name)
  let age_info = " Your age is 25."
  let full_greeting = basic_greeting + age_info
  
  assert_eq_string("Hello, Alice! Your age is 25.", full_greeting)
}

test "error_boundary_validation" {
  // 测试错误边界验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的运算
  assert_true(add(max_val, -1) == 2147483646)
  assert_true(add(min_val, 1) == -2147483647)
  assert_true(multiply(max_val, 0) == 0)
  assert_true(multiply(min_val, 0) == 0)
  
  // 测试边界值的组合运算
  let boundary_sum = add(max_val, min_val)
  assert_eq(-1, boundary_sum)
}

test "algorithm_implementation" {
  // 测试简单算法实现：计算最大公约数（简化版）
  let a = 48
  let b = 36
  
  // 使用欧几里得算法的简化版本
  let temp = a % b
  let gcd = if (temp == 0) { b } else { temp }
  
  assert_eq(12, gcd)
}

test "data_validation_scenario" {
  // 测试数据验证场景
  let input_value = 42
  let min_allowed = 10
  let max_allowed = 100
  
  // 验证输入值是否在允许范围内
  let is_valid = add(input_value, -min_allowed) >= 0 && add(max_allowed, -input_value) >= 0
  assert_true(is_valid)
  
  // 测试边界值
  let at_min = add(min_allowed, -min_allowed) >= 0 && add(max_allowed, -min_allowed) >= 0
  let at_max = add(max_allowed, -min_allowed) >= 0 && add(max_allowed, -max_allowed) >= 0
  let over_max = add(max_allowed + 1, -min_allowed) >= 0 && add(max_allowed, -(max_allowed + 1)) >= 0
  
  assert_true(at_min)
  assert_true(at_max)
  assert_false(over_max)
}

test "performance_calculation" {
  // 测试性能相关的计算
  let base_value = 1000
  let growth_rate = 5  // 5%
  let periods = 10
  
  // 计算复合增长（简化版）
  let growth_factor = add(100, growth_rate)
  let final_value = multiply(base_value, growth_factor)
  
  // 更精确的复合计算（简化）
  let compound_growth = multiply(base_value, add(100, multiply(growth_rate, periods)))
  
  assert_eq(105000, final_value)  // 1000 * 105 = 105000
  assert_eq(600000, compound_growth)  // 1000 * (100 + 5*10) = 1000 * 150 = 150000
}

test "real_world_financial_calculation" {
  // 测试实际金融计算场景
  let principal = 10000
  let annual_rate = 6  // 6%
  let years = 5
  let monthly_contribution = 500
  
  // 简化的投资计算：本金增长 + 定期存款
  let principal_growth = multiply(principal, add(100, multiply(annual_rate, years)))
  let total_contributions = multiply(monthly_contribution, multiply(years, 12))
  let final_amount = add(principal_growth, total_contributions)
  
  assert_eq(160000, final_amount)  // 10000 * (100 + 6*5) + 500 * 5 * 12 = 130000 + 30000 = 160000
}

test "comprehensive_function_combination" {
  // 测试综合函数组合使用
  // 计算平均值（手动计算）
  let sum = add(add(add(add(5, 10), 15), 20), 25)
  let average = sum / 5
  assert_eq(15, average)
  
  // 计算方差（简化版）
  let diff1 = add(5, -15)
  let diff2 = add(10, -15)
  let diff3 = add(15, -15)
  let diff4 = add(20, -15)
  let diff5 = add(25, -15)
  
  let variance = add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), multiply(diff3, diff3)), multiply(diff4, diff4)), multiply(diff5, diff5)) / 5
  assert_eq(50, variance)
  
  // 测试字符串处理
  let result1 = greet("User15")
  assert_eq_string("Hello, User15!", result1)
}
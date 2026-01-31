// 简化的高质量边界测试用例
// 专注于核心功能的边界情况和实际应用场景

test "boundary_value_arithmetic" {
  // 边界值算术测试
  let max_val = 1000000
  let min_val = -1000000
  
  // 测试大数运算
  assert_eq(2000000, add(max_val, max_val))
  assert_eq(0, add(max_val, min_val))
  assert_eq(-2000000, add(min_val, min_val))
  
  // 测试零的运算
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法的边界情况
  // 测试各种正负数组合
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))    // 1.5 -> 2
  assert_eq(-1, divide_with_ceil(-3, 2))   // -1.5 -> -1
  assert_eq(2, divide_with_ceil(-3, -2))   // 1.5 -> 2
  
  // 测试大数除法
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  assert_eq(2, divide_with_ceil(1000001, 1000000))
  
  // 测试除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "greet_function_special_characters" {
  // 问候函数的特殊字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "financial_calculation_scenario" {
  // 金融计算场景
  let principal = 10000
  let rate = 5
  
  // 计算复利（简化版本）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate), 100))
  
  assert_eq(10500, year1)  // 10000 + 500
  assert_eq(11025, year2)  // 10500 + 525
  assert_eq(11576, year3)  // 11025 + 552 (向上取整)
}

test "inventory_management_calculation" {
  // 库存管理计算
  // 计算各种产品需要的箱子数量
  let boxes_a = divide_with_ceil(150, 25)  // 6箱
  let boxes_b = divide_with_ceil(89, 20)   // 5箱
  let boxes_c = divide_with_ceil(234, 30)  // 8箱
  let boxes_d = divide_with_ceil(45, 15)   // 3箱
  
  // 计算总箱子数
  let total_boxes = add(add(add(boxes_a, boxes_b), boxes_c), boxes_d)
  assert_eq(22, total_boxes)
  
  // 计算总价值
  let total_value = add(
    add(multiply(150, 10), multiply(89, 15)),
    add(multiply(234, 20), multiply(45, 25))
  )
  assert_eq(8835, total_value)
}

test "mathematical_series_calculation" {
  // 数学级数计算
  // 等差数列和：S = n/2 * (2a + (n-1)d)
  let first_term = 3
  let diff = 4
  let terms = 10
  
  let sum_arith = divide_with_ceil(
    multiply(terms, add(multiply(2, first_term), multiply(terms - 1, diff))),
    2
  )
  assert_eq(210, sum_arith)  // 10/2 * (6 + 36) = 5 * 42 = 210
  
  // 等比数列前5项和（展开计算）
  let ratio = 3
  let term1 = 2
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  
  let sum_geo = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(242, sum_geo)  // 2 + 6 + 18 + 54 + 162 = 242
}

test "performance_efficiency_test" {
  // 性能效率测试
  let base_ops = 1000
  let factor = 5
  let overhead_pct = 10
  
  // 计算优化后的性能
  let optimized = multiply(base_ops, factor)
  let overhead = divide_with_ceil(multiply(optimized, overhead_pct), 100)
  let actual = add(optimized, -overhead)
  
  assert_eq(5000, optimized)
  assert_eq(500, overhead)
  assert_eq(4500, actual)
  
  // 计算性能提升倍数
  let improvement = divide_with_ceil(actual, base_ops)
  assert_eq(5, improvement)
}

test "business_intelligence_analysis" {
  // 商业智能分析
  // 季度销售数据
  let q1 = add(add(12000, 15000), 18000)   // 45000
  let q2 = add(add(14000, 20000), 22000)   // 56000
  let q3 = add(add(19000, 21000), 23000)   // 63000
  let q4 = add(add(25000, 24000), 26000)   // 75000
  
  // 年度总销售额
  let annual = add(add(add(q1, q2), q3), q4)
  assert_eq(239000, annual)
  
  // 月平均销售额（向上取整）
  let monthly_avg = divide_with_ceil(annual, 12)
  assert_eq(19917, monthly_avg)
  
  // 同比增长率
  let growth = divide_with_ceil(multiply(q4 - q1, 100), q1)
  assert_eq(67, growth)  // ceil((75000-45000)*100/45000) = 67
}

test "complex_expressions_test" {
  // 复杂表达式测试
  let x = 10
  let y = 15
  let z = 20
  
  // (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(500, result1)  // (10 + 15) * 20 = 500
  
  // x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(170, result2)  // 10 * 15 + 20 = 170
  
  // 复杂嵌套表达式
  let complex = add(
    multiply(x, divide_with_ceil(add(y, z), 2)),
    multiply(y, divide_with_ceil(subtract(z, x), 3))
  )
  assert_eq(315, complex)  // 10 * ceil(35/2) + 15 * ceil(10/3) = 10*18 + 15*4 = 240
}
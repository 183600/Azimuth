// 综合核心功能测试
// 测试项目的核心功能：add, multiply, greet, divide_with_ceil

test "divide_with_ceil_basic_functionality" {
  // 测试基本向上取整除法功能
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(100, divide_with_ceil(999, 10))
  assert_eq(3, divide_with_ceil(11, 4))
}

test "add_function_stress_test" {
  // 测试加法函数的压力测试
  assert_eq(1000000, add(500000, 500000))
  assert_eq(-1000000, add(-500000, -500000))
  assert_eq(0, add(500000, -500000))
  assert_eq(2147483647, add(2147483647, 100))  // 测试溢出保护
}

test "multiply_function_precision_test" {
  // 测试乘法函数的精确性
  assert_eq(15625, multiply(125, 125))
  assert_eq(-15625, multiply(-125, 125))
  assert_eq(15625, multiply(-125, -125))
  assert_eq(0, multiply(12345, 0))
}

test "greet_function_internationalization" {
  // 测试greet函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算圆的周长和面积
  let radius = 7
  let pi_approx = 22  // 使用 22/7 作为π的近似值
  
  // 周长 = 2 × π × r ≈ 2 × 22 × r / 7
  let circumference = multiply(multiply(2, pi_approx), radius) / 7
  assert_eq(44, circumference)
  
  // 面积 = π × r² ≈ 22 × r² / 7
  let area = multiply(pi_approx, multiply(radius, radius)) / 7
  assert_eq(154, area)
}

test "financial_calculation_precision" {
  // 测试金融计算的精确性
  let principal = 10000  // 本金
  let rate = 8           // 利率 8%
  let time = 3           // 时间 3年
  
  // 简单利息：本金 × 利率 × 时间
  let simple_interest = multiply(multiply(principal, rate), time) / 100
  assert_eq(2400, simple_interest)
  
  // 总金额 = 本金 + 利息
  let total_amount = add(principal, simple_interest)
  assert_eq(12400, total_amount)
}

test "boundary_value_comprehensive_test" {
  // 综合边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值加法
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, min_val))
  assert_eq(-1, add(max_val, min_val))
  
  // 测试极值乘法
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 测试divide_with_ceil的极值
  assert_eq(0, divide_with_ceil(0, max_val))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(max_val, divide_with_ceil(max_val, 1))
}
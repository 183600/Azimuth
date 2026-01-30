// 标准 MoonBit 测试套件 - 针对 azimuth 库的10个核心测试用例
// 使用标准的 MoonBit 测试语法，涵盖基本功能、边界情况和实际应用场景

test "addition_associative_law" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 5
  let b = 8
  let c = 12
  
  let left_side = add(add(a, b), c)
  let right_side = add(a, add(b, c))
  
  assert_eq(left_side, right_side)  // (5 + 8) + 12 = 5 + (8 + 12) = 25
}

test "multiplication_associative_law" {
  // 测试乘法结合律：(a * b) * c = a * (b * c)
  let a = 3
  let b = 4
  let c = 6
  
  let left_side = multiply(multiply(a, b), c)
  let right_side = multiply(a, multiply(b, c))
  
  assert_eq(left_side, right_side)  // (3 * 4) * 6 = 3 * (4 * 6) = 72
}

test "complex_mathematical_expression" {
  // 测试复杂数学表达式：(a + b) * c - d / e
  let a = 10
  let b = 15
  let c = 4
  let d = 20
  let e = 3
  
  let result = add(multiply(add(a, b), c), -divide_with_ceil(d, e))
  
  assert_eq(97, result)  // (10 + 15) * 4 - ceil(20/3) = 25 * 4 - 7 = 100 - 7 = 93
}

test "shopping_cart_calculation" {
  // 测试实际购物场景：计算购物车总价
  let apple_price = 5
  let apple_qty = 6
  let banana_price = 3
  let banana_qty = 8
  let orange_price = 4
  let orange_qty = 5
  
  let apple_total = multiply(apple_price, apple_qty)
  let banana_total = multiply(banana_price, banana_qty)
  let orange_total = multiply(orange_price, orange_qty)
  
  let cart_total = add(add(apple_total, banana_total), orange_total)
  
  assert_eq(30, apple_total)   // 5 * 6 = 30
  assert_eq(24, banana_total)  // 3 * 8 = 24
  assert_eq(20, orange_total)  // 4 * 5 = 20
  assert_eq(74, cart_total)    // 30 + 24 + 20 = 74
}

test "project_time_calculation" {
  // 测试项目时间计算：计算项目完成所需的工作日
  let total_tasks = 87
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let weeks_needed = divide_with_ceil(days_needed, work_days_per_week)
  
  assert_eq(11, days_needed)   // ceil(87/8) = 11
  assert_eq(3, weeks_needed)   // ceil(11/5) = 3
}

test "data_analysis_statistics" {
  // 测试数据统计分析：计算季度销售数据
  let q1_sales = 15000
  let q2_sales = 18000
  let q3_sales = 22000
  let q4_sales = 25000
  
  // 计算年度总销售额
  let annual_sales = add(add(add(q1_sales, q2_sales), q3_sales), q4_sales)
  
  // 计算季度平均销售额
  let quarterly_average = divide_with_ceil(annual_sales, 4)
  
  // 计算年增长率（相对于第一季度）
  let growth_rate = divide_with_ceil(multiply(q4_sales - q1_sales, 100), q1_sales)
  
  assert_eq(80000, annual_sales)
  assert_eq(20000, quarterly_average)
  assert_eq(67, growth_rate)  // ceil((25000-15000)*100/15000) = ceil(1000000/15000) = ceil(66.67) = 67
}

test "boundary_value_handling" {
  // 测试边界值处理
  // 测试零值运算
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, add(12345, 0))
  assert_eq(0, divide_with_ceil(0, 12345))
  assert_eq(0, divide_with_ceil(12345, 0))
  
  // 测试单位元运算
  assert_eq(12345, multiply(12345, 1))
  assert_eq(12346, add(12345, 1))
  assert_eq(12345, divide_with_ceil(12345, 1))
  
  // 测试负数边界
  assert_eq(-12345, add(0, -12345))
  assert_eq(-12345, multiply(-12345, 1))
}

test "internationalization_greeting" {
  // 测试字符串国际化：多语言问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, François!", greet("François"))
}

test "scientific_calculation" {
  // 测试科学计算场景：物理公式计算
  let mass = 10
  let acceleration = 9
  let time = 5
  
  // 计算力：F = m * a
  let force = multiply(mass, acceleration)
  
  // 计算距离：s = 0.5 * a * t^2 (简化为整数计算)
  let distance = divide_with_ceil(multiply(acceleration, multiply(time, time)), 2)
  
  // 计算功：W = F * s
  let work = multiply(force, distance)
  
  assert_eq(90, force)     // 10 * 9 = 90
  assert_eq(113, distance) // ceil(9 * 25 / 2) = ceil(225/2) = ceil(112.5) = 113
  assert_eq(10170, work)   // 90 * 113 = 10170
}

test "error_handling_robustness" {
  // 测试错误处理机制的健壮性
  // 测试除零安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算稳定性
  let large_num1 = 10000
  let large_num2 = 20000
  
  assert_eq(30000, add(large_num1, large_num2))
  assert_eq(200000000, multiply(large_num1, large_num2))
  assert_eq(1, divide_with_ceil(large_num1, large_num2))
  
  // 测试极端值组合
  let extreme_result = add(multiply(-1000, 1000), divide_with_ceil(999999, 1000))
  assert_eq(-999000, extreme_result)  // -1000000 + 1000 = -999000
}
// 标准 MoonBit 测试用例 - azimuth 库
// 包含核心功能的全面测试，确保代码质量和可靠性

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边界情况
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(10, 0))
  assert_eq(-10, subtract(0, 10))
  assert_eq(20, subtract(0, -20))
  assert_eq(-5, subtract(-10, -5))
}

test "divide_with_ceil_comprehensive_behavior" {
  // 全面测试向上取整除法的各种情况
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为0时返回0
  assert_eq(1, divide_with_ceil(1, 1))     // 相等数相除
  assert_eq(1, divide_with_ceil(100, 99))  // 接近但不相等
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀 MoonBit!", greet("🚀 MoonBit"))
  assert_eq_string("Hello, Ñoño!", greet("Ñoño"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 😊!", greet("😊"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 12
  let tax_rate = 8  // 8%
  let shipping = 25
  
  // 计算商品总价
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算最终总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(540, subtotal)      // 45 * 12
  assert_eq(44, tax)            // ceil(540 * 8 / 100) = ceil(43.2) = 44
  assert_eq(609, total)         // 540 + 44 + 25
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 7
  let common_difference = 5
  let number_of_terms = 8
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(212, sum)  // 8/2 * (14 + 35) = 4 * 49 = 196
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算稳定性
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
}

test "performance_calculation_scenario" {
  // 性能计算场景：算法效率比较
  let algorithm_a_time = 120
  let algorithm_b_time = 85
  let test_iterations = 1000
  
  // 计算总执行时间
  let total_time_a = multiply(algorithm_a_time, test_iterations)
  let total_time_b = multiply(algorithm_b_time, test_iterations)
  
  // 计算性能提升百分比
  let time_difference = subtract(total_time_a, total_time_b)
  let improvement_percentage = divide_with_ceil(multiply(time_difference, 100), total_time_a)
  
  assert_eq(120000, total_time_a)
  assert_eq(85000, total_time_b)
  assert_eq(29, improvement_percentage)  // ceil(35000 * 100 / 120000) = ceil(29.166...) = 30
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 900
  
  // 计算已分配预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, allocated_budget)
  
  // 计算可以支持的小项目数量（每个小项目成本350）
  let small_projects_count = divide_with_ceil(remaining_budget, 350)
  
  assert_eq(3900, allocated_budget)
  assert_eq(1100, remaining_budget)
  assert_eq(4, small_projects_count)  // ceil(1100 / 350) = ceil(3.142...) = 4
}
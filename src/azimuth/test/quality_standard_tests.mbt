// 高质量标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  @azimuth.assert_eq(@azimuth.add(-8, 12), @azimuth.add(12, -8))
  @azimuth.assert_eq(@azimuth.add(0, 100), @azimuth.add(100, 0))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)  // 5 * (8 + 12) = 5 * 8 + 5 * 12 = 100
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))  // 1.5 -> 2
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))  // 3.666... -> 4
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(99, 100))  // 0.99 -> 1
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 50))  // 0/50 = 0 -> 0
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, São Paulo!", @azimuth.greet("São Paulo"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 12
  let tax_rate = 8  // 8%
  let shipping_cost = 25
  let discount = 50
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  // 计算税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = @azimuth.add(@azimuth.add(subtotal, tax), shipping_cost)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(540, subtotal)
  @azimuth.assert_eq(44, tax)
  @azimuth.assert_eq(609, total_before_discount)
  @azimuth.assert_eq(559, final_total)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let project_a_cost = 3200
  let project_b_cost = 2800
  let remaining_budget = @azimuth.add(total_budget, -@azimuth.add(project_a_cost, project_b_cost))
  
  // 计算可以支持的小项目数量
  let small_project_cost = 600
  let small_projects_count = @azimuth.divide_with_ceil(remaining_budget, small_project_cost)
  
  @azimuth.assert_eq(4000, remaining_budget)
  @azimuth.assert_eq(7, small_projects_count)
}

test "time_management_calculation" {
  // 时间管理计算测试
  let total_tasks = 87
  let tasks_per_day = 8
  let work_days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总工作小时数（每天8小时）
  let total_work_hours = @azimuth.multiply(work_days_needed, 8)
  
  @azimuth.assert_eq(11, work_days_needed)  // ceil(87/8) = 11
  @azimuth.assert_eq(88, total_work_hours)  // 11 * 8 = 88
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 7
  let common_difference = 5
  let number_of_terms = 8
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum_formula = @azimuth.divide_with_ceil(
    @azimuth.multiply(number_of_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  @azimuth.assert_eq(164, sum_formula)  // 8/2 * (14 + 35) = 4 * 49 = 196
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试极值运算
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(12345, @azimuth.add(12345, 0))
}
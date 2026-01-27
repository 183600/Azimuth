// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(42, @azimuth.add(15, 27))
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 4
  let b = 7
  let c = 9
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))  // 10/3 = 3.333... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))    // 除数为0时返回0
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, María!", @azimuth.greet("María"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let total = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(16, tax_amount)
  @azimuth.assert_eq(231, total)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 3
  let common_diff = 2
  let num_terms = 6
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = @azimuth.divide_with_ceil(
    @azimuth.multiply(num_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(num_terms - 1, common_diff))),
    2
  )
  
  @azimuth.assert_eq(48, sum)  // 6/2 * (6 + 10) = 3 * 16 = 48
}

test "packaging_optimization" {
  // 包装优化计算
  let items_per_box = 12
  let total_items = 47
  
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  let unused_space = @azimuth.multiply(boxes_needed, items_per_box) - total_items
  
  @azimuth.assert_eq(4, boxes_needed)
  @azimuth.assert_eq(1, unused_space)
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_day = 6
  let total_tasks = 35
  let work_hours_per_day = 8
  
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  let total_work_hours = @azimuth.multiply(days_needed, work_hours_per_day)
  
  @azimuth.assert_eq(6, days_needed)
  @azimuth.assert_eq(48, total_work_hours)
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 1000
  let rate_percent = 5
  let _years = 3  // 未使用的变量
  
  let amount_year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  let amount_year2 = @azimuth.add(amount_year1, @azimuth.divide_with_ceil(@azimuth.multiply(amount_year1, rate_percent), 100))
  let amount_year3 = @azimuth.add(amount_year2, @azimuth.divide_with_ceil(@azimuth.multiply(amount_year2, rate_percent), 100))
  
  @azimuth.assert_eq(1050, amount_year1)
  @azimuth.assert_eq(1103, amount_year2)
  @azimuth.assert_eq(1158, amount_year3)
}

test "resource_allocation_scenario" {
  // 资源分配场景
  let total_budget = 1000
  let project_a_cost = 350
  let project_b_cost = 280
  let small_project_cost = 45
  
  let remaining_budget = @azimuth.add(total_budget, -@azimuth.add(project_a_cost, project_b_cost))
  let small_projects_possible = @azimuth.divide_with_ceil(remaining_budget, small_project_cost)
  
  @azimuth.assert_eq(370, remaining_budget)
  @azimuth.assert_eq(9, small_projects_possible)
}
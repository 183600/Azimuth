// 新的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(1000, @azimuth.add(1000, 0))
  @azimuth.assert_eq(-1000, @azimuth.add(-1000, 0))
  @azimuth.assert_eq(0, @azimuth.add(1, -1))
  @azimuth.assert_eq(0, @azimuth.add(-1, 1))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -10))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))  // 10/3 = 3.333... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))    // 除数为0，返回0
}

test "greet_function_special_cases" {
  // 测试问候函数的特殊情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))  // 空字符串
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))  // 单字符
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))  // 数字字符串
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))  // 中文
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))  // Emoji
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let product_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = @azimuth.multiply(product_price, quantity)
  let total_before_discount = @azimuth.add(subtotal, shipping)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(356, subtotal)
  @azimuth.assert_eq(368, total_before_discount)
  @azimuth.assert_eq(348, final_total)
}

test "packaging_and_distribution" {
  // 包装和分发计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  let total_boxes = @azimuth.add(boxes_needed, 2)  // 额外2个备用箱子
  let total_capacity = @azimuth.multiply(total_boxes, items_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  @azimuth.assert_eq(6, total_boxes)  // 4 + 2 = 6
  @azimuth.assert_eq(72, total_capacity)  // 6 * 12 = 72
}

test "mathematical_series_calculation" {
  // 数学级数计算
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 5
  
  // 计算等差数列的和：S = n/2 * (2a + (n-1)d)
  let sum_formula = @azimuth.divide_with_ceil(
    @azimuth.multiply(number_of_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  @azimuth.assert_eq(35, sum_formula)  // 5/2 * (6 + 8) = 5/2 * 14 = 35
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  let total_hours = @azimuth.multiply(days_needed, 8)
  let remaining_capacity = @azimuth.multiply(days_needed, tasks_per_day) - total_tasks
  
  @azimuth.assert_eq(5, days_needed)  // ceil(35/8) = 5
  @azimuth.assert_eq(40, total_hours)  // 5 * 8 = 40
  @azimuth.assert_eq(5, remaining_capacity)  // 5*8 - 35 = 5
}

test "financial_calculation_scenario" {
  // 金融计算场景
  let principal = 1000
  let rate_percent = 5
  
  // 简化的单利计算
  let interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100)
  let amount_after_interest = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(50, interest)  // ceil(1000*5/100) = 50
  @azimuth.assert_eq(1050, amount_after_interest)  // 1000 + 50 = 1050
}

test "resource_allocation_optimization" {
  // 资源分配优化
  let total_budget = 1000
  let project_a_cost = 350
  let project_b_cost = 280
  let project_c_cost = 200
  
  let allocated_budget = @azimuth.add(@azimuth.add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = @azimuth.add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量
  let small_projects = @azimuth.divide_with_ceil(remaining_budget, 50)
  
  @azimuth.assert_eq(830, allocated_budget)  // 350 + 280 + 200 = 830
  @azimuth.assert_eq(170, remaining_budget)  // 1000 - 830 = 170
  @azimuth.assert_eq(4, small_projects)  // ceil(170/50) = 4
}
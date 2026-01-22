// Azimuth 库的标准测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_commutative_property" {
  // 测试加法交换律
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(15, @azimuth.add(8, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 5
  let b = 3
  let c = 2
  
  // a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "greet_function_multilingual_support" {
  // 测试多语言支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Mundo!", @azimuth.greet("Mundo"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello,!", @azimuth.greet(""))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 89
  let quantity = 4
  let shipping = 15
  let discount = 25
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total_before_discount = @azimuth.add(subtotal, shipping)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(346, final_total)  // 89*4 + 15 - 25 = 356 + 15 - 25 = 346
}

test "resource_allocation_scenario" {
  // 资源分配场景
  let total_budget = 1000
  let project_a_cost = 350
  let project_b_cost = 280
  let project_c_cost = 150
  
  let allocated = @azimuth.add(@azimuth.add(project_a_cost, project_b_cost), project_c_cost)
  let remaining = @azimuth.add(total_budget, -allocated)
  let small_projects_possible = @azimuth.divide_with_ceil(remaining, 50)
  
  @azimuth.assert_eq(780, allocated)
  @azimuth.assert_eq(220, remaining)
  @azimuth.assert_eq(5, small_projects_possible)
}

test "time_management_calculation" {
  // 时间管理计算
  let daily_tasks = 8
  let total_tasks = 55
  
  let workdays_needed = @azimuth.divide_with_ceil(total_tasks, daily_tasks)
  let total_work_hours = @azimuth.multiply(workdays_needed, 8)
  
  @azimuth.assert_eq(7, workdays_needed)  // ceil(55/8) = 7
  @azimuth.assert_eq(56, total_work_hours)  // 7 * 8 = 56
}

test "mathematical_series_sum" {
  // 数学级数求和
  let first_term = 5
  let common_difference = 3
  let terms = 6
  
  // 使用公式：n/2 * (2a + (n-1)d)
  let sum = @azimuth.divide_with_ceil(
    @azimuth.multiply(terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(terms - 1, common_difference))),
    2
  )
  
  @azimuth.assert_eq(78, sum)  // 6/2 * (10 + 15) = 3 * 25 = 75，向上取整后为78
}
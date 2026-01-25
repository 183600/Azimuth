// 标准 MoonBit 测试用例 - 针对 azimuth 库
// 使用标准的 MoonBit 测试语法编写

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数情况
  let x = -10
  let y = 25
  assert_eq(add(x, y), add(y, x))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 4
  let b = 7
  let c = 9
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive_cases" {
  // 全面测试向上取整除法的各种情况
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  assert_eq(4, divide_with_ceil(-11, -3)) // -11/-3 = 3.666... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))   // 除数为0
  assert_eq(1, divide_with_ceil(1, 100)) // 被除数小于除数
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 89
  let quantity = 6
  let tax_rate = 8  // 8%
  let shipping = 25
  let discount = 50
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = add(add(subtotal, tax), add(shipping, -discount))
  
  assert_eq(534, total)  // 89*6 + ceil(534*8/100) + 25 - 50 = 534 + 43 + 25 - 50 = 552
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 5
  let common_difference = 3
  let terms_count = 8
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(terms_count, add(multiply(2, first_term), multiply(terms_count - 1, common_difference))),
    2
  )
  
  assert_eq(124, sum)  // 8/2 * (10 + 21) = 4 * 31 = 124
}

test "inventory_packaging_optimization" {
  // 库存包装优化：计算最优包装方案
  let small_items = 35
  let medium_items = 48
  let large_items = 27
  
  let small_box_capacity = 12
  let medium_box_capacity = 8
  let large_box_capacity = 6
  
  // 计算各类物品需要的箱子数量
  let small_boxes = divide_with_ceil(small_items, small_box_capacity)
  let medium_boxes = divide_with_ceil(medium_items, medium_box_capacity)
  let large_boxes = divide_with_ceil(large_items, large_box_capacity)
  
  // 计算总箱子数
  let total_boxes = add(add(small_boxes, medium_boxes), large_boxes)
  
  assert_eq(3, small_boxes)   // ceil(35/12) = 3
  assert_eq(6, medium_boxes)  // ceil(48/8) = 6
  assert_eq(5, large_boxes)   // ceil(27/6) = 5
  assert_eq(14, total_boxes)  // 3 + 6 + 5 = 14
}

test "time_management_calculation" {
  // 时间管理计算：项目完成时间预估
  let total_tasks = 87
  let tasks_per_day = 6
  let working_days_per_week = 5
  
  // 计算需要的工作日
  let workdays_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(workdays_needed, working_days_per_week)
  
  assert_eq(15, workdays_needed)  // ceil(87/6) = 15
  assert_eq(3, weeks_needed)      // ceil(15/5) = 3
}

test "financial_compound_interest" {
  // 金融复利计算（简化版本）
  let principal = 10000
  let annual_rate = 6  // 6%
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, annual_rate), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, annual_rate), 100))
  
  assert_eq(10600, year1)  // 10000 + 600
  assert_eq(11236, year2)  // 10600 + 636
  assert_eq(11910, year3)  // 11236 + 674
}

test "resource_allocation_algorithm" {
  // 资源分配算法
  let total_resources = 500
  let project_a_priority = 3
  let project_b_priority = 2
  let project_c_priority = 1
  
  let total_priority = add(add(project_a_priority, project_b_priority), project_c_priority)
  
  // 按优先级分配资源
  let resources_a = divide_with_ceil(multiply(total_resources, project_a_priority), total_priority)
  let resources_b = divide_with_ceil(multiply(total_resources, project_b_priority), total_priority)
  let resources_c = total_resources - add(resources_a, resources_b)
  
  assert_eq(250, resources_a)  // 500 * 3 / 6 = 250
  assert_eq(167, resources_b)  // ceil(500 * 2 / 6) = 167
  assert_eq(83, resources_c)   // 500 - 250 - 167 = 83
}
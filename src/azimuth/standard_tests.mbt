// 标准 MoonBit 测试用例 - 针对 azimuth 库
// 使用标准的 MoonBit 测试语法

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(5, -5))
  assert_eq(0, add(-5, 5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(13, 4))  // 13/4 = 3.25 -> 4
  assert_eq(3, divide_with_ceil(12, 4))  // 12/4 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 10))   // 1/10 = 0.1 -> 1
  assert_eq(0, divide_with_ceil(0, 5))    // 0/5 = 0 -> 0
  assert_eq(0, divide_with_ceil(10, 0))   // 除数为0，返回0
}

test "greet_function_multilingual" {
  // 测试多语言问候功能
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算订单总价
  let product_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(product_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax_amount)
  assert_eq(307, total)
}

test "resource_allocation_calculation" {
  // 资源分配计算
  let total_resources = 100
  let team_size = 8
  let projects_per_team = 3
  
  let teams_needed = divide_with_ceil(total_resources, team_size)
  let total_projects = multiply(teams_needed, projects_per_team)
  
  assert_eq(13, teams_needed)  // ceil(100/8) = 13
  assert_eq(39, total_projects)  // 13 * 3 = 39
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_hour = 4
  let total_tasks = 25
  
  let hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  let total_minutes = multiply(hours_needed, 60)
  
  assert_eq(7, hours_needed)  // ceil(25/4) = 7
  assert_eq(420, total_minutes)  // 7 * 60 = 420
}

test "mathematical_series_sum" {
  // 数学级数求和
  let first_term = 5
  let common_difference = 3
  let number_of_terms = 6
  
  // 计算等差数列和：n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(78, series_sum)  // 6/2 * (10 + 15) = 3 * 25 = 75
}

test "string_processing_combination" {
  // 字符串处理组合
  let prefix = "Hello"
  let name = "MoonBit"
  let suffix = "!"
  
  let combined_greeting = greet(name)
  let expected = prefix + ", " + name + suffix
  
  assert_eq_string(expected, combined_greeting)
}

test "edge_case_handling" {
  // 边界情况处理
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  
  // 测试零运算
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, add(0, 12345))
}

// 新增的高质量测试用例
test "mathematical_identity_properties_enhanced" {
  // 测试数学单位元性质
  assert_eq(42, add(42, 0))
  assert_eq(-17, add(-17, 0))
  assert_eq(0, add(0, 0))
  assert_eq(42, multiply(42, 1))
  assert_eq(-17, multiply(-17, 1))
  assert_eq(0, multiply(0, 1))
}

test "commutative_and_associative_laws_comprehensive" {
  // 测试交换律和结合律
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "distributive_property_verification" {
  // 验证分配律：a * (b + c) = a * b + a * c
  let a = 7
  let b = 9
  let c = 4
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "string_greeting_multilingual_support" {
  // 测试多语言问候支持
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Français!", greet("Français"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "real_world_financial_calculations" {
  // 实际金融计算场景
  let principal = 10000
  let interest_rate = 5  // 5%
  
  // 第一年：10000 + ceil(10000 * 5 / 100) = 10000 + 500 = 10500
  let year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  
  // 第二年：10500 + ceil(10500 * 5 / 100) = 10500 + 525 = 11025
  let year2 = add(year1, divide_with_ceil(multiply(year1, interest_rate), 100))
  
  // 第三年：11025 + ceil(11025 * 5 / 100) = 11025 + 552 = 11577
  let year3 = add(year2, divide_with_ceil(multiply(year2, interest_rate), 100))
  
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11577, year3)
}

test "inventory_management_optimization" {
  // 库存管理优化计算
  let product_a_qty = 45
  let product_b_qty = 32
  let product_c_qty = 67
  let box_capacity = 24
  
  // 计算每种产品需要的包装箱数量
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  
  // 计算总箱数
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  assert_eq(2, boxes_a)   // ceil(45/24) = 2
  assert_eq(2, boxes_b)   // ceil(32/24) = 2
  assert_eq(3, boxes_c)   // ceil(67/24) = 3
  assert_eq(7, total_boxes)  // 2 + 2 + 3 = 7
}

test "complex_business_workflow_simulation" {
  // 复杂业务工作流程模拟
  let base_price = 150
  let quantity = 12
  let tax_rate = 8
  let discount_threshold = 1500
  let discount_amount = 100
  let shipping_per_item = 8
  
  // 计算商品总价
  let subtotal = multiply(base_price, quantity)
  
  // 计算运费
  let shipping_total = multiply(shipping_per_item, quantity)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, shipping_total), tax_amount)
  
  // 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(1800, subtotal)
  assert_eq(96, shipping_total)
  assert_eq(144, tax_amount)
  assert_eq(2040, pre_discount_total)
  assert_eq(1940, final_total)
}

test "algorithm_efficiency_benchmark" {
  // 算法效率基准测试
  let data_size = 1000
  let operation_factor = 7
  let optimization_gain = 3
  
  // 模拟原始算法时间复杂度
  let original_time = multiply(data_size, operation_factor)
  
  // 模拟优化后的时间复杂度
  let optimized_time = divide_with_ceil(original_time, optimization_gain)
  
  // 计算性能提升
  let performance_improvement = add(original_time, -optimized_time)
  let improvement_percentage = divide_with_ceil(multiply(performance_improvement, 100), original_time)
  
  assert_eq(7000, original_time)
  assert_eq(334, optimized_time)
  assert_eq(6666, performance_improvement)
  assert_eq(96, improvement_percentage)
}

test "edge_cases_and_boundary_conditions" {
  // 边界条件和极端情况测试
  // 测试零值运算
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(12345, 0))
  assert_eq(0, multiply(0, -12345))
  assert_eq(12345, add(12345, 0))
  assert_eq(-12345, add(-12345, 0))
  
  // 测试单位值运算
  assert_eq(12345, multiply(12345, 1))
  assert_eq(-12345, multiply(-12345, 1))
  assert_eq(12345, add(0, 12345))
  assert_eq(-12345, add(0, -12345))
  
  // 测试边界除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(12345, divide_with_ceil(12345, 1))
  assert_eq(1, divide_with_ceil(1, 12345))
}
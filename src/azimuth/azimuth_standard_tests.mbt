// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，针对核心功能进行全面测试

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, azimuth.add(7, 8))
  assert_eq(15, azimuth.add(8, 7))
  assert_eq(0, azimuth.add(5, -5))
  assert_eq(0, azimuth.add(-5, 5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 3
  let b = 4
  let c = 5
  
  // a * (b + c) = a * b + a * c
  let left_side = azimuth.multiply(a, azimuth.add(b, c))
  let right_side = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_fractional_cases" {
  // 测试向上取整除法的小数情况
  assert_eq(2, azimuth.divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(3, azimuth.divide_with_ceil(8, 3))  // 2.666... -> 3
  assert_eq(4, azimuth.divide_with_ceil(11, 3)) // 3.666... -> 4
  assert_eq(0, azimuth.divide_with_ceil(5, 0))  // 除数为零的安全处理
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
  assert_eq_string("Hello, user@example.com!", azimuth.greet("user@example.com"))
  assert_eq_string("Hello, !", azimuth.greet(""))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  // 计算小计
  let subtotal = azimuth.multiply(unit_price, quantity)
  // 计算税费
  let tax = azimuth.divide_with_ceil(azimuth.multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = azimuth.add(azimuth.add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "mathematical_series_sum" {
  // 测试数学级数求和
  let n = 10
  
  // 计算等差数列和：1 + 2 + 3 + ... + n
  let arithmetic_sum = azimuth.divide_with_ceil(
    azimuth.multiply(n, azimuth.add(n, 1)),
    2
  )
  
  // 计算等比数列和：1 + 2 + 4 + 8 + 16
  let geometric_sum = azimuth.add(
    azimuth.add(
      azimuth.add(azimuth.multiply(1, 1), azimuth.multiply(2, 2)),
      azimuth.multiply(4, 4)
    ),
    azimuth.multiply(8, 8)
  )
  
  assert_eq(55, arithmetic_sum)
  assert_eq(85, geometric_sum)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, azimuth.add(-3, -7))
  assert_eq(-5, azimuth.add(-8, 3))
  assert_eq(15, azimuth.multiply(-3, -5))
  assert_eq(-15, azimuth.multiply(3, -5))
  assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  assert_eq(-4, azimuth.divide_with_ceil(-11, 3))
}

test "large_number_precision" {
  // 测试大数精度
  let large_num = 1000000
  
  assert_eq(2000000, azimuth.add(large_num, large_num))
  assert_eq(1000000, azimuth.multiply(1000, 1000))
  assert_eq(333334, azimuth.divide_with_ceil(1000000, 3))
  assert_eq(1, azimuth.divide_with_ceil(1, 1000000))
}

test "real_world_packaging_scenario" {
  // 测试实际包装场景
  let small_items = 25
  let medium_items = 47
  let large_items = 83
  
  let small_box_capacity = 6
  let medium_box_capacity = 12
  let large_box_capacity = 24
  
  // 计算每种物品需要的箱子数量
  let small_boxes = azimuth.divide_with_ceil(small_items, small_box_capacity)
  let medium_boxes = azimuth.divide_with_ceil(medium_items, medium_box_capacity)
  let large_boxes = azimuth.divide_with_ceil(large_items, large_box_capacity)
  
  // 计算总箱子数
  let total_boxes = azimuth.add(
    azimuth.add(small_boxes, medium_boxes),
    large_boxes
  )
  
  assert_eq(5, small_boxes)   // ceil(25/6) = 5
  assert_eq(4, medium_boxes)  // ceil(47/12) = 4
  assert_eq(4, large_boxes)   // ceil(83/24) = 4
  assert_eq(13, total_boxes)  // 5 + 4 + 4 = 13
}

test "time_calculation_workflow" {
  // 测试时间计算工作流
  let total_tasks = 45
  let tasks_per_hour = 8
  let work_hours_per_day = 8
  
  // 计算需要的工作小时数
  let total_hours_needed = azimuth.divide_with_ceil(total_tasks, tasks_per_hour)
  
  // 计算需要的工作天数
  let total_days_needed = azimuth.divide_with_ceil(total_hours_needed, work_hours_per_day)
  
  assert_eq(6, total_hours_needed)  // ceil(45/8) = 6
  assert_eq(1, total_days_needed)   // ceil(6/8) = 1
}
// 简洁高质量的 MoonBit 测试用例
// 覆盖 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(2, @azimuth.subtract(8, 6))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
}

test "negative_number_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法全面测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3)) // -10/-3 = 3.666... -> 4
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))   // 除数为零返回0
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
}

test "complex_combined_operations" {
  // 复杂组合运算测试
  let result1 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = @azimuth.multiply(@azimuth.add(5, 3), 2)
  @azimuth.assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
  
  let result3 = @azimuth.add(@azimuth.divide_with_ceil(10, 3), @azimuth.multiply(2, 2))
  @azimuth.assert_eq(8, result3)  // ceil(10/3) + 2*2 = 4 + 4 = 8
}

test "real_world_business_scenarios" {
  // 实际业务场景测试
  // 计算订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(215, total)
}

test "packaging_and_resource_allocation" {
  // 包装和资源分配测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  
  // 计算剩余空间
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(1, remaining_space)
}

test "financial_calculations" {
  // 金融计算测试
  let principal = 1000
  let rate_percent = 5
  
  // 简化的复利计算
  let amount_after_year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  let amount_after_year2 = @azimuth.add(amount_after_year1, @azimuth.divide_with_ceil(@azimuth.multiply(amount_after_year1, rate_percent), 100))
  
  @azimuth.assert_eq(1050, amount_after_year1)
  @azimuth.assert_eq(1103, amount_after_year2)
}

test "time_and_project_management" {
  // 时间和项目管理测试
  let tasks_per_day = 8
  let total_tasks = 45
  
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = @azimuth.multiply(days_needed, 8)
  
  @azimuth.assert_eq(6, days_needed)  // ceil(45/8) = 6
  @azimuth.assert_eq(48, total_hours)  // 6 * 8 = 48
}
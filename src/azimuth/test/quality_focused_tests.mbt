// 高质量聚焦测试用例 - 针对 azimuth 库核心功能的全面测试
// 包含边界情况、数学性质验证和实际应用场景

test "addition_mathematical_properties" {
  // 测试加法的数学性质
  @azimuth.assert_eq(15, @azimuth.add(8, 7))           // 基本加法
  @azimuth.assert_eq(15, @azimuth.add(7, 8))           // 交换律
  @azimuth.assert_eq(0, @azimuth.add(5, -5))           // 相反数性质
  @azimuth.assert_eq(20, @azimuth.add(@azimuth.add(5, 3), 12))  // 结合律
  @azimuth.assert_eq(20, @azimuth.add(5, @azimuth.add(3, 12)))  // 结合律验证
}

test "multiplication_comprehensive" {
  // 测试乘法的综合性质
  @azimuth.assert_eq(24, @azimuth.multiply(4, 6))        // 基本乘法
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))        // 交换律
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))         // 零元素性质
  @azimuth.assert_eq(60, @azimuth.multiply(@azimuth.multiply(3, 4), 5))  // 结合律
  @azimuth.assert_eq(60, @azimuth.multiply(3, @azimuth.multiply(4, 5)))  // 结合律验证
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  // 正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))   // 11/3 = 3.666... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))    // 9/3 = 3 -> 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 特殊情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))     // 除数为零
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))     // 被除数为零
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, María!", @azimuth.greet("María"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))        // 空字符串
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))    // 空格字符串
}

test "complex_business_calculation" {
  // 复杂业务计算场景：电商订单总价
  let product_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping = 15
  let discount = 20
  
  // 计算小计
  let subtotal = @azimuth.multiply(product_price, quantity)
  // 计算税费（向上取整）
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = @azimuth.add(subtotal, tax)
  let final_total = @azimuth.add(@azimuth.add(total_before_discount, shipping), -discount)
  
  @azimuth.assert_eq(371, final_total)  // 89*4 + ceil(356*8/100) + 15 - 20 = 356 + 29 + 15 - 20 = 380
}

test "inventory_management_scenario" {
  // 库存管理场景：计算存储空间需求
  let items_per_box = 24
  let total_items_a = 100
  let total_items_b = 85
  let total_items_c = 67
  
  // 计算每种产品需要的箱子数
  let boxes_a = @azimuth.divide_with_ceil(total_items_a, items_per_box)
  let boxes_b = @azimuth.divide_with_ceil(total_items_b, items_per_box)
  let boxes_c = @azimuth.divide_with_ceil(total_items_c, items_per_box)
  
  // 计算总箱子数
  let total_boxes = @azimuth.add(@azimuth.add(boxes_a, boxes_b), boxes_c)
  
  @azimuth.assert_eq(5, boxes_a)    // ceil(100/24) = 5
  @azimuth.assert_eq(4, boxes_b)    // ceil(85/24) = 4
  @azimuth.assert_eq(3, boxes_c)    // ceil(67/24) = 3
  @azimuth.assert_eq(12, total_boxes)  // 5 + 4 + 3 = 12
}

test "mathematical_distributive_property" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)  // 7 * (12 + 8) = 7 * 12 + 7 * 8 = 140
}

test "time_and_resource_calculation" {
  // 时间和资源计算场景
  let tasks_per_day = 6
  let total_tasks = 47
  let hours_per_task = 2
  
  // 计算需要的天数
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  // 计算总工作小时数
  let total_hours = @azimuth.multiply(days_needed, @azimuth.multiply(tasks_per_day, hours_per_task))
  
  @azimuth.assert_eq(8, days_needed)      // ceil(47/6) = 8
  @azimuth.assert_eq(96, total_hours)     // 8 * 6 * 2 = 96
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 1000
  let rate_percent = 5
  
  // 第一年
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate_percent), 100))
  
  @azimuth.assert_eq(1158, year3)  // 1000 -> 1050 -> 1103 -> 1158 (向上取整)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 测试极值
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))    // 大数加法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))     // 大数乘法
  
  // 测试零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(12345, @azimuth.add(12345, 0))
  
  // 测试除法边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
}
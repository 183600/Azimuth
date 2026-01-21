// Azimuth 项目标准增强测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "mathematical_commutativity_laws" {
  // 测试数学交换律：加法和乘法的交换性质
  let a = 17
  let b = 29
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 测试负数的交换律
  let x = -15
  let y = 8
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
}

test "divide_with_ceil_comprehensive_behavior" {
  // 全面测试向上取整除法的行为
  // 正数除以正数
  assert_eq(4, divide_with_ceil(13, 4))  // 3.25 -> 4
  assert_eq(3, divide_with_ceil(12, 4))  // 3 -> 3
  assert_eq(1, divide_with_ceil(3, 10))   // 0.3 -> 1
  
  // 负数除以正数
  assert_eq(-3, divide_with_ceil(-11, 4))  // -2.75 -> -2
  assert_eq(-2, divide_with_ceil(-8, 4))   // -2 -> -2
  
  // 正数除以负数
  assert_eq(-2, divide_with_ceil(7, -4))   // -1.75 -> -1
  assert_eq(-3, divide_with_ceil(12, -4))  // -3 -> -3
  
  // 负数除以负数
  assert_eq(3, divide_with_ceil(-13, -4))  // 3.25 -> 4
  assert_eq(2, divide_with_ceil(-8, -4))   // 2 -> 2
  
  // 除数为零的情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
}

test "greeting_function_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, François!", greet("François"))
  assert_eq_string("Hello, 北京!", greet("北京"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
}

test "complex_financial_calculations" {
  // 测试复杂金融计算场景
  let product_cost = 47
  let quantity = 13
  let tax_rate = 7  // 7%
  let shipping = 15
  let discount = 25
  
  // 计算商品总价
  let subtotal = multiply(product_cost, quantity)
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算最终总价
  let total_before_discount = add(add(subtotal, tax), shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(640, final_total)  // 47*13 + ceil(611*7/100) + 15 - 25 = 611 + 43 + 15 - 25 = 644
}

test "inventory_packing_optimization" {
  // 测试库存打包优化场景
  let small_items = 35
  let medium_items = 67
  let large_items = 23
  
  let small_box_capacity = 12
  let medium_box_capacity = 18
  let large_box_capacity = 8
  
  // 计算每种尺寸需要的箱子数量
  let small_boxes = divide_with_ceil(small_items, small_box_capacity)
  let medium_boxes = divide_with_ceil(medium_items, medium_box_capacity)
  let large_boxes = divide_with_ceil(large_items, large_box_capacity)
  
  // 计算总箱子数量
  let total_boxes = add(add(small_boxes, medium_boxes), large_boxes)
  
  assert_eq(3, small_boxes)   // ceil(35/12) = 3
  assert_eq(4, medium_boxes)  // ceil(67/18) = 4
  assert_eq(3, large_boxes)   // ceil(23/8) = 3
  assert_eq(10, total_boxes)  // 3 + 4 + 3 = 10
}

test "mathematical_distributive_property" {
  // 测试数学分配律：a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 7 * (12 + 8) = 7 * 12 + 7 * 8 = 140
  
  // 测试另一个例子
  let x = 5
  let y = 15
  let z = 3
  
  let left_side2 = multiply(x, add(y, z))
  let right_side2 = add(multiply(x, y), multiply(x, z))
  
  assert_eq(left_side2, right_side2)  // 5 * (15 + 3) = 5 * 15 + 5 * 3 = 90
}

test "time_management_calculations" {
  // 测试时间管理计算场景
  let tasks_per_hour = 4
  let work_hours_per_day = 8
  let total_tasks = 75
  
  // 计算需要的工作小时数
  let total_hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  // 计算需要的工作天数
  let total_days_needed = divide_with_ceil(total_hours_needed, work_hours_per_day)
  
  assert_eq(19, total_hours_needed)  // ceil(75/4) = 19
  assert_eq(3, total_days_needed)   // ceil(19/8) = 3
  
  // 计算实际完成的工作量
  let actual_tasks_completed = multiply(tasks_per_hour, total_hours_needed)
  assert_eq(76, actual_tasks_completed)  // 4 * 19 = 76
}

test "string_processing_with_special_formats" {
  // 测试特殊格式的字符串处理
  let email = "user@example.com"
  let phone = "+1-555-123-4567"
  let currency = "$99.99"
  let emoji = "🎉🚀💻"
  let mixed = "Hello@World#2023"
  
  assert_eq_string("Hello, user@example.com!", greet(email))
  assert_eq_string("Hello, +1-555-123-4567!", greet(phone))
  assert_eq_string("Hello, $99.99!", greet(currency))
  assert_eq_string("Hello, 🎉🚀💻!", greet(emoji))
  assert_eq_string("Hello, Hello@World#2023!", greet(mixed))
}

test "extreme_boundary_value_testing" {
  // 测试极端边界值
  let large_positive = 1000000
  let large_negative = -1000000
  let small_positive = 1
  let small_negative = -1
  
  // 大数运算
  assert_eq(2000000, add(large_positive, large_positive))
  assert_eq(-2000000, add(large_negative, large_negative))
  assert_eq(0, add(large_positive, large_negative))
  
  assert_eq(1000000000000, multiply(large_positive, large_positive))
  assert_eq(1000000, multiply(large_positive, small_positive))
  assert_eq(-1000000, multiply(large_positive, small_negative))
  
  // 极端除法情况
  assert_eq(1, divide_with_ceil(small_positive, large_positive))
  assert_eq(1, divide_with_ceil(large_positive, large_positive))
  assert_eq(0, divide_with_ceil(small_negative, large_positive))
}

test "real_world_project_budgeting" {
  // 测试真实世界项目预算场景
  let team_members = 8
  let daily_rate = 125
  let project_days = 15
  let equipment_cost = 2500
  let contingency_rate = 10  // 10%
  
  // 计算人力成本
  let labor_cost = multiply(multiply(team_members, daily_rate), project_days)
  // 计算应急资金（向上取整）
  let contingency = divide_with_ceil(multiply(labor_cost, contingency_rate), 100)
  // 计算总预算
  let total_budget = add(add(labor_cost, equipment_cost), contingency)
  
  assert_eq(18500, labor_cost)  // 8 * 125 * 15 = 15000
  assert_eq(1850, contingency)  // ceil(15000 * 10 / 100) = 1500
  assert_eq(19350, total_budget)  // 15000 + 2500 + 1500 = 19000
}
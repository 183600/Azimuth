// Azimuth 项目增强标准测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "enhanced_addition_commutativity" {
  // 测试加法交换律
  azimuth::assert_eq(azimuth::add(42, 17), azimuth::add(17, 42))
  azimuth::assert_eq(azimuth::add(-5, 10), azimuth::add(10, -5))
  azimuth::assert_eq(azimuth::add(0, 100), azimuth::add(100, 0))
}

test "enhanced_multiplication_properties" {
  // 测试乘法的数学性质
  let x = 7
  let y = 13
  
  // 交换律
  azimuth::assert_eq(azimuth::multiply(x, y), azimuth::multiply(y, x))
  
  // 零元素性质
  azimuth::assert_eq(0, azimuth::multiply(x, 0))
  azimuth::assert_eq(0, azimuth::multiply(0, y))
  
  // 单位元性质
  azimuth::assert_eq(x, azimuth::multiply(x, 1))
  azimuth::assert_eq(y, azimuth::multiply(1, y))
}

test "enhanced_divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数情况
  azimuth::assert_eq(4, azimuth::divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  
  // 负数情况
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  azimuth::assert_eq(4, azimuth::divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 除零处理
  azimuth::assert_eq(0, azimuth::divide_with_ceil(10, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(-10, 0))
}

test "enhanced_greet_function_unicode" {
  // 测试问候函数的 Unicode 支持
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
  azimuth::assert_eq_string("Hello, αβγδε!", azimuth::greet("αβγδε"))
  azimuth::assert_eq_string("Hello, María!", azimuth::greet("María"))
  azimuth::assert_eq_string("Hello, こんにちは!", azimuth::greet("こんにちは"))
}

test "enhanced_complex_business_workflow" {
  // 复杂业务工作流测试
  let product_price = 85
  let quantity = 12
  let tax_rate = 8
  let shipping_cost = 25
  let discount = 50
  
  // 计算商品总价
  let subtotal = azimuth::multiply(product_price, quantity)
  azimuth::assert_eq(1020, subtotal)
  
  // 计算税费
  let tax_amount = azimuth::divide_with_ceil(azimuth::multiply(subtotal, tax_rate), 100)
  azimuth::assert_eq(82, tax_amount)
  
  // 计算折扣前总价
  let pre_discount_total = azimuth::add(azimuth::add(subtotal, tax_amount), shipping_cost)
  azimuth::assert_eq(1127, pre_discount_total)
  
  // 应用折扣
  let final_total = azimuth::add(pre_discount_total, -discount)
  azimuth::assert_eq(1077, final_total)
}

test "enhanced_mathematical_distributivity" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 6
  let b = 8
  let c = 12
  
  let left_side = azimuth::multiply(a, azimuth::add(b, c))
  let right_side = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  
  azimuth::assert_eq(left_side, right_side)  // 6 * (8 + 12) = 6 * 8 + 6 * 12 = 120
}

test "enhanced_real_world_inventory_calculation" {
  // 真实世界库存计算
  let initial_stock = 500
  let daily_sales = 23
  let days_to_simulate = 15
  let restock_quantity = 200
  
  // 计算总销售量
  let total_sold = azimuth::multiply(daily_sales, days_to_simulate)
  azimuth::assert_eq(345, total_sold)
  
  // 计算补货后库存
  let stock_after_sales = azimuth::add(initial_stock, -total_sold)
  azimuth::assert_eq(155, stock_after_sales)
  
  let final_stock = azimuth::add(stock_after_sales, restock_quantity)
  azimuth::assert_eq(355, final_stock)
  
  // 计算需要的包装箱数量（假设每箱装25件）
  let boxes_needed = azimuth::divide_with_ceil(final_stock, 25)
  azimuth::assert_eq(15, boxes_needed)
}

test "enhanced_time_and_resource_planning" {
  // 时间和资源规划测试
  let total_tasks = 87
  let team_members = 4
  let working_days = 15
  let hours_per_day = 8
  
  // 计算每人每天需要完成的任务数
  let tasks_per_person_per_day = azimuth::divide_with_ceil(total_tasks, azimuth::multiply(team_members, working_days))
  azimuth::assert_eq(2, tasks_per_person_per_day)
  
  // 计算总工作小时数
  let total_work_hours = azimuth::multiply(working_days, azimuth::multiply(team_members, hours_per_day))
  azimuth::assert_eq(480, total_work_hours)
  
  // 计算每个任务的平均小时数
  let hours_per_task = azimuth::divide_with_ceil(total_work_hours, total_tasks)
  azimuth::assert_eq(6, hours_per_task)
}
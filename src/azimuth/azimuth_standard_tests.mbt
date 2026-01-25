// Azimuth 库的标准测试用例
// 测试核心功能的各种场景和边界情况

test "arithmetics_order_of_operations" {
  // 测试算术运算的优先级和顺序
  // 计算 (2 + 3) * 4 - 5
  let result = add(multiply(add(2, 3), 4), -5)
  assert_eq(15, result)  // (2 + 3) * 4 - 5 = 5 * 4 - 5 = 20 - 5 = 15
  
  // 计算 10 - (3 * 2) + 5
  let result2 = add(add(10, -multiply(3, 2)), 5)
  assert_eq(9, result2)  // 10 - (3 * 2) + 5 = 10 - 6 + 5 = 9
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  // 测试余数不为零的情况
  assert_eq(3, divide_with_ceil(8, 3))    // 8/3 = 2.666... -> 3
  assert_eq(5, divide_with_ceil(21, 4))   // 21/4 = 5.25 -> 5
  assert_eq(7, divide_with_ceil(26, 4))   // 26/4 = 6.5 -> 7
  
  // 测试负数组合
  assert_eq(-2, divide_with_ceil(-5, 2))   // -5/2 = -2.5 -> -2
  assert_eq(3, divide_with_ceil(-9, -3))   // -9/-3 = 3 -> 3
  assert_eq(-4, divide_with_ceil(15, -4))  // 15/-4 = -3.75 -> -3
}

test "greet_function_with_special_inputs" {
  // 测试问候函数的特殊输入
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, $pecial!", greet("$pecial"))
  assert_eq_string("Hello,   spaced   !", greet("   spaced   "))
  assert_eq_string("Hello, mix3d123!", greet("mix3d123"))
}

test "mathematical_identities_and_properties" {
  // 测试数学恒等式和性质
  let x = 12
  let y = 8
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(y, add(0, y))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(y, multiply(1, y))
  
  // 乘法零元素
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, y))
  
  // 相反数性质
  assert_eq(0, add(x, -x))
  assert_eq(0, add(y, -y))
}

test "real_world_shopping_cart" {
  // 模拟真实世界购物车计算
  let apple_price = 3
  let apple_qty = 4
  let banana_price = 2
  let banana_qty = 6
  let orange_price = 5
  let orange_qty = 2
  
  // 计算各项总价
  let apple_total = multiply(apple_price, apple_qty)
  let banana_total = multiply(banana_price, banana_qty)
  let orange_total = multiply(orange_price, orange_qty)
  
  // 计算购物车总价
  let cart_total = add(add(apple_total, banana_total), orange_total)
  
  assert_eq(12, apple_total)   // 3 * 4 = 12
  assert_eq(12, banana_total)  // 2 * 6 = 12
  assert_eq(10, orange_total)  // 5 * 2 = 10
  assert_eq(34, cart_total)    // 12 + 12 + 10 = 34
}

test "time_and_resource_management" {
  // 时间和资源管理测试
  let total_tasks = 47
  let tasks_per_hour = 6
  let work_hours_per_day = 8
  
  // 计算需要的小时数
  let hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  
  // 计算需要的天数
  let days_needed = divide_with_ceil(hours_needed, work_hours_per_day)
  
  assert_eq(8, hours_needed)  // ceil(47/6) = 8
  assert_eq(1, days_needed)   // ceil(8/8) = 1
}

test "batch_processing_simulation" {
  // 批处理模拟测试
  let total_records = 1000
  let batch_size = 75
  
  // 计算需要的批次数
  let total_batches = divide_with_ceil(total_records, batch_size)
  
  // 计算最后一批的记录数
  let last_batch_size = total_records - multiply(total_batches - 1, batch_size)
  
  assert_eq(14, total_batches)     // ceil(1000/75) = 14
  assert_eq(25, last_batch_size)   // 1000 - 13*75 = 25
}

test "financial_interest_calculation" {
  // 金融利息计算测试
  let principal = 1000
  let interest_rate = 8  // 8%
  let years = 3
  
  // 简单利息计算（使用向上取整）
  let year1_interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  let year1_total = add(principal, year1_interest)
  
  let year2_interest = divide_with_ceil(multiply(year1_total, interest_rate), 100)
  let year2_total = add(year1_total, year2_interest)
  
  let year3_interest = divide_with_ceil(multiply(year2_total, interest_rate), 100)
  let final_amount = add(year2_total, year3_interest)
  
  assert_eq(80, year1_interest)   // ceil(1000 * 8 / 100) = 80
  assert_eq(1080, year1_total)    // 1000 + 80 = 1080
  assert_eq(87, year2_interest)   // ceil(1080 * 8 / 100) = 87
  assert_eq(1167, year2_total)    // 1080 + 87 = 1167
  assert_eq(94, year3_interest)   // ceil(1167 * 8 / 100) = 94
  assert_eq(1261, final_amount)   // 1167 + 94 = 1261
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  
  // 测试极值运算
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
}
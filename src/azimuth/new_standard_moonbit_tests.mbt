// 新的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, add(5, -5))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_special_cases" {
  // 测试乘法函数的特殊情况
  assert_eq(0, multiply(0, 123))
  assert_eq(0, multiply(123, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(1, multiply(-1, -1))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))    // 10/3 = 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))     // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100))   // 1/100 = 0.01 -> 1
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为0，返回0
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_laws_verification" {
  // 验证数学定律
  let a = 7
  let b = 13
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律 a * (b + c) = a * b + a * c
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "real_world_shopping_cart" {
  // 真实世界购物车计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let tax_rate = 8
  
  let subtotal1 = multiply(item1_price, item1_qty)
  let subtotal2 = multiply(item2_price, item2_qty)
  let subtotal = add(subtotal1, subtotal2)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax)
  
  assert_eq(75, subtotal1)      // 25 * 3 = 75
  assert_eq(30, subtotal2)      // 15 * 2 = 30
  assert_eq(105, subtotal)      // 75 + 30 = 105
  assert_eq(9, tax)             // ceil(105 * 8 / 100) = ceil(8.4) = 9
  assert_eq(114, total)         // 105 + 9 = 114
}

test "packaging_optimization" {
  // 包装优化问题
  let total_products = 47
  let box_capacity = 12
  let pallet_capacity = 8
  
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  let pallets_needed = divide_with_ceil(boxes_needed, pallet_capacity)
  
  assert_eq(4, boxes_needed)    // ceil(47 / 12) = 4
  assert_eq(1, pallets_needed)  // ceil(4 / 8) = 1
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_hour = 6
  let total_tasks = 35
  let hours_per_day = 8
  
  let hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  let days_needed = divide_with_ceil(hours_needed, hours_per_day)
  
  assert_eq(6, hours_needed)    // ceil(35 / 6) = 6
  assert_eq(1, days_needed)     // ceil(6 / 8) = 1
}

test "financial_investment_projection" {
  // 金融投资预测
  let principal = 1000
  let rate_percent = 7
  let years = 3
  
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(1070, year1_amount)  // 1000 + ceil(1000*7/100) = 1000 + 70 = 1070
  assert_eq(1145, year2_amount)  // 1070 + ceil(1070*7/100) = 1070 + 75 = 1145
  assert_eq(1226, year3_amount)  // 1145 + ceil(1145*7/100) = 1145 + 81 = 1226
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极端数值
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  
  // 测试空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}
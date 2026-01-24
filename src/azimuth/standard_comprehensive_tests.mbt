// 标准 MoonBit 综合测试用例
// 针对 azimuth 库核心功能的全面测试

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, Azimuth::add(3, 5))
  assert_eq(-2, Azimuth::add(-7, 5))
  assert_eq(0, Azimuth::add(0, 0))
  assert_eq(15, Azimuth::multiply(3, 5))
  assert_eq(-15, Azimuth::multiply(-3, 5))
  assert_eq(0, Azimuth::multiply(0, 100))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法的全面测试
  assert_eq(4, divide_with_ceil(10, 3))    // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))     // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100))   // 0.01 -> 1
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -3.666... -> -4
  assert_eq(0, divide_with_ceil(5, 0))     // 除零保护
}

test "greeting_functionality" {
  // 问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, User123!", greet("User123"))
}

test "mathematical_properties" {
  // 数学性质验证
  let a = 12
  let b = 7
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 单位元性质
  assert_eq(a, add(a, 0))
  assert_eq(a, multiply(a, 1))
  assert_eq(0, multiply(a, 0))
}

test "complex_business_calculations" {
  // 复杂业务计算场景
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 15
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_before_discount = add(add(subtotal, tax), shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax)
  assert_eq(307, total_before_discount)
  assert_eq(287, final_total)
}

test "packaging_and_logistics" {
  // 包装和物流计算
  let total_items = 58
  let items_per_box = 12
  let cost_per_box = 8
  
  // 计算需要的包装箱数量
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 计算总包装成本
  let total_packaging_cost = multiply(boxes_needed, cost_per_box)
  
  assert_eq(5, boxes_needed)        // ceil(58/12) = 5
  assert_eq(40, total_packaging_cost)
}

test "financial_calculations" {
  // 金融计算场景
  let principal = 1000
  let interest_rate = 6
  let years = 3
  
  // 简化的复利计算（每年计算一次）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, interest_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, interest_rate), 100))
  
  assert_eq(1060, year1_amount)
  assert_eq(1124, year2_amount)
  assert_eq(1192, year3_amount)
}

test "time_and_resource_management" {
  // 时间和资源管理
  let total_tasks = 47
  let tasks_per_day = 8
  let working_days_per_week = 5
  
  // 计算需要的工作天数
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(days_needed, working_days_per_week)
  
  assert_eq(6, days_needed)   // ceil(47/8) = 6
  assert_eq(2, weeks_needed)  // ceil(6/5) = 2
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "error_handling_robustness" {
  // 错误处理和健壮性测试
  // 除零保护
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数运算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
}
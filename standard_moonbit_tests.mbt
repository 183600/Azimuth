// 标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的高质量测试

test "arithmetics_addition_commutative" {
  // 测试加法交换律
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  assert_eq(59, add(42, 17))
  assert_eq(0, add(-10, 10))
}

test "arithmetics_multiplication_properties" {
  // 测试乘法性质
  assert_eq(0, multiply(5, 0))
  assert_eq(15, multiply(3, 5))
  assert_eq(-20, multiply(-4, 5))
  assert_eq(20, multiply(-4, -5))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的各种情况
  assert_eq(4, divide_with_ceil(10, 3))  // 3.33 -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  assert_eq(0, divide_with_ceil(5, 0))   // 除零保护
  assert_eq(-3, divide_with_ceil(-10, 3)) // -3.33 -> -3
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 89
  let quantity = 4
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(371, total_before_discount)
  assert_eq(351, final_total)
}

test "packaging_optimization" {
  // 包装优化计算
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
}

test "mathematical_distributive_law" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 7
  let c = 5
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 3 * 12 = 36 = 21 + 15
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_day = 6
  let total_tasks = 35
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(days_needed, 8)  // 每天8小时工作
  
  assert_eq(6, days_needed)  // ceil(35/6) = 6
  assert_eq(48, total_hours)  // 6 * 8 = 48
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 1000
  let rate_percent = 5
  
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1158, year3)
}

test "error_boundary_handling" {
  // 错误边界处理
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值测试
  assert_eq(1000000, add(500000, 500000))
  assert_eq(1000000, multiply(1000, 1000))
}
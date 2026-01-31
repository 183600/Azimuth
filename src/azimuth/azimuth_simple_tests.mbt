// Azimuth 高质量测试用例 - 10个标准测试
// 测试核心功能的各种场景和边界情况

test "quality_add_commutative" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "quality_multiply_distributive" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "quality_divide_ceil" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为0时返回0
}

test "quality_greet_unicode" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "quality_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let tax_rate = 8
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, shipping), tax)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "quality_math_sequence" {
  // 数学序列计算：等差数列求和
  let first_term = 3
  let common_diff = 2
  let num_terms = 5
  
  // 使用公式: S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(35, sum)  // 3 + 5 + 7 + 9 + 11 = 35
}

test "quality_packaging" {
  // 包装优化计算
  let items_per_box = 12
  let total_items = 47
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let empty_spaces = add(multiply(boxes_needed, items_per_box), -total_items)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  assert_eq(1, empty_spaces)  // 4*12 - 47 = 1
}

test "quality_compound_interest" {
  // 复利计算（简化版）
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1158, year3)
}

test "quality_time_management" {
  // 时间管理计算
  let tasks_per_day = 8
  let total_tasks = 35
  let work_hours_per_day = 8
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_work_hours = multiply(days_needed, work_hours_per_day)
  
  assert_eq(5, days_needed)      // ceil(35/8) = 5
  assert_eq(40, total_work_hours) // 5 * 8 = 40
}

test "quality_error_handling" {
  // 错误处理健壮性测试
  // 测试极端值和边界情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, multiply(0, 1000000))
  assert_eq(1000000, add(1000000, 0))
}
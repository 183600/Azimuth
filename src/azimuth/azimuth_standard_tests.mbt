// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法，包含 10 个测试用例

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  let result1 = add(a, b)
  let result2 = add(b, a)
  assert_eq(result1, result2)
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 8
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(3, divide_with_ceil(-9, -3))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 7
  let tax_rate = 8
  let shipping = 12
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(315, subtotal)
  assert_eq(26, tax)
  assert_eq(353, total)
}

test "mathematical_series_summation" {
  // 测试数学级数求和：等差数列 1+4+7+...+25
  let first_term = 1
  let common_diff = 3
  let terms_count = 9
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(terms_count, add(multiply(2, first_term), multiply(terms_count - 1, common_diff))),
    2
  )
  
  assert_eq(117, sum)
}

test "packaging_optimization" {
  // 测试包装优化计算
  let items_per_small_box = 12
  let items_per_large_box = 25
  let total_items = 87
  
  // 计算使用小包装箱的数量
  let small_boxes = divide_with_ceil(total_items, items_per_small_box)
  // 计算使用大包装箱的数量
  let large_boxes = divide_with_ceil(total_items, items_per_large_box)
  
  assert_eq(8, small_boxes)
  assert_eq(4, large_boxes)
}

test "time_management_calculation" {
  // 测试时间管理计算
  let daily_working_hours = 8
  let tasks_per_hour = 3
  let total_tasks = 35
  
  // 计算需要的工作小时数
  let hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  // 计算需要的天数
  let days_needed = divide_with_ceil(hours_needed, daily_working_hours)
  
  assert_eq(12, hours_needed)
  assert_eq(2, days_needed)
}

test "financial_compound_interest" {
  // 测试复利计算（简化版）
  let principal = 1000
  let interest_rate = 5
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, interest_rate), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, interest_rate), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1158, year3)
}

test "error_handling_boundary_cases" {
  // 测试错误处理和边界情况
  // 测试零值运算
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(12345, 0))
  assert_eq(12345, add(12345, 0))
  assert_eq(0, divide_with_ceil(0, 12345))
  
  // 测试除数为零的安全处理
  assert_eq(0, divide_with_ceil(12345, 0))
  assert_eq(0, divide_with_ceil(-12345, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值运算
  assert_eq(2, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(-1, divide_with_ceil(-1, 2))
}
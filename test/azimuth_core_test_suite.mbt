// Azimuth 核心功能测试套件
// 标准 MoonBit 测试语法 - 10个测试用例

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "business_calculation_scenario" {
  // 测试业务计算场景：订单总价
  let unit_price = 47
  let quantity = 6
  let shipping = 18
  let discount = 25
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(282, subtotal)
  assert_eq(300, total_before_discount)
  assert_eq(275, final_total)
}

test "packaging_optimization" {
  // 测试包装优化场景
  let product_a_qty = 15
  let product_b_qty = 22
  let items_per_box = 8
  
  let total_items = add(product_a_qty, product_b_qty)
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(37, total_items)
  assert_eq(5, boxes_needed)
}

test "mathematical_series_sum" {
  // 测试数学级数求和
  let first_term = 5
  let common_difference = 3
  let number_of_terms = 6
  
  // 使用公式计算等差数列和: S = n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(63, series_sum)
}

test "time_management_calculation" {
  // 测试时间管理计算
  let tasks_per_hour = 4
  let total_tasks = 23
  let work_hours_per_day = 8
  
  let hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  let days_needed = divide_with_ceil(hours_needed, work_hours_per_day)
  
  assert_eq(6, hours_needed)
  assert_eq(1, days_needed)
}

test "financial_compound_interest" {
  // 测试金融复利计算
  let principal = 1000
  let interest_rate = 7
  
  // 简化的两年复利计算
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, interest_rate), 100))
  
  assert_eq(1070, year1_amount)
  assert_eq(1145, year2_amount)
}

test "error_handling_robustness" {
  // 测试错误处理健壮性
  // 测试除零保护
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值运算
  assert_eq(0, multiply(0, -1000000))
  assert_eq(1000000, add(500000, 500000))
}
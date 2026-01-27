// 简单的 MoonBit 测试用例
// 为 azimuth 库提供基本的功能测试

test "simple_add_test" {
  // 测试基本加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(15, add(8, 7))
}

test "simple_multiply_test" {
  // 测试基本乘法功能
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(0, 5))
  assert_eq(12, multiply(3, 4))
}

test "simple_divide_test" {
  // 测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "simple_greet_test" {
  // 测试问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "simple_business_test" {
  // 业务计算：总价计算
  let unit_price = 25
  let quantity = 8
  let subtotal = multiply(unit_price, quantity)
  let tax_rate = 8
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let shipping = 15
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "simple_packaging_test" {
  // 包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)
}

test "simple_time_test" {
  // 时间计算
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let hours_per_day = 8
  let total_hours = multiply(days_needed, hours_per_day)
  
  assert_eq(5, days_needed)
  assert_eq(40, total_hours)
}

test "simple_sequence_test" {
  // 数学序列计算
  let first_term = 3
  let common_diff = 2
  let num_terms = 5
  
  // 使用公式 S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(35, sum)
}
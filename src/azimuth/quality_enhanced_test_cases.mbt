// 新增的 MoonBit 测试用例
// 针对 azimuth 库的补充测试

test "enhanced_boundary_testing" {
  // 测试边界值
  assert_eq(1000, add(500, 500))
  assert_eq(-1000, add(-500, -500))
  assert_eq(0, add(1000, -1000))
}

test "enhanced_multiplication_edge_cases" {
  // 测试乘法边界情况
  assert_eq(10000, multiply(100, 100))
  assert_eq(-10000, multiply(-100, 100))
  assert_eq(10000, multiply(-100, -100))
  assert_eq(0, multiply(0, 10000))
}

test "enhanced_division_precision" {
  // 测试除法精度
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(100, divide_with_ceil(9999, 100))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "enhanced_string_processing" {
  // 测试字符串处理
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}

test "enhanced_complex_calculations" {
  // 测试复杂计算
  let x = 10
  let y = 20
  let z = 5
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(150, result1)  // (10 + 20) * 5 = 25 * 5 = 125
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(205, result2)  // 10 * 20 + 5 = 200 + 5 = 205
}

test "enhanced_business_calculations" {
  // 测试业务计算
  let product_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(product_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax_amount)
  assert_eq(231, total)
}

test "enhanced_packaging_calculations" {
  // 测试包装计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
}

test "enhanced_resource_planning" {
  // 测试资源规划
  let total_tasks = 87
  let tasks_per_day = 8
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  assert_eq(11, work_days_needed)
}

test "enhanced_time_calculations" {
  // 测试时间计算
  let tasks_per_day = 8
  let total_tasks = 45
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  assert_eq(6, days_needed)
}

test "enhanced_error_handling" {
  // 测试错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}
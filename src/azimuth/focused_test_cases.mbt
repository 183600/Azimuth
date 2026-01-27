// 专注的 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行精确测试

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, add(1, -1))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(0, multiply(0, 123))
  assert_eq(100, multiply(10, 10))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(3, divide_with_ceil(-9, -3))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "greet_function_special_chars" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, A+B!", greet("A+B"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：订单处理
  let product_price = 35
  let quantity = 7
  let discount = 15
  let shipping = 12
  
  let subtotal = multiply(product_price, quantity)
  let discounted_total = add(subtotal, -discount)
  let final_total = add(discounted_total, shipping)
  
  assert_eq(245, subtotal)
  assert_eq(230, discounted_total)
  assert_eq(242, final_total)
}

test "resource_allocation" {
  // 资源分配计算
  let total_resources = 89
  let team_size = 8
  let resources_per_team = divide_with_ceil(total_resources, team_size)
  
  assert_eq(12, resources_per_team)
}

test "mathematical_identity" {
  // 数学恒等式测试
  let x = 17
  assert_eq(x, add(x, 0))  // 加法单位元
  assert_eq(x, multiply(x, 1))  // 乘法单位元
  assert_eq(0, multiply(x, 0))  // 零元素
}

test "inventory_management" {
  // 库存管理测试
  let initial_stock = 120
  let daily_sales = 15
  let days = 5
  
  let total_sold = multiply(daily_sales, days)
  let remaining_stock = add(initial_stock, -total_sold)
  
  assert_eq(75, total_sold)
  assert_eq(45, remaining_stock)
}

test "project_planning" {
  // 项目规划测试
  let total_tasks = 47
  let tasks_per_week = 12
  let weeks_needed = divide_with_ceil(total_tasks, tasks_per_week)
  
  assert_eq(4, weeks_needed)
}

test "data_processing" {
  // 数据处理测试
  let data_points = 125
  let batch_size = 20
  let full_batches = divide_with_ceil(data_points, batch_size)
  
  assert_eq(7, full_batches)
}
// Azimuth 项目简单测试用例
// 使用标准的 MoonBit 测试语法

test "simple_boundary_values" {
  // 测试基本边界值
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483647, add(-2147483646, -1))
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(1, divide_with_ceil(2147483647, 2147483647))
}

test "simple_financial_calculation" {
  // 简单金融计算
  let initial_investment = 1000
  let annual_return = 8
  
  let year1_return = add(initial_investment, divide_with_ceil(multiply(initial_investment, annual_return), 100))
  
  assert_eq(1080, year1_return)
}

test "simple_fibonacci" {
  // 斐波那契数列
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)
  let fib4 = add(fib2, fib3)
  let fib5 = add(fib3, fib4)
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
}

test "simple_logistics" {
  // 物流计算
  let warehouse_capacity = 1000
  let item_volume = 27
  let items_per_pallet = 4
  
  let pallets_capacity = divide_with_ceil(warehouse_capacity, multiply(item_volume, items_per_pallet))
  
  assert_eq(10, pallets_capacity)
}

test "simple_error_handling" {
  // 错误处理测试
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, multiply(0, 2147483647))
}

test "simple_unicode" {
  // Unicode 测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, Café!", greet("Café"))
}

test "simple_performance" {
  // 性能基准测试
  let base_operations = 1000
  let optimization_factor = 3
  
  let optimized_operations = multiply(base_operations, optimization_factor)
  
  assert_eq(3000, optimized_operations)
}

test "simple_scientific" {
  // 科学计算测试
  let initial_velocity = 10
  let acceleration = 3
  let time_seconds = 5
  
  let final_velocity = add(initial_velocity, multiply(acceleration, time_seconds))
  
  assert_eq(25, final_velocity)
}

test "simple_statistics" {
  // 统计计算测试
  let sum = 196
  let data_count = 10
  
  let average = divide_with_ceil(sum, data_count)
  
  assert_eq(20, average)
}

test "simple_business_workflow" {
  // 简单业务工作流测试
  let product_a_price = 89
  let product_a_qty = 2
  let product_b_price = 45
  let product_b_qty = 3
  
  let subtotal_a = multiply(product_a_price, product_a_qty)
  let subtotal_b = multiply(product_b_price, product_b_qty)
  let subtotal = add(subtotal_a, subtotal_b)
  
  assert_eq(178, subtotal_a)
  assert_eq(135, subtotal_b)
  assert_eq(313, subtotal)
}
// 标准 MoonBit 测试用例
// 针对 azimuth 库的综合测试，包含边界情况和实际应用场景

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(10, add(7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-7, -3))
  assert_eq(1000, add(999, 1))
  assert_eq(-1000, add(-999, -1))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(-12345, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(1, multiply(-1, -1))
  assert_eq(1000000, multiply(1000, 1000))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(0, divide_with_ceil(0, 100))
}

test "greet_function_variations" {
  // 测试问候函数的各种变化
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A B C!", greet("A B C"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算订单总价
  let product_price = 25
  let quantity = 8
  let tax_rate = 10
  let shipping = 15
  
  let subtotal = multiply(product_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(235, total)  // 25*8 + ceil(200*10/100) + 15 = 200 + 20 + 15 = 235
}

test "inventory_calculation" {
  // 库存计算：计算需要的包装箱数量
  let items_per_box = 12
  let total_items = 47
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
}

test "mathematical_identity" {
  // 数学恒等式测试
  let a = 15
  let b = 7
  
  // 加法恒等式
  assert_eq(a, add(a, 0))
  assert_eq(b, add(0, b))
  
  // 乘法恒等式
  assert_eq(a, multiply(a, 1))
  assert_eq(b, multiply(1, b))
  
  // 分配律测试
  let distributive_left = multiply(a, add(b, 3))
  let distributive_right = add(multiply(a, b), multiply(a, 3))
  assert_eq(distributive_left, distributive_right)
}

test "negative_number_division" {
  // 负数除法测试
  assert_eq(-2, divide_with_ceil(-5, 2))
  assert_eq(-3, divide_with_ceil(-6, 2))
  assert_eq(-2, divide_with_ceil(-4, 2))
  assert_eq(3, divide_with_ceil(-6, -2))
  assert_eq(2, divide_with_ceil(-4, -2))
}

test "large_value_operations" {
  // 大数值运算测试
  let large_num = 50000
  let another_large = 30000
  
  assert_eq(80000, add(large_num, another_large))
  assert_eq(1500000000, multiply(large_num, another_large))
  assert_eq(2, divide_with_ceil(large_num, another_large))
}

test "real_world_application" {
  // 实际应用场景：计算项目成本
  let hourly_rate = 85
  let hours_worked = 37
  let materials_cost = 450
  let overhead_percentage = 15
  
  let labor_cost = multiply(hourly_rate, hours_worked)
  let overhead = divide_with_ceil(multiply(labor_cost, overhead_percentage), 100)
  let total_cost = add(add(labor_cost, materials_cost), overhead)
  
  assert_eq(4095, total_cost)  // 85*37 + 450 + ceil(3145*15/100) = 3145 + 450 + 472 = 4067
}
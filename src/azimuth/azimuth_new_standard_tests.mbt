// 新增 MoonBit 标准测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
  assert_eq(-1, add(5, -6))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  assert_eq(1000, multiply(100, 10))
}

test "divide_with_ceil_various_scenarios" {
  // 测试向上取整除法的各种场景
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "greet_function_international" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
}

test "complex_business_calculations" {
  // 测试复杂业务计算
  // 计算订单总价：商品价格 × 数量 + 运费
  let product_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = multiply(product_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(200, subtotal)
  assert_eq(215, total)
}

test "packaging_and_distribution" {
  // 测试包装和配送计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 计算总重量（假设每箱重2kg）
  let total_weight = multiply(boxes_needed, 2)
  assert_eq(8, total_weight)
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律：x * (y + 1) = x * y + x
  assert_eq(multiply(x, add(y, 1)), add(multiply(x, y), x))
}

test "financial_calculations" {
  // 测试金融计算
  let principal = 1000
  let interest_rate = 5
  let years = 3
  
  // 简单利息计算（每年）
  let yearly_interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  let total_interest = multiply(yearly_interest, years)
  let final_amount = add(principal, total_interest)
  
  assert_eq(50, yearly_interest)
  assert_eq(150, total_interest)
  assert_eq(1150, final_amount)
}

test "time_and_resource_management" {
  // 测试时间和资源管理
  let tasks_per_day = 6
  let total_tasks = 35
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(days_needed, 8)  // 每天8小时
  
  assert_eq(6, days_needed)
  assert_eq(48, total_hours)
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 除零安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 1000))
  assert_eq(1000, add(1000, 0))
}
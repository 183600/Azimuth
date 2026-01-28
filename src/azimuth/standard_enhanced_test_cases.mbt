// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行补充测试

test "subtract_basic_functionality" {
  // 测试基本减法功能
  assert_eq(2, subtract(5, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-3, subtract(2, 5))
  assert_eq(8, subtract(10, 2))
}

test "subtract_negative_numbers" {
  // 测试负数减法
  assert_eq(-8, subtract(-5, 3))
  assert_eq(-2, subtract(-5, -3))
  assert_eq(2, subtract(3, -5))
  assert_eq(0, subtract(-3, -3))
}

test "subtract_zero_operations" {
  // 测试与零相关的减法运算
  assert_eq(5, subtract(5, 0))
  assert_eq(-5, subtract(0, 5))
  assert_eq(0, subtract(0, 0))
  assert_eq(10, subtract(10, 0))
}

test "add_large_numbers" {
  // 测试大数加法
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(0, add(1000000, -1000000))
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(1000001, add(1000000, 1))
}

test "multiply_boundary_values" {
  // 测试乘法边界值
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(1, multiply(-1, -1))
  assert_eq(0, multiply(2147483647, 0))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(100, divide_with_ceil(9999, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "greet_function_comprehensive" {
  // 全面测试问候函数
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, 🚀 MoonBit! 🌟!", greet("🚀 MoonBit! 🌟"))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000
  let rate1 = 5
  let rate2 = 3
  let years = 2
  
  // 第一年利息
  let interest1 = divide_with_ceil(multiply(principal, rate1), 100)
  let amount1 = add(principal, interest1)
  
  // 第二年利息
  let interest2 = divide_with_ceil(multiply(amount1, rate2), 100)
  let final_amount = add(amount1, interest2)
  
  assert_eq(500, interest1)
  assert_eq(10500, amount1)
  assert_eq(315, interest2)
  assert_eq(10815, final_amount)
}

test "inventory_management_calculation" {
  // 库存管理计算
  let initial_stock = 500
  let daily_sales = 25
  let days = 7
  let new_shipment = 200
  
  let total_sold = multiply(daily_sales, days)
  let stock_before_shipment = subtract(initial_stock, total_sold)
  let final_stock = add(stock_before_shipment, new_shipment)
  
  assert_eq(175, total_sold)
  assert_eq(325, stock_before_shipment)
  assert_eq(525, final_stock)
}
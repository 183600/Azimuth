// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含基本功能测试、边界条件测试和实际应用场景测试

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_basic_cases" {
  // 测试乘法函数的基本情况
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_comprehensive" {
  // 测试问候函数的全面功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列
  let first_term = 3
  let common_diff = 4
  let terms_count = 5
  
  // 计算等差数列第5项：a_n = a_1 + (n-1)d
  let nth_term = add(first_term, multiply(common_diff, terms_count - 1))
  assert_eq(19, nth_term)  // 3 + 4*4 = 19
}

test "financial_compound_interest" {
  // 金融复利计算
  let principal = 1000
  let rate_percent = 5
  
  // 第一年：1000 + ceil(1000*5/100) = 1000 + 50 = 1050
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  
  assert_eq(1050, year1)
}

test "inventory_optimization" {
  // 库存优化算法
  let demand_forecast = 150
  let safety_stock = 30
  let reorder_point = add(demand_forecast, safety_stock)
  
  let current_stock = 100
  let order_quantity = 200
  
  // 如果当前库存低于再订购点，则订购
  let should_order = current_stock < reorder_point
  let new_stock = if should_order { add(current_stock, order_quantity) } else { current_stock }
  
  assert_eq(180, reorder_point)
  assert_eq(true, should_order)
  assert_eq(300, new_stock)
}

test "temperature_conversion" {
  // 温度转换计算（简化版本）
  let celsius = 25
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  // 验证转换公式：°F = (°C × 9/5) + 32
  assert_eq(77, fahrenheit)  // 25*9/5 + 32 = 45 + 32 = 77
}
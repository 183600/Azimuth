// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能和边界情况

test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  assert_eq(0, add(0, 0))
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic_cases" {
  // 测试 multiply 函数的基本情况
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(100, multiply(10, 10))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试特殊值
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(0, multiply(0, max_val))
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "greet_function_special_characters" {
  // 测试 greet 函数的特殊字符处理
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello, user@example.com!", greet("user@example.com"))
}

test "complex_business_calculation" {
  // 测试复杂商业计算：订单总价计算
  let item_price = 199
  let quantity = 3
  let discount = 50
  let tax_rate = 8
  let shipping = 20
  
  // 计算商品总价
  let subtotal = multiply(item_price, quantity)
  // 计算折后价格
  let discounted_total = add(subtotal, -discount)
  // 计算税费
  let tax_amount = multiply(discounted_total, tax_rate) / 100
  // 计算最终总价
  let final_total = add(add(discounted_total, tax_amount), shipping)
  
  assert_eq(603, final_total)
}

test "mathematical_series_calculation" {
  // 测试数学级数计算：等差数列求和
  // 计算 1 + 3 + 5 + ... + 19（前10个奇数）
  let n = 10
  let first = 1
  let last = 19
  
  // 使用公式：n × (首项 + 末项) / 2
  let sum = multiply(n, add(first, last)) / 2
  assert_eq(100, sum)
  
  // 计算等比数列：2 + 4 + 8 + 16
  let geo_sum = add(add(add(2, 4), 8), 16)
  assert_eq(30, geo_sum)
}

test "geometry_area_calculations" {
  // 测试几何面积计算
  let length = 12
  let width = 8
  let radius = 5
  
  // 矩形面积
  let rectangle_area = multiply(length, width)
  assert_eq(96, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = multiply(base, height) / 2
  assert_eq(30, triangle_area)
  
  // 圆面积（简化计算）：π × r² ≈ 3 × r²
  let circle_area = multiply(3, multiply(radius, radius))
  assert_eq(75, circle_area)
}

test "time_conversion_calculations" {
  // 测试时间转换计算
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  // 转换为总分钟数
  let total_minutes = add(multiply(hours, 60), minutes)
  assert_eq(150, total_minutes)
  
  // 转换为总秒数
  let total_seconds = add(multiply(total_minutes, 60), seconds)
  assert_eq(9045, total_seconds)
  
  // 计算时间差（分钟）
  let start_time = 480  // 8:00 AM in minutes
  let end_time = 1020    // 5:00 PM in minutes
  let duration = add(end_time, -start_time)
  assert_eq(540, duration)
}
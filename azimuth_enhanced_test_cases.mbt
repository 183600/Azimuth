// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能的各种场景

test "add_function_comprehensive" {
  // 测试加法函数的综合场景
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(2147483647, add(2147483647, 100))
  assert_eq(-2147483648, add(-2147483648, -100))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合场景
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(0, multiply(2147483647, 0))
  assert_eq(-2147483648, multiply(-2147483648, 1))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "divide_with_ceil_function" {
  // 测试向上取整除法函数
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(-9, 3))
}

test "complex_business_calculation" {
  // 测试复杂商业计算场景
  let unit_price = 199
  let quantity = 3
  let discount_rate = 10
  let tax_rate = 8
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算折扣
  let discount = multiply(subtotal, discount_rate) / 100
  // 计算折后价
  let discounted_price = add(subtotal, -discount)
  // 计算税费
  let tax = multiply(discounted_price, tax_rate) / 100
  // 计算总价
  let total = add(add(discounted_price, tax), shipping)
  
  assert_eq(674, total)
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 计算等差数列前5项和：2 + 4 + 6 + 8 + 10 = 30
  let term1 = 2
  let term2 = 4
  let term3 = 6
  let term4 = 8
  let term5 = 10
  
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(30, sum)
  
  // 计算等比数列前4项积：2 × 4 × 8 × 16 = 1024
  let geo_term1 = 2
  let geo_term2 = 4
  let geo_term3 = 8
  let geo_term4 = 16
  
  let product = multiply(multiply(multiply(geo_term1, geo_term2), geo_term3), geo_term4)
  assert_eq(1024, product)
}

test "string_processing_comprehensive" {
  // 测试字符串处理综合场景
  let first_name = "John"
  let last_name = "Doe"
  let middle_initial = "M"
  
  // 构建全名
  let full_name = first_name + " " + middle_initial + ". " + last_name
  let greeting_result = greet(full_name)
  
  assert_eq_string("Hello, John M. Doe!", greeting_result)
  
  // 测试特殊字符组合
  let special_chars = "@#$%^&*()"
  let special_result = greet(special_chars)
  assert_eq_string("Hello, @#$%^&*()!", special_result)
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数：F = C × 2 + 30
  
  let celsius_freezing = 0
  let celsius_boiling = 100
  let celsius_room = 20
  
  let fahrenheit_freezing = add(multiply(celsius_freezing, 2), 30)
  let fahrenheit_boiling = add(multiply(celsius_boiling, 2), 30)
  let fahrenheit_room = add(multiply(celsius_room, 2), 30)
  
  assert_eq(30, fahrenheit_freezing)
  assert_eq(230, fahrenheit_boiling)
  assert_eq(70, fahrenheit_room)
}

test "geometry_calculation_scenario" {
  // 测试几何计算场景
  let rectangle_length = 12
  let rectangle_width = 8
  
  // 计算矩形周长：2 × (length + width)
  let perimeter = multiply(2, add(rectangle_length, rectangle_width))
  // 计算矩形面积：length × width
  let area = multiply(rectangle_length, rectangle_width)
  
  assert_eq(40, perimeter)
  assert_eq(96, area)
  
  // 计算三角形面积：base × height ÷ 2
  let triangle_base = 10
  let triangle_height = 6
  let triangle_area = multiply(triangle_base, triangle_height) / 2
  
  assert_eq(30, triangle_area)
}

test "assert_functions_validation" {
  // 测试断言函数的验证
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试复合条件
  let x = 10
  let y = 20
  assert_true(x < y)
  assert_false(x > y)
  assert_eq(add(x, y), 30)
  assert_eq(multiply(x, y), 200)
}
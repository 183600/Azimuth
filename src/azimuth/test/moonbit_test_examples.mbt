// MoonBit 测试用例集
// 测试 azimuth 库的核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(2, -7))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "greet_function_basic_tests" {
  // 测试 greet 函数的基本功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
}

test "overflow_handling_tests" {
  // 测试溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 15
  let b = 7
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let c = 5
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
}

test "special_case_handling" {
  // 测试特殊情况处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0的运算
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  
  // 测试与-1的运算
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试极值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, User#123!", @azimuth.greet("User#123"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,   LeadingSpace!", @azimuth.greet("  LeadingSpace"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1: 计算矩形面积和周长
  let length = 12
  let width = 8
  
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 场景2: 简单的利息计算
  let principal = 1000
  let rate = 5
  let years = 3
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(16000, total)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与不同数值的组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值与不同数值的组合
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, -1))
  
  // 测试边界值附近的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

test "real_world_application_tests" {
  // 测试实际应用场景
  // 场景1: 购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  let subtotal1 = @azimuth.multiply(item1_price, item1_quantity)
  let subtotal2 = @azimuth.multiply(item2_price, item2_quantity)
  let items_total = @azimuth.add(subtotal1, subtotal2)
  let final_total = @azimuth.add(items_total, shipping)
  
  @azimuth.assert_eq(717, final_total)
  
  // 场景2: 温度转换（简化版: F = C * 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 场景3: 平均值计算
  let scores_sum = @azimuth.add(@azimuth.add(@azimuth.add(85, 90), 78), 92)
  let average = scores_sum / 4
  @azimuth.assert_eq(86, average)
}
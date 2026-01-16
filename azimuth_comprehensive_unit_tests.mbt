// Azimuth 项目综合单元测试
// 使用标准 MoonBit 测试语法，覆盖核心功能的各种场景

// 导入主模块中的函数
use azimuth

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(2, -3))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
  assert_eq(25, multiply(5, 5))
  assert_eq(100, multiply(-10, -10))
}

test "greet_function_basic_functionality" {
  // 测试 greet 函数的基本功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, User!", greet("User"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 15
  let b = 27
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 42
  
  // 加法恒等式: x + 0 = x
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式: x * 1 = x
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 零乘法: x * 0 = 0
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "practical_application_scenarios" {
  // 测试实际应用场景
  
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping_fee = 15
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping_fee)
  
  assert_eq(597, subtotal)
  assert_eq(612, total)
  
  // 温度转换 (摄氏度转华氏度: F = C * 9/5 + 32)
  let celsius = 20
  let fahrenheit = add(multiply(celsius, 9) / 5, 32)
  assert_eq(68, fahrenheit)
}

test "edge_cases_and_boundary_values" {
  // 测试边界条件和特殊情况
  
  // 大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  assert_eq(3000000, add(large_num1, large_num2))
  assert_eq(2000000000000, multiply(large_num1, large_num2))
  
  // 负数运算
  assert_eq(-10, add(-5, -5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  
  // 长字符串
  let long_name = "ThisIsAVeryLongNameThatExceedsNormalLengthExpectations"
  let result = greet(long_name)
  assert_eq_string("Hello, " + long_name + "!", result)
  
  // 特殊字符
  let special_chars = "test@example.com"
  let result2 = greet(special_chars)
  assert_eq_string("Hello, test@example.com!", result2)
  
  // Unicode 字符
  let unicode_name = "世界"
  let result3 = greet(unicode_name)
  assert_eq_string("Hello, 世界!", result3)
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  
  // 复合利息计算 (简化版)
  let principal = 1000
  let rate = 5
  let years = 3
  let amount = principal
  
  // 简单的年复利计算
  let year1 = add(amount, multiply(amount, rate) / 100)
  let year2 = add(year1, multiply(year1, rate) / 100)
  let year3 = add(year2, multiply(year2, rate) / 100)
  
  assert_eq(1050, year1)
  assert_eq(1102, year2)  // 取整
  assert_eq(1157, year3)  // 取整
}
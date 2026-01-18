// Azimuth 项目新的标准测试用例
// 使用标准 MoonBit 测试语法，测试核心功能

// 导入必要的模块
use azimuth

test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  @assertion.assert_eq(5, azimuth.add(2, 3))?
  @assertion.assert_eq(0, azimuth.add(0, 0))?
  @assertion.assert_eq(-2, azimuth.add(-5, 3))?
  @assertion.assert_eq(10, azimuth.add(10, 0))?
  @assertion.assert_eq(7, azimuth.add(0, 7))?
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @assertion.assert_eq(max_val, azimuth.add(max_val, 1))?
  @assertion.assert_eq(max_val, azimuth.add(max_val, max_val))?
  @assertion.assert_eq(min_val, azimuth.add(min_val, -1))?
  @assertion.assert_eq(min_val, azimuth.add(min_val, min_val))?
  
  // 测试边界值加法
  @assertion.assert_eq(max_val, azimuth.add(max_val, 0))?
  @assertion.assert_eq(min_val, azimuth.add(min_val, 0))?
}

test "multiply_function_basic_cases" {
  // 测试 multiply 函数的基本情况
  @assertion.assert_eq(6, azimuth.multiply(2, 3))?
  @assertion.assert_eq(0, azimuth.multiply(5, 0))?
  @assertion.assert_eq(0, azimuth.multiply(0, 10))?
  @assertion.assert_eq(-15, azimuth.multiply(-3, 5))?
  @assertion.assert_eq(20, azimuth.multiply(-4, -5))?
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, 2))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, -1))?
  @assertion.assert_eq(min_val, azimuth.multiply(max_val, -2))?
  
  // 测试特殊值
  @assertion.assert_eq(5, azimuth.multiply(5, 1))?
  @assertion.assert_eq(5, azimuth.multiply(1, 5))?
  @assertion.assert_eq(-5, azimuth.multiply(5, -1))?
  @assertion.assert_eq(-5, azimuth.multiply(-1, 5))?
}

test "greet_function_basic_cases" {
  // 测试 greet 函数的基本情况
  @assertion.assert_eq_string("Hello, World!", azimuth.greet("World"))?
  @assertion.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))?
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))?
  @assertion.assert_eq_string("Hello, 123!", azimuth.greet("123"))?
}

test "greet_function_special_characters" {
  // 测试 greet 函数的特殊字符处理
  @assertion.assert_eq_string("Hello, 中文!", azimuth.greet("中文"))?
  @assertion.assert_eq_string("Hello, 🌙!", azimuth.greet("🌙"))?
  @assertion.assert_eq_string("Hello, Hello-World!", azimuth.greet("Hello-World"))?
  @assertion.assert_eq_string("Hello, @#$%!", azimuth.greet("@#$%"))?
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 10
  let b = 7
  
  // 加法交换律
  @assertion.assert_eq(azimuth.add(a, b), azimuth.add(b, a))?
  
  // 乘法交换律
  @assertion.assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))?
  
  // 加法单位元
  @assertion.assert_eq(a, azimuth.add(a, 0))?
  @assertion.assert_eq(a, azimuth.add(0, a))?
  
  // 乘法单位元
  @assertion.assert_eq(a, azimuth.multiply(a, 1))?
  @assertion.assert_eq(a, azimuth.multiply(1, a))?
}

test "complex_calculations" {
  // 测试复杂计算
  let a = 3
  let b = 4
  let c = 5
  
  // (a + b) * c
  let result1 = azimuth.multiply(azimuth.add(a, b), c)
  @assertion.assert_eq(35, result1)?
  
  // a * b + a * c
  let result2 = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  @assertion.assert_eq(27, result2)?
  
  // (a + b + c) * 2
  let result3 = azimuth.multiply(azimuth.add(azimuth.add(a, b), c), 2)
  @assertion.assert_eq(24, result3)?
}

test "real_world_scenarios" {
  // 测试真实世界场景
  // 购物车计算
  let item_price = 299
  let quantity = 3
  let shipping = 20
  let total = azimuth.add(azimuth.multiply(item_price, quantity), shipping)
  @assertion.assert_eq(917, total)?
  
  // 面积计算
  let length = 10
  let width = 5
  let area = azimuth.multiply(length, width)
  @assertion.assert_eq(50, area)?
  
  // 周长计算
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  @assertion.assert_eq(30, perimeter)?
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出时不会崩溃
  let overflow_result = azimuth.add(max_val, max_val)
  @assertion.assert_eq(max_val, overflow_result)?
  
  let underflow_result = azimuth.add(min_val, min_val)
  @assertion.assert_eq(min_val, underflow_result)?
  
  let mul_overflow_result = azimuth.multiply(max_val, max_val)
  @assertion.assert_eq(max_val, mul_overflow_result)?
  
  let mul_underflow_result = azimuth.multiply(min_val, min_val)
  @assertion.assert_eq(max_val, mul_underflow_result)?  // 负负得正，但会溢出
}
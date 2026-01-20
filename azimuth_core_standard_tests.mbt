// Azimuth 项目核心标准测试用例
// 使用标准 MoonBit 测试语法，测试核心功能

test "add_function_basic" {
  // 测试基本加法功能
  let result1 = azimuth.add(2, 3)
  let result2 = azimuth.add(10, 20)
  let result3 = azimuth.add(-5, 15)
  let result4 = azimuth.add(0, 42)
  
  // 验证结果
  azimuth.assert_eq(5, result1)
  azimuth.assert_eq(30, result2)
  azimuth.assert_eq(10, result3)
  azimuth.assert_eq(42, result4)
}

test "add_function_edge_cases" {
  // 测试加法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0相加
  azimuth.assert_eq(100, azimuth.add(100, 0))
  azimuth.assert_eq(100, azimuth.add(0, 100))
  azimuth.assert_eq(0, azimuth.add(0, 0))
  
  // 测试最小值情况
  azimuth.assert_eq(min_val, azimuth.add(min_val, 0))
  azimuth.assert_eq(-2147483647, azimuth.add(min_val, 1))
}

test "add_function_overflow" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  azimuth.assert_eq(max_val, azimuth.add(max_val, 1))
  azimuth.assert_eq(max_val, azimuth.add(1000000000, 1500000000))
  
  // 测试负数溢出
  azimuth.assert_eq(min_val, azimuth.add(min_val, -1))
  azimuth.assert_eq(min_val, azimuth.add(-1500000000, -1000000000))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  let result1 = azimuth.multiply(2, 3)
  let result2 = azimuth.multiply(10, 10)
  let result3 = azimuth.multiply(-5, 4)
  let result4 = azimuth.multiply(0, 42)
  
  // 验证结果
  azimuth.assert_eq(6, result1)
  azimuth.assert_eq(100, result2)
  azimuth.assert_eq(-20, result3)
  azimuth.assert_eq(0, result4)
}

test "multiply_function_edge_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0和1相乘
  azimuth.assert_eq(0, azimuth.multiply(5, 0))
  azimuth.assert_eq(0, azimuth.multiply(0, 5))
  azimuth.assert_eq(5, azimuth.multiply(5, 1))
  azimuth.assert_eq(5, azimuth.multiply(1, 5))
  
  // 测试与-1相乘
  azimuth.assert_eq(-5, azimuth.multiply(5, -1))
  azimuth.assert_eq(-5, azimuth.multiply(-1, 5))
  azimuth.assert_eq(5, azimuth.multiply(-5, -1))
}

test "multiply_function_overflow" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最小值与-1相乘的特殊情况
  azimuth.assert_eq(min_val, azimuth.multiply(min_val, -1))
  
  // 测试大数相乘溢出
  azimuth.assert_eq(max_val, azimuth.multiply(46341, 46341))
  azimuth.assert_eq(min_val, azimuth.multiply(-46341, 46341))
}

test "greet_function_basic" {
  // 测试基本问候功能
  let result1 = azimuth.greet("World")
  let result2 = azimuth.greet("MoonBit")
  let result3 = azimuth.greet("测试")
  
  // 验证结果
  azimuth.assert_eq_string("Hello, World!", result1)
  azimuth.assert_eq_string("Hello, MoonBit!", result2)
  azimuth.assert_eq_string("Hello, 测试!", result3)
}

test "greet_function_edge_cases" {
  // 测试问候函数边界情况
  // 测试空字符串
  let result1 = azimuth.greet("")
  azimuth.assert_eq_string("Hello, !", result1)
  
  // 测试单字符
  let result2 = azimuth.greet("A")
  azimuth.assert_eq_string("Hello, A!", result2)
  
  // 测试包含特殊字符的字符串
  let result3 = azimuth.greet("test@example.com")
  azimuth.assert_eq_string("Hello, test@example.com!", result3)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的周长
  let length = 10
  let width = 5
  let perimeter = azimuth.add(azimuth.multiply(2, length), azimuth.multiply(2, width))
  azimuth.assert_eq(30, perimeter)
  
  // 测试温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 20
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  azimuth.assert_eq(70, fahrenheit)
}

test "assert_functions_validation" {
  // 测试断言函数本身
  azimuth.assert_true(true)
  azimuth.assert_false(false)
  azimuth.assert_eq(42, 42)
  azimuth.assert_eq_string("test", "test")
  
  // 测试计算结果的断言
  let sum = azimuth.add(15, 25)
  azimuth.assert_eq(40, sum)
  
  let product = azimuth.multiply(6, 7)
  azimuth.assert_eq(42, product)
  
  let greeting = azimuth.greet("MoonBit")
  azimuth.assert_eq_string("Hello, MoonBit!", greeting)
}
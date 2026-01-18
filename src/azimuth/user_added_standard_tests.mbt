// 用户添加的标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，测试 azimuth 模块的核心功能

// 导入必要的模块
use azimuth

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @assertion.assert_eq(15, azimuth.add(7, 8))?
  @assertion.assert_eq(-3, azimuth.add(5, -8))?
  @assertion.assert_eq(0, azimuth.add(10, -10))?
  
  @assertion.assert_eq(42, azimuth.multiply(6, 7))?
  @assertion.assert_eq(-24, azimuth.multiply(8, -3))?
  @assertion.assert_eq(0, azimuth.multiply(15, 0))?
}

test "overflow_edge_cases" {
  // 溢出边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @assertion.assert_eq(max_val, azimuth.add(max_val, 1))?
  @assertion.assert_eq(max_val, azimuth.add(max_val, 100))?
  @assertion.assert_eq(min_val, azimuth.add(min_val, -1))?
  @assertion.assert_eq(min_val, azimuth.add(min_val, -100))?
  
  // 测试乘法溢出保护
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, 2))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, 2))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, -1))?
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  @assertion.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))?
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))?
  @assertion.assert_eq_string("Hello, 张三!", azimuth.greet("张三"))?
  @assertion.assert_eq_string("Hello, 123!", azimuth.greet("123"))?
  @assertion.assert_eq_string("Hello, 🌍!", azimuth.greet("🌍"))?
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 5
  let c = 3
  
  // 验证加法交换律
  @assertion.assert_eq(azimuth.add(a, b), azimuth.add(b, a))?
  
  // 验证乘法交换律
  @assertion.assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))?
  
  // 验证分配律
  @assertion.assert_eq(
    azimuth.multiply(a, azimuth.add(b, c)),
    azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  )?
}

test "real_world_calculations" {
  // 实际应用计算测试
  // 计算矩形面积和周长
  let length = 8
  let width = 6
  
  let area = azimuth.multiply(length, width)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  
  @assertion.assert_eq(48, area)?
  @assertion.assert_eq(28, perimeter)?
  
  // 简单的温度转换（摄氏度转华氏度：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  @assertion.assert_eq(80, fahrenheit)?
}

test "divide_with_ceil_function" {
  // 向上取整除法测试
  @assertion.assert_eq(3, azimuth.divide_with_ceil(10, 3))?
  @assertion.assert_eq(4, azimuth.divide_with_ceil(12, 3))?
  @assertion.assert_eq(5, azimuth.divide_with_ceil(21, 4))?
  @assertion.assert_eq(7, azimuth.divide_with_ceil(14, 2))?
  @assertion.assert_eq(0, azimuth.divide_with_ceil(0, 5))?
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 计算订单总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let tax_rate = 8  // 8%
  
  let subtotal = azimuth.multiply(item_price, quantity)
  let tax = azimuth.multiply(subtotal, tax_rate) / 100
  let total = azimuth.add(azimuth.add(subtotal, tax), shipping)
  
  @assertion.assert_eq(661, total)?
  
  // 计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 78
  let average = azimuth.add(azimuth.add(score1, score2), score3) / 3
  @assertion.assert_eq(84, average)?
}

test "assertion_functions_test" {
  // 断言函数测试
  @assertion.assert_true(true)?
  @assertion.assert_false(false)?
  
  @assertion.assert_eq(100, 100)?
  @assertion.assert_eq(-50, -50)?
  
  @assertion.assert_eq_string("test", "test")?
  @assertion.assert_eq_string("", "")?
}

test "special_value_handling" {
  // 特殊值处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0的运算
  @assertion.assert_eq(max_val, azimuth.add(max_val, 0))?
  @assertion.assert_eq(min_val, azimuth.add(min_val, 0))?
  @assertion.assert_eq(0, azimuth.multiply(max_val, 0))?
  @assertion.assert_eq(0, azimuth.multiply(min_val, 0))?
  
  // 测试与1的运算
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, 1))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, 1))?
  @assertion.assert_eq(max_val, azimuth.add(max_val, 0))?
  @assertion.assert_eq(min_val, azimuth.add(min_val, 0))?
}

test "sequential_operations" {
  // 连续运算测试
  // 连续加法
  let result1 = azimuth.add(azimuth.add(1, 2), azimuth.add(3, 4))
  @assertion.assert_eq(10, result1)?
  
  // 连续乘法
  let result2 = azimuth.multiply(azimuth.multiply(2, 3), azimuth.multiply(4, 5))
  @assertion.assert_eq(120, result2)?
  
  // 混合运算
  let result3 = azimuth.add(azimuth.multiply(5, 3), azimuth.multiply(2, 4))
  @assertion.assert_eq(23, result3)?
}
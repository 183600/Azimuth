// MoonBit 标准测试用例示例
// 测试 Azimuth 项目的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(30, @azimuth.multiply(5, 6))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
}

test "negative_number_calculations" {
  // 测试负数计算
  @azimuth.assert_eq(-5, @azimuth.add(10, -15))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-30, @azimuth.multiply(6, -5))
  @azimuth.assert_eq(0, @azimuth.add(-100, 100))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 7
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
}

test "real_world_shopping_scenario" {
  // 测试实际购物场景
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(612, total)
}

test "temperature_conversion_simplified" {
  // 测试简化的温度转换（摄氏度转华氏度）
  // 使用简化公式：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(80, fahrenheit)
}

test "sequential_operations" {
  // 测试连续运算
  // 计算 (a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(38, result)
}

test "string_with_special_characters" {
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "complex_calculation_pattern" {
  // 测试复杂计算模式
  // 计算平均值的整数部分
  let numbers_total = @azimuth.add(@azimuth.add(@azimuth.add(85, 90), 95), 100)
  let count = 4
  let average = numbers_total / count
  
  @azimuth.assert_eq(92, average)
}
// 新增的 MoonBit 测试用例 - 不超过10个
// 使用标准的 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(4, 6))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "edge_case_calculations" {
  // 测试边界情况计算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  // 计算 (a + b) * c - d
  let a = 8
  let b = 7
  let c = 3
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(35, result)
}

test "conditional_logic_with_math" {
  // 测试条件逻辑与数学运算结合
  let x = 25
  let y = 30
  
  // 如果 x < y，计算 x + y，否则计算 x * y
  let condition = x < y
  let result = if condition { @azimuth.add(x, y) } else { @azimuth.multiply(x, y) }
  
  @azimuth.assert_eq(55, result)
  
  // 反向测试
  let condition2 = x > y
  let result2 = if condition2 { @azimuth.add(x, y) } else { @azimuth.multiply(x, y) }
  
  @azimuth.assert_eq(750, result2)
}

test "real_world_shopping_cart" {
  // 测试实际购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  // 计算总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let final_total = @azimuth.add(@azimuth.add(total1, total2), shipping)
  
  @azimuth.assert_eq(717, final_total)
}

test "temperature_conversion_formula" {
  // 测试温度转换公式（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30
  let celsius = 18
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(66, fahrenheit)
  
  // 测试冰点
  @azimuth.assert_eq(30, @azimuth.add(@azimuth.multiply(0, 2), 30))
  
  // 测试沸点
  @azimuth.assert_eq(230, @azimuth.add(@azimuth.multiply(100, 2), 30))
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算平均值
  let numbers_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(85, 90), 75), 95), 80)
  let count = 5
  let average = numbers_sum / count
  
  @azimuth.assert_eq(85, average)
  
  // 计算总和
  @azimuth.assert_eq(425, numbers_sum)
}

test "string_processing_with_special_chars" {
  // 测试特殊字符字符串处理
  let email = "user@example.com"
  let phone = "123-456-7890"
  let mixed = "Test_123@Project"
  
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet(email))
  @azimuth.assert_eq_string("Hello, 123-456-7890!", @azimuth.greet(phone))
  @azimuth.assert_eq_string("Hello, Test_123@Project!", @azimuth.greet(mixed))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律 a × (b + c) = a × b + a × c
  let c = 5
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}
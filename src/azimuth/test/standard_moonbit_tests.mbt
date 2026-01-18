// 标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

// 导入被测试的模块
use @azimuth

// 测试1: 基本加法运算
test "basic_addition" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

// 测试2: 加法边界值测试
test "addition_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试最小值加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试零值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

// 测试3: 基本乘法运算
test "basic_multiplication" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
}

// 测试4: 乘法边界值测试
test "multiplication_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试特殊值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

// 测试5: 字符串问候功能
test "greet_functionality" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

// 测试6: 数学运算组合
test "combined_math_operations" {
  // 测试 (a + b) * c 的计算
  let a = 5
  let b = 3
  let c = 4
  
  let sum_ab = @azimuth.add(a, b)  // 5 + 3 = 8
  let result = @azimuth.multiply(sum_ab, c)  // 8 * 4 = 32
  
  @azimuth.assert_eq(32, result)
}

// 测试7: 实际应用场景 - 购物车计算
test "shopping_cart_calculation" {
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)  // 199 * 2 = 398
  let total2 = @azimuth.multiply(item2_price, item2_quantity)  // 299 * 1 = 299
  
  // 计算购物车总价
  let cart_total = @azimuth.add(total1, total2)  // 398 + 299 = 697
  
  @azimuth.assert_eq(697, cart_total)
}

// 测试8: 温度转换计算
test "temperature_conversion" {
  // 摄氏度转华氏度（简化版）：F = C * 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  
  @azimuth.assert_eq(30, freezing_fahrenheit)  // 0 * 2 + 30 = 30
}

// 测试9: 字符串特殊字符处理
test "special_characters_in_strings" {
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, émoji!", @azimuth.greet("émoji"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

// 测试10: 复杂数学公式验证
test "mathematical_formula_validation" {
  // 验证分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))  // 3 * (4 + 5) = 27
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))  // 3*4 + 3*5 = 27
  
  @azimuth.assert_eq(left_side, right_side)
}
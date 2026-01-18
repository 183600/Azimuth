// 用户指定的 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(15, @azimuth.add(8, 7))
  
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
  @azimuth.assert_eq(-5, @azimuth.add(5, -10))
  
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(0, @azimuth.add(-100, 100))
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 6
  let b = 4
  let c = 5
  
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left, right)
}

test "temperature_conversion" {
  // 测试温度转换场景：摄氏度转华氏度（简化版 F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
}

test "calculate_rectangle_perimeter" {
  // 测试计算矩形周长：2 × (长 + 宽)
  let length = 12
  let width = 8
  
  let sum = @azimuth.add(length, width)
  let perimeter = @azimuth.multiply(2, sum)
  
  @azimuth.assert_eq(40, perimeter)
}

test "compound_interest_calculation" {
  // 测试复利计算（简化版）：本金 × (1 + 利率 × 年数)
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(16000, total)  // 1000 + 1000×5×3 = 16000
}

test "shopping_cart_with_discount" {
  // 测试购物车计算（含折扣）
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let discount = 100
  
  let subtotal1 = @azimuth.multiply(item1_price, item1_quantity)
  let subtotal2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(subtotal1, subtotal2)
  let total = @azimuth.add(subtotal, -discount)
  
  @azimuth.assert_eq(598, total)  // 299×2 + 199×1 - 100 = 598
}

test "string_processing_variations" {
  // 测试各种字符串处理情况
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 2024!", @azimuth.greet("2024"))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 最大值与0相乘
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 最小值与1相乘
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "sequential_operations" {
  // 测试连续运算：(a + b) × (c - d)
  let a = 15
  let b = 10
  let c = 20
  let d = 8
  
  let sum = @azimuth.add(a, b)  // 25
  let diff = @azimuth.add(c, -d)  // 12
  let result = @azimuth.multiply(sum, diff)  // 300
  
  @azimuth.assert_eq(300, result)
}

test "average_calculation" {
  // 测试平均值计算：(a + b + c) ÷ 3
  let num1 = 85
  let num2 = 90
  let num3 = 95
  
  let sum = @azimuth.add(@azimuth.add(num1, num2), num3)  // 270
  let average = sum / 3  // 90
  
  @azimuth.assert_eq(90, average)
}
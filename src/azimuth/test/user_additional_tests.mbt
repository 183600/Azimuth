// 用户添加的 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_function_commutative" {
  // 测试加法交换律
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(15, @azimuth.add(8, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "add_function_associative" {
  // 测试加法结合律
  @azimuth.assert_eq(15, @azimuth.add(@azimuth.add(3, 4), 8))
  @azimuth.assert_eq(15, @azimuth.add(3, @azimuth.add(4, 8)))
  @azimuth.assert_eq(-10, @azimuth.add(@azimuth.add(-5, -3), -2))
  @azimuth.assert_eq(-10, @azimuth.add(-5, @azimuth.add(-3, -2)))
}

test "multiply_function_commutative" {
  // 测试乘法交换律
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(4, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(5, -3))
}

test "multiply_function_distributive" {
  // 测试乘法分配律
  @azimuth.assert_eq(22, @azimuth.multiply(2, @azimuth.add(5, 6)))
  @azimuth.assert_eq(22, @azimuth.add(@azimuth.multiply(2, 5), @azimuth.multiply(2, 6)))
  @azimuth.assert_eq(-6, @azimuth.multiply(3, @azimuth.add(-2, 0)))
  @azimuth.assert_eq(-6, @azimuth.add(@azimuth.multiply(3, -2), @azimuth.multiply(3, 0)))
}

test "greet_function_unicode" {
  // 测试包含Unicode字符的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, Café!", @azimuth.greet("Café"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(total1, total2)
  
  // 验证总价：199*2 + 299*1 = 398 + 299 = 697
  @azimuth.assert_eq(697, cart_total)
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化版）：F = C * 2 + 30
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = @azimuth.add(@azimuth.multiply(100, 2), 30)
  @azimuth.assert_eq(230, boiling)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与0的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 最小值与0的运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "mathematical_formula_validation" {
  // 测试数学公式验证
  let a = 5
  let b = 3
  
  // 平方差公式：(a+b)*(a-b) = a²-b²
  let sum = @azimuth.add(a, b)
  let diff = @azimuth.add(a, -b)
  let product = @azimuth.multiply(sum, diff)
  let square_diff = @azimuth.add(@azimuth.multiply(a, a), @azimuth.multiply(-b, b))
  
  @azimuth.assert_eq(product, square_diff)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景
  // 简单利息计算：本金 + 本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 3年
  
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  let total = @azimuth.add(principal, interest)
  
  // 验证：10000 + 10000*5*3 = 10000 + 150000 = 160000
  @azimuth.assert_eq(160000, total)
}
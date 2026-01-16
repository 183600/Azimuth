// 简洁的标准 MoonBit 测试用例
// 测试核心功能：add、multiply 和 greet 函数

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-1, -2))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(15, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-8, @azimuth.multiply(-2, 4))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "add_boundary_values" {
  // 测试加法的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_boundary_values" {
  // 测试乘法的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 10
  let width = 5
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
  
  // 周长 = 2 × 长 + 2 × 宽
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(30, perimeter)
}

test "financial_calculation" {
  // 测试金融计算场景：简单利息
  let principal = 1000  // 本金
  let rate = 5          // 利率百分比
  let years = 3         // 年数
  
  // 利息 = 本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  @azimuth.assert_eq(15000, interest)
  
  // 总金额 = 本金 + 利息
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(16000, total)
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度（简化版）
  // F = C × 2 + 30
  
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
}

test "shopping_cart_total" {
  // 测试购物车总价计算
  let item1_price = 100
  let item1_quantity = 2
  let item2_price = 50
  let item2_quantity = 3
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(total1, total2)
  @azimuth.assert_eq(350, cart_total)
}

test "string_with_special_characters" {
  // 测试包含特殊字符的字符串处理
  @azimuth.assert_eq_string("Hello, John@Doe!", @azimuth.greet("John@Doe"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}
// 新增的 MoonBit 测试用例示例
// 这些测试用例展示了标准的 MoonBit 测试语法

test "add_basic_functionality" {
  // 基本加法功能测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
}

test "add_overflow_protection" {
  // 加法溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 基本乘法功能测试
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "multiply_overflow_protection" {
  // 乘法溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "greet_functionality" {
  // greet 函数功能测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "assert_functions_work_correctly" {
  // 断言函数功能测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试：计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(26, perimeter)
}

test "shopping_cart_total" {
  // 购物车总价计算测试
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  // 计算商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(total1, total2)
  
  // 计算最终总价（含运费）
  let final_total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(717, final_total)  // 199*2 + 299*1 + 20 = 398 + 299 + 20 = 717
}

test "temperature_conversion" {
  // 温度转换测试（简化版）
  // 摄氏度转华氏度：F = C * 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 30) / 2
  let fahrenheit2 = 80
  let celsius2 = @azimuth.add(fahrenheit2, -30) / 2
  @azimuth.assert_eq(25, celsius2)
}

test "compound_interest_calculation" {
  // 复利计算测试（简化版）
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  
  // 简化的复利计算：本息和 = 本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let total = @azimuth.multiply(principal, growth_factor)
  
  @azimuth.assert_eq(16000, total)  // 1000 × (1 + 5×3) = 1000 × 16 = 16000
}
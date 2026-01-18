// 标准 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(8, @azimuth.add(5, 3))
  @azimuth.assert_eq(-2, @azimuth.add(5, -7))
  @azimuth.assert_eq(12, @azimuth.multiply(4, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(5, -3))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_processing" {
  // 测试字符串处理
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试123!", @azimuth.greet("测试123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 3)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3))
  )
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(length, 2), @azimuth.multiply(width, 2))
  
  @azimuth.assert_eq(40, area)
  @azimuth.assert_eq(26, perimeter)
}

test "financial_calculation" {
  // 测试金融计算场景：简单利息计算
  let principal = 10000
  let rate = 5  // 5%
  let time = 3   // 3年
  
  // 简单利息：本金 × 利率 × 时间
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  
  // 总金额：本金 + 利息
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(150000, total)
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度（简化版）
  // F = C × 2 + 30 （避免浮点数运算）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
}

test "error_handling" {
  // 测试错误处理
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 验证溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "real_world_shopping_cart" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  let shipping = 15
  
  // 计算每种商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(@azimuth.add(total1, total2), shipping)
  
  @azimuth.assert_eq(710, cart_total)
}

test "sequential_operations" {
  // 测试连续运算
  // 计算：(a + b) * (c + d) - e
  let a = 10
  let b = 15
  let c = 5
  let d = 8
  let e = 20
  
  let sum1 = @azimuth.add(a, b)  // 25
  let sum2 = @azimuth.add(c, d)  // 13
  let product = @azimuth.multiply(sum1, sum2)  // 325
  let result = @azimuth.add(product, -e)  // 305
  
  @azimuth.assert_eq(305, result)
}
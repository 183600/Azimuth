// 标准 MoonBit 测试用例 v9
// 包含10个精心设计的测试用例，覆盖各种场景

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  let a = 12
  let b = 18
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 边界值处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算矩形面积和周长
  let length = 15
  let width = 8
  
  // 计算面积：长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(120, area)
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(46, perimeter)
}

test "financial_calculation_simple_interest" {
  // 金融计算：单利计算
  let principal = 5000  // 本金
  let rate = 6          // 利率百分比
  let time = 3          // 时间（年）
  
  // 简单利息计算：利息 = 本金 × 利率 × 时间
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  @azimuth.assert_eq(90000, interest)  // 5000 × 6 × 3 = 90000
  
  // 总金额 = 本金 + 利息
  let total_amount = @azimuth.add(principal, interest)
  @azimuth.assert_eq(95000, total_amount)
}

test "temperature_conversion_simplified" {
  // 温度转换：摄氏度转华氏度（简化版）
  // 使用简化公式：F = C × 2 + 30（避免小数运算）
  
  let celsius_0 = 0      // 冰点
  let fahrenheit_0 = @azimuth.add(@azimuth.multiply(celsius_0, 2), 30)
  @azimuth.assert_eq(30, fahrenheit_0)
  
  let celsius_25 = 25    // 室温
  let fahrenheit_25 = @azimuth.add(@azimuth.multiply(celsius_25, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_25)
  
  let celsius_100 = 100  // 沸点
  let fahrenheit_100 = @azimuth.add(@azimuth.multiply(celsius_100, 2), 30)
  @azimuth.assert_eq(230, fahrenheit_100)
}

test "shopping_cart_calculation" {
  // 购物车计算测试
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算每种商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)  // 398
  let total2 = @azimuth.multiply(item2_price, item2_quantity)  // 299
  
  // 计算商品总价
  let subtotal = @azimuth.add(total1, total2)  // 697
  
  // 加上运费
  let final_total = @azimuth.add(subtotal, shipping_fee)  // 717
  
  @azimuth.assert_eq(717, final_total)
}

test "mathematical_series_summation" {
  // 数学数列求和：计算等差数列的和
  // 数列：3, 7, 11, 15, 19（公差为4的等差数列）
  
  let term1 = 3
  let term2 = @azimuth.add(term1, 4)   // 7
  let term3 = @azimuth.add(term2, 4)   // 11
  let term4 = @azimuth.add(term3, 4)   // 15
  let term5 = @azimuth.add(term4, 4)   // 19
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(55, sum)
  
  // 使用公式验证：n × (首项 + 末项) / 2
  let n = 5
  let formula_sum = @azimuth.multiply(n, @azimuth.add(term1, term5)) / 2
  @azimuth.assert_eq(55, formula_sum)
}

test "assert_functions_validation" {
  // 断言函数验证测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  
  // 验证计算结果
  let calculation_result = @azimuth.add(@azimuth.multiply(10, 5), 20)
  @azimuth.assert_eq(70, calculation_result)
  
  let string_result = @azimuth.greet("TestUser")
  @azimuth.assert_eq_string("Hello, TestUser!", string_result)
}
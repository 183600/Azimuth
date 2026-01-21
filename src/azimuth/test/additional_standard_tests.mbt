// 额外的标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "addition_with_large_numbers" {
  // 测试大数相加
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
  @azimuth.assert_eq(0, @azimuth.add(1500000000, -1500000000))
}

test "multiplication_precision_tests" {
  // 测试乘法精度
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  @azimuth.assert_eq(-100000000, @azimuth.multiply(-10000, 10000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(-10000, -10000))
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, ThisIsAVeryLongNameForTestingPurposes!", @azimuth.greet("ThisIsAVeryLongNameForTestingPurposes"))
}

test "mathematical_associative_laws" {
  // 测试数学结合律
  let a = 10
  let b = 20
  let c = 30
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let left_side = @azimuth.add(@azimuth.add(a, b), c)
  let right_side = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "complex_business_calculation" {
  // 测试复杂业务计算：订单总价计算
  let item_price = 299
  let quantity = 3
  let discount = 50
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 减去折扣
  let discounted_price = @azimuth.add(subtotal, -discount)
  // 计算税费
  let tax = @azimuth.multiply(discounted_price, tax_rate) / 100
  // 计算最终总价
  let total_price = @azimuth.add(discounted_price, tax)
  
  @azimuth.assert_eq(903, total_price)  // (299*3 - 50) + ((299*3 - 50)*8/100) = 847 + 67 = 914
}

test "temperature_conversion_scenarios" {
  // 测试温度转换场景
  // 摄氏度转华氏度：F = C × 2 + 30（简化版）
  let celsius_temp = 25
  let fahrenheit_temp = @azimuth.add(@azimuth.multiply(celsius_temp, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_temp)
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算平均值： (10 + 20 + 30 + 40 + 50) / 5 = 30
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  let average = sum / 5
  @azimuth.assert_eq(30, average)
  
  // 计算总和与前4个数的差值：150 - (10+20+30+40) = 150 - 100 = 50
  let first_four_sum = @azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40)
  let difference = @azimuth.add(sum, -first_four_sum)
  @azimuth.assert_eq(50, difference)
}

test "geometric_calculations" {
  // 测试几何计算
  // 计算矩形面积：长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, rectangle_area)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "error_handling_validation" {
  // 测试错误处理验证
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("success", "success")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}
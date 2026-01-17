// 用户请求的新增 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "addition_with_various_numbers" {
  // 测试各种数字的加法运算
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(-50, 50))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
}

test "multiplication_with_various_numbers" {
  // 测试各种数字的乘法运算
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-46341, 46341))
}

test "greet_function_with_different_inputs" {
  // 测试 greet 函数的不同输入
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob123!", @azimuth.greet("Bob123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "mathematical_associative_law" {
  // 测试数学结合律: (a + b) + c = a + (b + c)
  let a = 10
  let b = 20
  let c = 30
  
  let left_side = @azimuth.add(@azimuth.add(a, b), c)  // (10 + 20) + 30 = 60
  let right_side = @azimuth.add(a, @azimuth.add(b, c))  // 10 + (20 + 30) = 60
  
  @azimuth.assert_eq(left_side, right_side)
}

test "complex_financial_calculation" {
  // 测试复杂金融计算：贷款利息计算
  let principal = 10000  // 本金
  let rate = 5           // 年利率5%
  let years = 3          // 3年
  let fee = 100          // 手续费
  
  // 计算利息：本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  // 计算总额：本金 + 利息 + 手续费
  let total = @azimuth.add(@azimuth.add(principal, interest), fee)
  
  @azimuth.assert_eq(160100, total)  // 10000 + 10000*5*3 + 100 = 160100
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景：摄氏度转华氏度
  // 简化公式：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = @azimuth.add(@azimuth.multiply(100, 2), 30)
  @azimuth.assert_eq(230, boiling)
}

test "geometry_area_calculations" {
  // 测试几何面积计算
  // 矩形面积：长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "shopping_cart_with_discount" {
  // 测试购物车折扣计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let discount_threshold = 500
  let discount_amount = 50
  
  // 计算商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(total1, total2)
  
  // 应用折扣（如果超过阈值）
  let final_total = if (subtotal > discount_threshold) {
    @azimuth.add(subtotal, -discount_amount)
  } else {
    subtotal
  }
  
  @azimuth.assert_eq(747, final_total)  // (299*2 + 199*1) - 50 = 797 - 50 = 747
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算平均值：(a + b + c + d + e) / 5
  let a = 85
  let b = 90
  let c = 78
  let d = 92
  let e = 80
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(a, b), c), d), e)
  let average = sum / 5
  
  @azimuth.assert_eq(85, average)  // (85 + 90 + 78 + 92 + 80) / 5 = 425 / 5 = 85
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}
// 用户自定义 MoonBit 测试用例
// 包含10个实用的测试用例，使用标准 MoonBit 测试语法

test "addition_identity_properties" {
  // 测试加法单位元性质
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 测试负数的单位元性质
  @azimuth.assert_eq(-10, @azimuth.add(-10, 0))
  @azimuth.assert_eq(-10, @azimuth.add(0, -10))
}

test "multiplication_identity_properties" {
  // 测试乘法单位元性质
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
  
  // 测试零的性质
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 7))
  
  // 测试负数的单位元性质
  @azimuth.assert_eq(-12, @azimuth.multiply(-12, 1))
  @azimuth.assert_eq(-12, @azimuth.multiply(1, -12))
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
}

test "arithmetic_distributive_property" {
  // 测试分配律: a × (b + c) = a × b + a × c
  let a = 6
  let b = 8
  let c = 4
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
  
  // 测试包含负数的情况
  let d = -3
  let e = 7
  let f = 2
  
  let left_side_neg = @azimuth.multiply(d, @azimuth.add(e, f))
  let right_side_neg = @azimuth.add(@azimuth.multiply(d, e), @azimuth.multiply(d, f))
  
  @azimuth.assert_eq(left_side_neg, right_side_neg)
}

test "integer_division_simulation" {
  // 使用加法和乘法模拟除法运算
  let dividend = 27
  let divisor = 6
  
  // 模拟整数除法：27 ÷ 6 = 4 余 3
  let quotient = 4
  let remainder = 3
  
  // 验证：dividend = divisor × quotient + remainder
  let verification = @azimuth.add(@azimuth.multiply(divisor, quotient), remainder)
  @azimuth.assert_eq(dividend, verification)
  
  // 测试整除情况
  let dividend2 = 24
  let divisor2 = 6
  let quotient2 = 4
  let remainder2 = 0
  
  let verification2 = @azimuth.add(@azimuth.multiply(divisor2, quotient2), remainder2)
  @azimuth.assert_eq(dividend2, verification2)
}

test "shopping_cart_with_discount" {
  // 测试购物车计算与折扣
  let item1_price = 50
  let item1_quantity = 2
  let item2_price = 30
  let item2_quantity = 3
  let discount = 20  // 20元折扣
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 应用折扣
  let final_total = @azimuth.add(subtotal, -discount)
  
  // 验证结果: 50×2 + 30×3 - 20 = 100 + 90 - 20 = 170
  @azimuth.assert_eq(170, final_total)
}

test "simple_interest_calculation" {
  // 测试单利计算：利息 = 本金 × 利率 × 时间
  let principal = 1000
  let rate = 5  // 5%
  let time = 3   // 3年
  
  // 计算利息
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  
  // 计算本息和
  let total = @azimuth.add(principal, interest)
  
  // 验证结果: 1000 + 1000×5×3 = 1000 + 15000 = 16000
  @azimuth.assert_eq(16000, total)
}

test "binary_number_simulation" {
  // 模拟二进制运算：使用加法模拟二进制左移（相当于乘以2的幂）
  let number = 13
  
  // 模拟左移1位：13 << 1 = 26 (13 × 2)
  let left_shift_1 = @azimuth.multiply(number, 2)
  @azimuth.assert_eq(26, left_shift_1)
  
  // 模拟左移2位：13 << 2 = 52 (13 × 4)
  let left_shift_2 = @azimuth.multiply(number, 4)
  @azimuth.assert_eq(52, left_shift_2)
  
  // 模拟左移3位：13 << 3 = 104 (13 × 8)
  let left_shift_3 = @azimuth.multiply(number, 8)
  @azimuth.assert_eq(104, left_shift_3)
}

test "polygon_perimeter_calculation" {
  // 测试多边形周长计算
  // 三角形边长
  let triangle_side1 = 5
  let triangle_side2 = 7
  let triangle_side3 = 9
  
  let triangle_perimeter = @azimuth.add(
    @azimuth.add(triangle_side1, triangle_side2), 
    triangle_side3
  )
  @azimuth.assert_eq(21, triangle_perimeter)
  
  // 四边形边长
  let quad_side1 = 6
  let quad_side2 = 8
  let quad_side3 = 6
  let quad_side4 = 8
  
  let quad_perimeter = @azimuth.add(
    @azimuth.add(
      @azimuth.add(quad_side1, quad_side2), 
      quad_side3
    ), 
    quad_side4
  )
  @azimuth.assert_eq(28, quad_perimeter)
}

test "time_conversion_practical" {
  // 测试时间转换实用场景
  // 将小时、分钟转换为总秒数
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  // 计算总秒数
  let total_seconds = @azimuth.add(
    @azimuth.add(
      @azimuth.multiply(hours, 3600),  // 小时转秒
      @azimuth.multiply(minutes, 60)   // 分钟转秒
    ), 
    seconds
  )
  
  // 验证结果: 2×3600 + 30×60 + 45 = 7200 + 1800 + 45 = 9045
  @azimuth.assert_eq(9045, total_seconds)
  
  // 计算工作周时长（秒）
  let work_days = 5
  let work_hours_per_day = 8
  let work_week_seconds = @azimuth.multiply(work_days, @azimuth.multiply(work_hours_per_day, 3600))
  
  // 验证结果: 5 × 8 × 3600 = 144000
  @azimuth.assert_eq(144000, work_week_seconds)
}
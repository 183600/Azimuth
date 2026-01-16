// 用户自定义的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加法测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 最小值加法测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 极值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "boundary_value_multiplication" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值乘法测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值乘法测试
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_processing_comprehensive" {
  // 测试字符串处理的综合情况
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 长字符串
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  let long_result = @azimuth.greet(long_string)
  @azimuth.assert_eq_string("Hello, ThisIsAVeryLongStringForTestingPurposes!", long_result)
  
  // Unicode 字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法分配律: a × (b + c) = a × b + a × c
  let distrib_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distrib_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distrib_left, distrib_right)
}

test "real_world_financial_calculation" {
  // 实际金融计算场景
  let principal = 10000  // 本金
  let rate = 5           // 利率 5%
  let years = 3          // 年数
  
  // 简单利息计算: 本金 + (本金 × 利率 × 年数)
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)  // 10000 + 10000×5×3 = 160000
  
  // 月供计算（简化版）: 总金额 / (年数 × 12)
  let monthly_payment = total / @azimuth.multiply(years, 12)
  @azimuth.assert_eq(4444, monthly_payment)  // 160000 / 36 ≈ 4444
}

test "geometry_calculations" {
  // 几何计算测试
  let length = 10
  let width = 6
  let height = 4
  
  // 矩形面积
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(60, rectangle_area)
  
  // 矩形周长
  let perimeter = @azimuth.multiply(@azimuth.add(length, width), 2)
  @azimuth.assert_eq(32, perimeter)
  
  // 长方体体积
  let volume = @azimuth.multiply(rectangle_area, height)
  @azimuth.assert_eq(240, volume)
  
  // 表面积
  let surface_area = @azimuth.add(
    @azimuth.multiply(@azimuth.multiply(length, width), 2),
    @azimuth.add(
      @azimuth.multiply(@azimuth.multiply(width, height), 2),
      @azimuth.multiply(@azimuth.multiply(length, height), 2)
    )
  )
  @azimuth.assert_eq(248, surface_area)
}

test "temperature_conversion" {
  // 温度转换测试（简化版）
  // 摄氏度转华氏度: F = C × 2 + 30 (简化公式)
  
  let celsius_0 = 0
  let fahrenheit_0 = @azimuth.add(@azimuth.multiply(celsius_0, 2), 30)
  @azimuth.assert_eq(30, fahrenheit_0)
  
  let celsius_1 = 10
  let fahrenheit_1 = @azimuth.add(@azimuth.multiply(celsius_1, 2), 30)
  @azimuth.assert_eq(50, fahrenheit_1)
  
  let celsius_2 = 25
  let fahrenheit_2 = @azimuth.add(@azimuth.multiply(celsius_2, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_2)
  
  let celsius_3 = 100
  let fahrenheit_3 = @azimuth.add(@azimuth.multiply(celsius_3, 2), 30)
  @azimuth.assert_eq(230, fahrenheit_3)
}

test "statistical_calculations" {
  // 统计计算测试
  let num1 = 15
  let num2 = 25
  let num3 = 35
  let num4 = 45
  let num5 = 55
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), num4), num5)
  @azimuth.assert_eq(175, sum)
  
  // 计算平均值
  let average = sum / 5
  @azimuth.assert_eq(35, average)
  
  // 计算方差（简化版）
  let diff1 = @azimuth.add(num1, -average)
  let diff2 = @azimuth.add(num2, -average)
  let diff3 = @azimuth.add(num3, -average)
  let diff4 = @azimuth.add(num4, -average)
  let diff5 = @azimuth.add(num5, -average)
  
  let sq_diff1 = @azimuth.multiply(diff1, diff1)
  let sq_diff2 = @azimuth.multiply(diff2, diff2)
  let sq_diff3 = @azimuth.multiply(diff3, diff3)
  let sq_diff4 = @azimuth.multiply(diff4, diff4)
  let sq_diff5 = @azimuth.multiply(diff5, diff5)
  
  let variance = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(sq_diff1, sq_diff2), sq_diff3), sq_diff4), sq_diff5) / 5
  @azimuth.assert_eq(200, variance)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let n = 10
  
  // O(1) - 常数时间
  let constant_time = 42
  @azimuth.assert_eq(42, constant_time)
  
  // O(n) - 线性时间: 1 + 2 + ... + n = n × (n + 1) / 2
  let linear_sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, linear_sum)
  
  // O(n²) - 平方时间: 1² + 2² + ... + n²
  let square1 = @azimuth.multiply(1, 1)
  let square2 = @azimuth.multiply(2, 2)
  let square3 = @azimuth.multiply(3, 3)
  let square4 = @azimuth.multiply(4, 4)
  let square5 = @azimuth.multiply(5, 5)
  let square6 = @azimuth.multiply(6, 6)
  let square7 = @azimuth.multiply(7, 7)
  let square8 = @azimuth.multiply(8, 8)
  let square9 = @azimuth.multiply(9, 9)
  let square10 = @azimuth.multiply(10, 10)
  
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(square1, square2), square3), square4), square5), @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(square6, square7), square8), square9), square10))
  @azimuth.assert_eq(385, square_sum)
}
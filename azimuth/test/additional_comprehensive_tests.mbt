test "comprehensive_add_test" {
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "comprehensive_multiply_test" {
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "comprehensive_greet_test" {
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "negative_addition_comprehensive" {
  // 测试两个负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  
  // 测试负数与正数相加
  @azimuth.assert_eq(-2, @azimuth.add(-10, 8))
  @azimuth.assert_eq(5, @azimuth.add(-3, 8))
  
  // 测试对称性
  @azimuth.assert_eq(@azimuth.add(-5, 10), @azimuth.add(10, -5))
}

test "negative_multiplication_comprehensive" {
  // 测试两个负数相乘得正数
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  
  // 测试负数与正数相乘得负数
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
  
  // 测试乘法分配律 (a * (b + c) = a * b + a * c)
  let a = 3
  let b = 4
  let c = 5
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "string_greet_edge_cases" {
  // 测试非常长的字符串
  let long_name = "ThisIsAVeryLongNameThatCouldPotentiallyCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  let result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, " + long_name + "!", result)
  
  // 测试包含引号的字符串
  @azimuth.assert_eq_string("Hello, \"John\"!", @azimuth.greet("\"John\""))
  
  // 测试包含反斜杠的字符串
  @azimuth.assert_eq_string("Hello, C:\\Users\\Test!", @azimuth.greet("C:\\Users\\Test"))
}

test "mathematical_function_combination" {
  // 测试多项式计算：2x² + 3x + 1，当 x = 5 时
  let x = 5
  let x_squared = @azimuth.multiply(x, x)  // x²
  let term1 = @azimuth.multiply(2, x_squared)  // 2x²
  let term2 = @azimuth.multiply(3, x)  // 3x
  let result = @azimuth.add(@azimuth.add(term1, term2), 1)  // 2x² + 3x + 1
  @azimuth.assert_eq(66, result)  // 2*25 + 3*5 + 1 = 50 + 15 + 1 = 66
  
  // 测试几何计算：计算梯形面积 (上底 + 下底) * 高 / 2
  let top_base = 5
  let bottom_base = 10
  let height = 4
  let area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), height) / 2
  @azimuth.assert_eq(30, area)  // (5 + 10) * 4 / 2 = 15 * 4 / 2 = 30
}

test "temperature_conversion_scenario" {
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)
  @azimuth.assert_eq(77, fahrenheit)  // 25 * 9/5 + 32 = 45 + 32 = 77
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let fahrenheit2 = 77
  let celsius2 = @azimuth.multiply(fahrenheit2 - 32, 5) / 9
  @azimuth.assert_eq(25, celsius2)  // (77 - 32) * 5/9 = 45 * 5/9 = 25
}

test "data_type_boundary_conditions" {
  // 测试连续运算的边界情况
  let max_val = 2147483647
  
  // 测试达到最大值后继续运算
  let result1 = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, result1)  // 应该饱和到最大值
  
  // 测试从最大值减法
  let result2 = max_val - 100
  @azimuth.assert_eq(2147483547, result2)
  
  // 测试最小值的特殊情况
  let min_val = -2147483648
  let result3 = @azimuth.add(min_val, 1)
  @azimuth.assert_eq(-2147483647, result3)
}
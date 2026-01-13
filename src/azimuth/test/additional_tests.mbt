// 额外的 MoonBit 测试用例
// 这些测试用例涵盖了边界条件、实际应用场景和特殊输入情况

test "add_zero_properties" {
  // 测试加法的零元素性质：a + 0 = a 和 0 + a = a
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_symmetric_property" {
  // 测试加法的对称性质：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  @azimuth.assert_eq(@azimuth.add(-15, 27), @azimuth.add(27, -15))
  @azimuth.assert_eq(@azimuth.add(-15, -27), @azimuth.add(-27, -15))
}

test "multiply_distributive_property" {
  // 测试乘法对加法的分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 3
  let c = 7
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "multiply_identity_element" {
  // 测试乘法的单位元：a * 1 = a 和 1 * a = a
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(1, -17))
}

test "multiply_zero_property" {
  // 测试乘法的零元素性质：a * 0 = 0 和 0 * a = 0
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(-17, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -17))
}

test "greet_empty_and_whitespace" {
  // 测试空字符串和各种空白字符的问候
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \t!", @azimuth.greet("\t"))
  @azimuth.assert_eq_string("Hello, \n!", @azimuth.greet("\n"))
}

test "greet_numeric_and_special_chars" {
  // 测试数字和特殊字符的问候
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
  @azimuth.assert_eq_string("Hello, C++!", @azimuth.greet("C++"))
  @azimuth.assert_eq_string("Hello, a+b=c!", @azimuth.greet("a+b=c"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算 (a+b)² - (a-b)² = 4ab
  let a = 5
  let b = 3
  
  let sum = @azimuth.add(a, b)
  let diff = @azimuth.add(a, -b)
  
  let sum_squared = @azimuth.multiply(sum, sum)
  let diff_squared = @azimuth.multiply(diff, diff)
  
  let result = @azimuth.add(sum_squared, -diff_squared)
  let expected = @azimuth.multiply(4, @azimuth.multiply(a, b))
  
  @azimuth.assert_eq(expected, result)
}

test "financial_calculation_compound_interest" {
  // 测试金融计算：复利计算（简化版）
  // 公式：A = P(1 + r/n)^(nt)，这里简化为 A = P + P*r*t
  let principal = 1000  // 本金
  let rate = 5  // 年利率（百分比）
  let time = 3  // 年数
  
  // 计算利息：P * r * t / 100
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), time) / 100
  // 计算总额：P + interest
  let total = @azimuth.add(principal, interest)
  
  // 验证结果：1000 + 1000*5*3/100 = 1000 + 150 = 1150
  @azimuth.assert_eq(1150, total)
}

test "physics_calculation_velocity" {
  // 测试物理计算：速度和距离
  // 公式：距离 = 速度 × 时间
  let initial_velocity = 10  // 初始速度 (m/s)
  let acceleration = 2  // 加速度 (m/s²)
  let time = 5  // 时间 (s)
  
  // 计算最终速度：v = u + at
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(acceleration, time))
  // 计算平均速度：(u + v) / 2
  let average_velocity = @azimuth.add(initial_velocity, final_velocity) / 2
  // 计算距离：s = v_avg × t
  let distance = @azimuth.multiply(average_velocity, time)
  
  // 验证结果：v = 10 + 2*5 = 20, v_avg = (10+20)/2 = 15, s = 15*5 = 75
  @azimuth.assert_eq(20, final_velocity)
  @azimuth.assert_eq(75, distance)
}
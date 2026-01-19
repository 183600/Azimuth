// Azimuth 项目高质量 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含多种测试场景

test "financial_compound_interest" {
  // 测试复利计算：本金 × (1 + 利率)^年数
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  let growth_factor = @azimuth.add(100, @azimuth.multiply(rate_percent, years))
  let total_amount = @azimuth.multiply(principal, growth_factor)
  @azimuth.assert_eq(1150000, total_amount)
}

test "physics_kinematics" {
  // 测试物理学运动学公式：s = ut + 1/2at²
  let initial_velocity = 10
  let acceleration = 2
  let time = 5
  
  let ut = @azimuth.multiply(initial_velocity, time)
  let half_at_squared = @azimuth.multiply(acceleration, @azimuth.multiply(time, time)) / 2
  let displacement = @azimuth.add(ut, half_at_squared)
  
  @azimuth.assert_eq(75, displacement)
}

test "geometry_pythagorean_theorem" {
  // 测试勾股定理：a² + b² = c²
  let a = 3
  let b = 4
  
  let a_squared = @azimuth.multiply(a, a)
  let b_squared = @azimuth.multiply(b, b)
  let c_squared = @azimuth.add(a_squared, b_squared)
  
  let c = 5
  let c_squared_expected = @azimuth.multiply(c, c)
  
  @azimuth.assert_eq(c_squared_expected, c_squared)
}

test "string_processing_edge_cases" {
  // 测试字符串处理
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result1 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result1)
  
  let unicode_chars = "测试🌍123"
  let result2 = @azimuth.greet(unicode_chars)
  @azimuth.assert_eq_string("Hello, 测试🌍123!", result2)
  
  let empty_string = ""
  let result3 = @azimuth.greet(empty_string)
  @azimuth.assert_eq_string("Hello, !", result3)
}

test "error_boundary_comprehensive" {
  // 测试错误边界
  let max_val = 2147483647
  let min_val = -2147483648
  
  let overflow1 = @azimuth.add(max_val, 1)
  let overflow2 = @azimuth.add(overflow1, 100)
  @azimuth.assert_eq(max_val, overflow2)
  
  let underflow1 = @azimuth.add(min_val, -1)
  let underflow2 = @azimuth.add(underflow1, -100)
  @azimuth.assert_eq(min_val, underflow2)
  
  let mixed1 = @azimuth.multiply(max_val, 2)
  let mixed2 = @azimuth.add(mixed1, 1000)
  @azimuth.assert_eq(max_val, mixed2)
  
  let extreme_sum = @azimuth.add(max_val, min_val)
  let extreme_product = @azimuth.multiply(extreme_sum, -1)
  @azimuth.assert_eq(1, extreme_product)
}
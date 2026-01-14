// MoonBit 测试套件
// 标准的 MoonBit 测试用例，覆盖核心功能和边界情况

test "add_zero_handling" {
  // 测试加法的零值处理
  @azimuth.assert_eq(10, @azimuth.add(0, 10))
  @azimuth.assert_eq(10, @azimuth.add(10, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(0, -5))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
}

test "multiply_identity_elements" {
  // 测试乘法的单位元素
  @azimuth.assert_eq(15, @azimuth.multiply(15, 1))
  @azimuth.assert_eq(15, @azimuth.multiply(1, 15))
  @azimuth.assert_eq(-8, @azimuth.multiply(-8, 1))
  @azimuth.assert_eq(-8, @azimuth.multiply(1, -8))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
}

test "multiply_negative_one" {
  // 测试乘以-1的情况
  @azimuth.assert_eq(-7, @azimuth.multiply(7, -1))
  @azimuth.assert_eq(-7, @azimuth.multiply(-1, 7))
  @azimuth.assert_eq(7, @azimuth.multiply(-7, -1))
  @azimuth.assert_eq(7, @azimuth.multiply(-1, -7))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候功能
  @azimuth.assert_eq_string("Hello, @user!", @azimuth.greet("@user"))
  @azimuth.assert_eq_string("Hello, test-name!", @azimuth.greet("test-name"))
  @azimuth.assert_eq_string("Hello, user_123!", @azimuth.greet("user_123"))
  @azimuth.assert_eq_string("Hello, user@email.com!", @azimuth.greet("user@email.com"))
}

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-15, @azimuth.add(-20, 5))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  @azimuth.assert_eq(0, @azimuth.add(-8, 8))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(-20, @azimuth.multiply(4, -5))
  @azimuth.assert_eq(-20, @azimuth.multiply(-4, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正溢出测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 10))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(max_val, @azimuth.add(1000000000, 1500000000))
  
  // 负溢出测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -10))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.add(-1000000000, -1500000000))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正溢出测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(100000, 30000))
  
  // 负溢出测试
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-100000, 30000))
  
  // 最小值特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let test_cases = [
    (5, 10),
    (-3, 7),
    (-8, -12),
    (0, 15),
    (100, -50)
  ]
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(5, 10), @azimuth.add(10, 5))
  @azimuth.assert_eq(@azimuth.add(-3, 7), @azimuth.add(7, -3))
  @azimuth.assert_eq(@azimuth.add(-8, -12), @azimuth.add(-12, -8))
  @azimuth.assert_eq(@azimuth.add(0, 15), @azimuth.add(15, 0))
  @azimuth.assert_eq(@azimuth.add(100, -50), @azimuth.add(-50, 100))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(5, 10), @azimuth.multiply(10, 5))
  @azimuth.assert_eq(@azimuth.multiply(-3, 7), @azimuth.multiply(7, -3))
  @azimuth.assert_eq(@azimuth.multiply(-8, -12), @azimuth.multiply(-12, -8))
  @azimuth.assert_eq(@azimuth.multiply(0, 15), @azimuth.multiply(15, 0))
  @azimuth.assert_eq(@azimuth.multiply(100, -50), @azimuth.multiply(-50, 100))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 场景：计算银行复利
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  
  // 简化的复利计算：本金 × (1 + 利率 × 年数)
  let simple_interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years)) / 100
  let total_amount = @azimuth.add(principal, simple_interest)
  @azimuth.assert_eq(1150, total_amount)
  
  // 场景：计算平均速度
  let distance = 300
  let time = 5
  let average_speed = distance / time
  @azimuth.assert_eq(60, average_speed)
  
  // 场景：计算面积差
  let large_square_area = @azimuth.multiply(10, 10)
  let small_square_area = @azimuth.multiply(6, 6)
  let area_difference = @azimuth.add(large_square_area, -small_square_area)
  @azimuth.assert_eq(64, area_difference)
}
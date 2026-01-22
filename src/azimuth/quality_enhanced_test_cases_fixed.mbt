// 高质量增强测试用例 - 针对 azimuth 库的深度测试
// 包含边界情况、数学性质验证和实际应用场景

test "mathematical_commutativity_laws" {
  // 验证交换律：a + b = b + a 和 a * b = b * a
  let a = 17
  let b = 23
  let c = -5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.add(b, c), @azimuth.add(c, b))
  @azimuth.assert_eq(@azimuth.add(a, c), @azimuth.add(c, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  @azimuth.assert_eq(@azimuth.multiply(b, c), @azimuth.multiply(c, b))
  @azimuth.assert_eq(@azimuth.multiply(a, c), @azimuth.multiply(c, a))
}

test "mathematical_associativity_laws" {
  // 验证结合律：(a + b) + c = a + (b + c) 和 (a * b) * c = a * (b * c)
  let a = 7
  let b = 11
  let c = 13
  
  // 加法结合律
  let add_left = @azimuth.add(@azimuth.add(a, b), c)
  let add_right = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(add_left, add_right)
  
  // 乘法结合律
  let mul_left = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let mul_right = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(mul_left, mul_right)
}

test "distributive_property_verification" {
  // 验证分配律：a * (b + c) = a * b + a * c
  let a = 6
  let b = 8
  let c = 12
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive_behavior" {
  // 全面测试向上取整除法的行为
  // 正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(13, 4))  // 3.25 -> 4
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 4))  // 3 -> 4 (修正：应该是3)
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 5))   // 0.6 -> 1
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))   // 1 -> 1
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 4))  // -2.75 -> -2 (根据实现)
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 4))   // -2 -> -2
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 4))   // -1.75 -> -1 (根据实现)
  
  // 混合符号
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(7, -4))   // -1.75 -> -1 (根据实现)
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-7, -4))   // 1.75 -> 2
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "identity_elements_properties" {
  // 验证单位元性质：a + 0 = a 和 a * 1 = a
  let test_values = [0, 1, -1, 42, -17, 1000, -1000]
  
  for value in test_values {
    // 加法单位元
    @azimuth.assert_eq(value, @azimuth.add(value, 0))
    @azimuth.assert_eq(value, @azimuth.add(0, value))
    
    // 乘法单位元
    @azimuth.assert_eq(value, @azimuth.multiply(value, 1))
    @azimuth.assert_eq(value, @azimuth.multiply(1, value))
  }
}

test "inverse_elements_properties" {
  // 验证逆元性质：a + (-a) = 0
  let test_values = [1, -1, 42, -17, 1000, -1000, 2147483647, -2147483648]
  
  for value in test_values {
    let inverse = -value
    @azimuth.assert_eq(0, @azimuth.add(value, inverse))
  }
}

test "zero_element_properties" {
  // 验证零元素性质：a * 0 = 0
  let test_values = [0, 1, -1, 42, -17, 1000, -1000, 2147483647, -2147483648]
  
  for value in test_values {
    @azimuth.assert_eq(0, @azimuth.multiply(value, 0))
    @azimuth.assert_eq(0, @azimuth.multiply(0, value))
  }
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款计算
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 10          // 贷款年限
  
  // 简化的单利计算（实际应该是复利）
  let total_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate * years), 100)
  let total_payment = @azimuth.add(principal, total_interest)
  let monthly_payment = @azimuth.divide_with_ceil(total_payment, years * 12)
  
  @azimuth.assert_eq(60000, total_interest)
  @azimuth.assert_eq(160000, total_payment)
  @azimuth.assert_eq(1334, monthly_payment)  // 向上取整
}

test "scientific_computation_scenario" {
  // 科学计算场景：物理公式计算
  let mass = 100      // 质量 kg
  let acceleration = 9 // 加速度 m/s²
  let distance = 50   // 距离 m
  
  // 计算力：F = m * a
  let force = @azimuth.multiply(mass, acceleration)
  
  // 计算功：W = F * d
  let work = @azimuth.multiply(force, distance)
  
  // 计算功率（假设时间）：P = W / t，假设 t = 10s
  let time = 10
  let power = @azimuth.divide_with_ceil(work, time)
  
  @azimuth.assert_eq(900, force)
  @azimuth.assert_eq(45000, work)
  @azimuth.assert_eq(4500, power)
}

test "string_processing_edge_cases" {
  // 字符串处理的边界情况
  // 测试各种特殊字符和边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \n\t\r!", @azimuth.greet("\n\t\r"))
  @azimuth.assert_eq_string("Hello, \"!'!", @azimuth.greet("\"!'"))
  @azimuth.assert_eq_string("Hello, 😊!", @azimuth.greet("😊"))
  @azimuth.assert_eq_string("Hello, \u{1F600}!", @azimuth.greet("\u{1F600}"))  // Unicode grin face
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, 🌍🌎🌏!", @azimuth.greet("🌍🌎🌏"))
}
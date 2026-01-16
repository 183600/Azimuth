// Azimuth 项目增强测试套件
// 使用标准 MoonBit 测试语法，提供全面的功能测试覆盖

test "add_function_comprehensive" {
  // 加法函数综合测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(-2, @azimuth.add(5, -7))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(1, @azimuth.add(2147483647, -2147483646))
}

test "multiply_function_comprehensive" {
  // 乘法函数综合测试
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(36, @azimuth.multiply(-6, -6))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
}

test "greet_function_edge_cases" {
  // 问候函数边缘情况测试
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_commutativity" {
  // 数学交换律测试
  let test_cases = [(2, 3), (-5, 7), (0, 10), (-8, -3)]
  for (a, b) in test_cases {
    @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
    @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  }
}

test "mathematical_associativity" {
  // 数学结合律测试
  let a = 4
  let b = 6
  let c = 8
  
  // 加法结合律
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.add(a, b), c),
    @azimuth.add(a, @azimuth.add(b, c))
  )
  
  // 乘法结合律
  @azimuth.assert_eq(
    @azimuth.multiply(@azimuth.multiply(a, b), c),
    @azimuth.multiply(a, @azimuth.multiply(b, c))
  )
}

test "distributive_property" {
  // 分配律测试: a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "identity_elements" {
  // 单位元测试
  let test_values = [-10, -1, 0, 1, 5, 100]
  
  for value in test_values {
    // 加法单位元: a + 0 = a
    @azimuth.assert_eq(value, @azimuth.add(value, 0))
    @azimuth.assert_eq(value, @azimuth.add(0, value))
    
    // 乘法单位元: a * 1 = a
    @azimuth.assert_eq(value, @azimuth.multiply(value, 1))
    @azimuth.assert_eq(value, @azimuth.multiply(1, value))
  }
}

test "inverse_elements" {
  // 逆元测试
  let test_values = [-5, -1, 1, 5, 10]
  
  for value in test_values {
    // 加法逆元: a + (-a) = 0
    @azimuth.assert_eq(0, @azimuth.add(value, -value))
    
    // 零元素: a * 0 = 0
    @azimuth.assert_eq(0, @azimuth.multiply(value, 0))
    @azimuth.assert_eq(0, @azimuth.multiply(0, value))
  }
}

test "complex_calculations" {
  // 复杂计算测试
  
  // 计算表达式: (2 + 3) * (4 + 5) = 5 * 9 = 45
  let result1 = @azimuth.multiply(
    @azimuth.add(2, 3),
    @azimuth.add(4, 5)
  )
  @azimuth.assert_eq(45, result1)
  
  // 计算表达式: (10 * 2) + (15 * 3) = 20 + 45 = 65
  let result2 = @azimuth.add(
    @azimuth.multiply(10, 2),
    @azimuth.multiply(15, 3)
  )
  @azimuth.assert_eq(65, result2)
  
  // 计算平方和: 6² + 8² = 36 + 64 = 100
  let result3 = @azimuth.add(
    @azimuth.multiply(6, 6),
    @azimuth.multiply(8, 8)
  )
  @azimuth.assert_eq(100, result3)
}
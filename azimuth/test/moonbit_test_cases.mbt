// MoonBit 测试用例集合
// 测试 azimuth 包中的核心功能和数学特性

// 测试加法交换律
test "add_commutative_property" {
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  @azimuth.assert_eq(@azimuth.add(-10, 20), @azimuth.add(20, -10))
  @azimuth.assert_eq(@azimuth.add(0, 100), @azimuth.add(100, 0))
}

// 测试乘法交换律
test "multiply_commutative_property" {
  @azimuth.assert_eq(@azimuth.multiply(6, 7), @azimuth.multiply(7, 6))
  @azimuth.assert_eq(@azimuth.multiply(-3, 8), @azimuth.multiply(8, -3))
  @azimuth.assert_eq(@azimuth.multiply(0, 50), @azimuth.multiply(50, 0))
}

// 测试加法结合律
test "add_associative_property" {
  let a = 10
  let b = 20
  let c = 30
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
  
  // 测试负数情况
  let x = -15
  let y = 25
  let z = -10
  let result3 = @azimuth.add(@azimuth.add(x, y), z)
  let result4 = @azimuth.add(x, @azimuth.add(y, z))
  @azimuth.assert_eq(result3, result4)
}

// 测试乘法结合律
test "multiply_associative_property" {
  let a = 2
  let b = 3
  let c = 4
  let result1 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result2 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(result1, result2)
}

// 测试分配律
test "distributive_property" {
  let a = 5
  let b = 7
  let c = 3
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

// 测试边界值的特殊组合
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试接近边界值的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

// 测试字符串处理的边界情况
test "string_edge_cases" {
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, \t\n!", @azimuth.greet("\t\n"))
  @azimuth.assert_eq_string("Hello, \"quotes\"!", @azimuth.greet("\"quotes\""))
  
  // 测试长字符串
  let long_string = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  let expected = "Hello, " + long_string + "!"
  @azimuth.assert_eq_string(expected, @azimuth.greet(long_string))
}

// 测试数学恒等式
test "mathematical_identities" {
  // 加法恒等式
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  
  // 乘法恒等式
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  
  // 负数恒等式
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  @azimuth.assert_eq(-42, @azimuth.multiply(-1, 42))
}

// 测试复杂表达式计算
test "complex_expression_evaluation" {
  // 嵌套运算
  let result1 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result1)
  
  // 多层嵌套
  let result2 = @azimuth.multiply(@azimuth.add(10, 5), @azimuth.add(3, 2))
  @azimuth.assert_eq(75, result2)
  
  // 混合正负数
  let result3 = @azimuth.add(@azimuth.multiply(-2, 3), @azimuth.multiply(4, -5))
  @azimuth.assert_eq(-26, result3)
}

// 测试错误处理场景
test "error_handling_scenarios" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试最小值乘法特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}
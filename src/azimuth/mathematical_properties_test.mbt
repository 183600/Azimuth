// 新增的测试用例 - 数学性质和特殊场景测试

///|
test "add_commutative_property" {
  // 测试加法交换律: a + b = b + a
  assert_eq(@azimuth.add(5, 3), @azimuth.add(3, 5))
  assert_eq(@azimuth.add(-5, 3), @azimuth.add(3, -5))
  assert_eq(@azimuth.add(-5, -3), @azimuth.add(-3, -5))
  assert_eq(@azimuth.add(0, 100), @azimuth.add(100, 0))
  assert_eq(@azimuth.add(2147483647, -2147483648), @azimuth.add(-2147483648, 2147483647))
}

///|
test "add_associative_property" {
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let a = 10
  let b = 20
  let c = 30
  assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 负数测试
  let a = -10
  let b = -20
  let c = -30
  assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 混合符号测试
  let a = 100
  let b = -50
  let c = 25
  assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
}

///|
test "multiply_commutative_property" {
  // 测试乘法交换律: a * b = b * a
  assert_eq(@azimuth.multiply(5, 3), @azimuth.multiply(3, 5))
  assert_eq(@azimuth.multiply(-5, 3), @azimuth.multiply(3, -5))
  assert_eq(@azimuth.multiply(-5, -3), @azimuth.multiply(-3, -5))
  assert_eq(@azimuth.multiply(0, 100), @azimuth.multiply(100, 0))
  assert_eq(@azimuth.multiply(2147483647, -1), @azimuth.multiply(-1, 2147483647))
}

///|
test "multiply_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 3
  let c = 2
  assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
  
  // 负数测试
  let a = -5
  let b = 3
  let c = 2
  assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
  
  // 混合符号测试
  let a = 5
  let b = -3
  let c = 2
  assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

///|
test "identity_elements" {
  // 测试单位元：加法单位元是0，乘法单位元是1
  assert_eq(5, @azimuth.add(5, 0))
  assert_eq(5, @azimuth.add(0, 5))
  assert_eq(-5, @azimuth.add(-5, 0))
  assert_eq(-5, @azimuth.add(0, -5))
  
  assert_eq(5, @azimuth.multiply(5, 1))
  assert_eq(5, @azimuth.multiply(1, 5))
  assert_eq(-5, @azimuth.multiply(-5, 1))
  assert_eq(-5, @azimuth.multiply(1, -5))
}

///|
test "greet_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq("Hello, @#$%!", @azimuth.greet("@#$%"))
  assert_eq("Hello, 中文测试!", @azimuth.greet("中文测试"))
  assert_eq("Hello,   !", @azimuth.greet("  "))  // 空格
  assert_eq("Hello, \n\t!", @azimuth.greet("\n\t"))  // 换行和制表符
  assert_eq("Hello, 🌙!", @azimuth.greet("🌙"))  // emoji
}

///|
test "add_sequence_pattern" {
  // 测试序列模式：连续加法
  let result = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  assert_eq(10, result)
  
  // 负数序列
  let result = @azimuth.add(@azimuth.add(-1, -2), @azimuth.add(-3, -4))
  assert_eq(-10, result)
  
  // 混合序列
  let result = @azimuth.add(@azimuth.add(100, -50), @azimuth.add(25, -75))
  assert_eq(0, result)
}

///|
test "multiply_power_pattern" {
  // 测试幂模式：重复乘法
  let base = 2
  let result = @azimuth.multiply(@azimuth.multiply(base, base), @azimuth.multiply(base, base))
  assert_eq(16, result)  // 2^4 = 16
  
  // 负数幂
  let base = -2
  let result = @azimuth.multiply(@azimuth.multiply(base, base), @azimuth.multiply(base, base))
  assert_eq(16, result)  // (-2)^4 = 16
  
  // 零幂
  let base = 0
  let result = @azimuth.multiply(@azimuth.multiply(base, base), @azimuth.multiply(base, base))
  assert_eq(0, result)  // 0^4 = 0
}

///|
test "extreme_boundary_combinations" {
  // 测试极端边界值的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算组合
  assert_eq(-1, @azimuth.add(max_val, min_val))
  assert_eq(-1, @azimuth.add(min_val, max_val))
  assert_eq(min_val, @azimuth.multiply(max_val, min_val))
  assert_eq(min_val, @azimuth.multiply(min_val, max_val))
  
  // 边界值与自身的运算
  assert_eq(max_val, @azimuth.add(max_val, 0))
  assert_eq(min_val, @azimuth.add(min_val, 0))
  assert_eq(max_val, @azimuth.multiply(max_val, 1))
  assert_eq(min_val, @azimuth.multiply(min_val, 1))
}
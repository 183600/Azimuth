// 新增的MoonBit测试用例
// 包含各种边界条件和特殊情况的测试

///|
test "add_symmetry_properties" {
  // 测试加法交换律
  assert_eq(@azimuth.add(5, 10), @azimuth.add(10, 5))
  assert_eq(@azimuth.add(-5, 10), @azimuth.add(10, -5))
  assert_eq(@azimuth.add(-5, -10), @azimuth.add(-10, -5))
  assert_eq(@azimuth.add(2147483647, -2147483648), @azimuth.add(-2147483648, 2147483647))
}

///|
test "add_associativity_with_overflow" {
  // 测试加法结合律（考虑溢出）
  assert_eq(@azimuth.add(@azimuth.add(1, 2), 3), @azimuth.add(1, @azimuth.add(2, 3)))
  assert_eq(@azimuth.add(@azimuth.add(-1, -2), -3), @azimuth.add(-1, @azimuth.add(-2, -3)))
  // 溢出情况下的结合律测试
  assert_eq(@azimuth.add(@azimuth.add(2147483647, 1), 1), @azimuth.add(2147483647, @azimuth.add(1, 1)))
}

///|
test "multiply_distributive_property" {
  // 测试乘法分配律
  assert_eq(@azimuth.multiply(5, @azimuth.add(2, 3)), @azimuth.add(@azimuth.multiply(5, 2), @azimuth.multiply(5, 3)))
  assert_eq(@azimuth.multiply(-2, @azimuth.add(3, 4)), @azimuth.add(@azimuth.multiply(-2, 3), @azimuth.multiply(-2, 4)))
}

///|
test "multiply_identity_elements" {
  // 测试乘法单位元
  assert_eq(5, @azimuth.multiply(5, 1))
  assert_eq(5, @azimuth.multiply(1, 5))
  assert_eq(-5, @azimuth.multiply(-5, 1))
  assert_eq(-5, @azimuth.multiply(1, -5))
  assert_eq(2147483647, @azimuth.multiply(2147483647, 1))
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
}

///|
test "greet_unicode_and_special_chars" {
  // 测试包含Unicode字符和特殊字符的字符串
  assert_eq("Hello, 🌙!", @azimuth.greet("🌙"))
  assert_eq("Hello, αβγ!", @azimuth.greet("αβγ"))
  assert_eq("Hello, 中文!", @azimuth.greet("中文"))
  assert_eq("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  assert_eq("Hello, \n\t!", @azimuth.greet("\n\t"))
}

///|
test "add_extreme_value_sequences" {
  // 测试极值序列
  let values = [0, 1, -1, 2147483647, -2147483648]
  for i = 0; i < values.length(); i = i + 1 {
    for j = 0; j < values.length(); j = j + 1 {
      let result = @azimuth.add(values[i], values[j])
      // 验证结果在合理范围内
      assert_true(result >= -2147483648 && result <= 2147483647)
    }
  }
}

///|
test "multiply_power_of_two_patterns" {
  // 测试2的幂次方模式
  assert_eq(2, @azimuth.multiply(1, 2))
  assert_eq(4, @azimuth.multiply(2, 2))
  assert_eq(8, @azimuth.multiply(4, 2))
  assert_eq(16, @azimuth.multiply(8, 2))
  assert_eq(32, @azimuth.multiply(16, 2))
  assert_eq(64, @azimuth.multiply(32, 2))
  assert_eq(128, @azimuth.multiply(64, 2))
  assert_eq(256, @azimuth.multiply(128, 2))
}

///|
test "multiply_negative_power_patterns" {
  // 测试负数的幂次方模式
  assert_eq(-2, @azimuth.multiply(-1, 2))
  assert_eq(4, @azimuth.multiply(-2, -2))
  assert_eq(-8, @azimuth.multiply(4, -2))
  assert_eq(16, @azimuth.multiply(-8, -2))
  assert_eq(-32, @azimuth.multiply(16, -2))
  assert_eq(64, @azimuth.multiply(-32, -2))
}

///|
test "combined_operations_complex" {
  // 复合运算测试
  let x = 10
  let y = -5
  let z = 3
  
  // (x + y) * z
  let result1 = @azimuth.multiply(@azimuth.add(x, y), z)
  assert_eq(15, result1)
  
  // x * y + z
  let result2 = @azimuth.add(@azimuth.multiply(x, y), z)
  assert_eq(-47, result2)
  
  // (x + z) * (y + z)
  let result3 = @azimuth.multiply(@azimuth.add(x, z), @azimuth.add(y, z))
  assert_eq(26, result3)
}
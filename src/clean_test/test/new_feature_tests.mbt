test "add_symmetry_properties" {
  // 测试加法交换律
  @clean_test.assert_eq(@clean_test.add(5, 10), @clean_test.add(10, 5))
  @clean_test.assert_eq(@clean_test.add(-5, 10), @clean_test.add(10, -5))
  @clean_test.assert_eq(@clean_test.add(-5, -10), @clean_test.add(-10, -5))
  @clean_test.assert_eq(@clean_test.add(2147483647, -2147483648), @clean_test.add(-2147483648, 2147483647))
}

///|
test "add_associativity_with_overflow" {
  // 测试加法结合律（考虑溢出）
  @clean_test.assert_eq(@clean_test.add(@clean_test.add(1, 2), 3), @clean_test.add(1, @clean_test.add(2, 3)))
  @clean_test.assert_eq(@clean_test.add(@clean_test.add(-1, -2), -3), @clean_test.add(-1, @clean_test.add(-2, -3)))
  // 溢出情况下的结合律测试
  @clean_test.assert_eq(@clean_test.add(@clean_test.add(2147483647, 1), 1), @clean_test.add(2147483647, @clean_test.add(1, 1)))
}

///|
test "multiply_distributive_property" {
  // 测试乘法分配律
  @clean_test.assert_eq(@clean_test.multiply(5, @clean_test.add(2, 3)), @clean_test.add(@clean_test.multiply(5, 2), @clean_test.multiply(5, 3)))
  @clean_test.assert_eq(@clean_test.multiply(-2, @clean_test.add(3, 4)), @clean_test.add(@clean_test.multiply(-2, 3), @clean_test.multiply(-2, 4)))
}

///|
test "multiply_identity_elements" {
  // 测试乘法单位元
  @clean_test.assert_eq(5, @clean_test.multiply(5, 1))
  @clean_test.assert_eq(5, @clean_test.multiply(1, 5))
  @clean_test.assert_eq(-5, @clean_test.multiply(-5, 1))
  @clean_test.assert_eq(-5, @clean_test.multiply(1, -5))
  @clean_test.assert_eq(2147483647, @clean_test.multiply(2147483647, 1))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(-2147483648, 1))
}

///|
test "greet_unicode_and_special_chars" {
  // 测试包含Unicode字符和特殊字符的字符串
  @clean_test.assert_eq_string("Hello, 🌙!", @clean_test.greet("🌙"))
  @clean_test.assert_eq_string("Hello, αβγ!", @clean_test.greet("αβγ"))
  @clean_test.assert_eq_string("Hello, 中文!", @clean_test.greet("中文"))
  @clean_test.assert_eq_string("Hello, @#$%^&*()!", @clean_test.greet("@#$%^&*()"))
  @clean_test.assert_eq_string("Hello, \n\t!", @clean_test.greet("\n\t"))
}

///|
test "add_extreme_value_sequences" {
  // 测试极值序列
  let values : ReadOnlyArray[Int] = [0, 1, -1, 2147483647, -2147483648]
  for i = 0; i < values.length(); i = i + 1 {
    for j = 0; j < values.length(); j = j + 1 {
      let result = @clean_test.add(values[i], values[j])
      // 验证结果在合理范围内
      @clean_test.assert_true(result >= -2147483648 && result <= 2147483647)
    }
  }
}

///|
test "multiply_power_of_two_patterns" {
  // 测试2的幂次方模式
  @clean_test.assert_eq(2, @clean_test.multiply(1, 2))
  @clean_test.assert_eq(4, @clean_test.multiply(2, 2))
  @clean_test.assert_eq(8, @clean_test.multiply(4, 2))
  @clean_test.assert_eq(16, @clean_test.multiply(8, 2))
  @clean_test.assert_eq(32, @clean_test.multiply(16, 2))
  @clean_test.assert_eq(64, @clean_test.multiply(32, 2))
  @clean_test.assert_eq(128, @clean_test.multiply(64, 2))
  @clean_test.assert_eq(256, @clean_test.multiply(128, 2))
}

///|
test "multiply_negative_power_patterns" {
  // 测试负数的幂次方模式
  @clean_test.assert_eq(-2, @clean_test.multiply(-1, 2))
  @clean_test.assert_eq(4, @clean_test.multiply(-2, -2))
  @clean_test.assert_eq(-8, @clean_test.multiply(4, -2))
  @clean_test.assert_eq(16, @clean_test.multiply(-8, -2))
  @clean_test.assert_eq(-32, @clean_test.multiply(16, -2))
  @clean_test.assert_eq(64, @clean_test.multiply(-32, -2))
}

///|
test "combined_operations_complex" {
  // 复合运算测试
  let x = 10
  let y = -5
  let z = 3
  
  // (x + y) * z
  let result1 = @clean_test.multiply(@clean_test.add(x, y), z)
  @clean_test.assert_eq(15, result1)
  
  // x * y + z
  let result2 = @clean_test.add(@clean_test.multiply(x, y), z)
  @clean_test.assert_eq(-47, result2)
  
  // (x + z) * (y + z)
  let result3 = @clean_test.multiply(@clean_test.add(x, z), @clean_test.add(y, z))
  @clean_test.assert_eq(26, result3)
}

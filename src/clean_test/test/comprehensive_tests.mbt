test "add_commutative_property" {
  // 测试加法交换律 a + b = b + a
  @clean_test.assert_eq(@clean_test.add(123, 456), @clean_test.add(456, 123))
  @clean_test.assert_eq(@clean_test.add(-123, 456), @clean_test.add(456, -123))
  @clean_test.assert_eq(@clean_test.add(-123, -456), @clean_test.add(-456, -123))
  @clean_test.assert_eq(@clean_test.add(0, 123), @clean_test.add(123, 0))
  @clean_test.assert_eq(@clean_test.add(2147483647, -2147483648), @clean_test.add(-2147483648, 2147483647))
}

///|
test "add_associative_property" {
  // 测试加法结合律 (a + b) + c = a + (b + c)
  @clean_test.assert_eq(@clean_test.add(@clean_test.add(10, 20), 30), @clean_test.add(10, @clean_test.add(20, 30)))
  @clean_test.assert_eq(@clean_test.add(@clean_test.add(-10, 20), 30), @clean_test.add(-10, @clean_test.add(20, 30)))
  @clean_test.assert_eq(@clean_test.add(@clean_test.add(100, 200), -300), @clean_test.add(100, @clean_test.add(200, -300)))
}

///|
test "multiply_commutative_property" {
  // 测试乘法交换律 a * b = b * a
  @clean_test.assert_eq(@clean_test.multiply(12, 34), @clean_test.multiply(34, 12))
  @clean_test.assert_eq(@clean_test.multiply(-12, 34), @clean_test.multiply(34, -12))
  @clean_test.assert_eq(@clean_test.multiply(-12, -34), @clean_test.multiply(-34, -12))
  @clean_test.assert_eq(@clean_test.multiply(0, 123), @clean_test.multiply(123, 0))
  @clean_test.assert_eq(@clean_test.multiply(1, -2147483648), @clean_test.multiply(-2147483648, 1))
}

///|
test "multiply_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  @clean_test.assert_eq(@clean_test.multiply(a, @clean_test.add(b, c)), @clean_test.add(@clean_test.multiply(a, b), @clean_test.multiply(a, c)))
  
  let a2 = -3
  let b2 = 7
  let c2 = -2
  @clean_test.assert_eq(@clean_test.multiply(a2, @clean_test.add(b2, c2)), @clean_test.add(@clean_test.multiply(a2, b2), @clean_test.multiply(a2, c2)))
}

///|
test "greet_special_characters" {
  // 测试包含特殊字符的字符串
  @clean_test.assert_eq_string("Hello, @#$%!", @clean_test.greet("@#$%"))
  @clean_test.assert_eq_string("Hello, 中文测试!", @clean_test.greet("中文测试"))
  @clean_test.assert_eq_string("Hello,   !", @clean_test.greet("  "))  // 空格
  @clean_test.assert_eq_string("Hello, \n\t!", @clean_test.greet("\n\t"))  // 换行和制表符
  @clean_test.assert_eq_string("Hello, 🌙!", @clean_test.greet("🌙"))  // emoji
}

///|
test "add_identity_elements" {
  // 测试加法单位元：a + 0 = a
  @clean_test.assert_eq(42, @clean_test.add(42, 0))
  @clean_test.assert_eq(-42, @clean_test.add(-42, 0))
  @clean_test.assert_eq(2147483647, @clean_test.add(2147483647, 0))
  @clean_test.assert_eq(-2147483648, @clean_test.add(-2147483648, 0))
  @clean_test.assert_eq(0, @clean_test.add(0, 0))
}

///|
test "multiply_identity_elements" {
  // 测试乘法单位元：a * 1 = a
  @clean_test.assert_eq(42, @clean_test.multiply(42, 1))
  @clean_test.assert_eq(-42, @clean_test.multiply(-42, 1))
  @clean_test.assert_eq(2147483647, @clean_test.multiply(2147483647, 1))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(-2147483648, 1))
  @clean_test.assert_eq(1, @clean_test.multiply(1, 1))
  
  // 测试乘法零元：a * 0 = 0
  @clean_test.assert_eq(0, @clean_test.multiply(42, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(-42, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(2147483647, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(-2147483648, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(0, 0))
}

///|
test "boundary_value_combinations" {
  // 测试边界值的各种组合
  let max = 2147483647
  let min = -2147483648
  
  // 加法边界组合
  @clean_test.assert_eq(max, @clean_test.add(max, max))
  @clean_test.assert_eq(min, @clean_test.add(min, min))
  @clean_test.assert_eq(-1, @clean_test.add(max, min))
  @clean_test.assert_eq(-1, @clean_test.add(min, max))
  @clean_test.assert_eq(max - 1, @clean_test.add(max, -1))
  @clean_test.assert_eq(min + 1, @clean_test.add(min, 1))
  
  // 乘法边界组合
  @clean_test.assert_eq(max, @clean_test.multiply(max, 2))
  @clean_test.assert_eq(min, @clean_test.multiply(min, 2))
  @clean_test.assert_eq(min, @clean_test.multiply(min, -1))
  @clean_test.assert_eq(0, @clean_test.multiply(max, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(min, 0))
}

///|
test "power_of_two_operations" {
  // 测试2的幂次运算
  @clean_test.assert_eq(4, @clean_test.multiply(2, 2))
  @clean_test.assert_eq(8, @clean_test.multiply(2, 4))
  @clean_test.assert_eq(16, @clean_test.multiply(4, 4))
  @clean_test.assert_eq(1024, @clean_test.multiply(32, 32))
  @clean_test.assert_eq(65536, @clean_test.multiply(256, 256))
  
  // 测试2的幂次加法
  @clean_test.assert_eq(6, @clean_test.add(2, 4))
  @clean_test.assert_eq(12, @clean_test.add(4, 8))
  @clean_test.assert_eq(24, @clean_test.add(8, 16))
  @clean_test.assert_eq(48, @clean_test.add(16, 32))
}

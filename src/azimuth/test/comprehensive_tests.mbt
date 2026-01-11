// 综合测试用例 - 补充现有测试的覆盖范围

///|
test "add_commutative_property" {
  // 测试加法交换律 a + b = b + a
  assert_eq(add(123, 456), add(456, 123))
  assert_eq(add(-123, 456), add(456, -123))
  assert_eq(add(-123, -456), add(-456, -123))
  assert_eq(add(0, 123), add(123, 0))
  assert_eq(add(2147483647, -2147483648), add(-2147483648, 2147483647))
}

///|
test "add_associative_property" {
  // 测试加法结合律 (a + b) + c = a + (b + c)
  assert_eq(add(add(10, 20), 30), add(10, add(20, 30)))
  assert_eq(add(add(-10, 20), 30), add(-10, add(20, 30)))
  assert_eq(add(add(100, 200), -300), add(100, add(200, -300)))
}

///|
test "multiply_commutative_property" {
  // 测试乘法交换律 a * b = b * a
  assert_eq(multiply(12, 34), multiply(34, 12))
  assert_eq(multiply(-12, 34), multiply(34, -12))
  assert_eq(multiply(-12, -34), multiply(-34, -12))
  assert_eq(multiply(0, 123), multiply(123, 0))
  assert_eq(multiply(1, -2147483648), multiply(-2147483648, 1))
}

///|
test "multiply_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  let a2 = -3
  let b2 = 7
  let c2 = -2
  assert_eq(multiply(a2, add(b2, c2)), add(multiply(a2, b2), multiply(a2, c2)))
}

///|
test "greet_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq("Hello, @#$%!", greet("@#$%"))
  assert_eq("Hello, 中文测试!", greet("中文测试"))
  assert_eq("Hello,   !", greet("  "))  // 空格
  assert_eq("Hello, \n\t!", greet("\n\t"))  // 换行和制表符
  assert_eq("Hello, 🌙!", greet("🌙"))  // emoji
}

///|
test "add_identity_elements" {
  // 测试加法单位元：a + 0 = a
  assert_eq(42, add(42, 0))
  assert_eq(-42, add(-42, 0))
  assert_eq(2147483647, add(2147483647, 0))
  assert_eq(-2147483648, add(-2147483648, 0))
  assert_eq(0, add(0, 0))
}

///|
test "multiply_identity_elements" {
  // 测试乘法单位元：a * 1 = a
  assert_eq(42, multiply(42, 1))
  assert_eq(-42, multiply(-42, 1))
  assert_eq(2147483647, multiply(2147483647, 1))
  assert_eq(-2147483648, multiply(-2147483648, 1))
  assert_eq(1, multiply(1, 1))
  
  // 测试乘法零元：a * 0 = 0
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(-42, 0))
  assert_eq(0, multiply(2147483647, 0))
  assert_eq(0, multiply(-2147483648, 0))
  assert_eq(0, multiply(0, 0))
}

///|
test "boundary_value_combinations" {
  // 测试边界值的各种组合
  let max = 2147483647
  let min = -2147483648
  
  // 加法边界组合
  assert_eq(max, add(max, max))
  assert_eq(min, add(min, min))
  assert_eq(-1, add(max, min))
  assert_eq(-1, add(min, max))
  assert_eq(max - 1, add(max, -1))
  assert_eq(min + 1, add(min, 1))
  
  // 乘法边界组合
  assert_eq(max, multiply(max, 2))
  assert_eq(min, multiply(min, 2))
  assert_eq(min, multiply(min, -1))
  assert_eq(0, multiply(max, 0))
  assert_eq(0, multiply(min, 0))
}

///|
test "power_of_two_operations" {
  // 测试2的幂次运算
  assert_eq(4, multiply(2, 2))
  assert_eq(8, multiply(2, 4))
  assert_eq(16, multiply(4, 4))
  assert_eq(1024, multiply(32, 32))
  assert_eq(65536, multiply(256, 256))
  
  // 测试2的幂次加法
  assert_eq(6, add(2, 4))
  assert_eq(12, add(4, 8))
  assert_eq(24, add(8, 16))
  assert_eq(48, add(16, 32))
}
// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法，涵盖核心功能和边界情况

test "basic_addition_operations" {
  // 测试基本加法运算
  assert_eq(azimuth::add(2, 3), 5)
  assert_eq(azimuth::add(-2, 3), 1)
  assert_eq(azimuth::add(2, -3), -1)
  assert_eq(azimuth::add(-2, -3), -5)
  assert_eq(azimuth::add(0, 0), 0)
}

test "basic_multiplication_operations" {
  // 测试基本乘法运算
  assert_eq(azimuth::multiply(2, 3), 6)
  assert_eq(azimuth::multiply(-2, 3), -6)
  assert_eq(azimuth::multiply(2, -3), -6)
  assert_eq(azimuth::multiply(-2, -3), 6)
  assert_eq(azimuth::multiply(0, 5), 0)
}

test "zero_operations" {
  // 测试零值运算
  assert_eq(azimuth::add(0, 42), 42)
  assert_eq(azimuth::add(42, 0), 42)
  assert_eq(azimuth::multiply(0, 42), 0)
  assert_eq(azimuth::multiply(42, 0), 0)
}

test "identity_elements" {
  // 测试单位元运算
  // 加法单位元是 0
  assert_eq(azimuth::add(123, 0), 123)
  assert_eq(azimuth::add(0, -456), -456)
  
  // 乘法单位元是 1
  assert_eq(azimuth::multiply(123, 1), 123)
  assert_eq(azimuth::multiply(1, -456), -456)
}

test "greet_function_basic" {
  // 测试基本问候功能
  assert_eq(azimuth::greet("World"), "Hello, World!")
  assert_eq(azimuth::greet("Alice"), "Hello, Alice!")
  assert_eq(azimuth::greet("Bob"), "Hello, Bob!")
}

test "greet_function_special_cases" {
  // 测试问候功能的特殊情况
  assert_eq(azimuth::greet(""), "Hello, !")
  assert_eq(azimuth::greet("123"), "Hello, 123!")
  assert_eq(azimuth::greet("Alice Bob"), "Hello, Alice Bob!")
}

test "combined_operations" {
  // 测试组合运算
  // (2 + 3) * 4 = 20
  let sum = azimuth::add(2, 3)
  let result = azimuth::multiply(sum, 4)
  assert_eq(result, 20)
  
  // 5 * 6 + 7 = 37
  let product = azimuth::multiply(5, 6)
  let result2 = azimuth::add(product, 7)
  assert_eq(result2, 37)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(azimuth::add(-10, -20), -30)
  assert_eq(azimuth::add(-10, 20), 10)
  assert_eq(azimuth::add(10, -20), -10)
  
  assert_eq(azimuth::multiply(-5, -6), 30)
  assert_eq(azimuth::multiply(-5, 6), -30)
  assert_eq(azimuth::multiply(5, -6), -30)
}

test "large_number_operations" {
  // 测试大数运算
  let large1 = 10000
  let large2 = 20000
  
  assert_eq(azimuth::add(large1, large2), 30000)
  assert_eq(azimuth::multiply(large1, 2), 20000)
  assert_eq(azimuth::multiply(100, 1000), 100000)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 15
  let b = 25
  let c = 10
  
  // 加法交换律: a + b = b + a
  assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 分配律: a * (b + c) = a * b + a * c
  let left = azimuth::multiply(a, azimuth::add(b, c))
  let right = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  assert_eq(left, right)
}
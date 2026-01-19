// 新的标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "add_zero_cases" {
  // 测试与0相加的各种情况
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-10, @azimuth.add(-10, 0))
  @azimuth.assert_eq(-10, @azimuth.add(0, -10))
}

test "add_large_numbers" {
  // 测试大数相加
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(-1000000, @azimuth.add(-500000, -500000))
  @azimuth.assert_eq(0, @azimuth.add(500000, -500000))
}

test "multiply_by_negative_numbers" {
  // 测试与负数相乘
  @azimuth.assert_eq(-20, @azimuth.multiply(4, -5))
  @azimuth.assert_eq(-20, @azimuth.multiply(-4, 5))
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -5))
}

test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  @azimuth.assert_eq(16, @azimuth.multiply(4, 4))
  @azimuth.assert_eq(64, @azimuth.multiply(8, 8))
  @azimuth.assert_eq(1024, @azimuth.multiply(32, 32))
}

test "greet_with_spaces" {
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a b c!", @azimuth.greet("a b c"))
}

test "divide_with_ceil_negative_dividend" {
  // 测试负数被除数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-4, 2))
}

test "divide_with_ceil_negative_divisor" {
  // 测试负数除数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(11, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(9, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(4, -2))
}

test "complex_calculation_chain" {
  // 测试复杂的计算链
  let a = 5
  let b = 3
  let c = 2
  
  // 计算 (a + b) * c
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(16, product)
  
  // 计算 a * (b + c)
  let sum2 = @azimuth.add(b, c)
  let product2 = @azimuth.multiply(a, sum2)
  @azimuth.assert_eq(25, product2)
}

test "string_operations_with_greet" {
  // 测试字符串操作与greet函数的组合
  let name1 = "Alice"
  let name2 = "Bob"
  
  let greeting1 = @azimuth.greet(name1)
  let greeting2 = @azimuth.greet(name2)
  
  @azimuth.assert_eq_string("Hello, Alice!", greeting1)
  @azimuth.assert_eq_string("Hello, Bob!", greeting2)
  @azimuth.assert_true(greeting1 != greeting2)
}
// 用户标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_basic" {
  // 测试 add 函数的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-3, 2))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_basic" {
  // 测试 multiply 函数的基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "subtract_function_basic" {
  // 测试 subtract 函数的基本功能
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(15, 15))
  @azimuth.assert_eq(20, @azimuth.subtract(10, -10))
}

test "divide_with_ceil_function_basic" {
  // 测试 divide_with_ceil 函数的基本功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "greet_function_basic" {
  // 测试 greet 函数的基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(2, @azimuth.add(5, -3))
  @azimuth.assert_eq(-8, @azimuth.add(-5, -3))
  @azimuth.assert_eq(-24, @azimuth.multiply(-4, 6))
  @azimuth.assert_eq(24, @azimuth.multiply(-4, -6))
  @azimuth.assert_eq(-17, @azimuth.subtract(-10, 7))
}

test "large_number_operations" {
  // 测试大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  @azimuth.assert_eq(500000, @azimuth.subtract(1000000, 500000))
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(1000000, 3))
}

test "complex_calculation" {
  // 测试复合计算
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(16, result1)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(17, result2)
  
  // (a - b) / c (向上取整)
  let result3 = @azimuth.divide_with_ceil(@azimuth.subtract(a, b), c)
  @azimuth.assert_eq(1, result3)
}
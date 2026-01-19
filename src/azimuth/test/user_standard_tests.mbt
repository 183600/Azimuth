// 用户自定义的标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "basic_addition_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
}

test "basic_multiplication_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-8, @azimuth.multiply(2, -4))
}

test "greet_function_basic_cases" {
  // 测试 greet 函数的基本情况
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "addition_with_zero" {
  // 测试与零相加的情况
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "multiplication_with_one" {
  // 测试与一相乘的情况
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(-5, 1))
}

test "mixed_operations" {
  // 测试混合运算
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(16, result)
}

test "boundary_values_addition" {
  // 测试边界值的加法运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "boundary_values_multiplication" {
  // 测试边界值的乘法运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_with_special_characters" {
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 5
  let width = 3
  
  // 面积 = 长 * 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(15, area)
  
  // 周长 = 2 * (长 + 宽)
  let sum = @azimuth.add(length, width)
  let perimeter = @azimuth.multiply(2, sum)
  @azimuth.assert_eq(16, perimeter)
}
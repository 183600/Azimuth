// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_basic_functionality" {
  // 测试加法基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_basic_functionality" {
  // 测试乘法基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "greet_basic_functionality" {
  // 测试问候函数基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
}

test "boundary_values_test" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 15
  let b = 25
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素性质
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  
  // 单位元素性质
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
}

test "complex_operations" {
  // 测试复合运算
  // (a + b) * c
  let sum = @azimuth.add(10, 20)
  let result = @azimuth.multiply(sum, 3)
  @azimuth.assert_eq(90, result)
  
  // a * b + c
  let product = @azimuth.multiply(5, 6)
  let result2 = @azimuth.add(product, 7)
  @azimuth.assert_eq(37, result2)
}

test "special_characters_greet" {
  // 测试特殊字符问候
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, test@domain!", @azimuth.greet("test@domain"))
}
// 标准MoonBit测试用例 - 覆盖核心功能
test "add_function_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
}

test "multiply_function_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "greet_function_basic_functionality" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_basic_functionality" {
  // 测试基本向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "add_function_edge_cases" {
  // 测试加法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val + 1))
}

test "multiply_function_edge_cases" {
  // 测试乘法边界情况
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
}

test "complex_arithmetic_operations" {
  // 测试复杂算术运算组合
  let sum = @azimuth.add(10, 20)
  let product = @azimuth.multiply(5, 6)
  let divide_result = @azimuth.divide_with_ceil(25, 8)
  
  @azimuth.assert_eq(30, sum)
  @azimuth.assert_eq(30, product)
  @azimuth.assert_eq(4, divide_result)
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(7, 13), @azimuth.add(13, 7))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(7, 13), @azimuth.multiply(13, 7))
  
  // 零元素性质
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
}
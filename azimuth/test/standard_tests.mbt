// 标准 MoonBit 测试用例
// 测试 azimuth 包中的核心功能

// 测试 add 函数的基本功能
test "add_positive_numbers" {
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

// 测试 add 函数的负数处理
test "add_negative_numbers" {
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
}

// 测试 add 函数的边界值处理
test "add_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

// 测试 multiply 函数的基本功能
test "multiply_basic_operations" {
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(7, -1))
}

// 测试 multiply 函数的负数处理
test "multiply_negative_numbers" {
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
}

// 测试 multiply 函数的边界值处理
test "multiply_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试最小值的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

// 测试 greet 函数的基本功能
test "greet_basic_functionality" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

// 测试 greet 函数的特殊字符处理
test "greet_special_characters" {
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
}

// 测试 greet 函数的Unicode字符处理
test "greet_unicode_characters" {
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, Ñoño!", @azimuth.greet("Ñoño"))
}
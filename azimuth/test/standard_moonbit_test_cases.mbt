// 标准 MoonBit 测试用例 - 测试 azimuth 库的核心功能
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "add_positive_numbers" {
  // 测试正数加法
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-2, @azimuth.add(-10, 8))
}

test "multiply_basic_operations" {
  // 测试基本乘法运算
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
}

test "greet_functionality" {
  // 测试问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
}

test "complex_calculations" {
  // 测试复杂计算
  let a = @azimuth.add(5, 3)
  let b = @azimuth.multiply(a, 2)
  let c = @azimuth.divide_with_ceil(b, 4)
  @azimuth.assert_eq(4, c)
}

test "mathematical_properties" {
  // 测试数学性质
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(7, 13), @azimuth.add(13, 7))
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(7, 13), @azimuth.multiply(13, 7))
  // 加法单位元
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "edge_cases" {
  // 测试边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-3, -2))
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
}
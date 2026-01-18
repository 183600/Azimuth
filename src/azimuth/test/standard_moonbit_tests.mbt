// 标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "identity_elements" {
  // 测试恒等元素
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "commutative_properties" {
  // 测试交换律性质
  let a = 15
  let b = 27
  
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "sequential_operations" {
  // 测试连续运算
  let result = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result)
  
  let nested = @azimuth.multiply(@azimuth.add(2, 3), @azimuth.add(4, 5))
  @azimuth.assert_eq(45, nested)
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(4, @azimuth.add(-3, 7))
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
}

test "large_number_calculations" {
  // 测试大数计算
  @azimuth.assert_eq(3000000, @azimuth.add(1000000, 2000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-2000000, @azimuth.multiply(-1000, 2000))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算表达式：(a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(75, result)
}

test "boolean_assertions" {
  // 测试布尔断言
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(@azimuth.add(2, 3) == 5)
  @azimuth.assert_false(@azimuth.add(2, 3) == 6)
}
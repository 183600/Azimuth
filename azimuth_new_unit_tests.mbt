// Azimuth 项目新增单元测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "add_positive_numbers" {
  // 测试正数加法
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(-10, @azimuth.add(-15, 5))
  @azimuth.assert_eq(0, @azimuth.add(-10, 10))
}

test "add_mixed_numbers" {
  // 测试正负数混合加法
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-2, @azimuth.add(-10, 8))
  @azimuth.assert_eq(7, @azimuth.add(-3, 10))
}

test "multiply_positive_numbers" {
  // 测试正数乘法
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "complex_expressions" {
  // 测试复合表达式
  @azimuth.assert_eq(10, @azimuth.add(@azimuth.multiply(2, 3), 4))
  @azimuth.assert_eq(20, @azimuth.multiply(@azimuth.add(2, 3), 4))
  @azimuth.assert_eq(7, @azimuth.add(@azimuth.multiply(-2, 3), 13))
}

test "greet_functionality" {
  // 测试问候功能
  @azimuth.assert_eq_string("Hello World", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello MoonBit", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello Azimuth", @azimuth.greet("Azimuth"))
}

test "chained_operations" {
  // 测试链式操作
  let result1 = @azimuth.add(10, 20)
  let result2 = @azimuth.multiply(result1, 2)
  let result3 = @azimuth.add(result2, 5)
  
  @azimuth.assert_eq(30, result1)
  @azimuth.assert_eq(60, result2)
  @azimuth.assert_eq(65, result3)
}

test "edge_cases" {
  // 测试边界情况
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
}
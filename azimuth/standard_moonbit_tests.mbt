// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖项目核心功能

test "add_zero_cases" {
  // 测试加法中的零值处理
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-3, @azimuth.add(0, -3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
}

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(-2147483647, @azimuth.add(-1073741824, -1073741823))
}

test "add_mixed_signs" {
  // 测试正负数混合加法
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  @azimuth.assert_eq(-5, @azimuth.add(3, -8))
  @azimuth.assert_eq(7, @azimuth.add(-3, 10))
  @azimuth.assert_eq(-2147483647, @azimuth.add(-2147483648, 1))
}

test "multiply_by_zero_and_one" {
  // 测试乘以0和1的情况
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-456, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 789))
  @azimuth.assert_eq(123, @azimuth.multiply(123, 1))
  @azimuth.assert_eq(-456, @azimuth.multiply(-456, 1))
  @azimuth.assert_eq(789, @azimuth.multiply(1, 789))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-20, @azimuth.multiply(4, -5))
  @azimuth.assert_eq(-30, @azimuth.multiply(-6, 5))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-46341, 46341))
}

test "greet_various_inputs" {
  // 测试各种输入的问候功能
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob123!", @azimuth.greet("Bob123"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

test "assert_bool_functions" {
  // 测试布尔断言函数
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 < 100)
  @azimuth.assert_true(-5 < 0)
  @azimuth.assert_false(1 == 2)
  @azimuth.assert_false(100 < 0)
  @azimuth.assert_false(-5 > 0)
}

test "combined_operations" {
  // 测试组合运算
  let x = 5
  let y = 3
  let z = -2
  
  // 计算 (x + y) * z
  let sum = @azimuth.add(x, y)
  let result = @azimuth.multiply(sum, z)
  @azimuth.assert_eq(-16, result)
  
  // 计算 x * y + z
  let product = @azimuth.multiply(x, y)
  let result2 = @azimuth.add(product, z)
  @azimuth.assert_eq(13, result2)
}
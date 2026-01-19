// Azimuth 高质量测试用例
// 使用标准的 MoonBit 测试语法，覆盖主要功能

test "basic_addition" {
  // 测试基本加法运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "addition_overflow_protection" {
  // 测试加法溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正溢出情况
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 2))
  
  // 负溢出情况
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -2))
  
  // 边界情况：最大值和最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "basic_multiplication" {
  // 测试基本乘法运算
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  @azimuth.assert_eq(123, @azimuth.multiply(123, 1))
}

test "multiplication_edge_cases" {
  // 测试乘法边界情况和特殊值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 乘以-1的特殊情况
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 最小值乘法处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(2, min_val))
  
  // 大数乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
}

test "ceil_division_precision" {
  // 测试向上取整除法的精确性
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-4, 2))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 2))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let x = 15
  let y = 4
  let z = -2
  
  // (x * y) + z
  let product = @azimuth.multiply(x, y)
  let result = @azimuth.add(product, z)
  @azimuth.assert_eq(58, result)
  
  // x + (y * z)
  let product2 = @azimuth.multiply(y, z)
  let result2 = @azimuth.add(x, product2)
  @azimuth.assert_eq(7, result2)
}

test "extreme_value_handling" {
  // 测试极值处理能力
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 1, 0))
  
  // 极值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}
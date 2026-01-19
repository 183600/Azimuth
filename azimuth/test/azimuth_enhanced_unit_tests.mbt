// Azimuth 增强单元测试
// 使用标准的 MoonBit 测试语法，专注于边界情况和复合场景

test "add_large_numbers" {
  // 测试大数加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近边界的加法
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 100, 100))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 100, -100))
  
  // 混合符号的大数加法
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val + 1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数与小数的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 边界值的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "chained_operations" {
  // 测试链式运算
  let base = 100
  
  // 链式加法
  let result1 = @azimuth.add(@azimuth.add(base, 50), 25)
  @azimuth.assert_eq(175, result1)
  
  // 链式乘法
  let result2 = @azimuth.multiply(@azimuth.multiply(base, 2), 3)
  @azimuth.assert_eq(600, result2)
  
  // 混合链式运算
  let result3 = @azimuth.add(@azimuth.multiply(base, 2), 50)
  @azimuth.assert_eq(250, result3)
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(max_val, max_val))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(min_val, min_val))
  
  // 负数除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-6, 3))
  
  // 小数除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 3))
}

test "greet_with_unicode" {
  // 测试包含Unicode字符的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, naïve!", @azimuth.greet("naïve"))
}

test "add_sequence_operations" {
  // 测试序列加法运算（不使用数组）
  let num1 = 1
  let num2 = 2
  let num3 = 3
  let num4 = 4
  let num5 = 5
  
  // 手动累加
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), num4), num5)
  @azimuth.assert_eq(15, sum1)
  
  // 分组累加
  let sum2 = @azimuth.add(@azimuth.add(num1, num2), @azimuth.add(num3, @azimuth.add(num4, num5)))
  @azimuth.assert_eq(15, sum2)
}

test "multiply_sequence_operations" {
  // 测试序列乘法运算（不使用数组）
  let num1 = 1
  let num2 = 2
  let num3 = 3
  let num4 = 4
  
  // 手动累乘
  let product1 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(num1, num2), num3), num4)
  @azimuth.assert_eq(24, product1)
  
  // 分组累乘
  let product2 = @azimuth.multiply(@azimuth.multiply(num1, num2), @azimuth.multiply(num3, num4))
  @azimuth.assert_eq(24, product2)
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let a = 10
  let b = 20
  let c = 30
  let d = 40
  
  // (a + b) * (c + d)
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(c, d)
  let result1 = @azimuth.multiply(sum1, sum2)
  @azimuth.assert_eq(2100, result1)
  
  // a * b + c * d
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(c, d)
  let result2 = @azimuth.add(product1, product2)
  @azimuth.assert_eq(1400, result2)
  
  // (a * b) + (c / d) 向上取整
  let product = @azimuth.multiply(a, b)
  let div_ceil = @azimuth.divide_with_ceil(c, d)
  let result3 = @azimuth.add(product, div_ceil)
  @azimuth.assert_eq(201, result3)
}

test "assert_with_complex_conditions" {
  // 测试复杂条件断言
  let x = 15
  let y = 25
  let z = 35
  
  // 计算结果断言
  let complex_result = @azimuth.add(@azimuth.multiply(x, y), @azimuth.divide_with_ceil(z, x))
  @azimuth.assert_eq(392, complex_result)
}

test "numeric_precision_tests" {
  // 测试数值精度
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 精确的边界运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -1))
  
  // 精确的除法运算
  @azimuth.assert_eq(1073741824, @azimuth.divide_with_ceil(2147483647, 2))
  @azimuth.assert_eq(-1073741824, @azimuth.divide_with_ceil(-2147483647, 2))
}
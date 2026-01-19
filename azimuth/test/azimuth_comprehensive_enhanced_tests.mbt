// Azimuth 综合增强测试用例
// 使用标准的 MoonBit 测试语法，测试各种边界情况和复杂场景

test "add_large_numbers" {
  // 测试大数相加
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近最大值的加法
  @azimuth.assert_eq(max_val, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(2147483640, 10))
  
  // 接近最小值的加法
  @azimuth.assert_eq(min_val, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(-2147483640, -10))
}

test "add_mixed_sign_numbers" {
  // 测试混合符号数相加
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
  @azimuth.assert_eq(-1, @azimuth.add(2147483647, -2147483648))
  @azimuth.assert_eq(1, @azimuth.add(-2147483648, 2147483647))
  @azimuth.assert_eq(-2147483647, @azimuth.add(-2147483648, 1))
}

test "multiply_large_numbers" {
  // 测试大数相乘
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数相乘的溢出检查
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(46341, -46341))
  @azimuth.assert_eq(max_val, @azimuth.multiply(-46341, -46341))
}

test "multiply_edge_values" {
  // 测试乘法边界值
  let min_val = -2147483648
  
  // 最小值的各种乘法情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(2, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-2, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(46341, min_val))
}

test "greet_unicode_characters" {
  // 测试包含Unicode字符的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, naïve!", @azimuth.greet("naïve"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(max_val, @azimuth.divide_with_ceil(max_val, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(max_val, max_val))
  
  // 负数边界情况
  @azimuth.assert_eq(min_val, @azimuth.divide_with_ceil(min_val, 1))
  @azimuth.assert_eq(-2147483647, @azimuth.divide_with_ceil(min_val, -1))
  
  // 特殊除数
  @azimuth.assert_eq(1073741824, @azimuth.divide_with_ceil(min_val, -2))
  @azimuth.assert_eq(-1073741824, @azimuth.divide_with_ceil(min_val, 2))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let a = 100
  let b = 50
  let c = -25
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(-3750, product)
  
  // a * b + c
  let product2 = @azimuth.multiply(a, b)
  let sum2 = @azimuth.add(product2, c)
  @azimuth.assert_eq(4975, sum2)
  
  // (a - b) * (b + c)
  let diff = @azimuth.add(a, -b)
  let sum3 = @azimuth.add(b, c)
  let product3 = @azimuth.multiply(diff, sum3)
  @azimuth.assert_eq(1250, product3)
}

test "sequential_operations" {
  // 测试连续操作
  let mut result = 0
  
  // 连续加法
  result = @azimuth.add(result, 10)
  @azimuth.assert_eq(10, result)
  
  result = @azimuth.add(result, 20)
  @azimuth.assert_eq(30, result)
  
  result = @azimuth.add(result, -5)
  @azimuth.assert_eq(25, result)
  
  // 连续乘法
  result = @azimuth.multiply(result, 2)
  @azimuth.assert_eq(50, result)
  
  result = @azimuth.multiply(result, -1)
  @azimuth.assert_eq(-50, result)
  
  result = @azimuth.multiply(result, 0)
  @azimuth.assert_eq(0, result)
}

test "string_operations_complex" {
  // 测试复杂的字符串操作
  let name1 = "MoonBit"
  let name2 = "Azimuth"
  let name3 = ""
  
  let greeting1 = @azimuth.greet(name1)
  let greeting2 = @azimuth.greet(name2)
  let greeting3 = @azimuth.greet(name3)
  
  @azimuth.assert_eq_string("Hello, MoonBit!", greeting1)
  @azimuth.assert_eq_string("Hello, Azimuth!", greeting2)
  @azimuth.assert_eq_string("Hello, !", greeting3)
}
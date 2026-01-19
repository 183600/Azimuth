// 增强测试用例 - 覆盖更多边界情况和功能组合
// 使用标准的 MoonBit 测试语法

test "add_function_negative_overflow" {
  // 测试 add 函数的负数溢出情况
  let min_val = -2147483648
  
  // 测试接近最小值的加法
  @azimuth.assert_eq(min_val, @azimuth.add(-2147483640, -10))
  @azimuth.assert_eq(min_val, @azimuth.add(-1073741824, -1073741824))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
}

test "multiply_function_large_numbers" {
  // 测试 multiply 函数的大数处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "divide_with_ceil_negative_cases" {
  // 测试向上取整除法的负数情况
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-6, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-7, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(7, -3))
}

test "greet_function_unicode_handling" {
  // 测试 greet 函数的 Unicode 字符处理
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
}

test "complex_arithmetic_expressions" {
  // 测试复杂的算术表达式
  let x = 15
  let y = -3
  let z = 7
  
  // 计算 (x + y) * z
  let sum = @azimuth.add(x, y)
  let product = @azimuth.multiply(sum, z)
  @azimuth.assert_eq(84, product)
  
  // 计算 x * (y + z)
  let sum2 = @azimuth.add(y, z)
  let product2 = @azimuth.multiply(x, sum2)
  @azimuth.assert_eq(60, product2)
}

test "edge_case_zero_operations" {
  // 测试零值的各种运算
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
  
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(-12345, 0))
  
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值和最小值的基本运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试1和-1的特殊情况
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
}

test "string_concatenation_edge_cases" {
  // 测试字符串拼接的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, abcdefghijklmnopqrstuvwxyz!", @azimuth.greet("abcdefghijklmnopqrstuvwxyz"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律 (在不会溢出的情况下)
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}
test "divide_with_ceil_basic_cases" {
  // 测试 divide_with_ceil 函数的基本情况
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(10, 5))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(11, 5))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(14, 5))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(16, 5))
}

test "divide_with_ceil_negative_cases" {
  // 测试 divide_with_ceil 函数的负数情况
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-10, 5))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-9, 5))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-6, 5))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 5))
}

test "divide_with_ceil_mixed_sign_cases" {
  // 测试 divide_with_ceil 函数的混合符号情况
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(10, -5))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(11, -5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-10, -5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-11, -5))
}

test "divide_with_ceil_edge_cases" {
  // 测试 divide_with_ceil 函数的边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(1, -1))
}

test "add_with_extreme_values" {
  // 测试加法函数的极值情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值与接近极值的组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1000, 1000))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1000000, 1000000))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1000, -1000))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1000000, -1000000))
}

test "multiply_with_powers_of_two" {
  // 测试乘法函数的2的幂次方情况
  @azimuth.assert_eq(1024, @azimuth.multiply(32, 32))
  @azimuth.assert_eq(1024, @azimuth.multiply(256, 4))
  @azimuth.assert_eq(1024, @azimuth.multiply(512, 2))
  @azimuth.assert_eq(1024, @azimuth.multiply(1024, 1))
  @azimuth.assert_eq(-1024, @azimuth.multiply(512, -2))
  @azimuth.assert_eq(1024, @azimuth.multiply(-512, -2))
}

test "multiply_boundary_precision" {
  // 测试乘法函数在边界附近的精度
  @azimuth.assert_eq(46340, @azimuth.multiply(46340, 1))
  @azimuth.assert_eq(46341, @azimuth.multiply(46341, 1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46340, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-46340, 46341))
}

test "greet_with_unicode_edge_cases" {
  // 测试问候函数的Unicode边界情况
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, 🚀🌙!", @azimuth.greet("🚀🌙"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, русский!", @azimuth.greet("русский"))
}

test "complex_arithmetic_sequences" {
  // 测试复杂的算术序列
  // 斐波那契数列前几项的和
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(fib1, fib2), fib3), fib4), fib5), fib6)
  @azimuth.assert_eq(20, sum)
  
  // 测试阶乘计算（简化版）
  let fact3 = @azimuth.multiply(3, @azimuth.multiply(2, 1))
  let fact4 = @azimuth.multiply(4, fact3)
  let fact5 = @azimuth.multiply(5, fact4)
  @azimuth.assert_eq(6, fact3)
  @azimuth.assert_eq(24, fact4)
  @azimuth.assert_eq(120, fact5)
}

test "mathematical_properties_comprehensive" {
  // 测试数学性质的综合情况
  
  // 加法结合律
  let a = 15
  let b = 25
  let c = 35
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法结合律
  let x = 4
  let y = 5
  let z = 6
  let mult_left = @azimuth.multiply(@azimuth.multiply(x, y), z)
  let mult_right = @azimuth.multiply(x, @azimuth.multiply(y, z))
  @azimuth.assert_eq(mult_left, mult_right)
  
  // 分配律：(a + b) * c = a * c + b * c
  let sum_mult = @azimuth.multiply(@azimuth.add(a, b), c)
  let dist_sum = @azimuth.add(@azimuth.multiply(a, c), @azimuth.multiply(b, c))
  @azimuth.assert_eq(sum_mult, dist_sum)
  
  // 测试零元素和单位元素的性质
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(0, a))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, a))
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(a, @azimuth.multiply(1, a))
}
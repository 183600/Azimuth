// Azimuth 核心测试用例
// 使用标准的 MoonBit 测试语法

test "add_zero_cases" {
  // 测试加法的零值情况
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-100, @azimuth.add(0, -100))
  @azimuth.assert_eq(-100, @azimuth.add(-100, 0))
}

test "add_mixed_sign_numbers" {
  // 测试混合符号的加法
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(-50, @azimuth.add(50, -100))
  @azimuth.assert_eq(50, @azimuth.add(100, -50))
  @azimuth.assert_eq(-200, @azimuth.add(-100, -100))
  @azimuth.assert_eq(200, @azimuth.add(100, 100))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(1000, -1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
  @azimuth.assert_eq(123456789, @azimuth.multiply(12345, 10001))
}

test "multiply_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
}

test "greet_various_inputs" {
  // 测试各种输入的问候函数
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
}

test "divide_with_ceil_various_cases" {
  // 测试向上取整除法的各种情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(20, 4))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(21, 4))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(16, 4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 3))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-2, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
}

test "complex_combined_operations" {
  // 测试复合运算
  let x = 15
  let y = 3
  let z = -2
  
  // 计算 (x * y) + z
  let step1 = @azimuth.multiply(x, y)
  let result1 = @azimuth.add(step1, z)
  @azimuth.assert_eq(43, result1)
  
  // 计算 x + (y * z)
  let step2 = @azimuth.multiply(y, z)
  let result2 = @azimuth.add(x, step2)
  @azimuth.assert_eq(9, result2)
  
  // 计算 (x + z) * y
  let step3 = @azimuth.add(x, z)
  let result3 = @azimuth.multiply(step3, y)
  @azimuth.assert_eq(39, result3)
}

test "edge_case_min_max_operations" {
  // 测试最小值和最大值的边界运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1000000))
  
  // 测试最小值加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1000000))
  
  // 测试最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, -1))
}
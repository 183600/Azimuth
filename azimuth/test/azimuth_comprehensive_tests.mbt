// Azimuth 综合测试用例
// 使用标准的 MoonBit 测试语法，覆盖所有主要功能

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
}

test "add_function_zero_handling" {
  // 测试加法函数的零值处理
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_function_overflow_cases" {
  // 测试加法函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  
  // 负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 最大值和最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_function_basic_cases" {
  // 测试乘法函数的基本情况
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 乘以1
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 乘以-1
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_function" {
  // 测试向上取整除法函数
  // 能整除的情况
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(6, 2))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-6, 2))
  
  // 需要向上取整的情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(7, 2))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(15, 4))
}

test "complex_combined_operations" {
  // 测试复合运算
  let a = 12
  let b = -5
  let c = 3
  
  // 计算 (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(21, result)
  
  // 计算 a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(-57, result2)
}
// Azimuth 增强测试用例
// 使用标准的 MoonBit 测试语法，覆盖更多边界情况和功能组合

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2, 5))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数处理
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-6, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-2, 5))
}

test "add_large_numbers" {
  // 测试大数加法
  let large_num = 1000000
  @azimuth.assert_eq(2000000, @azimuth.add(large_num, large_num))
  @azimuth.assert_eq(1001000, @azimuth.add(large_num, 1000))
  @azimuth.assert_eq(0, @azimuth.add(large_num, -large_num))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  let large_num = 10000
  @azimuth.assert_eq(100000000, @azimuth.multiply(large_num, large_num))
  @azimuth.assert_eq(-100000000, @azimuth.multiply(large_num, -large_num))
  @azimuth.assert_eq(0, @azimuth.multiply(large_num, 0))
}

test "add_sequence_operations" {
  // 测试加法的序列运算
  let result = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result)
  
  let result2 = @azimuth.add(@azimuth.add(-5, 10), @azimuth.add(-3, 8))
  @azimuth.assert_eq(10, result2)
}

test "multiply_sequence_operations" {
  // 测试乘法的序列运算
  let result = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result)
  
  let result2 = @azimuth.multiply(@azimuth.multiply(-2, 3), @azimuth.multiply(4, -5))
  @azimuth.assert_eq(120, result2)
}

test "mixed_operations" {
  // 测试混合运算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(900, result)
  
  // a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(230, result2)
}

test "greet_unicode_characters" {
  // 测试包含Unicode字符的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
}

test "edge_case_min_max_operations" {
  // 测试最小值和最大值的运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与-1的加法
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val, -1))
  
  // 最小值与1的加法
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
  
  // 最大值与1的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 最小值与1的乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (10 * 20) + (30 * 40) - (50 / 10) 的向上取整
  let product1 = @azimuth.multiply(10, 20)  // 200
  let product2 = @azimuth.multiply(30, 40)  // 1200
  let sum = @azimuth.add(product1, product2)  // 1400
  let division = @azimuth.divide_with_ceil(50, 10)  // 5
  let final_result = @azimuth.add(sum, -division)  // 1395
  
  @azimuth.assert_eq(1395, final_result)
}
test "string_greet_special_characters" {
  // 测试特殊字符和Unicode
  @clean_test.assert_eq_string("Hello, 世界!", @clean_test.greet("世界"))
  @clean_test.assert_eq_string("Hello, @#$%!", @clean_test.greet("@#$%"))
  @clean_test.assert_eq_string("Hello, 🌙!", @clean_test.greet("🌙"))
  @clean_test.assert_eq_string("Hello, space !", @clean_test.greet("space "))
  @clean_test.assert_eq_string("Hello, \nnewline!", @clean_test.greet("\nnewline"))
}

///|
test "add_large_numbers_sequence" {
  // 测试连续大数相加
  let result1 = @clean_test.add(1000000000, 1000000000)
  @clean_test.assert_eq(2000000000, result1)
  
  let result2 = @clean_test.add(result1, 1000000000)
  @clean_test.assert_eq(2147483647, result2) // 应该溢出到最大值
  
  // 测试负数序列
  let result3 = @clean_test.add(-1000000000, -1000000000)
  @clean_test.assert_eq(-2000000000, result3)
  
  let result4 = @clean_test.add(result3, -1000000000)
  @clean_test.assert_eq(-2147483648, result4) // 应该溢出到最小值
}

///|
test "multiply_sequence_operations" {
  // 测试连续乘法运算
  let result1 = @clean_test.multiply(1000, 1000)
  @clean_test.assert_eq(1000000, result1)
  
  let result2 = @clean_test.multiply(result1, 1000)
  @clean_test.assert_eq(1000000000, result2)
  
  let result3 = @clean_test.multiply(result2, 3)
  @clean_test.assert_eq(2147483647, result3) // 应该溢出到最大值
  
  // 测试负数序列
  let result4 = @clean_test.multiply(-1000, 1000)
  @clean_test.assert_eq(-1000000, result4)
  
  let result5 = @clean_test.multiply(result4, 1000)
  @clean_test.assert_eq(-1000000000, result5)
  
  let result6 = @clean_test.multiply(result5, 3)
  @clean_test.assert_eq(-2147483648, result6) // 应该溢出到最小值
}

///|
test "add_multiply_combination" {
  // 测试加法和乘法的组合运算
  let add_result = @clean_test.add(100, 200)
  let mul_result = @clean_test.multiply(add_result, 1000)
  @clean_test.assert_eq(300000, mul_result)
  
  // 测试负数组合
  let add_result2 = @clean_test.add(-100, -200)
  let mul_result2 = @clean_test.multiply(add_result2, 1000)
  @clean_test.assert_eq(-300000, mul_result2)
  
  // 测试混合符号
  let add_result3 = @clean_test.add(1000, -500)
  let mul_result3 = @clean_test.multiply(add_result3, 1000)
  @clean_test.assert_eq(500000, mul_result3)
}

///|
test "boundary_value_precision" {
  // 测试边界值的精确性
  @clean_test.assert_eq(2147483646, @clean_test.add(2147483645, 1))
  @clean_test.assert_eq(2147483647, @clean_test.add(2147483646, 1))
  @clean_test.assert_eq(2147483647, @clean_test.add(2147483647, 0))
  @clean_test.assert_eq(2147483647, @clean_test.add(2147483647, 1))
  
  @clean_test.assert_eq(-2147483647, @clean_test.add(-2147483646, -1))
  @clean_test.assert_eq(-2147483648, @clean_test.add(-2147483647, -1))
  @clean_test.assert_eq(-2147483648, @clean_test.add(-2147483648, 0))
  @clean_test.assert_eq(-2147483648, @clean_test.add(-2147483648, -1))
}

///|
test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  @clean_test.assert_eq(2, @clean_test.multiply(1, 2))
  @clean_test.assert_eq(4, @clean_test.multiply(2, 2))
  @clean_test.assert_eq(8, @clean_test.multiply(4, 2))
  @clean_test.assert_eq(16, @clean_test.multiply(8, 2))
  @clean_test.assert_eq(32, @clean_test.multiply(16, 2))
  @clean_test.assert_eq(64, @clean_test.multiply(32, 2))
  @clean_test.assert_eq(128, @clean_test.multiply(64, 2))
  @clean_test.assert_eq(256, @clean_test.multiply(128, 2))
  @clean_test.assert_eq(512, @clean_test.multiply(256, 2))
  @clean_test.assert_eq(1024, @clean_test.multiply(512, 2))
  @clean_test.assert_eq(2048, @clean_test.multiply(1024, 2))
  
  // 测试大数2的幂次方
  @clean_test.assert_eq(1048576, @clean_test.multiply(1024, 1024))
  @clean_test.assert_eq(2097152, @clean_test.multiply(1048576, 2))
}

///|
test "zero_operations_comprehensive" {
  // 全面测试0相关的运算
  @clean_test.assert_eq(0, @clean_test.add(0, 0))
  @clean_test.assert_eq(5, @clean_test.add(0, 5))
  @clean_test.assert_eq(-5, @clean_test.add(-5, 0))
  @clean_test.assert_eq(2147483647, @clean_test.add(0, 2147483647))
  @clean_test.assert_eq(-2147483648, @clean_test.add(0, -2147483648))
  
  @clean_test.assert_eq(0, @clean_test.multiply(0, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(0, 5))
  @clean_test.assert_eq(0, @clean_test.multiply(-5, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(0, 2147483647))
  @clean_test.assert_eq(0, @clean_test.multiply(0, -2147483648))
}

///|
test "identity_elements" {
  // 测试单位元（加法的0，乘法的1）
  // 加法单位元测试
  @clean_test.assert_eq(5, @clean_test.add(5, 0))
  @clean_test.assert_eq(5, @clean_test.add(0, 5))
  @clean_test.assert_eq(-5, @clean_test.add(-5, 0))
  @clean_test.assert_eq(-5, @clean_test.add(0, -5))
  @clean_test.assert_eq(2147483647, @clean_test.add(2147483647, 0))
  @clean_test.assert_eq(-2147483648, @clean_test.add(-2147483648, 0))
  
  // 乘法单位元测试
  @clean_test.assert_eq(5, @clean_test.multiply(5, 1))
  @clean_test.assert_eq(5, @clean_test.multiply(1, 5))
  @clean_test.assert_eq(-5, @clean_test.multiply(-5, 1))
  @clean_test.assert_eq(-5, @clean_test.multiply(1, -5))
  @clean_test.assert_eq(2147483647, @clean_test.multiply(2147483647, 1))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(-2147483648, 1))
}

///|
test "negative_one_operations" {
  // 测试-1的特殊运算
  @clean_test.assert_eq(-5, @clean_test.multiply(5, -1))
  @clean_test.assert_eq(-5, @clean_test.multiply(-1, 5))
  @clean_test.assert_eq(5, @clean_test.multiply(-5, -1))
  @clean_test.assert_eq(5, @clean_test.multiply(-1, -5))
  
  // 最小值与-1的特殊情况
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(-2147483648, -1))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(-1, -2147483648))
}

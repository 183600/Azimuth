// 扩展测试文件 - 补充测试用例

///|
test "string_greet_special_characters" {
  // 测试特殊字符和Unicode
  assert_eq("Hello, 世界!", @azimuth.greet("世界"))
  assert_eq("Hello, @#$%!", @azimuth.greet("@#$%"))
  assert_eq("Hello, 🌙!", @azimuth.greet("🌙"))
  assert_eq("Hello, space !", @azimuth.greet("space "))
  assert_eq("Hello, \nnewline!", @azimuth.greet("\nnewline"))
}

///|
test "add_large_numbers_sequence" {
  // 测试连续大数相加
  let result1 = @azimuth.add(1000000000, 1000000000)
  assert_eq(2000000000, result1)
  
  let result2 = @azimuth.add(result1, 1000000000)
  assert_eq(2147483647, result2) // 应该溢出到最大值
  
  // 测试负数序列
  let result3 = @azimuth.add(-1000000000, -1000000000)
  assert_eq(-2000000000, result3)
  
  let result4 = @azimuth.add(result3, -1000000000)
  assert_eq(-2147483648, result4) // 应该溢出到最小值
}

///|
test "multiply_sequence_operations" {
  // 测试连续乘法运算
  let result1 = @azimuth.multiply(1000, 1000)
  assert_eq(1000000, result1)
  
  let result2 = @azimuth.multiply(result1, 1000)
  assert_eq(1000000000, result2)
  
  let result3 = @azimuth.multiply(result2, 3)
  assert_eq(2147483647, result3) // 应该溢出到最大值
  
  // 测试负数序列
  let result4 = @azimuth.multiply(-1000, 1000)
  assert_eq(-1000000, result4)
  
  let result5 = @azimuth.multiply(result4, 1000)
  assert_eq(-1000000000, result5)
  
  let result6 = @azimuth.multiply(result5, 3)
  assert_eq(-2147483648, result6) // 应该溢出到最小值
}

///|
test "add_multiply_combination" {
  // 测试加法和乘法的组合运算
  let add_result = @azimuth.add(100, 200)
  let mul_result = @azimuth.multiply(add_result, 1000)
  assert_eq(300000, mul_result)
  
  // 测试负数组合
  let add_result2 = @azimuth.add(-100, -200)
  let mul_result2 = @azimuth.multiply(add_result2, 1000)
  assert_eq(-300000, mul_result2)
  
  // 测试混合符号
  let add_result3 = @azimuth.add(1000, -500)
  let mul_result3 = @azimuth.multiply(add_result3, 1000)
  assert_eq(500000, mul_result3)
}

///|
test "boundary_value_precision" {
  // 测试边界值的精确性
  assert_eq(2147483646, @azimuth.add(2147483645, 1))
  assert_eq(2147483647, @azimuth.add(2147483646, 1))
  assert_eq(2147483647, @azimuth.add(2147483647, 0))
  assert_eq(2147483647, @azimuth.add(2147483647, 1))
  
  assert_eq(-2147483647, @azimuth.add(-2147483646, -1))
  assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
  assert_eq(-2147483648, @azimuth.add(-2147483648, -1))
}

///|
test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  assert_eq(2, @azimuth.multiply(1, 2))
  assert_eq(4, @azimuth.multiply(2, 2))
  assert_eq(8, @azimuth.multiply(4, 2))
  assert_eq(16, @azimuth.multiply(8, 2))
  assert_eq(32, @azimuth.multiply(16, 2))
  assert_eq(64, @azimuth.multiply(32, 2))
  assert_eq(128, @azimuth.multiply(64, 2))
  assert_eq(256, @azimuth.multiply(128, 2))
  assert_eq(512, @azimuth.multiply(256, 2))
  assert_eq(1024, @azimuth.multiply(512, 2))
  assert_eq(2048, @azimuth.multiply(1024, 2))
  
  // 测试大数2的幂次方
  assert_eq(1048576, @azimuth.multiply(1024, 1024))
  assert_eq(2097152, @azimuth.multiply(1048576, 2))
}

///|
test "zero_operations_comprehensive" {
  // 全面测试0相关的运算
  assert_eq(0, @azimuth.add(0, 0))
  assert_eq(5, @azimuth.add(0, 5))
  assert_eq(-5, @azimuth.add(-5, 0))
  assert_eq(2147483647, @azimuth.add(0, 2147483647))
  assert_eq(-2147483648, @azimuth.add(0, -2147483648))
  
  assert_eq(0, @azimuth.multiply(0, 0))
  assert_eq(0, @azimuth.multiply(0, 5))
  assert_eq(0, @azimuth.multiply(-5, 0))
  assert_eq(0, @azimuth.multiply(0, 2147483647))
  assert_eq(0, @azimuth.multiply(0, -2147483648))
}

///|
test "identity_elements" {
  // 测试单位元（加法的0，乘法的1）
  // 加法单位元测试
  assert_eq(5, @azimuth.add(5, 0))
  assert_eq(5, @azimuth.add(0, 5))
  assert_eq(-5, @azimuth.add(-5, 0))
  assert_eq(-5, @azimuth.add(0, -5))
  assert_eq(2147483647, @azimuth.add(2147483647, 0))
  assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
  
  // 乘法单位元测试
  assert_eq(5, @azimuth.multiply(5, 1))
  assert_eq(5, @azimuth.multiply(1, 5))
  assert_eq(-5, @azimuth.multiply(-5, 1))
  assert_eq(-5, @azimuth.multiply(1, -5))
  assert_eq(2147483647, @azimuth.multiply(2147483647, 1))
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
}

///|
test "negative_one_operations" {
  // 测试-1的特殊运算
  assert_eq(-5, @azimuth.multiply(5, -1))
  assert_eq(-5, @azimuth.multiply(-1, 5))
  assert_eq(5, @azimuth.multiply(-5, -1))
  assert_eq(5, @azimuth.multiply(-1, -5))
  
  // 最小值与-1的特殊情况
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))
  assert_eq(-2147483648, @azimuth.multiply(-1, -2147483648))
}
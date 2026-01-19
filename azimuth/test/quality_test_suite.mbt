// Azimuth 高质量测试用例集
// 使用标准的 MoonBit 测试语法，覆盖核心功能的各种场景

test "add_zero_handling" {
  // 测试加法中的零值处理
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
}

test "add_boundary_values" {
  // 测试边界值的加法运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近边界的值
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -1))
  
  // 边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
}

test "multiply_identity_elements" {
  // 测试乘法的单位元（0和1）处理
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -12345))
  @azimuth.assert_eq(0, @azimuth.multiply(-12345, 0))
  
  @azimuth.assert_eq(12345, @azimuth.multiply(1, 12345))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(1, -12345))
  @azimuth.assert_eq(-12345, @azimuth.multiply(-12345, 1))
}

test "multiply_negative_handling" {
  // 测试乘法的负数处理
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  
  // 测试-1的特殊处理
  @azimuth.assert_eq(-42, @azimuth.multiply(-1, 42))
  @azimuth.assert_eq(42, @azimuth.multiply(-1, -42))
  
  // 最小值与-1的特殊情况
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_unicode_handling" {
  // 测试问候函数的Unicode字符处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, naïve!", @azimuth.greet("naïve"))
}

test "greet_empty_and_whitespace" {
  // 测试问候函数的空字符串和空白字符处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, \t!", @azimuth.greet("\t"))
  @azimuth.assert_eq_string("Hello, \n!", @azimuth.greet("\n"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列计算
  let values = [1, 2, 3, 4, 5]
  let sum = @azimuth.add(0, 0)
  
  // 计算数列的和
  sum = @azimuth.add(sum, values[0])
  sum = @azimuth.add(sum, values[1])
  sum = @azimuth.add(sum, values[2])
  sum = @azimuth.add(sum, values[3])
  sum = @azimuth.add(sum, values[4])
  
  @azimuth.assert_eq(15, sum)
  
  // 计算数列的乘积
  let product = @azimuth.multiply(1, values[0])
  product = @azimuth.multiply(product, values[1])
  product = @azimuth.multiply(product, values[2])
  product = @azimuth.multiply(product, values[3])
  product = @azimuth.multiply(product, values[4])
  
  @azimuth.assert_eq(120, product)
}

test "overflow_edge_cases_comprehensive" {
  // 综合测试溢出边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 乘法溢出测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(2, min_val))
  
  // 混合运算测试
  let result1 = @azimuth.multiply(1000, 1000)
  let result2 = @azimuth.add(result1, max_val - 1000000)
  @azimuth.assert_eq(max_val, result2)
}

test "string_processing_with_greet" {
  // 测试使用greet函数进行字符串处理
  let names = ["Alice", "Bob", "Charlie"]
  
  let greeting1 = @azimuth.greet(names[0])
  let greeting2 = @azimuth.greet(names[1])
  let greeting3 = @azimuth.greet(names[2])
  
  @azimuth.assert_eq_string("Hello, Alice!", greeting1)
  @azimuth.assert_eq_string("Hello, Bob!", greeting2)
  @azimuth.assert_eq_string("Hello, Charlie!", greeting3)
  
  // 测试长字符串
  let long_name = "VeryLongNameThatExceedsNormalExpectations"
  let long_greeting = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, VeryLongNameThatExceedsNormalExpectations!", long_greeting)
}
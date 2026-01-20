// Azimuth 高质量测试套件
// 包含10个精心设计的测试用例，覆盖核心功能和边界情况

test "add_function_comprehensive_test" {
  // 全面测试加法函数的各种情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 基本加法测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  
  // 零值特殊情况
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-42, @azimuth.add(-42, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 溢出边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况和特殊值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 基本乘法测试
  @azimuth.assert_eq(24, @azimuth.multiply(4, 6))
  @azimuth.assert_eq(-24, @azimuth.multiply(-4, 6))
  @azimuth.assert_eq(24, @azimuth.multiply(-4, -6))
  
  // 零值和单位元测试
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(12345, -1))
  
  // 最小值特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 溢出测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
}

test "greet_function_unicode_and_special_chars" {
  // 测试问候函数的Unicode和特殊字符处理
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, \t\n!", @azimuth.greet("\t\n"))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  // 正数测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  
  // 负数被除数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 3))
  
  // 负数除数测试
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(9, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
  
  // 大数测试
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(1000000, 3))
  @azimuth.assert_eq(-333333, @azimuth.divide_with_ceil(-999999, 3))
}

test "complex_arithmetic_operations" {
  // 测试复杂的算术运算组合
  let a = 15
  let b = 25
  let c = -10
  
  // 计算 (a * b) + c
  let product = @azimuth.multiply(a, b)
  let result1 = @azimuth.add(product, c)
  @azimuth.assert_eq(365, result1)
  
  // 计算 a * (b + c)
  let sum = @azimuth.add(b, c)
  let result2 = @azimuth.multiply(a, sum)
  @azimuth.assert_eq(225, result2)
  
  // 计算 (a + b) * c
  let sum2 = @azimuth.add(a, b)
  let result3 = @azimuth.multiply(sum2, c)
  @azimuth.assert_eq(-400, result3)
  
  // 验证分配律 a * (b + c) = a * b + a * c
  let distributive_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distributive_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distributive_left, distributive_right)
}

test "boolean_assertions_complex_conditions" {
  // 测试布尔断言的复杂条件
  // 数值比较
  @azimuth.assert_true(100 > 50)
  @azimuth.assert_true(-100 < -50)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_true(2147483647 > -2147483648)
  
  @azimuth.assert_false(100 < 50)
  @azimuth.assert_false(-100 > -50)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(-2147483648 > 2147483647)
  
  // 复合条件
  @azimuth.assert_true(10 > 5 && 5 > 0)
  @azimuth.assert_true(10 > 5 || 5 > 10)
  @azimuth.assert_false(10 < 5 && 5 < 0)
  @azimuth.assert_false(10 < 5 || 5 < 10)
}

test "extreme_value_arithmetic" {
  // 测试极值算术运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -1))
  
  // 极值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 极值组合运算
  let extreme_product = @azimuth.multiply(1000, 1000)
  let extreme_sum = @azimuth.add(extreme_product, max_val - 1000000)
  @azimuth.assert_eq(max_val, extreme_sum)
}

test "string_assertions_various_cases" {
  // 测试字符串断言的各种情况
  // 基本字符串比较
  @azimuth.assert_eq_string("hello", "hello")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("12345", "12345")
  
  // 特殊字符字符串
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  
  // 长字符串和复杂内容
  let long_name = "VeryLongNameWithManyCharacters123456789"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
  
  let email = "user@example.com"
  @azimuth.assert_eq_string("Hello, " + email + "!", @azimuth.greet(email))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  // 加法交换律: a + b = b + a
  let a = 12345
  let b = 67890
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let c = 11111
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let left_mult = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mult = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mult, right_mult)
  
  // 分配律: a * (b + c) = a * b + a * c
  let distrib_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distrib_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distrib_left, distrib_right)
}

test "performance_and_stress_test" {
  // 性能和压力测试
  // 大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  let large_num3 = -3000000
  
  // 大数基本运算
  @azimuth.assert_eq(3000000, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(-1000000, @azimuth.add(large_num1, large_num3))
  @azimuth.assert_eq(-5000000, @azimuth.add(large_num2, large_num3))
  
  @azimuth.assert_eq(2000000000000, @azimuth.multiply(large_num1, large_num2))
  @azimuth.assert_eq(-3000000000000, @azimuth.multiply(large_num1, large_num3))
  @azimuth.assert_eq(-6000000000000, @azimuth.multiply(large_num2, large_num3))
  
  // 连续运算测试
  let result = large_num1
  result = @azimuth.add(result, large_num2)
  result = @azimuth.multiply(result, 2)
  result = @azimuth.add(result, large_num3)
  @azimuth.assert_eq(3000000, result)
  
  // 向上取整除法压力测试
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(1000000, 3))
  @azimuth.assert_eq(666667, @azimuth.divide_with_ceil(2000000, 3))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(3000000, 3))
}
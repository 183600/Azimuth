// 高质量 MoonBit 测试用例
// 覆盖 lib.mbt 中的主要功能和边界情况

// 测试 add 函数的边界值处理
test "add_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  
  // 测试零的特殊处理
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-42, @azimuth.add(42, -84))
}

// 测试 add 函数的数学性质
test "add_mathematical_properties" {
  // 测试交换律
  let a = 12345
  let b = -6789
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试结合律（通过两次加法验证）
  let x = 100
  let y = 200
  let z = -150
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
  
  // 测试加法单位元
  @azimuth.assert_eq(999, @azimuth.add(999, 0))
  @azimuth.assert_eq(-999, @azimuth.add(0, -999))
}

// 测试 multiply 函数的复杂边界情况
test "multiply_complex_edge_cases" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相乘的溢出处理
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  
  // 测试最小值的各种乘法情况
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

// 测试 multiply 函数的数学性质
test "multiply_mathematical_properties" {
  // 测试交换律
  let a = 123
  let b = -456
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试分配律（a * (b + c) = a * b + a * c）
  let a = 10
  let b = 20
  let c = 30
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
  
  // 测试乘法单位元
  @azimuth.assert_eq(777, @azimuth.multiply(777, 1))
  @azimuth.assert_eq(-777, @azimuth.multiply(-777, 1))
}

// 测试 greet 函数的字符串处理
test "greet_string_processing" {
  // 测试特殊字符和Unicode
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  
  // 测试空字符串和单字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameThatTestsStringHandlingCapacity"
  let expected = "Hello, " + long_name + "!"
  @azimuth.assert_eq_string(expected, @azimuth.greet(long_name))
}

// 测试 divide_with_ceil 函数的各种情况
test "divide_with_ceil_comprehensive" {
  // 测试正数除法的向上取整
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(10, 2))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(7, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  
  // 测试负数除法的向上取整
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-6, 2))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 2))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 2))
  
  // 测试混合符号除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(6, -2))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(3, -2))
  
  // 测试边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(2147483647, 1))
}

// 测试复合运算场景
test "combined_operations" {
  // 测试加法和乘法的组合
  let a = 10
  let b = 20
  let c = 30
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(@azimuth.multiply(a, c), @azimuth.multiply(b, c))
  @azimuth.assert_eq(result1, result2)
  
  // 测试包含除法的复杂表达式
  let x = 100
  let y = 3
  let z = 7
  let complex_result = @azimuth.multiply(@azimuth.divide_with_ceil(x, y), z)
  // 100/3 向上取整是 34，然后 34*7 = 238
  @azimuth.assert_eq(238, complex_result)
}

// 测试极端值和错误处理
test "extreme_values_and_error_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试add函数的极端值组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试multiply函数的极端值
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 测试连续运算的稳定性
  let chain_result = @azimuth.add(@azimuth.multiply(100, 200), @azimuth.multiply(300, 400))
  @azimuth.assert_eq(140000, chain_result)
}

// 测试断言函数的功能
test "assertion_functionality" {
  // 测试数值断言
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-42, -42)
  @azimuth.assert_eq(0, 0)
  
  // 测试字符串断言
  @azimuth.assert_eq_string("Hello", "Hello")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("123", "123")
  
  // 测试布尔断言
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert(false == false)
}
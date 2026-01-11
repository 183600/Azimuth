// 额外的边界情况测试 - 补充现有测试的覆盖范围

///|
test "add_chained_operations" {
  // 测试连续加法操作的稳定性
  let mut result = 1000
  result = add(result, 2000)  // 3000
  result = add(result, -1000) // 2000
  result = add(result, 47)    // 2047
  result = add(result, -48)   // 1999
  assert_eq(1999, result)
}

///|
test "multiply_chained_operations" {
  // 测试连续乘法操作的稳定性
  let mut result = 2
  result = multiply(result, 3)   // 6
  result = multiply(result, 4)   // 24
  result = multiply(result, 5)   // 120
  result = multiply(result, 0)   // 0
  result = multiply(result, 100) // 0
  assert_eq(0, result)
}

///|
test "mixed_operations_stability" {
  // 测试混合运算的稳定性
  let a = add(100, 200)        // 300
  let b = multiply(a, 2)       // 600
  let c = add(b, -100)         // 500
  let d = multiply(c, 0)       // 0
  let e = add(d, 42)           // 42
  assert_eq(42, e)
}

///|
test "greet_long_string" {
  // 测试长字符串的处理
  let long_name = "ThisIsAVeryLongNameThatMightCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  let expected = "Hello, " + long_name + "!"
  assert_eq(expected, greet(long_name))
}

///|
test "greet_unicode_and_emoji" {
  // 测试Unicode和emoji字符
  assert_eq("Hello, 🌙!", greet("🌙"))
  assert_eq("Hello, 🚀!", greet("🚀"))
  assert_eq("Hello, ñáéíóú!", greet("ñáéíóú"))
}

///|
test "add_extreme_boundary_combinations" {
  // 测试极端边界值的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值减去自身
  assert_eq(0, add(max_val, -max_val))
  
  // 最小值减去自身
  assert_eq(-1, add(min_val, 2147483647)) // -2147483648 + 2147483647 = -1
  
  // 接近边界的值
  assert_eq(2147483646, add(1073741823, 1073741823))
  assert_eq(-2147483647, add(-1073741824, -1073741823))
}

///|
test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  assert_eq(2, multiply(1, 2))
  assert_eq(4, multiply(2, 2))
  assert_eq(8, multiply(4, 2))
  assert_eq(16, multiply(8, 2))
  assert_eq(32, multiply(16, 2))
  
  // 测试2的幂次方的溢出
  assert_eq(2147483647, multiply(1073741824, 2))
  assert_eq(2147483647, multiply(1073741824, 3))
}

///|
test "multiply_negative_power_patterns" {
  // 测试负数的乘法模式
  assert_eq(1, multiply(-1, -1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(-1, multiply(1, -1))
  
  // 负数的连续乘法
  let mut result = -2
  result = multiply(result, -2)  // 4
  result = multiply(result, -2)  // -8
  result = multiply(result, -2)  // 16
  assert_eq(16, result)
}

///|
test "greet_whitespace_handling" {
  // 测试各种空白字符的处理
  assert_eq("Hello, \t!", greet("\t"))
  assert_eq("Hello, \n!", greet("\n"))
  assert_eq("Hello, \r!", greet("\r"))
  assert_eq("Hello,  \t !", greet(" \t "))
  assert_eq("Hello,  leading!", greet(" leading"))
  assert_eq("Hello, trailing !", greet("trailing "))
}

///|
test "stress_test_repeated_operations" {
  // 压力测试：重复操作
  let mut sum = 0
  // 重复加1然后减1
  for i = 0; i < 100; i = i + 1 {
    sum = add(sum, 1)
    sum = add(sum, -1)
  }
  assert_eq(0, sum)
  
  // 重复乘2然后除2（通过乘0.5的整数近似）
  let mut product = 100
  product = multiply(product, 2)    // 200
  product = multiply(product, 0)    // 0
  product = multiply(product, 100)  // 0
  assert_eq(0, product)
}
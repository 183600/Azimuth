// Azimuth 项目的额外测试用例
// 使用标准 MoonBit 测试语法，覆盖主要功能的边界情况和实际应用场景

test "add_function_negative_numbers" {
  // 测试 add 函数处理负数的情况
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
  assert_eq(-2147483648, add(-2147483648, 0))
}

test "multiply_function_negative_combinations" {
  // 测试 multiply 函数的各种负数组合
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(0, multiply(-5, 0))
  assert_eq(2147483647, multiply(-2147483648, -1))
}

test "greet_function_unicode_handling" {
  // 测试 greet 函数处理各种 Unicode 字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "divide_with_ceil_function" {
  // 测试 divide_with_ceil 函数的各种情况
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(-11, 3))
  assert_eq(-4, divide_with_ceil(-10, -3))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算管道：多个函数的组合使用
  let initial_value = 100
  let doubled = multiply(initial_value, 2)
  let added = add(doubled, 50)
  let final_result = multiply(added, 3)
  
  assert_eq(750, final_result)
  
  // 测试另一个计算管道
  let base = 25
  let expanded = add(base, 75)
  let halved = divide_with_ceil(expanded, 2)
  let processed = multiply(halved, 4)
  
  assert_eq(200, processed)
}

test "edge_case_combinations" {
  // 测试各种边界情况的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算组合
  let sum_result = add(max_val, min_val)
  let mul_result = multiply(max_val, 0)
  let final_result = add(sum_result, mul_result)
  
  assert_eq(-1, final_result)
  
  // 链式运算
  let chain = add(multiply(add(1, 2), 3), multiply(4, 5))
  assert_eq(26, chain)
}

test "real_world_invoice_calculation" {
  // 测试实际应用场景：发票计算
  let item_price = 199
  let quantity = 3
  let tax_rate = 8
  let discount = 50
  
  // 计算商品总价
  let subtotal = multiply(item_price, quantity)
  // 计算税费
  let tax = multiply(subtotal, tax_rate) / 100
  // 应用折扣
  let discounted_total = add(subtotal, tax)
  let final_total = add(discounted_total, -discount)
  
  assert_eq(603, final_total)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<?>!", greet(special_chars))
  
  // 测试包含数字和字母混合的字符串
  let mixed = "User123Admin456"
  assert_eq_string("Hello, User123Admin456!", greet(mixed))
  
  // 测试包含空格的字符串
  let spaced = "  leading and trailing  "
  assert_eq_string("Hello,   leading and trailing  !", greet(spaced))
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算：斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  // 手动计算前几个斐波那契数
  let f0 = 0
  let f1 = 1
  let f2 = add(f0, f1)  // 1
  let f3 = add(f1, f2)  // 2
  let f4 = add(f2, f3)  // 3
  let f5 = add(f3, f4)  // 5
  let f6 = add(f4, f5)  // 8
  let f7 = add(f5, f6)  // 13
  let f8 = add(f6, f7)  // 21
  
  // 验证计算结果
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  assert_eq(8, f6)
  assert_eq(13, f7)
  assert_eq(21, f8)
  
  // 计算前8个斐波那契数的和
  let fib_sum = add(add(add(add(add(add(add(f0, f1), f2), f3), f4), f5), f6), f7)
  assert_eq(33, fib_sum)
}

test "performance_stress_test" {
  // 测试性能压力场景：大量连续运算
  let base = 1000
  
  // 执行10次连续乘法
  let result1 = multiply(base, 2)
  let result2 = multiply(result1, 2)
  let result3 = multiply(result2, 2)
  let result4 = multiply(result3, 2)
  let result5 = multiply(result4, 2)
  let result6 = multiply(result5, 2)
  let result7 = multiply(result6, 2)
  let result8 = multiply(result7, 2)
  let result9 = multiply(result8, 2)
  let result10 = multiply(result9, 2)
  
  assert_eq(1024000, result10)
  
  // 执行10次连续加法
  let add_result1 = add(base, 100)
  let add_result2 = add(add_result1, 100)
  let add_result3 = add(add_result2, 100)
  let add_result4 = add(add_result3, 100)
  let add_result5 = add(add_result4, 100)
  let add_result6 = add(add_result5, 100)
  let add_result7 = add(add_result6, 100)
  let add_result8 = add(add_result7, 100)
  let add_result9 = add(add_result8, 100)
  let add_result10 = add(add_result9, 100)
  
  assert_eq(2000, add_result10)
}
// 综合增强测试套件
// 使用标准的 MoonBit 测试语法，覆盖尚未充分测试的场景

test "performance_stress_test" {
  // 性能压力测试：大量运算的稳定性
  let mut sum = 0
  for i = 1; i <= 100; i = i + 1 {
    sum = add(sum, i)
  }
  assert_eq(5050, sum)  // 1+2+...+100 = 5050
  
  // 测试乘法累加
  let mut product = 1
  for i = 1; i <= 5; i = i + 1 {
    product = multiply(product, i)
  }
  assert_eq(120, product)  // 1*2*3*4*5 = 120
}

test "extreme_boundary_combinations" {
  // 极端边界值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与接近最大值的运算
  assert_eq(max_val, add(max_val - 1000000, 1000000))
  assert_eq(max_val, add(max_val - 1000, 1000))
  
  // 最小值与接近最小值的运算
  assert_eq(min_val, add(min_val + 1000000, -1000000))
  assert_eq(min_val, add(min_val + 1000, -1000))
  
  // 边界值乘法测试
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
}

test "mathematical_identities_verification" {
  // 数学恒等式验证测试
  // 加法交换律
  let a = 12345
  let b = 67890
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律（在安全范围内）
  let c = 11111
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律（在安全范围内）
  assert_eq(multiply(multiply(10, 20), 30), multiply(10, multiply(20, 30)))
  
  // 分配律（在安全范围内）
  assert_eq(multiply(10, add(3, 4)), add(multiply(10, 3), multiply(10, 4)))
}

test "complex_string_scenarios" {
  // 复杂字符串场景测试
  // 包含各种特殊字符的字符串
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, \\\"\\'!", greet("\\\"\\'"))
  assert_eq_string("Hello, 中文字符🌟!", greet("中文字符🌟"))
  assert_eq_string("Hello, emoji😀😁😂!", greet("emoji😀😁😂"))
  
  // 长字符串测试
  let long_string = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()"
  assert_eq_string("Hello, " + long_string + "!", greet(long_string))
  
  // 重复字符字符串
  let repeated = "aaaaabbbbbcccccddddd"
  assert_eq_string("Hello, " + repeated + "!", greet(repeated))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值除法
  assert_eq(1073741824, divide_with_ceil(max_val, 2))
  assert_eq(-1073741824, divide_with_ceil(min_val, 2))
  
  // 小数除法的向上取整
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(1, divide_with_ceil(3, 3))
  
  // 接近整数的除法
  assert_eq(4, divide_with_ceil(13, 4))
  assert_eq(4, divide_with_ceil(14, 4))
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(4, divide_with_ceil(16, 4))
  
  // 负数除法的边界情况
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-1, divide_with_ceil(-2, 3))
  assert_eq(-2, divide_with_ceil(-4, 3))
}

test "nested_function_calls" {
  // 嵌套函数调用测试
  // 深度嵌套的加法
  assert_eq(15, add(add(add(1, 2), 3), add(4, 5)))
  
  // 深度嵌套的乘法
  assert_eq(120, multiply(multiply(2, 3), multiply(4, 5)))
  
  // 混合嵌套运算
  assert_eq(25, add(multiply(3, 5), multiply(2, 5)))
  assert_eq(20, multiply(add(3, 2), 4))
  
  // 复杂嵌套表达式
  let result = add(
    multiply(2, 3),
    divide_with_ceil(add(10, 5), 3)
  )
  assert_eq(11, result)  // 6 + ceil(15/3) = 6 + 5 = 11
}

test "error_recovery_and_stability" {
  // 错误恢复和稳定性测试
  let max_val = 2147483647
  
  // 触发溢出后验证函数仍然可用
  let _ = add(max_val, 1)  // 这会溢出
  assert_eq(8, add(5, 3))  // 验证正常加法仍然工作
  
  let _ = multiply(max_val, 2)  // 这会溢出
  assert_eq(15, multiply(5, 3))  // 验证正常乘法仍然工作
  
  // 连续多次操作的稳定性
  let mut accumulator = 0
  for i = 1; i <= 10; i = i + 1 {
    accumulator = add(accumulator, i)
    assert_true(accumulator >= 0)  // 确保累加器始终非负
  }
  assert_eq(55, accumulator)  // 1+2+...+10 = 55
}

test "data_consistency_validation" {
  // 数据一致性验证测试
  // 相同输入应该产生相同输出
  let test_val = 12345
  let result1 = add(test_val, test_val)
  let result2 = add(test_val, test_val)
  assert_eq(result1, result2)
  
  let result3 = multiply(test_val, 2)
  let result4 = multiply(test_val, 2)
  assert_eq(result3, result4)
  
  // 字符串操作的一致性
  let test_str = "TestString"
  let result5 = greet(test_str)
  let result6 = greet(test_str)
  assert_eq_string(result5, result6)
  
  // 除法操作的一致性
  let result7 = divide_with_ceil(10, 3)
  let result8 = divide_with_ceil(10, 3)
  assert_eq(result7, result8)
}

test "boundary_arithmetic_progressions" {
  // 边界等差数列测试
  // 从接近边界值开始的等差数列
  let max_val = 2147483647
  let start_val = max_val - 10
  
  let mut sum = 0
  for i = 0; i < 5; i = i + 1 {
    sum = add(sum, add(start_val, i))
  }
  assert_eq(max_val, sum)  // (max-10) + (max-9) + ... + (max-6) = 5*max - 40，应该溢出到max
  
  // 负数等差数列
  let min_val = -2147483648
  let start_neg = min_val + 10
  
  let mut sum_neg = 0
  for i = 0; i < 5; i = i + 1 {
    sum_neg = add(sum_neg, add(start_neg, -i))
  }
  assert_eq(min_val, sum_neg)  // 应该溢出到min_val
}

test "comprehensive_assertion_validation" {
  // 综合断言验证测试
  // 测试所有断言函数的组合使用
  assert_true(1 == 1)
  assert_true(0 == 0)
  assert_true(-1 == -1)
  assert_true(true)
  
  assert_false(1 == 2)
  assert_false(false)
  assert_false(1 > 2)
  
  assert_eq(42, 42)
  assert_eq(-42, -42)
  assert_eq(0, 0)
  assert_eq(max_val, max_val)
  
  assert_eq_string("", "")
  assert_eq_string("test", "test")
  assert_eq_string("Hello, World!", "Hello, World!")
  
  // 复杂条件的断言
  let x = 10
  let y = 20
  assert_true(add(x, y) == 30)
  assert_true(multiply(x, y) == 200)
  assert_false(add(x, y) == 25)
  assert_eq_string("Hello, Test!", greet("Test"))
}
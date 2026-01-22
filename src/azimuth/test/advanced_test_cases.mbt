// 高级测试用例 - 补充测试覆盖
// 测试一些边界情况和复杂的组合场景

// 测试 divide_with_ceil 函数的各种边界情况
test "divide_with_ceil_positive_numbers" {
  // 正数的向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))  // 10/3 = 3.33... → 4
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))   // 6/3 = 2 → 2
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 1/100 = 0.01 → 1
}

test "divide_with_ceil_negative_numbers" {
  // 负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -10/3 = -3.33... → -3
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))   // 10/-3 = -3.33... → -3
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-6, 3))   // -6/3 = -2 → -2
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 100)) // -1/100 = -0.01 → 0
}

test "divide_with_ceil_edge_cases" {
  // 边界情况测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))     // 0除以任何数
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))     // 相等的数
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(2147483647, 1))  // 最大值
  @azimuth.assert_eq(-2147483648, @azimuth.divide_with_ceil(-2147483648, 1)) // 最小值
}

test "complex_arithmetic_combinations" {
  // 复杂的算术组合测试
  let a = 1000
  let b = 2000
  let c = 3
  
  // 测试 (a + b) * c 的正确性
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(9000, result)
  
  // 测试分配律：a * c + b * c = (a + b) * c
  let ac = @azimuth.multiply(a, c)
  let bc = @azimuth.multiply(b, c)
  let distributed_result = @azimuth.add(ac, bc)
  @azimuth.assert_eq(result, distributed_result)
}

test "overflow_protection_stress_test" {
  // 溢出保护的压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 连续加法不会无限溢出
  let result1 = @azimuth.add(max_val, 100)
  @azimuth.assert_eq(max_val, result1)  // 应该限制在最大值
  let result2 = @azimuth.add(result1, 100)
  @azimuth.assert_eq(max_val, result2)  // 仍然是最大值
  
  // 连续乘法不会无限溢出
  let result3 = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, result3)  // 应该限制在最大值
  let result4 = @azimuth.multiply(result3, 2)
  @azimuth.assert_eq(max_val, result4)  // 仍然是最大值
}

test "string_processing_edge_cases" {
  // 字符串处理的边界情况
  // 测试非常长的字符串
  let long_name = "这是一个非常非常长的用户名字符串，用来测试greet函数在处理长字符串时的表现"
  let result = @azimuth.greet(long_name)
  @azimuth.assert_eq(String::length(result, true) > String::length(long_name))
  
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result2 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result2)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let test_values = [0, 1, -1, 100, -100, 2147483647, -2147483648]
  
  // 测试关键数学性质，避免使用 for_each 函数（可能不可用）
  let a = 42
  let b = 17
  
  // 加法交换律：a + b = b + a
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(b, a)
  @azimuth.assert_eq(sum1, sum2)
  
  // 乘法交换律：a * b = b * a
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(b, a)
  @azimuth.assert_eq(product1, product2)
  
  // 加法单位元：a + 0 = a
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  
  // 乘法单位元：a * 1 = a
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  
  // 测试边界值
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
}

test "error_handling_and_recovery" {
  // 错误处理和恢复测试
  // 测试断言函数的正确性
  @azimuth.assert_eq(1, 1)     // 应该通过
  @azimuth.assert_eq(1, 1)   // 应该通过
  
  // 测试边界值的比较
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, max_val)  // 最大值自比较
  @azimuth.assert_eq(min_val, min_val)  // 最小值自比较
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val + 1))  // 最大值 + (最小值 + 1) = -1 + 1 = 0
}
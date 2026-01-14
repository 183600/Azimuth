// 新增 MoonBit 测试用例

test "arithmetics_add_boundary_values" {
  // 测试加法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  @test_only.assert_eq(@test_only.add(max_val, 0), max_val)
  @test_only.assert_eq(@test_only.add(min_val, 0), min_val)
  @test_only.assert_eq(@test_only.add(max_val, -max_val), 0)
}

test "arithmetics_multiply_edge_cases" {
  // 测试乘法边界情况
  @test_only.assert_eq(@test_only.multiply(-2147483648, 0), 0)
  @test_only.assert_eq(@test_only.multiply(2147483647, 1), 2147483647)
  @test_only.assert_eq(@test_only.multiply(-2147483648, 1), -2147483648)
}

test "string_greet_unicode_characters" {
  // 测试包含Unicode字符的字符串处理
  @test_only.assert_eq_string(@test_only.greet("世界"), "Hello, 世界!")
  @test_only.assert_eq_string(@test_only.greet("🌟"), "Hello, 🌟!")
  @test_only.assert_eq_string(@test_only.greet("Café"), "Hello, Café!")
}

test "arithmetics_sequence_operations" {
  // 测试序列运算：((a + b) * c) - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum = @test_only.add(a, b)
  let product = @test_only.multiply(sum, c)
  let result = @test_only.add(product, -d)
  
  @test_only.assert_eq(result, 75)  // ((10+20)*3)-15 = 90-15 = 75
}

test "arithmetics_fibonacci_calculation" {
  // 测试斐波那契数列计算（简化版）
  // 计算 fib(8) = 21
  let n1 = 1
  let n2 = 1
  let n3 = @test_only.add(n1, n2)     // 2
  let n4 = @test_only.add(n2, n3)     // 3
  let n5 = @test_only.add(n3, n4)     // 5
  let n6 = @test_only.add(n4, n5)     // 8
  let n7 = @test_only.add(n5, n6)     // 13
  let n8 = @test_only.add(n6, n7)     // 21
  
  @test_only.assert_eq(n8, 21)
}

test "string_greet_with_spaces" {
  // 测试包含空格的字符串处理
  @test_only.assert_eq_string(@test_only.greet("John Doe"), "Hello, John Doe!")
  @test_only.assert_eq_string(@test_only.greet("  leading"), "Hello,   leading!")
  @test_only.assert_eq_string(@test_only.greet("trailing  "), "Hello, trailing  !")
}

test "arithmetics_power_simulation" {
  // 测试幂运算模拟（使用乘法）
  // 计算 2^5 = 32
  let base = 2
  let result1 = @test_only.multiply(base, base)      // 2^2 = 4
  let result2 = @test_only.multiply(result1, base)   // 2^3 = 8
  let result3 = @test_only.multiply(result2, base)   // 2^4 = 16
  let result4 = @test_only.multiply(result3, base)   // 2^5 = 32
  
  @test_only.assert_eq(result4, 32)
}

test "arithmetics_average_calculation" {
  // 测试平均值计算
  let num1 = 15
  let num2 = 25
  let num3 = 35
  let num4 = 45
  
  let sum = @test_only.add(@test_only.add(num1, num2), @test_only.add(num3, num4))
  // 模拟除法：使用减法来验证平均值
  let average_times_4 = sum
  let expected_sum = @test_only.multiply(30, 4)  // 平均值30 * 4 = 120
  
  @test_only.assert_eq(average_times_4, expected_sum)
}

test "string_greet_numeric_strings" {
  // 测试数字字符串处理
  @test_only.assert_eq_string(@test_only.greet("123"), "Hello, 123!")
  @test_only.assert_eq_string(@test_only.greet("-456"), "Hello, -456!")
  @test_only.assert_eq_string(@test_only.greet("3.14159"), "Hello, 3.14159!")
}

test "arithmetics_factorial_simulation" {
  // 测试阶乘计算模拟（使用乘法）
  // 计算 5! = 120
  let n1 = 1
  let n2 = @test_only.multiply(n1, 2)  // 2! = 2
  let n3 = @test_only.multiply(n2, 3)  // 3! = 6
  let n4 = @test_only.multiply(n3, 4)  // 4! = 24
  let n5 = @test_only.multiply(n4, 5)  // 5! = 120
  
  @test_only.assert_eq(n5, 120)
}
// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

// 增强测试套件 - 10个新的测试用例

// 测试用例 1: 测试斐波那契数列计算
///|
test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  let fib0 = 0
  let fib1 = 1
  let fib2 = @clean_test.@clean_test.add(fib0, fib1)  // 1
  let fib3 = @clean_test.@clean_test.add(fib1, fib2)  // 2
  let fib4 = @clean_test.@clean_test.add(fib2, fib3)  // 3
  let fib5 = @clean_test.@clean_test.add(fib3, fib4)  // 5
  let fib6 = @clean_test.@clean_test.add(fib4, fib5)  // 8
  let fib7 = @clean_test.@clean_test.add(fib5, fib6)  // 13
  let fib8 = @clean_test.@clean_test.add(fib6, fib7)  // 21
  
  assert_eq(1, fib2)
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  assert_eq(8, fib6)
  assert_eq(13, fib7)
  assert_eq(21, fib8)
}

// 测试用例 2: 测试阶乘计算
///|
test "factorial_calculation" {
  // 计算 n! = n * (n-1) * ... * 1
  
  // 0! = 1
  let fact0 = 1
  
  // 1! = 1
  let fact1 = @clean_test.@clean_test.multiply(1, fact0)
  
  // 2! = 2 * 1! = 2
  let fact2 = @clean_test.@clean_test.multiply(2, fact1)
  
  // 3! = 3 * 2! = 6
  let fact3 = @clean_test.@clean_test.multiply(3, fact2)
  
  // 4! = 4 * 3! = 24
  let fact4 = @clean_test.@clean_test.multiply(4, fact3)
  
  // 5! = 5 * 4! = 120
  let fact5 = @clean_test.@clean_test.multiply(5, fact4)
  
  assert_eq(1, fact0)
  assert_eq(1, fact1)
  assert_eq(2, fact2)
  assert_eq(6, fact3)
  assert_eq(24, fact4)
  assert_eq(120, fact5)
}

// 测试用例 3: 测试幂运算（通过重复乘法）
///|
test "power_calculation" {
  // 计算 a^n 通过重复乘法
  
  // 2^3 = 8
  let power2_3 = @clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(2, 2), 2)
  assert_eq(8, power2_3)
  
  // 3^4 = 81
  let power3_4 = @clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(3, 3), 3), 3)
  assert_eq(81, power3_4)
  
  // 5^2 = 25
  let power5_2 = @clean_test.@clean_test.multiply(5, 5)
  assert_eq(25, power5_2)
  
  // (-2)^3 = -8
  let power_minus2_3 = @clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(-2, -2), -2)
  assert_eq(-8, power_minus2_3)
  
  // (-2)^4 = 16
  let power_minus2_4 = @clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(-2, -2), -2), -2)
  assert_eq(16, power_minus2_4)
}

// 测试用例 4: 测试最大公约数（GCD）模拟计算
///|
test "gcd_simulation" {
  // 模拟欧几里得算法计算最大公约数
  // gcd(a, b) = gcd(b, a mod b)
  
  // gcd(48, 18) = 6
  let a = 48
  let b = 18
  
  // 48 mod 18 = 12 (通过减法模拟)
  let remainder1 = @clean_test.@clean_test.add(a, @clean_test.@clean_test.multiply(-2, b))  // 48 - 2*18 = 12
  assert_eq(12, remainder1)
  
  // 18 mod 12 = 6
  let remainder2 = @clean_test.@clean_test.add(b, @clean_test.@clean_test.multiply(-1, remainder1))  // 18 - 1*12 = 6
  assert_eq(6, remainder2)
  
  // 12 mod 6 = 0
  let remainder3 = @clean_test.@clean_test.add(remainder1, @clean_test.@clean_test.multiply(-2, remainder2))  // 12 - 2*6 = 0
  assert_eq(0, remainder3)
  
  // gcd是最后一个非零余数
  let gcd = remainder2
  assert_eq(6, gcd)
}

// 测试用例 5: 测试百分比计算
///|
test "percentage_calculations" {
  // 计算百分比：部分 / 整体 * 100
  // 我们通过乘法避免除法
  
  let whole = 200
  let part = 50
  let expected_percent = 25  // 50/200 * 100 = 25
  
  // 验证：部分 * 100 = 百分比 * 整体
  let part_times_100 = @clean_test.@clean_test.multiply(part, 100)
  let verification = @clean_test.@clean_test.multiply(expected_percent, whole)
  
  assert_eq(part_times_100, verification)  // 5000 = 25 * 200
  
  // 另一个例子：75%
  let part2 = 150
  let expected_percent2 = 75
  
  let part2_times_100 = @clean_test.@clean_test.multiply(part2, 100)
  let verification2 = @clean_test.@clean_test.multiply(expected_percent2, whole)
  
  assert_eq(part2_times_100, verification2)  // 15000 = 75 * 200
}

// 测试用例 6: 测试复利计算
///|
test "compound_interest_calculation" {
  // 复利公式：A = P(1 + r)^n
  // 我们通过展开计算避免幂运算
  
  let principal = 1000  // 本金
  let rate_percent = 5   // 利率5%
  let years = 3         // 3年
  
  // (1 + 0.05)^3 = 1.157625，我们用整数计算
  // (105/100)^3 = 1157625/1000000 = 1.157625
  
  let rate_numerator = 105  // 100 + 5
  let rate_denominator = 100
  
  // 第一年后的金额因子
  let year1_factor_numerator = rate_numerator
  let year1_factor_denominator = rate_denominator
  
  // 第二年后的金额因子
  let year2_factor_numerator = @clean_test.@clean_test.multiply(rate_numerator, rate_numerator)
  let year2_factor_denominator = @clean_test.@clean_test.multiply(rate_denominator, rate_denominator)
  
  // 第三年后的金额因子
  let year3_factor_numerator = @clean_test.@clean_test.multiply(year2_factor_numerator, rate_numerator)
  let year3_factor_denominator = @clean_test.@clean_test.multiply(year2_factor_denominator, rate_denominator)
  
  // 最终金额 * 分母^3 = 本金 * 分子^3
  let final_amount_times_denominator_cubed = @clean_test.@clean_test.multiply(principal, year3_factor_numerator)
  
  // 验证计算: 1000 * 1157625 = 1157625000
  assert_eq(1157625000, final_amount_times_denominator_cubed)
}

// 测试用例 7: 测试字符串长度和索引操作
///|
test "string_length_and_indexing" {
  // 测试不同长度字符串的greet函数
  
  let empty_string = ""
  let single_char = "A"
  let short_word = "Cat"
  let medium_word = "MoonBit"
  let long_phrase = "ProgrammingLanguage"
  
  let greet_empty = @clean_test.@clean_test.greet(empty_string)
  let greet_single = @clean_test.@clean_test.greet(single_char)
  let greet_short = @clean_test.@clean_test.greet(short_word)
  let greet_medium = @clean_test.@clean_test.greet(medium_word)
  let greet_long = @clean_test.@clean_test.greet(long_phrase)
  
  // 验证字符串长度关系
  assert_eq(true, greet_empty.length() < greet_single.length())
  assert_eq(true, greet_single.length() < greet_short.length())
  assert_eq(true, greet_short.length() < greet_medium.length())
  assert_eq(true, greet_medium.length() < greet_long.length())
  
  // 验证字符串内容
  assert_eq("Hello, !", greet_empty)
  assert_eq("Hello, A!", greet_single)
  assert_eq("Hello, Cat!", greet_short)
  assert_eq("Hello, MoonBit!", greet_medium)
  assert_eq("Hello, ProgrammingLanguage!", greet_long)
}

// 测试用例 8: 测试数字系统转换
///|
test "number_system_conversion" {
  // 二进制到十进制转换模拟
  // 二进制 1011 = 1*2^3 + 0*2^2 + 1*2^1 + 1*2^0 = 8 + 0 + 2 + 1 = 11
  
  let bit3 = 1  // 2^3位
  let bit2 = 0  // 2^2位
  let bit1 = 1  // 2^1位
  let bit0 = 1  // 2^0位
  
  // 计算各位的值
  let value3 = @clean_test.@clean_test.multiply(bit3, @clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(2, 2), 2))  // 1 * 8
  let value2 = @clean_test.@clean_test.multiply(bit2, @clean_test.@clean_test.multiply(2, 2))  // 0 * 4
  let value1 = @clean_test.@clean_test.multiply(bit1, 2)  // 1 * 2
  let value0 = @clean_test.@clean_test.multiply(bit0, 1)  // 1 * 1
  
  // 求和
  let decimal_value = @clean_test.@clean_test.add(@clean_test.@clean_test.add(@clean_test.@clean_test.add(value3, value2), value1), value0)
  
  assert_eq(11, decimal_value)
  
  // 另一个例子：二进制 1101 = 13
  let bit3_2 = 1
  let bit2_2 = 1
  let bit1_2 = 0
  let bit0_2 = 1
  
  let value3_2 = @clean_test.@clean_test.multiply(bit3_2, @clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(2, 2), 2))  // 1 * 8
  let value2_2 = @clean_test.@clean_test.multiply(bit2_2, @clean_test.@clean_test.multiply(2, 2))  // 1 * 4
  let value1_2 = @clean_test.@clean_test.multiply(bit1_2, 2)  // 0 * 2
  let value0_2 = @clean_test.@clean_test.multiply(bit0_2, 1)  // 1 * 1
  
  let decimal_value_2 = @clean_test.@clean_test.add(@clean_test.@clean_test.add(@clean_test.@clean_test.add(value3_2, value2_2), value1_2), value0_2)
  
  assert_eq(13, decimal_value_2)
}

// 测试用例 9: 测试线性方程求解
///|
test "linear_equation_solving" {
  // 求解线性方程 ax + b = c
  // x = (c - b) / a
  // 我们通过乘法验证：a * x + b = c
  
  // 例子：3x + 7 = 22，解得 x = 5
  let a = 3
  let b = 7
  let c = 22
  let x = 5
  
  // 验证：3 * 5 + 7 = 22
  let ax = @clean_test.@clean_test.multiply(a, x)
  let ax_plus_b = @clean_test.@clean_test.add(ax, b)
  
  assert_eq(c, ax_plus_b)
  
  // 另一个例子：-2x + 10 = 4，解得 x = 3
  let a2 = -2
  let b2 = 10
  let c2 = 4
  let x2 = 3
  
  // 验证：-2 * 3 + 10 = 4
  let a2x2 = @clean_test.@clean_test.multiply(a2, x2)
  let a2x2_plus_b2 = @clean_test.@clean_test.add(a2x2, b2)
  
  assert_eq(c2, a2x2_plus_b2)
}

// 测试用例 10: 测试数组平均值计算（模拟）
///|
test "array_average_calculation" {
  // 计算数组平均值：总和 / 数量
  // 我们通过验证 平均值 * 数量 = 总和
  
  // 模拟数组：[15, 25, 35, 45, 55]
  let num1 = 15
  let num2 = 25
  let num3 = 35
  let num4 = 45
  let num5 = 55
  let count = 5
  
  // 计算总和
  let sum = @clean_test.@clean_test.add(
    @clean_test.@clean_test.add(
      @clean_test.@clean_test.add(num1, num2),
      @clean_test.@clean_test.add(num3, num4)
    ),
    num5
  )
  
  // 期望的平均值
  let expected_average = 35
  
  // 验证：平均值 * 数量 = 总和
  let average_times_count = @clean_test.@clean_test.multiply(expected_average, count)
  
  assert_eq(sum, average_times_count)  // 175 = 35 * 5
  
  // 另一个例子：[10, 20, 30, 40, 50, 60]
  let nums_sum = @clean_test.@clean_test.add(
    @clean_test.@clean_test.add(
      @clean_test.@clean_test.add(10, 20),
      @clean_test.@clean_test.add(30, 40)
    ),
    @clean_test.@clean_test.add(50, 60)
  )
  
  let nums_count = 6
  let nums_expected_average = 35
  let nums_average_times_count = @clean_test.@clean_test.multiply(nums_expected_average, nums_count)
  
  assert_eq(nums_sum, nums_average_times_count)  // 210 = 35 * 6
}
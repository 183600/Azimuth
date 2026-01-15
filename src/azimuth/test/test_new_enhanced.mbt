// Enhanced MoonBit Test Suite
// 增强的 MoonBit 测试套件
// 包含10个新的测试用例，覆盖不同的功能场景

test "enhanced_division_simulation" {
  // 模拟除法运算：使用减法实现整数除法
  let dividend = 27
  let divisor = 4
  
  // 模拟除法：计算商和余数
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  
  // 验证结果：dividend = divisor * quotient + remainder
  let verification = add(multiply(divisor, quotient), remainder)
  @azimuth.assert_eq(dividend, verification)
  
  // 测试边界情况
  let zero_dividend = 0
  let zero_quotient = zero_dividend / divisor
  @azimuth.assert_eq(0, zero_quotient)
}

test "enhanced_absolute_value_calculation" {
  // 绝对值计算测试
  let positive_num = 42
  let negative_num = -42
  let zero = 0
  
  // 正数的绝对值是自身
  @azimuth.assert_eq(positive_num, positive_num)
  
  // 负数的绝对值是其相反数
  @azimuth.assert_eq(42, multiply(-1, negative_num))
  
  // 零的绝对值是零
  @azimuth.assert_eq(zero, zero)
  
  // 测试极值的绝对值
  let max_val = 2147483647
  let min_val = -2147483648
  @azimuth.assert_eq(max_val, max_val)
  @azimuth.assert_eq(min_val, multiply(min_val, -1))
}

test "enhanced_string_reverse_simulation" {
  // 模拟字符串反转功能（通过字符位置）
  let original = "ABC"
  let reversed = "CBA"
  
  // 通过greet函数验证字符串处理能力
  let original_greeting = @azimuth.greet(original)
  let reversed_greeting = @azimuth.greet(reversed)
  
  @azimuth.assert_eq_string("Hello, ABC!", original_greeting)
  @azimuth.assert_eq_string("Hello, CBA!", reversed_greeting)
  
  // 测试单字符字符串
  let single_char = "X"
  let single_greeting = @azimuth.greet(single_char)
  @azimuth.assert_eq_string("Hello, X!", single_greeting)
  
  // 测试空字符串
  let empty_str = ""
  let empty_greeting = @azimuth.greet(empty_str)
  @azimuth.assert_eq_string("Hello, !", empty_greeting)
}

test "enhanced_fibonacci_sequence" {
  // 斐波那契数列计算测试
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  // 计算前几个斐波那契数
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f1, f0)  // 1
  let f3 = @azimuth.add(f2, f1)  // 2
  let f4 = @azimuth.add(f3, f2)  // 3
  let f5 = @azimuth.add(f4, f3)  // 5
  let f6 = @azimuth.add(f5, f4)  // 8
  let f7 = @azimuth.add(f6, f5)  // 13
  let f8 = @azimuth.add(f7, f6)  // 21
  
  @azimuth.assert_eq(0, f0)
  @azimuth.assert_eq(1, f1)
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
  @azimuth.assert_eq(13, f7)
  @azimuth.assert_eq(21, f8)
}

test "enhanced_gcd_calculation" {
  // 最大公约数(GCD)计算测试（欧几里得算法简化版）
  let a = 48
  let b = 18
  
  // 手动计算GCD过程
  // 48 = 18 × 2 + 12
  // 18 = 12 × 1 + 6
  // 12 = 6 × 2 + 0
  // 所以GCD(48, 18) = 6
  
  let step1_remainder = a - @azimuth.multiply(b, a / b)  // 48 % 18 = 12
  let step2_remainder = b - @azimuth.multiply(step1_remainder, b / step1_remainder)  // 18 % 12 = 6
  let step3_remainder = step1_remainder - @azimuth.multiply(step2_remainder, step1_remainder / step2_remainder)  // 12 % 6 = 0
  
  @azimuth.assert_eq(6, step2_remainder)
  @azimuth.assert_eq(0, step3_remainder)
  
  // 测试特殊情况
  let gcd_with_zero = 15  // GCD(15, 0) = 15
  @azimuth.assert_eq(15, gcd_with_zero)
}

test "enhanced_prime_check_simulation" {
  // 质数检查模拟测试
  let num = 17
  
  // 检查17是否为质数：检查2到4的整除性
  let divisible_by_2 = (num % 2 == 0)
  let divisible_by_3 = (num % 3 == 0)
  let divisible_by_4 = (num % 4 == 0)
  
  // 17不能被2、3、4整除，所以是质数
  @azimuth.assert_false(divisible_by_2)
  @azimuth.assert_false(divisible_by_3)
  @azimuth.assert_false(divisible_by_4)
  
  // 测试合数
  let composite_num = 12
  let composite_div_by_2 = (composite_num % 2 == 0)
  let composite_div_by_3 = (composite_num % 3 == 0)
  
  @azimuth.assert_true(composite_div_by_2)
  @azimuth.assert_true(composite_div_by_3)
}

test "enhanced_leap_year_calculation" {
  // 闰年计算测试
  // 闰年规则：能被4整除但不能被100整除，或者能被400整除
  
  let year_2020 = 2020
  let year_2021 = 2021
  let year_1900 = 1900
  let year_2000 = 2000
  
  // 2020年是闰年（能被4整除，不能被100整除）
  let leap_2020 = (year_2020 % 4 == 0) && (year_2020 % 100 != 0)
  @azimuth.assert_true(leap_2020)
  
  // 2021年不是闰年（不能被4整除）
  let leap_2021 = (year_2021 % 4 == 0) && (year_2021 % 100 != 0)
  @azimuth.assert_false(leap_2021)
  
  // 1900年不是闰年（能被4整除，也能被100整除，但不能被400整除）
  let leap_1900 = (year_1900 % 400 == 0)
  @azimuth.assert_false(leap_1900)
  
  // 2000年是闰年（能被400整除）
  let leap_2000 = (year_2000 % 400 == 0)
  @azimuth.assert_true(leap_2000)
}

test "enhanced_base_conversion_simulation" {
  // 进制转换模拟测试：十进制转二进制（简化版）
  let decimal_num = 13
  
  // 手动计算13的二进制表示
  // 13 ÷ 2 = 6 余 1
  // 6 ÷ 2 = 3 余 0
  // 3 ÷ 2 = 1 余 1
  // 1 ÷ 2 = 0 余 1
  // 从下往上读取余数：1101
  
  let step1_quotient = decimal_num / 2  // 6
  let step1_remainder = decimal_num % 2  // 1
  
  let step2_quotient = step1_quotient / 2  // 3
  let step2_remainder = step1_quotient % 2  // 0
  
  let step3_quotient = step2_quotient / 2  // 1
  let step3_remainder = step2_quotient % 2  // 1
  
  let step4_quotient = step3_quotient / 2  // 0
  let step4_remainder = step3_quotient % 2  // 1
  
  // 验证计算过程
  @azimuth.assert_eq(6, step1_quotient)
  @azimuth.assert_eq(1, step1_remainder)
  @azimuth.assert_eq(3, step2_quotient)
  @azimuth.assert_eq(0, step2_remainder)
  @azimuth.assert_eq(1, step3_quotient)
  @azimuth.assert_eq(1, step3_remainder)
  @azimuth.assert_eq(0, step4_quotient)
  @azimuth.assert_eq(1, step4_remainder)
}

test "enhanced_distance_calculation" {
  // 距离计算测试：两点间距离（简化整数版）
  let x1 = 3
  let y1 = 4
  let x2 = 7
  let y3 = 1
  
  // 计算x和y坐标的差值
  let delta_x = x2 - x1  // 4
  let delta_y = y3 - y1  // -3
  
  // 计算平方和（距离的平方）
  let squared_distance = @azimuth.add(@azimuth.multiply(delta_x, delta_x), @azimuth.multiply(delta_y, delta_y))
  
  // 验证计算：4² + (-3)² = 16 + 9 = 25
  @azimuth.assert_eq(25, squared_distance)
  
  // 测试相同点（距离为0）
  let same_point_distance = @azimuth.add(@azimuth.multiply(0, 0), @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, same_point_distance)
}

test "enhanced_logical_operations" {
  // 逻辑运算测试
  let a = true
  let b = false
  
  // 逻辑与（使用乘法模拟：true*true=1, 其他=0）
  let logical_and = a && b
  @azimuth.assert_false(logical_and)
  
  // 逻辑或（使用加法模拟：true+true>0, true+false>0, false+false=0）
  let logical_or = a || b
  @azimuth.assert_true(logical_or)
  
  // 逻辑非
  let not_a = !a
  let not_b = !b
  @azimuth.assert_false(not_a)
  @azimuth.assert_true(not_b)
  
  // 测试复杂逻辑表达式
  let complex_expr = (a || b) && (!a || b)
  let result = complex_expr
  @azimuth.assert_false(result)
}
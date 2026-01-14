// 新增 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "binary_number_operations" {
  // 测试二进制数相关的运算
  // 模拟左移操作：x << n = x * 2^n
  let x = 5
  let n = 3
  let left_shift_result = @azimuth.multiply(x, @azimuth.multiply(2, @azimuth.multiply(2, 2)))
  @azimuth.assert_eq(40, left_shift_result)
  
  // 模拟右移操作：x >> n = x / 2^n
  let right_shift_result = x / (@azimuth.multiply(2, 2))
  @azimuth.assert_eq(1, right_shift_result)
}

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的第n项（简化版）
  // F(5) = F(4) + F(3) = (F(3) + F(2)) + (F(2) + F(1)) = 5
  let f1 = 1
  let f2 = 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  
  @azimuth.assert_eq(5, f5)
  
  // 验证斐波那契性质：F(n+1) * F(n-1) - F(n)^2 = (-1)^n
  let n = 5
  let left_side = @azimuth.add(@azimuth.multiply(f5, f3), @azimuth.multiply(-f4, f4))
  @azimuth.assert_eq(-1, left_side)
}

test "greatest_common_divisor" {
  // 计算最大公约数（简化版欧几里得算法）
  let a = 48
  let b = 18
  
  // 48 % 18 = 12
  // 18 % 12 = 6
  // 12 % 6 = 0
  // 所以 GCD(48, 18) = 6
  
  let remainder1 = a % b  // 12
  let remainder2 = b % remainder1  // 6
  let remainder3 = remainder1 % remainder2  // 0
  
  @azimuth.assert_eq(0, remainder3)
  @azimuth.assert_eq(6, remainder2)
}

test "prime_number_check" {
  // 质数检查（简化版）
  let num = 17
  
  // 检查能否被2到16之间的数整除
  let divisible_by_2 = (num % 2 == 0)
  let divisible_by_3 = (num % 3 == 0)
  let divisible_by_5 = (num % 5 == 0)
  
  // 17是质数，所以都不能整除
  @azimuth.assert_false(divisible_by_2)
  @azimuth.assert_false(divisible_by_3)
  @azimuth.assert_false(divisible_by_5)
  
  // 测试非质数
  let composite = 15
  let composite_divisible_by_3 = (composite % 3 == 0)
  let composite_divisible_by_5 = (composite % 5 == 0)
  
  @azimuth.assert_true(composite_divisible_by_3 || composite_divisible_by_5)
}

test "base_conversion" {
  // 进制转换（十进制转其他进制）
  let decimal = 42
  
  // 转换为二进制（简化版）
  let binary_32 = decimal % 2  // 0
  let binary_16 = (decimal / 2) % 2  // 1
  let binary_8 = (decimal / 4) % 2   // 0
  let binary_4 = (decimal / 8) % 2   // 1
  let binary_2 = (decimal / 16) % 2  // 0
  let binary_1 = (decimal / 32) % 2  // 1
  
  // 验证二进制转回十进制：101010 = 32 + 8 + 2 = 42
  let back_to_decimal = @azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(binary_1, 32),
    @azimuth.multiply(binary_2, 16)
  ), @azimuth.add(
    @azimuth.multiply(binary_4, 8),
    @azimuth.multiply(binary_8, 4)
  )), @azimuth.add(
    @azimuth.multiply(binary_16, 2),
    binary_32
  ))
  
  @azimuth.assert_eq(decimal, back_to_decimal)
}

test "quadratic_equation" {
  // 解二次方程 ax² + bx + c = 0（特殊情况）
  // 解 x² - 5x + 6 = 0，根为 x=2 和 x=3
  
  let a = 1
  let b = -5
  let c = 6
  
  // 验证 x=2 是根
  let x1 = 2
  let x1_squared = @azimuth.multiply(x1, x1)  // 4
  let ax1_squared = @azimuth.multiply(a, x1_squared)  // 4
  let bx1 = @azimuth.multiply(b, x1)  // -10
  let equation_value1 = @azimuth.add(@azimuth.add(ax1_squared, bx1), c)  // 0
  
  @azimuth.assert_eq(0, equation_value1)
  
  // 验证 x=3 是根
  let x2 = 3
  let x2_squared = @azimuth.multiply(x2, x2)  // 9
  let ax2_squared = @azimuth.multiply(a, x2_squared)  // 9
  let bx2 = @azimuth.multiply(b, x2)  // -15
  let equation_value2 = @azimuth.add(@azimuth.add(ax2_squared, bx2), c)  // 0
  
  @azimuth.assert_eq(0, equation_value2)
}

test "string_palindrome_check" {
  // 字符串回文检查
  let palindrome = "level"
  let reversed_palindrome = "level"  // 简化版，直接写反转结果
  
  @azimuth.assert_eq_string(palindrome, reversed_palindrome)
  
  let non_palindrome = "hello"
  let reversed_non_palindrome = "olleh"
  
  @azimuth.assert_false(non_palindrome == reversed_non_palindrome)
  
  // 测试数字回文
  let num_palindrome_str = "12321"
  let reversed_num_palindrome = "12321"
  
  @azimuth.assert_eq_string(num_palindrome_str, reversed_num_palindrome)
}

test "factorial_calculation" {
  // 计算阶乘 n! = n × (n-1) × ... × 1
  let n = 6
  
  // 6! = 6 × 5 × 4 × 3 × 2 × 1 = 720
  let factorial = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(
    n - 2, @azimuth.multiply(n - 3, @azimuth.multiply(n - 4, n - 5)
  ))))
  
  @azimuth.assert_eq(720, factorial)
  
  // 验证阶乘性质：n! = n × (n-1)!
  let n_minus_1_factorial = @azimuth.multiply(n - 1, @azimuth.multiply(
    n - 2, @azimuth.multiply(n - 3, @azimuth.multiply(n - 4, n - 5)
  )))
  let factorial_property = @azimuth.multiply(n, n_minus_1_factorial)
  
  @azimuth.assert_eq(factorial, factorial_property)
}

test "power_calculation" {
  // 幂运算计算（简化版）
  let base = 3
  let exponent = 4
  
  // 3^4 = 3 × 3 × 3 × 3 = 81
  let power = @azimuth.multiply(base, @azimuth.multiply(base, @azimuth.multiply(base, base)))
  
  @azimuth.assert_eq(81, power)
  
  // 验证幂运算性质：a^m × a^n = a^(m+n)
  let m = 2
  let n = 3
  let a = 2
  
  let a_to_m = @azimuth.multiply(a, a)  // 4
  let a_to_n = @azimuth.multiply(@azimuth.multiply(a, a), a)  // 8
  let product = @azimuth.multiply(a_to_m, a_to_n)  // 32
  
  let a_to_m_plus_n = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(
    a, a), a), @azimuth.multiply(a, a))  // 32
  
  @azimuth.assert_eq(product, a_to_m_plus_n)
}

test "triangle_properties" {
  // 三角形性质验证
  let side_a = 3
  let side_b = 4
  let side_c = 5
  
  // 验证三角形不等式：a + b > c, a + c > b, b + c > a
  let inequality1 = @azimuth.add(side_a, side_b) > side_c
  let inequality2 = @azimuth.add(side_a, side_c) > side_b
  let inequality3 = @azimuth.add(side_b, side_c) > side_a
  
  @azimuth.assert_true(inequality1)
  @azimuth.assert_true(inequality2)
  @azimuth.assert_true(inequality3)
  
  // 验证勾股定理（直角三角形）：a² + b² = c²
  let a_squared = @azimuth.multiply(side_a, side_a)  // 9
  let b_squared = @azimuth.multiply(side_b, side_b)  // 16
  let c_squared = @azimuth.multiply(side_c, side_c)  // 25
  
  let sum_of_squares = @azimuth.add(a_squared, b_squared)  // 25
  
  @azimuth.assert_eq(c_squared, sum_of_squares)
}
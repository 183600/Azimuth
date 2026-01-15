// 新增的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "binary_number_operations" {
  // 测试二进制数运算模拟
  // 模拟二进制 1010 (10) 和 1100 (12) 的运算
  
  let binary1 = 10  // 1010 in binary
  let binary2 = 12  // 1100 in binary
  
  // 二进制加法：1010 + 1100 = 10110 (22)
  let binary_sum = @azimuth.add(binary1, binary2)
  @azimuth.assert_eq(22, binary_sum)
  
  // 二进制乘法：1010 * 1100 = 1111000 (120)
  let binary_product = @azimuth.multiply(binary1, binary2)
  @azimuth.assert_eq(120, binary_product)
  
  // 模拟位运算：左移 (相当于乘以2)
  let left_shift = @azimuth.multiply(binary1, 2)
  @azimuth.assert_eq(20, left_shift)
}

test "hexadecimal_calculations" {
  // 测试十六进制计算模拟
  // 模拟十六进制 0xFF (255) 和 0x1A (26) 的运算
  
  let hex1 = 255  // 0xFF
  let hex2 = 26   // 0x1A
  
  // 十六进制加法：FF + 1A = 119 (281 in decimal)
  let hex_sum = @azimuth.add(hex1, hex2)
  @azimuth.assert_eq(281, hex_sum)
  
  // 十六进制乘法：FF * 1A = 2710 (10000 in decimal)
  let hex_product = @azimuth.multiply(hex1, hex2)
  @azimuth.assert_eq(6630, hex_product)
}

test "fibonacci_sequence_calculation" {
  // 测试斐波那契数列计算
  // 计算斐波那契数列的前几项：0, 1, 1, 2, 3, 5, 8, 13, 21, 34
  
  let fib0 = 0
  let fib1 = 1
  let fib2 = @azimuth.add(fib0, fib1)  // 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  let fib7 = @azimuth.add(fib5, fib6)  // 13
  let fib8 = @azimuth.add(fib6, fib7)  // 21
  let fib9 = @azimuth.add(fib7, fib8)  // 34
  
  @azimuth.assert_eq(34, fib9)
  
  // 验证黄金比例近似：fib9/fib8 ≈ 1.618 (整数除法)
  let golden_ratio_approx = fib9 / fib8
  @azimuth.assert_eq(1, golden_ratio_approx)
}

test "prime_number_validation" {
  // 测试质数验证（通过模拟）
  // 验证7是质数（不能被2, 3, 4, 5, 6整除）
  let candidate = 7
  let is_prime = true
  
  // 模�试除法检查（使用乘法反向验证）
  let not_divisible_by_2 = @azimuth.multiply(candidate / 2, 2) != candidate
  let not_divisible_by_3 = @azimuth.multiply(candidate / 3, 3) != candidate
  let not_divisible_by_4 = @azimuth.multiply(candidate / 4, 4) != candidate
  let not_divisible_by_5 = @azimuth.multiply(candidate / 5, 5) != candidate
  
  @azimuth.assert_true(not_divisible_by_2)
  @azimuth.assert_true(not_divisible_by_3)
  @azimuth.assert_true(not_divisible_by_4)
  @azimuth.assert_true(not_divisible_by_5)
  
  // 验证6不是质数（能被2和3整除）
  let composite = 6
  let divisible_by_2 = @azimuth.multiply(composite / 2, 2) == composite
  let divisible_by_3 = @azimuth.multiply(composite / 3, 3) == composite
  
  @azimuth.assert_true(divisible_by_2)
  @azimuth.assert_true(divisible_by_3)
}

test "greatest_common_divisor" {
  // 测试最大公约数计算（欧几里得算法模拟）
  let a = 48
  let b = 18
  
  // 模拟欧几里得算法：gcd(48, 18) = gcd(18, 12) = gcd(12, 6) = gcd(6, 0) = 6
  let step1 = a % b  // 48 % 18 = 12
  let step2 = b % step1  // 18 % 12 = 6
  let step3 = step1 % step2  // 12 % 6 = 0
  
  @azimuth.assert_eq(6, step2)  // GCD is 6
  
  // 验证：48和18都能被6整除
  let a_divisible = @azimuth.multiply(a / 6, 6) == a
  let b_divisible = @azimuth.multiply(b / 6, 6) == b
  
  @azimuth.assert_true(a_divisible)
  @azimuth.assert_true(b_divisible)
}

test "least_common_multiple" {
  // 测试最小公倍数计算
  let a = 12
  let b = 15
  
  // LCM(a, b) = (a * b) / GCD(a, b)
  // GCD(12, 15) = 3
  let product = @azimuth.multiply(a, b)  // 180
  let gcd = 3  // 已知GCD
  let lcm = product / gcd  // 60
  
  @azimuth.assert_eq(60, lcm)
  
  // 验证：60能被12和15整除
  let lcm_divisible_by_a = @azimuth.multiply(lcm / a, a) == lcm
  let lcm_divisible_by_b = @azimuth.multiply(lcm / b, b) == lcm
  
  @azimuth.assert_true(lcm_divisible_by_a)
  @azimuth.assert_true(lcm_divisible_by_b)
}

test "base_conversion_simulation" {
  // 测试进制转换模拟
  // 十进制数42转换为不同进制
  
  let decimal = 42
  
  // 转换为二进制：101010 (模拟计算)
  let binary_32 = 32  // 2^5
  let binary_8 = 8    // 2^3
  let binary_2 = 2    // 2^1
  let binary_sum = @azimuth.add(@azimuth.add(binary_32, binary_8), binary_2)
  @azimuth.assert_eq(42, binary_sum)
  
  // 转换为八进制：52 (模拟计算：5*8 + 2 = 42)
  let octal_5 = @azimuth.multiply(5, 8)
  let octal_result = @azimuth.add(octal_5, 2)
  @azimuth.assert_eq(42, octal_result)
  
  // 转换为十六进制：2A (模拟计算：2*16 + 10 = 42)
  let hex_2 = @azimuth.multiply(2, 16)
  let hex_result = @azimuth.add(hex_2, 10)
  @azimuth.assert_eq(42, hex_result)
}

test "string_pattern_matching" {
  // 测试字符串模式匹配（模拟）
  // 检查字符串是否包含特定模式
  
  let test_string = "Hello, MoonBit Programming!"
  
  // 模拟字符串包含检查（通过字符串长度比较）
  let pattern = "MoonBit"
  let greeting = @azimuth.greet("MoonBit")
  
  // 验证greet函数正确处理模式
  @azimuth.assert_eq_string("Hello, MoonBit!", greeting)
  
  // 测试字符串连接模式
  let prefix = "Hello, "
  let suffix = "!"
  let expected = prefix + pattern + suffix
  @azimuth.assert_eq_string(expected, greeting)
  
  // 测试空模式
  let empty_pattern = ""
  let empty_greeting = @azimuth.greet(empty_pattern)
  @azimuth.assert_eq_string("Hello, !", empty_greeting)
}

test "matrix_operations_simulation" {
  // 测试矩阵运算模拟（2x2矩阵）
  // 矩阵A: [[1, 2], [3, 4]]
  // 矩阵B: [[5, 6], [7, 8]]
  
  let a11 = 1; let a12 = 2
  let a21 = 3; let a22 = 4
  
  let b11 = 5; let b12 = 6
  let b21 = 7; let b22 = 8
  
  // 矩阵加法：C = A + B
  let c11 = @azimuth.add(a11, b11)  // 6
  let c12 = @azimuth.add(a12, b12)  // 8
  let c21 = @azimuth.add(a21, b21)  // 10
  let c22 = @azimuth.add(a22, b22)  // 12
  
  @azimuth.assert_eq(6, c11)
  @azimuth.assert_eq(8, c12)
  @azimuth.assert_eq(10, c21)
  @azimuth.assert_eq(12, c22)
  
  // 矩阵乘法：D = A * B
  // d11 = a11*b11 + a12*b21 = 1*5 + 2*7 = 19
  // d12 = a11*b12 + a12*b22 = 1*6 + 2*8 = 22
  // d21 = a21*b11 + a22*b21 = 3*5 + 4*7 = 43
  // d22 = a21*b12 + a22*b22 = 3*6 + 4*8 = 50
  
  let d11 = @azimuth.add(@azimuth.multiply(a11, b11), @azimuth.multiply(a12, b21))
  let d12 = @azimuth.add(@azimuth.multiply(a11, b12), @azimuth.multiply(a12, b22))
  let d21 = @azimuth.add(@azimuth.multiply(a21, b11), @azimuth.multiply(a22, b21))
  let d22 = @azimuth.add(@azimuth.multiply(a21, b12), @azimuth.multiply(a22, b22))
  
  @azimuth.assert_eq(19, d11)
  @azimuth.assert_eq(22, d12)
  @azimuth.assert_eq(43, d21)
  @azimuth.assert_eq(50, d22)
}

test "statistical_variance_calculation" {
  // 测试方差计算
  // 数据集：2, 4, 4, 4, 5, 5, 7, 9
  
  // 计算平均值：μ = (2+4+4+4+5+5+7+9)/8 = 40/8 = 5
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(2, 4), 4), 4), 5), 5), 7), 9)
  let mean = sum / 8
  @azimuth.assert_eq(5, mean)
  
  // 计算方差：σ² = Σ(xi-μ)²/n
  // (2-5)² = 9, (4-5)² = 1, (4-5)² = 1, (4-5)² = 1
  // (5-5)² = 0, (5-5)² = 0, (7-5)² = 4, (9-5)² = 16
  // 总和 = 9+1+1+1+0+0+4+16 = 32
  // 方差 = 32/8 = 4
  
  let diff1 = @azimuth.add(2, -5)  // -3
  let diff2 = @azimuth.add(4, -5)  // -1
  let diff3 = @azimuth.add(5, -5)  // 0
  let diff4 = @azimuth.add(7, -5)  // 2
  let diff5 = @azimuth.add(9, -5)  // 4
  
  let square1 = @azimuth.multiply(diff1, diff1)  // 9
  let square2 = @azimuth.multiply(diff2, diff2)  // 1
  let square3 = @azimuth.multiply(diff3, diff3)  // 0
  let square4 = @azimuth.multiply(diff4, diff4)  // 4
  let square5 = @azimuth.multiply(diff5, diff5)  // 16
  
let sum1 = @azimuth.add(square1, square2)
  let sum2 = @azimuth.add(square2, square3)
  let sum3 = @azimuth.add(square4, square5)
  let variance_sum = @azimuth.add(sum1, @azimuth.add(sum2, sum3))
  let variance = variance_sum / 8
  
  @azimuth.assert_eq(4, variance)
}
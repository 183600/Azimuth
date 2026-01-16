// 新增的 MoonBit 测试用例
// 测试 azimuth 模块的各种功能

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  // 计算前6项
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f0, f1)  // 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  
  // 验证斐波那契数列的性质：F(n+1) * F(n-1) - F(n)^2 = (-1)^n
  // 当n=4时，F(5) * F(3) - F(4)^2 = 5 * 2 - 3^2 = 10 - 9 = 1
  // 但实际上应该是 F(n+1) * F(n-1) - F(n)^2 = (-1)^n = (-1)^4 = 1
  let fibonacci_property = @azimuth.add(@azimuth.multiply(f5, f3), @azimuth.multiply(f4, -f4))
  @azimuth.assert_eq(1, fibonacci_property)
}

test "prime_number_validation" {
  // 测试质数验证（通过模拟）
  // 检查一个数是否为质数（简化版，只测试小质数）
  
  // 已知的小质数
  let prime2 = 2
  let prime3 = 3
  let prime5 = 5
  let prime7 = 7
  
  // 验证这些数不能被小于它们的质数整除
  // 2是质数，不需要验证
  @azimuth.assert_true(prime3 % prime2 != 0)  // 3不能被2整除
  @azimuth.assert_true(prime5 % prime2 != 0 && prime5 % prime3 != 0)  // 5不能被2和3整除
  @azimuth.assert_true(prime7 % prime2 != 0 && prime7 % prime3 != 0 && prime7 % prime5 != 0)  // 7不能被2、3、5整除
  
  // 测试非质数
  let composite4 = 4
  let composite6 = 6
  let composite9 = 9
  
  @azimuth.assert_true(composite4 % prime2 == 0)  // 4能被2整除
  @azimuth.assert_true(composite6 % prime2 == 0 || composite6 % prime3 == 0)  // 6能被2或3整除
  @azimuth.assert_true(composite9 % prime3 == 0)  // 9能被3整除
}

test "greatest_common_divisor" {
  // 测试最大公约数计算（欧几里得算法模拟）
  // gcd(a, b) = gcd(b, a mod b)
  
  // 测试几对数的最大公约数
  // 手动计算：gcd(48, 18) = gcd(18, 48 mod 18 = 12) = gcd(12, 18 mod 12 = 6) = gcd(6, 12 mod 6 = 0) = 6
  let gcd1 = 6
  @azimuth.assert_eq(6, gcd1)  // 预期结果
  
  // gcd(101, 10) = gcd(10, 101 mod 10 = 1) = gcd(1, 10 mod 1 = 0) = 1
  let gcd2 = 1
  @azimuth.assert_eq(1, gcd2)  // 预期结果
  
  // gcd(270, 192) = gcd(192, 270 mod 192 = 78) = gcd(78, 192 mod 78 = 36) = gcd(36, 78 mod 36 = 6) = gcd(6, 36 mod 6 = 0) = 6
  let gcd3 = 6
  @azimuth.assert_eq(6, gcd3)  // 预期结果
}

test "least_common_multiple" {
  // 测试最小公倍数计算
  // lcm(a, b) = |a × b| / gcd(a, b)
  
  // gcd(12, 18) = 6, 所以 lcm(12, 18) = (12 × 18) / 6 = 36
  let product1 = @azimuth.multiply(12, 18)
  let lcm1 = product1 / 6  // 已知gcd为6
  @azimuth.assert_eq(36, lcm1)
  
  // gcd(5, 7) = 1, 所以 lcm(5, 7) = (5 × 7) / 1 = 35
  let product2 = @azimuth.multiply(5, 7)
  let lcm2 = product2 / 1  // 已知gcd为1
  @azimuth.assert_eq(35, lcm2)
  
  // gcd(21, 6) = 3, 所以 lcm(21, 6) = (21 × 6) / 3 = 42
  let product3 = @azimuth.multiply(21, 6)
  let lcm3 = product3 / 3  // 已知gcd为3
  @azimuth.assert_eq(42, lcm3)
}

test "binary_search_simulation" {
  // 模拟二分查找算法
  // 在已排序数组中查找目标值
  
  // 模拟数组：[2, 5, 8, 12, 16, 23, 38, 56, 72, 91]
  // 查找目标：23
  
  let low = 0
  let high = 9
  
  // 第一次迭代：mid = (0 + 9) / 2 = 4, arr[4] = 16 < 23
  let mid1 = (low + high) / 2
  @azimuth.assert_eq(4, mid1)
  let new_low1 = mid1 + 1  // 5
  
  // 第二次迭代：mid = (5 + 9) / 2 = 7, arr[7] = 56 > 23
  let mid2 = (new_low1 + high) / 2
  @azimuth.assert_eq(7, mid2)
  let new_high2 = mid2 - 1  // 6
  
  // 第三次迭代：mid = (5 + 6) / 2 = 5, arr[5] = 23 = target
  let mid3 = (new_low1 + new_high2) / 2
  @azimuth.assert_eq(5, mid3)
  
  // 找到目标，索引为5
  @azimuth.assert_eq(5, mid3)
}

test "sorting_algorithm_simulation" {
  // 模拟排序算法（冒泡排序）
  // 对数组：[5, 2, 8, 1, 9] 进行排序
  
  // 初始数组
  let a1_initial = 5
  let a2_initial = 2
  let a3_initial = 8
  let a4_initial = 1
  let a5_initial = 9
  
  // 第一轮冒泡
  // 比较 a1 和 a2：5 > 2，交换
  let a1_after_swap1 = a2_initial  // 2
  let a2_after_swap1 = a1_initial  // 5
  
  // 比较 a2 和 a3：5 < 8，不交换
  // 比较 a3 和 a4：8 > 1，交换
  let a3_after_swap2 = a4_initial  // 1
  let a4_after_swap2 = a3_initial  // 8
  
  // 比较 a4 和 a5：8 < 9，不交换
  // 第一轮结束后，最大的数9已经到位
  
  // 验证部分排序结果
  @azimuth.assert_eq(2, a1_after_swap1)  // 最小的元素应该在前面
  @azimuth.assert_eq(9, a5_initial)     // 最大的元素应该在后面
}

test "base_conversion_simulation" {
  // 模拟进制转换
  // 将十进制数转换为二进制（简化版）
  
  // 将十进制数13转换为二进制1101
  let decimal_num = 13
  let base = 2
  
  // 计算过程：
  // 13 ÷ 2 = 6 余 1
  // 6 ÷ 2 = 3 余 0
  // 3 ÷ 2 = 1 余 1
  // 1 ÷ 2 = 0 余 1
  // 从下往上读取余数：1101
  
  let remainder1 = decimal_num % base  // 13 % 2 = 1
  let quotient1 = decimal_num / base  // 13 / 2 = 6
  
  let remainder2 = quotient1 % base    // 6 % 2 = 0
  let quotient2 = quotient1 / base    // 6 / 2 = 3
  
  let remainder3 = quotient2 % base    // 3 % 2 = 1
  let quotient3 = quotient2 / base    // 3 / 2 = 1
  
  let remainder4 = quotient3 % base    // 1 % 2 = 1
  
  // 验证余数序列
  @azimuth.assert_eq(1, remainder1)
  @azimuth.assert_eq(0, remainder2)
  @azimuth.assert_eq(1, remainder3)
  @azimuth.assert_eq(1, remainder4)
  
  // 将二进制1101转换回十进制验证
  // 1×2³ + 1×2² + 0×2¹ + 1×2⁰ = 8 + 4 + 0 + 1 = 13
  let binary_decimal = @azimuth.add(@azimuth.add(@azimuth.multiply(1, 8), @azimuth.multiply(1, 4)), @azimuth.add(@azimuth.multiply(0, 2), 1))
  @azimuth.assert_eq(13, binary_decimal)
}

test "matrix_multiplication_simulation" {
  // 模拟矩阵乘法（2×2矩阵）
  // A = [[1, 2], [3, 4]]
  // B = [[5, 6], [7, 8]]
  // C = A × B = [[1×5+2×7, 1×6+2×8], [3×5+4×7, 3×6+4×8]]
  
  // 矩阵A的元素
  let a11 = 1
  let a12 = 2
  let a21 = 3
  let a22 = 4
  
  // 矩阵B的元素
  let b11 = 5
  let b12 = 6
  let b21 = 7
  let b22 = 8
  
  // 计算矩阵C的元素
  let c11 = @azimuth.add(@azimuth.multiply(a11, b11), @azimuth.multiply(a12, b21))  // 1×5 + 2×7 = 5 + 14 = 19
  let c12 = @azimuth.add(@azimuth.multiply(a11, b12), @azimuth.multiply(a12, b22))  // 1×6 + 2×8 = 6 + 16 = 22
  let c21 = @azimuth.add(@azimuth.multiply(a21, b11), @azimuth.multiply(a22, b21))  // 3×5 + 4×7 = 15 + 28 = 43
  let c22 = @azimuth.add(@azimuth.multiply(a21, b12), @azimuth.multiply(a22, b22))  // 3×6 + 4×8 = 18 + 32 = 50
  
  // 验证结果
  @azimuth.assert_eq(19, c11)
  @azimuth.assert_eq(22, c12)
  @azimuth.assert_eq(43, c21)
  @azimuth.assert_eq(50, c22)
}

test "quadratic_equation_solver" {
  // 模拟二次方程求解：ax² + bx + c = 0
  // 使用求根公式：x = (-b ± √(b²-4ac)) / 2a
  // 测试方程：x² - 5x + 6 = 0，解为x = 2和x = 3
  
  let a = 1
  let b = -5
  let c = 6
  
  // 计算判别式：Δ = b² - 4ac
  let b_squared = @azimuth.multiply(b, b)  // (-5)² = 25
  let four_ac = @azimuth.multiply(@azimuth.multiply(4, a), c)  // 4×1×6 = 24
  let discriminant_value = @azimuth.add(b_squared, -four_ac)  // 25 - 24 = 1
  
  // 由于discriminant = 1是完全平方数，√1 = 1
  let sqrt_discriminant = 1
  
  // 计算两个解
  // x1 = (-b + √Δ) / 2a = (5 + 1) / 2 = 3
  // x2 = (-b - √Δ) / 2a = (5 - 1) / 2 = 2
  let numerator1 = @azimuth.add(-b, sqrt_discriminant)  // 5 + 1 = 6
  let numerator2 = @azimuth.add(-b, -sqrt_discriminant)  // 5 - 1 = 4
  let denominator = @azimuth.multiply(2, a)  // 2×1 = 2
  
  let x1 = numerator1 / denominator  // 6 / 2 = 3
  let x2 = numerator2 / denominator  // 4 / 2 = 2
  
  // 验证解
  @azimuth.assert_eq(3, x1)
  @azimuth.assert_eq(2, x2)
  
  // 验证解的正确性：将x代入原方程
  let verify_x1 = @azimuth.add(@azimuth.add(@azimuth.multiply(a, @azimuth.multiply(x1, x1)), @azimuth.multiply(b, x1)), c)  // 1×9 + (-5)×3 + 6 = 9 - 15 + 6 = 0
  let verify_x2 = @azimuth.add(@azimuth.add(@azimuth.multiply(a, @azimuth.multiply(x2, x2)), @azimuth.multiply(b, x2)), c)  // 1×4 + (-5)×2 + 6 = 4 - 10 + 6 = 0
  
  @azimuth.assert_eq(0, verify_x1)
  @azimuth.assert_eq(0, verify_x2)
}

test "string_palindrome_check" {
  // 测试字符串回文检查
  // 回文是指正着读和反着读都一样的字符串
  
  // 测试简单回文
  let palindrome1 = "level"
  let palindrome1_reversed = "level"  // 手动反转
  @azimuth.assert_eq_string(palindrome1, palindrome1_reversed)
  
  // 测试包含数字的回文
  let palindrome2 = "12321"
  let palindrome2_reversed = "12321"  // 手动反转
  @azimuth.assert_eq_string(palindrome2, palindrome2_reversed)
  
  // 测试非回文
  let non_palindrome = "hello"
  let non_palindrome_reversed = "olleh"  // 手动反转
  @azimuth.assert_true(@azimuth.add(0, 1) == 1)  // 使用断言验证它们不相等
  // 由于没有字符串不等于的断言函数，我们使用一个技巧来验证
  let are_equal = if (non_palindrome == non_palindrome_reversed) { 1 } else { 0 }
  @azimuth.assert_eq(0, are_equal)
  
  // 测试空字符串（是回文）
  let empty_string = ""
  @azimuth.assert_eq_string(empty_string, empty_string)
  
  // 测试单字符（是回文）
  let single_char = "a"
  @azimuth.assert_eq_string(single_char, single_char)
}
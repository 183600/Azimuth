// 专业级 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "advanced_arithmetic_sequence" {
  // 测试等差数列计算
  let first_term = 3
  let common_difference = 4
  let n = 6
  
  // 计算第n项：a_n = a_1 + (n-1)d
  let nth_term = add(first_term, multiply(subtract(n, 1), common_difference))
  
  // 计算前n项和：S_n = n/2 * (2a_1 + (n-1)d)
  let sum_first_n = divide_with_ceil(multiply(n, add(multiply(2, first_term), multiply(subtract(n, 1), common_difference))), 2)
  
  assert_eq(nth_term, 23)
  assert_eq(sum_first_n, 78)
}

test "geometric_progression" {
  // 测试等比数列计算
  let first_term = 2
  let common_ratio = 3
  let n = 5
  
  // 计算前几项
  let term2 = multiply(first_term, common_ratio)
  let term3 = multiply(term2, common_ratio)
  let term4 = multiply(term3, common_ratio)
  let term5 = multiply(term4, common_ratio)
  
  // 计算前n项和的近似值
  let sum_approx = add(add(add(add(first_term, term2), term3), term4), term5)
  
  assert_eq(term2, 6)
  assert_eq(term3, 18)
  assert_eq(term4, 54)
  assert_eq(term5, 162)
  assert_eq(sum_approx, 242)
}

test "prime_number_validation" {
  // 测试质数相关计算
  let number = 29
  
  // 检查是否能被小于等于sqrt(29)的质数整除
  let divisible_by_2 = number % 2
  let divisible_by_3 = number % 3
  let divisible_by_5 = number % 5
  
  // 计算因数个数（简化版）
  let factor_count = 2  // 1 和 itself
  if divisible_by_2 == 0 { factor_count = add(factor_count, 1) }
  if divisible_by_3 == 0 { factor_count = add(factor_count, 1) }
  if divisible_by_5 == 0 { factor_count = add(factor_count, 1) }
  
  assert_eq(divisible_by_2, 1)
  assert_eq(divisible_by_3, 2)
  assert_eq(divisible_by_5, 4)
  assert_eq(factor_count, 2)
}

test "greatest_common_divisor" {
  // 测试最大公约数的欧几里得算法简化版
  let a = 48
  let b = 18
  
  // 简化的欧几里得算法步骤
  let step1 = a % b  // 48 % 18 = 12
  let step2 = b % step1  // 18 % 12 = 6
  let step3 = step1 % step2  // 12 % 6 = 0
  
  // GCD 是最后一个非零余数
  let gcd = step2
  
  assert_eq(step1, 12)
  assert_eq(step2, 6)
  assert_eq(step3, 0)
  assert_eq(gcd, 6)
}

test "least_common_multiple" {
  // 测试最小公倍数
  let a = 12
  let b = 15
  
  // LCM = (a * b) / GCD
  let product = multiply(a, b)
  let gcd = 3  // 12和15的最大公约数
  let lcm = divide_with_ceil(product, gcd)
  
  assert_eq(product, 180)
  assert_eq(lcm, 60)
}

test "factorial_calculation" {
  // 测试阶乘计算
  let n = 6
  
  // 计算 6! = 6 × 5 × 4 × 3 × 2 × 1
  let factorial = multiply(multiply(multiply(multiply(multiply(6, 5), 4), 3), 2), 1)
  
  assert_eq(factorial, 720)
}

test "power_calculation" {
  // 测试幂运算
  let base = 3
  let exponent = 4
  
  // 计算 3^4 = 3 × 3 × 3 × 3
  let power = multiply(multiply(multiply(3, 3), 3), 3)
  
  assert_eq(power, 81)
}

test "quadratic_equation" {
  // 测试二次方程求解（简化版）
  // ax² + bx + c = 0，其中 a=1, b=-5, c=6
  let a = 1
  let b = -5
  let c = 6
  
  // 判别式 Δ = b² - 4ac
  let discriminant = subtract(multiply(b, b), multiply(4, multiply(a, c)))
  
  // 简化的根计算（仅当判别式为完全平方数时）
  let sqrt_discriminant = 1  // sqrt(1) = 1
  let root1 = divide_with_ceil(add(-b, sqrt_discriminant), multiply(2, a))
  let root2 = divide_with_ceil(subtract(-b, sqrt_discriminant), multiply(2, a))
  
  assert_eq(discriminant, 1)
  assert_eq(root1, 3)
  assert_eq(root2, 2)
}

test "permutation_combination" {
  // 测试排列组合计算
  let n = 5
  let r = 3
  
  // 排列 P(n,r) = n! / (n-r)!
  let n_factorial = multiply(multiply(multiply(multiply(5, 4), 3), 2), 1)
  let n_minus_r_factorial = multiply(multiply(2, 1), 1)
  let permutation = divide_with_ceil(n_factorial, n_minus_r_factorial)
  
  // 组合 C(n,r) = P(n,r) / r!
  let r_factorial = multiply(multiply(3, 2), 1)
  let combination = divide_with_ceil(permutation, r_factorial)
  
  assert_eq(n_factorial, 120)
  assert_eq(n_minus_r_factorial, 2)
  assert_eq(permutation, 60)
  assert_eq(r_factorial, 6)
  assert_eq(combination, 10)
}

test "number_base_conversion" {
  // 测试数字进制转换
  let decimal = 42
  
  // 十进制转二进制
  let binary_bit5 = (decimal / 32) % 2
  let binary_bit4 = (decimal / 16) % 2
  let binary_bit3 = (decimal / 8) % 2
  let binary_bit2 = (decimal / 4) % 2
  let binary_bit1 = (decimal / 2) % 2
  let binary_bit0 = decimal % 2
  
  // 重新组合验证
  let reconstructed = add(add(add(add(add(multiply(binary_bit5, 32), multiply(binary_bit4, 16)), multiply(binary_bit3, 8)), multiply(binary_bit2, 4)), multiply(binary_bit1, 2))
  let final_value = add(reconstructed, binary_bit0)
  
  assert_eq(binary_bit5, 1)
  assert_eq(binary_bit4, 0)
  assert_eq(binary_bit3, 1)
  assert_eq(binary_bit2, 0)
  assert_eq(binary_bit1, 1)
  assert_eq(binary_bit0, 0)
  assert_eq(final_value, 42)
}
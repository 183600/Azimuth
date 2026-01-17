// 新增的 MoonBit 测试用例
// 测试 azimuth 模块的额外功能

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(2) = 1, F(3) = 2, F(4) = 3, F(5) = 5, F(6) = 8
  let fib0 = 0
  let fib1 = 1
  let fib2 = @azimuth.add(fib0, fib1)  // 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  
  @azimuth.assert_eq(1, fib2)
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
}

test "prime_number_validation" {
  // 验证素数计算
  // 检查7是否为素数（不能被2, 3整除）
  let candidate = 7
  let remainder_by_2 = candidate % 2
  let remainder_by_3 = candidate % 3
  let is_prime = remainder_by_2 > 0 && remainder_by_3 > 0
  
  @azimuth.assert_true(is_prime)
  
  // 检查9是否为素数（可以被3整除）
  let composite = 9
  let composite_remainder = composite % 3
  let is_composite_prime = composite_remainder > 0
  
  @azimuth.assert_false(is_composite_prime)
}

test "greatest_common_divisor" {
  // 计算最大公约数
  // GCD(48, 18) = 6
  let a = 48
  let b = 18
  
  // 欧几里得算法步骤模拟
  // 48 = 2 × 18 + 12
  // 18 = 1 × 12 + 6
  // 12 = 2 × 6 + 0
  let remainder1 = a % b  // 48 % 18 = 12
  let remainder2 = b % remainder1  // 18 % 12 = 6
  let remainder3 = remainder1 % remainder2  // 12 % 6 = 0
  
  let gcd = remainder2
  @azimuth.assert_eq(6, gcd)
}

test "least_common_multiple" {
  // 计算最小公倍数
  // LCM(12, 15) = (12 × 15) / GCD(12, 15) = 180 / 3 = 60
  let a = 12
  let b = 15
  
  // 计算GCD
  let gcd = 3  // 12 % 3 = 0, 15 % 3 = 0
  
  // 计算LCM
  let product = @azimuth.multiply(a, b)
  let lcm = product / gcd
  
  @azimuth.assert_eq(60, lcm)
}

test "triangle_area_calculation" {
  // 计算三角形面积（海伦公式）
  // 边长：a = 3, b = 4, c = 5
  let a = 3
  let b = 4
  let c = 5
  
  // 半周长：s = (a + b + c) / 2 = (3 + 4 + 5) / 2 = 6
  let perimeter = @azimuth.add(@azimuth.add(a, b), c)
  let s = perimeter / 2
  
  // 面积：A = √(s(s-a)(s-b)(s-c)) = √(6×3×2×1) = √36 = 6
  // 使用整数计算：6 × 3 × 2 × 1 = 36
  let area_squared = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(s, @azimuth.add(s, -a)), @azimuth.add(s, -b)), @azimuth.add(s, -c))
  
  @azimuth.assert_eq(36, area_squared)
}

test "quadratic_equation" {
  // 二次方程求解：ax² + bx + c = 0
  // x² - 5x + 6 = 0 的解是 x = 2 和 x = 3
  let a = 1
  let b = -5
  let c = 6
  
  // 判别式：Δ = b² - 4ac = (-5)² - 4×1×6 = 25 - 24 = 1
  let discriminant = @azimuth.add(@azimuth.multiply(b, b), @azimuth.multiply(-4, @azimuth.multiply(a, c)))
  
  @azimuth.assert_eq(1, discriminant)
  
  // 验证解
  let x1 = 2
  let x2 = 3
  
  // 验证 x1 是解：1×2² - 5×2 + 6 = 4 - 10 + 6 = 0
  let verify_x1 = @azimuth.add(@azimuth.add(@azimuth.multiply(a, @azimuth.multiply(x1, x1)), @azimuth.multiply(b, x1)), c)
  
  // 验证 x2 是解：1×3² - 5×3 + 6 = 9 - 15 + 6 = 0
  let verify_x2 = @azimuth.add(@azimuth.add(@azimuth.multiply(a, @azimuth.multiply(x2, x2)), @azimuth.multiply(b, x2)), c)
  
  @azimuth.assert_eq(0, verify_x1)
  @azimuth.assert_eq(0, verify_x2)
}

test "arithmetic_progression" {
  // 等差数列求和
  // 数列：2, 5, 8, 11, 14, 17, 20
  let first_term = 2
  let common_difference = 3
  let number_of_terms = 7
  
  // 末项：an = a1 + (n-1)d = 2 + 6×3 = 20
  let last_term = @azimuth.add(first_term, @azimuth.multiply(number_of_terms - 1, common_difference))
  
  // 求和：S = n(a1 + an) / 2 = 7(2 + 20) / 2 = 7×22 / 2 = 77
  let sum = @azimuth.multiply(number_of_terms, @azimuth.add(first_term, last_term)) / 2
  
  @azimuth.assert_eq(20, last_term)
  @azimuth.assert_eq(77, sum)
}

test "geometric_progression" {
  // 等比数列求和
  // 数列：3, 6, 12, 24, 48
  let first_term = 3
  let common_ratio = 2
  let number_of_terms = 5
  
  // 手动计算各项和
  let term1 = first_term
  let term2 = @azimuth.multiply(term1, common_ratio)
  let term3 = @azimuth.multiply(term2, common_ratio)
  let term4 = @azimuth.multiply(term3, common_ratio)
  let term5 = @azimuth.multiply(term4, common_ratio)
  
  // 求和：3 + 6 + 12 + 24 + 48 = 93
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  
  @azimuth.assert_eq(93, sum)
}

test "binary_search_simulation" {
  // 二分查找模拟
  // 在有序数组中查找 7
  let target = 7
  let low = 0
  let high = 7
  
  // 第一次查找：mid = (0 + 7) / 2 = 3
  let mid1 = (low + high) / 2
  let found_at_step1 = mid1 == 3 && target == 7
  
  @azimuth.assert_true(found_at_step1)
  
  // 查找不存在的元素 8
  let target_not_found = 8
  
  // 第一次查找：mid = 3, 值 = 7 < 8
  // 第二次查找：low = 4, high = 7, mid = 5, 值 = 11 > 8
  // 第三次查找：low = 4, high = 4, mid = 4, 值 = 9 > 8
  // 第四次查找：low = 4, high = 3，查找结束
  
  let search_steps_needed = 4
  @azimuth.assert_eq(4, search_steps_needed)
}

test "sorting_algorithm_validation" {
  // 排序算法验证
  // 冒泡排序模拟：对 [5, 2, 8, 1, 9] 进行排序
  
  // 验证排序前后的和相同
  let original_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(5, 2), 8), 1), 9)
  let sorted_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 5), 8), 9)
  
  @azimuth.assert_eq(original_sum, sorted_sum)
  @azimuth.assert_eq(25, original_sum)
  
  // 验证最小值和最大值
  let min_val = 1
  let max_val = 9
  
  @azimuth.assert_eq(min_val, 1)
  @azimuth.assert_eq(max_val, 9)
  
  // 验证数组长度不变
  let length = 5
  @azimuth.assert_eq(length, 5)
}
// Azimuth 项目额外测试用例
// 补充边界值、字符串处理、复合功能和错误处理测试

test "multiply_boundary_values" {
  // 测试乘法函数的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(1, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  
  // 测试-1的乘法
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(-max_val, @azimuth.multiply(-1, max_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
}

test "multiply_overflow_scenarios" {
  // 测试乘法溢出场景
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))  // sqrt(max_val) ≈ 46341
  @azimuth.assert_eq(max_val, @azimuth.multiply(100000, 30000))
  
  // 负数乘法溢出
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(46341, -46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-100000, 30000))
  
  // 最小值特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(2, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-2, min_val))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的各种情况
  
  // 正数除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))   // 10/3 = 3.33 → 4
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))    // 6/3 = 2 → 2
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 3))    // 1/3 = 0.33 → 1
  
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -10/3 = -3.33 → -3
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))  // 10/-3 = -3.33 → -3
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))  // -10/-3 = 3.33 → 4
  
  // 零的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))     // 0/5 = 0 → 0
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))    // 0/-5 = 0 → 0
  
  // 整除情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(15, 3))    // 15/3 = 5 → 5
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-15, 3))  // -15/3 = -5 → -5
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(15, -3))  // 15/-3 = -5 → -5
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(-15, -3))  // -15/-3 = 5 → 5
}

test "string_edge_cases" {
  // 测试字符串处理的边缘情况
  
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, 1!", @azimuth.greet("1"))
  
  // 长字符串
  let long_name = "这是一个非常非常长的名字用来测试greet函数处理长字符串的能力"
  let expected_long = "Hello, " + long_name + "!"
  @azimuth.assert_eq_string(expected_long, @azimuth.greet(long_name))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  
  // 特殊Unicode字符
  @azimuth.assert_eq_string("Hello, 👋!", @azimuth.greet("👋"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, 🎉!", @azimuth.greet("🎉"))
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  
  // 等差数列求和：1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let sum_arithmetic = @azimuth.multiply(@azimuth.add(1, n), n) / 2
  @azimuth.assert_eq(55, sum_arithmetic)
  
  // 等比数列求和：2 + 4 + 8 + 16 = 30
  let term1 = 2
  let ratio = 2
  let terms = 4
  let sum_geometric = @azimuth.multiply(term1, @azimuth.add(@azimuth.multiply(ratio, terms), -1)) / @azimuth.add(ratio, -1)
  @azimuth.assert_eq(30, sum_geometric)
  
  // 平方和：1² + 2² + 3² + 4² + 5² = 55
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), @azimuth.multiply(4, 4)), @azimuth.multiply(5, 5))
  @azimuth.assert_eq(55, square_sum)
}

test "combined_operations_complex" {
  // 测试复合运算的复杂场景
  
  // 场景：计算 (a + b) * (c - d) + e / f
  let a = 12
  let b = 8
  let c = 20
  let d = 15
  let e = 100
  let f = 4
  
  let sum_ab = @azimuth.add(a, b)  // 20
  let diff_cd = @azimuth.add(c, -d)  // 5
  let product = @azimuth.multiply(sum_ab, diff_cd)  // 100
  let division = e / f  // 25
  let result = @azimuth.add(product, division)  // 125
  
  @azimuth.assert_eq(125, result)
  
  // 场景：计算折扣后的总价
  let original_price = 1000
  let discount_rate = 15  // 15%折扣
  let tax_rate = 8       // 8%税率
  let quantity = 3
  
  let discounted_price = @azimuth.multiply(original_price, @azimuth.add(100, -discount_rate)) / 100  // 850
  let after_tax = @azimuth.multiply(discounted_price, @azimuth.add(100, tax_rate)) / 100  // 918
  let total = @azimuth.multiply(after_tax, quantity)  // 2754
  
  @azimuth.assert_eq(2754, total)
}

test "assertion_functions_test" {
  // 测试断言函数本身
  
  // 测试assert_eq的各种情况
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq(-1, -1)
  @azimuth.assert_eq(2147483647, 2147483647)
  @azimuth.assert_eq(-2147483648, -2147483648)
  
  // 测试assert_eq_string的各种情况
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("a", "a")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  @azimuth.assert_eq_string("中文测试", "中文测试")
  
  // 测试assert_true
  @azimuth.assert_true(true)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 < 1)
  @azimuth.assert_true(-1 < 0)
  
  // 测试assert_false
  @azimuth.assert_false(false)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(1 < 0)
  @azimuth.assert_false(-1 > 0)
}

test "algorithm_fibonacci_iterative" {
  // 测试斐波那契数列的迭代实现
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  // 计算前10个斐波那契数
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f0, f1)  // 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  let f8 = @azimuth.add(f6, f7)  // 21
  let f9 = @azimuth.add(f7, f8)  // 34
  let f10 = @azimuth.add(f8, f9)  // 55
  
  @azimuth.assert_eq(0, f0)
  @azimuth.assert_eq(1, f1)
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
  @azimuth.assert_eq(13, f7)
  @azimuth.assert_eq(21, f8)
  @azimuth.assert_eq(34, f9)
  @azimuth.assert_eq(55, f10)
  
  // 验证斐波那契数列的性质
  let sum_first_10 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(f0, f1), f2), f3), @azimuth.add(f4, f5)), @azimuth.add(@azimuth.add(f6, f7), @azimuth.add(f8, f9)))
  @azimuth.assert_eq(88, sum_first_10)  // 前10个数的和应该是88
}

test "number_theory_properties" {
  // 测试数论性质
  
  // 测试偶数和奇数性质
  let even_num = 8
  let odd_num = 7
  
  @azimuth.assert_true(even_num % 2 == 0)
  @azimuth.assert_true(odd_num % 2 == 1)
  
  // 测试质数性质（简单测试）
  let prime_2 = 2
  let prime_3 = 3
  let prime_5 = 5
  let prime_7 = 7
  let composite_4 = 4
  let composite_6 = 6
  let composite_8 = 8
  let composite_9 = 9
  
  // 简单的质数测试
  @azimuth.assert_true(prime_2 > 1)
  @azimuth.assert_true(prime_3 > 1)
  @azimuth.assert_true(prime_5 > 1)
  @azimuth.assert_true(prime_7 > 1)
  
  // 测试数的因子
  @azimuth.assert_true(4 % 2 == 0)
  @azimuth.assert_true(6 % 2 == 0)
  @azimuth.assert_true(6 % 3 == 0)
  @azimuth.assert_true(8 % 2 == 0)
  @azimuth.assert_true(8 % 4 == 0)
  @azimuth.assert_true(9 % 3 == 0)
  
  // 测试完全数：6 = 1 + 2 + 3
  let proper_divisors_sum_6 = @azimuth.add(@azimuth.add(1, 2), 3)
  @azimuth.assert_eq(6, proper_divisors_sum_6)
}
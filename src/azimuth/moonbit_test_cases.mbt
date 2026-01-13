// 新增的 MoonBit 测试用例
// 这些测试用例提供了对 azimuth 库的额外验证，覆盖了一些新的场景

// 测试阶乘计算（使用乘法实现）
test "factorial_calculation" {
  // 计算 5! = 5 * 4 * 3 * 2 * 1 = 120
  let fact1 = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, fact1)
  
  // 计算 4! = 4 * 3 * 2 * 1 = 24
  let fact2 = @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1)))
  @azimuth.assert_eq(24, fact2)
  
  // 计算 3! = 3 * 2 * 1 = 6
  let fact3 = @azimuth.multiply(3, @azimuth.multiply(2, 1))
  @azimuth.assert_eq(6, fact3)
}

// 测试斐波那契数列（使用加法实现）
test "fibonacci_sequence" {
  // 斐波那契数列：F(0)=0, F(1)=1, F(n)=F(n-1)+F(n-2)
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f1, f0)  // 1
  let f3 = @azimuth.add(f2, f1)  // 2
  let f4 = @azimuth.add(f3, f2)  // 3
  let f5 = @azimuth.add(f4, f3)  // 5
  let f6 = @azimuth.add(f5, f4)  // 8
  let f7 = @azimuth.add(f6, f5)  // 13
  let f8 = @azimuth.add(f7, f6)  // 21
  let f9 = @azimuth.add(f8, f7)  // 34
  let f10 = @azimuth.add(f9, f8) // 55
  
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
}

// 测试平方和立方计算
test "power_calculations" {
  // 平方计算：n^2 = n * n
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(4, @azimuth.multiply(2, 2))
  @azimuth.assert_eq(9, @azimuth.multiply(3, 3))
  @azimuth.assert_eq(16, @azimuth.multiply(4, 4))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(10000, @azimuth.multiply(100, 100))
  
  // 立方计算：n^3 = n * n * n
  @azimuth.assert_eq(0, @azimuth.multiply(@azimuth.multiply(0, 0), 0))
  @azimuth.assert_eq(1, @azimuth.multiply(@azimuth.multiply(1, 1), 1))
  @azimuth.assert_eq(8, @azimuth.multiply(@azimuth.multiply(2, 2), 2))
  @azimuth.assert_eq(27, @azimuth.multiply(@azimuth.multiply(3, 3), 3))
  @azimuth.assert_eq(64, @azimuth.multiply(@azimuth.multiply(4, 4), 4))
  @azimuth.assert_eq(125, @azimuth.multiply(@azimuth.multiply(5, 5), 5))
}

// 测试平均数计算
test "average_calculation" {
  // 计算两个数的平均数：(a + b) / 2
  let avg1 = @azimuth.add(10, 20) / 2
  @azimuth.assert_eq(15, avg1)
  
  let avg2 = @azimuth.add(5, 9) / 2
  @azimuth.assert_eq(7, avg2)  // 整数除法，向下取整
  
  let avg3 = @azimuth.add(-5, 5) / 2
  @azimuth.assert_eq(0, avg3)
  
  // 计算三个数的平均数：(a + b + c) / 3
  let avg4 = @azimuth.add(@azimuth.add(10, 20), 30) / 3
  @azimuth.assert_eq(20, avg4)
  
  let avg5 = @azimuth.add(@azimuth.add(1, 2), 3) / 3
  @azimuth.assert_eq(2, avg5)
}

// 测试字符串长度和复杂度
test "string_complexity_tests" {
  // 测试单字符字符串
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, 1!", @azimuth.greet("1"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet("!"))
  
  // 测试包含各种特殊字符的字符串
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, \n!", @azimuth.greet("\n"))
  @azimuth.assert_eq_string("Hello, \t!", @azimuth.greet("\t"))
  @azimuth.assert_eq_string("Hello, \\!", @azimuth.greet("\\"))
  
  // 测试包含数字和字母混合的字符串
  @azimuth.assert_eq_string("Hello, abc123!", @azimuth.greet("abc123"))
  @azimuth.assert_eq_string("Hello, 123abc!", @azimuth.greet("123abc"))
  @azimuth.assert_eq_string("Hello, a1b2c3!", @azimuth.greet("a1b2c3"))
}

// 测试递归计算场景（模拟递归）
test "recursive_calculation_simulation" {
  // 模拟递归计算：sum(n) = n + sum(n-1), sum(0) = 0
  // 计算 sum(5) = 5 + 4 + 3 + 2 + 1 + 0 = 15
  let sum5 = @azimuth.add(5, @azimuth.add(4, @azimuth.add(3, @azimuth.add(2, @azimuth.add(1, 0)))))
  @azimuth.assert_eq(15, sum5)
  
  // 计算 sum(4) = 4 + 3 + 2 + 1 + 0 = 10
  let sum4 = @azimuth.add(4, @azimuth.add(3, @azimuth.add(2, @azimuth.add(1, 0))))
  @azimuth.assert_eq(10, sum4)
  
  // 模拟递归计算：pow(n, m) = n * pow(n, m-1), pow(n, 0) = 1
  // 计算 pow(2, 4) = 2 * 2 * 2 * 2 * 1 = 16
  let pow2_4 = @azimuth.multiply(2, @azimuth.multiply(2, @azimuth.multiply(2, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(16, pow2_4)
  
  // 计算 pow(3, 3) = 3 * 3 * 3 * 1 = 27
  let pow3_3 = @azimuth.multiply(3, @azimuth.multiply(3, @azimuth.multiply(3, 1)))
  @azimuth.assert_eq(27, pow3_3)
}

// 测试最大公约数（GCD）计算（欧几里得算法模拟）
test "gcd_calculation" {
  // GCD(48, 18) = GCD(18, 48 % 18) = GCD(18, 12) = GCD(12, 6) = GCD(6, 0) = 6
  // 由于我们没有模运算，我们使用减法模拟
  let a = 48
  let b = 18
  // 48 - 18*2 = 12
  let remainder = @azimuth.add(a, @azimuth.multiply(-2, b))
  // 18 - 12 = 6
  let remainder2 = @azimuth.add(b, -remainder)
  @azimuth.assert_eq(6, remainder2)
  
  // GCD(60, 25) = GCD(25, 60 % 25) = GCD(25, 10) = GCD(10, 5) = GCD(5, 0) = 5
  let c = 60
  let d = 25
  // 60 - 25*2 = 10
  let remainder3 = @azimuth.add(c, @azimuth.multiply(-2, d))
  // 25 - 10*2 = 5
  let remainder4 = @azimuth.add(d, @azimuth.multiply(-2, remainder3))
  @azimuth.assert_eq(5, remainder4)
}

// 测试百分比和比例计算
test "percentage_and_ratio_calculations" {
  // 计算百分比：value * percentage / 100
  let value = 500
  let percentage = 20  // 20%
  let result = @azimuth.multiply(value, percentage) / 100
  @azimuth.assert_eq(100, result)  // 500 * 20 / 100 = 100
  
  // 计算另一个百分比
  let value2 = 1200
  let percentage2 = 15  // 15%
  let result2 = @azimuth.multiply(value2, percentage2) / 100
  @azimuth.assert_eq(180, result2)  // 1200 * 15 / 100 = 180
  
  // 计算比例：a / b = c / d => a * d = b * c
  // 如果 3/4 = x/20，那么 x = 3 * 20 / 4 = 15
  let a = 3
  let b = 4
  let d = 20
  let x = @azimuth.multiply(a, d) / b
  @azimuth.assert_eq(15, x)
}

// 测试利息计算（金融应用）
test "interest_calculation" {
  // 简单利息：本金 * 利率 * 时间
  let principal = 1000  // 本金
  let rate = 5  // 年利率（百分比）
  let time = 3  // 年数
  
  // 计算利息：1000 * 5% * 3 = 1000 * 5 / 100 * 3 = 150
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate) / 100, time)
  @azimuth.assert_eq(150, interest)
  
  // 计算本息和：本金 + 利息
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(1150, total)
  
  // 复利计算（简化版）：本息和 = 本金 * (1 + 利率)^时间
  // 对于3年，本息和 = 本金 * (1 + 利率) * (1 + 利率) * (1 + 利率)
  // 这里我们简化计算，不使用真正的复利公式
  let compound_interest = @azimuth.multiply(principal, 105) / 100  // 第一年
  let compound_interest2 = @azimuth.multiply(compound_interest, 105) / 100  // 第二年
  let compound_interest3 = @azimuth.multiply(compound_interest2, 105) / 100  // 第三年
  @azimuth.assert_eq(1157, compound_interest3)  // 向下取整
}
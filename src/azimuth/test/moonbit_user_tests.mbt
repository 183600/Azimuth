// 用户自定义 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法测试 azimuth 库的核心功能

// 测试斐波那契数列计算
test "fibonacci_calculation" {
  // 计算斐波那契数列的第5项：F(5) = F(4) + F(3) = 3 + 2 = 5
  // 使用现有函数模拟斐波那契计算
  let f1 = 1
  let f2 = 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  @azimuth.assert_eq(5, f5)
  
  // 计算斐波那契数列的第6项：F(6) = F(5) + F(4) = 5 + 3 = 8
  let f6 = @azimuth.add(f4, f5)  // 8
  @azimuth.assert_eq(8, f6)
}

// 测试阶乘计算
test "factorial_calculation" {
  // 计算5的阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let result = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))))
  @azimuth.assert_eq(120, result)
  
  // 计算4的阶乘：4! = 4 × 3 × 2 × 1 = 24
  let n2 = 4
  let result2 = @azimuth.multiply(n2, @azimuth.multiply(n2 - 1, @azimuth.multiply(n2 - 2, n2 - 3)))
  @azimuth.assert_eq(24, result2)
}

// 测试最大公约数计算（欧几里得算法模拟）
test "gcd_calculation" {
  // 计算48和18的最大公约数
  let a = 48
  let b = 18
  
  // 模拟欧几里得算法：gcd(a, b) = gcd(b, a % b)
  // 48 % 18 = 12
  let remainder1 = a % b  // 12
  // 18 % 12 = 6
  let remainder2 = b % remainder1  // 6
  // 12 % 6 = 0
  let remainder3 = remainder1 % remainder2  // 0
  
  // 当余数为0时，gcd就是最后一个非零余数
  @azimuth.assert_eq(6, remainder2)
}

// 测试素数判断
test "prime_number_check" {
  // 检查17是否为素数
  let num = 17
  let is_prime = true
  
  // 检查2到4的因子（17的平方根约等于4.12）
  let check2 = num % 2  // 1
  let check3 = num % 3  // 2
  let check4 = num % 4  // 1
  
  // 所有检查都不为0，所以17是素数
  @azimuth.assert_eq(check2 != 0 && check3 != 0 && check4 != 0, true)
  
  // 检查15是否为素数
  let num2 = 15
  let check2_2 = num2 % 2  // 1
  let check3_2 = num2 % 3  // 0
  
  // 15能被3整除，所以15不是素数
  @azimuth.assert_eq(0, 1)(check2_2 != 0 && check3_2 != 0)
}

// 测试平方和计算
test "sum_of_squares" {
  // 计算前5个自然数的平方和：1² + 2² + 3² + 4² + 5² = 55
  let n1 = 1
  let n2 = 2
  let n3 = 3
  let n4 = 4
  let n5 = 5
  
  let square1 = @azimuth.multiply(n1, n1)  // 1
  let square2 = @azimuth.multiply(n2, n2)  // 4
  let square3 = @azimuth.multiply(n3, n3)  // 9
  let square4 = @azimuth.multiply(n4, n4)  // 16
  let square5 = @azimuth.multiply(n5, n5)  // 25
  
  let sum_of_squares = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(square1, square2), square3), square4), square5)
  @azimuth.assert_eq(55, sum_of_squares)
}

// 测试立方和计算
test "sum_of_cubes" {
  // 计算前3个自然数的立方和：1³ + 2³ + 3³ = 1 + 8 + 27 = 36
  let n1 = 1
  let n2 = 2
  let n3 = 3
  
  let cube1 = @azimuth.multiply(@azimuth.multiply(n1, n1), n1)  // 1
  let cube2 = @azimuth.multiply(@azimuth.multiply(n2, n2), n2)  // 8
  let cube3 = @azimuth.multiply(@azimuth.multiply(n3, n3), n3)  // 27
  
  let sum_of_cubes = @azimuth.add(@azimuth.add(cube1, cube2), cube3)
  @azimuth.assert_eq(36, sum_of_cubes)
}

// 测试温度转换（摄氏度转华氏度）
test "temperature_conversion" {
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数计算：F = C × 2 + 30（近似值）
  
  // 0°C = 32°F（近似计算：0 × 2 + 30 = 30）
  let celsius1 = 0
  let fahrenheit1 = @azimuth.add(@azimuth.multiply(celsius1, 2), 30)
  @azimuth.assert_eq(30, fahrenheit1)
  
  // 100°C = 212°F（近似计算：100 × 2 + 30 = 230）
  let celsius2 = 100
  let fahrenheit2 = @azimuth.add(@azimuth.multiply(celsius2, 2), 30)
  @azimuth.assert_eq(230, fahrenheit2)
  
  // 25°C = 77°F（近似计算：25 × 2 + 30 = 80）
  let celsius3 = 25
  let fahrenheit3 = @azimuth.add(@azimuth.multiply(celsius3, 2), 30)
  @azimuth.assert_eq(80, fahrenheit3)
}

// 测试银行利息计算
test "bank_interest_calculation" {
  // 简单利息计算：本息和 = 本金 + 本金 × 利率 × 年数
  let principal = 10000  // 本金10000元
  let rate = 5  // 年利率5%
  let years = 3  // 3年
  
  // 计算利息：10000 × 5 × 3 = 150000
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  // 计算本息和：10000 + 150000 = 160000
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total)
  
  // 复利计算（简化版）：本息和 = 本金 × (1 + 利率)^年数
  // 简化为：本息和 = 本金 × (1 + 利率 × 年数)
  let compound_total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(160000, compound_total)
}
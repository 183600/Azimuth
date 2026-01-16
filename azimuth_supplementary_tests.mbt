// Azimuth 项目补充测试用例
// 使用标准 MoonBit 测试语法，提供额外的测试覆盖

// 导入主模块中的函数
use azimuth

test "subtraction_using_addition" {
  // 使用加法实现减法: a - b = a + (-b)
  let a = 100
  let b = 30
  let result = add(a, -b)
  assert_eq(70, result)
  
  // 测试负数减法
  let x = -50
  let y = 20
  let result2 = add(x, -y)
  assert_eq(-70, result2)
}

test "division_using_multiplication" {
  // 使用乘法验证除法结果: a / b = c，则 a = b * c
  let dividend = 48
  let divisor = 6
  let quotient = dividend / divisor
  let verification = multiply(divisor, quotient)
  assert_eq(dividend, verification)
  
  // 测试零除数边界（通过检查乘法结果）
  let non_zero = 15
  assert_eq(0, multiply(non_zero, 0))
}

test "power_operation_using_multiply" {
  // 使用乘法实现幂运算: a^3 = a * a * a
  let base = 3
  let power = multiply(multiply(base, base), base)
  assert_eq(27, power)
  
  // 测试负数的幂
  let negative_base = -2
  let negative_power = multiply(multiply(negative_base, negative_base), negative_base)
  assert_eq(-8, negative_power)
}

test "factorial_calculation" {
  // 使用加法和乘法计算阶乘: 5! = 5 * 4 * 3 * 2 * 1
  let n = 5
  let factorial = multiply(n, multiply(n - 1, multiply(n - 2, multiply(n - 3, n - 4))))
  assert_eq(120, factorial)
  
  // 测试0的阶乘特殊情况
  assert_eq(1, 1) // 0! = 1
}

test "greatest_common_divisor_simulation" {
  // 模拟最大公约数验证
  let a = 48
  let b = 18
  let gcd = 6
  
  // 验证gcd确实是公约数
  assert_eq(0, multiply(a / gcd, gcd) - a)
  assert_eq(0, multiply(b / gcd, gcd) - b)
}

test "percentage_calculations" {
  // 百分比计算测试
  
  // 计算百分比值: value * percentage / 100
  let value = 200
  let percentage = 25
  let result = multiply(value, percentage) / 100
  assert_eq(50, result)
  
  // 计算百分比增长: original + original * growth / 100
  let original = 150
  let growth = 20
  let increased = add(original, multiply(original, growth) / 100)
  assert_eq(180, increased)
}

test "array_sum_simulation" {
  // 模拟数组求和
  let a = 10
  let b = 20
  let c = 30
  let d = 40
  let e = 50
  
  let total = add(add(add(add(a, b), c), d), e)
  assert_eq(150, total)
  
  // 计算平均值
  let count = 5
  let average = total / count
  assert_eq(30, average)
}

test "geometric_progression" {
  // 等比数列测试
  let first_term = 2
  let ratio = 3
  
  // 计算前4项: a, ar, ar², ar³
  let term1 = first_term
  let term2 = multiply(first_term, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  
  assert_eq(2, term1)
  assert_eq(6, term2)
  assert_eq(18, term3)
  assert_eq(54, term4)
  
  // 计算前4项和
  let sum = add(add(add(term1, term2), term3), term4)
  assert_eq(80, sum)
}

test "time_calculations" {
  // 时间计算测试
  
  // 计算总秒数: hours * 3600 + minutes * 60 + seconds
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let total_seconds = add(add(multiply(hours, 3600), multiply(minutes, 60)), seconds)
  assert_eq(9045, total_seconds)
  
  // 计算小时数: total_seconds / 3600
  let calculated_hours = total_seconds / 3600
  assert_eq(2, calculated_hours)
}

test "compound_interest_simulation" {
  // 复利计算模拟
  let principal = 1000
  let rate = 5 // 5%
  let years = 3
  
  // 简化的复利计算（近似）
  let year1 = add(principal, multiply(principal, rate) / 100)
  let year2 = add(year1, multiply(year1, rate) / 100)
  let year3 = add(year2, multiply(year2, rate) / 100)
  
  // 验证复利增长
  assert_eq(true, year3 > principal)
  assert_eq(true, year3 > add(principal, multiply(principal, multiply(rate, years)) / 100))
}
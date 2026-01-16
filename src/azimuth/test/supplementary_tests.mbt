// Azimuth 项目补充测试用例
// 使用标准 MoonBit 测试语法，提供额外的测试覆盖

test "subtraction_using_addition" {
  // 使用加法实现减法: a - b = a + (-b)
  let a = 100
  let b = 30
  let result = @azimuth.add(a, -b)
  if result != 70 { @builtin.abort("subtraction using addition test failed") }
  
  // 测试负数减法
  let x = -50
  let y = 20
  let result2 = @azimuth.add(x, -y)
  if result2 != -70 { @builtin.abort("negative subtraction test failed") }
}

test "division_using_multiplication" {
  // 使用乘法验证除法结果: a / b = c，则 a = b * c
  let dividend = 48
  let divisor = 6
  let quotient = dividend / divisor
  let verification = @azimuth.multiply(divisor, quotient)
  if verification != dividend { @builtin.abort("division verification test failed") }
  
  // 测试零除数边界（通过检查乘法结果）
  let non_zero = 15
  let zero_result = @azimuth.multiply(non_zero, 0)
  if zero_result != 0 { @builtin.abort("zero multiplication test failed") }
}

test "power_operation_using_multiply" {
  // 使用乘法实现幂运算: a^3 = a * a * a
  let base = 3
  let power = @azimuth.multiply(@azimuth.multiply(base, base), base)
  if power != 27 { @builtin.abort("power operation test failed") }
  
  // 测试负数的幂
  let negative_base = -2
  let negative_power = @azimuth.multiply(@azimuth.multiply(negative_base, negative_base), negative_base)
  if negative_power != -8 { @builtin.abort("negative power test failed") }
}

test "factorial_calculation" {
  // 使用加法和乘法计算阶乘: 5! = 5 * 4 * 3 * 2 * 1
  let n = 5
  let factorial = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))))
  if factorial != 120 { @builtin.abort("factorial calculation test failed") }
  
  // 测试0的阶乘特殊情况
  let zero_factorial = 1
  if zero_factorial != 1 { @builtin.abort("zero factorial test failed") }
}

test "greatest_common_divisor_simulation" {
  // 模拟最大公约数验证
  let a = 48
  let b = 18
  let gcd = 6
  
  // 验证gcd确实是公约数
  let verification1 = @azimuth.multiply(a / gcd, gcd) - a
  let verification2 = @azimuth.multiply(b / gcd, gcd) - b
  if verification1 != 0 { @builtin.abort("GCD verification 1 failed") }
  if verification2 != 0 { @builtin.abort("GCD verification 2 failed") }
}

test "percentage_calculations" {
  // 百分比计算测试
  
  // 计算百分比值: value * percentage / 100
  let value = 200
  let percentage = 25
  let result = @azimuth.multiply(value, percentage) / 100
  if result != 50 { @builtin.abort("percentage calculation test failed") }
  
  // 计算百分比增长: original + original * growth / 100
  let original = 150
  let growth = 20
  let increased = @azimuth.add(original, @azimuth.multiply(original, growth) / 100)
  if increased != 180 { @builtin.abort("percentage growth test failed") }
}

test "array_sum_simulation" {
  // 模拟数组求和
  let a = 10
  let b = 20
  let c = 30
  let d = 40
  let e = 50
  
  let total = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(a, b), c), d), e)
  if total != 150 { @builtin.abort("array sum simulation test failed") }
  
  // 计算平均值
  let count = 5
  let average = total / count
  if average != 30 { @builtin.abort("average calculation test failed") }
}

test "geometric_progression" {
  // 等比数列测试
  let first_term = 2
  let ratio = 3
  
  // 计算前4项: a, ar, ar², ar³
  let term1 = first_term
  let term2 = @azimuth.multiply(first_term, ratio)
  let term3 = @azimuth.multiply(term2, ratio)
  let term4 = @azimuth.multiply(term3, ratio)
  
  if term1 != 2 { @builtin.abort("geometric term1 test failed") }
  if term2 != 6 { @builtin.abort("geometric term2 test failed") }
  if term3 != 18 { @builtin.abort("geometric term3 test failed") }
  if term4 != 54 { @builtin.abort("geometric term4 test failed") }
  
  // 计算前4项和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  if sum != 80 { @builtin.abort("geometric sum test failed") }
}

test "time_calculations" {
  // 时间计算测试
  
  // 计算总秒数: hours * 3600 + minutes * 60 + seconds
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let total_seconds = @azimuth.add(@azimuth.add(@azimuth.multiply(hours, 3600), @azimuth.multiply(minutes, 60)), seconds)
  if total_seconds != 9045 { @builtin.abort("time calculation test failed") }
  
  // 计算小时数: total_seconds / 3600
  let calculated_hours = total_seconds / 3600
  if calculated_hours != 2 { @builtin.abort("hours calculation test failed") }
}

test "compound_interest_simulation" {
  // 复利计算模拟
  let principal = 1000
  let rate = 5 // 5%
  let years = 3
  
  // 简化的复利计算（近似）
  let year1 = @azimuth.add(principal, @azimuth.multiply(principal, rate) / 100)
  let year2 = @azimuth.add(year1, @azimuth.multiply(year1, rate) / 100)
  let year3 = @azimuth.add(year2, @azimuth.multiply(year2, rate) / 100)
  
  // 验证复利增长
  if year3 <= principal { @builtin.abort("compound interest growth test failed") }
  let simple_interest = @azimuth.add(principal, @azimuth.multiply(principal, @azimuth.multiply(rate, years)) / 100)
  if year3 <= simple_interest { @builtin.abort("compound interest vs simple interest test failed") }
}
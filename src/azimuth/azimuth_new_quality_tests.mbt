// Azimuth 项目新增的高质量测试用例
// 使用标准 MoonBit 测试语法，覆盖各种测试场景

test "subtract_function_basic" {
  // 测试减法函数的基本功能
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(10, subtract(0, -10))
}

test "absolute_value_simulation" {
  // 模拟绝对值函数
  let num1 = 15
  let num2 = -15
  let num3 = 0
  
  let abs1 = if num1 >= 0 { num1 } else { -num1 }
  let abs2 = if num2 >= 0 { num2 } else { -num2 }
  let abs3 = if num3 >= 0 { num3 } else { -num3 }
  
  assert_eq(15, abs1)
  assert_eq(15, abs2)
  assert_eq(0, abs3)
}

test "factorial_calculation" {
  // 计算阶乘 n! = n * (n-1) * ... * 1
  let n = 5
  let factorial = multiply(n, multiply(n - 1, multiply(n - 2, multiply(n - 3, n - 4))))
  assert_eq(120, factorial)
}

test "compound_interest_calculation" {
  // 复利计算：本金 × (1 + 利率)^年数
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 简化计算：(1000 × 105%) × 105% × 105%
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1158, year3)
}

test "temperature_conversion" {
  // 温度转换：摄氏度转华氏度 F = C × 9/5 + 32
  let celsius = 25
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  assert_eq(77, fahrenheit)
}

test "geometric_area_calculation" {
  // 几何面积计算：梯形面积 = (上底 + 下底) × 高 ÷ 2
  let top_base = 8
  let bottom_base = 12
  let height = 5
  
  let trapezoid_area = divide_with_ceil(multiply(add(top_base, bottom_base), height), 2)
  assert_eq(50, trapezoid_area)
}

test "digital_root_calculation" {
  // 数根计算：反复求数字之和直到得到一位数
  let number = 9875
  let sum1 = add(add(add(number / 1000, (number / 100) % 10), (number / 10) % 10), number % 10)
  let sum2 = add(sum1 / 10, sum1 % 10)
  
  assert_eq(2, sum2)
}

test "binary_conversion_simulation" {
  // 模拟二进制转换：将二进制数 1010 转换为十进制
  let binary_num = 1010
  let digit1 = (binary_num / 1000) % 10
  let digit2 = (binary_num / 100) % 10
  let digit3 = (binary_num / 10) % 10
  let digit4 = binary_num % 10
  
  let decimal_value = add(add(add(multiply(digit1, 8), multiply(digit2, 4)), multiply(digit3, 2)), multiply(digit4, 1))
  assert_eq(10, decimal_value)
}

test "shipping_cost_calculation" {
  // 运费计算：基础费用 + 重量费用 + 距离费用
  let base_fee = 10
  let weight = 15
  let weight_rate = 2
  let distance = 100
  let distance_rate = 1
  
  let weight_fee = multiply(weight, weight_rate)
  let distance_fee = divide_with_ceil(multiply(distance, distance_rate), 50)
  let total_shipping = add(add(base_fee, weight_fee), distance_fee)
  
  assert_eq(50, total_shipping)
}

test "array_sum_simulation" {
  // 模拟数组求和：计算 [3, 7, 12, 8, 5] 的总和
  let item1 = 3
  let item2 = 7
  let item3 = 12
  let item4 = 8
  let item5 = 5
  
  let total_sum = add(add(add(add(item1, item2), item3), item4), item5)
  let average = divide_with_ceil(total_sum, 5)
  
  assert_eq(35, total_sum)
  assert_eq(7, average)
}
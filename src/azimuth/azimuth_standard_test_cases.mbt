// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "subtract_basic_functionality" {
  // 测试基本减法功能
  assert_eq(2, subtract(5, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-3, subtract(2, 5))
  assert_eq(10, subtract(0, -10))
}

test "power_of_two_simulation" {
  // 模拟2的幂次运算：2^1, 2^2, 2^3, 2^4
  let power_of_2_1 = 2
  let power_of_2_2 = multiply(2, 2)
  let power_of_2_3 = multiply(power_of_2_2, 2)
  let power_of_2_4 = multiply(power_of_2_3, 2)
  
  assert_eq(2, power_of_2_1)
  assert_eq(4, power_of_2_2)
  assert_eq(8, power_of_2_3)
  assert_eq(16, power_of_2_4)
}

test "average_calculation" {
  // �平均值计算
  let numbers_sum = add(add(add(10, 20), 30), 40)
  let count = 4
  let average = divide_with_ceil(numbers_sum, count)
  
  assert_eq(100, numbers_sum)
  assert_eq(25, average)
}

test "modulo_simulation" {
  // 模拟取模运算：a % b = a - (a / b) * b
  let a = 17
  let b = 5
  let quotient = divide_with_ceil(a, b) - 1  // 向下取整
  let remainder = subtract(a, multiply(quotient, b))
  
  assert_eq(3, remainder)
}

test "compound_interest_calculation" {
  // 复利计算：本金1000，利率5%，3年
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(1050, year1_amount)
  assert_eq(1103, year2_amount)
  assert_eq(1158, year3_amount)
}

test "factorial_simulation" {
  // 模拟阶乘计算：5! = 5 * 4 * 3 * 2 * 1
  let factorial_5 = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  let factorial_4 = multiply(4, multiply(3, multiply(2, 1)))
  let factorial_3 = multiply(3, multiply(2, 1))
  
  assert_eq(120, factorial_5)
  assert_eq(24, factorial_4)
  assert_eq(6, factorial_3)
}

test "temperature_conversion" {
  // 温度转换：摄氏度到华氏度 F = C * 9/5 + 32
  let celsius_0 = 0
  let celsius_100 = 100
  let celsius_25 = 25
  
  let fahrenheit_0 = add(divide_with_ceil(multiply(celsius_0, 9), 5), 32)
  let fahrenheit_100 = add(divide_with_ceil(multiply(celsius_100, 9), 5), 32)
  let fahrenheit_25 = add(divide_with_ceil(multiply(celsius_25, 9), 5), 32)
  
  assert_eq(32, fahrenheit_0)
  assert_eq(212, fahrenheit_100)
  assert_eq(77, fahrenheit_25)
}

test "geometric_progression" {
  // 等比数列：2, 6, 18, 54, 162
  let first_term = 2
  let ratio = 3
  
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  
  assert_eq(2, term1)
  assert_eq(6, term2)
  assert_eq(18, term3)
  assert_eq(54, term4)
  assert_eq(162, term5)
}

test "area_perimeter_calculation" {
  // 计算矩形的面积和周长
  let length = 8
  let width = 5
  
  let area = multiply(length, width)
  let perimeter = multiply(2, add(length, width))
  
  assert_eq(40, area)
  assert_eq(26, perimeter)
}

test "discount_calculation" {
  // 折扣计算：原价、折扣率、最终价格
  let original_price = 200
  let discount_percent = 15
  let discount_amount = divide_with_ceil(multiply(original_price, discount_percent), 100)
  let final_price = subtract(original_price, discount_amount)
  
  assert_eq(30, discount_amount)
  assert_eq(170, final_price)
}
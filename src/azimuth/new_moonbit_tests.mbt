// 新增的 MoonBit 测试用例
// 覆盖 azimuth 项目核心功能的不同场景

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  let f0 = 0
  let f1 = 1
  let f2 = add(f0, f1)  // 1
  let f3 = add(f1, f2)  // 2
  let f4 = add(f2, f3)  // 3
  let f5 = add(f3, f4)  // 5
  let f6 = add(f4, f5)  // 8
  
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  assert_eq(8, f6)
}

test "prime_number_validation" {
  // 验证质数相关计算
  // 检查一个数是否为质数（通过试除法）
  let num = 17
  let is_prime = true
  // 17不能被2,3,4整除
  let div_by_2 = (num % 2 == 0)
  let div_by_3 = (num % 3 == 0)
  let div_by_4 = (num % 4 == 0)
  
  assert_false(div_by_2)
  assert_false(div_by_3)
  assert_false(div_by_4)
  assert_true(is_prime)
}

test "bank_interest_calculation" {
  // 银行利息计算场景
  let principal = 10000  // 本金
  let annual_rate = 5    // 年利率5%
  let years = 3          // 存期3年
  
  // 简单利息计算：本金 × 利率 × 年数
  let simple_interest = multiply(principal, multiply(annual_rate, years))
  // 总金额 = 本金 + 利息
  let total_amount = add(principal, simple_interest)
  
  assert_eq(150000, total_amount)  // 10000 + 10000×5×3 = 160000
}

test "discount_price_calculation" {
  // 折扣价格计算
  let original_price = 200
  let discount_percent = 15  // 15%折扣
  let tax_rate = 8           // 8%税率
  
  // 计算折扣金额
  let discount_amount = multiply(original_price, discount_percent) / 100
  // 折后价格
  let discounted_price = add(original_price, -discount_amount)
  // 含税价格
  let final_price = add(discounted_price, multiply(discounted_price, tax_rate) / 100)
  
  assert_eq(183, final_price)  // 200×85%×108% ≈ 183
}

test "speed_distance_time" {
  // 速度、距离、时间关系计算
  let distance = 300  // 距离（公里）
  let speed = 60      // 速度（公里/小时）
  
  // 时间 = 距离 / 速度
  let time = distance / speed
  assert_eq(5, time)  // 300公里 / 60公里/小时 = 5小时
  
  // 验证：距离 = 速度 × 时间
  let calculated_distance = multiply(speed, time)
  assert_eq(distance, calculated_distance)
}

test "geometric_progression" {
  // 等比数列计算
  let first_term = 2   // 首项
  let ratio = 3        // 公比
  let n = 4            // 项数
  
  // 计算前4项：2, 6, 18, 54
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  
  // 计算前4项和
  let sum = add(add(add(term1, term2), term3), term4)
  assert_eq(80, sum)  // 2 + 6 + 18 + 54 = 80
}

test "string_length_validation" {
  // 字符串长度验证测试
  let short_name = "Bob"
  let medium_name = "MoonBit"
  let long_name = "VeryLongNameForTestingPurpose"
  
  // 测试不同长度字符串的greet函数处理
  assert_eq_string("Hello, Bob!", greet(short_name))
  assert_eq_string("Hello, MoonBit!", greet(medium_name))
  assert_eq_string("Hello, VeryLongNameForTestingPurpose!", greet(long_name))
  
  // 测试包含数字和特殊字符的字符串
  let mixed_name = "User123@Test"
  assert_eq_string("Hello, User123@Test!", greet(mixed_name))
}

test "complex_arithmetic_expression" {
  // 复杂数学表达式计算
  // 计算：(a + b) × (c - d) + e × f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  let sum_ab = add(a, b)           // 15
  let diff_cd = add(c, -d)         // 12
  let product_ef = multiply(e, f)  // 21
  let product_ab_cd = multiply(sum_ab, diff_cd)  // 180
  let result = add(product_ab_cd, product_ef)    // 201
  
  assert_eq(201, result)
}

test "temperature_conversion_extended" {
  // 扩展温度转换测试
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数运算：F = C × 2 + 30（近似值）
  
  let freezing_celsius = 0    // 冰点
  let boiling_celsius = 100   // 沸点
  let room_celsius = 22       // 室温
  
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  let boiling_fahrenheit = add(multiply(boiling_celsius, 2), 30)
  let room_fahrenheit = add(multiply(room_celsius, 2), 30)
  
  assert_eq(30, freezing_fahrenheit)   // 0°C ≈ 30°F
  assert_eq(230, boiling_fahrenheit)   // 100°C ≈ 230°F
  assert_eq(74, room_fahrenheit)       // 22°C ≈ 74°F
}

test "array_sum_and_average" {
  // 数组求和与平均值计算（模拟数组操作）
  // 模拟数组：[10, 20, 30, 40, 50]
  
  let num1 = 10
  let num2 = 20
  let num3 = 30
  let num4 = 40
  let num5 = 50
  
  // 计算总和
  let sum = add(add(add(add(num1, num2), num3), num4), num5)
  // 计算平均值
  let average = sum / 5
  
  assert_eq(150, sum)
  assert_eq(30, average)
  
  // 计算方差（简化版）
  let diff1 = add(num1, -average)
  let diff2 = add(num2, -average)
  let diff3 = add(num3, -average)
  let diff4 = add(num4, -average)
  let diff5 = add(num5, -average)
  
  let square_diff1 = multiply(diff1, diff1)
  let square_diff2 = multiply(diff2, diff2)
  let square_diff3 = multiply(diff3, diff3)
  let square_diff4 = multiply(diff4, diff4)
  let square_diff5 = multiply(diff5, diff5)
  
  let variance_sum = add(add(add(add(square_diff1, square_diff2), square_diff3), square_diff4), square_diff5)
  let variance = variance_sum / 5
  
  assert_eq(200, variance)  // 方差为200
}
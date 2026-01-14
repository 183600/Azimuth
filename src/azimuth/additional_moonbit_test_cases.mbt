// 额外的 MoonBit 测试用例
// 这些测试用例展示了不同的应用场景和边界情况

test "fibonacci_sequence" {
  // 测试斐波那契数列：F(7) = 13
  // 使用基本运算计算：F(0)=0, F(1)=1, F(2)=1, F(3)=2, F(4)=3, F(5)=5, F(6)=8, F(7)=13
  let fib_0 = 0
  let fib_1 = 1
  let fib_2 = add(fib_0, fib_1)  // 1
  let fib_3 = add(fib_1, fib_2)  // 2
  let fib_4 = add(fib_2, fib_3)  // 3
  let fib_5 = add(fib_3, fib_4)  // 5
  let fib_6 = add(fib_4, fib_5)  // 8
  let fib_7 = add(fib_5, fib_6)  // 13
  
  assert_eq(13, fib_7)
}

test "prime_number_check" {
  // 测试素数检查：检查7是否为素数
  let num = 7
  let is_prime = true
  
  // 7不能被2、3整除
  assert_true(num % 2 != 0)
  assert_true(num % 3 != 0)
  assert_true(is_prime)
}

test "triangle_area_calculation" {
  // 测试三角形面积计算：海伦公式
  // 边长为3, 4, 5的三角形，半周长p = (3+4+5)/2 = 6
  // 面积 = √(p(p-a)(p-b)(p-c)) = √(6×3×2×1) = √36 = 6
  let a = 3
  let b = 4
  let c = 5
  
  let perimeter = add(add(a, b), c)
  let s = perimeter / 2  // 半周长
  let area_squared = multiply(s, multiply(multiply(s - a, s - b), s - c))
  
  // 由于我们只有整数运算，我们验证面积平方
  assert_eq(36, area_squared)
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度
  // 使用简化公式：F = C × 2 + 30（避免小数）
  let celsius = 20
  let fahrenheit = add(multiply(celsius, 2), 30)
  
  assert_eq(70, fahrenheit)  // 20 × 2 + 30 = 70
  
  // 测试反向转换
  let fahrenheit_back = 70
  let celsius_back = (fahrenheit_back - 30) / 2
  
  assert_eq(20, celsius_back)
}

test "simple_interest_calculation" {
  // 测试简单利息计算
  // 利息 = 本金 × 利率 × 时间
  let principal = 1000
  let rate_percent = 5
  let time_years = 3
  
  let interest = multiply(principal, multiply(rate_percent, time_years))
  // 由于利率是百分比，需要除以100
  let actual_interest = interest / 100
  
  let total_amount = add(principal, actual_interest)
  
  assert_eq(150, actual_interest)  // 1000 × 5 × 3 / 100 = 150
  assert_eq(1150, total_amount)   // 1000 + 150 = 1150
}

test "speed_distance_time" {
  // 测试速度、距离、时间关系
  // 距离 = 速度 × 时间
  let speed = 60  // km/h
  let time = 2.5  // 小时，使用整数表示为250分钟
  let time_minutes = 250
  
  // 距离（公里）= 速度 × 时间（分钟）/ 60
  let distance = multiply(speed, time_minutes) / 60
  
  assert_eq(250, distance)  // 60 × 250 / 60 = 250 公里
  
  // 计算返回时间
  let return_speed = 50  // km/h
  let return_time = multiply(distance, 60) / return_speed
  
  assert_eq(300, return_time)  // 250 × 60 / 50 = 300 分钟
}

test "array_sum_and_average" {
  // 测试数组求和与平均值
  // 模拟数组：[10, 20, 30, 40, 50]
  let num1 = 10
  let num2 = 20
  let num3 = 30
  let num4 = 40
  let num5 = 50
  
  let sum = add(add(add(add(num1, num2), num3), num4), num5)
  let average = sum / 5
  
  assert_eq(150, sum)
  assert_eq(30, average)
}

test "binary_number_conversion" {
  // 测试二进制数转换：二进制1010转十进制
  // 1010₂ = 1×2³ + 0×2² + 1×2¹ + 0×2⁰ = 8 + 0 + 2 + 0 = 10
  let bit3 = 1
  let bit2 = 0
  let bit1 = 1
  let bit0 = 0
  
  let decimal_value = add(add(multiply(bit3, 8), multiply(bit2, 4)), add(multiply(bit1, 2), bit0))
  
  assert_eq(10, decimal_value)
  
  // 测试另一个二进制数：1101₂ = 13
  let binary_1101 = add(add(multiply(1, 8), multiply(1, 4)), add(multiply(0, 2), 1))
  assert_eq(13, binary_1101)
}

test "greeting_with_different_languages" {
  // 测试不同语言的问候
  let english_name = "Alice"
  let chinese_name = "张三"
  let japanese_name = "田中"
  let korean_name = "김철수"
  
  let english_greeting = greet(english_name)
  let chinese_greeting = greet(chinese_name)
  let japanese_greeting = greet(japanese_name)
  let korean_greeting = greet(korean_name)
  
  assert_eq_string("Hello, Alice!", english_greeting)
  assert_eq_string("Hello, 张三!", chinese_greeting)
  assert_eq_string("Hello, 田中!", japanese_greeting)
  assert_eq_string("Hello, 김철수!", korean_greeting)
}

test "complex_expression_evaluation" {
  // 测试复杂表达式求值：(a + b) × (c - d) + e × f
  let a = 8
  let b = 12
  let c = 20
  let d = 5
  let e = 3
  let f = 7
  
  let sum_ab = add(a, b)  // 20
  let diff_cd = add(c, -d)  // 15
  let product_ef = multiply(e, f)  // 21
  
  let result = add(multiply(sum_ab, diff_cd), product_ef)
  
  assert_eq(321, result)  // 20 × 15 + 21 = 300 + 21 = 321
}
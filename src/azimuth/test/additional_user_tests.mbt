// 用户请求的额外 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, add(-3, -7))
  assert_eq(0, add(-5, 5))
  assert_eq(-100, add(-50, -50))
}

test "multiply_by_negative_numbers" {
  // 测试与负数相乘
  assert_eq(-15, multiply(3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(0, multiply(0, -100))
}

test "greet_with_multilingual_names" {
  // 测试多语言名字的问候
  assert_eq_string("Hello, José!", greet("José"))
  assert_eq_string("Hello, 北京!", greet("北京"))
  assert_eq_string("Hello, München!", greet("München"))
  assert_eq_string("Hello, संस्कृतम्!", greet("संस्कृतम्"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂数学序列计算
  // 计算斐波那契数列前几项的和（简化版）
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)  // 2
  let fib4 = add(fib2, fib3)  // 3
  let fib5 = add(fib3, fib4)  // 5
  
  let sum = add(add(add(add(fib1, fib2), fib3), fib4), fib5)
  assert_eq(12, sum)
}

test "business_calculation_discount" {
  // 测试商业计算：折扣和总价
  let unit_price = 120
  let quantity = 5
  let discount_rate = 10  // 10% 折扣
  
  let subtotal = multiply(unit_price, quantity)
  let discount_amount = multiply(subtotal, discount_rate) / 100
  let final_price = add(subtotal, -discount_amount)
  
  assert_eq(540, final_price)  // 120*5 - 120*5*10/100 = 600 - 60 = 540
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 测试温度转换：摄氏度转华氏度
  // 使用简化公式 F = C * 2 + 30 （避免浮点数）
  
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)
  
  let room_celsius = 20
  let room_fahrenheit = add(multiply(room_celsius, 2), 30)
  assert_eq(70, room_fahrenheit)
  
  let boiling_celsius = 100
  let boiling_fahrenheit = add(multiply(boiling_celsius, 2), 30)
  assert_eq(230, boiling_fahrenheit)
}

test "string_processing_with_special_chars" {
  // 测试包含特殊字符的字符串处理
  let email = "user@example.com"
  let phone = "+1-555-123-4567"
  let url = "https://moonbitlang.com"
  
  assert_eq_string("Hello, user@example.com!", greet(email))
  assert_eq_string("Hello, +1-555-123-4567!", greet(phone))
  assert_eq_string("Hello, https://moonbitlang.com!", greet(url))
}

test "geometric_calculations_perimeter" {
  // 测试几何计算：周长
  // 正方形周长：4 * 边长
  let square_side = 7
  let square_perimeter = multiply(4, square_side)
  assert_eq(28, square_perimeter)
  
  // 长方形周长：2 * (长 + 宽)
  let rect_length = 8
  let rect_width = 5
  let rect_perimeter = multiply(2, add(rect_length, rect_width))
  assert_eq(26, rect_perimeter)
  
  // 三角形周长：三边之和
  let side_a = 3
  let side_b = 4
  let side_c = 5
  let triangle_perimeter = add(add(side_a, side_b), side_c)
  assert_eq(12, triangle_perimeter)
}

test "digital_root_calculation" {
  // 测试数字根计算（简化版）
  // 数字根：将数字的各位相加，直到得到一位数
  // 这里我们只测试简单的数字根计算
  
  let num1 = 1234
  let sum1 = add(add(add(1, 2), 3), 4)  // 1+2+3+4 = 10
  let digital_root1 = add(sum1 % 10, sum1 / 10)  // 1+0 = 1
  assert_eq(1, digital_root1)
  
  let num2 = 567
  let sum2 = add(5, add(6, 7))  // 5+6+7 = 18
  let digital_root2 = add(sum2 % 10, sum2 / 10)  // 8+1 = 9
  assert_eq(9, digital_root2)
}

test "array_sum_and_average" {
  // 测试数组求和与平均值（模拟）
  // 模拟数组 [10, 20, 30, 40, 50]
  
  let val1 = 10
  let val2 = 20
  let val3 = 30
  let val4 = 40
  let val5 = 50
  
  // 计算总和
  let total = add(add(add(add(val1, val2), val3), val4), val5)
  assert_eq(150, total)
  
  // 计算平均值
  let count = 5
  let average = total / count
  assert_eq(30, average)
}
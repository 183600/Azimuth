// 新的综合 MoonBit 测试用例

test "factorial_calculation" {
  // 使用 multiply 函数计算阶乘：5! = 5 * 4 * 3 * 2 * 1 = 120
  let n = 5
  
  let step1 = @azimuth.multiply(n, 4)  // 5 * 4 = 20
  let step2 = @azimuth.multiply(step1, 3)  // 20 * 3 = 60
  let step3 = @azimuth.multiply(step2, 2)  // 60 * 2 = 120
  let factorial = @azimuth.multiply(step3, 1)  // 120 * 1 = 120
  
  @azimuth.assert_eq(factorial, 120)
}

test "compound_interest_calculation" {
  // 复利计算：本金 * (1 + 利率)^年数
  // 简化版：使用连乘代替幂运算
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 简化计算：1000 * (1 + 0.05) * (1 + 0.05) * (1 + 0.05)
  // 使用整数近似：1000 + 1000*5*3 = 1000 + 15000 = 16000
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(total, 16000)
}

test "geometric_progression" {
  // 等比数列求和：a + ar + ar² + ar³
  // 首项 a=2，公比 r=3，前4项和：2 + 6 + 18 + 54 = 80
  let a = 2
  let r = 3
  
  let term1 = a  // 2
  let term2 = @azimuth.multiply(a, r)  // 2 * 3 = 6
  let term3 = @azimuth.multiply(term2, r)  // 6 * 3 = 18
  let term4 = @azimuth.multiply(term3, r)  // 18 * 3 = 54
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(sum, 80)
}

test "string_template_processing" {
  // 测试字符串模板处理场景
  let first_name = "John"
  let last_name = "Doe"
  let age = "30"
  
  // 模拟字符串模板："{first_name} {last_name} is {age} years old"
  let full_name = first_name + " " + last_name
  let template = full_name + " is " + age + " years old"
  
  // 使用 greet 函数测试部分拼接
  @azimuth.assert_eq_string(@azimuth.greet(full_name), "Hello, John Doe!")
  @azimuth.assert_eq_string(@azimuth.greet(age), "Hello, 30!")
}

test "array_sum_simulation" {
  // 模拟数组求和：sum([10, 20, 30, 40, 50])
  let elem1 = 10
  let elem2 = 20
  let elem3 = 30
  let elem4 = 40
  let elem5 = 50
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(elem1, elem2), elem3), elem4), elem5)
  @azimuth.assert_eq(sum, 150)
}

test "discount_calculation" {
  // 折扣计算：原价 - 折扣金额
  let original_price = 200
  let discount_percentage = 15
  
  // 计算折扣金额：200 * 15 / 100 = 200 * 0.15 = 30（整数近似）
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(final_price, 170)
}

test "temperature_conversion_formula" {
  // 温度转换：华氏度转摄氏度 C = (F - 32) * 5/9
  // 简化版：C = (F - 32) / 2（整数近似）
  let fahrenheit = 100
  
  // (100 - 32) / 2 = 68 / 2 = 34
  let temp_diff = @azimuth.add(fahrenheit, -32)
  let celsius = temp_diff / 2
  
  @azimuth.assert_eq(celsius, 34)
}

test "perimeter_calculation" {
  // 计算复杂图形的周长
  // 假设一个L形图形，由三个矩形组成
  let rect1_length = 10
  let rect1_width = 4
  let rect2_length = 6
  let rect2_width = 4
  let rect3_length = 6
  let rect3_width = 2
  
  // 计算每个矩形的周长
  let perim1 = @azimuth.multiply(@azimuth.add(rect1_length, rect1_width), 2)
  let perim2 = @azimuth.multiply(@azimuth.add(rect2_length, rect2_width), 2)
  let perim3 = @azimuth.multiply(@azimuth.add(rect3_length, rect3_width), 2)
  
  // L形总周长（减去内部边界的两倍）
  let total_perim = @azimuth.add(@azimuth.add(perim1, perim2), perim3)
  let internal_boundary = @azimuth.multiply(@azimuth.add(rect2_width, rect3_width), 2)
  let l_shape_perimeter = @azimuth.add(total_perim, -internal_boundary)
  
  @azimuth.assert_eq(l_shape_perimeter, 48)
}

test "velocity_calculation" {
  // 速度计算：v = s/t（距离/时间）
  // 使用整数除法模拟
  let distance = 150  // 米
  let time = 5  // 秒
  
  let velocity = distance / time
  
  @azimuth.assert_eq(velocity, 30)
  
  // 计算加速后的速度：v_new = v + a*t
  let acceleration = 2
  let new_time = 3
  let velocity_increase = @azimuth.multiply(acceleration, new_time)
  let new_velocity = @azimuth.add(velocity, velocity_increase)
  
  @azimuth.assert_eq(new_velocity, 36)
}

test "binary_operations_simulation" {
  // 模拟二进制运算：位左移相当于乘以2的幂
  let number = 13
  
  // 模拟 13 << 2 = 13 * 4 = 52
  let shifted_left = @azimuth.multiply(number, 4)
  @azimuth.assert_eq(shifted_left, 52)
  
  // 模拟 13 >> 1 = 13 / 2 = 6（整数除法）
  let shifted_right = number / 2
  @azimuth.assert_eq(shifted_right, 6)
  
  // 模拟按位与：13 & 7 = 5
  // 13 (1101) & 7 (0111) = 0101 (5)
  // 使用模运算模拟
  let and_result = (@azimuth.multiply(number, 7)) % 8
  @azimuth.assert_eq(and_result, 5)
}
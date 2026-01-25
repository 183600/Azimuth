// Azimuth 新增综合测试用例
// 包含10个新的高质量 MoonBit 测试用例，涵盖各种实际应用场景

test "number_system_conversion" {
  // 数制转换测试
  // 十进制转二进制模拟（简化版本）
  let decimal_num = 42
  let binary_32 = 32
  let binary_8 = 8
  let binary_2 = 2
  
  // 42 = 32 + 8 + 2 = 2^5 + 2^3 + 2^1
  let binary_representation = add(add(binary_32, binary_8), binary_2)
  assert_eq(42, binary_representation)
  
  // 验证二进制位权
  let bit_5_value = multiply(1, 32)  // 2^5
  let bit_3_value = multiply(1, 8)   // 2^3
  let bit_1_value = multiply(1, 2)   // 2^1
  let total_binary_value = add(add(bit_5_value, bit_3_value), bit_1_value)
  assert_eq(42, total_binary_value)
}

test "date_time_calculations" {
  // 日期时间计算测试
  let days_in_month = 30
  let months_in_year = 12
  let days_in_year = multiply(days_in_month, months_in_year)
  assert_eq(360, days_in_year)
  
  // 计算周数
  let days_in_week = 7
  let weeks_in_year = divide_with_ceil(days_in_year, days_in_week)
  assert_eq(52, weeks_in_year)  // ceil(360/7) = 52
  
  // 计算工作日（假设每周5个工作日）
  let workdays_per_week = 5
  let workdays_in_year = multiply(weeks_in_year, workdays_per_week)
  assert_eq(260, workdays_in_year)
}

test "statistical_calculations" {
  // 统计计算测试
  // 模拟数据集：[10, 20, 30, 40, 50]
  let data_1 = 10
  let data_2 = 20
  let data_3 = 30
  let data_4 = 40
  let data_5 = 50
  let data_count = 5
  
  // 计算总和
  let sum = add(add(add(add(data_1, data_2), data_3), data_4), data_5)
  assert_eq(150, sum)
  
  // 计算平均值
  let average = divide_with_ceil(sum, data_count)
  assert_eq(30, average)  // ceil(150/5) = 30
  
  // 计算中位数（已排序数据的中间值）
  let median = data_3
  assert_eq(30, median)
}

test "geometric_calculations" {
  // 几何计算测试
  // 矩形面积和周长
  let rectangle_length = 8
  let rectangle_width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(rectangle_length, rectangle_width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(rectangle_length, rectangle_width))
  assert_eq(26, perimeter)
  
  // 圆形面积和周长（简化计算，使用π≈3）
  let circle_radius = 7
  let pi_approximation = 3
  
  // 面积 = π × r²
  let circle_area = multiply(pi_approximation, multiply(circle_radius, circle_radius))
  assert_eq(147, circle_area)  // 3 × 7 × 7 = 147
  
  // 周长 = 2 × π × r
  let circle_circumference = multiply(2, multiply(pi_approximation, circle_radius))
  assert_eq(42, circle_circumference)  // 2 × 3 × 7 = 42
}

test "recursive_calculation_simulation" {
  // 递归计算模拟测试
  // 模拟阶乘计算：5! = 5 × 4 × 3 × 2 × 1
  let factorial_5 = multiply(multiply(multiply(multiply(5, 4), 3), 2), 1)
  assert_eq(120, factorial_5)
  
  // 模拟斐波那契数列：F(6) = F(5) + F(4) = 5 + 3 = 8
  let fib_1 = 1
  let fib_2 = 1
  let fib_3 = add(fib_1, fib_2)  // 2
  let fib_4 = add(fib_2, fib_3)  // 3
  let fib_5 = add(fib_3, fib_4)  // 5
  let fib_6 = add(fib_4, fib_5)  // 8
  assert_eq(8, fib_6)
  
  // 模拟幂运算：2^6 = 2 × 2 × 2 × 2 × 2 × 2
  let power_2_6 = multiply(multiply(multiply(multiply(multiply(2, 2), 2), 2), 2), 2)
  assert_eq(64, power_2_6)
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试各种字符串长度和组合
  let short_name = "AI"
  let medium_name = "MoonBit"
  let long_name = "Artificial Intelligence"
  
  // 验证字符串处理
  assert_eq_string("Hello, AI!", greet(short_name))
  assert_eq_string("Hello, MoonBit!", greet(medium_name))
  assert_eq_string("Hello, Artificial Intelligence!", greet(long_name))
  
  // 测试包含特殊字符的字符串
  let special_chars_1 = "C++"
  let special_chars_2 = "C#"
  let special_chars_3 = "JavaScript"
  
  assert_eq_string("Hello, C++!", greet(special_chars_1))
  assert_eq_string("Hello, C#!", greet(special_chars_2))
  assert_eq_string("Hello, JavaScript!", greet(special_chars_3))
  
  // 测试包含数字的字符串
  let numeric_string_1 = "123"
  let numeric_string_2 = "Version 2.0"
  let numeric_string_3 = "2024"
  
  assert_eq_string("Hello, 123!", greet(numeric_string_1))
  assert_eq_string("Hello, Version 2.0!", greet(numeric_string_2))
  assert_eq_string("Hello, 2024!", greet(numeric_string_3))
}

test "error_handling_comprehensive" {
  // 全面错误处理测试
  // 测试各种可能导致错误的边界情况
  
  // 极大数除法
  let large_num_1 = 1000000
  let large_num_2 = 999999
  let large_division = divide_with_ceil(large_num_1, large_num_2)
  assert_eq(2, large_division)  // ceil(1000000/999999) = 2
  
  // 极小数除法
  let small_num_1 = 1
  let small_num_2 = 1000000
  let small_division = divide_with_ceil(small_num_1, small_num_2)
  assert_eq(1, small_division)  // ceil(1/1000000) = 1
  
  // 负数边界情况
  let neg_boundary_1 = divide_with_ceil(-1, 2)
  let neg_boundary_2 = divide_with_ceil(-1000000, 1)
  let neg_boundary_3 = divide_with_ceil(1, -2)
  
  assert_eq(-1, neg_boundary_1)
  assert_eq(-1000000, neg_boundary_2)
  assert_eq(0, neg_boundary_3)
}

test "performance_related_calculations" {
  // 性能相关计算测试
  // 模拟算法性能比较
  
  // O(n) 线性算法
  let input_size = 1000
  let linear_operations = multiply(input_size, 3)
  assert_eq(3000, linear_operations)
  
  // O(n²) 二次算法
  let quadratic_operations = multiply(input_size, input_size)
  assert_eq(1000000, quadratic_operations)
  
  // O(log n) 对数算法（简化模拟）
  let logarithmic_operations = divide_with_ceil(input_size, 100)
  assert_eq(10, logarithmic_operations)
  
  // 性能提升计算
  let old_performance = 5000  // 毫秒
  let new_performance = 1000  // 毫秒
  let performance_improvement = divide_with_ceil(
    multiply(subtract(old_performance, new_performance), 100),
    old_performance
  )
  assert_eq(80, performance_improvement)  // ceil((5000-1000)*100/5000) = 80%
}

test "mathematical_sequences" {
  // 数学序列测试
  // 等差数列：a_n = a_1 + (n-1)d
  let arithmetic_first = 3
  let arithmetic_diff = 5
  let arithmetic_term_5 = add(arithmetic_first, multiply(arithmetic_diff, 4))
  assert_eq(23, arithmetic_term_5)  // 3 + 4*5 = 23
  
  // 等比数列：a_n = a_1 × r^(n-1)
  let geometric_first = 2
  let geometric_ratio = 3
  let geometric_term_4 = multiply(geometric_first, multiply(multiply(geometric_ratio, geometric_ratio), geometric_ratio))
  assert_eq(54, geometric_term_4)  // 2 × 3³ = 54
  
  // 平方数序列：1, 4, 9, 16, 25...
  let square_1 = multiply(1, 1)
  let square_2 = multiply(2, 2)
  let square_3 = multiply(3, 3)
  let square_4 = multiply(4, 4)
  let square_5 = multiply(5, 5)
  
  assert_eq(1, square_1)
  assert_eq(4, square_2)
  assert_eq(9, square_3)
  assert_eq(16, square_4)
  assert_eq(25, square_5)
  
  // 立方数序列：1, 8, 27, 64...
  let cube_1 = multiply(multiply(1, 1), 1)
  let cube_2 = multiply(multiply(2, 2), 2)
  let cube_3 = multiply(multiply(3, 3), 3)
  let cube_4 = multiply(multiply(4, 4), 4)
  
  assert_eq(1, cube_1)
  assert_eq(8, cube_2)
  assert_eq(27, cube_3)
  assert_eq(64, cube_4)
}

test "real_world_business_scenarios" {
  // 真实世界商业场景测试
  // 电子商务订单处理
  let item_1_price = 299
  let item_1_quantity = 2
  let item_2_price = 199
  let item_2_quantity = 3
  let item_3_price = 99
  let item_3_quantity = 1
  
  // 计算小计
  let subtotal_1 = multiply(item_1_price, item_1_quantity)
  let subtotal_2 = multiply(item_2_price, item_2_quantity)
  let subtotal_3 = multiply(item_3_price, item_3_quantity)
  
  assert_eq(598, subtotal_1)  // 299 × 2
  assert_eq(597, subtotal_2)  // 199 × 3
  assert_eq(99, subtotal_3)   // 99 × 1
  
  // 计算订单总额
  let order_subtotal = add(add(subtotal_1, subtotal_2), subtotal_3)
  assert_eq(1294, order_subtotal)
  
  // 计算税费（8%）
  let tax_rate = 8
  let tax_amount = divide_with_ceil(multiply(order_subtotal, tax_rate), 100)
  assert_eq(104, tax_amount)  // ceil(1294 × 8 / 100)
  
  // 计算运费（满1000免运费，否则99）
  let shipping_fee = 99
  let free_shipping_threshold = 1000
  let final_shipping = if (order_subtotal >= free_shipping_threshold) { 0 } else { shipping_fee }
  let total_amount = add(add(order_subtotal, tax_amount), final_shipping)
  
  assert_eq(0, final_shipping)  // 免运费
  assert_eq(1398, total_amount)  // 1294 + 104 + 0
  
  // 计算分期付款（12期）
  let installment_months = 12
  let monthly_payment = divide_with_ceil(total_amount, installment_months)
  assert_eq(117, monthly_payment)  // ceil(1398 / 12)
  
  // 验证总支付金额
  let total_paid = multiply(monthly_payment, installment_months)
  assert_eq(1404, total_paid)  // 117 × 12
}
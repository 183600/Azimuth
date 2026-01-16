// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含10个测试用例

test "string_concatenation_with_special_chars" {
  // 测试包含特殊字符的字符串连接
  assert_eq_string("Hello, C++!", greet("C++"))
  assert_eq_string("Hello, Java/C#!", greet("Java/C#"))
  assert_eq_string("Hello, Python@3.9!", greet("Python@3.9"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂算术序列计算
  // 计算 (1+2+3+4+5) * (6+7+8+9+10) = 15 * 40 = 600
  let sum1 = add(add(add(add(1, 2), 3), 4), 5)
  let sum2 = add(add(add(add(6, 7), 8), 9), 10)
  let result = multiply(sum1, sum2)
  assert_eq(600, result)
}

test "temperature_conversion_comprehensive" {
  // 测试温度转换（摄氏度转华氏度）：F = C * 9/5 + 32
  // 简化为整数：F = C * 2 + 30 - C/5
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), add(30, multiply(celsius, -1) / 5))
  assert_eq(77, fahrenheit)
  
  // 测试冰点
  let freezing = add(multiply(0, 2), add(30, multiply(0, -1) / 5))
  assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = add(multiply(100, 2), add(30, multiply(100, -1) / 5))
  assert_eq(210, boiling)
}

test "financial_compound_interest" {
  // 测试复利计算：A = P * (1 + r)^n
  // 简化为整数计算：10000 * (1 + 5%)^3 ≈ 11576
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 第一年：10000 * 105 = 1050000 (扩大100倍)
  let year1 = multiply(principal, add(100, rate))
  // 第二年：1050000 * 105 = 110250000
  let year2 = multiply(year1, add(100, rate))
  // 第三年：110250000 * 105 = 115762500
  let year3 = multiply(year2, add(100, rate))
  // 缩小10000倍得到最终结果
  let final_amount = year3 / 10000
  
  assert_eq(11576, final_amount)
}

test "geometric_calculations" {
  // 测试几何计算
  let radius = 7
  
  // 圆周长：2 * π * r ≈ 2 * 22/7 * 7 = 44
  let circumference = multiply(2, multiply(22, multiply(radius, 1) / 7))
  assert_eq(44, circumference)
  
  // 圆面积：π * r² ≈ 22/7 * 49 = 154
  let area = multiply(22, multiply(radius, radius) / 7)
  assert_eq(154, area)
  
  // 球体体积：4/3 * π * r³ ≈ 4/3 * 22/7 * 343 = 1436
  let volume = multiply(4, multiply(22, multiply(radius, multiply(radius, radius)) / 7)) / 3
  assert_eq(1436, volume)
}

test "digital_root_calculation" {
  // 测试数字根计算（反复求数字之和直到个位数）
  // 9875 的数字根：9+8+7+5 = 29, 2+9 = 11, 1+1 = 2
  let number = 9875
  
  // 计算各位数字之和
  let digit1 = number / 1000
  let digit2 = (number % 1000) / 100
  let digit3 = (number % 100) / 10
  let digit4 = number % 10
  
  let sum1 = add(add(add(digit1, digit2), digit3), digit4)
  // sum1 = 9 + 8 + 7 + 5 = 29
  
  // 计算数字根
  let sum2 = add(sum1 / 10, sum1 % 10)
  // sum2 = 2 + 9 = 11
  
  let digital_root = add(sum2 / 10, sum2 % 10)
  // digital_root = 1 + 1 = 2
  
  assert_eq(2, digital_root)
}

test "shopping_cart_with_discount" {
  // 测试带折扣的购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount_threshold = 1000
  let discount_rate = 10
  let shipping = 20
  
  // 计算商品总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let subtotal = add(total1, total2)
  
  // 计算折扣（如果超过阈值）
  let discount = if (subtotal > discount_threshold) {
    multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算最终总价
  let final_total = add(add(subtotal, multiply(discount, -1)), shipping)
  
  assert_eq(1176, final_total)
}

test "time_zone_conversion" {
  // 测试时区转换
  let local_hour = 15  // 下午3点
  let time_difference = 8  // 与UTC时差8小时
  
  // 转换为UTC时间
  let utc_hour = add(local_hour, multiply(time_difference, -1))
  // 处理负数时间
  let adjusted_utc = if (utc_hour < 0) { add(utc_hour, 24) } else { utc_hour }
  
  assert_eq(7, adjusted_utc)
  
  // 从UTC转换回本地时间
  let converted_back = add(adjusted_utc, time_difference)
  let adjusted_local = if (converted_back >= 24) { add(converted_back, -24) } else { converted_back }
  
  assert_eq(local_hour, adjusted_local)
}

test "binary_search_simulation" {
  // 模拟二分查找的计算
  let array_size = 100
  let target_index = 37
  let target_value = add(target_index, 1)  // 假设值为索引+1
  
  // 模拟二分查找的步骤计算
  let low = 0
  let high = add(array_size, -1)
  
  // 第一次查找：(0 + 99) / 2 = 49
  let mid1 = add(low, high) / 2
  // 第二次查找：(0 + 48) / 2 = 24
  let mid2 = add(low, add(mid1, -1)) / 2
  // 第三次查找：(25 + 48) / 2 = 36
  let mid3 = add(add(mid2, 1), add(mid1, -1)) / 2
  // 第四次查找：(37 + 48) / 2 = 42
  let mid4 = add(add(mid3, 1), add(mid1, -1)) / 2
  // 第五次查找：(37 + 41) / 2 = 39
  let mid5 = add(add(mid3, 1), add(mid4, -1)) / 2
  // 第六次查找：(37 + 38) / 2 = 37
  let mid6 = add(add(mid3, 1), add(mid5, -1)) / 2
  
  assert_eq(target_index, mid6)
  assert_eq(target_value, add(mid6, 1))
}

test "checksum_calculation" {
  // 测试校验和计算（简化版）
  let data = add(add(add(12, 34), 56), 78)  // 12 + 34 + 56 + 78 = 180
  
  // 计算数字之和
  let digit_sum = add(add(data / 100, (data % 100) / 10), data % 10)
  // digit_sum = 1 + 8 + 0 = 9
  
  // 计算校验和（简化为数字之和）
  let checksum = digit_sum
  
  assert_eq(9, checksum)
  
  // 验证数据完整性
  let verification_data = add(add(add(21, 43), 65), 87)  // 21 + 43 + 65 + 87 = 216
  let verification_sum = add(add(verification_data / 100, (verification_data % 100) / 10), verification_data % 10)
  // verification_sum = 2 + 1 + 6 = 9
  
  assert_eq(checksum, verification_sum)
}
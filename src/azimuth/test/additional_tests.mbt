// 额外的 MoonBit 测试用例 - 覆盖更多场景

test "large_number_operations" {
  // 测试大数运算
  @azimuth.assert_eq(@azimuth.add(100000000, 200000000), 300000000)
  @azimuth.assert_eq(@azimuth.add(-100000000, -200000000), -300000000)
  @azimuth.assert_eq(@azimuth.multiply(10000, 10000), 100000000)
  @azimuth.assert_eq(@azimuth.multiply(-10000, 10000), -100000000)
}

test "negative_number_operations" {
  // 测试负数运算的各种情况
  @azimuth.assert_eq(@azimuth.add(-50, -30), -80)
  @azimuth.assert_eq(@azimuth.add(-50, 30), -20)
  @azimuth.assert_eq(@azimuth.add(50, -30), 20)
  @azimuth.assert_eq(@azimuth.multiply(-5, -6), 30)
  @azimuth.assert_eq(@azimuth.multiply(-5, 6), -30)
  @azimuth.assert_eq(@azimuth.multiply(5, -6), -30)
}

test "zero_value_operations" {
  // 测试零值运算的各种情况
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
  @azimuth.assert_eq(@azimuth.add(0, 12345), 12345)
  @azimuth.assert_eq(@azimuth.add(12345, 0), 12345)
  @azimuth.assert_eq(@azimuth.add(0, -12345), -12345)
  @azimuth.assert_eq(@azimuth.add(-12345, 0), -12345)
  
  @azimuth.assert_eq(@azimuth.multiply(0, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(0, 12345), 0)
  @azimuth.assert_eq(@azimuth.multiply(12345, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(0, -12345), 0)
  @azimuth.assert_eq(@azimuth.multiply(-12345, 0), 0)
}

test "string_boundary_cases" {
  // 测试字符串边界情况
  @azimuth.assert_eq_string(@azimuth.greet("A"), "Hello, A!")
  @azimuth.assert_eq_string(@azimuth.greet(" "), "Hello,  !")
  @azimuth.assert_eq_string(@azimuth.greet("   "), "Hello,    !")
  @azimuth.assert_eq_string(@azimuth.greet("!@#$%^&*()"), "Hello, !@#$%^&*()!")
  @azimuth.assert_eq_string(@azimuth.greet("1234567890"), "Hello, 1234567890!")
}

test "discount_calculation" {
  // 测试实际应用场景：折扣计算
  let original_price = 1000
  let discount_percentage = 20  // 20% 折扣
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(discount_amount, 200)  // 1000 * 20 / 100 = 200
  @azimuth.assert_eq(final_price, 800)     // 1000 - 200 = 800
  
  // 测试多个商品的折扣计算
  let item1_price = 500
  let item2_price = 300
  let item3_price = 200
  let subtotal = @azimuth.add(@azimuth.add(item1_price, item2_price), item3_price)
  let total_discount = @azimuth.multiply(subtotal, 15) / 100  // 15% 折扣
  let total_final = @azimuth.add(subtotal, -total_discount)
  
  @azimuth.assert_eq(subtotal, 1000)       // 500 + 300 + 200 = 1000
  @azimuth.assert_eq(total_discount, 150)  // 1000 * 15 / 100 = 150
  @azimuth.assert_eq(total_final, 850)     // 1000 - 150 = 850
}

test "mathematical_formulas" {
  // 测试数学公式：平方和计算
  // 计算 1² + 2² + 3² + 4² + 5²
  let square1 = @azimuth.multiply(1, 1)
  let square2 = @azimuth.multiply(2, 2)
  let square3 = @azimuth.multiply(3, 3)
  let square4 = @azimuth.multiply(4, 4)
  let square5 = @azimuth.multiply(5, 5)
  
  let sum_of_squares = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(square1, square2), square3), square4), square5)
  @azimuth.assert_eq(sum_of_squares, 55)  // 1 + 4 + 9 + 16 + 25 = 55
  
  // 测试立方和计算：1³ + 2³ + 3³
  let cube1 = @azimuth.multiply(@azimuth.multiply(1, 1), 1)
  let cube2 = @azimuth.multiply(@azimuth.multiply(2, 2), 2)
  let cube3 = @azimuth.multiply(@azimuth.multiply(3, 3), 3)
  
  let sum_of_cubes = @azimuth.add(@azimuth.add(cube1, cube2), cube3)
  @azimuth.assert_eq(sum_of_cubes, 36)  // 1 + 8 + 27 = 36
}

test "conditional_logic_scenarios" {
  // 测试条件逻辑场景
  let temperature = 25
  
  // 如果温度大于30，则是热天；如果温度小于10，则是冷天；否则是温和天气
  let is_hot = temperature > 30
  let is_cold = temperature < 10
  let weather_description = if is_hot { "Hot" } else { if is_cold { "Cold" } else { "Mild" } }
  
  @azimuth.assert_eq_string(weather_description, "Mild")
  
  // 测试热天
  let hot_temp = 35
  let is_hot_day = hot_temp > 30
  let hot_weather = if is_hot_day { "Hot" } else { "Not Hot" }
  @azimuth.assert_eq_string(hot_weather, "Hot")
  
  // 测试冷天
  let cold_temp = 5
  let is_cold_day = cold_temp < 10
  let cold_weather = if is_cold_day { "Cold" } else { "Not Cold" }
  @azimuth.assert_eq_string(cold_weather, "Cold")
}

test "data_conversion_and_formatting" {
  // 测试数据转换和格式化
  let year = 2023
  let month = 12
  let day = 25
  
  // 格式化日期字符串
  let year_str = "2023"
  let month_str = "12"
  let day_str = "25"
  let date_string = @azimuth.greet(year_str + "-" + month_str + "-" + day_str)
  
  @azimuth.assert_eq_string(date_string, "Hello, 2023-12-25!")
  
  // 测试数字格式化
  let price = 1999
  let price_str = "1999"
  let formatted_price = @azimuth.greet("$" + price_str)
  @azimuth.assert_eq_string(formatted_price, "Hello, $1999!")
}

test "performance_related_scenarios" {
  // 测试性能相关场景：多次运算
  let base_value = 10
  let iterations = 5
  
  // 连续加法：10 + 10 + 10 + 10 + 10 = 50
  let result1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(base_value, base_value), base_value), base_value), base_value)
  @azimuth.assert_eq(result1, 50)
  
  // 连续乘法：2 * 2 * 2 * 2 = 16
  let result2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2)
  @azimuth.assert_eq(result2, 16)
  
  // 混合运算：(10 + 5) * (8 - 3) = 15 * 5 = 75
  let add_result = @azimuth.add(10, 5)
  let sub_result = @azimuth.add(8, -3)
  let mixed_result = @azimuth.multiply(add_result, sub_result)
  @azimuth.assert_eq(mixed_result, 75)
}

test "error_boundary_handling" {
  // 测试错误边界处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.assert_eq(@azimuth.add(max_val, max_val), max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, min_val), min_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, max_val), max_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, min_val), min_val)
  
  // 测试边界值附近的运算
  @azimuth.assert_eq(@azimuth.add(max_val, -1), max_val - 1)
  @azimuth.assert_eq(@azimuth.add(min_val, 1), min_val + 1)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(min_val, 0), 0)
}
// 额外的 MoonBit 测试用例 - 覆盖更多场景
test "large_number_operations" {
  // 测试大数运算
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(100000000, 200000000), 300000000)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(-100000000, -200000000), -300000000)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(10000, 10000), 100000000)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(-10000, 10000), -100000000)
}
test "negative_number_operations" {
  // 测试负数运算的各种情况
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(-50, -30), -80)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(-50, 30), -20)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(50, -30), 20)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(-5, -6), 30)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(-5, 6), -30)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(5, -6), -30)
}
test "zero_value_operations" {
  // 测试零值运算的各种情况
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(0, 0), 0)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(0, 12345), 12345)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(12345, 0), 12345)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(0, -12345), -12345)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(-12345, 0), -12345)
  
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(0, 0), 0)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(0, 12345), 0)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(12345, 0), 0)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(0, -12345), 0)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(-12345, 0), 0)
}
test "string_boundary_cases" {
  // 测试字符串边界情况
  @azimuth.@azimuth.assert_eq_string(@azimuth.@azimuth.greet("A"), "Hello, A!")
  @azimuth.@azimuth.assert_eq_string(@azimuth.@azimuth.greet(" "), "Hello,  !")
  @azimuth.@azimuth.assert_eq_string(@azimuth.@azimuth.greet("   "), "Hello,    !")
  @azimuth.@azimuth.assert_eq_string(@azimuth.@azimuth.greet("!@#$%^&*()"), "Hello, !@#$%^&*()!")
  @azimuth.@azimuth.assert_eq_string(@azimuth.@azimuth.greet("1234567890"), "Hello, 1234567890!")
}
test "discount_calculation" {
  // 测试实际应用场景：折扣计算
  let original_price = 1000
  let discount_percentage = 20  // 20% 折扣
  let discount_amount = @azimuth.@azimuth.multiply(original_price, discount_percentage) / 100
  let final_price = @azimuth.@azimuth.add(original_price, -discount_amount)
  
  @azimuth.@azimuth.assert_eq(discount_amount, 200)  // 1000 * 20 / 100 = 200
  @azimuth.@azimuth.assert_eq(final_price, 800)     // 1000 - 200 = 800
  
  // 测试多个商品的折扣计算
  let item1_price = 500
  let item2_price = 300
  let item3_price = 200
  let subtotal = @azimuth.@azimuth.add(@azimuth.@azimuth.add(item1_price, item2_price), item3_price)
  let total_discount = @azimuth.@azimuth.multiply(subtotal, 15) / 100  // 15% 折扣
  let total_final = @azimuth.@azimuth.add(subtotal, -total_discount)
  
  @azimuth.@azimuth.assert_eq(subtotal, 1000)       // 500 + 300 + 200 = 1000
  @azimuth.@azimuth.assert_eq(total_discount, 150)  // 1000 * 15 / 100 = 150
  @azimuth.@azimuth.assert_eq(total_final, 850)     // 1000 - 150 = 850
}
test "mathematical_formulas" {
  // 测试数学公式：平方和计算
  // 计算 1² + 2² + 3² + 4² + 5²
  let square1 = @azimuth.@azimuth.multiply(1, 1)
  let square2 = @azimuth.@azimuth.multiply(2, 2)
  let square3 = @azimuth.@azimuth.multiply(3, 3)
  let square4 = @azimuth.@azimuth.multiply(4, 4)
  let square5 = @azimuth.@azimuth.multiply(5, 5)
  
  let sum_of_squares = @azimuth.@azimuth.add(@azimuth.@azimuth.add(@azimuth.@azimuth.add(@azimuth.@azimuth.add(square1, square2), square3), square4), square5)
  @azimuth.@azimuth.assert_eq(sum_of_squares, 55)  // 1 + 4 + 9 + 16 + 25 = 55
  
  // 测试立方和计算：1³ + 2³ + 3³
  let cube1 = @azimuth.@azimuth.multiply(@azimuth.@azimuth.multiply(1, 1), 1)
  let cube2 = @azimuth.@azimuth.multiply(@azimuth.@azimuth.multiply(2, 2), 2)
  let cube3 = @azimuth.@azimuth.multiply(@azimuth.@azimuth.multiply(3, 3), 3)
  
  let sum_of_cubes = @azimuth.@azimuth.add(@azimuth.@azimuth.add(cube1, cube2), cube3)
  @azimuth.@azimuth.assert_eq(sum_of_cubes, 36)  // 1 + 8 + 27 = 36
}
test "conditional_logic_scenarios" {
  // 测试条件逻辑场景
  let temperature = 25
  
  // 如果温度大于30，则是热天；如果温度小于10，则是冷天；否则是温和天气
  let is_hot = temperature > 30
  let is_cold = temperature < 10
  let weather_description = if is_hot { "Hot" } else { if is_cold { "Cold" } else { "Mild" } }
  
  @azimuth.@azimuth.assert_eq_string(weather_description, "Mild")
  
  // 测试热天
  let hot_temp = 35
  let is_hot_day = hot_temp > 30
  let hot_weather = if is_hot_day { "Hot" } else { "Not Hot" }
  @azimuth.@azimuth.assert_eq_string(hot_weather, "Hot")
  
  // 测试冷天
  let cold_temp = 5
  let is_cold_day = cold_temp < 10
  let cold_weather = if is_cold_day { "Cold" } else { "Not Cold" }
  @azimuth.@azimuth.assert_eq_string(cold_weather, "Cold")
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
  let date_string = @azimuth.@azimuth.greet(year_str + "-" + month_str + "-" + day_str)
  
  @azimuth.@azimuth.assert_eq_string(date_string, "Hello, 2023-12-25!")
  
  // 测试数字格式化
  let price = 1999
  let price_str = "1999"
  let formatted_price = @azimuth.@azimuth.greet("$" + price_str)
  @azimuth.@azimuth.assert_eq_string(formatted_price, "Hello, $1999!")
}
test "performance_related_scenarios" {
  // 测试性能相关场景：多次运算
  let base_value = 10
  let iterations = 5
  
  // 连续加法：10 + 10 + 10 + 10 + 10 = 50
  let result1 = @azimuth.@azimuth.add(@azimuth.@azimuth.add(@azimuth.@azimuth.add(@azimuth.@azimuth.add(base_value, base_value), base_value), base_value), base_value)
  @azimuth.@azimuth.assert_eq(result1, 50)
  
  // 连续乘法：2 * 2 * 2 * 2 = 16
  let result2 = @azimuth.@azimuth.multiply(@azimuth.@azimuth.multiply(@azimuth.@azimuth.multiply(2, 2), 2), 2)
  @azimuth.@azimuth.assert_eq(result2, 16)
  
  // 混合运算：(10 + 5) * (8 - 3) = 15 * 5 = 75
  let add_result = @azimuth.@azimuth.add(10, 5)
  let sub_result = @azimuth.@azimuth.add(8, -3)
  let mixed_result = @azimuth.@azimuth.multiply(add_result, sub_result)
  @azimuth.@azimuth.assert_eq(mixed_result, 75)
}
test "error_boundary_handling" {
  // 测试错误边界处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(max_val, max_val), max_val)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(min_val, min_val), min_val)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(max_val, max_val), max_val)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(min_val, min_val), min_val)
  
  // 测试边界值附近的运算
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(max_val, -1), max_val - 1)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(min_val, 1), min_val + 1)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(max_val, 0), 0)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(min_val, 0), 0)
}
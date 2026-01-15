// MoonBit 测试用例集
// 覆盖核心功能和边界情况

test "boundary_value_addition" {
  // 测试边界值的加法运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与正数相加（应溢出并返回最大值）
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 测试最小值与负数相加（应溢出并返回最小值）
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试最大值与最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "boundary_value_multiplication" {
  // 测试边界值的乘法运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与2相乘（应溢出）
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值与-1相乘（特殊情况）
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试最小值与2相乘（应溢出）
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试与0和1的运算
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "string_special_characters" {
  // 测试包含特殊字符的字符串处理
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  @azimuth.assert_eq_string("Hello, line1\nline2!", @azimuth.greet("line1\nline2"))
}

test "mathematical_distributive_property" {
  // 测试数学分配律：a × (b + c) = a × b + a × c
  let a = 7
  let b = 5
  let c = 3
  
  // 计算左侧：a × (b + c)
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  
  // 计算右侧：a × b + a × c
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
  
  // 测试另一组数值
  let x = 12
  let y = 8
  let z = 4
  
  let left_side2 = @azimuth.multiply(x, @azimuth.add(y, z))
  let right_side2 = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  
  @azimuth.assert_eq(left_side2, right_side2)
}

test "shopping_cart_total" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount = 50  // 折扣
  
  // 计算每种商品的小计
  let subtotal1 = @azimuth.multiply(item1_price, item1_quantity)
  let subtotal2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算总价
  let total_before_discount = @azimuth.add(subtotal1, subtotal2)
  
  // 应用折扣
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  // 验证结果：299*2 + 199*3 - 50 = 598 + 597 - 50 = 1145
  @azimuth.assert_eq(1145, final_total)
}

test "temperature_conversion" {
  // 测试实际应用场景：温度转换
  // 使用简化公式：F = C × 2 + 30
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  // 测试室温
  let room_celsius = 20
  let room_fahrenheit = @azimuth.add(@azimuth.multiply(room_celsius, 2), 30)
  @azimuth.assert_eq(70, room_fahrenheit)
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
}

test "overflow_protection" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(max_val, max_val) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.add(min_val, min_val) == min_val)
  
  // 测试乘法溢出保护
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, max_val) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 2) == min_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -2) == min_val)
}

test "complex_calculations" {
  // 测试复合运算：多个函数的组合使用
  let length = 12
  let width = 8
  let height = 5
  
  // 计算矩形面积：长 × 宽
  let area = @azimuth.multiply(length, width)
  
  // 计算体积：面积 × 高
  let volume = @azimuth.multiply(area, height)
  
  // 计算表面积（简化）：2 × (长×宽 + 长×高 + 宽×高)
  let side1 = @azimuth.multiply(length, width)
  let side2 = @azimuth.multiply(length, height)
  let side3 = @azimuth.multiply(width, height)
  let surface_area = @azimuth.multiply(2, @azimuth.add(@azimuth.add(side1, side2), side3))
  
  @azimuth.assert_eq(96, area)      // 12 × 8 = 96
  @azimuth.assert_eq(480, volume)   // 96 × 5 = 480
  @azimuth.assert_eq(392, surface_area)  // 2 × (96 + 60 + 40) = 392
}

test "large_number_operations" {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  let large_num3 = 3000
  
  // 测试大数相加
  let sum_result = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(3000000, sum_result)
  
  // 测试大数相乘
  let product_result = @azimuth.multiply(large_num1, large_num3)
  @azimuth.assert_eq(2147483647, product_result)  // 由于溢出保护，结果应为最大值
  
  // 测试连续运算
  let complex_result = @azimuth.add(@azimuth.multiply(large_num1, 2), @azimuth.multiply(large_num2, 3))
  @azimuth.assert_eq(8000000, complex_result)  // 1000000×2 + 2000000×3 = 2000000 + 6000000 = 8000000
}

test "string_boundary_cases" {
  // 测试字符串边界情况
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringThatTestsTheGreetingFunctionWithExtendedInput"
  let expected_long = "Hello, " + long_string + "!"
  @azimuth.assert_eq_string(expected_long, @azimuth.greet(long_string))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,   LeadingSpace!", @azimuth.greet("  LeadingSpace"))
  @azimuth.assert_eq_string("Hello, TrailingSpace   !", @azimuth.greet("TrailingSpace  "))
}
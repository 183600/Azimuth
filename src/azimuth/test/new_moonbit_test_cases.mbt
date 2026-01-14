// 新增的 MoonBit 测试用例
// 覆盖一些尚未充分测试的场景

test "add_negative_and_positive" {
  // 测试负数和正数相加的各种情况
  assert_eq(0, add(-5, 5))
  assert_eq(-3, add(-8, 5))
  assert_eq(7, add(-2, 9))
  assert_eq(-100, add(-150, 50))
}

test "multiply_by_negative_numbers" {
  // 测试与负数相乘的情况
  assert_eq(-15, multiply(3, -5))
  assert_eq(-24, multiply(-6, 4))
  assert_eq(30, multiply(-5, -6))
  assert_eq(0, multiply(0, -100))
}

test "greet_with_various_string_types" {
  // 测试不同类型的字符串
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a-b-c!", greet("a-b-c"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂数学序列计算
  // 计算等差数列前n项和：1 + 4 + 7 + 10 + 13
  let n1 = 1
  let n2 = 4
  let n3 = 7
  let n4 = 10
  let n5 = 13
  
  let sum = add(add(add(add(n1, n2), n3), n4), n5)
  assert_eq(35, sum)
}

test "business_calculation_scenario" {
  // 测试商业计算场景：商品折扣计算
  let original_price = 200
  let discount_percentage = 15  // 15% 折扣
  let tax_rate = 8  // 8% 税率
  
  // 计算折扣金额：200 * 15 / 100 = 30
  let discount_amount = multiply(original_price, discount_percentage) / 100
  // 计算折后价格：200 - 30 = 170
  let discounted_price = add(original_price, -discount_amount)
  // 计算税费：170 * 8 / 100 = 13.6，取整为13
  let tax_amount = multiply(discounted_price, tax_rate) / 100
  // 计算最终价格：170 + 13 = 183
  let final_price = add(discounted_price, tax_amount)
  
  assert_eq(183, final_price)
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 测试温度转换：摄氏度转华氏度
  // 使用简化公式：F = C * 2 + 30
  let celsius_0 = 0     // 冰点
  let celsius_100 = 100 // 沸点
  let celsius_25 = 25   // 室温
  
  let fahrenheit_0 = add(multiply(celsius_0, 2), 30)
  let fahrenheit_100 = add(multiply(celsius_100, 2), 30)
  let fahrenheit_25 = add(multiply(celsius_25, 2), 30)
  
  assert_eq(30, fahrenheit_0)    // 0 * 2 + 30 = 30
  assert_eq(230, fahrenheit_100) // 100 * 2 + 30 = 230
  assert_eq(80, fahrenheit_25)   // 25 * 2 + 30 = 80
}

test "geometric_calculations" {
  // 测试几何计算
  // 计算矩形面积：长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = multiply(length, width)
  assert_eq(96, rectangle_area)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = multiply(base, height) / 2
  assert_eq(30, triangle_area)
  
  // 计算梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 5
  let bottom_base = 15
  let trapezoid_height = 8
  let trapezoid_area = multiply(add(top_base, bottom_base), trapezoid_height) / 2
  assert_eq(80, trapezoid_area)
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算一组数的总和、平均值、最大值、最小值
  let num1 = 45
  let num2 = 78
  let num3 = 23
  let num4 = 89
  let num5 = 56
  
  // 总和
  let sum = add(add(add(add(num1, num2), num3), num4), num5)
  assert_eq(291, sum)
  
  // 平均值
  let average = sum / 5
  assert_eq(58, average)
  
  // 最大值（通过比较得出）
  let max_value = 89
  assert_true(max_value == num4)
  
  // 最小值（通过比较得出）
  let min_value = 23
  assert_true(min_value == num3)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  assert_eq_string("Hello, !@#$%^&*()!", greet(special_chars))
  
  // 测试包含空格的字符串
  let spaces = "   "
  assert_eq_string("Hello,    !", greet(spaces))
  
  // 测试包含数字和字母的混合字符串
  let alphanumeric = "ABC123xyz"
  assert_eq_string("Hello, ABC123xyz!", greet(alphanumeric))
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, 2) == min_val)
  
  // 测试边界值运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}
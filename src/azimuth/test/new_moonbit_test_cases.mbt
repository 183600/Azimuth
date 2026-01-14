// 新的 MoonBit 测试用例
// 这些测试用例展示了 MoonBit 的标准测试语法和各种测试场景

// 测试1: 数值范围验证
test "numeric_range_validation" {
  // 测试正数范围
  azimuth::@azimuth.assert_true(azimuth::@azimuth.add(1000000, 2000000) > 0)
  azimuth::@azimuth.assert_true(azimuth::@azimuth.multiply(1000, 1000) > 0)
  
  // 测试负数范围
  azimuth::@azimuth.assert_true(azimuth::@azimuth.add(-1000000, -2000000) < 0)
  azimuth::@azimuth.assert_true(azimuth::@azimuth.multiply(-1000, 1000) < 0)
  
  // 测试零值
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.multiply(0, 123456789))
  azimuth::@azimuth.assert_eq(123456789, azimuth::@azimuth.add(0, 123456789))
}

// 测试2: 字符串连接性能测试
test "string_concatenation_performance" {
  // 测试短字符串连接
  let short_name = "Bob"
  let short_result = azimuth::@azimuth.greet(short_name)
  azimuth::@azimuth.assert_eq_string("Hello, Bob!", short_result)
  
  // 测试中等长度字符串连接
  let medium_name = "Alexander"
  let medium_result = azimuth::@azimuth.greet(medium_name)
  azimuth::@azimuth.assert_eq_string("Hello, Alexander!", medium_result)
  
  // 测试包含空格的字符串
  let spaced_name = "Mary Jane"
  let spaced_result = azimuth::@azimuth.greet(spaced_name)
  azimuth::@azimuth.assert_eq_string("Hello, Mary Jane!", spaced_result)
}

// 测试3: 数学函数组合测试
test "math_function_combination" {
  // 测试 (a + b) * c - d 的复合运算
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  // 步骤计算
  let sum_ab = azimuth::@azimuth.add(a, b)  // 5 + 3 = 8
  let product = azimuth::@azimuth.multiply(sum_ab, c)  // 8 * 4 = 32
  let result = azimuth::@azimuth.add(product, -d)  // 32 - 10 = 22
  
  azimuth::@azimuth.assert_eq(22, result)
}

// 测试4: 边界值特殊处理
test "boundary_value_special_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与正数相加
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, 100))
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, max_val))
  
  // 测试最小值与负数相加
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, -100))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, min_val))
  
  // 测试最大值与负数相乘
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(max_val, -1))
  
  // 测试最小值与奇数相乘
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, 3))
}

// 测试5: 平均值计算
test "average_calculation" {
  // 计算三个数的平均值：(a + b + c) / 3
  let num1 = 90
  let num2 = 80
  let num3 = 85
  
  // 计算总和：90 + 80 + 85 = 255
  let sum = azimuth::@azimuth.add(azimuth::@azimuth.add(num1, num2), num3)
  // 计算平均值：255 / 3 = 85（整数除法）
  let average = sum / 3
  
  azimuth::@azimuth.assert_eq(85, average)
}

// 测试6: 面积计算
test "area_calculation" {
  // 计算矩形面积：长 × 宽
  let length = 12
  let width = 8
  
  let rectangle_area = azimuth::@azimuth.multiply(length, width)
  azimuth::@azimuth.assert_eq(96, rectangle_area)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  
  let triangle_area = azimuth::@azimuth.multiply(base, height) / 2
  azimuth::@azimuth.assert_eq(30, triangle_area)
}

// 测试7: 字符串格式化场景
test "string_formatting_scenarios" {
  // 测试数字字符串
  let numeric_name = "12345"
  azimuth::@azimuth.assert_eq_string("Hello, 12345!", azimuth::@azimuth.greet(numeric_name))
  
  // 测试混合字符串
  let mixed_name = "user_2023"
  azimuth::@azimuth.assert_eq_string("Hello, user_2023!", azimuth::@azimuth.greet(mixed_name))
  
  // 测试单字符
  let single_char = "A"
  azimuth::@azimuth.assert_eq_string("Hello, A!", azimuth::@azimuth.greet(single_char))
}

// 测试8: 平方和计算
test "sum_of_squares_calculation" {
  // 计算前n个自然数的平方和：1² + 2² + 3² + ... + n²
  let n = 5
  
  // 手动计算：1² + 2² + 3² + 4² + 5² = 1 + 4 + 9 + 16 + 25 = 55
  let square1 = azimuth::@azimuth.multiply(1, 1)
  let square2 = azimuth::@azimuth.multiply(2, 2)
  let square3 = azimuth::@azimuth.multiply(3, 3)
  let square4 = azimuth::@azimuth.multiply(4, 4)
  let square5 = azimuth::@azimuth.multiply(5, 5)
  
  let sum_of_squares = azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(square1, square2), square3), square4), square5)
  azimuth::@azimuth.assert_eq(55, sum_of_squares)
}

// 测试9: 条件逻辑测试
test "conditional_logic_test" {
  // 测试基于比较结果的条件运算
  let x = 15
  let y = 20
  
  // 如果 x < y，则计算 x + y，否则计算 x * y
  let condition = x < y
  let result = if condition { azimuth::@azimuth.add(x, y) } else { azimuth::@azimuth.multiply(x, y) }
  
  // 由于 15 < 20 为真，所以应该计算 15 + 20 = 35
  azimuth::@azimuth.assert_eq(35, result)
  
  // 反向测试
  let condition2 = x > y
  let result2 = if condition2 { azimuth::@azimuth.add(x, y) } else { azimuth::@azimuth.multiply(x, y) }
  
  // 由于 15 > 20 为假，所以应该计算 15 * 20 = 300
  azimuth::@azimuth.assert_eq(300, result2)
}

// 测试10: 数据转换场景
test "data_conversion_scenarios" {
  // 测试数值到字符串的间接转换（通过greet函数）
  let number_str = "42"
  
  // 通过greet函数验证字符串拼接
  let result1 = azimuth::@azimuth.greet(number_str)
  let result2 = azimuth::@azimuth.greet("Answer is " + number_str)
  
  azimuth::@azimuth.assert_eq_string("Hello, 42!", result1)
  azimuth::@azimuth.assert_eq_string("Hello, Answer is 42!", result2)
  
  // 测试布尔值场景（通过字符串表示）
  let true_str = "true"
  let false_str = "false"
  
  azimuth::@azimuth.assert_eq_string("Hello, true!", azimuth::@azimuth.greet(true_str))
  azimuth::@azimuth.assert_eq_string("Hello, false!", azimuth::@azimuth.greet(false_str))
}
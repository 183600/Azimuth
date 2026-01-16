// 新增的 MoonBit 测试用例
// 这些测试用例覆盖了不同的功能和场景，使用标准的 MoonBit 测试语法

// 测试用例1: 负数运算测试
test "negative_number_operations" {
  // 测试负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-2, @azimuth.add(-8, 6))
  
  // 测试负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

// 测试用例2: 复合运算测试
test "compound_operations" {
  // 测试 (a + b) * c - d 的复合运算
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  // 步骤计算
  let sum_ab = @azimuth.add(a, b)  // 5 + 3 = 8
  let product = @azimuth.multiply(sum_ab, c)  // 8 * 4 = 32
  let result = @azimuth.add(product, -d)  // 32 - 10 = 22
  
  @azimuth.assert_eq(22, result)
}

// 测试用例3: 字符串边界情况测试
test "string_boundary_cases" {
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  
  // 测试包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

// 测试用例4: 数学公式应用测试
test "mathematical_formulas" {
  // 测试等差数列求和：n * (a1 + an) / 2
  let n = 10
  let first = 1
  let last = 10
  
  let sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(55, sum)  // 10 * (1 + 10) / 2 = 55
  
  // 测试矩形面积：长 * 宽
  let length = 8
  let width = 6
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, area)
}

// 测试用例5: 边界条件组合测试
test "boundary_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试边界值与0和1的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

// 测试用例6: 实际应用场景测试 - 购物车计算
test "shopping_cart_calculation" {
  // 商品价格和数量
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(total1, total2)
  
  // 验证总价：299*2 + 199*3 = 598 + 597 = 1195
  @azimuth.assert_eq(1195, cart_total)
}

// 测试用例7: 温度转换测试
test "temperature_conversion" {
  // 摄氏度转华氏度（简化版）：F = C * 2 + 30
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)  // 0 * 2 + 30 = 30
  
  // 测试室温
  let room_celsius = 20
  let room_fahrenheit = @azimuth.add(@azimuth.multiply(room_celsius, 2), 30)
  @azimuth.assert_eq(70, room_fahrenheit)  // 20 * 2 + 30 = 70
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)  // 100 * 2 + 30 = 230
}

// 测试用例8: 错误处理测试
test "error_handling" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
}

// 测试用例9: 性能相关计算测试
test "performance_calculations" {
  // 模拟累加计算：1 + 2 + ... + 100
  // 使用公式：n * (n + 1) / 2
  let n = 100
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, sum)
  
  // 模拟平方和计算：1² + 2² + ... + 10²
  // 手动计算前几项验证
  let square1 = @azimuth.multiply(1, 1)
  let square2 = @azimuth.multiply(2, 2)
  let square3 = @azimuth.multiply(3, 3)
  let square4 = @azimuth.multiply(4, 4)
  let square5 = @azimuth.multiply(5, 5)
  
  let sum_of_squares = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(square1, square2), square3), square4), square5)
  @azimuth.assert_eq(55, sum_of_squares)  // 1 + 4 + 9 + 16 + 25 = 55
}

// 测试用例10: 数据转换场景测试
test "data_conversion_scenarios" {
  // 测试数字到字符串的间接转换（通过greet函数）
  let number_str = "2023"
  let result = @azimuth.greet(number_str)
  @azimuth.assert_eq_string("Hello, 2023!", result)
  
  // 测试包含数字和字母的混合字符串
  let mixed_str = "User123"
  let result2 = @azimuth.greet(mixed_str)
  @azimuth.assert_eq_string("Hello, User123!", result2)
  
  // 测试包含空格的字符串
  let spaced_str = "John Doe"
  let result3 = @azimuth.greet(spaced_str)
  @azimuth.assert_eq_string("Hello, John Doe!", result3)
  
  // 测试包含特殊字符的字符串
  let email_str = "test@example.com"
  let result4 = @azimuth.greet(email_str)
  @azimuth.assert_eq_string("Hello, test@example.com!", result4)
}
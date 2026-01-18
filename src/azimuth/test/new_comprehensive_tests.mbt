// 新的综合测试用例 - 使用标准 MoonBit 测试语法
// 测试 azimuth 模块的核心功能

// 导入被测试的模块
use @azimuth

// 测试1: 加法交换律验证
test "addition_commutativity" {
  // 验证 a + b = b + a
  let a = 42
  let b = 17
  
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  
  @azimuth.assert_eq(result1, result2)
  
  // 测试负数情况
  let c = -15
  let d = 23
  
  let result3 = @azimuth.add(c, d)
  let result4 = @azimuth.add(d, c)
  
  @azimuth.assert_eq(result3, result4)
}

// 测试2: 乘法结合律验证
test "multiplication_associativity" {
  // 验证 (a * b) * c = a * (b * c)
  let a = 3
  let b = 4
  let c = 5
  
  let result1 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result2 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  
  @azimuth.assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let x = -2
  let y = 7
  let z = 3
  
  let result3 = @azimuth.multiply(@azimuth.multiply(x, y), z)
  let result4 = @azimuth.multiply(x, @azimuth.multiply(y, z))
  
  @azimuth.assert_eq(result3, result4)
}

// 测试3: 分配律验证
test "distributive_property" {
  // 验证 a * (b + c) = a * b + a * c
  let a = 6
  let b = 7
  let c = 8
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

// 测试4: 字符串处理边界情况
test "string_edge_cases" {
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  
  // 测试包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
}

// 测试5: 极值运算组合
test "extreme_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试边界值附近的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

// 测试6: 实际应用场景 - 订单计算
test "order_calculation_scenario" {
  // 商品价格
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算订单总价
  let order_total = @azimuth.add(total1, total2)
  
  // 验证总价：199*2 + 299*1 = 398 + 299 = 697
  @azimuth.assert_eq(697, order_total)
}

// 测试7: 数学公式验证 - 平方差公式
test "difference_of_squares" {
  // 验证 (a + b) * (a - b) = a² - b²
  let a = 10
  let b = 3
  
  // 计算左边：(a + b) * (a - b)
  let sum = @azimuth.add(a, b)
  let diff = @azimuth.add(a, -b)
  let left_side = @azimuth.multiply(sum, diff)
  
  // 计算右边：a² - b²
  let a_squared = @azimuth.multiply(a, a)
  let b_squared = @azimuth.multiply(b, b)
  let right_side = @azimuth.add(a_squared, -b_squared)
  
  @azimuth.assert_eq(left_side, right_side)
}

// 测试8: 温度转换场景
test "temperature_conversion" {
  // 摄氏度转华氏度（简化版）：F = C * 2 + 30
  let celsius_temp = 25
  let fahrenheit_temp = @azimuth.add(@azimuth.multiply(celsius_temp, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_temp)  // 25 * 2 + 30 = 80
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)  // 0 * 2 + 30 = 30
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)  // 100 * 2 + 30 = 230
}

// 测试9: 复合运算场景
test "complex_calculation" {
  // 计算：(a + b) * (c + d) - e * f
  let a = 5
  let b = 3
  let c = 7
  let d = 2
  let e = 4
  let f = 6
  
  // 步骤计算
  let sum1 = @azimuth.add(a, b)  // 8
  let sum2 = @azimuth.add(c, d)  // 9
  let product1 = @azimuth.multiply(sum1, sum2)  // 72
  let product2 = @azimuth.multiply(e, f)  // 24
  let result = @azimuth.add(product1, -product2)  // 48
  
  @azimuth.assert_eq(48, result)
}

// 测试10: 边界条件错误处理
test "boundary_error_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 验证正常运算
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
  @azimuth.assert_true(@azimuth.multiply(4, 6) == 24)
  @azimuth.assert_true(@azimuth.greet("Test") == "Hello, Test!")
}
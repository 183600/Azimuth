// Azimuth 项目实用 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，提供实用的测试场景

// 导入必要的模块
use azimuth

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  let a = 15
  let b = 7
  
  // 加法测试
  let sum = azimuth.add(a, b)
  @assertion.assert_eq(22, sum)?
  
  // 减法测试（使用加负数实现）
  let difference = azimuth.add(a, -b)
  @assertion.assert_eq(8, difference)?
  
  // 乘法测试
  let product = azimuth.multiply(a, b)
  @assertion.assert_eq(105, product)?
  
  // 除法测试（使用乘法和倒数实现）
  let quotient = azimuth.multiply(a, 100) / azimuth.multiply(b, 100)
  @assertion.assert_eq(2, quotient)?
}

test "string_processing_functions" {
  // 字符串处理函数测试
  let first_name = "John"
  let last_name = "Doe"
  let age = "30"
  
  // 测试字符串连接
  let full_name = azimuth.greet(first_name + " " + last_name)
  @assertion.assert_eq_string("Hello, John Doe!", full_name)?
  
  // 测试数字字符串处理
  let age_greeting = azimuth.greet("Age: " + age)
  @assertion.assert_eq_string("Hello, Age: 30!", age_greeting)?
  
  // 测试空字符串处理
  let empty_greeting = azimuth.greet("")
  @assertion.assert_eq_string("Hello, !", empty_greeting)?
}

test "array_list_operations" {
  // 数组/列表操作测试
  let item1 = 10
  let item2 = 20
  let item3 = 30
  let item4 = 40
  let item5 = 50
  
  // 计算列表总和
  let total = azimuth.add(azimuth.add(azimuth.add(azimuth.add(item1, item2), item3), item4), item5)
  @assertion.assert_eq(150, total)?
  
  // 计算平均值
  let average = total / 5
  @assertion.assert_eq(30, average)?
  
  // 查找最大值（手动比较）
  let max1 = azimuth.add(item1, azimuth.add(item2, -item1) > 0 ? item2 - item1 : 0)
  let max2 = azimuth.add(max1, azimuth.add(item3, -max1) > 0 ? item3 - max1 : 0)
  let max3 = azimuth.add(max2, azimuth.add(item4, -max2) > 0 ? item4 - max2 : 0)
  let max4 = azimuth.add(max3, azimuth.add(item5, -max3) > 0 ? item5 - max3 : 0)
  @assertion.assert_eq(50, max4)?
}

test "business_calculations" {
  // 商业计算测试
  let unit_price = 99
  let quantity = 5
  let discount_rate = 10  // 10% 折扣
  let tax_rate = 8        // 8% 税率
  
  // 计算小计
  let subtotal = azimuth.multiply(unit_price, quantity)
  @assertion.assert_eq(495, subtotal)?
  
  // 计算折扣
  let discount = azimuth.multiply(subtotal, discount_rate) / 100
  @assertion.assert_eq(49, discount)?
  
  // 计算折后价格
  let discounted_price = azimuth.add(subtotal, -discount)
  @assertion.assert_eq(446, discounted_price)?
  
  // 计算税费
  let tax = azimuth.multiply(discounted_price, tax_rate) / 100
  @assertion.assert_eq(35, tax)?
  
  // 计算最终价格
  let final_price = azimuth.add(discounted_price, tax)
  @assertion.assert_eq(481, final_price)?
}

test "unit_conversion" {
  // 单位转换测试
  let meters = 100
  let kilograms = 50
  let celsius = 25
  
  // 米转厘米
  let centimeters = azimuth.multiply(meters, 100)
  @assertion.assert_eq(10000, centimeters)?
  
  // 千克转克
  let grams = azimuth.multiply(kilograms, 1000)
  @assertion.assert_eq(50000, grams)?
  
  // 摄氏度转华氏度（简化版）
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 9), 160) / 5
  @assertion.assert_eq(77, fahrenheit)?
  
  // 公里转米
  let kilometers = 5
  let meters_from_km = azimuth.multiply(kilometers, 1000)
  @assertion.assert_eq(5000, meters_from_km)?
}

test "data_validation" {
  // 数据验证测试
  let input_age = 25
  let min_age = 18
  let max_age = 65
  
  // 年龄范围验证
  let age_valid = azimuth.add(input_age, -min_age) >= 0 && azimuth.add(max_age, -input_age) >= 0
  @assertion.assert_eq(true, age_valid)?
  
  // 测试边界值
  let min_boundary = azimuth.add(min_age, -min_age) >= 0 && azimuth.add(max_age, -min_age) >= 0
  @assertion.assert_eq(true, min_boundary)?
  
  let max_boundary = azimuth.add(max_age, -min_age) >= 0 && azimuth.add(max_age, -max_age) >= 0
  @assertion.assert_eq(true, max_boundary)?
  
  // 测试超出范围
  let too_young = 17
  let too_old = 66
  let young_valid = azimuth.add(too_young, -min_age) >= 0 && azimuth.add(max_age, -too_young) >= 0
  let old_valid = azimuth.add(too_old, -min_age) >= 0 && azimuth.add(max_age, -too_old) >= 0
  @assertion.assert_eq(false, young_valid)?
  @assertion.assert_eq(false, old_valid)?
}

test "algorithm_efficiency" {
  // 算法效率测试
  let n = 10
  
  // 计算阶乘（简化版）
  let factorial = azimuth.multiply(azimuth.multiply(azimuth.multiply(azimuth.multiply(azimuth.multiply(
    azimuth.multiply(azimuth.multiply(azimuth.multiply(1, 2), 3), 4), 5), 6), 7), 8), 9), 10)
  @assertion.assert_eq(3628800, factorial)?
  
  // 计算斐波那契数列（前10项和）
  let fib1 = 1
  let fib2 = 1
  let fib3 = azimuth.add(fib1, fib2)
  let fib4 = azimuth.add(fib2, fib3)
  let fib5 = azimuth.add(fib3, fib4)
  let fib6 = azimuth.add(fib4, fib5)
  let fib7 = azimuth.add(fib5, fib6)
  let fib8 = azimuth.add(fib6, fib7)
  let fib9 = azimuth.add(fib7, fib8)
  let fib10 = azimuth.add(fib8, fib9)
  
  let fib_sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(
    azimuth.add(azimuth.add(fib1, fib2), fib3), fib4), fib5), fib6), fib7), fib8), azimuth.add(fib9, fib10))
  @assertion.assert_eq(143, fib_sum)?
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  let normal_value = 42
  let zero_value = 0
  let negative_value = -10
  
  // 测试除零保护（使用乘法模拟）
  let safe_division = zero_value == 0 ? 0 : azimuth.multiply(normal_value, 100) / azimuth.multiply(zero_value, 100)
  @assertion.assert_eq(0, safe_division)?
  
  // 测试负数输入处理
  let absolute_value = negative_value < 0 ? azimuth.add(0, -negative_value) : negative_value
  @assertion.assert_eq(10, absolute_value)?
  
  // 测试边界值处理
  let max_input = 2147483647
  let safe_addition = azimuth.add(max_input, 0)  // 避免溢出
  @assertion.assert_eq(2147483647, safe_addition)?
}

test "performance_optimization" {
  // 性能优化测试
  let base = 5
  let iterations = 100
  
  // 测试循环展开优化（手动展开）
  let unrolled_sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(
    azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(
      azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(
        base, base), base), base), base), base), base), base), base), base), base), base), base)
  
  // 验证结果：5 × 20 = 100
  @assertion.assert_eq(100, unrolled_sum)?
  
  // 测试预计算优化
  let multiplier = 25
  let precomputed = azimuth.multiply(multiplier, 4)  // 25 × 4 = 100
  @assertion.assert_eq(100, precomputed)?
  
  // 测试位运算优化（使用乘法模拟）
  let bit_shift_value = 8
  let left_shift = azimuth.multiply(bit_shift_value, 2)  // 模拟左移1位
  @assertion.assert_eq(16, left_shift)?
  
  let right_shift = azimuth.multiply(bit_shift_value, 50) / 100  // 模拟右移1位
  @assertion.assert_eq(4, right_shift)?
}
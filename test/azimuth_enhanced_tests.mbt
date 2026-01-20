// 新增的标准 MoonBit 测试用例
// 覆盖核心功能的各种使用场景

test "arithmetic_sequence_sum" {
  // 计算等差数列的和：1 + 4 + 7 + 10 + 13
  // 使用公式：n × (首项 + 末项) / 2
  let n = 5
  let first = 1
  let last = 13
  let sum = multiply(n, add(first, last)) / 2
  assert_eq(35, sum)
}

test "geometric_progression_product" {
  // 计算等比数列的积：2 × 4 × 8 × 16
  // 通过连续相乘实现
  let term1 = 2
  let term2 = multiply(term1, 2)  // 4
  let term3 = multiply(term2, 2)  // 8
  let term4 = multiply(term3, 2)  // 16
  let product = multiply(multiply(multiply(term1, term2), term3), term4)
  assert_eq(1024, product)
}

test "temperature_conversion_scenarios" {
  // 温度转换场景测试
  // 摄氏度转华氏度：F = C × 9/5 + 32，简化为：F = C × 2 + 30
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)
  
  let room_celsius = 20
  let room_fahrenheit = add(multiply(room_celsius, 2), 30)
  assert_eq(70, room_fahrenheit)
  
  let boiling_celsius = 100
  let boiling_fahrenheit = add(multiply(boiling_celsius, 2), 30)
  assert_eq(230, boiling_fahrenheit)
}

test "financial_calculation_compound" {
  // 复合金融计算：本金 + 利息 + 手续费
  let principal = 10000
  let rate = 5  // 5%
  let years = 2
  let fee = 50
  
  // 计算利息：本金 × 利率 × 年数
  let interest = multiply(principal, multiply(rate, years))
  // 计算总额：本金 + 利息 + 手续费
  let total = add(add(principal, interest), fee)
  assert_eq(11050, total)
}

test "string_validation_scenarios" {
  // 字符串验证场景测试
  // 测试各种用户输入类型
  assert_eq_string("Hello, user123!", greet("user123"))
  assert_eq_string("Hello, email@domain.com!", greet("email@domain.com"))
  assert_eq_string("Hello, User_Name!", greet("User_Name"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
}

test "boundary_extreme_values" {
  // 极值边界测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值与普通值的运算
  assert_eq(2147483640, add(max_val, -7))
  assert_eq(-2147483641, add(min_val, 7))
  assert_eq(2147483647, multiply(max_val, 1))
  assert_eq(-2147483648, multiply(min_val, 1))
  
  // 测试连续极值运算
  let chain_result = add(multiply(max_val, 0), min_val)
  assert_eq(min_val, chain_result)
}

test "logical_combination_calculations" {
  // 逻辑组合计算测试
  // 模拟条件表达式：(a > b) ? (a + c) : (b + c)
  let a = 15
  let b = 10
  let c = 5
  
  // 由于 a > b 为真，计算 a + c
  let condition_result = if (a > b) { add(a, c) } else { add(b, c) }
  assert_eq(20, condition_result)
  
  // 反向测试
  let condition_result2 = if (a < b) { add(a, c) } else { add(b, c) }
  assert_eq(15, condition_result2)
}

test "nested_function_calls" {
  // 嵌套函数调用测试
  // 计算：greet("User" + (add(1, 2)).to_string())
  // 由于没有字符串转数字的功能，我们测试数值嵌套
  
  let nested1 = add(multiply(2, 3), add(4, 5))
  assert_eq(15, nested1)
  
  let nested2 = multiply(add(10, 5), add(3, 2))
  assert_eq(75, nested2)
  
  let nested3 = add(multiply(add(2, 3), 4), multiply(5, add(1, 1)))
  assert_eq(30, nested3)
}

test "practical_measurement_conversions" {
  // 实用测量转换测试
  // 长度转换：1米 = 100厘米，1厘米 = 10毫米
  let meters = 2
  let centimeters = multiply(meters, 100)
  let millimeters = multiply(centimeters, 10)
  assert_eq(200, centimeters)
  assert_eq(2000, millimeters)
  
  // 重量转换：1公斤 = 1000克
  let kilograms = 5
  let grams = multiply(kilograms, 1000)
  assert_eq(5000, grams)
  
  // 时间转换：1小时 = 60分钟 = 3600秒
  let hours = 3
  let minutes = multiply(hours, 60)
  let seconds = multiply(minutes, 60)
  assert_eq(180, minutes)
  assert_eq(10800, seconds)
}

test "error_boundary_validation" {
  // 错误边界验证测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护机制
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 验证正常运算
  assert_true(add(100, 200) == 300)
  assert_true(multiply(15, 4) == 60)
  assert_true(greet("Test") == "Hello, Test!")
  
  // 验证断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("Hello", "Hello")
}
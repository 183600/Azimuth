// 增强的 MoonBit 测试用例
// 为 azimuth 包添加额外的测试覆盖

test "large_number_operations" {
  // 测试大数运算
  let million = 1000000
  let result1 = add(million, million)
  assert_eq(2000000, result1)
  
  let result2 = multiply(million, 2)
  assert_eq(2000000, result2)
  
  // 测试大数相加不溢出
  let large_sum = add(1500000000, 1000000000)
  assert_eq(2147483647, large_sum)
}

test "negative_number_operations" {
  // 测试负数运算
  let a = -100
  let b = -200
  
  // 负数相加
  assert_eq(-300, add(a, b))
  
  // 负数相乘
  assert_eq(20000, multiply(a, b))
  
  // 正负数相加
  assert_eq(-100, add(a, 100))
  
  // 正负数相乘
  assert_eq(-20000, multiply(a, 200))
}

test "string_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, $pecial$!", greet("$pecial$"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
}

test "compound_calculations" {
  // 测试复合计算
  // 计算 (a + b) * (c + d) - e
  let a = 10
  let b = 20
  let c = 5
  let d = 15
  let e = 100
  
  let sum1 = add(a, b)
  let sum2 = add(c, d)
  let product = multiply(sum1, sum2)
  let result = add(product, -e)
  
  assert_eq(400, result)
}

test "financial_calculations" {
  // 测试金融计算场景
  // 计算复利：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  let interest_rate = multiply(rate, years)
  let growth_factor = add(1, interest_rate)
  let total = multiply(principal, growth_factor)
  
  assert_eq(160000, total)
}

test "geometry_calculations" {
  // 测试几何计算
  // 计算矩形面积：长 × 宽
  let length = 12
  let width = 8
  let area = multiply(length, width)
  assert_eq(96, area)
  
  // 计算矩形周长：2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(40, perimeter)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度（简化版）：C = (F - 30) ÷ 2
  let converted_back = add(fahrenheit, -30) / 2
  assert_eq(celsius, converted_back)
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算平均值
  let numbers_sum = add(add(add(add(10, 20), 30), 40), 50)
  let count = 5
  let average = numbers_sum / count
  assert_eq(30, average)
  
  // 计算方差（简化版）
  let diff1 = add(10, -average)
  let diff2 = add(20, -average)
  let diff3 = add(30, -average)
  let diff4 = add(40, -average)
  let diff5 = add(50, -average)
  
  let squared_diff_sum = add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), 
                                   multiply(diff3, diff3)), 
                                   add(multiply(diff4, diff4), multiply(diff5, diff5)))
  let variance = squared_diff_sum / count
  assert_eq(200, variance)
}

test "boundary_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与中等值运算
  assert_eq(2147483647, add(max_val, 1000))
  assert_eq(2147483647, multiply(max_val, 2))
  
  // 最小值与中等值运算
  assert_eq(-2147483648, add(min_val, -1000))
  assert_eq(-2147483648, multiply(min_val, 2))
  
  // 边界值之间的运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, add(min_val, 1))
}

test "practical_scenarios" {
  // 测试实际应用场景
  // 计算订单总价：商品价格 × 数量 + 运费 - 折扣
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let discount = 50
  
  let subtotal = multiply(item_price, quantity)
  let total_with_shipping = add(subtotal, shipping)
  let final_total = add(total_with_shipping, -discount)
  
  assert_eq(607, final_total)
  
  // 计算速度：距离 ÷ 时间
  let distance = 300
  let time = 5
  let speed = distance / time
  assert_eq(60, speed)
  
  // 计算工作效率：工作量 ÷ 时间
  let work_done = 1000
  let hours_worked = 8
  let efficiency = work_done / hours_worked
  assert_eq(125, efficiency)
}
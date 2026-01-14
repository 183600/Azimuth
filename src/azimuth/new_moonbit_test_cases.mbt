// 新增的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "string_concatenation_with_spaces" {
  // 测试包含空格的字符串连接
  let result1 = greet("MoonBit Language")
  assert_eq_string("Hello, MoonBit Language!", result1)
  
  let result2 = greet("  Leading and trailing spaces  ")
  assert_eq_string("Hello,   Leading and trailing spaces  !", result2)
}

test "mathematical_sequence_sum" {
  // 计算斐波那契数列前10项的和（简化版）
  // 1, 1, 2, 3, 5, 8, 13, 21, 34, 55
  let fib1 = 1
  let fib2 = 1
  let fib3 = 2
  let fib4 = 3
  let fib5 = 5
  let fib6 = 8
  let fib7 = 13
  let fib8 = 21
  let fib9 = 34
  let fib10 = 55
  
  let sum = add(add(add(add(add(fib1, fib2), add(fib3, fib4)), add(fib5, fib6)), add(fib7, fib8)), add(fib9, fib10))
  assert_eq(143, sum)
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数近似：F = C × 2 + 30
  let celsius_0 = 0      // 冰点
  let celsius_100 = 100  // 沸点
  let celsius_25 = 25    // 室温
  
  let fahrenheit_0 = add(multiply(celsius_0, 2), 30)
  let fahrenheit_100 = add(multiply(celsius_100, 2), 30)
  let fahrenheit_25 = add(multiply(celsius_25, 2), 30)
  
  assert_eq(30, fahrenheit_0)    // 0 × 2 + 30 = 30
  assert_eq(230, fahrenheit_100) // 100 × 2 + 30 = 230
  assert_eq(80, fahrenheit_25)   // 25 × 2 + 30 = 80
}

test "simple_interest_calculation" {
  // 简单利息计算：利息 = 本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 年
  
  let interest = multiply(principal, multiply(rate, time))
  let total_amount = add(principal, interest)
  
  assert_eq(150000, interest)      // 10000 × 5 × 3 = 150000
  assert_eq(160000, total_amount)  // 10000 + 150000 = 160000
}

test "geometric_progression_sum" {
  // 计算等比数列的和：2 + 4 + 8 + 16 + 32
  let term1 = 2
  let term2 = 4
  let term3 = 8
  let term4 = 16
  let term5 = 32
  
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(62, sum)
}

test "perimeter_calculation" {
  // 计算不同图形的周长
  // 正方形周长：4 × 边长
  let square_side = 5
  let square_perimeter = multiply(4, square_side)
  assert_eq(20, square_perimeter)
  
  // 长方形周长：2 × (长 + 宽)
  let rectangle_length = 8
  let rectangle_width = 3
  let rectangle_perimeter = multiply(2, add(rectangle_length, rectangle_width))
  assert_eq(22, rectangle_perimeter)
  
  // 三角形周长：三边之和
  let triangle_side1 = 5
  let triangle_side2 = 7
  let triangle_side3 = 9
  let triangle_perimeter = add(add(triangle_side1, triangle_side2), triangle_side3)
  assert_eq(21, triangle_perimeter)
}

test "discount_calculation" {
  // 折扣计算：最终价格 = 原价 - 折扣金额
  let original_price = 200
  let discount_percentage = 15  // 15%
  
  let discount_amount = multiply(original_price, discount_percentage) / 100
  let final_price = add(original_price, -discount_amount)
  
  assert_eq(30, discount_amount)  // 200 × 15 / 100 = 30
  assert_eq(170, final_price)     // 200 - 30 = 170
}

test "digital_root_calculation" {
  // 计算数字根（简化版）：对于两位数，数字根 = 十位数 + 个位数
  let number1 = 47
  let number2 = 83
  let number3 = 29
  
  let tens1 = number1 / 10
  let ones1 = number1 % 10
  let digital_root1 = add(tens1, ones1)
  
  let tens2 = number2 / 10
  let ones2 = number2 % 10
  let digital_root2 = add(tens2, ones2)
  
  let tens3 = number3 / 10
  let ones3 = number3 % 10
  let digital_root3 = add(tens3, ones3)
  
  assert_eq(11, digital_root1)  // 4 + 7 = 11
  assert_eq(11, digital_root2)  // 8 + 3 = 11
  assert_eq(11, digital_root3)  // 2 + 9 = 11
}

test "binary_operations_simulation" {
  // 模拟二进制运算
  let a = 12  // 二进制 1100
  let b = 5   // 二进制 0101
  
  // 模拟位与运算（简化）：取较小的数
  let bitwise_and = if (a < b) { a } else { b }
  assert_eq(5, bitwise_and)
  
  // 模拟位或运算（简化）：取较大的数
  let bitwise_or = if (a > b) { a } else { b }
  assert_eq(12, bitwise_or)
  
  // 模拟位异或运算（简化）：两数之差的绝对值
  let xor_result = if (a > b) { add(a, -b) } else { add(b, -a) }
  assert_eq(7, xor_result)
}

test "complex_business_calculation" {
  // 复杂业务计算：计算商品的总成本
  let unit_cost = 50
  let quantity = 100
  let shipping_cost_per_unit = 5
  let tax_rate = 8  // 8%
  let discount_threshold = 5000
  let discount_rate = 5  // 如果超过阈值，5%折扣
  
  // 计算商品总成本
  let total_product_cost = multiply(unit_cost, quantity)
  
  // 计算总运费
  let total_shipping = multiply(shipping_cost_per_unit, quantity)
  
  // 计算小计
  let subtotal = add(total_product_cost, total_shipping)
  
  // 计算折扣（如果超过阈值）
  let discount_amount = if (subtotal > discount_threshold) {
    multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后金额
  let discounted_subtotal = add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = multiply(discounted_subtotal, tax_rate) / 100
  
  // 计算最终总成本
  let final_total_cost = add(discounted_subtotal, tax_amount)
  
  assert_eq(5500, total_product_cost)      // 50 × 100 = 5500
  assert_eq(500, total_shipping)          // 5 × 100 = 500
  assert_eq(6000, subtotal)               // 5500 + 500 = 6000
  assert_eq(300, discount_amount)         // 6000 × 5 / 100 = 300
  assert_eq(5700, discounted_subtotal)    // 6000 - 300 = 5700
  assert_eq(456, tax_amount)              // 5700 × 8 / 100 = 456
  assert_eq(6156, final_total_cost)       // 5700 + 456 = 6156
}
test "shopping_cart_total" {
  let item1_price = 199
  let item2_price = 299
  let item3_price = 99
  
  let item1_qty = 2
  let item2_qty = 1
  let item3_qty = 3
  
  let subtotal1 = @clean_test.multiply(item1_price, item1_qty)
  let subtotal2 = @clean_test.multiply(item2_price, item2_qty)
  let subtotal3 = @clean_test.multiply(item3_price, item3_qty)
  
  let total = @clean_test.add(@clean_test.add(subtotal1, subtotal2), subtotal3)
  
  @clean_test.assert_eq(199 * 2 + 299 + 99 * 3, total)
}

// 测试用例2: 计算折扣价格
///|
test "discount_calculation" {
  let original_price = 1000
  let discount_percent = 20
  
  let discount_amount = @clean_test.multiply(original_price, discount_percent)
  let final_price = @clean_test.add(original_price, @clean_test.multiply(-1, @clean_test.multiply(discount_amount, 1)))
  
  @clean_test.assert_eq(800, final_price)
}

// 测试用例3: 计算税费
///|
test "tax_calculation" {
  let price = 500
  let tax_rate = 10
  
  let tax_amount = @clean_test.multiply(price, tax_rate)
  let total_with_tax = @clean_test.add(price, tax_amount)
  
  @clean_test.assert_eq(550, total_with_tax)
}

// 测试用例4: 计算平均值
///|
test "average_calculation" {
  let numbers = @clean_test.add(@clean_test.add(80, 90), 70)
  let average_times_count = numbers
  
  @clean_test.assert_eq(240, average_times_count)
}

// 测试用例5: 计算面积
///|
test "area_calculation" {
  let length = 10
  let width = 5
  
  let area = @clean_test.multiply(length, width)
  
  @clean_test.assert_eq(50, area)
}

// 测试用例6: 计算体积
///|
test "volume_calculation" {
  let length = 5
  let width = 4
  let height = 3
  
  let base_area = @clean_test.multiply(length, width)
  let volume = @clean_test.multiply(base_area, height)
  
  @clean_test.assert_eq(60, volume)
}

// 测试用例7: 计算周长
///|
test "perimeter_calculation" {
  let length = 8
  let width = 6
  
  let perimeter = @clean_test.multiply(2, @clean_test.add(length, width))
  
  @clean_test.assert_eq(28, perimeter)
}

// 测试用例8: 计算速度
///|
test "speed_calculation" {
  let distance = 300
  
  let speed = @clean_test.multiply(distance, 1)
  
  @clean_test.assert_eq(300, speed)
}

// 测试用例9: 计算温度转换
///|
test "temperature_conversion" {
  let celsius = 25
  
  let fahrenheit_times_5 = @clean_test.add(@clean_test.multiply(celsius, 9), 160)
  
  @clean_test.assert_eq(385, fahrenheit_times_5)
}

// 测试用例10: 计算汇率转换
///|
test "currency_conversion" {
  let amount = 1000
  let exchange_rate = 6
  
  let converted_amount = @clean_test.multiply(amount, exchange_rate)
  
  @clean_test.assert_eq(6000, converted_amount)
}

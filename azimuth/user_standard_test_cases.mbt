// 用户标准 MoonBit 测试用例
// 实际应用场景测试

test "financial_calculator_simple_interest" {
  // 金融计算器：单利计算测试
  let principal = 5000
  let rate = 8
  let time = 2
  
  // 计算单利：principal * rate * time / 100
  let interest = multiply(multiply(principal, rate), time)
  let total = add(principal, divide_with_ceil(interest, 100))
  
  assert_eq(5800, total)  // 5000 + (5000 * 8 * 2 / 100) = 5800
}

test "inventory_stock_management" {
  // 库存管理：库存计算测试
  let initial_stock = 200
  let sold = 45
  let restocked = 30
  let returned = 5
  
  // 计算最终库存
  let current_stock = add(initial_stock, -sold)
  let after_restock = add(current_stock, restocked)
  let final_stock = add(after_restock, returned)
  
  assert_eq(190, final_stock)
}

test "temperature_conversion_kelvin_to_celsius" {
  // 温度转换：开尔文到摄氏度
  let kelvin = 300
  
  // 摄氏度 = 开尔文 - 273
  let celsius = add(kelvin, -273)
  
  assert_eq(27, celsius)
}

test "batch_processing_items_per_batch" {
  // 批处理：每批项目数计算
  let total_items = 85
  let batch_size = 10
  
  // 计算需要的批次数（向上取整）
  let batches_needed = divide_with_ceil(total_items, batch_size)
  
  assert_eq(9, batches_needed)  // 85/10 = 8.5，需要9批
}

test "user_greeting_multiple_languages" {
  // 用户问候：多语言测试
  let english_name = "Alice"
  let chinese_name = "小明"
  let spanish_name = "Carlos"
  
  // 测试不同语言用户名的问候
  assert_eq_string("Hello, Alice!", greet(english_name))
  assert_eq_string("Hello, 小明!", greet(chinese_name))
  assert_eq_string("Hello, Carlos!", greet(spanish_name))
}

test "mathematical_arithmetic_sequence" {
  // 数学序列：等差数列测试
  let first_term = 3
  let difference = 4
  let n = 5
  
  // 计算第n项：first_term + (n-1) * difference
  let nth_term = add(first_term, multiply(add(n, -1), difference))
  
  assert_eq(19, nth_term)  // 3 + (5-1) * 4 = 19
}

test "extreme_value_handling" {
  // 极值处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值的安全运算
  let safe_add1 = add(max_val, -1)
  let safe_add2 = add(min_val, 1)
  let safe_mul1 = multiply(max_val, 1)
  let safe_mul2 = multiply(min_val, 1)
  
  assert_eq(2147483646, safe_add1)
  assert_eq(-2147483647, safe_add2)
  assert_eq(2147483647, safe_mul1)
  assert_eq(-2147483648, safe_mul2)
}

test "practical_division_scenarios" {
  // 实际除法场景测试
  // 场景1：分配物品给人员
  let items = 23
  let people = 5
  let items_per_person = divide_with_ceil(items, people)
  
  // 场景2：计算页数
  let words = 1050
  let words_per_page = 300
  let pages_needed = divide_with_ceil(words, words_per_page)
  
  assert_eq(5, items_per_person)   // 23/5 = 4.6，每人5个
  assert_eq(4, pages_needed)       // 1050/300 = 3.5，需要4页
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  let base_price = 100
  let quantity = 15
  let discount_rate = 10
  let tax_rate = 8
  
  // 计算总价
  let subtotal = multiply(base_price, quantity)
  let discount_amount = divide_with_ceil(multiply(subtotal, discount_rate), 100)
  let after_discount = add(subtotal, -discount_amount)
  let tax_amount = divide_with_ceil(multiply(after_discount, tax_rate), 100)
  let final_total = add(after_discount, tax_amount)
  
  assert_eq(1500, subtotal)        // 100 * 15 = 1500
  assert_eq(150, discount_amount)  // ceil(1500 * 10 / 100) = 150
  assert_eq(1350, after_discount)  // 1500 - 150 = 1350
  assert_eq(108, tax_amount)       // ceil(1350 * 8 / 100) = 108
  assert_eq(1458, final_total)     // 1350 + 108 = 1458
}

test "data_processing_average_calculation" {
  // 数据处理：平均值计算测试
  let scores = [85, 92, 78, 96, 89]
  let count = 5
  
  // 计算总分（手动累加）
  let total = add(add(add(add(85, 92), 78), 96), 89)
  
  // 计算平均分（向上取整）
  let average = divide_with_ceil(total, count)
  
  assert_eq(440, total)   // 85 + 92 + 78 + 96 + 89 = 440
  assert_eq(88, average)  // ceil(440 / 5) = 88
}
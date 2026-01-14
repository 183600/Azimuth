// Azimuth 项目增强测试用例
// 使用标准的 MoonBit 测试语法，包含各种实际应用场景

test "array_index_calculation" {
  // 数组索引计算测试：二维数组转一维数组的索引计算
  let row = 3
  let col = 4
  let width = 5
  
  // 计算二维数组在一维数组中的索引：index = row * width + col
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(19, index)
  
  // 测试边界情况
  let first_row = 0
  let first_col = 0
  let first_index = @azimuth.add(@azimuth.multiply(first_row, width), first_col)
  @azimuth.assert_eq(0, first_index)
  
  let last_row = 9
  let last_col = 4
  let last_index = @azimuth.add(@azimuth.multiply(last_row, width), last_col)
  @azimuth.assert_eq(49, last_index)
}

test "string_template_processing" {
  // 字符串模板处理测试：模拟简单的字符串模板功能
  let name = "Alice"
  let day = "Monday"
  
  // 模拟模板替换（简化版）
  let greeting = @azimuth.greet(name)
  let day_prefix = " Today is "
  let day_suffix = "."
  let full_message = greeting + day_prefix + day + day_suffix
  
  @azimuth.assert_eq_string("Hello, Alice! Today is Monday.", full_message)
  
  // 测试空值处理
  let empty_name = ""
  let empty_greeting = @azimuth.greet(empty_name)
  @azimuth.assert_eq_string("Hello, !", empty_greeting)
  
  // 测试特殊字符
  let special_name = "Bob@123"
  let special_greeting = @azimuth.greet(special_name)
  @azimuth.assert_eq_string("Hello, Bob@123!", special_greeting)
}

test "mathematical_formula_validation" {
  // 数学公式验证测试：验证基本的数学恒等式
  let a = 7
  let b = 3
  
  // 验证分配律：a × (b + c) = a × b + a × c
  let c = 5
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // 验证平方差公式：(a + b) × (a - b) = a² - b²
  let sum = @azimuth.add(a, b)
  let diff = @azimuth.add(a, -b)
  let product = @azimuth.multiply(sum, diff)
  let square_diff = @azimuth.add(@azimuth.multiply(a, a), @azimuth.multiply(-b, b))
  @azimuth.assert_eq(product, square_diff)
}

test "boundary_condition_combinations" {
  // 边界条件组合测试：测试多个边界条件的复杂组合
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 测试最大值与其他边界值的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, one))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, neg_one))
  
  // 测试最小值与其他边界值的运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, one))
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, neg_one))
  
  // 测试零的运算特性
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, max_val))
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(zero, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(zero, min_val))
  
  // 测试连续边界运算
  let chain_result = @azimuth.add(@azimuth.multiply(@azimuth.add(max_val, min_val), one), max_val)
  @azimuth.assert_eq(max_val, chain_result)
}

test "real_world_business_scenario" {
  // 实际业务场景测试：模拟电商订单计算
  let product_price = 299
  let quantity = 3
  let discount_threshold = 500
  let discount_rate = 10
  let shipping_fee = 20
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(product_price, quantity)
  
  // 计算折扣（如果超过阈值）
  let discount_amount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = @azimuth.add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价（含运费）
  let final_total = @azimuth.add(@azimuth.add(discounted_total, tax_amount), shipping_fee)
  
  @azimuth.assert_eq(1076, final_total)
  
  // 测试小额订单（无折扣）
  let small_price = 50
  let small_quantity = 2
  let small_subtotal = @azimuth.multiply(small_price, small_quantity)
  let small_final = @azimuth.add(@azimuth.add(small_subtotal, @azimuth.multiply(small_subtotal, tax_rate) / 100), shipping_fee)
  
  @azimuth.assert_eq(127, small_final)
}

test "performance_related_calculations" {
  // 性能相关测试：模拟大量数据的计算
  let base_value = 1000
  let iterations = 100
  
  // 模拟累加计算：base_value + base_value*2 + ... + base_value*iterations
  // 使用公式：base_value × (1 + 2 + ... + iterations) = base_value × iterations × (iterations + 1) / 2
  let sum_formula = @azimuth.multiply(base_value, @azimuth.multiply(iterations, @azimuth.add(iterations, 1)) / 2)
  
  // 验证前几项的计算
  let sum_first_5 = @azimuth.multiply(base_value, @azimuth.multiply(5, 6) / 2)
  @azimuth.assert_eq(15000, sum_first_5)
  
  // 验证总和
  @azimuth.assert_eq(5050000, sum_formula)
  
  // 模拟指数增长计算
  let growth_factor = 2
  let periods = 10
  let initial_amount = 100
  
  // 计算：initial_amount × growth_factor^periods
  let exponential_growth = @azimuth.multiply(initial_amount, @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), @azimuth.multiply(2, 2)))
  @azimuth.assert_eq(102400, exponential_growth)
}

test "error_handling_scenarios" {
  // 错误处理测试：测试各种错误情况的处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  let overflow_add = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, overflow_add)
  
  let underflow_add = @azimuth.add(min_val, min_val)
  @azimuth.assert_eq(min_val, underflow_add)
  
  let overflow_mul = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, overflow_mul)
  
  let underflow_mul = @azimuth.multiply(min_val, 2)
  @azimuth.assert_eq(min_val, underflow_mul)
  
  // 测试边界条件的特殊处理
  let special_case_1 = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, special_case_1)
  
  let special_case_2 = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, special_case_2)
  
  // 测试断言函数的容错性
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
  @azimuth.assert_false(@azimuth.add(5, 3) == 9)
  @azimuth.assert_eq(8, @azimuth.add(5, 3))
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
}

test "data_type_conversion_simulation" {
  // 数据类型转换测试：模拟不同数据类型之间的转换
  // 模拟整数到字符串的转换（通过greet函数）
  let number_as_string = "2023"
  let converted_greeting = @azimuth.greet(number_as_string)
  
  @azimuth.assert_eq_string("Hello, 2023!", converted_greeting)
  
  // 模拟布尔值到字符串的转换
  let true_as_string = "true"
  let false_as_string = "false"
  
  @azimuth.assert_eq_string("Hello, true!", @azimuth.greet(true_as_string))
  @azimuth.assert_eq_string("Hello, false!", @azimuth.greet(false_as_string))
  
  // 模拟浮点数的简化表示（使用整数表示小数部分）
  let pi_approx_string = "3.14"
  
  @azimuth.assert_eq_string("Hello, 3.14!", @azimuth.greet(pi_approx_string))
  
  // 模拟日期时间字符串
  let date_string = "2023-12-25"
  let time_string = "14:30:00"
  let datetime_string = "2023-12-25T14:30:00"
  
  @azimuth.assert_eq_string("Hello, 2023-12-25!", @azimuth.greet(date_string))
  @azimuth.assert_eq_string("Hello, 14:30:00!", @azimuth.greet(time_string))
  @azimuth.assert_eq_string("Hello, 2023-12-25T14:30:00!", @azimuth.greet(datetime_string))
}

test "bitwise_operations_simulation" {
  // 模拟位运算：使用加减乘除实现位运算效果
  let a = 8
  let b = 3
  
  // 左移运算模拟：a << 1 = a * 2
  let left_shift = @azimuth.multiply(a, 2)
  @azimuth.assert_eq(16, left_shift)
  
  // 右移运算模拟：a >> 1 = a / 2
  let right_shift = a / 2
  @azimuth.assert_eq(4, right_shift)
  
  // 位或运算模拟（简化）：a | b = a + b - (a & b)
  // 这里我们简化为 a + b
  let bit_or_sim = @azimuth.add(a, b)
  @azimuth.assert_eq(11, bit_or_sim)
  
  // 测试连续位移操作
  let double_left_shift = @azimuth.multiply(a, 4)  // a << 2
  @azimuth.assert_eq(32, double_left_shift)
  
  let double_right_shift = a / 4  // a >> 2
  @azimuth.assert_eq(2, double_right_shift)
}

test "currency_conversion_practical" {
  // 实际货币转换场景
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY (简化汇率)
  
  // 转换为人民币
  let cny_amount = @azimuth.multiply(usd_amount, exchange_rate)
  @azimuth.assert_eq(700, cny_amount)
  
  // 添加手续费 5%
  let fee = @azimuth.multiply(cny_amount, 5) / 100
  let total_with_fee = @azimuth.add(cny_amount, fee)
  @azimuth.assert_eq(735, total_with_fee)
  
  // 测试多种货币转换
  let eur_amount = 100
  let eur_to_usd = 1.1  // 简化为整数 11/10
  let eur_in_usd = @azimuth.multiply(eur_amount, 11) / 10
  let eur_in_cny = @azimuth.multiply(eur_in_usd, exchange_rate)
  @azimuth.assert_eq(770, eur_in_cny)
  
  // 测试大额货币转换
  let large_usd = 10000
  let large_cny = @azimuth.multiply(large_usd, exchange_rate)
  let large_fee = @azimuth.multiply(large_cny, 3) / 100  // 3% 手续费
  let large_total = @azimuth.add(large_cny, large_fee)
  @azimuth.assert_eq(72100, large_total)
}
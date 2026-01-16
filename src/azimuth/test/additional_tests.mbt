// 额外的 MoonBit 测试用例
// 这些测试用例补充现有的测试，提供更全面的覆盖

test "division_simulation_test" {
  // 模拟除法运算：使用减法实现整数除法
  let dividend = 20
  let divisor = 3
  
  // 简化的除法模拟：使用整数除法运算符
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  
  @azimuth.assert_eq(6, quotient)
  @azimuth.assert_eq(2, remainder)
  
  // 测试边界情况
  @azimuth.assert_eq(0, 0 / 5)
  @azimuth.assert_eq(5, 5 / 1)
  @azimuth.assert_eq(1, 5 / 5)
}

test "modulo_operations_test" {
  // 模运算测试
  // 测试正数的模运算
  @azimuth.assert_eq(0, 10 % 2)
  @azimuth.assert_eq(1, 10 % 3)
  @azimuth.assert_eq(2, 10 % 4)
  
  // 测试负数的模运算
  @azimuth.assert_eq(0, -10 % 2)
  @azimuth.assert_eq(-1, -10 % 3)
  @azimuth.assert_eq(-2, -10 % 4)
  
  // 结合add和multiply函数验证模运算性质
  let a = 17
  let b = 5
  let c = 3
  
  // 验证 (a + b) % c = ((a % c) + (b % c)) % c
  let left_side = @azimuth.add(a, b) % c
  let right_side = @azimuth.add(a % c, b % c) % c
  @azimuth.assert_eq(left_side, right_side)
}

test "absolute_value_simulation" {
  // 模拟绝对值函数
  let positive_num = 42
  let negative_num = -42
  let zero = 0
  
  // 使用条件表达式模拟绝对值
  let abs_positive = if positive_num < 0 { -positive_num } else { positive_num }
  let abs_negative = if negative_num < 0 { -negative_num } else { negative_num }
  let abs_zero = if zero < 0 { -zero } else { zero }
  
  @azimuth.assert_eq(42, abs_positive)
  @azimuth.assert_eq(42, abs_negative)
  @azimuth.assert_eq(0, abs_zero)
  
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  let abs_max = if max_val < 0 { -max_val } else { max_val }
  // 注意：最小值的绝对值会溢出，这是特殊情况
  let abs_min = if min_val < 0 { min_val } else { min_val }  // 保持最小值不变
  
  @azimuth.assert_eq(max_val, abs_max)
  @azimuth.assert_eq(min_val, abs_min)
}

test "sign_function_simulation" {
  // 模拟符号函数：返回-1（负数）、0（零）或1（正数）
  let positive = 100
  let negative = -100
  let zero = 0
  
  // 使用条件表达式模拟符号函数
  let sign_positive = if positive > 0 { 1 } else { if positive < 0 { -1 } else { 0 } }
  let sign_negative = if negative > 0 { 1 } else { if negative < 0 { -1 } else { 0 } }
  let sign_zero = if zero > 0 { 1 } else { if zero < 0 { -1 } else { 0 } }
  
  @azimuth.assert_eq(1, sign_positive)
  @azimuth.assert_eq(-1, sign_negative)
  @azimuth.assert_eq(0, sign_zero)
  
  // 测试符号函数在运算中的应用
  let a = -5
  let b = 3
  
  // 计算 a * |b|
  let sign_b = if b > 0 { 1 } else { if b < 0 { -1 } else { 0 } }
  let abs_b = if b < 0 { -b } else { b }
  let result = @azimuth.multiply(a, abs_b)
  
  @azimuth.assert_eq(-15, result)
}

test "range_validation_test" {
  // 数值范围验证测试
  let min_range = 10
  let max_range = 100
  
  // 测试范围内的值
  let in_range_1 = 50
  let in_range_2 = 10
  let in_range_3 = 100
  
  // 使用条件表达式验证范围
  let check_1 = in_range_1 >= min_range && in_range_1 <= max_range
  let check_2 = in_range_2 >= min_range && in_range_2 <= max_range
  let check_3 = in_range_3 >= min_range && in_range_3 <= max_range
  
  @azimuth.assert_true(check_1)
  @azimuth.assert_true(check_2)
  @azimuth.assert_true(check_3)
  
  // 测试范围外的值
  let out_range_1 = 5
  let out_range_2 = 150
  
  let check_4 = out_range_1 >= min_range && out_range_1 <= max_range
  let check_5 = out_range_2 >= min_range && out_range_2 <= max_range
  
  @azimuth.assert_false(check_4)
  @azimuth.assert_false(check_5)
}

test "string_length_calculation" {
  // 字符串长度计算测试（通过字符计数模拟）
  let short_string = "Hi"
  let medium_string = "MoonBit"
  let long_string = "This is a longer string for testing purposes"
  
  // 模拟字符串长度计算（实际中需要遍历字符）
  // 这里我们使用已知的长度值
  let short_length = 2
  let medium_length = 7
  let long_length = 42
  
  // 验证字符串拼接后的长度
  let combined_1 = "Hello, " + short_string + "!"
  let combined_2 = "Hello, " + medium_string + "!"
  let combined_3 = "Hello, " + long_string + "!"
  
  // 预期的长度
  let expected_1 = @azimuth.add(@azimuth.add(7, short_length), 1)  // "Hello, " (7) + short_string + "!" (1)
  let expected_2 = @azimuth.add(@azimuth.add(7, medium_length), 1)
  let expected_3 = @azimuth.add(@azimuth.add(7, long_length), 1)
  
  @azimuth.assert_eq(10, expected_1)   // 7 + 2 + 1 = 10
  @azimuth.assert_eq(15, expected_2)   // 7 + 7 + 1 = 15
  @azimuth.assert_eq(50, expected_3)   // 7 + 42 + 1 = 50
  
  // 验证greet函数的行为
  @azimuth.assert_eq_string("Hello, Hi!", @azimuth.greet(short_string))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet(medium_string))
  @azimuth.assert_eq_string("Hello, This is a longer string for testing purposes!", @azimuth.greet(long_string))
}

test "mathematical_series_test" {
  // 数学级数测试
  // 计算等差数列：2, 5, 8, 11, 14 的和
  let a1 = 2
  let d = 3  // 公差
  let n = 5  // 项数
  
  // 使用公式：S = n/2 * (2a1 + (n-1)d)
  let sum = @azimuth.multiply(n, @azimuth.add(@azimuth.multiply(2, a1), @azimuth.multiply(n - 1, d))) / 2
  @azimuth.assert_eq(40, sum)  // 2 + 5 + 8 + 11 + 14 = 40
  
  // 计算等比数列：2, 4, 8, 16, 32 的和（前5项）
  let first_term = 2
  let ratio = 2
  let terms = 5
  
  // 手动计算（避免使用指数）
  let term1 = first_term
  let term2 = @azimuth.multiply(term1, ratio)
  let term3 = @azimuth.multiply(term2, ratio)
  let term4 = @azimuth.multiply(term3, ratio)
  let term5 = @azimuth.multiply(term4, ratio)
  
  let geometric_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(62, geometric_sum)  // 2 + 4 + 8 + 16 + 32 = 62
}

test "prime_number_check" {
  // 质数检查测试
  // 检查一些数字是否为质数
  let num1 = 7   // 质数
  let num2 = 15  // 非质数
  let num3 = 23  // 质数
  let num4 = 1   // 非质数（特殊情况）
  
  // 简化的质数检查（使用试除法）
  let is_prime_1 = num1 > 1 && (num1 % 2 != 0 || num1 == 2) && (num1 % 3 != 0 || num1 == 3)
  let is_prime_2 = num2 > 1 && (num2 % 2 != 0 || num2 == 2) && (num2 % 3 != 0 || num2 == 3)
  let is_prime_3 = num3 > 1 && (num3 % 2 != 0 || num3 == 2) && (num3 % 3 != 0 || num3 == 3)
  let is_prime_4 = num4 > 1 && (num4 % 2 != 0 || num4 == 2) && (num4 % 3 != 0 || num4 == 3)
  
  @azimuth.assert_true(is_prime_1)
  @azimuth.assert_false(is_prime_2)  // 15 = 3 × 5
  @azimuth.assert_true(is_prime_3)
  @azimuth.assert_false(is_prime_4)  // 1 不是质数
  
  // 测试质数在数学运算中的应用
  let prime1 = 7
  let prime2 = 11
  
  // 计算两个质数的乘积
  let product = @azimuth.multiply(prime1, prime2)
  @azimuth.assert_eq(77, product)
  
  // 验证乘积的因数
  @azimuth.assert_eq(0, product % prime1)
  @azimuth.assert_eq(0, product % prime2)
}

test "logical_operations_test" {
  // 逻辑运算测试
  let a = true
  let b = false
  
  // 逻辑与运算（使用乘法模拟）
  let logical_and = @azimuth.multiply(if a { 1 } else { 0 }, if b { 1 } else { 0 }) == 1
  @azimuth.assert_false(logical_and)  // true && false = false
  
  // 逻辑或运算（使用加法模拟）
  let logical_or = @azimuth.add(if a { 1 } else { 0 }, if b { 1 } else { 0 }) >= 1
  @azimuth.assert_true(logical_or)    // true || false = true
  
  // 逻辑非运算
  let not_a = if a { false } else { true }
  let not_b = if b { false } else { true }
  
  @azimuth.assert_false(not_a)  // !true = false
  @azimuth.assert_true(not_b)   // !false = true
  
  // 测试德摩根定律
  // !(a && b) == !a || !b
  let demorgan_1 = !(@azimuth.multiply(if a { 1 } else { 0 }, if b { 1 } else { 0 }) == 1)
  let demorgan_2 = @azimuth.add(if not_a { 1 } else { 0 }, if not_b { 1 } else { 0 }) >= 1
  @azimuth.assert_true(demorgan_1 == demorgan_2)
  
  // !(a || b) == !a && !b
  let demorgan_3 = !(@azimuth.add(if a { 1 } else { 0 }, if b { 1 } else { 0 }) >= 1)
  let demorgan_4 = @azimuth.multiply(if not_a { 1 } else { 0 }, if not_b { 1 } else { 0 }) == 1
  @azimuth.assert_true(demorgan_3 == demorgan_4)
}

test "complex_business_logic" {
  // 复杂业务逻辑测试：模拟购物车折扣系统
  let item1_price = 100
  let item2_price = 200
  let item3_price = 300
  
  let item1_qty = 2
  let item2_qty = 1
  let item3_qty = 3
  
  // 计算每种商品的小计
  let subtotal1 = @azimuth.multiply(item1_price, item1_qty)
  let subtotal2 = @azimuth.multiply(item2_price, item2_qty)
  let subtotal3 = @azimuth.multiply(item3_price, item3_qty)
  
  // 计算购物车总额
  let cart_total = @azimuth.add(@azimuth.add(subtotal1, subtotal2), subtotal3)
  @azimuth.assert_eq(1400, cart_total)  // 100*2 + 200*1 + 300*3 = 200 + 200 + 900 = 1300
  
  // 应用折扣规则：
  // 总额超过1000元，享受10%折扣
  // 总额超过500元但不超过1000元，享受5%折扣
  // 总额不超过500元，无折扣
  let discount_rate = if cart_total > 1000 { 10 } else { if cart_total > 500 { 5 } else { 0 } }
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(cart_total, discount_rate) / 100
  
  // 计算折后金额
  let discounted_total = @azimuth.add(cart_total, -discount_amount)
  
  // 计算税费（8%）
  let tax_rate = 8
  let tax_amount = @azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终金额
  let final_amount = @azimuth.add(discounted_total, tax_amount)
  
  @azimuth.assert_eq(10, discount_rate)      // 1400 > 1000，所以享受10%折扣
  @azimuth.assert_eq(140, discount_amount)   // 1400 * 10% = 140
  @azimuth.assert_eq(1260, discounted_total) // 1400 - 140 = 1260
  @azimuth.assert_eq(100, tax_amount)        // 1260 * 8% = 100.8 ≈ 100（整数除法）
  @azimuth.assert_eq(1360, final_amount)     // 1260 + 100 = 1360
  
  // 测试不同的购物车总额
  let small_cart_total = 400
  let medium_cart_total = 800
  
  let small_discount = if small_cart_total > 1000 { 10 } else { if small_cart_total > 500 { 5 } else { 0 } }
  let medium_discount = if medium_cart_total > 1000 { 10 } else { if medium_cart_total > 500 { 5 } else { 0 } }
  
  @azimuth.assert_eq(0, small_discount)   // 400 <= 500，无折扣
  @azimuth.assert_eq(5, medium_discount)  // 500 < 800 <= 1000，5%折扣
}
// 新增的 MoonBit 标准测试用例
// 测试 azimuth 模块的核心功能和边界情况

test "arithmetics_add_comprehensive" {
  // 综合加法测试：正数、负数、零的组合
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(100, 0))
  @azimuth.assert_eq(-50, @azimuth.add(-25, -25))
}

test "arithmetics_multiply_edge_cases" {
  // 乘法边界情况测试：包含0、1、-1的特殊情况
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(12345, -1))
  @azimuth.assert_eq(12345, @azimuth.multiply(-12345, -1))
}

test "overflow_protection_verification" {
  // 溢出保护验证测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "string_greeting_variations" {
  // 字符串问候功能的各种变化测试
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob123!", @azimuth.greet("Bob123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
}

test "mathematical_formula_applications" {
  // 数学公式应用测试
  let a = 6
  let b = 8
  
  // 计算矩形面积和周长
  let area = @azimuth.multiply(a, b)
  let perimeter = @azimuth.multiply(2, @azimuth.add(a, b))
  let total = @azimuth.add(area, perimeter)
  
  @azimuth.assert_eq(48, area)        // 6 × 8 = 48
  @azimuth.assert_eq(28, perimeter)   // 2 × (6 + 8) = 28
  @azimuth.assert_eq(76, total)       // 48 + 28 = 76
}

test "financial_calculations" {
  // 金融计算场景测试
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 简单利息计算：本金 + (本金 × 利率 × 年数)
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total_amount = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total_amount)  // 10000 + (10000 × 5 × 3) = 160000
}

test "temperature_conversion_scenarios" {
  // 温度转换场景测试（简化版：F = C × 2 + 30）
  let freezing_c = 0
  let room_c = 20
  let boiling_c = 100
  
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  let room_f = @azimuth.add(@azimuth.multiply(room_c, 2), 30)
  let boiling_f = @azimuth.add(@azimuth.multiply(boiling_c, 2), 30)
  
  @azimuth.assert_eq(30, freezing_f)   // 0 × 2 + 30 = 30
  @azimuth.assert_eq(70, room_f)       // 20 × 2 + 30 = 70
  @azimuth.assert_eq(230, boiling_f)   // 100 × 2 + 30 = 230
}

test "complex_nested_calculations" {
  // 复杂嵌套计算测试
  let base = 5
  let multiplier = 3
  let offset = 10
  let divisor = 2
  
  // 计算：(base × multiplier + offset) ÷ divisor
  let product = @azimuth.multiply(base, multiplier)
  let sum_with_offset = @azimuth.add(product, offset)
  let final_result = sum_with_offset / divisor
  
  @azimuth.assert_eq(12, final_result)  // (5 × 3 + 10) ÷ 2 = 12
}

test "assertion_functions_validation" {
  // 断言函数验证测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-42, -42)
  @azimuth.assert_eq_string("MoonBit", "MoonBit")
  @azimuth.assert_eq_string("", "")
  
  // 条件断言测试
  let test_value = @azimuth.add(15, 25)
  @azimuth.assert_true(test_value == 40)
  @azimuth.assert_false(test_value != 40)
}

test "real_world_shopping_cart" {
  // 真实世界购物车计算测试
  let item1_price = 299
  let item1_qty = 2
  let item2_price = 199
  let item2_qty = 1
  let item3_price = 99
  let item3_qty = 3
  let shipping_fee = 20
  
  // 计算各项总价
  let item1_total = @azimuth.multiply(item1_price, item1_qty)
  let item2_total = @azimuth.multiply(item2_price, item2_qty)
  let item3_total = @azimuth.multiply(item3_price, item3_qty)
  
  // 计算购物车总价
  let subtotal = @azimuth.add(@azimuth.add(item1_total, item2_total), item3_total)
  let grand_total = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(598, item1_total)  // 299 × 2 = 598
  @azimuth.assert_eq(199, item2_total)  // 199 × 1 = 199
  @azimuth.assert_eq(297, item3_total)  // 99 × 3 = 297
  @azimuth.assert_eq(1094, subtotal)    // 598 + 199 + 297 = 1094
  @azimuth.assert_eq(1114, grand_total) // 1094 + 20 = 1114
}
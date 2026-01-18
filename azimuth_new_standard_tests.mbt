// Azimuth 项目新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，提供10个多样化的测试场景

// 导入必要的模块
use azimuth

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  let a = 15
  let b = 7
  
  // 测试加法
  let sum_result = azimuth.add(a, b)
  @assertion.assert_eq(22, sum_result)?
  
  // 测试乘法
  let mul_result = azimuth.multiply(a, b)
  @assertion.assert_eq(105, mul_result)?
  
  // 测试组合运算
  let combined = azimuth.add(azimuth.multiply(a, 2), azimuth.multiply(b, 3))
  @assertion.assert_eq(51, combined)?
}

test "negative_number_operations" {
  // 负数运算测试
  let positive = 20
  let negative = -8
  
  // 正负数相加
  let pos_neg_add = azimuth.add(positive, negative)
  @assertion.assert_eq(12, pos_neg_add)?
  
  // 负负数相加
  let neg_neg_add = azimuth.add(negative, negative)
  @assertion.assert_eq(-16, neg_neg_add)?
  
  // 正负数相乘
  let pos_neg_mul = azimuth.multiply(positive, negative)
  @assertion.assert_eq(-160, pos_neg_mul)?
  
  // 负负数相乘
  let neg_neg_mul = azimuth.multiply(negative, negative)
  @assertion.assert_eq(64, neg_neg_mul)?
}

test "zero_value_operations" {
  // 零值运算测试
  let positive = 42
  let negative = -17
  let zero = 0
  
  // 与零相加
  @assertion.assert_eq(positive, azimuth.add(positive, zero))?
  @assertion.assert_eq(negative, azimuth.add(negative, zero))?
  @assertion.assert_eq(zero, azimuth.add(zero, zero))?
  
  // 与零相乘
  @assertion.assert_eq(zero, azimuth.multiply(positive, zero))?
  @assertion.assert_eq(zero, azimuth.multiply(negative, zero))?
  @assertion.assert_eq(zero, azimuth.multiply(zero, zero))?
}

test "boundary_value_operations" {
  // 边界值运算测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值相加
  let max_min_add = azimuth.add(max_val, min_val)
  @assertion.assert_eq(-1, max_min_add)?
  
  // 边界值与普通数相加
  let max_normal_add = azimuth.add(max_val, 100)
  @assertion.assert_eq(max_val, max_normal_add)?
  
  let min_normal_add = azimuth.add(min_val, -100)
  @assertion.assert_eq(min_val, min_normal_add)?
  
  // 边界值相乘
  let max_zero_mul = azimuth.multiply(max_val, 0)
  @assertion.assert_eq(0, max_zero_mul)?
  
  let min_one_mul = azimuth.multiply(min_val, 1)
  @assertion.assert_eq(min_val, min_one_mul)?
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 空字符串
  let empty_result = azimuth.greet("")
  @assertion.assert_eq_string("Hello, !", empty_result)?
  
  // 单个字符
  let single_char = "A"
  let single_result = azimuth.greet(single_char)
  @assertion.assert_eq_string("Hello, A!", single_result)?
  
  // 长字符串
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  let long_result = azimuth.greet(long_string)
  @assertion.assert_eq_string("Hello, ThisIsAVeryLongStringForTestingPurposes!", long_result)?
  
  // 包含特殊字符的字符串
  let special_chars = "test@example.com#123"
  let special_result = azimuth.greet(special_chars)
  @assertion.assert_eq_string("Hello, test@example.com#123!", special_result)?
}

test "mathematical_formulas" {
  // 数学公式测试
  // 矩形面积计算：长 × 宽
  let length = 12
  let width = 8
  let area = azimuth.multiply(length, width)
  @assertion.assert_eq(96, area)?
  
  // 矩形周长计算：2 × (长 + 宽)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  @assertion.assert_eq(40, perimeter)?
  
  // 简单利息计算：本金 × 利率 × 时间
  let principal = 1000
  let rate = 5
  let time = 3
  let interest = azimuth.multiply(principal, azimuth.multiply(rate, time))
  @assertion.assert_eq(15000, interest)?
}

test "sequential_operations" {
  // 连续运算测试
  // 连续加法：(1 + 2) + (3 + 4) + (5 + 6)
  let step1 = azimuth.add(1, 2)  // 3
  let step2 = azimuth.add(3, 4)  // 7
  let step3 = azimuth.add(5, 6)  // 11
  let final_sum = azimuth.add(azimuth.add(step1, step2), step3)
  @assertion.assert_eq(21, final_sum)?
  
  // 连续乘法：2 × 3 × 4 × 5
  let mul_step1 = azimuth.multiply(2, 3)  // 6
  let mul_step2 = azimuth.multiply(4, 5)  // 20
  let final_product = azimuth.multiply(mul_step1, mul_step2)
  @assertion.assert_eq(120, final_product)?
  
  // 混合运算：(10 + 20) × (30 - 15) + 5
  let mixed_step1 = azimuth.add(10, 20)  // 30
  let mixed_step2 = azimuth.add(30, -15)  // 15
  let mixed_step3 = azimuth.multiply(mixed_step1, mixed_step2)  // 450
  let mixed_result = azimuth.add(mixed_step3, 5)  // 455
  @assertion.assert_eq(455, mixed_result)?
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 购物车计算：商品价格 × 数量
  let item_price = 199
  let item_quantity = 3
  let item_total = azimuth.multiply(item_price, item_quantity)
  
  let item2_price = 99
  let item2_quantity = 2
  let item2_total = azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = azimuth.add(item_total, item2_total)
  @assertion.assert_eq(795, cart_total)?
  
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  @assertion.assert_eq(80, fahrenheit)?
}

test "overflow_protection" {
  // 溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  let add_overflow1 = azimuth.add(max_val, 1)
  @assertion.assert_eq(max_val, add_overflow1)?
  
  let add_overflow2 = azimuth.add(max_val, max_val)
  @assertion.assert_eq(max_val, add_overflow2)?
  
  let add_underflow1 = azimuth.add(min_val, -1)
  @assertion.assert_eq(min_val, add_underflow1)?
  
  let add_underflow2 = azimuth.add(min_val, min_val)
  @assertion.assert_eq(min_val, add_underflow2)?
  
  // 乘法溢出保护
  let mul_overflow1 = azimuth.multiply(max_val, 2)
  @assertion.assert_eq(max_val, mul_overflow1)?
  
  let mul_underflow1 = azimuth.multiply(min_val, -1)
  @assertion.assert_eq(min_val, mul_underflow1)?
  
  let mul_underflow2 = azimuth.multiply(min_val, 2)
  @assertion.assert_eq(min_val, mul_underflow2)?
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 平均值计算：(a + b + c + d) / 4
  let a = 85
  let b = 90
  let c = 78
  let d = 92
  
  let sum = azimuth.add(azimuth.add(azimuth.add(a, b), c), d)
  let average = sum / 4
  @assertion.assert_eq(86, average)?
  
  // 百分比计算：部分 / 总数 × 100
  let part = 25
  let total = 200
  let percentage = azimuth.multiply(part, 100) / total
  @assertion.assert_eq(12, percentage)?
  
  // 折扣计算：原价 - (原价 × 折扣率)
  let original_price = 500
  let discount_rate = 20  // 20%
  let discount_amount = azimuth.multiply(original_price, discount_rate) / 100
  let discounted_price = azimuth.add(original_price, -discount_amount)
  @assertion.assert_eq(400, discounted_price)?
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  // 验证断言函数
  @assertion.assert_true(true)?
  @assertion.assert_false(false)?
  @assertion.assert_eq(42, 42)?
  @assertion.assert_eq_string("test", "test")?
  
  // 验证数学性质
  let x = 15
  let y = 25
  
  // 加法交换律
  let commutative_add1 = azimuth.add(x, y)
  let commutative_add2 = azimuth.add(y, x)
  @assertion.assert_eq(commutative_add1, commutative_add2)?
  
  // 乘法交换律
  let commutative_mul1 = azimuth.multiply(x, y)
  let commutative_mul2 = azimuth.multiply(y, x)
  @assertion.assert_eq(commutative_mul1, commutative_mul2)?
  
  // 验证边界条件处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @assertion.assert_true(azimuth.add(max_val, 1) == max_val)?
  @assertion.assert_true(azimuth.add(min_val, -1) == min_val)?
  @assertion.assert_true(azimuth.multiply(max_val, 2) == max_val)?
  @assertion.assert_true(azimuth.multiply(min_val, -1) == min_val)?
}
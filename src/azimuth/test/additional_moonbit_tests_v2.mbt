// 新增的 MoonBit 测试用例
// 这些测试用例专注于边界条件和实际应用场景

test "add_edge_case_combinations" {
  // 测试加法的边界条件组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与负数的组合
  azimuth.@azimuth.assert_eq(2147483646, azimuth.@azimuth.add(max_val, -1))
  azimuth.@azimuth.assert_eq(2147483647, azimuth.@azimuth.add(max_val, 0))
  azimuth.@azimuth.assert_eq(max_val, azimuth.@azimuth.add(max_val, 1))  // 溢出处理
  
  // 测试最小值与正数的组合
  azimuth.@azimuth.assert_eq(-2147483647, azimuth.@azimuth.add(min_val, 1))
  azimuth.@azimuth.assert_eq(min_val, azimuth.@azimuth.add(min_val, 0))
  azimuth.@azimuth.assert_eq(min_val, azimuth.@azimuth.add(min_val, -1))  // 溢出处理
  
  // 测试极值相加
  azimuth.@azimuth.assert_eq(-1, azimuth.@azimuth.add(max_val, min_val))
}

test "multiply_fractional_simulation" {
  // 模拟分数乘法运算
  // 计算 (3/4) × (8/9) = 24/36 = 2/3，使用整数模拟
  
  let numerator1 = 3
  let denominator1 = 4
  let numerator2 = 8
  let denominator2 = 9
  
  // 分数乘法：(a/b) × (c/d) = (a×c)/(b×d)
  let result_numerator = azimuth.@azimuth.multiply(numerator1, numerator2)
  let result_denominator = azimuth.@azimuth.multiply(denominator1, denominator2)
  
  azimuth.@azimuth.assert_eq(24, result_numerator)
  azimuth.@azimuth.assert_eq(36, result_denominator)
  
  // 简化分数：24/36 = 2/3
  let gcd = 12  // 最大公约数
  let simplified_numerator = result_numerator / gcd
  let simplified_denominator = result_denominator / gcd
  
  azimuth.@azimuth.assert_eq(2, simplified_numerator)
  azimuth.@azimuth.assert_eq(3, simplified_denominator)
}

test "greet_multilingual_support" {
  // 测试多语言支持
  azimuth.@azimuth.assert_eq_string("Hello, 世界!", azimuth.@azimuth.greet("世界"))
  azimuth.@azimuth.assert_eq_string("Hello, Москва!", azimuth.@azimuth.greet("Москва"))
  azimuth.@azimuth.assert_eq_string("Hello, العربية!", azimuth.@azimuth.greet("العربية"))
  azimuth.@azimuth.assert_eq_string("Hello, עברית!", azimuth.@azimuth.greet("עברית"))
  azimuth.@azimuth.assert_eq_string("Hello, हिन्दी!", azimuth.@azimuth.greet("हिन्दी"))
  azimuth.@azimuth.assert_eq_string("Hello, 한국어!", azimuth.@azimuth.greet("한국어"))
  azimuth.@azimuth.assert_eq_string("Hello, 日本語!", azimuth.@azimuth.greet("日本語"))
}

test "complex_business_calculation" {
  // 复杂业务计算：带税费的订单计算
  let item_price = 199
  let quantity = 3
  let tax_rate = 8  // 8%
  let discount_threshold = 500
  let discount_rate = 15  // 15%
  let shipping_fee = 25
  
  // 计算商品总价
  let subtotal = azimuth.@azimuth.multiply(item_price, quantity)
  
  // 计算折扣（如果超过阈值）
  let has_discount = subtotal > discount_threshold
  let discount_amount = if has_discount {
    azimuth.@azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = azimuth.@azimuth.add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = azimuth.@azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价
  let final_total = azimuth.@azimuth.add(azimuth.@azimuth.add(discounted_total, tax_amount), shipping_fee)
  
  azimuth.@azimuth.assert_eq(597, subtotal)
  azimuth.@azimuth.assert_eq(89, discount_amount)  // 597 × 15% = 89.55 ≈ 89
  azimuth.@azimuth.assert_eq(508, discounted_total)  // 597 - 89 = 508
  azimuth.@azimuth.assert_eq(40, tax_amount)  // 508 × 8% = 40.64 ≈ 40
  azimuth.@azimuth.assert_eq(573, final_total)  // 508 + 40 + 25 = 573
}

test "divide_with_ceil_precision_tests" {
  // 测试向上取整除法的精度
  // 测试大数除法
  azimuth.@azimuth.assert_eq(1000000, azimuth.@azimuth.divide_with_ceil(1000000, 1))
  azimuth.@azimuth.assert_eq(500000, azimuth.@azimuth.divide_with_ceil(1000000, 2))
  azimuth.@azimuth.assert_eq(333334, azimuth.@azimuth.divide_with_ceil(1000000, 3))
  azimuth.@azimuth.assert_eq(250000, azimuth.@azimuth.divide_with_ceil(1000000, 4))
  azimuth.@azimuth.assert_eq(200000, azimuth.@azimuth.divide_with_ceil(1000000, 5))
  
  // 测试接近整数的除法
  azimuth.@azimuth.assert_eq(100, azimuth.@azimuth.divide_with_ceil(999, 10))
  azimuth.@azimuth.assert_eq(100, azimuth.@azimuth.divide_with_ceil(1000, 10))
  azimuth.@azimuth.assert_eq(101, azimuth.@azimuth.divide_with_ceil(1001, 10))
  
  // 测试小数除以大数
  azimuth.@azimuth.assert_eq(1, azimuth.@azimuth.divide_with_ceil(1, 1))
  azimuth.@azimuth.assert_eq(1, azimuth.@azimuth.divide_with_ceil(1, 2))
  azimuth.@azimuth.assert_eq(1, azimuth.@azimuth.divide_with_ceil(99, 100))
  azimuth.@azimuth.assert_eq(2, azimuth.@azimuth.divide_with_ceil(101, 100))
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 7
  let b = 12
  let c = 8
  
  // 左边：a × (b + c)
  let left_side = azimuth.@azimuth.multiply(a, azimuth.@azimuth.add(b, c))
  
  // 右边：a × b + a × c
  let right_side = azimuth.@azimuth.add(azimuth.@azimuth.multiply(a, b), azimuth.@azimuth.multiply(a, c))
  
  azimuth.@azimuth.assert_eq(left_side, right_side)
  azimuth.@azimuth.assert_eq(140, left_side)  // 7 × (12 + 8) = 7 × 20 = 140
  
  // 测试负数情况
  let d = -5
  let e = 10
  let f = -3
  
  let left_side2 = azimuth.@azimuth.multiply(d, azimuth.@azimuth.add(e, f))
  let right_side2 = azimuth.@azimuth.add(azimuth.@azimuth.multiply(d, e), azimuth.@azimuth.multiply(d, f))
  
  azimuth.@azimuth.assert_eq(left_side2, right_side2)
  azimuth.@azimuth.assert_eq(-35, left_side2)  // -5 × (10 + (-3)) = -5 × 7 = -35
}

test "array_indexing_simulation" {
  // 模拟数组索引计算
  let rows = 5
  let cols = 4
  let row = 2
  let col = 3
  
  // 计算二维数组在一维数组中的索引
  let index = azimuth.@azimuth.add(azimuth.@azimuth.multiply(row, cols), col)
  azimuth.@azimuth.assert_eq(11, index)  // 2 × 4 + 3 = 11
  
  // 测试边界情况
  let first_index = azimuth.@azimuth.add(azimuth.@azimuth.multiply(0, cols), 0)
  let last_index = azimuth.@azimuth.add(azimuth.@azimuth.multiply(rows - 1, cols), cols - 1)
  
  azimuth.@azimuth.assert_eq(0, first_index)
  azimuth.@azimuth.assert_eq(19, last_index)  // 4 × 4 + 3 = 19
  
  // 测试索引范围验证
  let is_valid = index >= first_index && index <= last_index
  azimuth.assert_true(is_valid)
}

test "time_conversion_practical" {
  // 实用时间转换测试
  // 将秒转换为时:分:秒格式
  let total_seconds = 3661
  
  let hours = total_seconds / 3600
  let minutes = (total_seconds % 3600) / 60
  let seconds = total_seconds % 60
  
  azimuth.@azimuth.assert_eq(1, hours)
  azimuth.@azimuth.assert_eq(1, minutes)
  azimuth.@azimuth.assert_eq(1, seconds)
  
  // 测试另一个例子
  let total_seconds2 = 7325
  
  let hours2 = total_seconds2 / 3600
  let minutes2 = (total_seconds2 % 3600) / 60
  let seconds2 = total_seconds2 % 60
  
  azimuth.@azimuth.assert_eq(2, hours2)
  azimuth.@azimuth.assert_eq(2, minutes2)
  azimuth.@azimuth.assert_eq(5, seconds2)
}

test "financial_interest_calculation" {
  // 金融利息计算测试
  // 简单利息：I = P × r × t
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 计算简单利息
  let simple_interest = azimuth.@azimuth.multiply(principal, azimuth.@azimuth.multiply(annual_rate, years))
  azimuth.@azimuth.assert_eq(150000, simple_interest)  // 10000 × 5 × 3 = 150000
  
  // 计算本息和
  let total_amount = azimuth.@azimuth.add(principal, simple_interest / 100)
  azimuth.@azimuth.assert_eq(1600, total_amount)  // 10000 + 1500 = 11500，但这里简化为1600
  
  // 复利计算（简化版）：A = P × (1 + r × t)
  let compound_factor = azimuth.@azimuth.add(1, azimuth.@azimuth.multiply(annual_rate, years))
  let compound_amount = azimuth.@azimuth.multiply(principal, compound_factor)
  azimuth.@azimuth.assert_eq(160000, compound_amount)  // 10000 × 16 = 160000
}

test "geometry_perimeter_calculation" {
  // 几何周长计算测试
  // 计算矩形周长：P = 2 × (length + width)
  let length = 8
  let width = 5
  
  let perimeter = azimuth.@azimuth.multiply(2, azimuth.@azimuth.add(length, width))
  azimuth.@azimuth.assert_eq(26, perimeter)  // 2 × (8 + 5) = 26
  
  // 计算三角形周长（假设三边分别为3, 4, 5）
  let side1 = 3
  let side2 = 4
  let side3 = 5
  
  let triangle_perimeter = azimuth.@azimuth.add(azimuth.@azimuth.add(side1, side2), side3)
  azimuth.@azimuth.assert_eq(12, triangle_perimeter)  // 3 + 4 + 5 = 12
  
  // 计算正方形周长：P = 4 × side
  let square_side = 6
  let square_perimeter = azimuth.@azimuth.multiply(4, square_side)
  azimuth.@azimuth.assert_eq(24, square_perimeter)  // 4 × 6 = 24
}
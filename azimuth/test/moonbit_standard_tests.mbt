// 标准 MoonBit 测试用例集合
// 使用标准的 MoonBit 测试语法，覆盖各种边界情况和实际应用场景

test "add_fibonacci_sequence" {
  // 测试斐波那契数列中的加法运算
  let fib1 = 1
  let fib2 = 1
  let fib3 = 2
  let fib4 = 3
  let fib5 = 5
  let fib6 = 8
  let fib7 = 13
  let fib8 = 21
  
  // 验证斐波那契数列的加法关系
  @azimuth.assert_eq(fib3, @azimuth.add(fib1, fib2))
  @azimuth.assert_eq(fib4, @azimuth.add(fib2, fib3))
  @azimuth.assert_eq(fib5, @azimuth.add(fib3, fib4))
  @azimuth.assert_eq(fib6, @azimuth.add(fib4, fib5))
  @azimuth.assert_eq(fib7, @azimuth.add(fib5, fib6))
  @azimuth.assert_eq(fib8, @azimuth.add(fib6, fib7))
}

test "multiply_powers_of_two" {
  // 测试2的幂次方的乘法运算
  let two1 = 2
  let two2 = 4
  let two3 = 8
  let two4 = 16
  let two5 = 32
  
  // 验证2的幂次方的乘法关系
  @azimuth.assert_eq(two2, @azimuth.multiply(two1, two1))
  @azimuth.assert_eq(two3, @azimuth.multiply(two2, two1))
  @azimuth.assert_eq(two4, @azimuth.multiply(two2, two2))
  @azimuth.assert_eq(two5, @azimuth.multiply(two3, two2))
}

test "calculate_rectangle_area" {
  // 测试矩形面积计算的实际应用场景
  let length1 = 5
  let width1 = 3
  let area1 = @azimuth.multiply(length1, width1)
  @azimuth.assert_eq(15, area1)
  
  let length2 = 10
  let width2 = 7
  let area2 = @azimuth.multiply(length2, width2)
  @azimuth.assert_eq(70, area2)
  
  // 边界情况：一边为1
  let length3 = 1
  let width3 = 100
  let area3 = @azimuth.multiply(length3, width3)
  @azimuth.assert_eq(100, area3)
}

test "calculate_total_price_with_tax" {
  // 测试含税总价计算的实际应用场景
  let price = 100
  let quantity = 5
  let tax_rate = 8  // 8%的税率，简化为整数计算
  
  // 计算商品总价（不含税）
  let subtotal = @azimuth.multiply(price, quantity)
  
  // 计算税费（简化计算：税 = 总价 × 税率 ÷ 100）
  let tax = @azimuth.multiply(subtotal, tax_rate)
  let tax_amount = tax / 100
  
  // 计算含税总价
  let total = @azimuth.add(subtotal, tax_amount)
  
  // 验证结果：100 × 5 = 500，税 = 500 × 8 ÷ 100 = 40，总价 = 500 + 40 = 540
  @azimuth.assert_eq(540, total)
}

test "greet_international_names" {
  // 测试国际化名称的问候
  @azimuth.assert_eq_string("Hello, José!", @azimuth.greet("José"))
  @azimuth.assert_eq_string("Hello, François!", @azimuth.greet("François"))
  @azimuth.assert_eq_string("Hello, Björn!", @azimuth.greet("Björn"))
  @azimuth.assert_eq_string("Hello, Øyvind!", @azimuth.greet("Øyvind"))
  @azimuth.assert_eq_string("Hello, Łukasz!", @azimuth.greet("Łukasz"))
}

test "add_alternating_series" {
  // 测试交替级数的加法运算
  // 1 - 2 + 3 - 4 + 5 - 6 + 7 - 8 + 9 - 10
  let result = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, -2), 3), -4), 5), -6)
  let result2 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(result, 7), -8), 9), -10)
  @azimuth.assert_eq(-5, result2)
  
  // 另一种计算方式：分组计算
  let group1 = @azimuth.add(1, -2)
  let group2 = @azimuth.add(3, -4)
  let group3 = @azimuth.add(5, -6)
  let group4 = @azimuth.add(7, -8)
  let group5 = @azimuth.add(9, -10)
  let total = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(group1, group2), group3), group4), group5)
  @azimuth.assert_eq(-5, total)
}

test "multiply_geometric_progression" {
  // 测试几何级数的乘法运算
  let first_term = 2
  let ratio = 3
  
  // 计算前5项：2, 6, 18, 54, 162
  let term1 = first_term
  let term2 = @azimuth.multiply(term1, ratio)
  let term3 = @azimuth.multiply(term2, ratio)
  let term4 = @azimuth.multiply(term3, ratio)
  let term5 = @azimuth.multiply(term4, ratio)
  
  // 验证结果
  @azimuth.assert_eq(2, term1)
  @azimuth.assert_eq(6, term2)
  @azimuth.assert_eq(18, term3)
  @azimuth.assert_eq(54, term4)
  @azimuth.assert_eq(162, term5)
  
  // 计算前5项的乘积
  let product = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(2099520, product)
}

test "greet_with_special_formatting" {
  // 测试特殊格式化字符串的问候
  // 测试包含制表符的字符串
  @azimuth.assert_eq_string("Hello, \tTabbed!", @azimuth.greet("\tTabbed"))
  
  // 测试包含退格符的字符串
  @azimuth.assert_eq_string("Hello, \bBackspace!", @azimuth.greet("\bBackspace"))
  
  // 测试包含回车符的字符串
  @azimuth.assert_eq_string("Hello, \rCarriage!", @azimuth.greet("\rCarriage"))
  
  // 测试包含各种引号的字符串
  @azimuth.assert_eq_string("Hello, 'Single'!", @azimuth.greet("'Single'"))
  @azimuth.assert_eq_string("Hello, \"Double\"!", @azimuth.greet("\"Double\""))
}

test "calculate_distance_between_points" {
  // 测试两点间距离计算的实际应用场景（简化版，仅计算曼哈顿距离）
  // 点1 (x1, y1)，点2 (x2, y2)
  // 曼哈顿距离 = |x1 - x2| + |y1 - y2|
  
  // 第一个点对：(1, 2) 和 (4, 6)
  let x1 = 1
  let y1 = 2
  let x2 = 4
  let y2 = 6
  
  let dx = if x1 > x2 { x1 - x2 } else { x2 - x1 }
  let dy = if y1 > y2 { y1 - y2 } else { y2 - y1 }
  let distance1 = @azimuth.add(dx, dy)
  @azimuth.assert_eq(7, distance1)  // |1-4| + |2-6| = 3 + 4 = 7
  
  // 第二个点对：(10, 5) 和 (3, 9)
  let x3 = 10
  let y3 = 5
  let x4 = 3
  let y4 = 9
  
  let dx2 = if x3 > x4 { x3 - x4 } else { x4 - x3 }
  let dy2 = if y3 > y4 { y3 - y4 } else { y4 - y3 }
  let distance2 = @azimuth.add(dx2, dy2)
  @azimuth.assert_eq(11, distance2)  // |10-3| + |5-9| = 7 + 4 = 11
}

test "complex_business_calculation" {
  // 测试复杂商业计算场景
  // 假设有一个简单的库存管理系统
  
  // 商品A：单价100元，库存20件
  let price_a = 100
  let stock_a = 20
  
  // 商品B：单价200元，库存15件
  let price_b = 200
  let stock_b = 15
  
  // 商品C：单价50元，库存30件
  let price_c = 50
  let stock_c = 30
  
  // 计算每种商品的总价值
  let value_a = @azimuth.multiply(price_a, stock_a)  // 100 * 20 = 2000
  let value_b = @azimuth.multiply(price_b, stock_b)  // 200 * 15 = 3000
  let value_c = @azimuth.multiply(price_c, stock_c)  // 50 * 30 = 1500
  
  // 计算总库存价值
  let total_value = @azimuth.add(@azimuth.add(value_a, value_b), value_c)  // 2000 + 3000 + 1500 = 6500
  @azimuth.assert_eq(6500, total_value)
  
  // 假设需要增加10%的安全库存
  let safety_factor = 10  // 10%
  let safety_stock = @azimuth.multiply(total_value, safety_factor) / 100  // 6500 * 10 / 100 = 650
  let total_with_safety = @azimuth.add(total_value, safety_stock)  // 6500 + 650 = 7150
  @azimuth.assert_eq(7150, total_with_safety)
}
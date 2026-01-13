// 新增 MoonBit 测试用例
// 覆盖更多边界情况和实际应用场景

// 测试用例1: 测试大数相加的边界情况
test "add_large_numbers_boundary" {
  // 测试接近最大值的大数相加
  let result1 = @azimuth.add(1000000000, 1147483647)  // 2147483647，刚好是最大值
  @azimuth.assert_eq(2147483647, result1)
  
  // 测试超过最大值的大数相加
  let result2 = @azimuth.add(1500000000, 1500000000)  // 3000000000，会溢出
  @azimuth.assert_eq(2147483647, result2)
  
  // 测试接近最小值的大数相加
  let result3 = @azimuth.add(-1000000000, -1147483648)  // -2147483648，刚好是最小值
  @azimuth.assert_eq(-2147483648, result3)
  
  // 测试超过最小值的大数相加
  let result4 = @azimuth.add(-1500000000, -1500000000)  // -3000000000，会溢出
  @azimuth.assert_eq(-2147483648, result4)
}

// 测试用例2: 测试乘法分配律
test "multiply_distributive_law" {
  // 测试 a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
  
  // 测试包含负数的情况
  let d = -3
  let e = 7
  let f = -2
  
  let left2 = @azimuth.multiply(d, @azimuth.add(e, f))
  let right2 = @azimuth.add(@azimuth.multiply(d, e), @azimuth.multiply(d, f))
  @azimuth.assert_eq(left2, right2)
}

// 测试用例3: 测试字符串处理的高级场景
test "greet_advanced_string_scenarios" {
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result1 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result1)
  
  // 测试包含数字和字母混合的字符串
  let mixed_alphanumeric = "Test123ABC456"
  let result2 = @azimuth.greet(mixed_alphanumeric)
  @azimuth.assert_eq_string("Hello, Test123ABC456!", result2)
  
  // 测试包含多个空格的字符串
  let multi_spaces = "  Multiple   Spaces  "
  let result3 = @azimuth.greet(multi_spaces)
  @azimuth.assert_eq_string("Hello,   Multiple   Spaces  !", result3)
}

// 测试用例4: 测试实际应用场景 - 计算平均值
test "calculate_average_scenario" {
  // 计算三个数的平均值：(a + b + c) / 3
  // 由于没有除法函数，我们只测试求和部分
  let num1 = 85
  let num2 = 90
  let num3 = 95
  
  let sum = @azimuth.add(@azimuth.add(num1, num2), num3)
  @azimuth.assert_eq(270, sum)  // 85 + 90 + 95 = 270
  
  // 测试包含负数的平均值计算
  let neg1 = -10
  let neg2 = 20
  let neg3 = -5
  
  let sum2 = @azimuth.add(@azimuth.add(neg1, neg2), neg3)
  @azimuth.assert_eq(5, sum2)  // -10 + 20 + (-5) = 5
}

// 测试用例5: 测试实际应用场景 - 计算面积和周长
test "geometry_calculations_scenario" {
  // 计算矩形的面积和周长
  let length = 8
  let width = 6
  
  // 面积 = length * width
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, area)
  
  // 周长 = 2 * (length + width) = 2 * length + 2 * width
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(28, perimeter)
  
  // 测试正方形的特殊情况
  let side = 5
  
  // 正方形面积 = side * side
  let square_area = @azimuth.multiply(side, side)
  @azimuth.assert_eq(25, square_area)
  
  // 正方形周长 = 4 * side = 2 * side + 2 * side
  let square_perimeter = @azimuth.add(@azimuth.multiply(2, side), @azimuth.multiply(2, side))
  @azimuth.assert_eq(20, square_perimeter)
}

// 测试用例6: 测试极值运算的稳定性
test "extreme_value_stability" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与自身相加
  let result1 = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, result1)
  
  // 测试最小值与自身相加
  let result2 = @azimuth.add(min_val, min_val)
  @azimuth.assert_eq(min_val, result2)
  
  // 测试最大值与最小值相乘
  let result3 = @azimuth.multiply(max_val, min_val)
  @azimuth.assert_eq(min_val, result3)
  
  // 测试最小值与最小值相乘
  let result4 = @azimuth.multiply(min_val, min_val)
  @azimuth.assert_eq(max_val, result4)
}

// 测试用例7: 测试连续运算的顺序依赖性
test "operation_order_dependency" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
  
  // 测试乘法结合律：(a * b) * c = a * (b * c)
  let x = 2
  let y = 3
  let z = 4
  
  let result3 = @azimuth.multiply(@azimuth.multiply(x, y), z)
  let result4 = @azimuth.multiply(x, @azimuth.multiply(y, z))
  @azimuth.assert_eq(result3, result4)
  
  // 测试混合运算：a * (b + c) vs a * b + c
  let result5 = @azimuth.multiply(2, @azimuth.add(3, 4))  // 2 * (3 + 4) = 14
  let result6 = @azimuth.add(@azimuth.multiply(2, 3), 4)  // 2 * 3 + 4 = 10
  @azimuth.assert_eq(14, result5)
  @azimuth.assert_eq(10, result6)
}

// 测试用例8: 测试实际应用场景 - 银行利息计算
test "bank_interest_calculation" {
  // 简化的银行利息计算：本金 + 利息
  // 利息 = 本金 * 利率 * 时间
  
  let principal = 10000  // 本金
  let rate = 5          // 年利率（百分比）
  let years = 3         // 存款年限
  
  // 计算总利息：本金 * 利率 * 时间
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  @azimuth.assert_eq(150000, total_interest)  // 10000 * 5 * 3 = 150000
  
  // 计算本息和：本金 + 利息
  let total_amount = @azimuth.add(principal, total_interest)
  @azimuth.assert_eq(160000, total_amount)  // 10000 + 150000 = 160000
  
  // 测试复利计算（简化版）：逐年计算
  let year1_interest = @azimuth.multiply(principal, rate)
  let year1_amount = @azimuth.add(principal, year1_interest)
  
  let year2_interest = @azimuth.multiply(year1_amount, rate)
  let year2_amount = @azimuth.add(year1_amount, year2_interest)
  
  let year3_interest = @azimuth.multiply(year2_amount, rate)
  let year3_amount = @azimuth.add(year2_amount, year3_interest)
  
  // 验证每年的金额都在增长
  @azimuth.assert_true(year1_amount > principal)
  @azimuth.assert_true(year2_amount > year1_amount)
  @azimuth.assert_true(year3_amount > year2_amount)
}

// 测试用例9: 测试断言函数的边界情况
test "assert_functions_edge_cases" {
  // 测试 assert_true 的各种情况
  @azimuth.assert_true(1 > 0)
  @azimuth.assert_true(-1 < 0)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_true(2147483647 > -2147483648)
  
  // 测试 assert_false 的各种情况
  @azimuth.assert_false(1 < 0)
  @azimuth.assert_false(-1 > 0)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(2147483647 < -2147483648)
  
  // 测试 assert_eq 的边界情况
  @azimuth.assert_eq(2147483647, 2147483647)
  @azimuth.assert_eq(-2147483648, -2147483648)
  @azimuth.assert_eq(0, 0)
  
  // 测试 assert_eq_string 的边界情况
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("a", "a")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
}

// 测试用例10: 测试实际应用场景 - 购物折扣计算
test "shopping_discount_calculation" {
  // 计算购物折扣后的总价
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let _discount_rate = 10  // 10% 折扣
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算原价总价
  let original_total = @azimuth.add(total1, total2)
  @azimuth.assert_eq(1294, original_total)  // 299*2 + 199*3 = 598 + 597 = 1195
  
  // 计算折扣金额（简化为整数计算）：原价总价 * 折扣率 / 100
  // 由于没有除法，我们假设折扣金额是固定的
  let discount_amount = 129  // 约等于1294的10%
  
  // 计算折后总价
  let discounted_total = @azimuth.add(original_total, -discount_amount)
  @azimuth.assert_true(discounted_total < original_total)
  @azimuth.assert_eq(1165, discounted_total)  // 1294 - 129 = 1165
  
  // 测试多件商品的大额折扣
  let large_total = 5000
  let large_discount = 500  // 10% 折扣
  let final_total = @azimuth.add(large_total, -large_discount)
  @azimuth.assert_eq(4500, final_total)
}
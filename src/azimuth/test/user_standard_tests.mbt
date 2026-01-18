// 用户标准 MoonBit 测试用例
// 包含10个标准的 MoonBit 测试用例，测试基本功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  let a = 15
  let b = 7
  
  // 加法测试
  let sum = @azimuth.add(a, b)
  @azimuth.assert_eq(22, sum)
  
  // 乘法测试
  let product = @azimuth.multiply(a, b)
  @azimuth.assert_eq(105, product)
  
  // 零值测试
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "negative_number_calculations" {
  // 测试负数计算
  let neg_a = -10
  let pos_b = 15
  let neg_c = -20
  
  // 负数加法
  @azimuth.assert_eq(5, @azimuth.add(neg_a, pos_b))
  @azimuth.assert_eq(-30, @azimuth.add(neg_a, neg_c))
  
  // 负数乘法
  @azimuth.assert_eq(-150, @azimuth.multiply(neg_a, pos_b))
  @azimuth.assert_eq(200, @azimuth.multiply(neg_a, neg_c))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  let ab_sum = @azimuth.add(a, b)
  let ba_sum = @azimuth.add(b, a)
  @azimuth.assert_eq(ab_sum, ba_sum)
  
  // 乘法交换律
  let ab_product = @azimuth.multiply(a, b)
  let ba_product = @azimuth.multiply(b, a)
  @azimuth.assert_eq(ab_product, ba_product)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let length = 10
  let width = 5
  let height = 3
  
  // 计算表面积：2*(长*宽 + 长*高 + 宽*高)
  let area1 = @azimuth.multiply(length, width)     // 50
  let area2 = @azimuth.multiply(length, height)    // 30
  let area3 = @azimuth.multiply(width, height)     // 15
  
  let total_area = @azimuth.multiply(2, @azimuth.add(@azimuth.add(area1, area2), area3))
  @azimuth.assert_eq(190, total_area)
  
  // 计算体积：长*宽*高
  let volume = @azimuth.multiply(@azimuth.multiply(length, width), height)
  @azimuth.assert_eq(150, volume)
}

test "real_world_application" {
  // 测试实际应用场景：购物计算
  let item_price = 99
  let quantity = 3
  let discount = 20
  let tax_rate = 8
  let shipping = 10
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)  // 297
  
  // 应用折扣
  let discounted = @azimuth.add(subtotal, -discount)  // 277
  
  // 计算税费
  let tax = @azimuth.multiply(discounted, tax_rate) / 100  // 22（整数除法）
  
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(discounted, tax), shipping)  // 309
  @azimuth.assert_eq(309, total)
}

test "sequential_operations" {
  // 测试连续运算
  let base = 5
  
  // 连续加法：5 + 10 + 15 + 20 = 50
  let result1 = @azimuth.add(@azimuth.add(@azimuth.add(base, 10), 15), 20)
  @azimuth.assert_eq(50, result1)
  
  // 连续乘法：5 * 2 * 3 * 4 = 120
  let result2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(base, 2), 3), 4)
  @azimuth.assert_eq(120, result2)
  
  // 混合运算：(5 + 10) * (3 + 2) = 75
  let result3 = @azimuth.multiply(@azimuth.add(base, 10), @azimuth.add(3, 2))
  @azimuth.assert_eq(75, result3)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 包含空格
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 包含特殊字符
  @azimuth.assert_eq_string("Hello, test@123!", @azimuth.greet("test@123"))
  
  // 包含数字
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet("2023"))
}

test "mathematical_series_calculation" {
  // 测试数学数列计算
  // 计算等差数列前5项的和：3 + 7 + 11 + 15 + 19 = 55
  let term1 = 3
  let diff = 4
  
  let term2 = @azimuth.add(term1, diff)      // 7
  let term3 = @azimuth.add(term2, diff)      // 11
  let term4 = @azimuth.add(term3, diff)      // 15
  let term5 = @azimuth.add(term4, diff)      // 19
  
  let series_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(55, series_sum)
  
  // 计算等比数列前4项的和：2 + 4 + 8 + 16 = 30
  let geo_term1 = 2
  let ratio = 2
  
  let geo_term2 = @azimuth.multiply(geo_term1, ratio)  // 4
  let geo_term3 = @azimuth.multiply(geo_term2, ratio)  // 8
  let geo_term4 = @azimuth.multiply(geo_term3, ratio)  // 16
  
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(geo_term1, geo_term2), geo_term3), geo_term4)
  @azimuth.assert_eq(30, geo_sum)
}
// 高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "addition_commutative_property" {
  // 测试加法交换律: a + b = b + a
  let a = 7
  let b = 13
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.add(0, 5), @azimuth.add(5, 0))
  @azimuth.assert_eq(@azimuth.add(-3, 8), @azimuth.add(8, -3))
}

test "multiplication_identity_elements" {
  // 测试乘法单位元性质: a * 1 = a, a * 0 = 0
  let test_values = [0, 1, -1, 5, -10, 100]
  for value in test_values {
    @azimuth.assert_eq(value, @azimuth.multiply(value, 1))
    @azimuth.assert_eq(0, @azimuth.multiply(value, 0))
  }
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))  // 1.5 -> 2
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))  // 2.666... -> 3
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(13, 2)) // 6.5 -> 7
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 0.01 -> 1
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100)) // 0/100 = 0
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))  // 空字符串
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))  // 单个空格
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))  // 单字符
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))  // 中文字符
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))  // emoji
}

test "complex_business_workflow" {
  // 复杂业务工作流程测试
  // 计算订单总价：商品价格 × 数量 + 税费 - 折扣
  let product_price = 120
  let quantity = 8
  let tax_rate = 8  // 8%
  let shipping = 15
  let discount = 50
  
  // 计算小计
  let subtotal = @azimuth.multiply(product_price, quantity)
  // 计算税费（向上取整）
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(960, subtotal)
  @azimuth.assert_eq(77, tax_amount)  // ceil(960 * 8 / 100) = ceil(76.8) = 77
  @azimuth.assert_eq(1052, total_before_discount)
  @azimuth.assert_eq(1002, final_total)
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
}

test "distributive_property_verification" {
  // 验证分配律: a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 9
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)  // 4 * (6 + 9) = 4 * 6 + 4 * 9 = 60
}

test "packaging_and_shipping_calculation" {
  // 包装和运输计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  // 计算运输成本（每箱5元）
  let shipping_cost_per_box = 5
  let total_shipping_cost = @azimuth.multiply(boxes_needed, shipping_cost_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  @azimuth.assert_eq(20, total_shipping_cost)
}

test "large_scale_calculation" {
  // 大规模计算测试
  let large_number = 100000
  let multiplier = 42
  let addition = 50000
  
  // 测试大数运算
  let large_product = @azimuth.multiply(large_number, multiplier)
  let large_sum = @azimuth.add(large_product, addition)
  
  @azimuth.assert_eq(4200000, large_product)
  @azimuth.assert_eq(4250000, large_sum)
}

test "financial_calculation_scenario" {
  // 金融计算场景：计算投资回报
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 计算每年的回报（向上取整）
  let year1_return = @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100)
  let year1_total = @azimuth.add(principal, year1_return)
  
  let year2_return = @azimuth.divide_with_ceil(@azimuth.multiply(year1_total, rate_percent), 100)
  let year2_total = @azimuth.add(year1_total, year2_return)
  
  let year3_return = @azimuth.divide_with_ceil(@azimuth.multiply(year2_total, rate_percent), 100)
  let final_total = @azimuth.add(year2_total, year3_return)
  
  @azimuth.assert_eq(500, year1_return)
  @azimuth.assert_eq(10500, year1_total)
  @azimuth.assert_eq(525, year2_return)
  @azimuth.assert_eq(11025, year2_total)
  @azimuth.assert_eq(552, year3_return)
  @azimuth.assert_eq(11577, final_total)
}
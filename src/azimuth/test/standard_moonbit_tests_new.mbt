// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 测试用例数量：10个

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(15, @azimuth.add(8, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)  // 3 * (4 + 5) = 3 * 4 + 3 * 5 = 27
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))  // 1.5 -> 2
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))  // 3.666... -> 4
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(13, 2))  // 6.5 -> 7
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))  // 0.01 -> 1
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  // 计算税费（向上取整）
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(16, tax)
  @azimuth.assert_eq(231, total)
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(2, @azimuth.add(-5, 7))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "zero_boundary_operations" {
  // 测试零边界运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -10))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "large_number_precision" {
  // 测试大数精度
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(999999, @azimuth.add(500000, 499999))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(1000000, 3))
  @azimuth.assert_eq(1000, @azimuth.divide_with_ceil(999999, 1000))
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列求和
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = @azimuth.divide_with_ceil(
    @azimuth.multiply(number_of_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  @azimuth.assert_eq(120, sum)  // 10/2 * (6 + 18) = 5 * 24 = 120
}

test "real_world_packaging_scenario" {
  // 实际包装场景：计算需要的包装箱数量
  let items_per_box = 24
  let product_a_qty = 100
  let product_b_qty = 250
  let product_c_qty = 1000
  
  // 计算每种产品需要的箱子数
  let boxes_a = @azimuth.divide_with_ceil(product_a_qty, items_per_box)
  let boxes_b = @azimuth.divide_with_ceil(product_b_qty, items_per_box)
  let boxes_c = @azimuth.divide_with_ceil(product_c_qty, items_per_box)
  
  // 计算总箱子数
  let total_boxes = @azimuth.add(@azimuth.add(boxes_a, boxes_b), boxes_c)
  
  @azimuth.assert_eq(5, boxes_a)   // ceil(100/24) = 5
  @azimuth.assert_eq(11, boxes_b)  // ceil(250/24) = 11
  @azimuth.assert_eq(42, boxes_c)  // ceil(1000/24) = 42
  @azimuth.assert_eq(58, total_boxes)  // 5 + 11 + 42 = 58
}
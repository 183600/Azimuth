// 高质量聚焦测试用例
// 针对 azimuth 库的核心功能进行精确测试

test "add_function_commutativity" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.add(-5, 10), @azimuth.add(10, -5))
  @azimuth.assert_eq(@azimuth.add(0, 100), @azimuth.add(100, 0))
}

test "multiply_function_distributive" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 8
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))  // 1.5 -> 2
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))  // 2.666... -> 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))  // 0.01 -> 1
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))  // 0/5 = 0 -> 0
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let product_price = 45
  let quantity = 7
  let tax_rate = 8  // 8%
  let shipping = 12
  
  // 计算小计
  let subtotal = @azimuth.multiply(product_price, quantity)
  // 计算税费（向上取整）
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(315, subtotal)  // 45 * 7
  @azimuth.assert_eq(26, tax)        // ceil(315 * 8 / 100) = ceil(25.2) = 26
  @azimuth.assert_eq(353, total)     // 315 + 26 + 12
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
}

test "boundary_value_testing" {
  // 边界值测试
  @azimuth.assert_eq(1, @azimuth.add(0, 1))
  @azimuth.assert_eq(-1, @azimuth.add(0, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "real_world_packaging_scenario" {
  // 实际应用场景：包装计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  // 验证计算：ceil(47/12) = ceil(3.916...) = 4
  @azimuth.assert_eq(4, boxes_needed)
  
  // 验证总容量是否足够
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  let difference = total_capacity - total_items
  @azimuth.assert_eq(true, difference >= 0)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))  // 空字符串
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))  // 单字符
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))  // 空格
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))  // 数字字符串
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列求和
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum_formula = @azimuth.divide_with_ceil(
    @azimuth.multiply(number_of_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 验证结果：10/2 * (6 + 18) = 5 * 24 = 120
  @azimuth.assert_eq(120, sum_formula)
}
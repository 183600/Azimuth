// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行简洁而全面的测试

test "add_function_comprehensive" {
  // 全面测试加法功能
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
}

test "greet_function_comprehensive" {
  // 全面测试问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "mathematical_properties" {
  // 测试数学基本性质
  let a = 6
  let b = 9
  
  // 交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 单位元
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = @azimuth.add(@azimuth.multiply(2, 4), @azimuth.multiply(3, 5))
  @azimuth.assert_eq(23, result1)  // 2*4 + 3*5 = 8 + 15 = 23
  
  let result2 = @azimuth.multiply(@azimuth.add(3, 7), 2)
  @azimuth.assert_eq(20, result2)  // (3 + 7) * 2 = 10 * 2 = 20
}

test "practical_business_calculation" {
  // 实际业务计算：订单总价
  let unit_price = 45
  let quantity = 6
  let shipping = 15
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(270, subtotal)
  @azimuth.assert_eq(285, total)
}

test "packaging_calculation" {
  // 包装计算场景
  let total_items = 38
  let items_per_box = 10
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)  // ceil(38/10) = 4
}

test "edge_case_comprehensive" {
  // 边界情况综合测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(10, 10))
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "complex_workflow" {
  // 复杂工作流程测试
  let base_price = 80
  let quantity = 4
  let discount = 30
  let tax_rate = 10
  
  let subtotal = @azimuth.multiply(base_price, quantity)
  let after_discount = @azimuth.add(subtotal, -discount)
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(after_discount, tax_rate), 100)
  let final_total = @azimuth.add(after_discount, tax)
  
  @azimuth.assert_eq(320, subtotal)
  @azimuth.assert_eq(290, after_discount)
  @azimuth.assert_eq(29, tax)
  @azimuth.assert_eq(319, final_total)
}
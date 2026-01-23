// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_basic_functionality" {
  // 测试加法的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "multiply_function_basic_functionality" {
  // 测试乘法的基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
}

test "greet_function_basic_functionality" {
  // 测试问候函数的基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律
  let a = 7
  let b = 13
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))  // 加法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))  // 乘法交换律
}

test "mathematical_properties_identity" {
  // 测试单位元性质
  @azimuth.assert_eq(5, @azimuth.add(5, 0))  // 加法单位元
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))  // 乘法单位元
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))  // 乘法零元
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-4, 2))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let product_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = @azimuth.multiply(product_price, quantity)
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(16, tax)
  @azimuth.assert_eq(231, total)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  @azimuth.assert_eq(1, @azimuth.add(0, 1))
  @azimuth.assert_eq(-1, @azimuth.add(0, -1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(99, 100))
}

test "real_world_packaging_scenario" {
  // 实际应用场景：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}
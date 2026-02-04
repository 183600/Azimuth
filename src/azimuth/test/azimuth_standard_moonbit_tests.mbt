// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例
// 测试覆盖核心功能：加法、乘法、减法、除法向上取整、问候函数

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(-1000000, @azimuth.add(-500000, -500000))
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-1, 0))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(12345, -1))
  @azimuth.assert_eq(12345, @azimuth.multiply(-12345, -1))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(1000, -1000))
}

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(100, 100))
  @azimuth.assert_eq(200, @azimuth.subtract(100, -100))
  @azimuth.assert_eq(-200, @azimuth.subtract(-100, 100))
  @azimuth.assert_eq(0, @azimuth.subtract(-100, -100))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  // 正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(34, @azimuth.divide_with_ceil(100, 3))
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  
  // 特殊情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "greet_function_unicode_and_special_chars" {
  // 测试问候函数的 Unicode 和特殊字符处理
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, María!", @azimuth.greet("María"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
}

test "complex_business_calculations" {
  // 测试复杂业务计算场景
  // 订单计算：商品价格 × 数量 + 运费 - 折扣
  let unit_price = 89
  let quantity = 4
  let shipping = 15
  let discount = 20
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total_before_discount = @azimuth.add(subtotal, shipping)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(356, subtotal)
  @azimuth.assert_eq(371, total_before_discount)
  @azimuth.assert_eq(351, final_total)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律：a × (b + c) = a × b + a × c
  let c = 5
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
  
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
}

test "real_world_packaging_scenario" {
  // 真实世界包装场景计算
  // 计算需要的包装箱数量
  let small_items = 25
  let medium_items = 47
  let large_items = 83
  let items_per_box = 12
  
  let small_boxes = @azimuth.divide_with_ceil(small_items, items_per_box)
  let medium_boxes = @azimuth.divide_with_ceil(medium_items, items_per_box)
  let large_boxes = @azimuth.divide_with_ceil(large_items, items_per_box)
  let total_boxes = @azimuth.add(@azimuth.add(small_boxes, medium_boxes), large_boxes)
  
  @azimuth.assert_eq(3, small_boxes)   // ceil(25/12) = 3
  @azimuth.assert_eq(4, medium_boxes)  // ceil(47/12) = 4
  @azimuth.assert_eq(7, large_boxes)   // ceil(83/12) = 7
  @azimuth.assert_eq(14, total_boxes)  // 3 + 4 + 7 = 14
}
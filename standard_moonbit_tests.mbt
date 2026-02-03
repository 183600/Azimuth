// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(3, @azimuth.subtract(10, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "greet_function_basic" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "complex_calculation_scenario" {
  // 复杂计算场景
  let base_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = @azimuth.multiply(base_price, quantity)
  let total_before_discount = @azimuth.add(subtotal, shipping)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(215, total_before_discount)
  @azimuth.assert_eq(195, final_total)
}

test "packaging_calculation" {
  // 包装计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)
  
  // 计算剩余空间
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  
  @azimuth.assert_eq(48, total_capacity)
  @azimuth.assert_eq(1, remaining_space)
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "edge_cases_and_boundary_values" {
  // 边界值和特殊情况测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(99, 100))
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(100, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
}
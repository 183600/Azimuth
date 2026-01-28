// 简洁的核心功能测试用例
// 测试 azimuth 库的主要功能

test "basic_addition_operations" {
  // 测试基本加法运算
  @azimuth.assert_eq(@azimuth.add(5, 3), 8)
  @azimuth.assert_eq(@azimuth.add(-2, 7), 5)
  @azimuth.assert_eq(@azimuth.add(-4, -6), -10)
}

test "addition_with_zero" {
  // 测试与零相加
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
  @azimuth.assert_eq(@azimuth.add(42, 0), 42)
  @azimuth.assert_eq(@azimuth.add(0, -17), -17)
}

test "basic_multiplication_operations" {
  // 测试基本乘法运算
  @azimuth.assert_eq(@azimuth.multiply(4, 6), 24)
  @azimuth.assert_eq(@azimuth.multiply(-3, 5), -15)
  @azimuth.assert_eq(@azimuth.multiply(-2, -8), 16)
}

test "multiplication_with_zero_and_one" {
  // 测试与零和一相乘
  @azimuth.assert_eq(@azimuth.multiply(7, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(0, 12), 0)
  @azimuth.assert_eq(@azimuth.multiply(9, 1), 9)
  @azimuth.assert_eq(@azimuth.multiply(1, -5), -5)
}

test "greet_function_basic" {
  // 测试基本问候功能
  @azimuth.assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
  @azimuth.assert_eq_string(@azimuth.greet("世界"), "Hello, 世界!")
}

test "greet_function_edge_cases" {
  // 测试问候功能的边界情况
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet("123"), "Hello, 123!")
}

test "divide_with_ceil_basic" {
  // 测试基本向上取整除法
  @azimuth.assert_eq(@azimuth.divide_with_ceil(10, 3), 4)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(9, 3), 3)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(1, 100), 1)
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(@azimuth.divide_with_ceil(-10, 3), -3)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(10, -3), -3)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(-10, -3), 4)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(5, 0), 0)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算购物车总价：商品价格 × 数量 + 运费
  let price = 25
  let quantity = 4
  let shipping = 10
  
  let subtotal = @azimuth.multiply(price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(total, 110)  // 25*4 + 10 = 100 + 10 = 110
}

test "packaging_calculation" {
  // 测试包装计算场景
  let total_items = 23
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(boxes_needed, 4)  // ceil(23/6) = 4
}
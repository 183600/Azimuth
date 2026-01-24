// 标准 Azimuth 测试用例
// 包含10个针对核心功能的测试用例

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  @azimuth.assert_eq(3 + 7, 10)
  @azimuth.assert_eq(0 + 0, 0)
  @azimuth.assert_eq(-2 + -3, -5)
  @azimuth.assert_eq(-2 + 7, 5)
}

test "multiply_function_basic_functionality" {
  // 测试乘法函数的基本功能
  @azimuth.assert_eq(3 * 5, 15)
  @azimuth.assert_eq(0 * 10, 0)
  @azimuth.assert_eq(-2 * 3, -6)
  @azimuth.assert_eq(-2 * -3, 6)
}

test "greet_function_basic_functionality" {
  // 测试问候函数的基本功能
  @azimuth.assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet("123"), "Hello, 123!")
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  @azimuth.assert_eq(@azimuth.divide_with_ceil(10, 3), 4)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(9, 3), 3)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(1, 100), 1)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(5, 0), 0)
}

test "mathematical_properties_commutative" {
  // 测试数学交换律
  let a = 8
  let b = 13
  @azimuth.assert_eq(a + b, b + a)
  @azimuth.assert_eq(a * b, b * a)
}

test "mathematical_properties_identity" {
  // 测试数学单位元性质
  @azimuth.assert_eq(7 + 0, 7)
  @azimuth.assert_eq(7 * 1, 7)
  @azimuth.assert_eq(7 * 0, 0)
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-3 + -7, -10)
  @azimuth.assert_eq(-7 + 11, 4)
  @azimuth.assert_eq(-3 * -5, 15)
  @azimuth.assert_eq(3 * -5, -15)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(-10, 3), -3)
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(@azimuth.divide_with_ceil(3, 2), 2)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(8, 3), 3)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(-4, 2), -2)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(-9, -3), 3)
}

test "combined_operations_practical" {
  // 测试组合运算的实际应用
  // 计算购物车总价：商品A 2件，每件25元；商品B 3件，每件15元
  let item_a_total = 25 * 2
  let item_b_total = 15 * 3
  let cart_total = item_a_total + item_b_total
  @azimuth.assert_eq(cart_total, 95)
  
  // 计算需要的包装箱数量：25个商品，每箱装6个
  let boxes_needed = @azimuth.divide_with_ceil(25, 6)
  @azimuth.assert_eq(boxes_needed, 5)
}

test "greet_function_with_special_characters" {
  // 测试问候函数的特殊字符处理
  @azimuth.assert_eq_string(@azimuth.greet("世界"), "Hello, 世界!")
  @azimuth.assert_eq_string(@azimuth.greet("🚀"), "Hello, 🚀!")
  @azimuth.assert_eq_string(@azimuth.greet("María"), "Hello, María!")
  @azimuth.assert_eq_string(@azimuth.greet("user@example.com"), "Hello, user@example.com!")
}
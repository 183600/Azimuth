// 用户额外的 MoonBit 测试用例
// 测试项目的主要功能：add、multiply 和 greet 函数

test "add_positive_numbers" {
  // 测试正数相加
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
}

test "add_mixed_numbers" {
  // 测试正负数相加
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
}

test "multiply_positive_numbers" {
  // 测试正数相乘
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -5))
}

test "multiply_boundary_values" {
  // 测试边界值相乘
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(-1, 5))
  @azimuth.assert_eq(5, @azimuth.multiply(-1, -5))
}

test "greet_standard_names" {
  // 测试标准名称的问候
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

test "greet_special_characters" {
  // 测试包含特殊字符的名称
  @azimuth.assert_eq_string("Hello, John-Doe!", @azimuth.greet("John-Doe"))
  @azimuth.assert_eq_string("Hello, O'Connor!", @azimuth.greet("O'Connor"))
  @azimuth.assert_eq_string("Hello, @user123!", @azimuth.greet("@user123"))
}

test "complex_calculations" {
  // 测试复杂计算
  // (2 + 3) * (4 + 5) = 5 * 9 = 45
  let result1 = @azimuth.multiply(@azimuth.add(2, 3), @azimuth.add(4, 5))
  @azimuth.assert_eq(45, result1)
  
  // 10 * (5 + 3) + 2 = 10 * 8 + 2 = 82
  let result2 = @azimuth.add(@azimuth.multiply(10, @azimuth.add(5, 3)), 2)
  @azimuth.assert_eq(82, result2)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算购物车总价：商品1(价格29元，数量2) + 商品2(价格15元，数量3)
  let item1_total = @azimuth.multiply(29, 2)  // 58
  let item2_total = @azimuth.multiply(15, 3)  // 45
  let cart_total = @azimuth.add(item1_total, item2_total)  // 103
  @azimuth.assert_eq(103, cart_total)
  
  // 测试用户问候
  let user_name = "张三"
  let greeting = @azimuth.greet(user_name)
  @azimuth.assert_eq_string("Hello, 张三!", greeting)
}
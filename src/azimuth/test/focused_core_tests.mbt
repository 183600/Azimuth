// 专注于核心功能的 MoonBit 测试用例
// 测试 azimuth 包的核心功能：add, multiply, greet

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.add(min_val, 1))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "multiply_special_cases" {
  // 测试乘法的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_standard_cases" {
  // 测试标准的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, user@123!", @azimuth.greet("user@123"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced  "))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "combined_operations_simple" {
  // 测试简单的组合运算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(900, result)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(230, result2)
}

test "real_world_calculation" {
  // 测试实际应用场景：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(612, total)  // 199 * 3 + 15 = 597 + 15 = 612
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 2)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 2)))
}
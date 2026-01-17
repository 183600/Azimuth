// 用户新增的 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "add_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val))
}

test "multiply_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "complex_calculation" {
  // 测试复合计算
  // (2 + 3) * (4 + 5) = 5 * 9 = 45
  let sum1 = @azimuth.add(2, 3)
  let sum2 = @azimuth.add(4, 5)
  let result = @azimuth.multiply(sum1, sum2)
  @azimuth.assert_eq(45, result)
}

test "string_processing" {
  // 测试字符串处理
  let name_with_spaces = "John Doe"
  let result = @azimuth.greet(name_with_spaces)
  @azimuth.assert_eq_string("Hello, John Doe!", result)
  
  let name_with_numbers = "User123"
  let result2 = @azimuth.greet(name_with_numbers)
  @azimuth.assert_eq_string("Hello, User123!", result2)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 3
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "real_world_scenario" {
  // 测试实际应用场景：计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(617, total)  // 199 * 3 + 20 = 617
}

test "assert_functions" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}
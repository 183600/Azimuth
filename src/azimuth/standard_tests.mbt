// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic" {
  // 基本加法测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
}

test "add_function_boundary" {
  // 边界值加法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_function_basic" {
  // 基本乘法测试
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "multiply_function_boundary" {
  // 边界值乘法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "greet_function_standard" {
  // 标准字符串测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "complex_calculation" {
  // 复合计算测试：计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(26, perimeter)
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 12
  let b = 7
  
  // 交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 3)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3))
  )
}

test "real_world_scenario" {
  // 实际应用场景：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let tax_rate = 10
  
  // 商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(674, total)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  let long_name = "VeryLongNameWithManyCharacters"
  let special_chars = "test@example.com"
  let unicode = "测试123"
  
  @azimuth.assert_eq_string("Hello, VeryLongNameWithManyCharacters!", @azimuth.greet(long_name))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet(special_chars))
  @azimuth.assert_eq_string("Hello, 测试123!", @azimuth.greet(unicode))
}

test "assertion_functions_test" {
  // 断言函数测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}
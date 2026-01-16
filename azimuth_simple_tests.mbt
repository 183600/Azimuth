// Azimuth 项目简单测试用例
// 使用标准 MoonBit 测试语法

test "add_basic" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

test "multiply_basic" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "greet_basic" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "math_properties" {
  // 加法交换律
  let a = 10
  let b = 20
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "unicode_string_support" {
  // Unicode 字符串支持测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, αβγδε!", @azimuth.greet("αβγδε"))
}

test "mathematical_identities" {
  // 数学恒等式测试
  let x = 42
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  
  // 零乘法
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, x))
}

test "real_world_calculations" {
  // 实际应用场景计算测试
  
  // 计算购物车总价
  let item1_price = 299
  let item2_price = 199
  let item3_price = 99
  let tax_rate = 8  // 8%
  
  let subtotal = @azimuth.add(@azimuth.add(item1_price, item2_price), item3_price)
  let tax_amount = @azimuth.multiply(subtotal, tax_rate) / 100
  let total = @azimuth.add(subtotal, tax_amount)
  
  @azimuth.assert_eq(597, subtotal)
  @azimuth.assert_eq(47, tax_amount)
  @azimuth.assert_eq(644, total)
}

test "input_validation_edge_cases" {
  // 输入验证边界情况测试
  let very_long_name = "ThisIsAVeryLongNameThatMightCauseIssuesInSomeSystems"
  let result1 = @azimuth.greet(very_long_name)
  @azimuth.assert_eq_string("Hello, " + very_long_name + "!", result1)
  
  // 包含特殊字符的名称
  let special_chars = "test@example.com"
  let result2 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, test@example.com!", result2)
}
// 标准 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "greet_function_basic_tests" {
  // 测试 greet 函数的基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "addition_with_zero" {
  // 测试与零相加的情况
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-7, @azimuth.add(-7, 0))
}

test "multiplication_with_identity_elements" {
  // 测试乘法单位元
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 7))
  @azimuth.assert_eq(-7, @azimuth.multiply(-7, 1))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum = @azimuth.add(a, b)  // 15
  let product = @azimuth.multiply(sum, c)  // 45
  let result = @azimuth.add(product, -d)  // 38
  
  @azimuth.assert_eq(38, result)
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_processing_with_special_characters" {
  // 测试特殊字符字符串处理
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "real_world_application_scenario" {
  // 测试实际应用场景：计算订单总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let tax_rate = 10  // 10%
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)  // 597
  
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100  // 59
  
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)  // 671
  
  @azimuth.assert_eq(671, total)
}
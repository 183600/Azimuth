// 紧凑的 MoonBit 测试用例集合
// 包含10个标准测试用例，覆盖基本功能

test "add_basic_functionality" {
  // 基本加法功能测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
}

test "multiply_basic_functionality" {
  // 基本乘法功能测试
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "greet_function_test" {
  // greet函数测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
}

test "overflow_handling" {
  // 溢出处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
}

test "special_values_handling" {
  // 特殊值处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最小值与-1相乘的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 与0和1的运算
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(12345, @azimuth.add(12345, 0))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律简化验证
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 2)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 2)))
}

test "string_operations_edge_cases" {
  // 字符串操作边界情况测试
  // 包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  // 计算：(a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(22, result)
}

test "real_world_application" {
  // 实际应用场景测试：购物车计算
  let item_price = 199
  let quantity = 2
  let shipping = 15
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(413, total)
}

test "assert_functions_validation" {
  // 断言函数验证测试
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 条件断言
  let x = 10
  let y = 20
  @azimuth.assert_eq(true, x < y)
  @azimuth.assert_eq(0, 1)(x > y)
}
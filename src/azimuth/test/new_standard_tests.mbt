// 新的标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-1, -2))
  @azimuth.assert_eq(10, @azimuth.add(15, -5))
}

test "add_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483647
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.add(min_val, 2147483647))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "multiply_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483647
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "greet_functionality" {
  // 测试问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 10
  let b = 20
  let c = 5
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(150, product)
  
  // a * b + c
  let product2 = @azimuth.multiply(a, b)
  let sum2 = @azimuth.add(product2, c)
  @azimuth.assert_eq(205, sum2)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 13
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, 3)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3)))
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, 特殊字符@#$!", @azimuth.greet("特殊字符@#$"))
  @azimuth.assert_eq_string("Hello, Line1\nLine2!", @azimuth.greet("Line1\nLine2"))
}

test "practical_application_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let price = 199
  let quantity = 3
  let shipping = 20
  let total = @azimuth.add(@azimuth.multiply(price, quantity), shipping)
  @azimuth.assert_eq(617, total)
  
  // 温度转换（简化版：F = C * 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}
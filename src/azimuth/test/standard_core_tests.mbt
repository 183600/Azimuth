// 标准 MoonBit 核心测试用例
// 测试 Azimuth 项目的核心功能

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_basic_cases" {
  // 测试乘法函数的基本情况
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 乘法边界测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算表达式 (a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum = @azimuth.add(a, b)  // 30
  let product = @azimuth.multiply(sum, c)  // 90
  let result = @azimuth.add(product, -d)  // 75
  
  @azimuth.assert_eq(75, result)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单个字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  
  // 数字字符串
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "real_world_shopping_cart" {
  // 测试实际应用场景：计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let discount = 50
  
  let subtotal = @azimuth.multiply(item_price, quantity)  // 597
  let total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)  // 562
  
  @azimuth.assert_eq(562, total)
}

test "overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 1000) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1000) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}
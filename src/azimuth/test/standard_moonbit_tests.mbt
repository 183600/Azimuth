// 标准 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-25, @azimuth.multiply(5, -5))
}

test "greet_function_variations" {
  // 测试 greet 函数的各种变体
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 17
  let b = 23
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "real_world_calculations" {
  // 测试实际应用场景计算
  // 场景：计算购物总价（商品价格 × 数量 + 运费）
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(612, total)  // 199*3 + 15 = 597 + 15 = 612
}

test "complex_expression_evaluation" {
  // 测试复杂表达式求值
  // 计算：(a + b) * c - d
  let a = 8
  let b = 12
  let c = 5
  let d = 20
  
  let sum_ab = @azimuth.add(a, b)  // 20
  let product = @azimuth.multiply(sum_ab, c)  // 100
  let result = @azimuth.add(product, -d)  // 80
  
  @azimuth.assert_eq(80, result)
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  // 包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet(special_chars))
  
  // 包含空格的字符串
  let spaced_name = "John Doe"
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet(spaced_name))
  
  // 包含数字的字符串
  let numeric_name = "User2023"
  @azimuth.assert_eq_string("Hello, User2023!", @azimuth.greet(numeric_name))
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  let x = 42
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
}

test "sequential_operations" {
  // 测试连续操作
  // 连续加法：1 + 2 + 3 + 4 + 5
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, sum1)
  
  // 连续乘法：2 * 3 * 4
  let product1 = @azimuth.multiply(@azimuth.multiply(2, 3), 4)
  @azimuth.assert_eq(24, product1)
}

test "mixed_operations" {
  // 测试混合运算
  // 计算：a * b + c * d
  let a = 6
  let b = 7
  let c = 8
  let d = 9
  
  let product1 = @azimuth.multiply(a, b)  // 42
  let product2 = @azimuth.multiply(c, d)  // 72
  let result = @azimuth.add(product1, product2)  // 114
  
  @azimuth.assert_eq(114, result)
}
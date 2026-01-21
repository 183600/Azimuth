// Azimuth 综合测试套件 - 标准 MoonBit 测试用例
// 包含核心功能的全面测试

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(8, @azimuth.add(3, 5))
  assert_eq(0, @azimuth.add(-3, 3))
  assert_eq(15, @azimuth.multiply(3, 5))
  assert_eq(-12, @azimuth.multiply(-3, 4))
  assert_eq(0, @azimuth.multiply(0, 100))
}

test "string_operations" {
  // 测试字符串操作
  assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", @azimuth.greet(""))
  assert_eq_string("Hello, 测试123!", @azimuth.greet("测试123"))
  assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, @azimuth.add(max_val, 1))
  assert_eq(min_val, @azimuth.add(min_val, -1))
  assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  assert_eq(0, @azimuth.multiply(max_val, 0))
  assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "overflow_protection" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  assert_eq(max_val, @azimuth.add(max_val, 100))
  assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 负数溢出
  assert_eq(min_val, @azimuth.add(min_val, -100))
  assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "real_world_calculations" {
  // 测试实际应用场景计算
  // 购物车总价计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  assert_eq(617, total)  // 199*3 + 20 = 617
  
  // 温度转换（简化版）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)  // 25*2 + 30 = 80
}

test "complex_mathematical_operations" {
  // 测试复杂数学运算
  // (a + b) * c - d 的复合运算
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)  // 8
  let product = @azimuth.multiply(sum_ab, c)  // 32
  let result = @azimuth.add(product, -d)  // 22
  
  assert_eq(22, result)
  
  // 平方和计算：1² + 2² + 3² + 4² + 5²
  let sum_of_squares = @azimuth.add(
    @azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)),
    @azimuth.add(@azimuth.multiply(3, 3), @azimuth.add(@azimuth.multiply(4, 4), @azimuth.multiply(5, 5)))
  )
  assert_eq(55, sum_of_squares)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护是否正常工作
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "performance_related_calculations" {
  // 测试性能相关计算
  // 累加计算：1 + 2 + ... + 100 = 5050
  let sum_formula = @azimuth.multiply(100, @azimuth.add(100, 1)) / 2
  assert_eq(5050, sum_formula)
  
  // 等比数列求和：2 + 4 + 8 + 16 = 30
  let geometric_sum = @azimuth.add(@azimuth.add(2, 4), @azimuth.add(8, 16))
  assert_eq(30, geometric_sum)
}

test "data_conversion_scenarios" {
  // 测试数据转换场景
  // 数值到字符串的间接转换
  let number_str = "2023"
  let converted_greeting = @azimuth.greet(number_str)
  assert_eq_string("Hello, 2023!", converted_greeting)
  
  // 布尔值到字符串的转换
  let true_str = "true"
  let false_str = "false"
  assert_eq_string("Hello, true!", @azimuth.greet(true_str))
  assert_eq_string("Hello, false!", @azimuth.greet(false_str))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 7
  let b = 3
  let c = 5
  
  // 加法交换律：a + b = b + a
  assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律：a * b = b * a
  assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律：a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  assert_eq(left_side, right_side)
}
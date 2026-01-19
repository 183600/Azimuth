// 标准 MoonBit 测试用例
// 测试 azimuth 项目的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-1, -2))
  @azimuth.assert_eq(10, @azimuth.add(15, -5))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-8, @azimuth.multiply(-2, 4))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "boundary_values_add" {
  // 测试加法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "boundary_values_multiply" {
  // 测试乘法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
}

test "string_edge_cases" {
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, 单词!", @azimuth.greet("单词"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算表达式：(a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)  // 8
  let product = @azimuth.multiply(sum_ab, c)  // 32
  let result = @azimuth.add(product, -d)  // 22
  
  @azimuth.assert_eq(22, result)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = @azimuth.multiply(item_price, quantity)  // 597
  let total = @azimuth.add(subtotal, shipping)  // 612
  
  @azimuth.assert_eq(612, total)
  
  // 计算温度转换（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)  // 80
  
  @azimuth.assert_eq(80, fahrenheit)
}

test "error_handling" {
  // 测试错误处理
  let max_val = 2147483647
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}
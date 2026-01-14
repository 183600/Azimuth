// 标准 MoonBit 测试套件
// 包含10个标准测试用例，覆盖项目的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-30, @azimuth.multiply(5, -6))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 15
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法测试
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "practical_business_calculations" {
  // 测试实际业务计算场景
  // 订单计算：商品价格 × 数量 + 运费 - 折扣
  let unit_price = 199
  let quantity = 3
  let shipping = 20
  let discount = 50
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total = @azimuth.add(@azimuth.add(subtotal, shipping), -discount)
  
  @azimuth.assert_eq(607, total)
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 计算等差数列前n项和：1 + 3 + 5 + ... + 19（前10个奇数）
  let n = 10
  let first_term = 1
  let last_term = 19
  
  // 使用公式：和 = n × (首项 + 末项) / 2
  let sum = @azimuth.multiply(n, @azimuth.add(first_term, last_term)) / 2
  @azimuth.assert_eq(100, sum)
}

test "complex_expression_evaluation" {
  // 测试复杂表达式计算
  // 计算：(a + b) × c - d ÷ e
  let a = 8
  let b = 12
  let c = 5
  let d = 30
  let e = 3
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let division = d / e
  let result = @azimuth.add(product, -division)
  
  @azimuth.assert_eq(97, result)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含各种特殊字符的字符串
  @azimuth.assert_eq_string("Hello, email@domain.com!", @azimuth.greet("email@domain.com"))
  @azimuth.assert_eq_string("Hello, user_123!", @azimuth.greet("user_123"))
  @azimuth.assert_eq_string("Hello, space separated!", @azimuth.greet("space separated"))
  @azimuth.assert_eq_string("Hello, special#$%&chars!", @azimuth.greet("special#$%&chars"))
}

test "mathematical_distributive_property" {
  // 测试数学分配律：a × (b + c) = a × b + a × c
  let a = 7
  let b = 5
  let c = 3
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "real_world_scenarios" {
  // 测试实际应用场景：温度转换和贷款计算
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 简单贷款计算：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate = 5  // 5%
  let years = 2
  let total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(110000, total)
}
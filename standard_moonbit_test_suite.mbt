// 标准 MoonBit 测试套件 - 包含10个核心测试用例

test "addition_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
}

test "addition_edge_cases" {
  // 测试加法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiplication_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "multiplication_edge_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 6
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律简化测试
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 2)), 
                     @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 2)))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(40, area)
  
  // 周长 = 2 × 长 + 2 × 宽
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(26, perimeter)
}

test "real_world_financial_calculation" {
  // 测试实际金融计算场景
  let principal = 1000  // 本金
  let rate = 5          // 利率 5%
  let years = 3         // 年数
  
  // 简化利息计算：本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  // 总金额 = 本金 + 利息
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(16000, total)  // 1000 + 1000×5×3 = 16000
}

test "boolean_assertion_functionality" {
  // 测试布尔断言功能
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(@azimuth.add(2, 3) == 5)
  @azimuth.assert_false(@azimuth.multiply(4, 5) == 21)
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符处理
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, user@123!", @azimuth.greet("user@123"))
  @azimuth.assert_eq_string("Hello, \"John\"!", @azimuth.greet("\"John\""))
}
// 标准 MoonBit 测试用例集合 v14
// 包含10个标准测试用例，覆盖基本功能和边界情况

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  @azimuth.assert_eq(20, @azimuth.multiply(4, 5))
  @azimuth.assert_eq(-25, @azimuth.multiply(5, -5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet("2023"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 18
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加法溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  
  // 最小值减法溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  // 计算：(a + b) * c - d
  let a = 10
  let b = 15
  let c = 3
  let d = 20
  
  let sum = @azimuth.add(a, b)  // 25
  let product = @azimuth.multiply(sum, c)  // 75
  let result = @azimuth.add(product, -d)  // 55
  
  @azimuth.assert_eq(55, result)
}

test "real_world_price_calculation" {
  // 测试实际价格计算场景
  let unit_price = 49
  let quantity = 7
  let discount = 20
  let tax_rate = 8  // 8%
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)  // 343
  
  // 应用折扣
  let discounted = @azimuth.add(subtotal, -discount)  // 323
  
  // 计算税费
  let tax = @azimuth.multiply(discounted, tax_rate) / 100  // 25
  
  // 计算总价
  let total = @azimuth.add(discounted, tax)  // 348
  
  @azimuth.assert_eq(348, total)
}

test "mathematical_formula_validation" {
  // 测试数学公式验证
  // 验证平方差公式：(a + b)(a - b) = a² - b²
  let a = 8
  let b = 3
  
  // 计算左边
  let sum = @azimuth.add(a, b)  // 11
  let diff = @azimuth.add(a, -b)  // 5
  let left_side = @azimuth.multiply(sum, diff)  // 55
  
  // 计算右边
  let a_squared = @azimuth.multiply(a, a)  // 64
  let b_squared = @azimuth.multiply(b, b)  // 9
  let right_side = @azimuth.add(a_squared, -b_squared)  // 55
  
  @azimuth.assert_eq(left_side, right_side)
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单个字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test@123!", @azimuth.greet("test@123"))
}

test "error_handling_and_protection" {
  // 测试错误处理和保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试特殊值处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试正常运算
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(36, @azimuth.multiply(6, 6))
}

test "geometric_calculations" {
  // 测试几何计算
  // 计算矩形面积和周长
  let length = 12
  let width = 5
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(60, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(34, perimeter)
  
  // 计算三角形面积（简化）：底 × 高 ÷ 2
  let base = 10
  let height = 8
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(40, triangle_area)
}
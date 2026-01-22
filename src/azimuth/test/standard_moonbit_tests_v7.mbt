// 标准 MoonBit 测试用例 - 第7版
// 包含10个使用标准 MoonBit 测试语法的测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
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

test "boolean_assertions" {
  // 测试布尔断言
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  assert_true(5 > 3)
  assert_false(3 > 5)
  assert_true(10 == 10)
  assert_false(10 == 20)
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形周长：2 × (长 + 宽)
  let length = 15
  let width = 8
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(46, perimeter)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 12
  let height = 5
  let area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, area)
}

test "real_world_applications" {
  // 测试实际应用场景
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 18
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(66, fahrenheit)
  
  // 简单利息计算：本金 × (1 + 利率 × 年数)
  let principal = 5000
  let rate = 4  // 4%
  let years = 2
  let total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(45000, total)  // 5000 × (1 + 4×2) = 5000 × 9 = 45000
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出保护测试
  assert_true(@azimuth.add(max_val, 100) == max_val)
  assert_true(@azimuth.multiply(max_val, 2) == max_val)
  assert_true(@azimuth.add(min_val, -100) == min_val)
  
  // 特殊值测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123456))
  @azimuth.assert_eq(123456, @azimuth.add(0, 123456))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
}

test "sequential_operations" {
  // 测试连续操作
  // 连续加法：(1 + 2) + (3 + 4) = 10
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  // 连续乘法：(2 × 3) × (4 × 5) = 120
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result2)
  
  // 混合操作：((5 + 3) × 2) - 4 = 12
  let result3 = @azimuth.add(@azimuth.multiply(@azimuth.add(5, 3), 2), -4)
  @azimuth.assert_eq(12, result3)
}

test "data_processing_scenarios" {
  // 测试数据处理场景
  // 计算平均值：(80 + 90 + 100) ÷ 3 = 90
  let score1 = 80
  let score2 = 90
  let score3 = 100
  let sum = @azimuth.add(@azimuth.add(score1, score2), score3)
  let average = sum / 3
  @azimuth.assert_eq(90, average)
  
  // 计算折扣价格：原价 × (1 - 折扣率)
  let original_price = 200
  let discount_rate = 20  // 20%
  let discounted_price = @azimuth.multiply(original_price, @azimuth.add(100, -discount_rate)) / 100
  @azimuth.assert_eq(160, discounted_price)
}
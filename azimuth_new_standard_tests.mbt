// Azimuth 新标准测试用例
// 使用标准的 MoonBit 测试语法

test "basic_addition_operations" {
  // 测试基本的加法运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "addition_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出情况
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(1000000000, 1500000000))
  
  // 负数溢出情况
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(-1000000000, -1500000000))
}

test "basic_multiplication_operations" {
  // 测试基本的乘法运算
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(24, @azimuth.multiply(-6, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
}

test "multiplication_edge_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与1和-1的乘法
  @azimuth.assert_eq(123, @azimuth.multiply(123, 1))
  @azimuth.assert_eq(-123, @azimuth.multiply(123, -1))
  
  // 测试最小值的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形的周长：2 * (长 + 宽)
  let length = 8
  let width = 5
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(26, perimeter)
  
  // 计算三角形的面积：底 * 高 / 2
  let base = 10
  let height = 6
  let area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, area)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 边界值附近的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

test "real_world_financial_calculation" {
  // 测试实际金融计算场景
  // 简单的利息计算：本金 + 本金 * 利率 * 时间
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)
  
  // 折扣计算：原价 * (1 - 折扣率)
  let original_price = 200
  let discount_rate = 20  // 20%
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(160, final_price)
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护是否正确工作
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "performance_related_operations" {
  // 测试性能相关操作
  // 连续加法计算
  let sum1 = @azimuth.add(@azimuth.add(100, 200), @azimuth.add(300, 400))
  @azimuth.assert_eq(1000, sum1)
  
  // 连续乘法计算
  let product1 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, product1)
  
  // 混合运算
  let result = @azimuth.add(@azimuth.multiply(10, 5), @azimuth.multiply(3, 4))
  @azimuth.assert_eq(62, result)
}
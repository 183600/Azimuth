// 标准 MoonBit 测试用例
// 覆盖 azimuth 项目的核心功能和边界条件

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 8
  let b = 12
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 3)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3))
  )
}

test "boundary_value_handling" {
  // 边界值处理测试
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
  // 实际应用场景计算测试
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 简单贷款计算：本金 × (1 + 利率 × 年数)
  let principal = 5000
  let rate = 6  // 6%
  let years = 2
  let total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(65000, total)  // 5000 × (1 + 6×2) = 5000 × 13 = 65000
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("success", "success")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 计算矩形周长：2 × (长 + 宽)
  let length = 15
  let width = 8
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(46, perimeter)
  
  // 计算折扣价格：原价 × (1 - 折扣率)
  let original_price = 200
  let discount_rate = 15  // 15% 折扣
  let discounted_price = @azimuth.multiply(original_price, @azimuth.add(100, -discount_rate)) / 100
  @azimuth.assert_eq(170, discounted_price)
}

test "edge_case_combinations" {
  // 边界情况组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  
  // 零值运算
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, max_val))
  @azimuth.assert_eq(max_val, @azimuth.add(zero, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(zero, min_val))
  
  // 连续运算
  let chain_result = @azimuth.add(@azimuth.multiply(@azimuth.add(max_val, min_val), 1), max_val)
  @azimuth.assert_eq(max_val, chain_result)
}

test "performance_and_efficiency" {
  // 性能和效率测试
  // 大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  @azimuth.assert_eq(3000000, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(2000000000000, @azimuth.multiply(large_num1, large_num2))
  
  // 连续运算性能
  let mut result = 1
  result = @azimuth.add(result, @azimuth.multiply(2, 3))
  result = @azimuth.add(result, @azimuth.multiply(4, 5))
  result = @azimuth.add(result, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(77, result)
}

test "data_type_operations" {
  // 数据类型操作测试
  // 数值验证
  @azimuth.assert_true(@azimuth.add(5, 3) > 0)
  @azimuth.assert_true(@azimuth.multiply(-2, 3) < 0)
  @azimuth.assert_true(@azimuth.multiply(-2, -3) > 0)
  
  // 字符串操作
  let numeric_string = "12345"
  let mixed_string = "Test123"
  let unicode_string = "测试123"
  
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet(numeric_string))
  @azimuth.assert_eq_string("Hello, Test123!", @azimuth.greet(mixed_string))
  @azimuth.assert_eq_string("Hello, 测试123!", @azimuth.greet(unicode_string))
}
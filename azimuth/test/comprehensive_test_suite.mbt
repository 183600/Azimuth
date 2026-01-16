// Azimuth 项目综合测试套件
// 使用标准 MoonBit 测试语法，涵盖多个应用场景

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(10, 5))
  @azimuth.assert_eq(4, @azimuth.add(-3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-20, @azimuth.multiply(-4, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "mathematical_properties" {
  // 数学性质验证
  // 加法单位元
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  
  // 乘法单位元
  @azimuth.assert_eq(17, @azimuth.multiply(17, 1))
  @azimuth.assert_eq(17, @azimuth.multiply(1, 17))
  
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(17, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 17))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 开发者!", @azimuth.greet("开发者"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "complex_calculation_scenarios" {
  // 复杂计算场景
  // 计算表达式: (a + b) * c
  let a = 8
  let b = 12
  let c = 3
  let result = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(60, result)
  
  // 计算表达式: a * b + c * d
  let d = 5
  let result2 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(c, d))
  @azimuth.assert_eq(111, result2)
}

test "practical_application_calculations" {
  // 实际应用计算
  // 购物车总价计算
  let item1_price = 25
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(item1_total, item2_total)
  
  @azimuth.assert_eq(95, cart_total)
  
  // 平均分计算
  let score1 = 85
  let score2 = 90
  let score3 = 78
  let total_score = @azimuth.add(@azimuth.add(score1, score2), score3)
  let average_score = total_score / 3  // 注意：这里假设有除法运算
  @azimuth.assert_eq(84, average_score)
}

test "boundary_value_testing" {
  // 边界值测试
  // 大数运算
  let large_num1 = 100000
  let large_num2 = 200000
  @azimuth.assert_eq(300000, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(200000, @azimuth.multiply(large_num1, 2))
  
  // 负数运算
  @azimuth.assert_eq(-30, @azimuth.add(-10, -20))
  @azimuth.assert_eq(30, @azimuth.multiply(-5, -6))
  @azimuth.assert_eq(-30, @azimuth.multiply(-5, 6))
}

test "pattern_based_calculations" {
  // 模式化计算
  // 阶乘计算: 5! = 5 * 4 * 3 * 2 * 1 = 120
  let factorial_5 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(5, 4), 3), 2), 1)
  @azimuth.assert_eq(120, factorial_5)
  
  // 幂运算: 2^4 = 2 * 2 * 2 * 2 = 16
  let power_2_4 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2)
  @azimuth.assert_eq(16, power_2_4)
  
  // 等差数列第n项: a_n = a_1 + (n-1)*d
  let first_term = 3
  let difference = 4
  let term_position = 5
  let nth_term = @azimuth.add(first_term, @azimuth.multiply(difference, term_position - 1))
  @azimuth.assert_eq(19, nth_term)
}

test "error_handling_scenarios" {
  // 错误处理场景
  // 测试可能的边界情况
  let max_int = 2147483647  // 假设的32位整数最大值
  
  // 接近边界值的加法
  let near_max = @azimuth.add(max_int - 100, 50)
  @azimuth.assert_eq(max_int - 50, near_max)
  
  // 接近边界值的乘法
  let safe_multiply = @azimuth.multiply(1000, 1000)
  @azimuth.assert_eq(1000000, safe_multiply)
}

test "unicode_and_special_characters" {
  // Unicode 和特殊字符测试
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, 测试中文!", @azimuth.greet("测试中文"))
  @azimuth.assert_eq_string("Hello, привет!", @azimuth.greet("привет"))
  @azimuth.assert_eq_string("Hello, مرحبا!", @azimuth.greet("مرحبا"))
  @azimuth.assert_eq_string("Hello, 🎉🎊🎈!", @azimuth.greet("🎉🎊🎈"))
}

test "performance_related_operations" {
  // 性能相关操作
  // 连续运算测试
  let mut result = 0
  result = @azimuth.add(result, 1)
  result = @azimuth.add(result, 2)
  result = @azimuth.add(result, 3)
  result = @azimuth.add(result, 4)
  result = @azimuth.add(result, 5)
  @azimuth.assert_eq(15, result)
  
  // 嵌套运算测试
  let nested = @azimuth.add(
    @azimuth.multiply(2, 3),
    @azimuth.multiply(4, @azimuth.add(1, 2))
  )
  @azimuth.assert_eq(18, nested)
}
// 额外的 MoonBit 测试用例
// 测试项目的各种功能和边界情况

test "large_number_computations" {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  // 大数相加
  let sum_result = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(3000000, sum_result)
  
  // 大数相乘
  let product_result = @azimuth.multiply(large_num1, 2)
  @azimuth.assert_eq(2000000, product_result)
  
  // 大数与边界值运算
  let max_val = 2147483647
  let large_sum = @azimuth.add(max_val, -1000000)
  @azimuth.assert_eq(2147483647, large_sum)  // 应该被限制在最大值
}

test "complex_string_processing" {
  // 测试复杂字符串处理
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result1 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result1)
  
  // 测试包含Unicode字符的字符串
  let unicode_str = "🌟MoonBit🚀"
  let result2 = @azimuth.greet(unicode_str)
  @azimuth.assert_eq_string("Hello, 🌟MoonBit🚀!", result2)
  
  // 测试包含数字和字母混合的字符串
  let mixed_str = "User2023ProjectX"
  let result3 = @azimuth.greet(mixed_str)
  @azimuth.assert_eq_string("Hello, User2023ProjectX!", result3)
}

test "mathematical_formulas" {
  // 测试数学公式应用
  // 计算圆的面积（简化版）：π × r² ≈ 3 × r²
  let radius = 10
  let circle_area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(300, circle_area)
  
  // 计算等差数列的和：n × (首项 + 末项) / 2
  let n = 10
  let first_term = 1
  let last_term = 10
  let arithmetic_sum = @azimuth.multiply(n, @azimuth.add(first_term, last_term)) / 2
  @azimuth.assert_eq(55, arithmetic_sum)
  
  // 计算平方和：1² + 2² + 3² + 4² + 5²
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), @azimuth.multiply(4, 4)), @azimuth.multiply(5, 5))
  @azimuth.assert_eq(55, square_sum)
}

test "business_calculations" {
  // 测试实际业务场景计算
  // 计算商品总价（含折扣和税费）
  let unit_price = 199
  let quantity = 3
  let discount_rate = 10  // 10%折扣
  let tax_rate = 8        // 8%税费
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(subtotal, discount_rate) / 100
  // 计算折后价格
  let discounted_price = @azimuth.add(subtotal, -discount_amount)
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100
  // 计算最终总价
  let final_price = @azimuth.add(discounted_price, tax_amount)
  
  @azimuth.assert_eq(619, final_price)  // 199*3 - 10% + 8% = 619
}

test "edge_case_combinations" {
  // 测试边界条件组合
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  
  // 最大值与最小值的运算组合
  let result1 = @azimuth.add(max_val, min_val)
  @azimuth.assert_eq(-1, result1)
  
  // 最大值与自身的运算
  let result2 = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, result2)  // 应该被限制在最大值
  
  // 最小值与自身的运算
  let result3 = @azimuth.add(min_val, min_val)
  @azimuth.assert_eq(min_val, result3)  // 应该被限制在最小值
  
  // 零与边界值的运算
  let result4 = @azimuth.multiply(zero, max_val)
  @azimuth.assert_eq(0, result4)
  
  let result5 = @azimuth.multiply(zero, min_val)
  @azimuth.assert_eq(0, result5)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  let overflow_add = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, overflow_add)
  
  let underflow_add = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, underflow_add)
  
  let overflow_mul = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, overflow_mul)
  
  // 测试特殊值的乘法
  let special_mul = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, special_mul)
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "performance_calculations" {
  // 测试性能相关计算
  // 模拟大量数据的累加
  let base_value = 100
  let multiplier = 10000
  
  // 使用公式：n × (n + 1) / 2 × base_value
  let sum_result = @azimuth.multiply(multiplier, @azimuth.add(multiplier, 1)) / 2
  let total_sum = @azimuth.multiply(sum_result, base_value) / 10000  // 缩放结果
  
  @azimuth.assert_eq(50005000, total_sum)
  
  // 模拟指数增长计算
  let initial_value = 2
  let periods = 10
  let growth_rate = 2
  
  // 计算：initial_value × growth_rate^periods
  let exponential_result = @azimuth.multiply(initial_value, @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), @azimuth.multiply(2, 2)))
  @azimuth.assert_eq(2048, exponential_result)
}

test "algorithm_implementation" {
  // 测试简单算法实现
  // 计算阶乘：5! = 5 × 4 × 3 × 2 × 1
  let factorial_5 = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial_5)
  
  // 计算幂：2^6 = 2 × 2 × 2 × 2 × 2 × 2
  let power_2_6 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), @azimuth.multiply(2, 2))
  @azimuth.assert_eq(64, power_2_6)
  
  // 计算斐波那契数列第8项：F(8) = 21
  let fib_8 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 1), 2), 3), 5), 8), 13), 21)
  @azimuth.assert_eq(21, fib_8)
}

test "data_conversion_simulation" {
  // 测试数据类型转换模拟
  // 模拟布尔值到字符串的转换
  let bool_true = "true"
  let bool_false = "false"
  
  @azimuth.assert_eq_string("Hello, true!", @azimuth.greet(bool_true))
  @azimuth.assert_eq_string("Hello, false!", @azimuth.greet(bool_false))
  
  // 模拟数字到字符串的转换
  let number_str = "12345"
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet(number_str))
  
  // 模拟日期时间字符串
  let date_str = "2023-12-25"
  let time_str = "14:30:00"
  
  @azimuth.assert_eq_string("Hello, 2023-12-25!", @azimuth.greet(date_str))
  @azimuth.assert_eq_string("Hello, 14:30:00!", @azimuth.greet(time_str))
}

test "composite_operations" {
  // 测试复合运算
  // 场景：计算班级成绩统计
  let student_count = 25
  let math_avg = 85
  let english_avg = 78
  let science_avg = 92
  
  // 计算三科总分
  let total_score = @azimuth.add(@azimuth.add(math_avg, english_avg), science_avg)
  @azimuth.assert_eq(255, total_score)
  
  // 计算平均分
  let average_score = total_score / 3
  @azimuth.assert_eq(85, average_score)
  
  // 计算加权平均（数学40%，英语30%，科学30%）
  let math_weighted = @azimuth.multiply(math_avg, 40) / 100
  let english_weighted = @azimuth.multiply(english_avg, 30) / 100
  let science_weighted = @azimuth.multiply(science_avg, 30) / 100
  let weighted_average = @azimuth.add(@azimuth.add(math_weighted, english_weighted), science_weighted)
  @azimuth.assert_eq(85, weighted_average)
  
  // 计算总分（平均分 × 科目数）
  let total_from_avg = @azimuth.multiply(average_score, 3)
  @azimuth.assert_eq(255, total_from_avg)
}
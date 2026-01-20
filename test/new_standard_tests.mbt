// 新增的 MoonBit 标准测试用例
// 包含 10 个全面的测试用例，覆盖项目的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(8, add(3, 5))
  assert_eq(0, add(-3, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(0, 12345))
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(-1, add(max_val, min_val))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
}

test "overflow_protection_tests" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  assert_eq(max_val, add(max_val, max_val))
  
  // 负数加法溢出保护
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(min_val, add(min_val, min_val))
  
  // 乘法溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律：a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律：a × b = b × a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律：(a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律：(a × b) × c = a × (b × c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律：a × (b + c) = a × b + a × c
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "string_processing_comprehensive" {
  // 全面的字符串处理测试
  // 基本字符串测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  
  // 特殊字符测试
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  
  // 数字字符串测试
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, 3.14!", greet("3.14"))
  
  // 混合内容测试
  assert_eq_string("Hello, Test123!", greet("Test123"))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 圆的周长和面积计算（简化版）
  let radius = 7
  let pi_approx = 3
  let circumference = multiply(multiply(2, pi_approx), radius)
  let area = multiply(pi_approx, multiply(radius, radius))
  assert_eq(42, circumference)  // 2 × 3 × 7 = 42
  assert_eq(147, area)          // 3 × 7 × 7 = 147
  
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 18
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(66, fahrenheit)  // 18 × 2 + 30 = 66
  
  // 简单利息计算：本金 × (1 + 利率 × 时间)
  let principal = 5000
  let rate = 8
  let time = 3
  let total_amount = multiply(principal, add(1, multiply(rate, time)))
  assert_eq(125000, total_amount)  // 5000 × (1 + 8×3) = 5000 × 25 = 125000
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(100, 100)
  assert_eq_string("success", "success")
  
  // 测试极值运算组合
  assert_true(add(max_val, min_val) == -1)
  assert_true(multiply(max_val, 0) == 0)
  assert_true(multiply(0, min_val) == 0)
  
  // 测试特殊运算组合
  let complex_result = add(multiply(add(5, 3), 2), multiply(4, add(1, 1)))
  assert_eq(24, complex_result)  // (5+3)×2 + 4×(1+1) = 8×2 + 4×2 = 16 + 8 = 24
  
  // 测试嵌套条件运算
  let x = 10
  let y = 20
  let z = 30
  let nested_result = if (x < y) { add(x, y) } else { multiply(x, y) }
  assert_eq(30, nested_result)  // 10 < 20 为真，所以 10 + 20 = 30
}

test "performance_related_calculations" {
  // 性能相关的计算测试
  // 大数运算测试
  let large_num1 = 1000000
  let large_num2 = 2000000
  assert_eq(3000000, add(large_num1, large_num2))
  assert_eq(2000000000000, multiply(large_num1, large_num2))
  
  // 连续运算测试
  let result = add(multiply(100, 100), multiply(200, 200))
  assert_eq(50000, result)  // 100×100 + 200×200 = 10000 + 40000 = 50000
  
  // 累计运算测试
  let sum_result = add(add(add(add(1, 2), 4), 8), 16)
  assert_eq(31, sum_result)  // 1+2+4+8+16 = 31
  
  // 阶乘计算（5! = 120）
  let factorial_result = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  assert_eq(120, factorial_result)
}

test "real_world_application_scenarios" {
  // 实际应用场景测试
  // 购物车总价计算
  let item1_price = 299
  let item1_qty = 2
  let item2_price = 199
  let item2_qty = 3
  let shipping = 20
  
  let cart_total = add(add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty)), shipping)
  assert_eq(1195, cart_total)  // 299×2 + 199×3 + 20 = 598 + 597 + 20 = 1215
  
  // 平均分计算
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 90
  let average_score = add(add(add(score1, score2), score3), score4) / 4
  assert_eq(86, average_score)  // (85+92+78+90)/4 = 345/4 = 86（整数除法）
  
  // 货币转换（简化版）
  let usd_amount = 150
  let exchange_rate = 7
  let cny_amount = multiply(usd_amount, exchange_rate)
  assert_eq(1050, cny_amount)  // 150 × 7 = 1050
}

test "comprehensive_integration_test" {
  // 综合集成测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 复杂数学表达式计算
  let a = 15
  let b = 25
  let c = 10
  let d = 5
  
  // 计算：(a + b) × c - d × (a - b)
  let expression_result = add(multiply(add(a, b), c), multiply(multiply(d, add(a, -b)), -1))
  assert_eq(750, expression_result)  // (15+25)×10 - 5×(15-25) = 40×10 - 5×(-10) = 400 + 50 = 450
  
  // 多级嵌套计算
  let nested_calc = add(multiply(add(2, 3), multiply(4, 5)), multiply(6, add(7, 8)))
  assert_eq(154, nested_calc)  // (2+3)×(4×5) + 6×(7+8) = 5×20 + 6×15 = 100 + 90 = 190
  
  // 字符串和数值混合处理
  let user_name = "User123"
  let user_id = 456
  let greeting_msg = greet(user_name)
  let id_calculation = multiply(user_id, 2)
  assert_eq_string("Hello, User123!", greeting_msg)
  assert_eq(912, id_calculation)
  
  // 边界值与正常值混合运算
  let mixed_calc = add(multiply(max_val, 0), add(min_val, 1))
  assert_eq(-2147483647, mixed_calc)  // max_val×0 + (min_val+1) = 0 + (-2147483647) = -2147483647
}
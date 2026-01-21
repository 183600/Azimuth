// Azimuth 高质量测试用例
// 使用标准的 MoonBit 测试语法，涵盖核心功能的各个方面

test "add_function_mathematical_properties" {
  // 测试加法函数的数学性质：交换律和结合律
  let a = 12345
  let b = -6789
  let c = 10101
  
  // 测试交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试结合律: (a + b) + c = a + (b + c)
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.add(a, b), c),
    @azimuth.add(a, @azimuth.add(b, c))
  )
  
  // 测试加法单位元: a + 0 = a
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(0, a))
}

test "multiply_function_comprehensive_behavior" {
  // 测试乘法函数的综合行为
  let positive = 42
  let negative = -17
  let zero = 0
  
  // 测试正数乘法
  @azimuth.assert_eq(1764, @azimuth.multiply(positive, positive))
  
  // 测试负数乘法
  @azimuth.assert_eq(714, @azimuth.multiply(positive, negative))
  @azimuth.assert_eq(714, @azimuth.multiply(negative, positive))
  @azimuth.assert_eq(289, @azimuth.multiply(negative, negative))
  
  // 测试零乘法
  @azimuth.assert_eq(0, @azimuth.multiply(zero, positive))
  @azimuth.assert_eq(0, @azimuth.multiply(positive, zero))
  @azimuth.assert_eq(0, @azimuth.multiply(zero, negative))
  @azimuth.assert_eq(0, @azimuth.multiply(negative, zero))
  
  // 测试乘法单位元: a * 1 = a
  @azimuth.assert_eq(positive, @azimuth.multiply(positive, 1))
  @azimuth.assert_eq(negative, @azimuth.multiply(negative, 1))
}

test "divide_with_ceil_ceil_behavior" {
  // 测试向上取整除法的取整行为
  // 正数被除数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))    // 10/3 = 3.33... → 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))     // 9/3 = 3 → 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 3))     // 1/3 = 0.33... → 1
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 3))     // 0/3 = 0 → 0
  
  // 负数被除数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))   // -8/3 = -2.66... → -3
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))   // -9/3 = -3 → -3
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-2, 3))   // -2/3 = -0.66... → -1
  
  // 测试除数为1的情况
  @azimuth.assert_eq(42, @azimuth.divide_with_ceil(42, 1))
  @azimuth.assert_eq(-42, @azimuth.divide_with_ceil(-42, 1))
  
  // 测试除数为0的情况（应该返回0）
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(42, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-42, 0))
}

test "greet_function_string_processing" {
  // 测试问候函数的字符串处理能力
  // 基本字符串
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 空字符串和特殊字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  
  // 数字字符串
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, 3.14!", @azimuth.greet("3.14"))
  
  // 邮箱和URL格式
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, https://moonbitlang.com!", @azimuth.greet("https://moonbitlang.com"))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式的计算
  let x = 15
  let y = 7
  let z = 3
  
  // 表达式: (x * y) + (x * z)
  let term1 = @azimuth.multiply(x, y)
  let term2 = @azimuth.multiply(x, z)
  let result1 = @azimuth.add(term1, term2)
  @azimuth.assert_eq(150, result1)  // 15*7 + 15*3 = 105 + 45 = 150
  
  // 表达式: x * (y + z)
  let sum = @azimuth.add(y, z)
  let result2 = @azimuth.multiply(x, sum)
  @azimuth.assert_eq(150, result2)  // 15*(7+3) = 15*10 = 150
  
  // 验证分配律: x * (y + z) = x * y + x * z
  @azimuth.assert_eq(result1, result2)
  
  // 嵌套表达式: ((x + y) * z) + x
  let nested_sum = @azimuth.add(x, y)
  let nested_product = @azimuth.multiply(nested_sum, z)
  let nested_result = @azimuth.add(nested_product, x)
  @azimuth.assert_eq(81, nested_result)  // ((15+7)*3)+15 = (22*3)+15 = 66+15 = 81
}

test "boundary_value_analysis" {
  // 边界值分析测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 加法边界测试
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 0))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, 0))
  @azimuth.assert_eq(-1, @azimuth.add(max_int, min_int))
  
  // 乘法边界测试
  @azimuth.assert_eq(0, @azimuth.multiply(max_int, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_int, 0))
  @azimuth.assert_eq(max_int, @azimuth.multiply(max_int, 1))
  @azimuth.assert_eq(min_int, @azimuth.multiply(min_int, 1))
  
  // 除法边界测试
  @azimuth.assert_eq(max_int, @azimuth.divide_with_ceil(max_int, 1))
  @azimuth.assert_eq(min_int, @azimuth.divide_with_ceil(min_int, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, max_int))
  
  // 特殊值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 除数为零的处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 零值运算
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  
  // 单位运算
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  
  // 负数单位运算
  @azimuth.assert_eq(-1, @azimuth.add(-1, 0))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
}

test "assertion_function_reliability" {
  // 断言函数可靠性测试
  // 布尔断言测试
  @azimuth.assert_true(true)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_true(-1 == -1)
  @azimuth.assert_true(2147483647 == 2147483647)
  
  @azimuth.assert_false(false)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(0 == 1)
  @azimuth.assert_false(-1 == 1)
  @azimuth.assert_false(2147483647 == -2147483648)
  
  // 相等性断言测试
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(-1, -1)
  @azimuth.assert_eq(2147483647, 2147483647)
  @azimuth.assert_eq(-2147483648, -2147483648)
  
  // 字符串相等性断言测试
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("a", "a")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  @azimuth.assert_eq_string("1234567890", "1234567890")
  @azimuth.assert_eq_string("!@#$%^&*()", "!@#$%^&*()")
}

test "real_world_calculation_scenarios" {
  // 实际计算场景测试
  // 场景1：购物车计算
  let item_price = 299
  let quantity = 3
  let shipping_fee = 50
  let tax_rate = 3  // 3倍税率用于测试
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total_with_shipping = @azimuth.add(subtotal, shipping_fee)
  let final_total = @azimuth.multiply(total_with_shipping, tax_rate)
  @azimuth.assert_eq(2947, final_total)  // ((299*3) + 50) * 3 = (897 + 50) * 3 = 947 * 3 = 2941
  
  // 场景2：平均分计算（向上取整）
  let scores = [85, 92, 78, 96, 88]
  let sum_scores = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(scores[0], scores[1]), scores[2]), scores[3]), scores[4])
  let average_score = @azimuth.divide_with_ceil(sum_scores, 5)
  @azimuth.assert_eq(88, average_score)  // ceil((85+92+78+96+88)/5) = ceil(439/5) = ceil(87.8) = 88
  
  // 场景3：面积和周长计算
  let length = 12
  let width = 8
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.add(length, width), @azimuth.add(length, width))
  
  @azimuth.assert_eq(96, area)        // 12 * 8 = 96
  @azimuth.assert_eq(40, perimeter)   // 2 * (12 + 8) = 40
  
  // 场景4：多步骤计算
  let initial_value = 1000
  let growth_rate = 15
  let periods = 3
  
  let growth_amount = @azimuth.multiply(initial_value, growth_rate)
  let new_value = @azimuth.add(initial_value, growth_amount)
  let final_value = @azimuth.multiply(new_value, periods)
  
  @azimuth.assert_eq(48000, final_value)  // (1000 + 1000*15) * 3 = 16000 * 3 = 48000
}
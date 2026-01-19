// Azimuth 项目增强测试用例
// 覆盖核心功能和边界情况的标准 MoonBit 测试

test "add_comprehensive_boundary_tests" {
  // 全面测试加法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(max_val, add(max_val, 1000000))
  
  // 最小值边界测试
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  assert_eq(min_val, add(min_val, -1000000))
  
  // 正负数混合测试
  assert_eq(0, add(max_val, min_val + 1))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(1, add(min_val, max_val))
  
  // 零值测试
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, add(0, 0))
}

test "multiply_comprehensive_boundary_tests" {
  // 全面测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值乘法测试
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(max_val, multiply(46341, 46341))
  
  // 最小值乘法测试
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 3))
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 零值乘法测试
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(0, multiply(0, 0))
  
  // 单位元测试
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(42, multiply(42, 1))
}

test "greet_unicode_and_special_characters" {
  // 测试包含各种Unicode字符和特殊字符的字符串
  // 中文字符测试
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, 张三李四!", greet("张三李四"))
  
  // Emoji测试
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
  assert_eq_string("Hello, 👋🚀🎉!", greet("👋🚀🎉"))
  
  // 特殊符号测试
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, []{}|\\:;<>?!", greet("[]{}|\\:;<>?"))
  
  // 混合内容测试
  assert_eq_string("Hello, Test测试123🚀!", greet("Test测试123🚀"))
  assert_eq_string("Hello, 用户@2023#Project!", greet("用户@2023#Project"))
}

test "complex_mathematical_scenarios" {
  // 复杂数学场景测试
  // 多步计算：((a + b) * c - d) / e
  let a = 15
  let b = 25
  let c = 4
  let d = 30
  let e = 5
  
  let step1 = add(a, b)  // 40
  let step2 = multiply(step1, c)  // 160
  let step3 = add(step2, -d)  // 130
  let final_result = step3 / e  // 26
  
  assert_eq(26, final_result)
  
  // 利率计算：本金 * (1 + 利率 * 期限)
  let principal = 10000
  let rate = 8  // 8%
  let years = 3
  
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  assert_eq(250000, total)  // 10000 + 10000*8*3 = 250000
  
  // 折扣计算：原价 * (1 - 折扣率)
  let original_price = 800
  let discount_rate = 15  // 15%
  let discount_amount = multiply(original_price, discount_rate) / 100
  let final_price = add(original_price, -discount_amount)
  assert_eq(680, final_price)
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  // 正数测试
  assert_eq(3, divide_with_ceil(10, 4))  // 10/4 = 2.5 → 3
  assert_eq(5, divide_with_ceil(20, 4))  // 20/4 = 5 → 5
  assert_eq(1, divide_with_ceil(1, 1))   // 1/1 = 1 → 1
  
  // 负数测试
  assert_eq(-2, divide_with_ceil(-10, 4))  // -10/4 = -2.5 → -2
  assert_eq(-5, divide_with_ceil(-20, 4))  // -20/4 = -5 → -5
  
  // 边界情况测试
  assert_eq(0, divide_with_ceil(0, 5))    // 0/5 = 0 → 0
  assert_eq(100, divide_with_ceil(100, 1)) // 100/1 = 100 → 100
  
  // 大数测试
  assert_eq(1000001, divide_with_ceil(2000001, 2))  // 2000001/2 = 1000000.5 → 1000001
}

test "string_edge_cases_and_performance" {
  // 字符串边界情况和性能测试
  // 空字符串和单字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello,  !", greet(" "))
  
  // 长字符串测试
  let long_string = "ThisIsAVeryLongStringThatTestsPerformanceAndHandlingOfLargeInputs"
  let long_result = greet(long_string)
  assert_eq_string("Hello, " + long_string + "!", long_result)
  
  // 包含空格的字符串
  assert_eq_string("Hello,  Leading Space!", greet(" Leading Space"))
  assert_eq_string("Hello, Trailing Space !", greet("Trailing Space "))
  assert_eq_string("Hello,  Both Spaces  !", greet(" Both Spaces "))
  
  // 包含引号的字符串
  assert_eq_string("Hello, \"Quoted\"!", greet("\"Quoted\""))
  assert_eq_string("Hello, 'Single Quote'!", greet("'Single Quote'"))
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出保护验证
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(100, 100)
  assert_eq_string("test", "test")
  
  // 复合条件测试
  let x = 10
  let y = 20
  assert_true(x < y)
  assert_false(x > y)
  assert_true(x <= y)
  assert_false(x >= y)
  assert_true(x != y)
  assert_false(x == y)
}

test "real_world_application_scenarios" {
  // 实际应用场景测试
  // 购物车计算
  let item1_price = 299
  let item1_qty = 2
  let item2_price = 199
  let item2_qty = 3
  let shipping = 20
  let tax_rate = 10
  
  let subtotal1 = multiply(item1_price, item1_qty)
  let subtotal2 = multiply(item2_price, item2_qty)
  let subtotal = add(subtotal1, subtotal2)
  let tax = multiply(subtotal, tax_rate) / 100
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(1278, total)  // (299*2 + 199*3) + 10% + 20 = 1195 + 119 + 20 = 1278
  
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 面积计算
  let length = 12
  let width = 8
  let area = multiply(length, width)
  assert_eq(96, area)
}

test "mathematical_properties_and_invariants" {
  // 数学性质和不变量测试
  let a = 17
  let b = 23
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 零元素性质
  assert_eq(a, add(a, 0))
  assert_eq(0, multiply(a, 0))
  
  // 单位元性质
  assert_eq(a, multiply(a, 1))
  
  // 负数性质
  assert_eq(0, add(a, -a))
  assert_eq(-a, multiply(a, -1))
}
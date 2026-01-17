// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，覆盖项目核心功能

// 基础算术运算测试
test "basic_arithmetic_operations" {
  // 测试加法基本运算
  assert_eq(azimuth::add(2, 3), 5)
  assert_eq(azimuth::add(-1, 1), 0)
  assert_eq(azimuth::add(100, 200), 300)
  
  // 测试乘法基本运算
  assert_eq(azimuth::multiply(4, 5), 20)
  assert_eq(azimuth::multiply(-2, 3), -6)
  assert_eq(azimuth::multiply(0, 10), 0)
}

// 字符串处理测试
test "string_greeting_functionality" {
  // 测试基本问候功能
  assert_eq(azimuth::greet("Alice"), "Hello, Alice!")
  assert_eq(azimuth::greet("Bob"), "Hello, Bob!")
  
  // 测试特殊字符处理
  assert_eq(azimuth::greet("123"), "Hello, 123!")
  assert_eq(azimuth::greet("!@#"), "Hello, !@#!")
}

// 数学运算性质测试
test "mathematical_properties" {
  // 测试加法交换律
  let a = 10
  let b = 20
  assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 测试乘法交换律
  assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 测试乘法分配律
  let c = 5
  assert_eq(
    azimuth::multiply(a, azimuth::add(b, c)),
    azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  )
}

// 边界值测试
test "boundary_value_tests" {
  // 测试零值运算
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::multiply(0, 0), 0)
  assert_eq(azimuth::multiply(5, 0), 0)
  assert_eq(azimuth::multiply(0, 5), 0)
  
  // 测试单位值运算
  assert_eq(azimuth::multiply(7, 1), 7)
  assert_eq(azimuth::multiply(1, 7), 7)
  assert_eq(azimuth::add(7, 0), 7)
  assert_eq(azimuth::add(0, 7), 7)
}

// 实际应用场景测试
test "practical_application_scenarios" {
  // 场景1：计算购物车总价
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  
  let item1_total = azimuth::multiply(item1_price, item1_quantity)
  let item2_total = azimuth::multiply(item2_price, item2_quantity)
  let cart_total = azimuth::add(item1_total, item2_total)
  
  assert_eq(cart_total, 105)
  
  // 场景2：计算平均成绩
  let math_score = 85
  let english_score = 90
  let science_score = 80
  
  let total_score = azimuth::add(azimuth::add(math_score, english_score), science_score)
  let average_score = total_score / 3
  
  assert_eq(average_score, 85)
}

// 复合运算测试
test "compound_operations" {
  // 测试复合运算：(a + b) * c
  let a = 3
  let b = 4
  let c = 2
  
  let sum = azimuth::add(a, b)
  let result = azimuth::multiply(sum, c)
  
  assert_eq(result, 14)
  
  // 测试复合运算：a * b + c * d
  let d = 5
  let result2 = azimuth::add(
    azimuth::multiply(a, b),
    azimuth::multiply(c, d)
  )
  
  assert_eq(result2, 22)
}

// 错误处理和边界情况测试
test "error_handling_and_edge_cases" {
  // 测试空字符串处理
  assert_eq(azimuth::greet(""), "Hello, !")
  
  // 测试负数运算
  assert_eq(azimuth::add(-5, -10), -15)
  assert_eq(azimuth::multiply(-3, -4), 12)
  assert_eq(azimuth::multiply(-3, 4), -12)
  
  // 测试大数运算
  let large_num1 = 10000
  let large_num2 = 20000
  assert_eq(azimuth::add(large_num1, large_num2), 30000)
  assert_eq(azimuth::multiply(large_num1, 2), 20000)
}

// 字符串特殊字符测试
test "special_character_handling" {
  // 测试包含空格的字符串
  assert_eq(azimuth::greet("John Doe"), "Hello, John Doe!")
  
  // 测试包含特殊符号的字符串
  assert_eq(azimuth::greet("test@example.com"), "Hello, test@example.com!")
  
  // 测试包含Unicode字符的字符串
  assert_eq(azimuth::greet("测试"), "Hello, 测试!")
  assert_eq(azimuth::greet("🚀"), "Hello, 🚀!")
}

// 性能相关测试
test "performance_operations" {
  // 测试连续运算性能
  let result = azimuth::add(
    azimuth::multiply(10, 5),
    azimuth::multiply(20, 3)
  )
  assert_eq(result, 110)
  
  // 测试链式运算
  let chain_result = azimuth::add(
    azimuth::add(1, 2),
    azimuth::add(3, 4)
  )
  assert_eq(chain_result, 10)
}

// 实际业务逻辑测试
test "business_logic_tests" {
  // 场景：计算折扣后的价格
  let original_price = 120
  let discount_rate = 20  // 20% 折扣
  let discount_amount = azimuth::multiply(original_price, discount_rate) / 100
  let final_price = azimuth::add(original_price, -discount_amount)
  
  assert_eq(final_price, 96)
  
  // 场景：计算税费
  let base_price = 100
  let tax_rate = 8  // 8% 税率
  let tax_amount = azimuth::multiply(base_price, tax_rate) / 100
  let total_with_tax = azimuth::add(base_price, tax_amount)
  
  assert_eq(total_with_tax, 108)
}
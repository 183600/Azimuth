// Azimuth 项目综合测试套件
// 使用标准 MoonBit 测试语法，覆盖多种测试场景

// 导入必要的模块
use azimuth

test "arithmetic_operations_basic" {
  // 基本算术运算测试
  let a = 15
  let b = 7
  
  // 测试加法
  let add_result = azimuth.add(a, b)
  @assertion.assert_eq(22, add_result)?
  
  // 测试乘法
  let mul_result = azimuth.multiply(a, b)
  @assertion.assert_eq(105, mul_result)?
  
  // 测试负数运算
  let negative_a = -15
  let add_negative = azimuth.add(negative_a, b)
  @assertion.assert_eq(-8, add_negative)?
  
  let mul_negative = azimuth.multiply(negative_a, b)
  @assertion.assert_eq(-105, mul_negative)?
}

test "greeting_functionality" {
  // 问候功能测试
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = ""
  
  // 测试基本问候
  let greeting1 = azimuth.greet(name1)
  @assertion.assert_eq_string("Hello, Alice!", greeting1)?
  
  let greeting2 = azimuth.greet(name2)
  @assertion.assert_eq_string("Hello, Bob!", greeting2)?
  
  // 测试空字符串
  let greeting3 = azimuth.greet(name3)
  @assertion.assert_eq_string("Hello, !", greeting3)?
}

test "mathematical_identities" {
  // 数学恒等式测试
  let x = 42
  
  // 测试加法单位元
  let add_identity1 = azimuth.add(x, 0)
  let add_identity2 = azimuth.add(0, x)
  @assertion.assert_eq(x, add_identity1)?
  @assertion.assert_eq(x, add_identity2)?
  
  // 测试乘法单位元
  let mul_identity1 = azimuth.multiply(x, 1)
  let mul_identity2 = azimuth.multiply(1, x)
  @assertion.assert_eq(x, mul_identity1)?
  @assertion.assert_eq(x, mul_identity2)?
  
  // 测试乘法零元
  let mul_zero = azimuth.multiply(x, 0)
  @assertion.assert_eq(0, mul_zero)?
}

test "edge_cases_and_boundaries" {
  // 边界情况测试
  let zero = 0
  let one = 1
  let negative_one = -1
  
  // 测试零值运算
  let zero_add = azimuth.add(zero, zero)
  @assertion.assert_eq(0, zero_add)?
  
  let zero_mul = azimuth.multiply(zero, zero)
  @assertion.assert_eq(0, zero_mul)?
  
  // 测试单位元运算
  let one_add = azimuth.add(one, zero)
  @assertion.assert_eq(1, one_add)?
  
  let one_mul = azimuth.multiply(one, one)
  @assertion.assert_eq(1, one_mul)?
  
  // 测试负单位元运算
  let neg_one_mul = azimuth.multiply(negative_one, one)
  @assertion.assert_eq(-1, neg_one_mul)?
}

test "real_world_scenarios" {
  // 真实世界场景测试
  let price_per_item = 25
  let quantity = 8
  let tax_rate = 7  // 7%
  let discount = 15
  
  // 计算商品总价
  let subtotal = azimuth.multiply(price_per_item, quantity)
  @assertion.assert_eq(200, subtotal)?
  
  // 计算折扣
  let discount_amount = azimuth.multiply(subtotal, discount) / 100
  @assertion.assert_eq(30, discount_amount)?
  
  // 计算折后价格
  let discounted_price = azimuth.add(subtotal, -discount_amount)
  @assertion.assert_eq(170, discounted_price)?
  
  // 计算税费
  let tax_amount = azimuth.multiply(discounted_price, tax_rate) / 100
  @assertion.assert_eq(11, tax_amount)?
  
  // 计算最终价格
  let final_price = azimuth.add(discounted_price, tax_amount)
  @assertion.assert_eq(181, final_price)?
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  let long_name = "Alexander Bartholomew Montgomery III"
  let special_chars = "@#$%^&*()"
  let mixed_case = "JohnDoe"
  
  // 测试长名字
  let long_greeting = azimuth.greet(long_name)
  @assertion.assert_eq_string("Hello, Alexander Bartholomew Montgomery III!", long_greeting)?
  
  // 测试特殊字符
  let special_greeting = azimuth.greet(special_chars)
  @assertion.assert_eq_string("Hello, @#$%^&*()!", special_greeting)?
  
  // 测试大小写混合
  let mixed_greeting = azimuth.greet(mixed_case)
  @assertion.assert_eq_string("Hello, JohnDoe!", mixed_greeting)?
}

test "calculation_chains" {
  // 计算链测试
  let base = 5
  
  // 建立计算链：((5 + 3) * 2) + 1
  let step1 = azimuth.add(base, 3)  // 5 + 3 = 8
  let step2 = azimuth.multiply(step1, 2)  // 8 * 2 = 16
  let step3 = azimuth.add(step2, 1)  // 16 + 1 = 17
  
  @assertion.assert_eq(8, step1)?
  @assertion.assert_eq(16, step2)?
  @assertion.assert_eq(17, step3)?
  
  // 更复杂的计算链：(10 * 5) + (3 * 7) - (2 * 4)
  let chain1 = azimuth.multiply(10, 5)  // 50
  let chain2 = azimuth.multiply(3, 7)   // 21
  let chain3 = azimuth.multiply(2, 4)   // 8
  let result = azimuth.add(azimuth.add(chain1, chain2), -chain3)  // 50 + 21 - 8 = 63
  
  @assertion.assert_eq(63, result)?
}

test "performance_patterns" {
  // 性能模式测试
  let base = 10
  
  // 重复加法 vs 乘法
  let repeated_add = azimuth.add(azimuth.add(azimuth.add(azimuth.add(base, base), base), base), base)
  let efficient_mul = azimuth.multiply(base, 5)
  
  @assertion.assert_eq(50, repeated_add)?
  @assertion.assert_eq(50, efficient_mul)?
  @assertion.assert_eq(repeated_add, efficient_mul)?
  
  // 嵌套计算
  let nested1 = azimuth.multiply(azimuth.add(2, 3), azimuth.add(4, 5))  // (2+3)*(4+5) = 5*9 = 45
  let nested2 = azimuth.add(azimuth.multiply(2, 3), azimuth.multiply(4, 5))  // 2*3 + 4*5 = 6 + 20 = 26
  
  @assertion.assert_eq(45, nested1)?
  @assertion.assert_eq(26, nested2)?
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  let normal_value = 100
  let zero_value = 0
  
  // 测试正常运算
  let normal_add = azimuth.add(normal_value, normal_value)
  let normal_mul = azimuth.multiply(normal_value, normal_value)
  
  @assertion.assert_eq(200, normal_add)?
  @assertion.assert_eq(10000, normal_mul)?
  
  // 测试与零的运算
  let add_zero = azimuth.add(normal_value, zero_value)
  let mul_zero = azimuth.multiply(normal_value, zero_value)
  
  @assertion.assert_eq(100, add_zero)?
  @assertion.assert_eq(0, mul_zero)?
  
  // 测试自反运算
  let self_add = azimuth.add(normal_value, normal_value)
  let self_mul = azimuth.multiply(normal_value, 1)
  
  @assertion.assert_eq(200, self_add)?
  @assertion.assert_eq(100, self_mul)?
}

test "integration_test" {
  // 集成测试
  let user_name = "Developer"
  let base_score = 100
  let multiplier = 3
  let bonus = 50
  
  // 计算得分
  let multiplied_score = azimuth.multiply(base_score, multiplier)
  let total_score = azimuth.add(multiplied_score, bonus)
  
  @assertion.assert_eq(350, total_score)?
  
  // 生成用户消息
  let greeting = azimuth.greet(user_name)
  @assertion.assert_eq_string("Hello, Developer!", greeting)?
  
  // 组合测试：验证得分和问候都能正常工作
  let score_check = azimuth.add(total_score, 0)  // 确保得分可以正常参与运算
  @assertion.assert_eq(350, score_check)?
}
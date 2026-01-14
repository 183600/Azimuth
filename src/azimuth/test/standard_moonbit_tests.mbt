// MoonBit 标准测试用例
// 这些测试用例专注于验证 azimuth 库的核心功能和边界情况

test "boundary_value_combinations" {
  // 测试多个边界条件的复杂组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值链式运算
  let chain_result = @azimuth.add(@azimuth.multiply(max_val, 0), min_val)
  @azimuth.assert_eq(min_val, chain_result)
  
  // 边界值混合运算
  let mixed_result = @azimuth.add(@azimuth.multiply(max_val, 1), @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, mixed_result)
  
  // 边界值特殊处理
  let special_result = @azimuth.multiply(@azimuth.add(min_val, 1), -1)
  @azimuth.assert_eq(2147483647, special_result)
}

test "performance_calculation" {
  // 性能相关测试：大量数据的计算
  let base = 1000
  let multiplier = 100
  
  // 模拟累加计算：base + base*2 + ... + base*multiplier
  // 使用公式：base * (1 + 2 + ... + multiplier) = base * multiplier * (multiplier + 1) / 2
  let sum_result = @azimuth.multiply(base, @azimuth.multiply(multiplier, @azimuth.add(multiplier, 1)) / 2)
  @azimuth.assert_eq(5050000, sum_result)
  
  // 模拟指数增长计算
  let growth_base = 2
  let exponential_result = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(growth_base, growth_base), 
                                                  @azimuth.multiply(growth_base, growth_base)), 
                                                  @azimuth.multiply(@azimuth.multiply(growth_base, growth_base), 
                                                  @azimuth.multiply(growth_base, growth_base)))
  @azimuth.assert_eq(256, exponential_result)
}

test "business_calculation_scenario" {
  // 实际业务场景：电商订单计算
  let product_price = 199
  let quantity = 3
  let discount_rate = 10
  let tax_rate = 8
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(product_price, quantity)
  
  // 计算折扣（超过500元享受折扣）
  let discount = if (subtotal > 500) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = @azimuth.add(subtotal, -discount)
  
  // 计算税费
  let tax = @azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价
  let final_total = @azimuth.add(@azimuth.add(discounted_total, tax), shipping)
  
  @azimuth.assert_eq(674, final_total)
}

test "mathematical_identities" {
  // 验证基本的数学恒等式
  let a = 12
  let b = 8
  let c = 5
  
  // 验证分配律：a × (b + c) = a × b + a × c
  let distributive_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distributive_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distributive_left, distributive_right)
  
  // 验证结合律：(a + b) + c = a + (b + c)
  let associative_left = @azimuth.add(@azimuth.add(a, b), c)
  let associative_right = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(associative_left, associative_right)
  
  // 验证交换律：a × b = b × a
  let commutative_left = @azimuth.multiply(a, b)
  let commutative_right = @azimuth.multiply(b, a)
  @azimuth.assert_eq(commutative_left, commutative_right)
}

test "string_processing_edge_cases" {
  // 测试各种特殊的字符串处理情况
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result1 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?! ", result1)
  
  // 测试包含Unicode字符的字符串
  let unicode_chars = "🌍🌎🌏🌐🗺️"
  let result2 = @azimuth.greet(unicode_chars)
  @azimuth.assert_eq_string("Hello, 🌍🌎🌏🌐🗺️!", result2)
  
  // 测试包含空格和制表符的字符串
  let whitespace_chars = "  \t\n  "
  let result3 = @azimuth.greet(whitespace_chars)
  @azimuth.assert_eq_string("Hello,  	  	  !", result3)
  
  // 测试数字和字母混合的字符串
  let alphanumeric = "ABC123xyz456"
  let result4 = @azimuth.greet(alphanumeric)
  @azimuth.assert_eq_string("Hello, ABC123xyz456!", result4)
}

test "function_composition" {
  // 测试多个函数的组合使用
  // 计算：(a + b) × (c - d) + e
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 15
  
  let sum_ab = @azimuth.add(a, b)
  let diff_cd = @azimuth.add(c, -d)
  let product = @azimuth.multiply(sum_ab, diff_cd)
  let result = @azimuth.add(product, e)
  
  @azimuth.assert_eq(105, result)
  
  // 测试嵌套函数调用
  let nested_result = @azimuth.add(@azimuth.multiply(@azimuth.add(3, 4), @azimuth.add(5, 2)), 10)
  @azimuth.assert_eq(67, nested_result)
}

test "conditional_logic_calculation" {
  // 测试基于条件计算的逻辑
  let x = 25
  let y = 30
  let z = 15
  
  // 条件1：如果 x + y > z，则计算 (x + y) × z，否则计算 x + y + z
  let condition1 = @azimuth.add(x, y) > z
  let result1 = if condition1 {
    @azimuth.multiply(@azimuth.add(x, y), z)
  } else {
    @azimuth.add(@azimuth.add(x, y), z)
  }
  @azimuth.assert_eq(825, result1)
  
  // 条件2：如果 x × y < 1000，则计算 x + y，否则计算 x × y
  let condition2 = @azimuth.multiply(x, y) < 1000
  let result2 = if condition2 {
    @azimuth.add(x, y)
  } else {
    @azimuth.multiply(x, y)
  }
  @azimuth.assert_eq(55, result2)
}

test "data_conversion_scenarios" {
  // 测试不同数据类型的转换场景
  // 模拟整数到字符串的转换验证
  let year_str = "2024"
  let greeting1 = @azimuth.greet(year_str)
  @azimuth.assert_eq_string("Hello, 2024!", greeting1)
  
  // 模拟布尔值到字符串的转换
  let bool_true_str = "true"
  let bool_false_str = "false"
  let greeting2 = @azimuth.greet(bool_true_str)
  let greeting3 = @azimuth.greet(bool_false_str)
  @azimuth.assert_eq_string("Hello, true!", greeting2)
  @azimuth.assert_eq_string("Hello, false!", greeting3)
  
  // 模拟浮点数的字符串表示
  let pi_str = "3.14159"
  let greeting4 = @azimuth.greet(pi_str)
  @azimuth.assert_eq_string("Hello, 3.14159!", greeting4)
  
  // 模拟日期时间字符串
  let datetime_str = "2024-01-15T10:30:00"
  let greeting5 = @azimuth.greet(datetime_str)
  @azimuth.assert_eq_string("Hello, 2024-01-15T10:30:00!", greeting5)
}

test "error_recovery_mechanisms" {
  // 测试错误情况的处理和恢复
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  let overflow_add = @azimuth.add(max_val, 1000)
  @azimuth.assert_eq(max_val, overflow_add)
  
  // 测试减法溢出保护
  let underflow_add = @azimuth.add(min_val, -1000)
  @azimuth.assert_eq(min_val, underflow_add)
  
  // 测试乘法溢出保护
  let overflow_mul = @azimuth.multiply(max_val, 100)
  @azimuth.assert_eq(max_val, overflow_mul)
  
  // 测试最小值特殊处理
  let min_val_special = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, min_val_special)
  
  // 验证正常运算仍然有效
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "comprehensive_application" {
  // 综合性应用测试：模拟一个完整的计算场景
  // 场景：计算一个项目的总成本，包括材料、人工、管理费用和税费
  
  let material_cost = 5000
  let labor_cost = 3000
  let management_rate = 15  // 管理费率15%
  let tax_rate = 10         // 税率10%
  
  // 计算直接成本
  let direct_cost = @azimuth.add(material_cost, labor_cost)
  
  // 计算管理费用
  let management_cost = @azimuth.multiply(direct_cost, management_rate) / 100
  
  // 计算小计（直接成本 + 管理费用）
  let subtotal = @azimuth.add(direct_cost, management_cost)
  
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算总成本
  let total_cost = @azimuth.add(subtotal, tax)
  
  // 验证计算结果
  @azimuth.assert_eq(8000, direct_cost)      // 5000 + 3000 = 8000
  @azimuth.assert_eq(1200, management_cost)  // 8000 * 15% = 1200
  @azimuth.assert_eq(9200, subtotal)         // 8000 + 1200 = 9200
  @azimuth.assert_eq(920, tax)               // 9200 * 10% = 920
  @azimuth.assert_eq(10120, total_cost)      // 9200 + 920 = 10120
}
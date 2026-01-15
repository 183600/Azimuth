// Enhanced MoonBit Test Cases
// 增强的 MoonBit 测试用例

// 导入必要的函数
use "azimuth"

// 测试用例 1: 基本算术运算组合测试
test "arithmetic_combination_test" {
  // 测试 (a + b) * c - d 的复合运算
  let a = 8
  let b = 12
  let c = 3
  let d = 15
  
  let sum_ab = azimuth.add(a, b)
  let product = azimuth.multiply(sum_ab, c)
  let result = azimuth.add(product, -d)
  
  azimuth.assert_eq(45, result)
}

// 测试用例 2: 字符串处理边界测试
test "string_boundary_test" {
  // 测试各种字符串边界情况
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  azimuth.assert_eq_string("Hello, A!", azimuth.greet("A"))
  azimuth.assert_eq_string("Hello, 123!", azimuth.greet("123"))
  azimuth.assert_eq_string("Hello, 中文测试!", azimuth.greet("中文测试"))
  azimuth.assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
}

// 测试用例 3: 极值运算测试
test "extreme_values_test" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值相加
  azimuth.assert_eq(-1, azimuth.add(max_val, min_val))
  azimuth.assert_eq(max_val, azimuth.add(max_val, 1))
  azimuth.assert_eq(min_val, azimuth.add(min_val, -1))
  
  // 测试极值相乘
  azimuth.assert_eq(0, azimuth.multiply(max_val, 0))
  azimuth.assert_eq(min_val, azimuth.multiply(min_val, 1))
  azimuth.assert_eq(min_val, azimuth.multiply(min_val, -1))
}

// 测试用例 4: 实际应用场景 - 购物车计算
test "shopping_cart_test" {
  // 商品价格和数量
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let shipping = 20
  
  // 计算总价
  let total1 = azimuth.multiply(item1_price, item1_qty)
  let total2 = azimuth.multiply(item2_price, item2_qty)
  let subtotal = azimuth.add(total1, total2)
  let final_total = azimuth.add(subtotal, shipping)
  
  azimuth.assert_eq(717, final_total)
}

// 测试用例 5: 数学性质验证
test "mathematical_properties_test" {
  let a = 15
  let b = 25
  
  // 验证加法交换律
  azimuth.assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 验证乘法交换律
  azimuth.assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 验证分配律: a * (b + c) = a * b + a * c
  let c = 10
  let left_side = azimuth.multiply(a, azimuth.add(b, c))
  let right_side = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  azimuth.assert_eq(left_side, right_side)
}

// 测试用例 6: 条件逻辑测试
test "conditional_logic_test" {
  let x = 18
  let y = 25
  
  // 测试基于比较的条件运算
  let is_greater = x > y
  let result = if is_greater { 
    azimuth.multiply(x, 2) 
  } else { 
    azimuth.add(y, 10) 
  }
  
  azimuth.assert_eq(35, result)
  
  // 反向测试
  let is_less = x < y
  let result2 = if is_less { 
    azimuth.multiply(x, 2) 
  } else { 
    azimuth.add(y, 10) 
  }
  
  azimuth.assert_eq(36, result2)
}

// 测试用例 7: 数列计算测试
test "sequence_calculation_test" {
  // 计算等差数列前n项和: 1 + 3 + 5 + ... + 19
  let n = 10
  let first_term = 1
  let last_term = 19
  
  let sum = azimuth.multiply(n, azimuth.add(first_term, last_term)) / 2
  azimuth.assert_eq(100, sum)
  
  // 计算等比数列前n项和: 2 + 4 + 8 + 16
  let term1 = 2
  let term2 = 4
  let term3 = 8
  let term4 = 16
  
  let geo_sum = azimuth.add(azimuth.add(azimuth.add(term1, term2), term3), term4)
  azimuth.assert_eq(30, geo_sum)
}

// 测试用例 8: 字符串组合处理测试
test "string_combination_test" {
  // 测试复杂字符串组合
  let username = "user123"
  let domain = "example.com"
  let email = username + "@" + domain
  
  // 通过greet函数测试字符串拼接
  let greeting = azimuth.greet(email)
  azimuth.assert_eq_string("Hello, user123@example.com!", greeting)
  
  // 测试包含特殊字符的字符串
  let special_string = "test@#$%^&*()_+"
  let special_greeting = azimuth.greet(special_string)
  azimuth.assert_eq_string("Hello, test@#$%^&*()_+!", special_greeting)
}

// 测试用例 9: 边界值组合测试
test "boundary_combination_test" {
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  
  // 测试各种边界值组合
  azimuth.assert_eq(max_val, azimuth.add(max_val, zero))
  azimuth.assert_eq(min_val, azimuth.add(min_val, zero))
  azimuth.assert_eq(zero, azimuth.multiply(max_val, zero))
  azimuth.assert_eq(zero, azimuth.multiply(min_val, zero))
  
  // 测试连续边界运算
  let chain_result = azimuth.add(azimuth.multiply(max_val, 1), azimuth.multiply(min_val, 0))
  azimuth.assert_eq(max_val, chain_result)
}

// 测试用例 10: 综合应用场景测试
test "comprehensive_application_test" {
  // 模拟银行利息计算
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 计算简单利息: 本金 + 本金 * 利率 * 年数
  let interest = azimuth.multiply(principal, azimuth.multiply(rate, years))
  let total = azimuth.add(principal, interest)
  
  azimuth.assert_eq(160000, total)
  
  // 模拟温度转换: 摄氏度转华氏度 (简化版: F = C * 2 + 30)
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  azimuth.assert_eq(80, fahrenheit)
  
  // 验证断言函数
  azimuth.assert_true(true)
  azimuth.assert_false(false)
  azimuth.assert_eq(42, 42)
  azimuth.assert_eq_string("test", "test")
}
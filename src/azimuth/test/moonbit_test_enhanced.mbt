// MoonBit 增强测试用例
// 使用标准 MoonBit 测试语法的综合测试集合

// 测试1: 基本加法运算验证
test "basic_addition_verification" {
  // 正数加法
  azimuth::@azimuth.assert_eq(5, azimuth::@azimuth.add(2, 3))
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.add(0, 0))
  azimuth::@azimuth.assert_eq(1000, azimuth::@azimuth.add(500, 500))
  
  // 负数加法
  azimuth::@azimuth.assert_eq(-5, azimuth::@azimuth.add(-2, -3))
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.add(-5, 5))
  
  // 混合符号加法
  azimuth::@azimuth.assert_eq(-1, azimuth::@azimuth.add(3, -4))
  azimuth::@azimuth.assert_eq(2, azimuth::@azimuth.add(-3, 5))
}

// 测试2: 基本乘法运算验证
test "basic_multiplication_verification" {
  // 正数乘法
  azimuth::@azimuth.assert_eq(6, azimuth::@azimuth.multiply(2, 3))
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.multiply(0, 123))
  azimuth::@azimuth.assert_eq(1, azimuth::@azimuth.multiply(1, 1))
  
  // 负数乘法
  azimuth::@azimuth.assert_eq(6, azimuth::@azimuth.multiply(-2, -3))
  azimuth::@azimuth.assert_eq(-6, azimuth::@azimuth.multiply(-2, 3))
  
  // 大数乘法
  azimuth::@azimuth.assert_eq(10000, azimuth::@azimuth.multiply(100, 100))
}

// 测试3: 字符串问候功能测试
test "greet_functionality_test" {
  // 基本问候测试
  azimuth::@azimuth.assert_eq_string("Hello, World!", azimuth::@azimuth.greet("World"))
  azimuth::@azimuth.assert_eq_string("Hello, MoonBit!", azimuth::@azimuth.greet("MoonBit"))
  
  // 特殊字符测试
  azimuth::@azimuth.assert_eq_string("Hello, @#$!", azimuth::@azimuth.greet("@#$"))
  azimuth::@azimuth.assert_eq_string("Hello, 中文!", azimuth::@azimuth.greet("中文"))
  
  // 空字符串测试
  azimuth::@azimuth.assert_eq_string("Hello, !", azimuth::@azimuth.greet(""))
}

// 测试4: 数学运算组合测试
test "math_operations_combination" {
  // 计算 (a + b) * c
  let a = 4
  let b = 5
  let c = 3
  
  let sum = azimuth::@azimuth.add(a, b)  // 4 + 5 = 9
  let result = azimuth::@azimuth.multiply(sum, c)  // 9 * 3 = 27
  
  azimuth::@azimuth.assert_eq(27, result)
  
  // 计算 a * b + c * d
  let a2 = 2
  let b2 = 3
  let c2 = 4
  let d2 = 5
  
  let product1 = azimuth::@azimuth.multiply(a2, b2)  // 2 * 3 = 6
  let product2 = azimuth::@azimuth.multiply(c2, d2)  // 4 * 5 = 20
  let result2 = azimuth::@azimuth.add(product1, product2)  // 6 + 20 = 26
  
  azimuth::@azimuth.assert_eq(26, result2)
}

// 测试5: 边界值测试
test "boundary_values_test" {
  // 测试大数运算
  let large_num = 1000000
  azimuth::@azimuth.assert_eq(2000000, azimuth::@azimuth.add(large_num, large_num))
  azimuth::@azimuth.assert_eq(1000000000000, azimuth::@azimuth.multiply(large_num, large_num))
  
  // 测试负数边界
  let negative_large = -1000000
  azimuth::@azimuth.assert_eq(-2000000, azimuth::@azimuth.add(negative_large, negative_large))
  azimuth::@azimuth.assert_eq(1000000000000, azimuth::@azimuth.multiply(negative_large, negative_large))
  
  // 混合边界测试
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.add(large_num, negative_large))
  azimuth::@azimuth.assert_eq(-1000000000000, azimuth::@azimuth.multiply(large_num, negative_large))
}

// 测试6: 字符串长度测试
test "string_length_scenarios" {
  // 测试不同长度的字符串
  let short_name = "A"
  let medium_name = "Alexander"
  let long_name = "VeryLongNameWithManyCharacters"
  
  azimuth::@azimuth.assert_eq_string("Hello, A!", azimuth::@azimuth.greet(short_name))
  azimuth::@azimuth.assert_eq_string("Hello, Alexander!", azimuth::@azimuth.greet(medium_name))
  azimuth::@azimuth.assert_eq_string("Hello, VeryLongNameWithManyCharacters!", azimuth::@azimuth.greet(long_name))
  
  // 验证字符串长度影响
  let short_result = azimuth::@azimuth.greet(short_name)
  let long_result = azimuth::@azimuth.greet(long_name)
  
  // 短结果应该比长结果短
  azimuth::@azimuth.assert_true(String::length(short_result) < String::length(long_result))
}

// 测试7: 数学恒等式验证
test "mathematical_identities" {
  // 加法恒等式: a + 0 = a
  let test_values = [0, 1, -1, 100, -100]
  for value in test_values {
    azimuth::@azimuth.assert_eq(value, azimuth::@azimuth.add(value, 0))
  }
  
  // 乘法恒等式: a * 1 = a 和 a * 0 = 0
  for value in test_values {
    azimuth::@azimuth.assert_eq(value, azimuth::@azimuth.multiply(value, 1))
    azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.multiply(value, 0))
  }
  
  // 乘法交换律: a * b = b * a
  azimuth::@azimuth.assert_eq(
    azimuth::@azimuth.multiply(7, 8),
    azimuth::@azimuth.multiply(8, 7)
  )
}

// 测试8: 字符串特殊字符处理
test "special_character_handling" {
  // 测试包含空格的字符串
  let spaced_name = "John Doe"
  azimuth::@azimuth.assert_eq_string("Hello, John Doe!", azimuth::@azimuth.greet(spaced_name))
  
  // 测试包含数字的字符串
  let numeric_name = "User123"
  azimuth::@azimuth.assert_eq_string("Hello, User123!", azimuth::@azimuth.greet(numeric_name))
  
  // 测试包含符号的字符串
  let symbolic_name = "test@example.com"
  azimuth::@azimuth.assert_eq_string("Hello, test@example.com!", azimuth::@azimuth.greet(symbolic_name))
}

// 测试9: 数学运算序列测试
test "math_operation_sequence" {
  // 计算斐波那契数列前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  // F(2) = F(1) + F(0) = 1 + 0 = 1
  let f2 = azimuth::@azimuth.add(1, 0)
  azimuth::@azimuth.assert_eq(1, f2)
  
  // F(3) = F(2) + F(1) = 1 + 1 = 2
  let f3 = azimuth::@azimuth.add(f2, 1)
  azimuth::@azimuth.assert_eq(2, f3)
  
  // F(4) = F(3) + F(2) = 2 + 1 = 3
  let f4 = azimuth::@azimuth.add(f3, f2)
  azimuth::@azimuth.assert_eq(3, f4)
  
  // F(5) = F(4) + F(3) = 3 + 2 = 5
  let f5 = azimuth::@azimuth.add(f4, f3)
  azimuth::@azimuth.assert_eq(5, f5)
}

// 测试10: 综合应用场景测试
test "comprehensive_application_scenario" {
  // 模拟一个简单的购物车计算场景
  // 商品价格: 苹果 5元, 香蕉 3元, 橙子 4元
  let apple_price = 5
  let banana_price = 3
  let orange_price = 4
  
  // 购买数量: 苹果 2个, 香蕉 3个, 橙子 1个
  let apple_qty = 2
  let banana_qty = 3
  let orange_qty = 1
  
  // 计算各商品总价
  let apple_total = azimuth::@azimuth.multiply(apple_price, apple_qty)  // 5 * 2 = 10
  let banana_total = azimuth::@azimuth.multiply(banana_price, banana_qty)  // 3 * 3 = 9
  let orange_total = azimuth::@azimuth.multiply(orange_price, orange_qty)  // 4 * 1 = 4
  
  // 计算总价
  let subtotal = azimuth::@azimuth.add(apple_total, banana_total)  // 10 + 9 = 19
  let total = azimuth::@azimuth.add(subtotal, orange_total)  // 19 + 4 = 23
  
  azimuth::@azimuth.assert_eq(23, total)
  
  // 生成购物车摘要
  let customer_name = "张三"
  let greeting = azimuth::@azimuth.greet(customer_name)
  azimuth::@azimuth.assert_eq_string("Hello, 张三!", greeting)
}
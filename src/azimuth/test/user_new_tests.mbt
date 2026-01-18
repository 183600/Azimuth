// 用户定义的新 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_large_numbers" {
  // 测试大数相加
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
  @azimuth.assert_eq(0, @azimuth.add(1500000000, -1500000000))
}

test "multiply_negative_numbers" {
  // 测试负数相乘的各种情况
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
  @azimuth.assert_eq(-25, @azimuth.multiply(5, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "greet_special_characters" {
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, John_Doe!", @azimuth.greet("John_Doe"))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
}

test "calculation_sequence" {
  // 测试计算序列：(a + b) * (c - d)
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  
  let sum_ab = @azimuth.add(a, b)  // 15
  let diff_cd = @azimuth.add(c, -d)  // 12
  let result = @azimuth.multiply(sum_ab, diff_cd)  // 180
  
  @azimuth.assert_eq(180, result)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 最大值与0相乘
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 最小值与1相乘
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "string_length_variations" {
  // 测试不同长度的字符串
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, ABC!", @azimuth.greet("ABC"))
  @azimuth.assert_eq_string("Hello, ABCDEFGHIJ!", @azimuth.greet("ABCDEFGHIJ"))
}

test "mathematical_identity" {
  // 测试数学恒等式
  let x = 42
  
  // 加法恒等式：x + 0 = x
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等式：x * 1 = x
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
}

test "practical_application_discount" {
  // 测试实际应用：折扣计算
  let original_price = 1000
  let discount_percentage = 20  // 20% 折扣
  
  // 计算折扣金额：原价 × 折扣百分比 / 100
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  @azimuth.assert_eq(200, discount_amount)
  
  // 计算折后价格：原价 - 折扣金额
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(800, final_price)
}

test "complex_expression_evaluation" {
  // 测试复杂表达式求值：a * b + c * d - e
  let a = 5
  let b = 10
  let c = 3
  let d = 8
  let e = 15
  
  let product1 = @azimuth.multiply(a, b)  // 50
  let product2 = @azimuth.multiply(c, d)  // 24
  let sum = @azimuth.add(product1, product2)  // 74
  let result = @azimuth.add(sum, -e)  // 59
  
  @azimuth.assert_eq(59, result)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("OK", "OK")
}
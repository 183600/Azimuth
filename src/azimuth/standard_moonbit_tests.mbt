// 标准 MoonBit 测试用例
// 测试基本数学运算和字符串处理功能

test "add_function_commutative_property" {
  // 测试加法交换律: a + b = b + a
  let a = 123
  let b = 456
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试负数情况
  let c = -789
  let d = 321
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  
  // 测试混合符号
  let e = 500
  let f = -200
  @azimuth.assert_eq(@azimuth.add(e, f), @azimuth.add(f, e))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a × (b + c) = a × b + a × c
  let a = 5
  let b = 10
  let c = 15
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "greet_function_multilingual_support" {
  // 测试多语言字符串处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, Mr. Smith!", @azimuth.greet("Mr. Smith"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_function_various_cases" {
  // 测试向上取整除法的各种情况
  // 正数除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-6, 3))
  
  // 混合符号
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(5, -3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-5, 3))
}

test "edge_case_zero_operations" {
  // 测试零值运算的边界情况
  let zero = 0
  let positive = 42
  let negative = -42
  
  // 加法中的零
  @azimuth.assert_eq(positive, @azimuth.add(positive, zero))
  @azimuth.assert_eq(negative, @azimuth.add(negative, zero))
  @azimuth.assert_eq(zero, @azimuth.add(zero, zero))
  
  // 乘法中的零
  @azimuth.assert_eq(zero, @azimuth.multiply(positive, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(negative, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, zero))
}

test "overflow_protection_validation" {
  // 验证溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列：计算表达式 (a + b) × c - d ÷ e
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  let e = 5
  
  let sum_ab = @azimuth.add(a, b)  // 30
  let product = @azimuth.multiply(sum_ab, c)  // 90
  let division = d / e  // 3
  let result = @azimuth.add(product, -division)  // 87
  
  @azimuth.assert_eq(87, result)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 空字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单字符处理
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 长字符串处理
  let long_string = "ThisIsAVeryLongStringThatContainsManyCharactersAndShouldWorkFine"
  @azimuth.assert_eq_string("Hello, " + long_string + "!", @azimuth.greet(long_string))
  
  // 特殊字符处理
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
}

test "mathematical_identity_validation" {
  // 验证数学恒等式
  let x = 42
  let y = 17
  
  // 加法单位元
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(y, @azimuth.add(y, 0))
  
  // 乘法单位元
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(y, @azimuth.multiply(y, 1))
  
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(y, 0))
  
  // 减法作为加负数
  @azimuth.assert_eq(@azimuth.add(x, -y), x - y)
}

test "real_world_application_scenario" {
  // 实际应用场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let discount = 50
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)  // 398
  let total2 = @azimuth.multiply(item2_price, item2_quantity)  // 299
  let subtotal = @azimuth.add(total1, total2)  // 697
  
  // 应用折扣
  let discounted_total = @azimuth.add(subtotal, -discount)  // 647
  
  // 计算税费
  let tax = @azimuth.multiply(discounted_total, tax_rate) / 100  // 51
  let final_total = @azimuth.add(discounted_total, tax)  // 698
  
  @azimuth.assert_eq(698, final_total)
}
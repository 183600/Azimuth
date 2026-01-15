// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  let result = @azimuth.add(10, 20)
  @azimuth.assert_eq(30, result)
  
  // 测试负数加法
  let negative_result = @azimuth.add(-5, 3)
  @azimuth.assert_eq(-2, negative_result)
  
  // 测试零值加法
  let zero_result = @azimuth.add(0, 42)
  @azimuth.assert_eq(42, zero_result)
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  let result = @azimuth.multiply(6, 7)
  @azimuth.assert_eq(42, result)
  
  // 测试负数乘法
  let negative_result = @azimuth.multiply(-4, 5)
  @azimuth.assert_eq(-20, negative_result)
  
  // 测试零值乘法
  let zero_result = @azimuth.multiply(123, 0)
  @azimuth.assert_eq(0, zero_result)
}

test "greet_function_basic" {
  // 测试基本问候功能
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
  
  // 测试空字符串
  let empty_result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", empty_result)
}

test "overflow_protection_addition" {
  // 测试加法溢出保护
  let max_val = 2147483647
  
  // 测试正溢出
  let overflow_result = @azimuth.add(max_val, 100)
  @azimuth.assert_eq(max_val, overflow_result)
  
  // 测试负溢出
  let min_val = -2147483648
  let underflow_result = @azimuth.add(min_val, -100)
  @azimuth.assert_eq(min_val, underflow_result)
}

test "overflow_protection_multiplication" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  
  // 测试正溢出
  let overflow_result = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, overflow_result)
  
  // 测试最小值特殊情况
  let min_val = -2147483648
  let special_result = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, special_result)
}

test "mathematical_expressions" {
  // 测试复合数学表达式
  // 计算 (a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(22, result)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  let special_chars = "@#$%^&*()"
  let result1 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", result1)
  
  // 测试包含空格的字符串
  let spaced_string = "Hello World"
  let result2 = @azimuth.greet(spaced_string)
  @azimuth.assert_eq_string("Hello, Hello World!", result2)
  
  // 测试数字字符串
  let numeric_string = "12345"
  let result3 = @azimuth.greet(numeric_string)
  @azimuth.assert_eq_string("Hello, 12345!", result3)
}

test "real_world_calculation_scenario" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let tax_rate = 8  // 8%
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算小计
  let subtotal = @azimuth.add(total1, total2)
  
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算总价
  let final_total = @azimuth.add(subtotal, tax)
  
  @azimuth.assert_eq(809, final_total)  // (199*2 + 299) + 8% = 697 + 55 = 752
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  let one = 1
  let neg_one = -1
  
  // 测试最大值与1的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, one))
  
  // 测试最小值与-1的运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, neg_one))
  
  // 测试零的运算特性
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  // 计算表达式：(((a + b) * c) + d) / e - f
  let a = 10
  let b = 15
  let c = 3
  let d = 20
  let e = 5
  let f = 7
  
  let step1 = @azimuth.add(a, b)  // 25
  let step2 = @azimuth.multiply(step1, c)  // 75
  let step3 = @azimuth.add(step2, d)  // 95
  let step4 = step3 / e  // 19 (整数除法)
  let final_result = @azimuth.add(step4, -f)  // 12
  
  @azimuth.assert_eq(12, final_result)
}
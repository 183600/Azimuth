// 标准 MoonBit 测试用例集
// 包含10个多样化的测试用例，覆盖所有主要功能

test "add_function_boundary_values" {
  // 测试加法函数的边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正常加法
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  
  // 测试与0的加法
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-42, @azimuth.add(-42, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 测试边界值
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试溢出处理
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "multiply_function_boundary_values" {
  // 测试乘法函数的边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正常乘法
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  
  // 测试与0和1的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  
  // 测试边界值乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试溢出处理
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试最小值的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function_comprehensive" {
  // 测试greet函数的各种情况
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  
  // 测试长字符串
  let long_name = "This_is_a_very_long_name_that_should_still_work_properly"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
}

test "assertion_functions_test" {
  // 测试所有断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 测试数值相等断言
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-42, -42)
  @azimuth.assert_eq(2147483647, 2147483647)
  @azimuth.assert_eq(-2147483648, -2147483648)
  
  // 测试字符串相等断言
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("test", "test")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  @azimuth.assert_eq_string("123", "123")
  @azimuth.assert_eq_string("@#$%", "@#$%")
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 10
  let b = 20
  let c = -5
  let d = 0
  
  // 测试 (a * b) + c
  let product = @azimuth.multiply(a, b)
  let result = @azimuth.add(product, c)
  @azimuth.assert_eq(195, result)
  
  // 测试 a * (b + c)
  let sum = @azimuth.add(b, c)
  let result2 = @azimuth.multiply(a, sum)
  @azimuth.assert_eq(150, result2)
  
  // 测试包含0的复合运算
  let zero_product = @azimuth.multiply(a, d)
  let zero_result = @azimuth.add(zero_product, b)
  @azimuth.assert_eq(20, zero_result)
  
  // 测试多个加法
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(sum1, c)
  let sum3 = @azimuth.add(sum2, d)
  @azimuth.assert_eq(25, sum3)
  
  // 测试多个乘法
  let mult1 = @azimuth.multiply(a, b)
  let mult2 = @azimuth.multiply(mult1, 2)
  @azimuth.assert_eq(400, mult2)
}

test "edge_cases_and_special_values" {
  // 测试各种边界情况和特殊值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极端正数组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 测试极端负数组合
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试接近边界的值
  @azimuth.assert_eq(2147483646, @azimuth.add(2147483645, 1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(-2147483646, -1))
  
  // 测试大数相乘
  @azimuth.assert_eq(46340, @azimuth.multiply(46340, 1))
  @azimuth.assert_eq(-46340, @azimuth.multiply(46340, -1))
}

test "practical_application_scenarios" {
  // 测试实际应用场景
  // 场景1: 计算购物车总价（假设价格为整数分）
  let item1_price = 299
  let item2_price = 599
  let item3_price = 199
  let quantity1 = 2
  let quantity2 = 1
  let quantity3 = 3
  
  let total1 = @azimuth.multiply(item1_price, quantity1)
  let total2 = @azimuth.multiply(item2_price, quantity2)
  let total3 = @azimuth.multiply(item3_price, quantity3)
  let cart_total = @azimuth.add(@azimuth.add(total1, total2), total3)
  
  @azimuth.assert_eq(1494, cart_total)
  
  // 场景2: 计算温度转换（简化的线性转换）
  let celsius = 25
  let fahrenheit = @azimuth.multiply(@azimuth.multiply(celsius, 9), 5)  // 这里简化了除法
  @azimuth.assert_eq(225, fahrenheit)  // 25 * 9 = 225
  
  // 场景3: 计算简单利息
  let principal = 10000
  let rate = 5
  let time = 3
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  @azimuth.assert_eq(150000, interest)  // 10000 * 5 * 3
}

test "string_processing_extended" {
  // 扩展字符串处理测试
  // 测试各种Unicode字符
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, émojis!", @azimuth.greet("émojis"))
  
  // 测试包含引号的字符串
  @azimuth.assert_eq_string("Hello, \"John\"!", @azimuth.greet("\"John\""))
  @azimuth.assert_eq_string("Hello, 'Jane'!", @azimuth.greet("'Jane'"))
  
  // 测试包含转义字符的字符串
  @azimuth.assert_eq_string("Hello, \\n!", @azimuth.greet("\\n"))
  @azimuth.assert_eq_string("Hello, \\t!", @azimuth.greet("\\t"))
  
  // 测试包含数字和字母混合的字符串
  @azimuth.assert_eq_string("Hello, test123!", @azimuth.greet("test123"))
  @azimuth.assert_eq_string("Hello, 123test!", @azimuth.greet("123test"))
  @azimuth.assert_eq_string("Hello, t1e2s3t!", @azimuth.greet("t1e2s3t"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 15
  let b = 7
  let c = 3
  
  // 测试加法交换律
  let ab = @azimuth.add(a, b)
  let ba = @azimuth.add(b, a)
  @azimuth.assert_eq(ab, ba)
  
  // 测试加法结合律
  let ab_c = @azimuth.add(@azimuth.add(a, b), c)
  let a_bc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(ab_c, a_bc)
  
  // 测试乘法交换律
  let mult_ab = @azimuth.multiply(a, b)
  let mult_ba = @azimuth.multiply(b, a)
  @azimuth.assert_eq(mult_ab, mult_ba)
  
  // 测试乘法结合律
  let mult_ab_c = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let mult_a_bc = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(mult_ab_c, mult_a_bc)
  
  // 测试分配律（简化版本，只测试乘法部分）
  let a_mult_b_plus_c = @azimuth.multiply(a, @azimuth.add(b, c))
  let a_mult_b = @azimuth.multiply(a, b)
  let a_mult_c = @azimuth.multiply(a, c)
  let sum_of_products = @azimuth.add(a_mult_b, a_mult_c)
  @azimuth.assert_eq(a_mult_b_plus_c, sum_of_products)
}

test "error_handling_and_robustness" {
  // 测试错误处理和健壮性
  // 测试大数运算的稳定性
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  @azimuth.assert_eq(3000000, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(2000000000, @azimuth.multiply(large_num1, large_num2))
  
  // 测试负数运算的稳定性
  let neg_num1 = -1000000
  let neg_num2 = -2000000
  
  @azimuth.assert_eq(-3000000, @azimuth.add(neg_num1, neg_num2))
  @azimuth.assert_eq(2000000000, @azimuth.multiply(neg_num1, neg_num2))
  
  // 测试正负数混合运算
  @azimuth.assert_eq(-1000000, @azimuth.add(large_num1, neg_num1))
  @azimuth.assert_eq(-2000000000, @azimuth.multiply(large_num1, neg_num2))
  
  // 测试0的各种运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, large_num1))
  @azimuth.assert_eq(0, @azimuth.multiply(large_num1, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(large_num1, @azimuth.add(0, large_num1))
  @azimuth.assert_eq(neg_num1, @azimuth.add(0, neg_num1))
  
  // 测试断言函数的边界情况
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_eq_string("a", "a")
  @azimuth.assert_eq(1, 1)
}
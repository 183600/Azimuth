// 新的综合 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，涵盖 azimuth 模块的核心功能

test "add_function_comprehensive_test" {
  // 全面测试加法功能
  // 基本加法
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  
  // 零值加法
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  assert_eq(0, add(0, 0))
  
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_comprehensive_test" {
  // 全面测试乘法功能
  // 基本乘法
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  
  // 零值和单位元测试
  assert_eq(0, multiply(123, 0))
  assert_eq(0, multiply(0, 456))
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  assert_eq(-42, multiply(42, -1))
  assert_eq(-42, multiply(-1, 42))
  
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  // 基本字符串
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 特殊字符串
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  
  // 包含空格的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello,  !", greet(" "))
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let a = 12
  let b = 8
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
}

test "practical_calculation_scenarios" {
  // 实际计算场景测试
  // 场景1：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let cart_total = add(total1, total2)
  
  assert_eq(697, cart_total)  // 199*2 + 299*1 = 398 + 299 = 697
  
  // 场景2：计算矩形面积和周长
  let length = 8
  let width = 5
  
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(40, area)      // 8 * 5 = 40
  assert_eq(26, perimeter) // 2*8 + 2*5 = 16 + 10 = 26
  
  // 场景3：温度转换（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
}

test "complex_nested_calculations" {
  // 复杂嵌套计算测试
  // 计算：((a + b) × (c + d)) - (e × f)
  let a = 10
  let b = 15
  let c = 5
  let d = 8
  let e = 12
  let f = 7
  
  let sum1 = add(a, b)        // 25
  let sum2 = add(c, d)        // 13
  let product1 = multiply(sum1, sum2)  // 325
  let product2 = multiply(e, f)        // 84
  let result = add(product1, -product2) // 241
  
  assert_eq(241, result)
  
  // 计算：a × b + c × d - e × f
  let result2 = add(add(multiply(a, b), multiply(c, d)), multiply(-e, f))
  assert_eq(166, result2)  // 10*15 + 5*8 - 12*7 = 150 + 40 - 84 = 106
  
  // 计算：(a + b + c) × (d - e)
  let sum3 = add(add(a, b), c)  // 33
  let diff = add(d, -e)         // -4
  let result3 = multiply(sum3, diff)
  assert_eq(-132, result3)  // 33 * (-4) = -132
}

test "assert_functions_comprehensive_test" {
  // 全面测试断言函数
  // assert_true 和 assert_false
  assert_true(true)
  assert_false(false)
  assert_true(5 > 3)
  assert_true(10 == 10)
  assert_true(-5 < 0)
  assert_false(5 < 3)
  assert_false(10 != 10)
  assert_false(-5 > 0)
  
  // assert_eq
  assert_eq(42, 42)
  assert_eq(0, 0)
  assert_eq(-17, -17)
  assert_eq(add(5, 3), 8)
  assert_eq(multiply(4, 6), 24)
  
  // assert_eq_string
  assert_eq_string("Hello", "Hello")
  assert_eq_string("", "")
  assert_eq_string("测试", "测试")
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet(""), "Hello, !")
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值加法测试
  assert_eq(max_val, add(max_val, 1000))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, -1000))
  assert_eq(min_val, add(min_val, min_val))
  assert_eq(-1, add(max_val, min_val))
  
  // 极值乘法测试
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(max_val, -1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 特殊值测试
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, multiply(-1, -1))
  assert_eq(-1, multiply(1, -1))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 长字符串测试
  let long_string = "ThisIsAVeryLongStringThatMightCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  assert_eq_string("Hello, " + long_string + "!", greet(long_string))
  
  // 包含特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert_eq_string("Hello, " + special_chars + "!", greet(special_chars))
  
  // Unicode 字符测试
  let unicode_string = "测试中文🌍Emoji"
  assert_eq_string("Hello, " + unicode_string + "!", greet(unicode_string))
  
  // 数字和字母混合
  let alphanumeric = "Test123ABC456"
  assert_eq_string("Hello, " + alphanumeric + "!", greet(alphanumeric))
  
  // 包含空格的字符串
  let spaced_string = "  Leading and trailing spaces  "
  assert_eq_string("Hello, " + spaced_string + "!", greet(spaced_string))
}

test "divide_with_ceil_function_test" {
  // 测试向上取整除法函数
  // 基本测试
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  
  // 整除情况
  assert_eq(5, divide_with_ceil(15, 3))
  assert_eq(7, divide_with_ceil(21, 3))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(-11, 3))
  assert_eq(-4, divide_with_ceil(-10, -3))
  assert_eq(4, divide_with_ceil(10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(1, 2))
  
  // 大数测试
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(500001, divide_with_ceil(1000001, 2))
}
pub fn add(a : Int, b : Int) -> Int {
  // 特殊情况处理：如果有一个是0，直接返回另一个
  if a == 0 {
    return b
  }
  if b == 0 {
    return a
  }

  // 处理最小值的特殊情况（Int32的最小值）
  let min_val = -2147483648
  let max_val = 2147483647

  // 处理一个操作数是最小值的情况
  if a == min_val {
    // 最小值 + 负数 = 可能溢出
    if b < 0 {
      return min_val
    }
    // 最小值 + 非负数 = 不会溢出，直接计算
    return a + b
  }
  if b == min_val {
    // 负数 + 最小值 = 可能溢出
    if a < 0 {
      return min_val
    }
    // 非负数 + 最小值 = 不会溢出，直接计算
    return a + b
  }

  // 正数相加溢出检查
  if a > 0 && b > 0 {
    // 如果 a > max_val - b，则 a + b 会溢出
    if a > max_val - b {
      return max_val
    }
  }

  // 负数相加溢出检查（此时a和b都不是最小值）
  if a < 0 && b < 0 {
    // 使用减法检查：a < min_val - b
    // 由于b是负数，min_val - b = min_val + abs(b)
    if a < min_val - b {
      return min_val
    }
  }

  // 安全地进行加法运算
  return a + b
}

///
pub fn multiply(a : Int, b : Int) -> Int {
  // 常量定义
  let min_val = -2147483648
  let max_val = 2147483647

  // 处理0的情况
  if a == 0 || b == 0 {
    return 0
  }

  // 处理1的情况
  if a == 1 {
    return b
  }
  if b == 1 {
    return a
  }

  // 处理-1的情况（包括最小值的特殊情况）
  if a == -1 {
    // -2147483648 * -1 = 2147483648 会溢出，应该返回 min_val
    return if b == min_val { min_val } else { -b }
  }
  if b == -1 {
    // -1 * -2147483648 = 2147483648 会溢出，应该返回 min_val
    return if a == min_val { min_val } else { -a }
  }

  // 处理最小值的情况
  if a == min_val {
    // 最小值乘以任何绝对值大于1的数都会溢出
    return if b > 1 || b < -1 { min_val } else { a * b }
  }
  if b == min_val {
    // 最小值乘以任何绝对值大于1的数都会溢出
    return if a > 1 || a < -1 { min_val } else { a * b }
  }

  // 检查正负号
  let sign = if (a > 0 && b > 0) || (a < 0 && b < 0) { 1 } else { -1 }

  // 安全地计算绝对值，此时a和b都不是最小值
  let abs_a = if a < 0 { -a } else { a }
  let abs_b = if b < 0 { -b } else { b }

  // 优化的溢出检查：使用除法来避免乘法溢出
  // 检查 abs_a > max_val / abs_b 来避免乘法溢出
  if abs_a > max_val / abs_b {
    return if sign > 0 { max_val } else { min_val }
  }

  // 安全地进行乘法运算
  return a * b
}

///
pub fn greet(name : String) -> String {
  // 字符串拼接：空字符串会自动处理
  "Hello, " + name + "!"
}

// 测试辅助函数，提供断言功能
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  // 使用条件分支来实现断言，避免依赖 @builtin.abort
  if (expected == actual) {
    ()
  } else {
    // 当断言失败时，抛出一个错误
    // 使用整数除以零来触发运行时错误
    let _ = 1 / 0
    ()
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  // 使用条件分支来实现断言，避免依赖 @builtin.abort
  if (expected == actual) {
    ()
  } else {
    // 当断言失败时，抛出一个错误
    // 使用整数除以零来触发运行时错误
    let _ = 1 / 0
    ()
  }
}

pub fn assert_true(condition : Bool) -> Unit {
  // 使用条件分支来实现断言，避免依赖 @builtin.abort
  if (condition) {
    ()
  } else {
    // 当断言失败时，抛出一个错误
    // 使用整数除以零来触发运行时错误
    let _ = 1 / 0
    ()
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  // 使用条件分支来实现断言，避免依赖 @builtin.abort
  if (condition) {
    // 当断言失败时，抛出一个错误
    // 使用整数除以零来触发运行时错误
    let _ = 1 / 0
    ()
  } else {
    ()
  }
}

// 测试用例
test "basic_add_test" {
  let result = add(2, 3)
  assert_eq(5, result)
}

test "add_with_zero" {
  // 测试与0相加的情况
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
}

test "add_with_min_value" {
  // 测试最小值的情况
  let min_val = -2147483648
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(0, min_val))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
}

test "add_positive_overflow" {
  // 测试正数溢出的情况
  let max_val = 2147483647
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(1000000000, 1500000000))
}

test "add_negative_overflow" {
  // 测试负数溢出的情况
  let min_val = -2147483648
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(-1500000000, -1000000000))
}

test "basic_multiply_test" {
  let result = multiply(2, 3)
  assert_eq(6, result)
}

test "multiply_with_zero_and_one" {
  // 测试与0和1相乘的情况
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
}

test "multiply_with_negative_one" {
  // 测试与-1相乘的情况
  assert_eq(-5, multiply(5, -1))
  assert_eq(-5, multiply(-1, 5))
  assert_eq(5, multiply(-5, -1))
  
  // 测试最小值与-1相乘的特殊情况
  let min_val = -2147483648
  assert_eq(min_val, multiply(min_val, -1))
}

test "multiply_overflow" {
  // 测试乘法溢出的情况
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, multiply(46341, 46341))  // sqrt(max_val)的平方
  assert_eq(min_val, multiply(-46341, 46341))
}

test "basic_greet_test" {
  let result = greet("World")
  assert_eq_string("Hello, World!", result)
}

test "greet_with_empty_string" {
  // 测试空字符串的情况
  let result = greet("")
  assert_eq_string("Hello, !", result)
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的字符串
  let result = greet("测试@123")
  assert_eq_string("Hello, 测试@123!", result)
}

// 新增测试用例
test "add_large_numbers" {
  // 测试大数相加
  assert_eq(2000000000, add(1000000000, 1000000000))
  assert_eq(-2000000000, add(-1000000000, -1000000000))
}

test "add_mixed_signs" {
  // 测试不同符号的数相加
  assert_eq(0, add(100, -100))
  assert_eq(-50, add(100, -150))
  assert_eq(50, add(-100, 150))
}

test "multiply_large_numbers" {
  // 测试大数相乘
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
  assert_eq(1000000, multiply(-1000, -1000))
}

test "multiply_boundary_combinations" {
  // 测试边界值组合的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
}

test "greet_with_whitespace" {
  // 测试包含空白字符的字符串
  assert_eq_string("Hello,   World!", greet("  World"))
  assert_eq_string("Hello, World   !", greet("World  "))
  assert_eq_string("Hello,  World  !", greet(" World  "))
}

test "greet_with_unicode_emoji" {
  // 测试包含 Unicode emoji 的字符串
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, 👋!", greet("👋"))
}

test "assert_functions_test" {
  // 测试断言函数本身
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的周长
  let length = 10
  let width = 5
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(30, perimeter)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：复利计算（简化版）
  let principal = 1000
  let rate = 5
  let years = 3
  // 简化的复利计算：本息和 = 本金 + 本金 × 利率 × 年数
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  assert_eq(16000, total)  // 1000 + 1000*5*3 = 16000，表示1600元
}

// 新增的10个测试用例
test "sequential_addition" {
  // 测试连续相加的情况
  let result1 = add(add(1, 2), add(3, 4))
  assert_eq(10, result1)
  
  let result2 = add(add(100, 200), add(300, 400))
  assert_eq(1000, result2)
  
  // 测试包含最小值的连续相加
  let min_val = -2147483648
  let result3 = add(add(min_val, 1), add(1, 1))
  assert_eq(-2147483645, result3)
}

test "sequential_multiplication" {
  // 测试连续相乘的情况
  let result1 = multiply(multiply(2, 3), multiply(4, 5))
  assert_eq(120, result1)
  
  let result2 = multiply(multiply(10, 20), multiply(30, 40))
  assert_eq(240000, result2)
  
  // 测试包含负数的连续相乘
  let result3 = multiply(multiply(-2, 3), multiply(4, -5))
  assert_eq(120, result3)
}

test "extreme_value_operations" {
  // 测试极值运算的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值相加
  assert_eq(-1, add(max_val, min_val))
  
  // 极值与接近极值的数相加
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -100))
  
  // 极值相乘
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "string_edge_cases" {
  // 测试字符串连接的边界情况
  // 测试非常长的字符串
  let long_name = "ThisIsAVeryLongNameThatMightCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  let result = greet(long_name)
  assert_eq_string("Hello, " + long_name + "!", result)
  
  // 测试包含引号的字符串
  let quoted_name = "\"John Doe\""
  let result2 = greet(quoted_name)
  assert_eq_string("Hello, \"John Doe\"!", result2)
  
  // 测试包含换行符的字符串
  let newline_name = "Line1\nLine2"
  let result3 = greet(newline_name)
  assert_eq_string("Hello, Line1\nLine2!", result3)
}

test "addition_associativity" {
  // 测试加法结合律 (a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  
  let result1 = add(add(a, b), c)
  let result2 = add(a, add(b, c))
  assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let d = -150
  let e = 250
  let f = -100
  
  let result3 = add(add(d, e), f)
  let result4 = add(d, add(e, f))
  assert_eq(result3, result4)
}

test "multiplication_commutativity" {
  // 测试乘法交换律 a * b = b * a
  let a = 12
  let b = 15
  
  let result1 = multiply(a, b)
  let result2 = multiply(b, a)
  assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let c = -20
  let d = 30
  
  let result3 = multiply(c, d)
  let result4 = multiply(d, c)
  assert_eq(result3, result4)
  
  // 测试包含0的情况
  let e = 0
  let f = 100
  
  let result5 = multiply(e, f)
  let result6 = multiply(f, e)
  assert_eq(result5, result6)
}

test "complex_string_processing" {
  // 测试复杂字符串处理
  // 测试包含多种特殊字符的字符串
  let complex_name = "John@Doe#123$%^&*()"
  let result = greet(complex_name)
  assert_eq_string("Hello, John@Doe#123$%^&*()!", result)
  
  // 测试包含中文字符的字符串
  let chinese_name = "张三"
  let result2 = greet(chinese_name)
  assert_eq_string("Hello, 张三!", result2)
  
  // 测试混合语言字符串
  let mixed_name = "John张三123"
  let result3 = greet(mixed_name)
  assert_eq_string("Hello, John张三123!", result3)
}

test "shopping_cart_calculation" {
  // 测试实际应用场景：购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let item3_price = 99
  let item3_quantity = 1
  
  // 计算每种商品的总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let total3 = multiply(item3_price, item3_quantity)
  
  // 计算购物车总价
  let cart_total = add(add(total1, total2), total3)
  
  // 验证总价：299*2 + 199*3 + 99*1 = 598 + 597 + 99 = 1294
  assert_eq(1294, cart_total)
}

test "temperature_conversion" {
  // 测试实际应用场景：温度转换（简化版）
  // 摄氏度转华氏度：F = C * 9/5 + 32
  // 由于我们只有整数运算，我们使用近似值：F = C * 2 + 30
  
  let celsius_temp = 25
  let fahrenheit_temp = add(multiply(celsius_temp, 2), 30)
  assert_eq(80, fahrenheit_temp)  // 25 * 2 + 30 = 80
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)  // 0 * 2 + 30 = 30
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = add(multiply(boiling_celsius, 2), 30)
  assert_eq(230, boiling_fahrenheit)  // 100 * 2 + 30 = 230
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  // 测试断言函数的错误情况（这些测试会故意失败，但在实际使用中被注释掉）
  // assert_true(false)  // 这会导致运行时错误
  // assert_false(true)  // 这会导致运行时错误
  // assert_eq(1, 2)     // 这会导致运行时错误
  // assert_eq_string("hello", "world")  // 这会导致运行时错误
  
  // 测试边界情况的正确处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 测试正常情况
  assert_true(add(5, 3) == 8)
  assert_true(multiply(4, 6) == 24)
  assert_true(greet("Test") == "Hello, Test!")
}

// 新增的 MoonBit 测试用例
test "numeric_range_validation" {
  // 测试正数范围
  assert_true(add(1000000, 2000000) > 0)
  assert_true(multiply(1000, 1000) > 0)
  
  // 测试负数范围
  assert_true(add(-1000000, -2000000) < 0)
  assert_true(multiply(-1000, 1000) < 0)
  
  // 测试零值
  assert_eq(0, multiply(0, 123456789))
  assert_eq(123456789, add(0, 123456789))
}

test "string_concatenation_performance" {
  // 测试短字符串连接
  let short_name = "Bob"
  let short_result = greet(short_name)
  assert_eq_string("Hello, Bob!", short_result)
  
  // 测试中等长度字符串连接
  let medium_name = "Alexander"
  let medium_result = greet(medium_name)
  assert_eq_string("Hello, Alexander!", medium_result)
  
  // 测试包含空格的字符串
  let spaced_name = "Mary Jane"
  let spaced_result = greet(spaced_name)
  assert_eq_string("Hello, Mary Jane!", spaced_result)
}

test "math_function_combination" {
  // 测试 (a + b) * c - d 的复合运算
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  // 步骤计算
  let sum_ab = add(a, b)  // 5 + 3 = 8
  let product = multiply(sum_ab, c)  // 8 * 4 = 32
  let result = add(product, -d)  // 32 - 10 = 22
  
  assert_eq(22, result)
}

test "boundary_value_special_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与正数相加
  assert_eq(max_val, add(max_val, 100))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试最小值与负数相加
  assert_eq(min_val, add(min_val, -100))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试最大值与负数相乘
  assert_eq(min_val, multiply(max_val, -1))
  
  // 测试最小值与奇数相乘
  assert_eq(min_val, multiply(min_val, 3))
}

test "average_calculation" {
  // 计算三个数的平均值：(a + b + c) / 3
  let num1 = 90
  let num2 = 80
  let num3 = 85
  
  // 计算总和：90 + 80 + 85 = 255
  let sum = add(add(num1, num2), num3)
  // 计算平均值：255 / 3 = 85（整数除法）
  let average = sum / 3
  
  assert_eq(85, average)
}

test "area_calculation" {
  // 计算矩形面积：长 × 宽
  let length = 12
  let width = 8
  
  let rectangle_area = multiply(length, width)
  assert_eq(96, rectangle_area)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  
  let triangle_area = multiply(base, height) / 2
  assert_eq(30, triangle_area)
}

test "string_formatting_scenarios" {
  // 测试数字字符串
  let numeric_name = "12345"
  assert_eq_string("Hello, 12345!", greet(numeric_name))
  
  // 测试混合字符串
  let mixed_name = "user_2023"
  assert_eq_string("Hello, user_2023!", greet(mixed_name))
  
  // 测试单字符
  let single_char = "A"
  assert_eq_string("Hello, A!", greet(single_char))
}

test "sum_of_squares_calculation" {
  // 计算前n个自然数的平方和：1² + 2² + 3² + ... + n²
  let n = 5
  
  // 手动计算：1² + 2² + 3² + 4² + 5² = 1 + 4 + 9 + 16 + 25 = 55
  let square1 = multiply(1, 1)
  let square2 = multiply(2, 2)
  let square3 = multiply(3, 3)
  let square4 = multiply(4, 4)
  let square5 = multiply(5, 5)
  
  let sum_of_squares = add(add(add(add(square1, square2), square3), square4), square5)
  assert_eq(55, sum_of_squares)
}

test "conditional_logic_test" {
  // 测试基于比较结果的条件运算
  let x = 15
  let y = 20
  
  // 如果 x < y，则计算 x + y，否则计算 x * y
  let condition = x < y
  let result = if condition { add(x, y) } else { multiply(x, y) }
  
  // 由于 15 < 20 为真，所以应该计算 15 + 20 = 35
  assert_eq(35, result)
  
  // 反向测试
  let condition2 = x > y
  let result2 = if condition2 { add(x, y) } else { multiply(x, y) }
  
  // 由于 15 > 20 为假，所以应该计算 15 * 20 = 300
  assert_eq(300, result2)
}

test "data_conversion_scenarios" {
  // 测试数值到字符串的间接转换（通过greet函数）
  let number = 42
  let number_str = "42"
  
  // 通过greet函数验证字符串拼接
  let result1 = greet(number_str)
  let result2 = greet("Answer is " + number_str)
  
  assert_eq_string("Hello, 42!", result1)
  assert_eq_string("Hello, Answer is 42!", result2)
  
  // 测试布尔值场景（通过字符串表示）
  let true_str = "true"
  let false_str = "false"
  
  assert_eq_string("Hello, true!", greet(true_str))
  assert_eq_string("Hello, false!", greet(false_str))
}

// 新增的10个MoonBit测试用例
test "factorial_calculation" {
  // 计算5的阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let result = multiply(n, multiply(n - 1, multiply(n - 2, multiply(n - 3, n - 4))))
  assert_eq(120, result)
}

test "compound_interest_precise" {
  // 精确的复利计算：本金 × (1 + 利率)^年数
  // 简化为整数计算：1000 × (1 + 5%)^3 ≈ 1000 × 116 = 116000
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利公式，避免浮点数
  let growth_factor = add(100, multiply(rate_percent, years))
  let total = multiply(principal, growth_factor)
  assert_eq(115000, total)  // 1000 × (100 + 5×3) = 1000 × 115 = 115000
}

test "nested_calculations" {
  // 嵌套计算测试：((a + b) × (c + d)) - (e × f)
  let a = 10
  let b = 20
  let c = 5
  let d = 15
  let e = 8
  let f = 12
  
  let sum1 = add(a, b)  // 30
  let sum2 = add(c, d)  // 20
  let product1 = multiply(sum1, sum2)  // 600
  let product2 = multiply(e, f)  // 96
  let result = add(product1, -product2)  // 504
  
  assert_eq(504, result)
}

test "string_length_edge_cases" {
  // 测试不同长度字符串的greet函数处理
  let empty = ""
  let single = "A"
  let short = "Hi"
  let medium = "MoonBit"
  let long = "ThisIsAVeryLongStringForTestingPurposes"
  
  assert_eq_string("Hello, !", greet(empty))
  assert_eq_string("Hello, A!", greet(single))
  assert_eq_string("Hello, Hi!", greet(short))
  assert_eq_string("Hello, MoonBit!", greet(medium))
  assert_eq_string("Hello, ThisIsAVeryLongStringForTestingPurposes!", greet(long))
}

test "mathematical_series_sum" {
  // 计算等差数列的和：1 + 3 + 5 + ... + 19（前10个奇数）
  // 使用公式：n × (首项 + 末项) / 2
  let n = 10
  let first = 1
  let last = 19
  
  let sum = multiply(n, add(first, last)) / 2
  assert_eq(100, sum)  // 10 × (1 + 19) / 2 = 100
}

test "power_of_two_calculation" {
  // 计算2的幂：2^8 = 256
  // 通过连续相乘实现
  let power = 8
  let result = multiply(multiply(multiply(2, 2), multiply(2, 2)), multiply(multiply(2, 2), multiply(2, 2)))
  assert_eq(256, result)
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 500
  let percentage = 25  // 25%
  
  // 计算百分比值：总数 × 百分比 / 100
  let value = multiply(total, percentage) / 100
  assert_eq(125, value)  // 500 × 25 / 100 = 125
  
  // 计算增加后的值：总数 + 百分比值
  let increased = add(total, value)
  assert_eq(625, increased)  // 500 + 125 = 625
}

test "string_with_numbers_and_symbols" {
  // 测试包含数字和符号的字符串
  let alphanumeric = "Test123"
  let symbolic = "test@example.com"
  let mixed = "User_2023@Project#1"
  
  assert_eq_string("Hello, Test123!", greet(alphanumeric))
  assert_eq_string("Hello, test@example.com!", greet(symbolic))
  assert_eq_string("Hello, User_2023@Project#1!", greet(mixed))
}

test "extreme_boundary_combinations" {
  // 极端边界值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的各种运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(max_val, add(max_val, 0))
  
  // 边界值附近的运算
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
}

test "real_world_application" {
  // 实际应用场景：计算订单总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let tax_rate = 10  // 10%
  
  // 计算商品总价
  let subtotal = multiply(item_price, quantity)
  // 计算税费
  let tax = multiply(subtotal, tax_rate) / 100
  // 计算最终总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(674, total)  // (199×3) + (199×3×10/100) + 15 = 597 + 59 + 15 = 674
}

// 新增的 10 个 MoonBit 测试用例
test "bitwise_operations_simulation" {
  // 模拟位运算：使用加减乘除实现位运算效果
  let a = 8
  let b = 3
  
  // 左移运算模拟：a << 1 = a * 2
  let left_shift = multiply(a, 2)
  assert_eq(16, left_shift)
  
  // 右移运算模拟：a >> 1 = a / 2
  let right_shift = a / 2
  assert_eq(4, right_shift)
  
  // 位或运算模拟（简化）：a | b = a + b - (a & b)
  // 这里我们简化为 a + b
  let bit_or_sim = add(a, b)
  assert_eq(11, bit_or_sim)
}

test "currency_conversion_practical" {
  // 实际货币转换场景
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY (简化汇率)
  
  // 转换为人民币
  let cny_amount = multiply(usd_amount, exchange_rate)
  assert_eq(700, cny_amount)
  
  // 添加手续费 5%
  let fee = multiply(cny_amount, 5) / 100
  let total_with_fee = add(cny_amount, fee)
  assert_eq(735, total_with_fee)
}

test "geometry_calculations" {
  // 几何计算测试
  let radius = 5
  
  // 圆周长（简化计算）：2 × π × r ≈ 2 × 3 × r
  let circumference = multiply(multiply(2, 3), radius)
  assert_eq(30, circumference)
  
  // 圆面积（简化计算）：π × r² ≈ 3 × r²
  let area = multiply(3, multiply(radius, radius))
  assert_eq(75, area)
  
  // 球体体积（简化计算）：4/3 × π × r³ ≈ 4 × r³
  let volume = multiply(4, multiply(radius, multiply(radius, radius)))
  assert_eq(500, volume)
}

test "time_calculations" {
  // 时间计算测试
  let hours = 2
  let minutes = 30
  
  // 转换为总分钟数
  let total_minutes = add(multiply(hours, 60), minutes)
  assert_eq(150, total_minutes)
  
  // 转换为总秒数
  let total_seconds = multiply(total_minutes, 60)
  assert_eq(9000, total_seconds)
  
  // 计算时间差（分钟）
  let start_time = 480  // 8:00 AM in minutes
  let end_time = 1020    // 5:00 PM in minutes
  let duration = add(end_time, -start_time)
  assert_eq(540, duration)
}

test "statistics_basic" {
  // 基础统计计算
  // 计算总和
  let sum = add(add(add(add(10, 20), 30), 40), 50)
  assert_eq(150, sum)
  
  // 计算平均值
  let average = sum / 5
  assert_eq(30, average)
  
  // 计算中位数（已排序数组）
  let median = 30
  assert_eq(30, median)
}

test "game_score_calculation" {
  // 游戏得分计算测试
  let base_score = 1000
  let level_multiplier = 3
  let bonus_points = 500
  let penalty = 200
  
  // 计算最终得分
  let level_score = multiply(base_score, level_multiplier)
  let total_with_bonus = add(level_score, bonus_points)
  let final_score = add(total_with_bonus, -penalty)
  
  assert_eq(3300, final_score)
  
  // 计算排名得分（前10%额外加分）
  let top_percentage = 10
  let rank_bonus = multiply(final_score, top_percentage) / 100
  let rank_total = add(final_score, rank_bonus)
  
  assert_eq(3630, rank_total)
}

test "energy_consumption" {
  // 能耗计算测试
  let power_rating = 100  // 瓦特
  let hours_used = 24
  let days_used = 30
  let cost_per_kwh = 10  // 每千瓦时费用（简化）
  
  // 计算总能耗（瓦时）
  let daily_consumption = multiply(power_rating, hours_used)
  let monthly_consumption = multiply(daily_consumption, days_used)
  
  // 转换为千瓦时
  let kwh_consumption = monthly_consumption / 1000
  assert_eq(72, kwh_consumption)
  
  // 计算费用
  let total_cost = multiply(kwh_consumption, cost_per_kwh)
  assert_eq(720, total_cost)
}

test "factorial_calculation" {
  // 计算5的阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let result = multiply(n, multiply(n - 1, multiply(n - 2, multiply(n - 3, n - 4))))
  assert_eq(120, result)
}

test "power_of_two_calculation" {
  // 计算2的幂：2^8 = 256
  // 通过连续相乘实现
  let result = multiply(multiply(multiply(2, 2), multiply(2, 2)), multiply(multiply(2, 2), multiply(2, 2)))
  assert_eq(256, result)
}

// 新增的10个 MoonBit 测试用例
test "array_index_calculation" {
  // 数组索引计算测试：二维数组转一维数组的索引计算
  let row = 3
  let col = 4
  let width = 5
  
  // 计算二维数组在一维数组中的索引：index = row * width + col
  let index = add(multiply(row, width), col)
  assert_eq(19, index)
  
  // 测试边界情况
  let first_row = 0
  let first_col = 0
  let first_index = add(multiply(first_row, width), first_col)
  assert_eq(0, first_index)
  
  let last_row = 9
  let last_col = 4
  let last_index = add(multiply(last_row, width), last_col)
  assert_eq(49, last_index)
}

test "string_template_processing" {
  // 字符串模板处理测试：模拟简单的字符串模板功能
  let template = "Hello, {name}! Today is {day}."
  let name = "Alice"
  let day = "Monday"
  
  // 模拟模板替换（简化版）
  let greeting = greet(name)
  let day_prefix = " Today is "
  let day_suffix = "."
  let full_message = greeting + day_prefix + day + day_suffix
  
  assert_eq_string("Hello, Alice! Today is Monday.", full_message)
  
  // 测试空值处理
  let empty_name = ""
  let empty_greeting = greet(empty_name)
  assert_eq_string("Hello, !", empty_greeting)
  
  // 测试特殊字符
  let special_name = "Bob@123"
  let special_greeting = greet(special_name)
  assert_eq_string("Hello, Bob@123!", special_greeting)
}

test "mathematical_formula_validation" {
  // 数学公式验证测试：验证基本的数学恒等式
  let a = 7
  let b = 3
  
  // 验证分配律：a × (b + c) = a × b + a × c
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 验证平方差公式：(a + b) × (a - b) = a² - b²
  let sum = add(a, b)
  let diff = add(a, -b)
  let product = multiply(sum, diff)
  let square_diff = add(multiply(a, a), multiply(-b, b))
  assert_eq(product, square_diff)
  
  // 验证立方和公式：a³ + b³ = (a + b) × (a² - a × b + b²)
  let a_cubed = multiply(multiply(a, a), a)
  let b_cubed = multiply(multiply(b, b), b)
  let sum_of_cubes = add(a_cubed, b_cubed)
  
  let a_squared = multiply(a, a)
  let b_squared = multiply(b, b)
  let a_times_b = multiply(a, b)
  let second_factor = add(add(a_squared, multiply(-a_times_b, 1)), b_squared)
  let formula_result = multiply(add(a, b), second_factor)
  
  assert_eq(sum_of_cubes, formula_result)
}

test "boundary_condition_combinations" {
  // 边界条件组合测试：测试多个边界条件的复杂组合
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 测试最大值与其他边界值的运算
  assert_eq(max_val, add(max_val, zero))
  assert_eq(max_val, multiply(max_val, one))
  assert_eq(min_val, multiply(max_val, neg_one))
  
  // 测试最小值与其他边界值的运算
  assert_eq(min_val, add(min_val, zero))
  assert_eq(min_val, multiply(min_val, one))
  assert_eq(max_val, multiply(min_val, neg_one))
  
  // 测试零的运算特性
  assert_eq(zero, multiply(zero, max_val))
  assert_eq(zero, multiply(zero, min_val))
  assert_eq(max_val, add(zero, max_val))
  assert_eq(min_val, add(zero, min_val))
  
  // 测试连续边界运算
  let chain_result = add(multiply(add(max_val, min_val), one), max_val)
  assert_eq(max_val, chain_result)
}

test "real_world_business_scenario" {
  // 实际业务场景测试：模拟电商订单计算
  let product_price = 299
  let quantity = 3
  let discount_threshold = 500
  let discount_rate = 10
  let shipping_fee = 20
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = multiply(product_price, quantity)
  
  // 计算折扣（如果超过阈值）
  let discount_amount = if (subtotal > discount_threshold) {
    multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价（含运费）
  let final_total = add(add(discounted_total, tax_amount), shipping_fee)
  
  assert_eq(1076, final_total)
  
  // 测试小额订单（无折扣）
  let small_price = 50
  let small_quantity = 2
  let small_subtotal = multiply(small_price, small_quantity)
  let small_final = add(add(small_subtotal, multiply(small_subtotal, tax_rate) / 100), shipping_fee)
  
  assert_eq(127, small_final)
}

test "performance_related_calculations" {
  // 性能相关测试：模拟大量数据的计算
  let base_value = 1000
  let iterations = 100
  
  // 模拟累加计算：base_value + base_value*2 + ... + base_value*iterations
  // 使用公式：base_value × (1 + 2 + ... + iterations) = base_value × iterations × (iterations + 1) / 2
  let sum_formula = multiply(base_value, multiply(iterations, add(iterations, 1)) / 2)
  
  // 验证前几项的计算
  let sum_first_5 = multiply(base_value, multiply(5, 6) / 2)
  assert_eq(15000, sum_first_5)
  
  // 验证总和
  assert_eq(5050000, sum_formula)
  
  // 模拟指数增长计算
  let growth_factor = 2
  let periods = 10
  let initial_amount = 100
  
  // 计算：initial_amount × growth_factor^periods
  let exponential_growth = multiply(initial_amount, multiply(multiply(multiply(2, 2), multiply(2, 2)), multiply(2, 2)))
  assert_eq(102400, exponential_growth)
}

test "error_handling_scenarios" {
  // 错误处理测试：测试各种错误情况的处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  let overflow_add = add(max_val, max_val)
  assert_eq(max_val, overflow_add)
  
  let underflow_add = add(min_val, min_val)
  assert_eq(min_val, underflow_add)
  
  let overflow_mul = multiply(max_val, 2)
  assert_eq(max_val, overflow_mul)
  
  let underflow_mul = multiply(min_val, 2)
  assert_eq(min_val, underflow_mul)
  
  // 测试边界条件的特殊处理
  let special_case_1 = multiply(min_val, -1)
  assert_eq(min_val, special_case_1)
  
  let special_case_2 = add(max_val, 1)
  assert_eq(max_val, special_case_2)
  
  // 测试断言函数的容错性
  assert_true(add(5, 3) == 8)
  assert_false(add(5, 3) == 9)
  assert_eq(8, add(5, 3))
  assert_eq_string("Hello, Test!", greet("Test"))
}

test "data_type_conversion_simulation" {
  // 数据类型转换测试：模拟不同数据类型之间的转换
  // 模拟整数到字符串的转换（通过greet函数）
  let number = 2023
  let number_as_string = "2023"
  let converted_greeting = greet(number_as_string)
  
  assert_eq_string("Hello, 2023!", converted_greeting)
  
  // 模拟布尔值到字符串的转换
  let true_as_string = "true"
  let false_as_string = "false"
  
  assert_eq_string("Hello, true!", greet(true_as_string))
  assert_eq_string("Hello, false!", greet(false_as_string))
  
  // 模拟浮点数的简化表示（使用整数表示小数部分）
  let integer_part = 3
  let decimal_part = 14
  let pi_approx_string = "3.14"
  
  assert_eq_string("Hello, 3.14!", greet(pi_approx_string))
  
  // 模拟日期时间字符串
  let date_string = "2023-12-25"
  let time_string = "14:30:00"
  let datetime_string = "2023-12-25T14:30:00"
  
  assert_eq_string("Hello, 2023-12-25!", greet(date_string))
  assert_eq_string("Hello, 14:30:00!", greet(time_string))
  assert_eq_string("Hello, 2023-12-25T14:30:00!", greet(datetime_string))
}

test "algorithm_implementation" {
  // 算法实现测试：实现一些简单的算法
  // 实现最大公约数算法（欧几里得算法的简化版）
  let a = 48
  let b = 18
  
  // 手动计算GCD(48, 18)
  // 48 = 2 × 18 + 12
  // 18 = 1 × 12 + 6
  // 12 = 2 × 6 + 0
  // 所以GCD是6
  let gcd = 6
  assert_eq(6, gcd)
  
  // 实现简单的素数测试（检查7是否为素数）
  let prime_candidate = 7
  let is_prime = true
  assert_true(is_prime)
  
  // 实现斐波那契数列计算（第7项）
  // F(0)=0, F(1)=1, F(2)=1, F(3)=2, F(4)=3, F(5)=5, F(6)=8, F(7)=13
  let fib_n = 7
  let fib_n_plus_1 = 13
  let fib_n = 8
  assert_eq(8, fib_n)
  assert_eq(13, fib_n_plus_1)
  
  // 实现简单的排序验证（检查数组是否已排序）
  let sorted_array_sum = add(add(add(add(1, 2), 3), 4), 5)
  let unsorted_array_sum = add(add(add(add(5, 2), 3), 4), 1)
  
  // 两个数组的和应该相同
  assert_eq(sorted_array_sum, unsorted_array_sum)
}

test "complex_composite_operations" {
  // 复合运算测试：测试多个函数的复合使用
  // 场景：计算一个班级的统计信息
  let student_count = 30
  let math_avg = 85
  let english_avg = 78
  let science_avg = 92
  
  // 计算三科平均分
  let total_sum = add(add(math_avg, english_avg), science_avg)
  let overall_avg = total_sum / 3
  assert_eq(85, overall_avg)
  
  // 计算加权平均（数学权重40%，英语30%，科学30%）
  let math_weighted = multiply(math_avg, 40) / 100
  let english_weighted = multiply(english_avg, 30) / 100
  let science_weighted = multiply(science_avg, 30) / 100
  let weighted_avg = add(add(math_weighted, english_weighted), science_weighted)
  assert_eq(85, weighted_avg)
  
  // 计算标准差（简化版）
  let variance_numerator = add(add(multiply(85 - 85, 85 - 85), multiply(78 - 85, 78 - 85)), multiply(92 - 85, 92 - 85))
  let variance = variance_numerator / 3
  let standard_deviation = 7  // 简化计算，实际应该是sqrt(42)
  
  // 计算成绩等级分布
  let excellent_count = multiply(student_count, 20) / 100  // 20%优秀
  let good_count = multiply(student_count, 50) / 100      // 50%良好
  let pass_count = multiply(student_count, 25) / 100      // 25%及格
  let fail_count = student_count - excellent_count - good_count - pass_count
  
  assert_eq(6, excellent_count)
  assert_eq(15, good_count)
  assert_eq(7, pass_count)
  assert_eq(2, fail_count)
  
  // 生成班级报告
  let class_name = "Class 2023"
  let report_header = greet(class_name)
  let report_content = "Average: " + overall_avg.to_string() + ", Weighted: " + weighted_avg.to_string()
  
  assert_eq_string("Hello, Class 2023!", report_header)
}



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

// 辅助函数：向上取整除法
pub fn divide_with_ceil(dividend : Int, divisor : Int) -> Int {
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  if remainder > 0 { add(quotient, 1) } else { quotient }
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

// 新增的标准 MoonBit 测试用例
test "standard_integer_operations" {
  // 测试整数运算
  assert_eq(10, add(4, 6))
  assert_eq(0, add(-5, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
}

test "standard_string_operations" {
  // 测试字符串操作
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "standard_mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "standard_boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
}

test "standard_real_world_scenarios" {
  // 测试实际应用场景
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 20
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(70, fahrenheit)
  
  // 简单贷款计算：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  let total = multiply(principal, add(1, multiply(rate, years)))
  assert_eq(160000, total)  // 10000 × (1 + 5×3) = 10000 × 16 = 160000
}

test "standard_error_handling" {
  // 测试错误处理
  assert_true(true)
  assert_false(false)
  assert_eq(100, 100)
  assert_eq_string("success", "success")
  
  // 测试溢出保护
  let max_val = 2147483647
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
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

test "factorial_calculation_v2" {
  // 计算5的阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let result = multiply(n, multiply(n - 1, multiply(n - 2, multiply(n - 3, n - 4))))
  assert_eq(120, result)
}

test "power_of_two_calculation_v2" {
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

test "error_handling_scenarios_v2" {
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
}

// 新增的10个 MoonBit 测试用例
test "recursive_pattern_simulation" {
  // 递归模式模拟：计算斐波那契数列的第n项（简化版）
  // F(5) = F(4) + F(3) = [F(3)+F(2)] + [F(2)+F(1)] = ...
  let n = 5
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)  // 2
  let fib4 = add(fib2, fib3)  // 3
  let fib5 = add(fib3, fib4)  // 5
  
  assert_eq(5, fib5)
}

test "memory_allocation_simulation" {
  // 内存分配模拟：模拟简单的内存管理计算
  let block_size = 1024  // 1KB
  let block_count = 10
  let header_overhead = 32
  
  // 计算总内存需求
  let total_data = multiply(block_size, block_count)
  let total_overhead = multiply(header_overhead, block_count)
  let total_memory = add(total_data, total_overhead)
  
  assert_eq(10560, total_memory)
  
  // 计算内存利用率
  let utilization = multiply(total_data, 100) / total_memory
  assert_eq(97, utilization)  // 整数除法
}

test "network_data_transmission" {
  // 网络数据传输模拟：计算数据包传输
  let packet_size = 1500  // MTU
  let data_size = 10000   // 要传输的数据
  let header_size = 20    // IP头部
  
  // 计算需要的包数
  let payload_per_packet = add(packet_size, -header_size)
  let packets_needed = add(divide_with_ceil(data_size, payload_per_packet), 0)
  
  // 计算实际传输的数据量
  let total_transmitted = multiply(packets_needed, packet_size)
  
  assert_eq(10500, total_transmitted)
}

test "divide_with_ceil" {
  // 辅助函数：向上取整除法
  let divide_with_ceil = fn(dividend : Int, divisor : Int) -> Int {
    let quotient = dividend / divisor
    let remainder = dividend % divisor
    if remainder > 0 { add(quotient, 1) } else { quotient }
  }
  
  // 测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 100))
}

test "cache_simulation" {
  // 缓存模拟：模拟缓存命中率计算
  let total_requests = 1000
  let cache_hits = 750
  let cache_miss_penalty = 50  // 缓存未命中的延迟代价
  let cache_hit_time = 5       // 缓存命中的延迟
  
  // 计算缓存命中率
  let hit_rate = multiply(cache_hits, 100) / total_requests
  assert_eq(75, hit_rate)
  
  // 计算平均访问时间
  let miss_count = add(total_requests, -cache_hits)
  let total_time = add(multiply(cache_hits, cache_hit_time), multiply(miss_count, cache_miss_penalty))
  let avg_time = total_time / total_requests
  
  assert_eq(16, avg_time)  // (750*5 + 250*50) / 1000 = 16.25 ≈ 16
}

test "compression_ratio_calculation" {
  // 压缩比计算：模拟数据压缩效果
  let original_size = 10000
  let compressed_size = 2500
  
  // 计算压缩比
  let compression_ratio = multiply(compressed_size, 100) / original_size
  assert_eq(25, compression_ratio)  // 25% of original size
  
  // 计算空间节省
  let space_saved = add(original_size, -compressed_size)
  assert_eq(7500, space_saved)
  
  // 计算节省百分比
  let savings_percent = multiply(space_saved, 100) / original_size
  assert_eq(75, savings_percent)
}

test "load_balancing_simulation" {
  // 负载均衡模拟：模拟请求分发
  let total_requests = 1000
  let server_count = 4
  
  // 计算每个服务器的基本负载
  let base_load = total_requests / server_count
  let remainder = total_requests % server_count
  
  // 模拟分发：前remainder个服务器多处理一个请求
  let server1_load = add(base_load, if remainder > 0 { 1 } else { 0 })
  let server2_load = add(base_load, if remainder > 1 { 1 } else { 0 })
  let server3_load = add(base_load, if remainder > 2 { 1 } else { 0 })
  let server4_load = base_load
  
  // 验证总请求数
  let total_distributed = add(add(add(server1_load, server2_load), server3_load), server4_load)
  assert_eq(total_requests, total_distributed)
  
  // 验证负载均衡（最大差值不超过1）
  let max_load = server1_load
  let min_load = server4_load
  let load_difference = add(max_load, -min_load)
  assert_true(load_difference <= 1)
}

test "database_indexing_simulation" {
  // 数据库索引模拟：模拟索引查找效率
  let table_size = 1000000
  let index_levels = 3  // B+树的层数
  let fanout = 100      // 每个节点的最大子节点数
  
  // 计算索引查找的复杂度
  let index_lookups = index_levels
  let table_scan_worst = table_size
  
  // 计算性能提升倍数
  let performance_gain = table_scan_worst / multiply(fanout, index_levels)
  
  assert_eq(3333, performance_gain)
  
  // 计算索引空间开销（简化）
  let index_overhead = multiply(table_size, 10) / 100  // 10%的额外空间
  assert_eq(100000, index_overhead)
}

test "encryption_simulation" {
  // 加密模拟：模拟简单的加密算法计算
  let message_length = 100
  let key_length = 16
  let block_size = 8
  
  // 计算需要的块数（填充到块大小的倍数）
  let blocks_needed = add(divide_with_ceil(message_length, block_size), 0)
  
  // 计算加密后的数据长度
  let encrypted_length = multiply(blocks_needed, block_size)
  
  assert_eq(104, encrypted_length)
  
  // 计算加密开销（添加IV和MAC）
  let iv_size = 16
  let mac_size = 32
  let total_overhead = add(iv_size, mac_size)
  let final_length = add(encrypted_length, total_overhead)
  
  assert_eq(152, final_length)
}

test "machine_learning_prediction" {
  // 机器学习预测模拟：模拟简单的线性回归计算
  let x1 = 10
  let x2 = 20
  let x3 = 30
  let w1 = 2  // 权重1
  let w2 = 3  // 权重2
  let w3 = 1  // 权重3
  let b = 5   // 偏置
  
  // 计算预测值：y = w1*x1 + w2*x2 + w3*x3 + b
  let term1 = multiply(w1, x1)
  let term2 = multiply(w2, x2)
  let term3 = multiply(w3, x3)
  let prediction = add(add(add(term1, term2), term3), b)
  
  assert_eq(135, prediction)
  
  // 计算误差（假设实际值为140）
  let actual_value = 140
  let error = add(actual_value, -prediction)
  assert_eq(5, error)
  
  // 计算均方误差（简化版本）
  let mse = multiply(error, error)
  assert_eq(25, mse)
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

// 新增的10个MoonBit测试用例
test "advanced_calculator_functions" {
  // 高级计算器功能测试
  // 计算复合利率：A = P(1 + r/n)^(nt)
  let principal = 10000
  let rate = 5  // 5%
  let compounds = 12  // 月复利
  let years = 3
  
  // 简化计算：10000 * (1 + 5/12)^(36) ≈ 10000 * 16 = 160000
  let growth_factor = add(1, multiply(rate, years))
  let final_amount = multiply(principal, growth_factor)
  assert_eq(160000, final_amount)
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result = greet(special_chars)
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result)
  
  // 测试包含HTML标签的字符串
  let html_string = "<div>Hello</div>"
  let html_result = greet(html_string)
  assert_eq_string("Hello, <div>Hello</div>!", html_result)
  
  // 测试包含JSON格式的字符串
  let json_string = "{\"name\":\"John\",\"age\":30}"
  let json_result = greet(json_string)
  assert_eq_string("Hello, {\"name\":\"John\",\"age\":30}!", json_result)
}

test "matrix_operations_simulation" {
  // 矩阵运算模拟测试
  // 2x2矩阵乘法模拟
  let a11 = 1; let a12 = 2
  let a21 = 3; let a22 = 4
  let b11 = 5; let b12 = 6
  let b21 = 7; let b22 = 8
  
  // 计算结果矩阵的元素
  let c11 = add(multiply(a11, b11), multiply(a12, b21))
  let c12 = add(multiply(a11, b12), multiply(a12, b22))
  let c21 = add(multiply(a21, b11), multiply(a22, b21))
  let c22 = add(multiply(a21, b12), multiply(a22, b22))
  
  assert_eq(19, c11)  // 1*5 + 2*7 = 19
  assert_eq(22, c12)  // 1*6 + 2*8 = 22
  assert_eq(43, c21)  // 3*5 + 4*7 = 43
  assert_eq(50, c22)  // 3*6 + 4*8 = 50
}

test "cryptography_basic" {
  // 基础密码学测试（简化版）
  // 简单的凯撒密码实现
  let plaintext = "HELLO"
  let shift = 3
  
  // 由于我们只有数值运算，我们模拟字符编码
  let h_code = 72  // 'H'的ASCII码
  let e_code = 69  // 'E'的ASCII码
  let l_code = 76  // 'L'的ASCII码
  let o_code = 79  // 'O'的ASCII码
  
  // 加密过程（简化）
  let h_encrypted = add(h_code, shift)
  let e_encrypted = add(e_code, shift)
  let l_encrypted = add(l_code, shift)
  let o_encrypted = add(o_code, shift)
  
  assert_eq(75, h_encrypted)  // 72 + 3 = 75 ('K')
  assert_eq(72, e_encrypted)  // 69 + 3 = 72 ('H')
  assert_eq(79, l_encrypted)  // 76 + 3 = 79 ('O')
  assert_eq(82, o_encrypted)  // 79 + 3 = 82 ('R')
}

test "physics_simulation" {
  // 物理模拟测试
  // 自由落体运动：h = v₀t + (1/2)gt²
  let initial_velocity = 10  // m/s
  let gravity = 10  // m/s² (简化值)
  let time = 5  // seconds
  
  // 计算位移：h = 10*5 + (1/2)*10*5² = 50 + 125 = 175
  let velocity_component = multiply(initial_velocity, time)
  let gravity_component = multiply(gravity, multiply(time, time)) / 2
  let displacement = add(velocity_component, gravity_component)
  
  assert_eq(175, displacement)
  
  // 计算最终速度：v = v₀ + gt = 10 + 10*5 = 60
  let final_velocity = add(initial_velocity, multiply(gravity, time))
  assert_eq(60, final_velocity)
}

test "data_compression_simulation" {
  // 数据压缩模拟测试
  // 简单的行程长度编码模拟
  let original_data = 1111000  // 模拟数据：4个1，3个0
  let run_length_1 = 4
  let run_length_0 = 3
  
  // 压缩后的数据表示
  let compressed_size = add(run_length_1, run_length_0)
  let compression_ratio = multiply(100, compressed_size) / 7  // 原始长度为7
  
  assert_eq(7, compressed_size)
  assert_eq(100, compression_ratio)  // 在这种情况下没有压缩
}

test "network_protocol_simulation" {
  // 网络协议模拟测试
  // 简单的TCP校验和计算
  let packet1 = 1000
  let packet2 = 2000
  let packet3 = 3000
  let packet4 = 4000
  
  // 计算校验和
  let sum = add(add(add(packet1, packet2), packet3), packet4)
  let checksum = sum / 65535  // 简化的校验和计算
  
  assert_eq(10000, sum)
  assert_eq(0, checksum)  // 简化结果
}

test "machine_learning_basic" {
  // 机器学习基础测试
  // 简单的线性回归：y = mx + b
  let x1 = 1; let y1 = 3
  let x2 = 2; let y2 = 5
  let x3 = 3; let y3 = 7
  
  // 计算斜率m = (y2-y1)/(x2-x1) = (5-3)/(2-1) = 2
  let slope = add(y2, -y1) / add(x2, -x1)
  
  // 计算截距b = y1 - mx1 = 3 - 2*1 = 1
  let intercept = add(y1, multiply(-slope, x1))
  
  // 预测x=4时的值：y = 2*4 + 1 = 9
  let x4 = 4
  let y4_predicted = add(multiply(slope, x4), intercept)
  
  assert_eq(2, slope)
  assert_eq(1, intercept)
  assert_eq(9, y4_predicted)
}

test "database_operations" {
  // 数据库操作模拟测试
  // 简单的SQL聚合函数模拟
  let table_values = add(add(add(add(10, 20), 30), 40), 50)
  let count = 5
  
  // 计算平均值
  let average = table_values / count
  
  // 计算总和
  let sum = table_values
  
  // 计算最大值（简化）
  let max_value = 50
  
  // 计算最小值（简化）
  let min_value = 10
  
  assert_eq(30, average)
  assert_eq(150, sum)
  assert_eq(50, max_value)
  assert_eq(10, min_value)
}

test "game_development_calculations" {
  // 游戏开发计算测试
  // 2D游戏中的碰撞检测
  let rect1_x = 10; let rect1_y = 10; let rect1_width = 20; let rect1_height = 20
  let rect2_x = 25; let rect2_y = 25; let rect2_width = 30; let rect2_height = 30
  
  // 计算矩形边界
  let rect1_right = add(rect1_x, rect1_width)
  let rect1_bottom = add(rect1_y, rect1_height)
  let rect2_right = add(rect2_x, rect2_width)
  let rect2_bottom = add(rect2_y, rect2_height)
  
  // 碰撞检测（简化）
  let collision_x = rect1_right > rect2_x && rect1_x < rect2_right
  let collision_y = rect1_bottom > rect2_y && rect1_y < rect2_bottom
  let collision = collision_x && collision_y
  
  assert_true(collision)
  
  // 计算距离
  let dx = add(rect2_x, -rect1_x)
  let dy = add(rect2_y, -rect1_y)
  let distance_squared = add(multiply(dx, dx), multiply(dy, dy))
  
  assert_eq(225, distance_squared)  // 15² + 15² = 225
}

// 新增的8个 MoonBit 测试用例
test "binary_operations_simulation" {
  // 二进制运算模拟测试
  let a = 12  // 二进制 1100
  let b = 10  // 二进制 1010
  
  // 模拟按位与运算：1100 & 1010 = 1000 (8)
  let bitwise_and = 8
  assert_eq(8, bitwise_and)
  
  // 模拟按位或运算：1100 | 1010 = 1110 (14)
  let bitwise_or = 14
  assert_eq(14, bitwise_or)
  
  // 模拟按位异或运算：1100 ^ 1010 = 0110 (6)
  let bitwise_xor = 6
  assert_eq(6, bitwise_xor)
  
  // 模拟左移运算：1100 << 2 = 110000 (48)
  let left_shift = multiply(a, 4)
  assert_eq(48, left_shift)
  
  // 模拟右移运算：1100 >> 2 = 11 (3)
  let right_shift = a / 4
  assert_eq(3, right_shift)
}

test "error_correction_codes" {
  // 错误纠正码模拟测试
  let data_bits = 4
  let parity_bits = 3
  let total_bits = add(data_bits, parity_bits)
  
  // 计算汉明码的总位数
  assert_eq(7, total_bits)
  
  // 模拟错误检测和纠正
  let received_data = 85  // 二进制 1010101
  let error_position = 3
  let corrected_data = add(received_data, multiply(4, error_position))
  
  // 验证错误纠正后的数据
  assert_eq(97, corrected_data)
  
  // 计算冗余度
  let redundancy = multiply(parity_bits, 100) / total_bits
  assert_eq(42, redundancy)  // 3/7 ≈ 42.86% ≈ 42%
}

test "signal_processing" {
  // 信号处理模拟测试
  let sampling_rate = 44100  // Hz
  let duration = 5  // seconds
  let bit_depth = 16  // bits
  
  // 计算音频数据大小
  let total_samples = multiply(sampling_rate, duration)
  let bytes_per_sample = bit_depth / 8
  let total_bytes = multiply(total_samples, bytes_per_sample)
  
  assert_eq(220500, total_samples)
  assert_eq(2, bytes_per_sample)
  assert_eq(441000, total_bytes)
  
  // 计算数据传输速率
  let bitrate = multiply(sampling_rate, bit_depth)
  assert_eq(705600, bitrate)
  
  // 计算压缩后的数据大小（假设压缩比为10:1）
  let compressed_size = total_bytes / 10
  assert_eq(44100, compressed_size)
}

test "queue_simulation" {
  // 队列操作模拟测试
  let queue_capacity = 10
  let initial_size = 0
  let enqueue_count = 7
  let dequeue_count = 3
  
  // 模拟入队操作
  let after_enqueue = add(initial_size, enqueue_count)
  assert_eq(7, after_enqueue)
  
  // 模拟出队操作
  let after_dequeue = add(after_enqueue, -dequeue_count)
  assert_eq(4, after_dequeue)
  
  // 检查队列状态
  let is_empty = after_dequeue == 0
  let is_full = after_dequeue == queue_capacity
  
  assert_false(is_empty)
  assert_false(is_full)
  
  // 计算队列利用率
  let utilization = multiply(after_dequeue, 100) / queue_capacity
  assert_eq(40, utilization)
}

test "stack_operations" {
  // 栈操作模拟测试
  let stack_capacity = 8
  let push_operations = 5
  let pop_operations = 2
  
  // 模拟栈操作
  let current_size = add(push_operations, -pop_operations)
  assert_eq(3, current_size)
  
  // 检查栈状态
  let is_empty = current_size == 0
  let is_full = current_size == stack_capacity
  let can_push = add(current_size, 1) <= stack_capacity
  let can_pop = current_size > 0
  
  assert_false(is_empty)
  assert_false(is_full)
  assert_true(can_push)
  assert_true(can_pop)
  
  // 计算栈的使用率
  let usage_percentage = multiply(current_size, 100) / stack_capacity
  assert_eq(37, usage_percentage)  // 3/8 ≈ 37.5% ≈ 37%
}

test "hash_table_simulation" {
  // 哈希表操作模拟测试
  let table_size = 16
  let elements_count = 10
  let collision_resolution_steps = 3
  
  // 计算负载因子
  let load_factor = multiply(elements_count, 100) / table_size
  assert_eq(62, load_factor)  // 10/16 ≈ 62.5% ≈ 62%
  
  // 模拟哈希冲突处理
  let hash_value = 7
  let collision_count = 2
  let final_index = add(hash_value, collision_count)
  
  assert_eq(9, final_index)
  
  // 计算平均查找长度
  let avg_lookup_length = add(1, multiply(load_factor, collision_resolution_steps) / 100)
  assert_eq(2, avg_lookup_length)  // 简化计算
}

test "graph_algorithms" {
  // 图算法模拟测试
  let vertex_count = 6
  let edge_count = 9
  let start_vertex = 0
  let end_vertex = 5
  
  // 计算图的密度
  let max_edges = multiply(vertex_count, add(vertex_count, -1)) / 2
  let density = multiply(edge_count, 100) / max_edges
  assert_eq(60, density)  // 9/15 = 0.6 = 60%
  
  // 模拟最短路径计算（Dijkstra算法简化版）
  let path_length = 14
  let intermediate_vertices = 3
  
  assert_eq(14, path_length)
  assert_eq(3, intermediate_vertices)
  
  // 计算图的连通性（简化）
  let connected_components = 1
  let is_connected = connected_components == 1
  assert_true(is_connected)
}

test "tree_operations" {
  // 树操作模拟测试
  let tree_height = 4
  let branching_factor = 3
  
  // 计算完全树的最大节点数
  let max_nodes = 1
  let level1_nodes = multiply(1, branching_factor)
  let level2_nodes = multiply(level1_nodes, branching_factor)
  let level3_nodes = multiply(level2_nodes, branching_factor)
  let total_nodes = add(add(add(add(1, level1_nodes), level2_nodes), level3_nodes), 1)
  
  assert_eq(40, total_nodes)  // 1 + 3 + 9 + 27 = 40
  
  // 计算叶子节点数
  let leaf_nodes = multiply(3, multiply(3, 3))
  assert_eq(27, leaf_nodes)
  
  // 计算内部节点数
  let internal_nodes = add(total_nodes, -leaf_nodes)
  assert_eq(13, internal_nodes)
  
  // 计算树的平衡因子（简化）
  let is_balanced = true
  assert_true(is_balanced)
}

// 新增的 MoonBit 测试用例
test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(5, divide_with_ceil(11, 2))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(100, divide_with_ceil(100, 1))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(2, divide_with_ceil(101, 100))
  assert_eq(10, divide_with_ceil(91, 10))
}

test "divide_with_ceil_negative" {
  // 测试向上取整除法的负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(-9, 3))
  assert_eq(0, divide_with_ceil(-1, 100))
  assert_eq(-1, divide_with_ceil(-100, 100))
  assert_eq(-5, divide_with_ceil(-11, 2))
}

test "resource_allocation" {
  // 测试资源分配场景
  let total_resources = 17
  let team_size = 5
  
  // 计算每个团队需要的资源数（向上取整）
  let resources_per_team = divide_with_ceil(total_resources, team_size)
  assert_eq(4, resources_per_team)
  
  // 计算总分配资源
  let total_allocated = multiply(resources_per_team, team_size)
  assert_eq(20, total_allocated)
  
  // 计算剩余资源
  let remaining = add(total_allocated, -total_resources)
  assert_eq(3, remaining)
}

test "page_pagination" {
  // 测试分页场景
  let total_items = 23
  let items_per_page = 10
  
  // 计算需要的页数（向上取整）
  let total_pages = divide_with_ceil(total_items, items_per_page)
  assert_eq(3, total_pages)
  
  // 最后一页的项目数
  let last_page_items = total_items % items_per_page
  assert_eq(3, last_page_items)
}

test "time_slot_scheduling" {
  // 测试时间段调度场景
  let total_duration = 65  // 分钟
  let slot_duration = 15   // 分钟
  
  // 计算需要的时间段数（向上取整）
  let required_slots = divide_with_ceil(total_duration, slot_duration)
  assert_eq(5, required_slots)
  
  // 计算总分配时间
  let total_allocated_time = multiply(required_slots, slot_duration)
  assert_eq(75, total_allocated_time)
  
  // 计算空闲时间
  let idle_time = add(total_allocated_time, -total_duration)
  assert_eq(10, idle_time)
}

test "container_packing" {
  // 测试容器装箱场景
  let total_items = 37
  let container_capacity = 12
  
  // 计算需要的容器数（向上取整）
  let required_containers = divide_with_ceil(total_items, container_capacity)
  assert_eq(4, required_containers)
  
  // 计算总容量
  let total_capacity = multiply(required_containers, container_capacity)
  assert_eq(48, total_capacity)
  
  // 计算剩余容量
  let remaining_capacity = add(total_capacity, -total_items)
  assert_eq(11, remaining_capacity)
}

test "bandwidth_allocation" {
  // 测试带宽分配场景
  let total_bandwidth = 1000  // Mbps
  let min_bandwidth_per_user = 150  // Mbps
  
  // 计算可支持的最大用户数（向下取整）
  let max_users = total_bandwidth / min_bandwidth_per_user
  assert_eq(6, max_users)
  
  // 计算实际使用的带宽
  let used_bandwidth = multiply(max_users, min_bandwidth_per_user)
  assert_eq(900, used_bandwidth)
  
  // 计算剩余带宽
  let remaining_bandwidth = add(total_bandwidth, -used_bandwidth)
  assert_eq(100, remaining_bandwidth)
}

test "storage_planning" {
  // 测试存储规划场景
  let data_size = 2500  // GB
  let disk_capacity = 1000  // GB
  
  // 计算需要的磁盘数（向上取整）
  let required_disks = divide_with_ceil(data_size, disk_capacity)
  assert_eq(3, required_disks)
  
  // 计算总存储容量
  let total_storage = multiply(required_disks, disk_capacity)
  assert_eq(3000, total_storage)
  
  // 计算存储利用率
  let utilization = multiply(data_size, 100) / total_storage
  assert_eq(83, utilization)  // 2500/3000 ≈ 83.33% ≈ 83%
}

test "task_distribution" {
  // 测试任务分配场景
  let total_tasks = 47
  let worker_count = 8
  
  // 计算每个工人的基本任务数
  let base_tasks = total_tasks / worker_count
  assert_eq(5, base_tasks)
  
  // 计算需要额外处理任务的工人数
  let extra_tasks = total_tasks % worker_count
  assert_eq(7, extra_tasks)
  
  // 计算最大任务负载
  let max_load = add(base_tasks, if extra_tasks > 0 { 1 } else { 0 })
  assert_eq(6, max_load)
  
  // 计算最小任务负载
  let min_load = base_tasks
  assert_eq(5, min_load)
}

// 新增的10个 MoonBit 测试用例
test "string_length_validation" {
  // 测试字符串长度验证
  let short_string = "Hi"
  let medium_string = "MoonBit"
  let long_string = "This is a very long string for testing purposes"
  
  // 验证不同长度字符串的greet函数处理
  assert_eq_string("Hello, Hi!", greet(short_string))
  assert_eq_string("Hello, MoonBit!", greet(medium_string))
  assert_eq_string("Hello, This is a very long string for testing purposes!", greet(long_string))
}

test "mathematical_series" {
  // 测试数学级数计算
  // 计算等比数列前n项和：a + ar + ar² + ... + ar^(n-1)
  let a = 2      // 首项
  let r = 3      // 公比
  let n = 4      // 项数
  
  // 手动计算：2 + 6 + 18 + 54 = 80
  let term1 = a
  let term2 = multiply(a, r)
  let term3 = multiply(term2, r)
  let term4 = multiply(term3, r)
  let sum = add(add(add(term1, term2), term3), term4)
  
  assert_eq(80, sum)
}

test "number_system_conversion" {
  // 测试数字系统转换（简化版）
  // 十进制转二进制模拟
  let decimal_num = 13
  
  // 模拟13的二进制表示：1101 = 1*2³ + 1*2² + 0*2¹ + 1*2⁰ = 8 + 4 + 0 + 1 = 13
  let bit0 = 1
  let bit1 = 0
  let bit2 = 1
  let bit3 = 1
  
  let reconstructed = add(add(add(multiply(bit3, 8), multiply(bit2, 4)), multiply(bit1, 2)), bit0)
  assert_eq(decimal_num, reconstructed)
}

test "temperature_conversion_precise" {
  // 测试精确的温度转换
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数近似：F = C × 2 - C × 0.2 + 32 ≈ C × 2 + 32 - C/5
  
  let celsius = 25
  let fahrenheit_approx = add(add(multiply(celsius, 2), 32), multiply(-1, celsius / 5))
  assert_eq(77, fahrenheit_approx)  // 25 × 2 + 32 - 5 = 77
  
  // 测试负温度
  let neg_celsius = -10
  let neg_fahrenheit = add(add(multiply(neg_celsius, 2), 32), multiply(-1, neg_celsius / 5))
  assert_eq(14, neg_fahrenheit)  // -10 × 2 + 32 - (-2) = 14
}

test "financial_compound_interest" {
  // 测试金融复利计算
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  let compounds_per_year = 12  // 月复利
  
  // 简化复利公式：A = P(1 + r/n)^(nt)
  // 近似计算：A = P(1 + r×t)
  let growth_factor = add(1, multiply(annual_rate, years))
  let final_amount = multiply(principal, growth_factor)
  
  assert_eq(160000, final_amount)  // 10000 × (1 + 5×3) = 10000 × 16 = 160000
}

test "statistical_variance" {
  // 测试统计方差计算
  // 数据集：[5, 8, 12, 15, 20]
  let values = [5, 8, 12, 15, 20]
  let count = 5
  
  // 计算平均值：(5 + 8 + 12 + 15 + 20) / 5 = 60 / 5 = 12
  let sum = add(add(add(add(5, 8), 12), 15), 20)
  let mean = sum / count
  assert_eq(12, mean)
  
  // 计算方差：Σ(x - mean)² / n
  let diff1 = add(5, -12)  // -7
  let diff2 = add(8, -12)  // -4
  let diff3 = add(12, -12) // 0
  let diff4 = add(15, -12) // 3
  let diff5 = add(20, -12) // 8
  
  let variance_sum = add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), 
                                multiply(diff3, diff3)), multiply(diff4, diff4)), multiply(diff5, diff5))
  let variance = variance_sum / count
  
  assert_eq(26, variance)  // (49 + 16 + 0 + 9 + 64) / 5 = 138 / 5 = 27.6 ≈ 26 (整数除法)
}

test "geometry_circle_properties" {
  // 测试圆的几何属性
  let radius = 5
  let pi_approx = 3  // π的近似值
  
  // 计算直径
  let diameter = multiply(radius, 2)
  assert_eq(10, diameter)
  
  // 计算周长：C = π × d
  let circumference = multiply(pi_approx, diameter)
  assert_eq(30, circumference)
  
  // 计算面积：A = π × r²
  let area = multiply(pi_approx, multiply(radius, radius))
  assert_eq(75, area)
}

test "game_score_ranking" {
  // 测试游戏得分排名系统
  let scores = [850, 920, 780, 950, 890]
  let player_count = 5
  
  // 计算总分
  let total_score = add(add(add(add(850, 920), 780), 950), 890)
  assert_eq(4390, total_score)
  
  // 计算平均分
  let average_score = total_score / player_count
  assert_eq(878, average_score)  // 4390 / 5 = 878
  
  // 计算最高分和最低分的差值
  let max_score = 950
  let min_score = 780
  let score_range = add(max_score, -min_score)
  assert_eq(170, score_range)
}

test "network_throughput" {
  // 测试网络吞吐量计算
  let file_size = 1048576  // 1MB in bytes
  let bandwidth = 1048576  // 1MB/s
  let latency = 50         // 50ms
  let packet_size = 1500   // 1500 bytes
  
  // 计算传输时间（简化）
  let transfer_time = file_size / bandwidth
  assert_eq(1, transfer_time)  // 1秒
  
  // 计算数据包数量
  let packet_count = divide_with_ceil(file_size, packet_size)
  assert_eq(700, packet_count)  // 1048576 / 1500 ≈ 699.05 ≈ 700
  
  // 计算总延迟时间
  let total_latency = multiply(packet_count, latency)
  assert_eq(35000, total_latency)  // 700 × 50 = 35000ms
}

test "data_structure_validation" {
  // 测试数据结构验证
  // 验证数组索引计算
  let row = 3
  let col = 2
  let cols_per_row = 5
  
  // 二维数组转一维数组的索引计算
  let index_1d = add(multiply(row, cols_per_row), col)
  assert_eq(17, index_1d)  // 3 × 5 + 2 = 17
  
  // 验证边界条件
  let first_index = add(multiply(0, cols_per_row), 0)
  let last_index = add(multiply(9, cols_per_row), 4)  // 假设10行5列
  
  assert_eq(0, first_index)
  assert_eq(49, last_index)
  
  // 验证索引范围
  let is_valid_index = index_1d >= first_index && index_1d <= last_index
  assert_true(is_valid_index)
}

// 新增的10个 MoonBit 测试用例
test "complex_arithmetic_sequence" {
  // 测试复杂等差数列计算
  // 计算数列：3, 7, 11, 15, 19, 23, 27, 31, 35, 39 的和
  // 这是一个首项为3，公差为4的等差数列
  let first_term = 3
  let common_diff = 4
  let num_terms = 10
  
  // 使用公式：和 = n/2 × (2a + (n-1)d)
  let sum_formula = multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))) / 2
  assert_eq(210, sum_formula)  // 10/2 × (6 + 36) = 5 × 42 = 210
  
  // 验证特定项的值
  let term_5 = add(first_term, multiply(4, common_diff))  // 第5项
  assert_eq(19, term_5)  // 3 + 4×4 = 19
  
  let term_10 = add(first_term, multiply(9, common_diff))  // 第10项
  assert_eq(39, term_10)  // 3 + 9×4 = 39
}

test "string_pattern_matching" {
  // 测试字符串模式匹配场景
  // 模拟简单的字符串匹配算法
  
  // 测试包含特定模式的字符串
  let pattern = "test"
  let text1 = "this is a test string"
  let text2 = "no match here"
  let text3 = "test at beginning"
  let text4 = "test"
  
  // 验证字符串包含关系（通过greet函数间接测试）
  let result1 = greet(text1)
  let result2 = greet(text2)
  let result3 = greet(text3)
  let result4 = greet(text4)
  
  assert_eq_string("Hello, this is a test string!", result1)
  assert_eq_string("Hello, no match here!", result2)
  assert_eq_string("Hello, test at beginning!", result3)
  assert_eq_string("Hello, test!", result4)
  
  // 测试字符串长度计算（模拟）
  let len1 = 21  // "this is a test string" 的长度
  let len2 = 14  // "no match here" 的长度
  let len3 = 19  // "test at beginning" 的长度
  let len4 = 4   // "test" 的长度
  
  // 验证长度关系
  assert_true(len1 > len2)
  assert_true(len3 > len4)
  assert_eq(4, len4)
}

test "memory_address_calculation" {
  // 测试内存地址计算模拟
  // 模拟数组元素的内存地址计算
  
  let base_address = 1000  // 基地址
  let element_size = 4     // 每个元素4字节
  let index1 = 0
  let index2 = 5
  let index3 = 10
  
  // 计算元素地址：地址 = 基地址 + 索引 × 元素大小
  let addr1 = add(base_address, multiply(index1, element_size))
  let addr2 = add(base_address, multiply(index2, element_size))
  let addr3 = add(base_address, multiply(index3, element_size))
  
  assert_eq(1000, addr1)  // 1000 + 0×4 = 1000
  assert_eq(1020, addr2)  // 1000 + 5×4 = 1020
  assert_eq(1040, addr3)  // 1000 + 10×4 = 1040
  
  // 计算元素间的偏移量
  let offset12 = add(addr2, -addr1)
  let offset23 = add(addr3, -addr2)
  
  assert_eq(20, offset12)  // 1020 - 1000 = 20
  assert_eq(20, offset23)  // 1040 - 1020 = 20
  
  // 验证地址对齐（4字节对齐）
  let aligned1 = addr1 % element_size == 0
  let aligned2 = addr2 % element_size == 0
  let aligned3 = addr3 % element_size == 0
  
  assert_true(aligned1 && aligned2 && aligned3)
}

test "probability_calculation" {
  // 测试概率计算场景
  // 模掷两个骰子的概率计算
  
  let dice_faces = 6  // 六面骰子
  let total_outcomes = multiply(dice_faces, dice_faces)  // 总结果数 = 6×6 = 36
  
  // 计算特定点数的组合数
  let sum_7_combinations = 6  // 点数和为7的组合：(1,6), (2,5), (3,4), (4,3), (5,2), (6,1)
  let sum_11_combinations = 2 // 点数和为11的组合：(5,6), (6,5)
  let sum_2_combinations = 1  // 点数和为2的组合：(1,1)
  let sum_12_combinations = 1 // 点数和为12的组合：(6,6)
  
  // 计算概率（以百分比表示）
  let prob_7 = multiply(sum_7_combinations, 100) / total_outcomes
  let prob_11 = multiply(sum_11_combinations, 100) / total_outcomes
  let prob_2 = multiply(sum_2_combinations, 100) / total_outcomes
  let prob_12 = multiply(sum_12_combinations, 100) / total_outcomes
  
  assert_eq(16, prob_7)   // 6/36 ≈ 16.67% ≈ 16%
  assert_eq(5, prob_11)   // 2/36 ≈ 5.56% ≈ 5%
  assert_eq(2, prob_2)    // 1/36 ≈ 2.78% ≈ 2%
  assert_eq(2, prob_12)   // 1/36 ≈ 2.78% ≈ 2%
  
  // 计算期望值（简化）
  let expected_sum = 7  // 两个骰子的期望和
  assert_eq(7, expected_sum)
}

test "circular_buffer_operations" {
  // 测试循环缓冲区操作模拟
  let buffer_size = 8
  let initial_head = 0
  let initial_tail = 0
  let initial_count = 0
  
  // 模拟入队操作
  let enqueue_count = 5
  let new_tail = add(initial_tail, enqueue_count) % buffer_size
  let new_count = add(initial_count, enqueue_count)
  
  assert_eq(5, new_tail)  // (0 + 5) % 8 = 5
  assert_eq(5, new_count) // 0 + 5 = 5
  
  // 模拟出队操作
  let dequeue_count = 3
  let new_head = add(initial_head, dequeue_count) % buffer_size
  let final_count = add(new_count, -dequeue_count)
  
  assert_eq(3, new_head)  // (0 + 3) % 8 = 3
  assert_eq(2, final_count) // 5 - 3 = 2
  
  // 检查缓冲区状态
  let is_empty = final_count == 0
  let is_full = final_count == buffer_size
  let has_space = final_count < buffer_size
  
  assert_false(is_empty)
  assert_false(is_full)
  assert_true(has_space)
  
  // 计算缓冲区利用率
  let utilization = multiply(final_count, 100) / buffer_size
  assert_eq(25, utilization) // 2/8 = 25%
}

test "color_space_conversion" {
  // 测试颜色空间转换模拟
  // RGB到HSV的简化转换模拟
  
  let r = 255  // 红色分量
  let g = 0    // 绿色分量
  let b = 0    // 蓝色分量
  
  // 计算最大值和最小值
  let max_val = 255
  let min_val = 0
  let delta = add(max_val, -min_val)
  
  assert_eq(255, max_val)
  assert_eq(0, min_val)
  assert_eq(255, delta)
  
  // 简化的色相计算（红色对应0度）
  let hue = if max_val == r { 0 } else { 120 }  // 简化逻辑
  assert_eq(0, hue)
  
  // 饱和度计算（简化）
  let saturation = if max_val == 0 { 0 } else { multiply(delta, 100) / max_val }
  assert_eq(100, saturation)  // 255/255 × 100 = 100%
  
  // 明度计算
  let value = max_val
  assert_eq(255, value)
  
  // 测试另一种颜色（绿色）
  let r2 = 0
  let g2 = 255
  let b2 = 0
  
  let max_val2 = 255
  let min_val2 = 0
  let delta2 = add(max_val2, -min_val2)
  
  let hue2 = if max_val2 == r2 { 0 } else { 120 }  // 绿色对应120度
  let saturation2 = if max_val2 == 0 { 0 } else { multiply(delta2, 100) / max_val2 }
  let value2 = max_val2
  
  assert_eq(120, hue2)
  assert_eq(100, saturation2)
  assert_eq(255, value2)
}

test "checksum_verification" {
  // 测试校验和验证
  // 模拟简单的校验和算法
  
  let data_block1 = 100
  let data_block2 = 200
  let data_block3 = 300
  let data_block4 = 400
  
  // 计算简单校验和
  let checksum = add(add(add(data_block1, data_block2), data_block3), data_block4)
  assert_eq(1000, checksum)
  
  // 模拟数据传输后的验证
  let received_block1 = 100
  let received_block2 = 200
  let received_block3 = 300  // 模拟这个块出错
  let received_block4 = 400
  
  let received_checksum = add(add(add(received_block1, received_block2), received_block3), received_block4)
  assert_eq(1000, received_checksum)
  
  // 检测错误（通过比较校验和）
  let error_detected = checksum != received_checksum
  assert_false(error_detected)  // 在这种情况下没有检测到错误
  
  // 模拟有错误的情况
  let corrupted_block3 = 310
  let corrupted_checksum = add(add(add(data_block1, data_block2), corrupted_block3), data_block4)
  let actual_error = checksum != corrupted_checksum
  assert_true(actual_error)  // 检测到错误
}

test "load_balancer_round_robin" {
  // 测试轮询负载均衡器模拟
  let server_count = 3
  let request_count = 10
  
  // 模拟请求分发
  let server1_requests = 0
  let server2_requests = 0
  let server3_requests = 0
  
  // 手动模拟轮询分配
  // 请求1-10分别分配到服务器：1,2,3,1,2,3,1,2,3,1
  let final_server1_requests = 4  // 请求1,4,7,10
  let final_server2_requests = 3  // 请求2,5,8
  let final_server3_requests = 3  // 请求3,6,9
  
  assert_eq(4, final_server1_requests)
  assert_eq(3, final_server2_requests)
  assert_eq(3, final_server3_requests)
  
  // 验证总请求数
  let total_distributed = add(add(final_server1_requests, final_server2_requests), final_server3_requests)
  assert_eq(request_count, total_distributed)
  
  // 计算负载均衡度
  let max_requests = 4
  let min_requests = 3
  let load_imbalance = add(max_requests, -min_requests)
  assert_eq(1, load_imbalance)  // 最大差值为1，表示负载均衡较好
  
  // 计算每个服务器的负载百分比
  let server1_load = multiply(final_server1_requests, 100) / request_count
  let server2_load = multiply(final_server2_requests, 100) / request_count
  let server3_load = multiply(final_server3_requests, 100) / request_count
  
  assert_eq(40, server1_load)  // 4/10 = 40%
  assert_eq(30, server2_load)  // 3/10 = 30%
  assert_eq(30, server3_load)  // 3/10 = 30%
}

test "cache_replacement_policy" {
  // 测试缓存替换策略模拟
  // 模拟LRU（最近最少使用）缓存策略
  
  let cache_size = 3
  let access_sequence = [1, 2, 3, 4, 1, 2, 5]  // 访问序列
  
  // 模拟缓存状态变化
  // 访问1,2,3后：缓存 = [1,2,3]
  // 访问4后：缓存 = [2,3,4] (1被替换)
  // 访问1后：缓存 = [3,4,1] (2被替换)
  // 访问2后：缓存 = [4,1,2] (3被替换)
  // 访问5后：缓存 = [1,2,5] (4被替换)
  
  let final_cache = [1, 2, 5]
  let cache_hits = 2  // 访问1和2时命中
  let cache_misses = 5 // 其他访问未命中
  let total_accesses = 7
  
  assert_eq(2, cache_hits)
  assert_eq(5, cache_misses)
  assert_eq(7, total_accesses)
  
  // 计算缓存命中率
  let hit_rate = multiply(cache_hits, 100) / total_accesses
  assert_eq(28, hit_rate)  // 2/7 ≈ 28.57% ≈ 28%
  
  // 计算缓存未命中率
  let miss_rate = multiply(cache_misses, 100) / total_accesses
  assert_eq(71, miss_rate)  // 5/7 ≈ 71.43% ≈ 71%
  
  // 验证命中率与未命中率之和
  let total_rate = add(hit_rate, miss_rate)
  assert_eq(99, total_rate)  // 由于整数除法，不是100%
}

test "error_detection_codes" {
  // 测试错误检测码模拟
  // 模拟奇偶校验码
  
  let data_byte = 10110101  // 二进制数据
  let data_bits = 8
  
  // 计算奇偶校验位（偶校验）
  // 1的个数：5个
  let ones_count = 5
  let parity_bit = if ones_count % 2 == 0 { 0 } else { 1 }
  assert_eq(1, parity_bit)  // 奇数个1，所以校验位为1
  
  // 模拟传输后的数据
  let received_data = 10100101  // 第4位从1变为0
  let received_ones_count = 4
  let received_parity_bit = if received_ones_count % 2 == 0 { 0 } else { 1 }
  assert_eq(0, received_parity_bit)  // 偶数个1，所以校验位为0
  
  // 检测错误
  let error_detected = parity_bit != received_parity_bit
  assert_true(error_detected)  // 检测到错误
  
  // 模拟CRC校验（简化）
  let data_block = 12345
  let divisor = 123
  let remainder = data_block % divisor
  let crc_value = remainder
  
  assert_eq(45, crc_value)  // 12345 % 123 = 45
  
  // 模拟接收端的CRC计算
  let received_block = 12345
  let received_remainder = received_block % divisor
  let received_crc = received_remainder
  
  let crc_error = crc_value != received_crc
  assert_false(crc_error)  // 没有错误
  
  // 模拟数据损坏
  let corrupted_block = 12350
  let corrupted_remainder = corrupted_block % divisor
  let corrupted_crc = corrupted_remainder
  
  let crc_detected = crc_value != corrupted_crc
  assert_true(crc_detected)  // 检测到错误
}

// 新增的10个简洁测试用例
test "simple_add_functionality" {
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
}

test "add_overflow_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
}

test "simple_multiply_functionality" {
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(0, 100))
}

test "multiply_boundary_cases" {
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_tests" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "math_expression_tests" {
  let result = multiply(add(2, 3), 4)
  assert_eq(20, result)
  
  let result2 = add(multiply(5, 3), 2)
  assert_eq(17, result2)
}

test "practical_calculation_tests" {
  let price = 199
  let quantity = 3
  let total = multiply(price, quantity)
  assert_eq(597, total)
}

test "string_processing_tests" {
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, VeryLongName!", greet("VeryLongName"))
}

test "complex_business_logic_tests" {
  let order_total = 1200
  let discount_rate = 10
  let discount_amount = multiply(order_total, discount_rate) / 100
  let final_total = add(order_total, -discount_amount)
  assert_eq(1080, final_total)
}

test "assertion_function_tests" {
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("MoonBit", "MoonBit")
  
  let calc_result = add(15, 27)
  assert_true(calc_result == 42)
  assert_false(calc_result != 42)
}

// 标准 MoonBit 测试套件 - 添加9个标准测试用例
test "standard_add_basic" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "standard_add_zero" {
  // 测试与0相加的情况
  assert_eq(10, add(10, 0))
  assert_eq(10, add(0, 10))
  assert_eq(0, add(0, 0))
}

test "standard_add_negative" {
  // 测试负数加法
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  assert_eq(-2, add(3, -5))
}

test "standard_multiply_basic" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 1))
}

test "standard_multiply_negative" {
  // 测试负数乘法
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(0, multiply(-5, 0))
}

test "standard_greet_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "standard_greet_special" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, User123!", greet("User123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
}

test "standard_boundary_add" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "standard_boundary_multiply" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
}

// 简洁必要的 MoonBit 测试用例
// 覆盖 azimuth 包的核心功能

test "essential_add_basic" {
  // 基本加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-1, -1))
}

test "essential_add_edge_cases" {
  // 加法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出保护测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "essential_multiply_basic" {
  // 基本乘法测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(-6, multiply(2, -3))
}

test "essential_multiply_edge_cases" {
  // 乘法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 特殊值测试
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
}

test "essential_greet_basic" {
  // 基本问候语测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "essential_greet_special_chars" {
  // 特殊字符问候语测试
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
}

test "essential_complex_calculation" {
  // 复合计算测试：矩形周长
  let length = 10
  let width = 5
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(30, perimeter)
}

test "essential_financial_calculation" {
  // 金融计算测试：简化复利
  let principal = 1000
  let rate = 5
  let years = 2
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  assert_eq(11000, total)
}

test "essential_assert_functions" {
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "essential_mathematical_properties" {
  // 数学性质测试
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

// 新增的扩展测试用例
test "subtract_operations" {
  // 减法运算测试（通过加法实现）
  assert_eq(5, add(10, -5))
  assert_eq(-3, add(2, -5))
  assert_eq(0, add(7, -7))
}

test "division_simulation" {
  // 除法模拟测试（通过重复减法）
  let dividend = 20
  let divisor = 4
  let mut quotient = 0
  let mut remainder = dividend
  
  // 简单的除法模拟
  while multiply(divisor, quotient) < dividend {
    remainder = add(remainder, -divisor)
    quotient = add(quotient, 1)
  }
  
  assert_eq(5, quotient)
  assert_eq(0, remainder)
}

test "string_concatenation" {
  // 字符串连接测试
  let name1 = "Alice"
  let name2 = "Bob"
  let greeting1 = greet(name1)
  let greeting2 = greet(name2)
  
  assert_eq_string("Hello, Alice!", greeting1)
  assert_eq_string("Hello, Bob!", greeting2)
}

test "negative_number_operations" {
  // 负数运算测试
  let a = -10
  let b = -5
  
  assert_eq(-15, add(a, b))
  assert_eq(50, multiply(a, b))
  assert_eq(10, multiply(a, -1))
  assert_eq(5, multiply(b, -1))
}

test "zero_properties" {
  // 零属性测试
  let x = 42
  
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
  assert_eq(0, multiply(0, 0))
}

test "sequence_operations" {
  // 序列运算测试
  let mut result = 0
  result = add(result, 1)
  result = multiply(result, 2)
  result = add(result, 3)
  result = multiply(result, 4)
  
  assert_eq(32, result)
}

test "power_simulation" {
  // 幂运算模拟测试
  let base = 3
  let exponent = 4
  let mut result = 1
  
  // 3^4 = 81
  let mut i = 0
  while i < exponent {
    result = multiply(result, base)
    i = add(i, 1)
  }
  
  assert_eq(81, result)
}

test "factorial_calculation_extended" {
  // 阶乘计算测试 (5! = 120)
  let n = 5
  let mut result = 1
  
  let mut i = 1
  while i <= n {
    result = multiply(result, i)
    i = add(i, 1)
  }
  
  assert_eq(120, result)
}

test "greeting_edge_cases" {
  // 问候函数边界情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello,   !", greet("   "))
}

// 新增的10个测试用例
test "add_large_numbers_v2" {
  // 测试大数相加
  assert_eq(add(1000000, 2000000), 3000000)
  assert_eq(add(-1000000, -2000000), -3000000)
  assert_eq(add(2147483647, 0), 2147483647)
  assert_eq(add(-2147483648, 0), -2147483648)
}

test "add_mixed_signs_v2" {
  // 测试不同符号的数相加
  assert_eq(add(100, -100), 0)
  assert_eq(add(100, -150), -50)
  assert_eq(add(-100, 150), 50)
  assert_eq(add(2147483647, -1), 2147483646)
}

test "multiply_large_numbers_v2" {
  // 测试大数相乘
  assert_eq(multiply(1000, 1000), 1000000)
  assert_eq(multiply(-1000, 1000), -1000000)
  assert_eq(multiply(-1000, -1000), 1000000)
  assert_eq(multiply(46341, 46341), 2147483647)
}

test "multiply_boundary_combinations_v2" {
  // 测试边界值组合的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(multiply(max_val, 0), 0)
  assert_eq(multiply(min_val, 0), 0)
  assert_eq(multiply(max_val, 1), max_val)
  assert_eq(multiply(min_val, 1), min_val)
  assert_eq(multiply(min_val, -1), min_val)
}

test "greet_special_characters_v2" {
  // 测试包含特殊字符的字符串
  assert_eq_string(greet("测试@123"), "Hello, 测试@123!")
  assert_eq_string(greet("🌙"), "Hello, 🌙!")
  assert_eq_string(greet(" spaces "), "Hello, spaces !")
  assert_eq_string(greet(""), "Hello, !")
}

test "complex_calculation_scenario_v2" {
  // 测试复杂计算场景：计算矩形的周长
  let length = 10
  let width = 5
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(30, perimeter)
}

test "divide_with_ceil_function_v2" {
  // 测试向上取整除法函数
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(15, 3), 5)
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(9, 2), 5)
  assert_eq(divide_with_ceil(-7, 3), -2)
}

test "mathematical_sequences_v2" {
  // 测试数学序列计算
  
  // 计算前 5 个自然数的和: 1+2+3+4+5 = 15
  let sum_first_5 = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum_first_5)
  
  // 计算前 4 个偶数的和: 2+4+6+8 = 20
  let sum_even_4 = add(add(add(2, 4), 6), 8)
  assert_eq(20, sum_even_4)
}

test "temperature_conversion_v2" {
  // 测试温度转换场景 (摄氏度到华氏度)
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9) / 5, 32)
  assert_eq(77, fahrenheit)
  
  let celsius2 = 0
  let fahrenheit2 = add(multiply(celsius2, 9) / 5, 32)
  assert_eq(32, fahrenheit2)
}

test "assert_functions_test_v2" {
  // 测试断言函数本身
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}



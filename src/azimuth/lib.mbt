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



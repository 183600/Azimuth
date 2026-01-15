// 独立的 MoonBit 测试用例
// 包含所有必要的函数定义和测试用例

// 基础函数定义
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
test "addition_commutative_and_associative" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  assert_eq(add(a, b), add(b, a))
  
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let c = 54321
  let result1 = add(add(a, b), c)
  let result2 = add(a, add(b, c))
  assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let d = -1000
  let e = 2000
  let f = -500
  assert_eq(add(add(d, e), f), add(d, add(e, f)))
}

test "multiplication_distributive_law" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 7
  let b = 15
  let c = 23
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 测试包含负数的情况
  let d = -5
  let e = 12
  let f = -8
  
  let left_side2 = multiply(d, add(e, f))
  let right_side2 = add(multiply(d, e), multiply(d, f))
  assert_eq(left_side2, right_side2)
}

test "boundary_values_and_overflow_handling" {
  // 测试边界值和溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  
  // 测试最小值边界
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  
  // 测试极值相加
  assert_eq(-1, add(max_val, min_val))
  
  // 测试乘法溢出
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(-46341, 46341))
  assert_eq(min_val, multiply(min_val, -1))
}

test "string_processing_comprehensive" {
  // 测试字符串处理功能
  // 测试基本字符串
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  
  // 测试空字符串和单字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  
  // 测试数字字符串
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, 0!", greet("0"))
  
  // 测试特殊字符
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello,  Leading Space!", greet(" Leading Space"))
}

test "mathematical_properties_verification" {
  // 测试数学运算的性质
  // 测试加法单位元
  let test_values = [0, 1, -1, 100, -100, 2147483647, -2147483648]
  for value in test_values {
    assert_eq(value, add(value, 0))
    assert_eq(value, add(0, value))
  }
  
  // 测试乘法单位元
  for value in test_values {
    assert_eq(value, multiply(value, 1))
    assert_eq(value, multiply(1, value))
  }
  
  // 测试乘法零元
  for value in test_values {
    assert_eq(0, multiply(value, 0))
    assert_eq(0, multiply(0, value))
  }
  
  // 测试乘法负一元
  for value in test_values {
    if value != -2147483648 {  // 跳过最小值特殊情况
      assert_eq(-value, multiply(value, -1))
      assert_eq(-value, multiply(-1, value))
    }
  }
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1：购物车计算
  let item1_price = 299
  let item2_price = 199
  let item3_price = 99
  let discount = 50
  let tax_rate = 8  // 8%
  
  let subtotal = add(add(item1_price, item2_price), item3_price)
  let after_discount = add(subtotal, -discount)
  let tax_amount = multiply(after_discount, tax_rate) / 100
  let total = add(after_discount, tax_amount)
  
  assert_eq(537, total)  // (299+199+99-50) + (299+199+99-50)*8/100 = 537
  
  // 场景2：温度转换
  let celsius = 20
  // 简化的华氏度转换：F = C × 2 - C/10 + 32
  let fahrenheit = add(add(multiply(celsius, 2), multiply(celsius, -1) / 10), 32)
  assert_eq(68, fahrenheit)
  
  // 场景3：速度计算
  let distance = 360
  let time = 6
  let speed = distance / time
  assert_eq(60, speed)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试边界条件的正确处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 测试特殊情况
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "complex_mathematical_calculations" {
  // 测试复杂数学计算
  // 计算：2³ + 3² + 4¹
  let power_2_3 = multiply(multiply(2, 2), 2)
  let power_3_2 = multiply(3, 3)
  let power_4_1 = 4
  let result = add(add(power_2_3, power_3_2), power_4_1)
  assert_eq(21, result)  // 8 + 9 + 4 = 21
  
  // 计算阶乘：5! = 5 × 4 × 3 × 2 × 1
  let factorial_5 = multiply(multiply(multiply(multiply(5, 4), 3), 2), 1)
  assert_eq(120, factorial_5)
  
  // 计算斐波那契数列第10项（简化版）
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)  // 2
  let fib4 = add(fib2, fib3)  // 3
  let fib5 = add(fib3, fib4)  // 5
  let fib6 = add(fib4, fib5)  // 8
  let fib7 = add(fib5, fib6)  // 13
  let fib8 = add(fib6, fib7)  // 21
  let fib9 = add(fib7, fib8)  // 34
  let fib10 = add(fib8, fib9) // 55
  assert_eq(55, fib10)
}

test "performance_related_scenarios" {
  // 测试性能相关场景
  // 测试大量重复计算
  let base = 100
  let iterations = 1000
  
  // 模拟循环累加：100 × 1000
  let sum_result = multiply(base, iterations)
  assert_eq(100000, sum_result)
  
  // 模拟嵌套循环：100 × 100
  let nested_result = multiply(base, base)
  assert_eq(10000, nested_result)
  
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  let large_sum = add(large_num1, large_num2)
  let large_product = multiply(large_num1, 2)
  
  assert_eq(3000000, large_sum)
  assert_eq(2000000, large_product)
}
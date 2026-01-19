// Azimuth 项目标准测试套件
// 使用标准 MoonBit 测试语法，完全自包含的测试

// 定义所需的函数
fn add(a : Int, b : Int) -> Int {
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

fn multiply(a : Int, b : Int) -> Int {
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

fn greet(name : String) -> String {
  // 字符串拼接：空字符串会自动处理
  "Hello, " + name + "!"
}

fn divide_with_ceil(dividend : Int, divisor : Int) -> Int {
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  if remainder > 0 { add(quotient, 1) } else { quotient }
}

// 测试辅助函数，提供断言功能
fn assert_eq(expected : Int, actual : Int) -> Unit {
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

fn assert_eq_string(expected : String, actual : String) -> Unit {
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

fn assert_true(condition : Bool) -> Unit {
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

fn assert_false(condition : Bool) -> Unit {
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
test "add_function_basic_tests" {
  // 测试 add 函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(0, 100))
  assert_eq(-50, add(-50, 0))
}

test "add_function_overflow_tests" {
  // 测试 add 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  
  // 负数溢出测试
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  
  // 边界值测试
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic_tests" {
  // 测试 multiply 函数的基本功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(0, multiply(10, 0))
  assert_eq(7, multiply(7, 1))
  assert_eq(-7, multiply(7, -1))
}

test "multiply_function_overflow_tests" {
  // 测试 multiply 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  assert_eq(max_val, multiply(46341, 46341))
  
  // 负数溢出测试
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 最小值特殊处理测试
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_tests" {
  // 测试 greet 函数
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_function_tests" {
  // 测试 divide_with_ceil 函数
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(-11, 3))
  assert_eq(-4, divide_with_ceil(-10, -3))
}

test "complex_calculation_tests" {
  // 测试复合计算
  let a = 12
  let b = 8
  let c = -5
  
  // 计算 (a + b) * c
  let sum = add(a, b)
  let result1 = multiply(sum, c)
  assert_eq(-100, result1)
  
  // 计算 a * b + c
  let product = multiply(a, b)
  let result2 = add(product, c)
  assert_eq(91, result2)
  
  // 计算 (a - b) * (b + c)
  let diff = add(a, -b)
  let sum2 = add(b, c)
  let result3 = multiply(diff, sum2)
  assert_eq(12, result3)
}

test "mathematical_formula_tests" {
  // 测试数学公式计算
  // 计算二次方程 ax² + bx + c，当 x=2 时的值
  let a = 3
  let b = 4
  let c = 5
  let x = 2
  
  // 计算 x²
  let x_squared = multiply(x, x)
  // 计算 ax²
  let ax_squared = multiply(a, x_squared)
  // 计算 bx
  let bx = multiply(b, x)
  // 计算 ax² + bx + c
  let result = add(add(ax_squared, bx), c)
  
  assert_eq(25, result)
}

test "assertion_functions_tests" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq(-42, -42)
  assert_eq_string("test", "test")
  assert_eq_string("", "")
  assert_eq_string("Hello, 世界!", "Hello, 世界!")
}
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

// 向上取整除法函数
pub fn divide_with_ceil(dividend : Int, divisor : Int) -> Int {
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  if remainder > 0 { add(quotient, 1) } else { quotient }
}

// 增强的核心功能测试用例
// 测试 azimuth 包的主要功能

test "add_function_edge_cases" {
  // 测试边界值和特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值测试
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(max_val, add(0, max_val))
  assert_eq(min_val, add(0, min_val))
  
  // 对称性测试
  assert_eq(add(5, 3), add(3, 5))
  assert_eq(add(-5, 3), add(3, -5))
}

test "multiply_function_special_cases" {
  // 测试乘法的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 零乘法测试
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 负一乘法测试
  assert_eq(-5, multiply(-1, 5))
  assert_eq(5, multiply(-1, -5))
  assert_eq(min_val, multiply(-1, min_val)) // 特殊情况：最小值乘以-1会溢出
}

test "greet_function_unicode_handling" {
  // 测试字符串处理和Unicode字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit🚀!", greet("MoonBit🚀"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello,   !", greet("   ")) // 空格测试
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的各种情况
  // 正数测试
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(2, 2))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-9, 3))
  assert_eq(-3, divide_with_ceil(-10, 4))
  
  // 混合符号测试
  assert_eq(-2, divide_with_ceil(5, -3))
  assert_eq(2, divide_with_ceil(-5, 3))
}

test "combined_operations_test" {
  // 测试组合操作
  let a = 10
  let b = 3
  
  // 加法后乘法
  assert_eq(39, multiply(add(a, b), 3))
  
  // 乘法后加法
  assert_eq(33, add(multiply(a, b), 3))
  
  // 复杂表达式
  assert_eq(16, add(multiply(2, 5), divide_with_ceil(10, 3)))
}

test "mathematical_properties" {
  // 测试数学性质
  
  // 加法结合律（在安全范围内）
  assert_eq(add(add(2, 3), 4), add(2, add(3, 4)))
  
  // 乘法结合律（在安全范围内）
  assert_eq(multiply(multiply(2, 3), 4), multiply(2, multiply(3, 4)))
  
  // 分配律（在安全范围内）
  assert_eq(multiply(2, add(3, 4)), add(multiply(2, 3), multiply(2, 4)))
}

test "boundary_comprehensive_test" {
  // 综合边界测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近边界的加法
  assert_eq(max_val, add(max_val - 100, 100))
  assert_eq(max_val, add(max_val - 1000, 1000))
  
  // 接近边界的乘法
  assert_eq(max_val, multiply(46340, 46341)) // 接近sqrt(max_val)
  
  // 最小值处理
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "string_edge_cases" {
  // 字符串边界情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, abcdefghijklmnopqrstuvwxyz!", greet("abcdefghijklmnopqrstuvwxyz"))
}

test "assert_functions_comprehensive" {
  // 断言函数综合测试
  assert_true(1 == 1)
  assert_true(0 == 0)
  assert_true(-1 == -1)
  assert_true(true)
  
  assert_false(1 == 2)
  assert_false(false)
  assert_false(1 > 2)
  
  assert_eq(42, 42)
  assert_eq(-42, -42)
  assert_eq(0, 0)
  
  assert_eq_string("", "")
  assert_eq_string("test", "test")
  assert_eq_string("Hello, World!", "Hello, World!")
}
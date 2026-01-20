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

test "extreme_boundary_combinations" {
  // 极端边界值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的组合运算
  assert_eq(max_val - 1, add(max_val, min_val))
  assert_eq(min_val + 1, add(min_val, max_val))
  
  // 边界值的乘法组合
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
}

test "large_number_operations" {
  // 大数运算测试
  let large_num = 1000000
  let large_neg = -1000000
  
  // 大数加法
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(0, add(large_num, large_neg))
  
  // 大数乘法（调整到范围内）
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(-100000000, multiply(10000, -10000))
  
  // 大数与边界值组合
  assert_eq(2147483647, add(2147483647 - 1000000, 1000000))
}

test "special_string_characters" {
  // 特殊字符串字符处理测试
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, \\\"\\'\\!", greet("\\\"\\'\\!"))
  assert_eq_string("Hello, \u0000\u0000\u0000!", greet("\u0000\u0000\u0000"))
  assert_eq_string("Hello, 中文字符🌟!", greet("中文字符🌟"))
  assert_eq_string("Hello, emoji😀😁😂!", greet("emoji😀😁😂"))
}

test "negative_division_edge_cases" {
  // 负数除法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 负数除以正数
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-2, divide_with_ceil(-3, 2))
  assert_eq(-1073741824, divide_with_ceil(min_val, 2))
  
  // 负数除以负数
  assert_eq(1, divide_with_ceil(-1, -2))
  assert_eq(2, divide_with_ceil(-3, -2))
  
  // 边界值除法
  assert_eq(-1073741824, divide_with_ceil(min_val, 2))
  assert_eq(1073741824, divide_with_ceil(max_val, 2))
}

test "function_chaining_operations" {
  // 函数链式调用测试
  let initial = 5
  
  // 多层函数调用链
  let result1 = multiply(add(initial, 3), 2)
  let result2 = add(multiply(initial, 2), 3)
  let result3 = divide_with_ceil(multiply(add(initial, 1), 3), 2)
  
  assert_eq(16, result1)  // (5 + 3) * 2 = 16
  assert_eq(13, result2)  // 5 * 2 + 3 = 13
  assert_eq(9, result3)   // ceil((5 + 1) * 3 / 2) = ceil(18 / 2) = 9
  
  // 更复杂的链式调用
  let complex_result = add(multiply(divide_with_ceil(10, 3), 2), 1)
  assert_eq(9, complex_result)  // ceil(10/3) * 2 + 1 = 4 * 2 + 1 = 9
}

test "numeric_range_validation" {
  // 数值范围验证测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证加法范围限制
  assert_true(add(max_val, 0) <= max_val)
  assert_true(add(min_val, 0) >= min_val)
  assert_true(add(1000000, 1000000) <= max_val)
  assert_true(add(-1000000, -1000000) >= min_val)
  
  // 验证乘法范围限制
  assert_true(multiply(46340, 46340) <= max_val)
  assert_true(multiply(-46340, 46340) >= min_val)
  
  // 极值验证
  let overflow_add = add(max_val, max_val)
  let overflow_mul = multiply(max_val, 2)
  assert_true(overflow_add == max_val || overflow_add == min_val)
  assert_true(overflow_mul == max_val || overflow_mul == min_val)
}

test "string_length_boundaries" {
  // 字符串长度边界测试
  let empty = ""
  let single = "a"
  let long = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  let _very_long = long + long + long + long + long  // 使用下划线前缀表示未使用
  
  // 不同长度字符串测试
  assert_eq_string("Hello, !", greet(empty))
  assert_eq_string("Hello, a!", greet(single))
  assert_eq_string("Hello, " + long + "!", greet(long))
  
  // 特殊长度字符串
  let spaces = "     "  // 5个空格
  let tabs = "\t\t\t\t\t"  // 5个制表符
  assert_eq_string("Hello,     !", greet(spaces))
  assert_eq_string("Hello, \t\t\t\t\t!", greet(tabs))
}

test "mixed_operation_precision" {
  // 混合运算精度测试
  // 测试不同数值组合的精度保持
  assert_eq(6, add(multiply(2, 3), 0))
  assert_eq(8, add(multiply(2, 3), 2))
  assert_eq(7, divide_with_ceil(multiply(2, 3), 1))
  assert_eq(4, divide_with_ceil(multiply(2, 3), 2))
  
  // 测试运算顺序对结果的影响
  let val1 = add(multiply(10, 2), divide_with_ceil(5, 2))
  let val2 = multiply(10, add(2, divide_with_ceil(5, 20)))
  assert_eq(24, val1)  // 20 + ceil(5/2) = 20 + 3 = 23
  assert_eq(21, val2)  // 10 * (2 + ceil(5/20)) = 10 * (2 + 1) = 30
  
  // 测试精度保持
  assert_eq(0, add(multiply(1000, 0), divide_with_ceil(0, 1000)))
}

test "error_recovery_mechanisms" {
  // 错误恢复机制测试
  let max_val = 2147483647
  let _min_val = -2147483648  // 使用下划线前缀表示未使用
  
  // 测试溢出后的函数仍然可用
  let _ = add(max_val, 1)  // 这会溢出
  let normal_add = add(5, 3)  // 验证正常加法仍然工作
  assert_eq(8, normal_add)
  
  let _ = multiply(max_val, 2)  // 这会溢出
  let normal_mul = multiply(5, 3)  // 验证正常乘法仍然工作
  assert_eq(15, normal_mul)
  
  // 测试断言失败不影响后续测试
  // 注意：实际的断言失败会终止程序，这里主要测试断言函数本身的逻辑
  assert_true(true)  // 这个应该通过
  assert_false(false)  // 这个应该通过
  assert_eq(1, 1)  // 这个应该通过
  assert_eq_string("test", "test")  // 这个应该通过
}

test "memory_safety_operations" {
  // 内存安全相关操作测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大量操作不会导致内存问题
  for i = 0; i < 100; i = i + 1 {
    let _ = add(i, i)
    let _ = multiply(i, 2)
    let _ = divide_with_ceil(i * 10 + 5, 3)
  }
  
  // 测试极值操作的稳定性
  let result1 = add(max_val, 0)
  let result2 = add(min_val, 0)
  let result3 = multiply(max_val, 1)
  let result4 = multiply(min_val, 1)
  let result5 = multiply(0, max_val)
  let result6 = multiply(0, min_val)
  
  assert_eq(max_val, result1)
  assert_eq(min_val, result2)
  assert_eq(max_val, result3)
  assert_eq(min_val, result4)
  assert_eq(0, result5)
  assert_eq(0, result6)
  
  // 测试字符串操作的内存安全性
  let _ = greet("")
  let _ = greet("a")
  let _ = greet("test")
  let _ = greet("very long string with many characters")
  
  // 验证最终状态
  assert_eq(198, add(99, 99))
  assert_eq(10000, multiply(100, 100))
}

// 新增的标准增强测试用例
test "add_overflow_protection_positive" {
  // 测试正数加法的溢出保护
  let max_val = 2147483647
  
  // 接近最大值的加法
  assert_eq(max_val, add(max_val - 1, 1))
  assert_eq(max_val, add(max_val - 100, 100))
  assert_eq(max_val, add(max_val - 1000, 1000))
  
  // 确保正常范围内的加法正确
  assert_eq(100, add(50, 50))
  assert_eq(0, add(100, -100))
}

test "add_overflow_protection_negative" {
  // 测试负数加法的溢出保护
  let min_val = -2147483648
  
  // 接近最小值的加法
  assert_eq(min_val, add(min_val + 1, -1))
  assert_eq(min_val, add(min_val + 100, -100))
  assert_eq(min_val, add(min_val + 1000, -1000))
  
  // 确保正常范围内的加法正确
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(-100, 100))
}

test "multiply_precision_boundary" {
  // 测试乘法精度边界
  let sqrt_max = 46340  // sqrt(2147483647) 的整数部分
  
  // 边界值乘法
  assert_eq(2147395600, multiply(sqrt_max, sqrt_max))
  assert_eq(2147483647, multiply(sqrt_max, sqrt_max + 1))
  
  // 负数乘法
  assert_eq(-2147395600, multiply(sqrt_max, -sqrt_max))
  assert_eq(-2147483647, multiply(sqrt_max, -(sqrt_max + 1)))
}

test "multiply_zero_and_one_cases" {
  // 测试乘法的零和一特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 零乘法
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  assert_eq(0, multiply(0, 0))
  
  // 一乘法
  assert_eq(max_val, multiply(1, max_val))
  assert_eq(min_val, multiply(1, min_val))
  assert_eq(5, multiply(1, 5))
  
  // 负一乘法
  assert_eq(-max_val, multiply(-1, max_val))
  assert_eq(min_val, multiply(-1, min_val))  // 特殊情况：最小值乘以-1
}

test "greet_function_special_characters" {
  // 测试 greet 函数的特殊字符处理
  assert_eq_string("Hello, \\n!", greet("\\n"))
  assert_eq_string("Hello, \\t!", greet("\\t"))
  assert_eq_string("Hello, \\\\!", greet("\\"))
  assert_eq_string("Hello, \\\"!", greet("\""))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  // 负数除以正数
  assert_eq(-1, divide_with_ceil(-1, 3))
  assert_eq(-2, divide_with_ceil(-4, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-3, divide_with_ceil(-7, 3))
  
  // 负数除以负数
  assert_eq(1, divide_with_ceil(-1, -3))
  assert_eq(2, divide_with_ceil(-4, -3))
  assert_eq(2, divide_with_ceil(-5, -3))
  assert_eq(3, divide_with_ceil(-7, -3))
}

test "divide_with_ceil_exact_division" {
  // 测试能够整除的情况
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(-5, divide_with_ceil(-10, 2))
  assert_eq(5, divide_with_ceil(10, -2))
  assert_eq(-5, divide_with_ceil(-10, -2))
  
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(-1, divide_with_ceil(-5, 5))
}

test "complex_mathematical_expressions" {
  // 测试复杂的数学表达式
  // 嵌套运算
  assert_eq(25, add(multiply(3, 5), multiply(2, 5)))
  assert_eq(20, multiply(add(3, 2), 4))
  assert_eq(8, divide_with_ceil(add(multiply(2, 3), 2), 2))
  
  // 多层嵌套
  assert_eq(15, add(multiply(divide_with_ceil(10, 2), 2), 5))
  assert_eq(12, multiply(add(divide_with_ceil(7, 3), 2), 3))
}

test "assertion_functions_edge_cases" {
  // 测试断言函数的边界情况
  // 极值比较
  assert_true(2147483647 == 2147483647)
  assert_true(-2147483648 == -2147483648)
  assert_false(2147483647 == -2147483648)
  
  // 字符串边界情况
  assert_eq_string("", "")
  assert_eq_string("a", "a")
  assert_eq_string("Hello, World!", "Hello, World!")
  
  // 布尔值边界
  assert_true(true)
  assert_false(false)
}
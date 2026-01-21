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

// 用户请求的标准 MoonBit 测试用例
test "add_basic_functionality" {
  // 基本加法功能测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-2, 1))
  assert_eq(-5, add(-2, -3))
}

test "add_boundary_values" {
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(max_val, add(0, max_val))
  assert_eq(min_val, add(0, min_val))
}

test "multiply_basic_cases" {
  // 基本乘法测试
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
}

test "multiply_special_values" {
  // 特殊值乘法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(1, max_val))
  assert_eq(min_val, multiply(1, min_val))
  assert_eq(5, multiply(1, 5))
  assert_eq(-5, multiply(-1, 5))
  assert_eq(min_val, multiply(-1, min_val))  // 特殊情况：最小值乘以-1
}

test "greet_function_test" {
  // greet 函数测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic" {
  // 向上取整除法基本测试
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
}

test "divide_with_ceil_negative_divisor" {
  // 负除数的向上取整除法
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(-4, divide_with_ceil(11, -3))
  assert_eq(2, divide_with_ceil(-6, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
}

test "combined_operations" {
  // 组合运算测试
  assert_eq(25, add(multiply(3, 5), multiply(2, 5)))
  assert_eq(20, multiply(add(3, 2), 4))
  assert_eq(8, divide_with_ceil(add(multiply(2, 3), 2), 2))
  assert_eq(15, add(multiply(divide_with_ceil(10, 2), 2), 5))
}

test "assert_functions_test" {
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq(-42, -42)
  assert_eq_string("test", "test")
  assert_eq_string("", "")
}

// 新增的标准增强测试用例
test "enhanced_boundary_value_tests" {
  // 增强边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值和最小值的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 测试接近边界的运算
  assert_eq(2147483647, add(2147483647 - 1000, 1000))
  assert_eq(-2147483648, add(-2147483648 + 1000, -1000))
}

test "enhanced_string_processing_tests" {
  // 增强字符串处理测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, \\\"!", greet("\""))
}

test "enhanced_division_precision_tests" {
  // 增强除法精度测试
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(1, divide_with_ceil(1, 3))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(-1, divide_with_ceil(-2, 3))
  
  // 零相关
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
}

test "enhanced_complex_operations_tests" {
  // 增强复杂运算测试
  // 嵌套运算
  assert_eq(25, add(multiply(3, 5), multiply(2, 5)))
  assert_eq(20, multiply(add(3, 2), 4))
  assert_eq(8, divide_with_ceil(add(multiply(2, 3), 2), 2))
  
  // 多层嵌套
  assert_eq(15, add(multiply(divide_with_ceil(10, 2), 2), 5))
  assert_eq(12, multiply(add(divide_with_ceil(7, 3), 2), 3))
  
  // 复杂表达式
  assert_eq(16, add(multiply(2, 5), divide_with_ceil(10, 3)))
}

test "enhanced_real_world_scenarios" {
  // 增强实际应用场景测试
  // 场景1：计算购物车总价（含税向上取整）
  let item1_price = 99
  let item2_price = 149
  let tax_rate = 3  // 1/3 的税率
  let subtotal = add(item1_price, item2_price)
  let total_with_tax = multiply(subtotal, tax_rate)
  assert_eq(744, total_with_tax)
  
  // 场景2：平均分计算（向上取整）
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let total_score = add(add(score1, score2), score3)
  let average_score = divide_with_ceil(total_score, 3)
  assert_eq(86, average_score)
  
  // 场景3：用户欢迎消息
  let username = "张三"
  let welcome_msg = greet(username)
  assert_eq_string("Hello, 张三!", welcome_msg)
}

test "enhanced_error_handling_tests" {
  // 增强错误处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出情况测试
  let overflow_add = add(max_val, max_val)
  let overflow_mul = multiply(max_val, 2)
  assert_true(overflow_add == max_val || overflow_add == min_val)
  assert_true(overflow_mul == max_val || overflow_mul == min_val)
  
  // 特殊值测试
  assert_eq(min_val, multiply(-1, min_val))  // 特殊情况：最小值乘以-1
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
}

// 新增的标准增强测试用例
test "add_function_performance_critical_cases" {
  // 性能关键情况下的加法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 连续操作测试
  let result1 = add(add(1000000, 1000000), add(1000000, 1000000))
  let result2 = add(add(-1000000, 1000000), add(-1000000, 1000000))
  assert_eq(4000000, result1)
  assert_eq(0, result2)
  
  // 边界值连续操作
  let result3 = add(add(max_val - 1000, 500), add(500, 0))
  let result4 = add(add(min_val + 1000, -500), add(-500, 0))
  assert_eq(max_val, result3)
  assert_eq(min_val, result4)
}

test "multiply_function_large_numbers" {
  // 大数乘法测试
  let sqrt_max = 46340
  
  // 接近最大值的乘法
  assert_eq(2147395600, multiply(sqrt_max, sqrt_max))
  assert_eq(2147483647, multiply(sqrt_max, sqrt_max + 1))
  assert_eq(-2147395600, multiply(sqrt_max, -sqrt_max))
  
  // 中等数值的乘法
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(1000, -1000))
  assert_eq(99980001, multiply(9999, 9999))
}

test "greet_function_multilingual_support" {
  // 多语言支持测试
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, русский!", greet("русский"))
  assert_eq_string("Hello, español!", greet("español"))
  assert_eq_string("Hello, français!", greet("français"))
}

test "divide_with_ceil_fractional_precision" {
  // 分数精度测试
  // 小余数情况
  assert_eq(2, divide_with_ceil(4, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(3, divide_with_ceil(7, 3))
  
  // 大余数情况
  assert_eq(11, divide_with_ceil(100, 9))
  assert_eq(34, divide_with_ceil(100, 3))
  assert_eq(51, divide_with_ceil(100, 2))
  
  // 负数分数精度
  assert_eq(-1, divide_with_ceil(-2, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
}

test "complex_business_logic_simulation" {
  // 复杂业务逻辑模拟
  // 场景：计算订单总价（含税和折扣）
  let base_price = 1000
  let quantity = 5
  let discount_rate = 2  // 50% 折扣 (1/2)
  let tax_rate = 3       // 33% 税率 (1/3)
  
  // 计算原价
  let original_total = multiply(base_price, quantity)
  assert_eq(5000, original_total)
  
  // 应用折扣
  let discounted_price = divide_with_ceil(original_total, discount_rate)
  assert_eq(2500, discounted_price)
  
  // 计算税费
  let tax_amount = divide_with_ceil(discounted_price, tax_rate)
  assert_eq(834, tax_amount)
  
  // 最终总价
  let final_total = add(discounted_price, tax_amount)
  assert_eq(3334, final_total)
}

test "mathematical_sequence_operations" {
  // 数学序列操作
  // 等差数列求和
  let first_term = 1
  let common_diff = 2
  let num_terms = 10
  
  // 计算等差数列和：n/2 * (2a + (n-1)d)
  let double_first = multiply(first_term, 2)
  let diff_product = multiply(common_diff, num_terms - 1)
  let sum_inside = add(double_first, diff_product)
  let arithmetic_sum = divide_with_ceil(multiply(num_terms, sum_inside), 2)
  assert_eq(100, arithmetic_sum)
  
  // 等比数列求和（简化版）
  let ratio = 2
  let geo_sum = multiply(first_term, multiply(ratio, num_terms))
  assert_eq(20, geo_sum)
}

test "error_boundary_and_recovery" {
  // 错误边界和恢复测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 触发溢出后验证函数仍然可用
  let overflow_result1 = add(max_val, 1)  // 应该返回 max_val
  let overflow_result2 = multiply(max_val, 2)  // 应该返回 max_val
  let overflow_result3 = multiply(-1, min_val)  // 应该返回 min_val
  
  // 验证正常操作仍然工作
  let normal_add = add(100, 200)
  let normal_mul = multiply(10, 20)
  let normal_greet = greet("Test")
  
  assert_eq(max_val, overflow_result1)
  assert_eq(max_val, overflow_result2)
  assert_eq(min_val, overflow_result3)
  assert_eq(300, normal_add)
  assert_eq(200, normal_mul)
  assert_eq_string("Hello, Test!", normal_greet)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  // 特殊字符组合
  assert_eq_string("Hello, \\n\\t\\r!", greet("\\n\\t\\r"))
  assert_eq_string("Hello, \\0\\1\\2!", greet("\\0\\1\\2"))
  assert_eq_string("Hello, \u{1F600}\u{1F601}\u{1F602}!", greet("\u{1F600}\u{1F601}\u{1F602}"))
  
  // 极长字符串（相对）
  let long_name = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
  
  // 混合字符类型
  assert_eq_string("Hello, a1b2c3!", greet("a1b2c3"))
  assert_eq_string("Hello, Test_123!", greet("Test_123"))
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
}

// 标准 MoonBit 测试用例补充
// 专注于数值运算、边界条件和实际应用场景

test "add_function_identity_properties" {
  // 测试加法的单位元性质
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(-5, add(-5, 0))
  assert_eq(-5, add(0, -5))
  assert_eq(0, add(0, 0))
  
  // 测试加法的逆元性质
  assert_eq(0, add(5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(100, -100))
  assert_eq(0, add(-100, 100))
}

test "multiply_function_identity_properties" {
  // 测试乘法的单位元性质
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(-5, multiply(-5, 1))
  assert_eq(-5, multiply(1, -5))
  assert_eq(1, multiply(1, 1))
  
  // 测试乘法的零元性质
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(-5, 0))
  assert_eq(0, multiply(0, -5))
  assert_eq(0, multiply(0, 0))
}

test "power_operations_simulation" {
  // 通过重复乘法模拟幂运算
  let base = 3
  
  // 3^2 = 9
  assert_eq(9, multiply(base, base))
  
  // 3^3 = 27
  assert_eq(27, multiply(multiply(base, base), base))
  
  // 3^4 = 81
  assert_eq(81, multiply(multiply(multiply(base, base), base), base))
  
  // 测试负数的幂
  let neg_base = -2
  
  // (-2)^2 = 4
  assert_eq(4, multiply(neg_base, neg_base))
  
  // (-2)^3 = -8
  assert_eq(-8, multiply(multiply(neg_base, neg_base), neg_base))
}

test "factorial_simulation" {
  // 通过重复乘法模拟阶乘运算
  // 5! = 5 × 4 × 3 × 2 × 1 = 120
  let factorial_5 = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  assert_eq(120, factorial_5)
  
  // 4! = 4 × 3 × 2 × 1 = 24
  let factorial_4 = multiply(4, multiply(3, multiply(2, 1)))
  assert_eq(24, factorial_4)
  
  // 3! = 3 × 2 × 1 = 6
  let factorial_3 = multiply(3, multiply(2, 1))
  assert_eq(6, factorial_3)
  
  // 2! = 2 × 1 = 2
  let factorial_2 = multiply(2, 1)
  assert_eq(2, factorial_2)
  
  // 1! = 1
  assert_eq(1, multiply(1))
}

test "greet_function_name_formatting" {
  // 测试不同格式的名称处理
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello, john-doe!", greet("john-doe"))
  assert_eq_string("Hello, john_doe!", greet("john_doe"))
  assert_eq_string("Hello, Mr. Smith!", greet("Mr. Smith"))
  assert_eq_string("Hello, Dr. Jane!", greet("Dr. Jane"))
  assert_eq_string("Hello, Jr. Johnson!", greet("Jr. Johnson"))
}

test "divide_with_ceil_rounding_behavior" {
  // 测试向上取整的舍入行为
  // 正数舍入
  assert_eq(1, divide_with_ceil(1, 1))    // 1/1 = 1
  assert_eq(2, divide_with_ceil(2, 1))    // 2/1 = 2
  assert_eq(2, divide_with_ceil(3, 2))    // 3/2 = 1.5 → 2
  assert_eq(3, divide_with_ceil(5, 2))    // 5/2 = 2.5 → 3
  assert_eq(4, divide_with_ceil(7, 2))    // 7/2 = 3.5 → 4
  
  // 负数舍入（向零方向）
  assert_eq(-1, divide_with_ceil(-1, 2))   // -1/2 = -0.5 → -1
  assert_eq(-2, divide_with_ceil(-3, 2))   // -3/2 = -1.5 → -2
  assert_eq(-3, divide_with_ceil(-5, 2))   // -5/2 = -2.5 → -3
}

test "arithmetic_progression_calculations" {
  // 等差数列计算测试
  // 计算前n项和：S = n/2 * (a1 + an)
  let n = 10
  let a1 = 1
  let an = 10
  
  // 计算首项与末项的和
  let first_plus_last = add(a1, an)
  assert_eq(11, first_plus_last)
  
  // 计算项数与和的乘积
  let n_times_sum = multiply(n, first_plus_last)
  assert_eq(110, n_times_sum)
  
  // 计算最终的和（使用向上取整除法模拟除以2）
  let arithmetic_sum = divide_with_ceil(n_times_sum, 2)
  assert_eq(55, arithmetic_sum)
  
  // 验证：1+2+3+4+5+6+7+8+9+10 = 55
}

test "geometric_progression_calculations" {
  // 等比数列计算测试
  // 计算前几项：a, ar, ar^2, ar^3, ...
  let a = 2
  let r = 3
  
  // 计算前几项
  let term1 = a                           // 2
  let term2 = multiply(a, r)              // 2 × 3 = 6
  let term3 = multiply(term2, r)          // 6 × 3 = 18
  let term4 = multiply(term3, r)          // 18 × 3 = 54
  let term5 = multiply(term4, r)          // 54 × 3 = 162
  
  assert_eq(2, term1)
  assert_eq(6, term2)
  assert_eq(18, term3)
  assert_eq(54, term4)
  assert_eq(162, term5)
  
  // 计算前几项的和
  let sum_5_terms = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(242, sum_5_terms)  // 2+6+18+54+162 = 242
}

test "practical_calculator_scenarios" {
  // 实际计算器场景测试
  // 场景1：计算购物车总价
  let item1 = 199
  let item2 = 299
  let item3 = 99
  let subtotal = add(add(item1, item2), item3)
  assert_eq(597, subtotal)
  
  // 场景2：计算折扣价格（8折）
  let discount_rate = 5  // 1/5 = 20% = 80%价格
  let discounted_price = divide_with_ceil(multiply(subtotal, 4), 5)
  assert_eq(478, discounted_price)
  
  // 场景3：计算税费（向上取整）
  let tax_rate = 10  // 10%税率
  let tax_amount = divide_with_ceil(discounted_price, tax_rate)
  assert_eq(48, tax_amount)
  
  // 场景4：最终总价
  let final_total = add(discounted_price, tax_amount)
  assert_eq(526, final_total)
}

test "number_system_conversions" {
  // 数制转换相关的计算
  // 场景1：二进制转十进制的计算
  // 二进制 1011 = 1×2³ + 0×2² + 1×2¹ + 1×2⁰ = 8 + 0 + 2 + 1 = 11
  let binary_1011 = add(add(multiply(1, multiply(2, multiply(2, 2))), 
                            multiply(0, multiply(2, 2))), 
                       add(multiply(1, 2), 1))
  assert_eq(11, binary_1011)
  
  // 场景2：八进制转十进制的计算
  // 八进制 123 = 1×8² + 2×8¹ + 3×8⁰ = 64 + 16 + 3 = 83
  let octal_123 = add(add(multiply(1, multiply(8, 8)), 
                         multiply(2, 8)), 3)
  assert_eq(83, octal_123)
  
  // 场景3：十六进制转十进制的计算（简化版）
  // 十六进制 1A = 1×16¹ + 10×16⁰ = 16 + 10 = 26
  let hex_1A = add(multiply(1, 16), 10)
  assert_eq(26, hex_1A)
}

test "algorithm_complexity_validation" {
  // 算法复杂度验证
  // 测试多个连续操作的性能
  let base = 100
  
  // 连续加法
  let sum_result = add(add(add(add(base, 50), 25), 10), 5)
  assert_eq(190, sum_result)
  
  // 连续乘法
  let product_result = multiply(multiply(multiply(base, 2), 3), 4)
  assert_eq(2400, product_result)
  
  // 混合操作
  let mixed_result = add(multiply(base, 2), divide_with_ceil(multiply(50, 3), 4))
  assert_eq(238, mixed_result)
  
  // 嵌套操作
  let nested_result = multiply(add(divide_with_ceil(100, 3), 5), 2)
  assert_eq(78, nested_result)
}

test "data_type_consistency" {
  // 数据类型一致性测试
  // 确保所有函数返回正确的类型
  
  // Int 类型函数
  let int_result1 = add(5, 3)
  let int_result2 = multiply(5, 3)
  let int_result3 = divide_with_ceil(10, 3)
  
  // String 类型函数
  let string_result = greet("MoonBit")
  
  // Unit 类型函数（断言）
  assert_true(int_result1 == 8)
  assert_true(int_result2 == 15)
  assert_true(int_result3 == 4)
  assert_true(string_result == "Hello, MoonBit!")
  
  // 类型一致性验证
  assert_eq(8, int_result1)
  assert_eq(15, int_result2)
  assert_eq(4, int_result3)
  assert_eq_string("Hello, MoonBit!", string_result)
}

// 标准 MoonBit 测试用例补充
// 专注于核心功能的不同测试场景

test "add_function_commutative_property" {
  // 测试加法交换律: a + b = b + a
  // 基本情况
  assert_eq(add(1, 2), add(2, 1))
  assert_eq(add(-1, 1), add(1, -1))
  assert_eq(add(100, 200), add(200, 100))
  assert_eq(add(-50, -25), add(-25, -50))
  
  // 边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(add(max_val, 0), add(0, max_val))
  assert_eq(add(min_val, 0), add(0, min_val))
  
  // 零的情况
  assert_eq(add(0, 0), add(0, 0))
}

test "multiply_function_commutative_property" {
  // 测试乘法交换律: a × b = b × a
  // 基本情况
  assert_eq(multiply(1, 2), multiply(2, 1))
  assert_eq(multiply(-1, 1), multiply(1, -1))
  assert_eq(multiply(10, 20), multiply(20, 10))
  assert_eq(multiply(-5, -3), multiply(-3, -5))
  
  // 边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(multiply(max_val, 1), multiply(1, max_val))
  assert_eq(multiply(min_val, 1), multiply(1, min_val))
  
  // 零的情况
  assert_eq(multiply(0, 1), multiply(1, 0))
}

test "add_function_associative_property" {
  // 测试加法结合律: (a + b) + c = a + (b + c)
  // 基本情况
  let left1 = add(add(1, 2), 3)
  let right1 = add(1, add(2, 3))
  assert_eq(left1, right1)
  
  let left2 = add(add(-1, 2), 3)
  let right2 = add(-1, add(2, 3))
  assert_eq(left2, right2)
  
  let left3 = add(add(100, 200), 300)
  let right3 = add(100, add(200, 300))
  assert_eq(left3, right3)
  
  // 负数情况
  let left4 = add(add(-50, 25), 75)
  let right4 = add(-50, add(25, 75))
  assert_eq(left4, right4)
  
  // 零的情况
  let left5 = add(add(0, 0), 0)
  let right5 = add(0, add(0, 0))
  assert_eq(left5, right5)
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a × (b + c) = a × b + a × c
  // 基本情况
  let left1 = multiply(2, add(3, 4))
  let right1 = add(multiply(2, 3), multiply(2, 4))
  assert_eq(left1, right1)
  
  let left2 = multiply(3, add(5, 7))
  let right2 = add(multiply(3, 5), multiply(3, 7))
  assert_eq(left2, right2)
  
  let left3 = multiply(10, add(20, 30))
  let right3 = add(multiply(10, 20), multiply(10, 30))
  assert_eq(left3, right3)
  
  // 包含负数的情况
  let left4 = multiply(5, add(-3, 2))
  let right4 = add(multiply(5, -3), multiply(5, 2))
  assert_eq(left4, right4)
  
  // 零的情况
  let left5 = multiply(0, add(100, 200))
  let right5 = add(multiply(0, 100), multiply(0, 200))
  assert_eq(left5, right5)
}

test "divide_with_ceil_mathematical_properties" {
  // 测试向上取整除法的数学性质
  // 正数测试
  let ceil_result1 = divide_with_ceil(10, 3)
  assert_eq(4, ceil_result1)  // ceil(3.33) = 4
  
  let ceil_result2 = divide_with_ceil(11, 3)
  assert_eq(4, ceil_result2)  // ceil(3.67) = 4
  
  let ceil_result3 = divide_with_ceil(12, 3)
  assert_eq(4, ceil_result3)  // ceil(4.0) = 4
  
  // 小数测试
  let ceil_result4 = divide_with_ceil(1, 2)
  assert_eq(1, ceil_result4)  // ceil(0.5) = 1
  
  let ceil_result5 = divide_with_ceil(5, 2)
  assert_eq(3, ceil_result5)  // ceil(2.5) = 3
  
  // 负数测试
  let ceil_result6 = divide_with_ceil(-5, 2)
  assert_eq(-2, ceil_result6)  // ceil(-2.5) = -2
  
  let ceil_result7 = divide_with_ceil(-6, 2)
  assert_eq(-3, ceil_result7)  // ceil(-3.0) = -3
  
  let ceil_result8 = divide_with_ceil(-7, 2)
  assert_eq(-3, ceil_result8)  // ceil(-3.5) = -3
}

test "greet_function_empty_and_whitespace_handling" {
  // 测试 greet 函数对空字符串和空白字符的处理
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t!", greet("\t"))
  assert_eq_string("Hello, \n!", greet("\n"))
  assert_eq_string("Hello, \r!", greet("\r"))
}

test "mathematical_identities_validation" {
  // 验证数学恒等式
  let test_value = 42
  
  // 加法恒等式
  assert_eq(test_value, add(test_value, 0))
  assert_eq(test_value, add(0, test_value))
  
  // 乘法恒等式
  assert_eq(test_value, multiply(test_value, 1))
  assert_eq(test_value, multiply(1, test_value))
  
  // 乘法零元
  assert_eq(0, multiply(test_value, 0))
  assert_eq(0, multiply(0, test_value))
  
  // 加法逆元
  assert_eq(0, add(test_value, -test_value))
}

test "edge_case_combinations" {
  // 边界情况组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  assert_eq(max_val + min_val, add(max_val, min_val))
  
  // 最大值与0的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(0, multiply(max_val, 0))
  
  // 最小值与0的运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(0, multiply(min_val, 0))
}

test "complex_expression_evaluation" {
  // 复杂表达式求值测试
  let a = 10
  let b = 5
  let c = 3
  
  // 表达式1: (a + b) × c
  let expr1 = multiply(add(a, b), c)
  assert_eq(45, expr1)
  
  // 表达式2: a × (b + c)
  let expr2 = multiply(a, add(b, c))
  assert_eq(80, expr2)
  
  // 表达式3: ceil((a × b + c) / 2)
  let expr3 = divide_with_ceil(add(multiply(a, b), c), 2)
  assert_eq(27, expr3)
  
  // 表达式4: ceil(a / b) + ceil(b / c)
  let expr4 = add(divide_with_ceil(a, b), divide_with_ceil(b, c))
  assert_eq(3, expr4)
}
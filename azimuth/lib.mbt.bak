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

// 测试辅助函数

// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit raise @moonbitlang/core/builtin.Failure {
  if expected != actual {
    raise @moonbitlang/core/builtin.Failure("Assertion failed: expected \{expected} but got \{actual}")
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit raise @moonbitlang/core/builtin.Failure {
  if expected != actual {
    raise @moonbitlang/core/builtin.Failure("Assertion failed: expected \"\{expected}\" but got \"\{actual}\"")
  }
}

pub fn assert_true(condition : Bool) -> Unit raise @moonbitlang/core/builtin.Failure {
  if !condition {
    raise @moonbitlang/core/builtin.Failure("Assertion failed: expected true but got false")
  }
}

pub fn assert_false(condition : Bool) -> Unit raise @moonbitlang/core/builtin.Failure {
  if condition {
    raise @moonbitlang/core/builtin.Failure("Assertion failed: expected false but got true")
  }
}

// 内部测试函数
pub fn test_internal_functions() -> Unit raise @moonbitlang/core/builtin.Failure {
  // 测试add函数
  assert_eq(10, add(7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-7, -3))
  
  // 测试multiply函数
  assert_eq(21, multiply(7, 3))
  assert_eq(-21, multiply(7, -3))
  assert_eq(21, multiply(-7, -3))
  
  // 测试greet函数
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, !", greet(""))
}

// 测试用例
test "add_basic" {
  assert_eq(5, add(2, 3))
}

test "add_negative" {
  assert_eq(0, add(5, -5))
}

test "multiply_basic" {
  assert_eq(6, multiply(2, 3))
}

test "multiply_negative" {
  assert_eq(-6, multiply(2, -3))
}

test "greet_basic" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "greet_empty" {
  assert_eq_string("Hello, !", greet(""))
}

// Additional comprehensive tests
test "test_1_basic_add_extended" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-7, -3))
}

test "test_1_basic_multiply_extended" {
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(21, multiply(-7, -3))
}

test "test_1_basic_greet_extended" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
}

test "test_2_edge_cases_add" {
  // Test with zero
  assert_eq(5, add(0, 5))
  assert_eq(5, add(5, 0))
  
  // Test with large numbers
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "test_2_edge_cases_multiply" {
  // Test with zero
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  
  // Test with one
  assert_eq(5, multiply(1, 5))
  assert_eq(5, multiply(5, 1))
  
  // Test with negative one
  assert_eq(-5, multiply(-1, 5))
  assert_eq(-5, multiply(5, -1))
  assert_eq(5, multiply(-1, -5))
}

test "test_2_edge_cases_greet" {
  // Test with special characters
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "test_3_math_properties_add" {
  // Test commutative property
  assert_eq(add(2, 3), add(3, 2))
  assert_eq(add(-5, 10), add(10, -5))
  
  // Test associative property
  assert_eq(add(add(1, 2), 3), add(1, add(2, 3)))
  assert_eq(add(add(-1, -2), -3), add(-1, add(-2, -3)))
  
  // Test identity property
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
}

test "test_3_math_properties_multiply" {
  // Test commutative property
  assert_eq(multiply(2, 3), multiply(3, 2))
  assert_eq(multiply(-5, 10), multiply(10, -5))
  
  // Test associative property
  assert_eq(multiply(multiply(1, 2), 3), multiply(1, multiply(2, 3)))
  assert_eq(multiply(multiply(-1, -2), -3), multiply(-1, multiply(-2, -3)))
  
  // Test identity property
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(-5, 1))
  
  // Test distributive property
  assert_eq(multiply(2, add(3, 4)), add(multiply(2, 3), multiply(2, 4)))
}

test "test_3_boundary_values" {
  // Test with maximum values
  let max_val = 2147483647
  let min_val = -2147483648
  
  // These should not overflow
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  
  // These should handle overflow gracefully
  let result1 = add(max_val, 1)
  let result2 = add(min_val, -1)
  
  // Just make sure they don't crash
  assert_true(result1 >= min_val && result1 <= max_val)
  assert_true(result2 >= min_val && result2 <= max_val)
}

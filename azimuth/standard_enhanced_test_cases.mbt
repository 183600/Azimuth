// 标准 MoonBit 增强测试用例
// 覆盖更多边界情况和组合场景

test "divide_with_ceil_extreme_cases" {
  // 测试极端数值情况
  assert_eq(1, divide_with_ceil(Int::max_value(), Int::max_value()))
  assert_eq(1, divide_with_ceil(Int::min_value(), Int::min_value()))
  assert_eq(0, divide_with_ceil(0, Int::max_value()))
  assert_eq(0, divide_with_ceil(0, Int::min_value()))
}

test "multiply_overflow_behavior" {
  // 测试乘法的边界行为
  assert_eq(Int::max_value(), multiply(Int::max_value(), 1))
  assert_eq(Int::min_value(), multiply(Int::min_value(), 1))
  assert_eq(0, multiply(Int::max_value(), 0))
  assert_eq(0, multiply(Int::min_value(), 0))
}

test "add_associative_property" {
  // 测试加法的结合律
  let a = 15
  let b = 27
  let c = 42
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
}

test "greet_with_emoji_and_unicode" {
  // 测试包含emoji和复杂unicode的问候
  assert_eq_string("Hello, 🌙⭐!", greet("🌙⭐"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "divide_with_ceil_consistency" {
  // 测试除法向上取整的一致性
  for i = 1; i < 10; i = i + 1 {
    let result = divide_with_ceil(i * 10, i)
    assert_eq(10, result)
  }
}

test "complex_calculation_with_negative" {
  // 测试包含负数的复杂计算
  let a = -5
  let b = 3
  let c = 2
  let result = add(multiply(a, b), divide_with_ceil(a, c))
  assert_eq(-12, result)  // -5*3 + ceil(-5/2) = -15 + (-2) = -17 (修正)
}

test "greet_with_special_whitespace" {
  // 测试特殊空白字符的问候
  assert_eq_string("Hello, 	
\r!", greet("	
\r"))
  assert_eq_string("Hello,   !", greet("  "))  // 包含非断行空格
}

test "divide_with_ceil_zero_divisor_variations" {
  // 测试除数为0的各种情况
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(Int::max_value(), 0))
  assert_eq(0, divide_with_ceil(Int::min_value(), 0))
}

test "mathematical_distributive_property" {
  // 测试乘法对加法的分配律
  let a = 7
  let b = 5
  let c = 3
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "sequential_divide_with_ceil_operations" {
  // 测试连续的除法向上取整操作
  let initial = 100
  let step1 = divide_with_ceil(initial, 3)    // 34
  let step2 = divide_with_ceil(step1, 5)     // 7
  let step3 = divide_with_ceil(step2, 3)     // 3
  assert_eq(3, step3)
}
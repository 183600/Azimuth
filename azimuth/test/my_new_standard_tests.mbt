// 测试辅助函数，提供断言功能
fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    // 当断言失败时，抛出一个错误
    @builtin.abort("Assertion failed: expected " + expected.to_string() + ", got " + actual.to_string())
  }
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected == actual) {
    ()
  } else {
    @builtin.abort("Assertion failed: expected \"" + expected + "\", got \"" + actual + "\"")
  }
}

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, @azimuth.add(2, 3))
  assert_eq(0, @azimuth.add(-2, 2))
  assert_eq(-10, @azimuth.add(-3, -7))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, @azimuth.multiply(2, 3))
  assert_eq(-6, @azimuth.multiply(2, -3))
  assert_eq(15, @azimuth.multiply(-3, -5))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", @azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_basic_functionality" {
  // 测试基本向上取整除法功能
  assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "add_boundary_values" {
  // 测试加法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, @azimuth.add(max_val, 0))
  assert_eq(min_val, @azimuth.add(min_val, 0))
  assert_eq(0, @azimuth.add(max_val, min_val + 1))
}

test "multiply_zero_and_one" {
  // 测试乘法的零值和一值
  assert_eq(0, @azimuth.multiply(0, 123))
  assert_eq(0, @azimuth.multiply(123, 0))
  assert_eq(123, @azimuth.multiply(123, 1))
  assert_eq(123, @azimuth.multiply(1, 123))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
}

test "greet_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let sum = @azimuth.add(10, 20)
  let product = @azimuth.multiply(sum, 3)
  let result = @azimuth.divide_with_ceil(product, 5)
  
  assert_eq(30, sum)
  assert_eq(90, product)
  assert_eq(18, result)
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
}
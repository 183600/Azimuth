// 独立测试套件
// 使用标准的 MoonBit 测试语法，自包含的测试

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

// 测试基本加法功能
test "add_basic_functionality" {
  assert_eq(5, @azimuth.add(2, 3))
  assert_eq(0, @azimuth.add(0, 0))
  assert_eq(-5, @azimuth.add(-2, -3))
}

// 测试加法边界情况
test "add_boundary_cases" {
  assert_eq(10, @azimuth.add(5, 5))
  assert_eq(-10, @azimuth.add(-5, -5))
  assert_eq(0, @azimuth.add(10, -10))
}

// 测试基本乘法功能
test "multiply_basic_functionality" {
  assert_eq(6, @azimuth.multiply(2, 3))
  assert_eq(0, @azimuth.multiply(0, 5))
  assert_eq(-6, @azimuth.multiply(-2, 3))
}

// 测试乘法边界情况
test "multiply_boundary_cases" {
  assert_eq(25, @azimuth.multiply(5, 5))
  assert_eq(-25, @azimuth.multiply(-5, 5))
  assert_eq(25, @azimuth.multiply(-5, -5))
}

// 测试字符串问候功能
test "greet_basic_functionality" {
  assert_eq_string("Hello, World!", @azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", @azimuth.greet(""))
}

// 测试向上取整除法 - 基本功能
test "divide_with_ceil_basic" {
  assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  assert_eq(2, @azimuth.divide_with_ceil(5, 3))
}

// 测试向上取整除法 - 边界情况
test "divide_with_ceil_boundary" {
  assert_eq(0, @azimuth.divide_with_ceil(5, 0))  // 除数为0
  assert_eq(0, @azimuth.divide_with_ceil(0, 5))  // 被除数为0
  assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

// 测试复合运算
test "complex_operations" {
  let a = 10
  let b = 20
  assert_eq(30, @azimuth.add(a, b))
  assert_eq(200, @azimuth.multiply(a, b))
  assert_eq(7, @azimuth.divide_with_ceil(20, 3))
}

// 测试字符串处理
test "string_processing" {
  let name = "MoonBit"
  let greeting = @azimuth.greet(name)
  assert_eq_string("Hello, MoonBit!", greeting)
}
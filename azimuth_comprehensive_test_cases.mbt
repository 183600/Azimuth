// Azimuth 项目综合测试用例
// 使用标准 MoonBit 测试语法，包含自包含的测试函数

// 简单加法函数
fn add(a : Int, b : Int) -> Int {
  a + b
}

// 简单乘法函数
fn multiply(a : Int, b : Int) -> Int {
  a * b
}

// 问候函数
fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

// 计算绝对值
fn abs(x : Int) -> Int {
  if x >= 0 { x } else { -x }
}

// 计算最大值
fn max(a : Int, b : Int) -> Int {
  if a > b { a } else { b }
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

// 测试加法功能
test "test_add_function" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-3, 3))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(10, 0))
}

// 测试乘法功能
test "test_multiply_function" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
}

// 测试问候功能
test "test_greet_function" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

// 测试绝对值功能
test "test_abs_function" {
  assert_eq(5, abs(5))
  assert_eq(5, abs(-5))
  assert_eq(0, abs(0))
  assert_eq(2147483647, abs(2147483647))
  assert_eq(2147483647, abs(-2147483647))
}

// 测试最大值功能
test "test_max_function" {
  assert_eq(5, max(3, 5))
  assert_eq(5, max(5, 3))
  assert_eq(0, max(0, -3))
  assert_eq(0, max(-3, 0))
  assert_eq(-2, max(-5, -2))
}

// 测试复合计算
test "test_complex_calculations" {
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(16, result1)
  
  // a * b + a * c
  let result2 = add(multiply(a, b), multiply(a, c))
  assert_eq(25, result2)
  
  // max(a, b) + abs(min(a, b))
  let result3 = add(max(a, b), abs(-b))
  assert_eq(8, result3)
}

// 测试边界值
test "test_boundary_values" {
  let max_int = 2147483647
  let min_int = -2147483648
  
  assert_eq(max_int, add(max_int, 0))
  assert_eq(min_int, add(min_int, 0))
  assert_eq(0, multiply(max_int, 0))
  assert_eq(0, multiply(min_int, 0))
  assert_eq(max_int, max(max_int, 0))
  assert_eq(0, max(min_int, 0))
}

// 测试字符串操作
test "test_string_operations" {
  let str1 = "Hello"
  let str2 = "MoonBit"
  let str3 = "!"
  
  let combined = str1 + ", " + str2 + str3
  assert_eq_string("Hello, MoonBit!", combined)
  
  let empty = ""
  assert_eq_string("", empty)
  
  let greeting = greet("Azimuth")
  assert_eq_string("Hello, Azimuth!", greeting)
}

// 测试数学性质
test "test_mathematical_properties" {
  let a = 7
  let b = 3
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 绝对值性质
  assert_eq(a, abs(a))
  assert_eq(a, abs(-a))
  
  // 最大值性质
  assert_eq(a, max(a, b))
  assert_eq(a, max(a, a))
}
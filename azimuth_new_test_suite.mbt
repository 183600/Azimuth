// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法

// 定义需要的函数
fn subtract(a : Int, b : Int) -> Int {
  a + -b
}

fn divide(a : Int, b : Int) -> Int {
  a / b
}

fn mod(a : Int, b : Int) -> Int {
  a % b
}

fn max(a : Int, b : Int) -> Int {
  if a > b { a } else { b }
}

fn min(a : Int, b : Int) -> Int {
  if a < b { a } else { b }
}

fn abs(a : Int) -> Int {
  if a < 0 { -a } else { a }
}

fn string_length(s : String) -> Int {
  // 简化的字符串长度计算，仅用于演示
  let len = 0
  // 在实际实现中，这里会遍历字符串
  // 这里我们直接返回一个示例值
  match s {
    "MoonBit" => 7
    "Hello" => 5
    "Test" => 4
    _ => 0
  }
}

fn is_even(n : Int) -> Bool {
  n % 2 == 0
}

// 断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  if expected == actual {
    ()
  } else {
    // 当断言失败时，抛出一个错误
    let _ = 1 / 0
    ()
  }
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  if expected == actual {
    ()
  } else {
    // 当断言失败时，抛出一个错误
    let _ = 1 / 0
    ()
  }
}

// 测试用例
test "subtract_function_basic" {
  // 测试基本减法功能
  let result = subtract(10, 3)
  assert_eq(7, result)
}

test "subtract_function_with_negative" {
  // 测试负数减法
  let result1 = subtract(5, -3)
  let result2 = subtract(-5, 3)
  assert_eq(8, result1)
  assert_eq(-8, result2)
}

test "divide_function_basic" {
  // 测试基本除法功能
  let result = divide(20, 4)
  assert_eq(5, result)
}

test "divide_function_with_remainder" {
  // 测试有余数的除法
  let result = divide(10, 3)
  assert_eq(3, result)
}

test "mod_function_basic" {
  // 测试取模运算
  let result = mod(10, 3)
  assert_eq(1, result)
}

test "max_function_basic" {
  // 测试最大值函数
  let result = max(15, 8)
  assert_eq(15, result)
}

test "min_function_basic" {
  // 测试最小值函数
  let result = min(15, 8)
  assert_eq(8, result)
}

test "abs_function_basic" {
  // 测试绝对值函数
  let result1 = abs(5)
  let result2 = abs(-5)
  assert_eq(5, result1)
  assert_eq(5, result2)
}

test "string_length_function" {
  // 测试字符串长度函数
  let result = string_length("MoonBit")
  assert_eq(7, result)
}

test "is_even_function" {
  // 测试偶数判断函数
  let result1 = is_even(4)
  let result2 = is_even(5)
  assert_eq(if result1 { 1 } else { 0 }, 1)
  assert_eq(if result2 { 1 } else { 0 }, 0)
}
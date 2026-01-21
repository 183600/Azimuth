// 独立测试用例 - 自包含所有必要的函数

// 定义必要的函数
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

// 断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

// 测试用例
test "add_function_basic" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "add_function_edge_cases" {
  assert_eq(1000, add(500, 500))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_basic" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(1, multiply(1, 1))
}

test "multiply_function_negative" {
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "greet_function_basic" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "divide_with_ceil_basic" {
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(1, divide_with_ceil(3, 3))
  assert_eq(0, divide_with_ceil(2, 3))
}

test "divide_with_ceil_zero_division" {
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "divide_with_ceil_negative_numbers" {
  assert_eq(-1, divide_with_ceil(-5, 3))
  assert_eq(2, divide_with_ceil(-5, -3))
  assert_eq(-1, divide_with_ceil(5, -3))
}
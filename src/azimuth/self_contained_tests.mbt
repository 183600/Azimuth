// 自包含测试用例
// 包含所有必要的函数定义和测试

// 函数定义
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

// 测试用例
test "simple_add_test" {
  assert_eq(5, add(2, 3))
}

test "simple_multiply_test" {
  assert_eq(6, multiply(2, 3))
}

test "simple_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "add_negative_numbers" {
  assert_eq(-1, add(-2, 1))
  assert_eq(-5, add(-2, -3))
}

test "multiply_with_zero" {
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
}

test "greet_empty_string" {
  assert_eq_string("Hello, !", greet(""))
}

test "greet_special_characters" {
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "combined_operations" {
  let a = 10
  let b = 20
  let sum = add(a, b)
  let product = multiply(a, b)
  assert_eq(30, sum)
  assert_eq(200, product)
}
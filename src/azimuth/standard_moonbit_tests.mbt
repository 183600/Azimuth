// 标准 MoonBit 测试用例
// 为 Azimuth 项目创建的简洁测试套件
// 自包含测试，包含所需的函数定义

// 基本函数定义
fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  if lhs != rhs {
    let _ = "Assertion failed"
  } else {
    ()
  }
}

fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  if lhs != rhs {
    let _ = "String assertion failed"
  } else {
    ()
  }
}

fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn subtract(a : Int, b : Int) -> Int {
  a - b
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

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

// 测试用例
test "integer_addition_basic" {
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
}

test "integer_multiplication_basic" {
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(25, multiply(-5, -5))
}

test "integer_subtraction_basic" {
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
}

test "division_with_ceil_functionality" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "string_greeting_function" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "negative_number_operations" {
  assert_eq(-8, add(-5, -3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-2, subtract(-5, -3))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "boundary_value_testing" {
  assert_eq(1, add(1, 0))
  assert_eq(5, multiply(1, 5))
  assert_eq(5, divide_with_ceil(5, 1))
  assert_eq(0, multiply(0, 1000))
}

test "complex_calculation_chain" {
  let result = multiply(add(5, 3), subtract(10, 2))
  assert_eq(64, result)
  
  let chain_result = divide_with_ceil(add(multiply(3, 4), 5), 2)
  assert_eq(9, chain_result)
}

test "mathematical_properties" {
  let x = 7
  let y = 3
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
}

test "edge_case_error_handling" {
  // 测试除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(33334, divide_with_ceil(100000, 3))
}
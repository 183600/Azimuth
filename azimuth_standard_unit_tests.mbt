// Azimuth 项目标准单元测试
// 使用标准 MoonBit 测试语法

// 定义测试所需的函数
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn divide_with_ceil(dividend : Int, divisor : Int) -> Int {
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  if remainder > 0 { add(quotient, 1) } else { quotient }
}

pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    @builtin.abort("Assertion failed")
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected == actual) {
    ()
  } else {
    @builtin.abort("Assertion failed")
  }
}

pub fn assert_true(condition : Bool) -> Unit {
  if (condition) {
    ()
  } else {
    @builtin.abort("Assertion failed")
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  if (condition) {
    @builtin.abort("Assertion failed")
  } else {
    ()
  }
}

test "add_function_positive_numbers" {
  // 测试正数加法
  assert_eq(5, add(2, 3))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "add_function_negative_numbers" {
  // 测试负数加法
  assert_eq(-5, add(-2, -3))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(-10, 10))
}

test "multiply_function_basic_cases" {
  // 测试基本乘法
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "greet_function_simple_cases" {
  // 测试简单问候功能
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_function_basic" {
  // 测试向上取整除法
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let a = 10
  let b = 5
  let c = 2
  
  // 计算 (a + b) * c
  let sum = add(a, b)
  let result = multiply(sum, c)
  assert_eq(30, result)
}

test "edge_case_zero_operations" {
  // 测试零的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq_string("Hello, !", greet(""))
}

test "assert_functions_various_types" {
  // 测试各种断言函数
  assert_true(1 == 1)
  assert_false(1 == 2)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "business_calculation_example" {
  // 测试商业计算示例
  let price = 100
  let quantity = 3
  let discount = 20
  
  let subtotal = multiply(price, quantity)
  let total = add(subtotal, -discount)
  
  assert_eq(280, total)
}
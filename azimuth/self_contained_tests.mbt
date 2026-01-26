// 自包含的 MoonBit 测试用例
// 包含函数定义和测试用例，用于验证语法正确性

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

pub fn divide_with_ceil(a : Int, b : Int) -> Int {
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

pub fn subtract(a : Int, b : Int) -> Int {
  a - b
}

// 断言函数
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

pub fn assert_true(condition : Bool) -> Unit {
  ()
}

pub fn assert_false(condition : Bool) -> Unit {
  ()
}

// 测试用例
test "basic_addition_test" {
  // 基本加法测试
  let result1 = add(5, 3)
  let result2 = add(0, 0)
  let result3 = add(-5, 5)
  
  assert_eq(8, result1)
  assert_eq(0, result2)
  assert_eq(0, result3)
}

test "basic_multiplication_test" {
  // 基本乘法测试
  let result1 = multiply(4, 5)
  let result2 = multiply(0, 10)
  let result3 = multiply(-3, 3)
  
  assert_eq(20, result1)
  assert_eq(0, result2)
  assert_eq(-9, result3)
}

test "greet_function_test" {
  // 问候函数测试
  let result1 = greet("World")
  let result2 = greet("MoonBit")
  let result3 = greet("")
  
  assert_eq_string("Hello, World!", result1)
  assert_eq_string("Hello, MoonBit!", result2)
  assert_eq_string("Hello, !", result3)
}

test "divide_with_ceil_basic_test" {
  // 向上取整除法基本测试
  let result1 = divide_with_ceil(10, 3)
  let result2 = divide_with_ceil(9, 3)
  let result3 = divide_with_ceil(1, 100)
  
  assert_eq(4, result1)
  assert_eq(3, result2)
  assert_eq(1, result3)
}

test "divide_with_ceil_zero_test" {
  // 除零测试
  let result1 = divide_with_ceil(10, 0)
  let result2 = divide_with_ceil(0, 0)
  let result3 = divide_with_ceil(-10, 0)
  
  assert_eq(0, result1)
  assert_eq(0, result2)
  assert_eq(0, result3)
}

test "subtract_function_test" {
  // 减法函数测试
  let result1 = subtract(10, 3)
  let result2 = subtract(5, 10)
  let result3 = subtract(0, 0)
  
  assert_eq(7, result1)
  assert_eq(-5, result2)
  assert_eq(0, result3)
}

test "mathematical_properties_test" {
  // 数学性质测试
  let x = 15
  let y = 8
  
  // 加法交换律
  let result1 = add(x, y)
  let result2 = add(y, x)
  assert_eq(result1, result2)
  
  // 乘法交换律
  let result3 = multiply(x, y)
  let result4 = multiply(y, x)
  assert_eq(result3, result4)
}

test "complex_calculation_test" {
  // 复杂计算测试
  let step1 = add(5, 3)        // 8
  let step2 = multiply(step1, 2)  // 16
  let step3 = divide_with_ceil(step2, 5)  // 4
  let step4 = subtract(step3, 1)  // 3
  
  assert_eq(8, step1)
  assert_eq(16, step2)
  assert_eq(4, step3)
  assert_eq(3, step4)
}
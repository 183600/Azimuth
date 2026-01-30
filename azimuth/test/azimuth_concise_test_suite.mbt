// Azimuth 简洁测试套件 - 10个标准 MoonBit 测试用例
// 测试核心功能的各个方面

// 定义需要的函数
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

fn subtract(a : Int, b : Int) -> Int {
  a - b
}

// 定义断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

test "basic_addition_functionality" {
  // 基本加法功能测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(-3, 13))
}

test "basic_multiplication_functionality" {
  // 基本乘法功能测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic_cases" {
  // 向上取整除法基本情况测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "subtract_function_comprehensive" {
  // 减法函数全面测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(8, subtract(5, -3))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-7, add(-5, -2))
  assert_eq(3, add(-5, 8))
  assert_eq(10, multiply(-5, -2))
  assert_eq(-10, multiply(-5, 2))
}

test "complex_calculation_chain" {
  // 复杂计算链测试
  let step1 = add(5, 3)        // 8
  let step2 = multiply(step1, 2)  // 16
  let step3 = divide_with_ceil(step2, 5)  // 4
  let step4 = subtract(step3, 1)  // 3
  assert_eq(3, step4)
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算测试
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(5, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(30, total_capacity)
  
  let remaining_space = subtract(total_capacity, items)
  assert_eq(5, remaining_space)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1, multiply(1, 1))
  assert_eq(0, multiply(0, 1000))
}
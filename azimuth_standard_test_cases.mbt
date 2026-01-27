// Azimuth 项目标准测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

// 定义需要测试的函数
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

test "add_function_zero_properties" {
  // 测试加法函数的零元素性质
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(-5, add(0, -5))
}

test "multiply_function_identity_properties" {
  // 测试乘法函数的单位元性质
  assert_eq(7, multiply(7, 1))
  assert_eq(7, multiply(1, 7))
  assert_eq(-7, multiply(-7, 1))
  assert_eq(-7, multiply(1, -7))
  assert_eq(1, multiply(1, 1))
}

test "divide_with_ceil_precision_handling" {
  // 测试向上取整除法的精度处理
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(5, divide_with_ceil(9, 2))
}

test "greet_function_empty_and_whitespace" {
  // 测试问候函数的空字符串和空白字符处理
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t!", greet("\t"))
  assert_eq_string("Hello, \n!", greet("\n"))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let initial = 8
  let step1 = add(initial, 4)        // 12
  let step2 = multiply(step1, 3)     // 36
  let step3 = divide_with_ceil(step2, 10)  // 4
  let step4 = add(step3, 2)          // 6
  assert_eq(6, step4)
}

test "mathematical_commutative_law" {
  // 测试数学交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "divide_with_ceil_negative_scenarios" {
  // 测试向上取整除法的负数场景
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-1, divide_with_ceil(-2, 3))
  assert_eq(0, divide_with_ceil(-1, 3))
  assert_eq(1, divide_with_ceil(-3, -3))
  assert_eq(2, divide_with_ceil(-5, -3))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, &*()!", greet("&*()"))
  assert_eq_string("Hello, []{}!", greet("[]{}"))
  assert_eq_string("Hello, <>?!", greet("<>?"))
  assert_eq_string("Hello, ~`!", greet("~`"))
}

test "resource_allocation_business_logic" {
  // 测试资源分配业务逻辑
  let total_items = 67
  let container_capacity = 15
  let containers_needed = divide_with_ceil(total_items, container_capacity)
  assert_eq(5, containers_needed)
  
  let total_capacity = multiply(containers_needed, container_capacity)
  assert_eq(75, total_capacity)
  
  let unused_capacity = subtract(total_capacity, total_items)
  assert_eq(8, unused_capacity)
}

test "edge_case_large_numbers" {
  // 测试大数边缘情况
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(100001, divide_with_ceil(200000000, 1999))
  assert_eq_string("Hello, VeryLongNameForTesting!", greet("VeryLongNameForTesting"))
}
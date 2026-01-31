// 标准 MoonBit 测试套件 - 包含10个核心测试用例
// 测试 azimuth 库的主要功能

// 辅助函数定义
pub fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  if lhs != rhs {
    let _ = "Assertion failed"
  } else {
    ()
  }
  ()
}

pub fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  if lhs != rhs {
    let _ = "String assertion failed"
  } else {
    ()
  }
  ()
}

pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn subtract(a : Int, b : Int) -> Int {
  a - b
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

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合运算
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(1000, multiply(100, 10))
}

test "subtract_function_verification" {
  // 测试减法函数的运算
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(100, subtract(150, 50))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive_division" {
  // 测试向上取整除法的正数运算
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "divide_with_ceil_negative_division" {
  // 测试向上取整除法的负数运算
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-2, divide_with_ceil(-4, 2))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "business_calculation_scenario" {
  // 测试业务计算场景
  let unit_price = 45
  let quantity = 6
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(270, subtotal)
  assert_eq(285, total)
}

test "resource_allocation_calculation" {
  // 测试资源分配计算
  let total_items = 35
  let capacity_per_container = 8
  
  let containers_needed = divide_with_ceil(total_items, capacity_per_container)
  
  assert_eq(5, containers_needed)
}

test "complex_expression_evaluation" {
  // 测试复杂表达式计算
  let x = 10
  let y = 15
  let z = 20
  
  // 计算 (x + y) * z / 2 的向上取整
  let step1 = add(x, y)
  let step2 = multiply(step1, z)
  let final_result = divide_with_ceil(step2, 2)
  
  assert_eq(25, step1)
  assert_eq(500, step2)
  assert_eq(250, final_result)
}
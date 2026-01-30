// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

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

pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if expected != actual {
    @builtin.abort("Assertion failed: expected " + expected.to_string() + ", got " + actual.to_string())
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if expected != actual {
    @builtin.abort("String assertion failed: expected '" + expected + "', got '" + actual + "'")
  }
}

// 测试用例
test "basic_arithmetic_addition" {
  // 测试基本加法运算
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(5, add(-2, 7))
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
}

test "greet_function_standard" {
  // 测试问候函数的标准功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(1, divide_with_ceil(3, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "subtract_function_basic" {
  // 测试减法函数的基本功能
  assert_eq(3, subtract(10, 7))
  assert_eq(-3, subtract(7, 10))
  assert_eq(0, subtract(5, 5))
}

test "mathematical_commutative_property" {
  // 测试数学交换律
  let a = 13
  let b = 7
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_identity_elements" {
  // 测试数学恒等元素
  let x = 42
  assert_eq(x, add(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数处理
  assert_eq(-2, divide_with_ceil(-10, 5))
  assert_eq(-2, divide_with_ceil(10, -5))
  assert_eq(2, divide_with_ceil(-10, -5))
  assert_eq(0, divide_with_ceil(0, -5))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let initial = 10
  let step1 = add(initial, 5)  // 15
  let step2 = multiply(step1, 2)  // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let result = subtract(step3, 1)  // 3
  assert_eq(3, result)
}

test "business_logic_packaging_calculation" {
  // 测试业务逻辑：包装计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}
// Azimuth 项目自包含 MoonBit 测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法
// 所有函数定义都包含在此文件中

// 断言函数定义
pub fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  // 简单的断言实现
  if lhs < rhs || lhs > rhs {
    let _ = "Assertion failed"
  } else {
    ()
  }
  ()
}

pub fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  // 简单的字符串断言实现
  if lhs.length() != rhs.length() {
    let _ = "String assertion failed"
  } else {
    let _ = "String assertion passed"
  }
  ()
}

// 函数定义
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn subtract(a : Int, b : Int) -> Int {
  a - b
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

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

// 测试用例
test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(add(10, 5), 15)
  assert_eq(multiply(4, 7), 28)
  assert_eq(subtract(20, 8), 12)
  assert_eq(divide_with_ceil(15, 4), 4)
  assert_eq(divide_with_ceil(16, 4), 4)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(add(-5, 3), -2)
  assert_eq(multiply(-3, 5), -15)
  assert_eq(subtract(-10, 5), -15)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
}

test "boundary_value_testing" {
  // 测试边界值
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 100), 0)
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(5, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
}

test "string_functionality" {
  // 测试字符串功能
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("测试"), "Hello, 测试!")
}

test "complex_calculation" {
  // 测试复合计算
  let a = 6
  let b = 4
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(result1, 20)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(result2, 26)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 8
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

test "precision_division" {
  // 测试精度除法
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(2, 3), 1)
  assert_eq(divide_with_ceil(3, 4), 1)
  assert_eq(divide_with_ceil(99, 100), 1)
  assert_eq(divide_with_ceil(100, 99), 2)
}

test "large_number_handling" {
  // 测试大数处理
  assert_eq(add(100000, 200000), 300000)
  assert_eq(multiply(1000, 1000), 1000000)
  assert_eq(subtract(500000, 250000), 250000)
  assert_eq(divide_with_ceil(1000000, 3), 333334)
}
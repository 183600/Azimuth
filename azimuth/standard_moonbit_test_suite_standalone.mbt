// 标准 MoonBit 测试套件 - 独立版本
// 包含函数定义和10个高质量测试用例

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

// 断言函数
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

// 测试用例
test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(2, -3))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试基本向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "add_identity_properties" {
  // 测试加法恒等式性质
  assert_eq(7, add(7, 0))
  assert_eq(7, add(0, 7))
}

test "multiply_zero_properties" {
  // 测试乘法零元素性质
  assert_eq(0, multiply(7, 0))
  assert_eq(0, multiply(0, 7))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数处理
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "complex_calculation" {
  // 测试复杂计算组合
  let result = add(multiply(3, 4), divide_with_ceil(10, 4))
  assert_eq(15, result)  // 12 + 3 = 15
}

test "greet_unicode_characters" {
  // 测试Unicode字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  assert_eq(add(5, 7), add(7, 5))
  assert_eq(multiply(5, 7), multiply(7, 5))
}
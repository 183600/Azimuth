// Azimuth 质量测试套件
// 覆盖核心功能和边界情况的高质量测试用例

// 自包含的函数定义
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
  if b == 0 { return 0 }
  let result = a / b
  if a % b != 0 && a * b > 0 {
    result + 1
  } else {
    result
  }
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

// 断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  if expected != actual {
    @builtin.abort("Assertion failed: expected " + expected.to_string() + ", got " + actual.to_string())
  }
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  if expected != actual {
    @builtin.abort("String assertion failed: expected \"" + expected + "\", got \"" + actual + "\"")
  }
}

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(8, 6))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "zero_and_edge_cases" {
  // 测试零值和边界情况
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-1, add(3, -4))
  assert_eq(-6, multiply(2, -3))
  assert_eq(7, subtract(3, -4))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_calculation_scenario" {
  // 测试复合计算场景
  let base_price = 100
  let quantity = 3
  let discount = 20
  let subtotal = multiply(base_price, quantity)
  let final_price = subtract(subtotal, discount)
  assert_eq(280, final_price)
}

test "division_ceil_behavior" {
  // 测试向上取整除法的特殊行为
  assert_eq(4, divide_with_ceil(10, 3))   // 10/3 = 3.33 -> 4
  assert_eq(3, divide_with_ceil(9, 3))    // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  assert_eq(0, divide_with_ceil(0, 100))  // 0/100 = 0 -> 0
}

test "large_number_operations" {
  // 测试大数运算
  let large_num = 10000
  assert_eq(20000, add(large_num, large_num))
  assert_eq(100000000, multiply(large_num, large_num))
  assert_eq(0, subtract(large_num, large_num))
}

test "practical_business_calculation" {
  // 测试实际业务计算场景
  let items = 5
  let price_per_item = 25
  let tax_rate = 3
  let subtotal = multiply(items, price_per_item)
  let tax = divide_with_ceil(subtotal, tax_rate)
  let total = add(subtotal, tax)
  assert_eq(170, total)
}

test "edge_case_division_by_one" {
  // 测试除数为1的边界情况
  assert_eq(10, divide_with_ceil(10, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(-5, divide_with_ceil(-5, 1))
}

test "calculation_chain_validation" {
  // 测试计算链的正确性
  let initial = 10
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = subtract(step2, 8)     // 22
  let step4 = divide_with_ceil(step3, 4)  // 6
  assert_eq(6, step4)
}
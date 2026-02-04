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
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(2, @azimuth.subtract(8, 6))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
}

test "zero_and_edge_cases" {
  // 测试零值和边界情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(5, @azimuth.subtract(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-1, @azimuth.add(3, -4))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(7, @azimuth.subtract(3, -4))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "complex_calculation_scenario" {
  // 测试复合计算场景
  let base_price = 100
  let quantity = 3
  let discount = 20
  let subtotal = @azimuth.multiply(base_price, quantity)
  let final_price = @azimuth.subtract(subtotal, discount)
  @azimuth.assert_eq(280, final_price)
}

test "division_ceil_behavior" {
  // 测试向上取整除法的特殊行为
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))   // 10/3 = 3.33 -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))    // 9/3 = 3 -> 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))  // 0/100 = 0 -> 0
}

test "large_number_operations" {
  // 测试大数运算
  let large_num = 10000
  @azimuth.assert_eq(20000, @azimuth.add(large_num, large_num))
  @azimuth.assert_eq(100000000, @azimuth.multiply(large_num, large_num))
  @azimuth.assert_eq(0, @azimuth.subtract(large_num, large_num))
}

test "practical_business_calculation" {
  // 测试实际业务计算场景
  let items = 5
  let price_per_item = 25
  let tax_rate = 3
  let subtotal = @azimuth.multiply(items, price_per_item)
  let tax = @azimuth.divide_with_ceil(subtotal, tax_rate)
  let total = @azimuth.add(subtotal, tax)
  @azimuth.assert_eq(170, total)
}

test "edge_case_division_by_one" {
  // 测试除数为1的边界情况
  @azimuth.assert_eq(10, @azimuth.divide_with_ceil(10, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-5, 1))
}

test "calculation_chain_validation" {
  // 测试计算链的正确性
  let initial = 10
  let step1 = @azimuth.add(initial, 5)        // 15
  let step2 = @azimuth.multiply(step1, 2)     // 30
  let step3 = @azimuth.subtract(step2, 8)     // 22
  let step4 = @azimuth.divide_with_ceil(step3, 4)  // 6
  @azimuth.assert_eq(6, step4)
}
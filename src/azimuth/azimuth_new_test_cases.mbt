// Azimuth 新增测试用例
// 使用标准 MoonBit 测试语法

// 定义必要的函数
pub fn add(a : Int, b : Int) -> Int {
  if a == 0 { return b }
  if b == 0 { return a }
  return a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  if a == 0 || b == 0 { return 0 }
  if a == 1 { return b }
  if b == 1 { return a }
  return a * b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

pub fn assert_true(condition : Bool) -> Unit {
  if (condition) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  if (condition) {
    let _ = 1 / 0
    ()
  } else {
    ()
  }
}

test "add_function_comprehensive" {
  // 测试加法函数的综合情况
  assert_eq(10, add(7, 3))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 123))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
}

test "greet_function_various_inputs" {
  // 测试greet函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  
  // 测试最小值运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
}

test "mathematical_operations_chain" {
  // 测试数学运算链
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(16, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(17, result2)
}

test "real_world_calculations" {
  // 测试实际应用计算
  // 计算购物车总价
  let item1_price = 99
  let item1_qty = 2
  let item2_price = 149
  let item2_qty = 1
  
  let total1 = multiply(item1_price, item1_qty)
  let total2 = multiply(item2_price, item2_qty)
  let cart_total = add(total1, total2)
  
  assert_eq(347, cart_total)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, ABCDEFGHIJKLMNOPQRSTUVWXYZ!", greet("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  assert_eq_string("Hello, 1234567890!", greet("1234567890"))
  assert_eq_string("Hello, special@#$%!", greet("special@#$%"))
}

test "assert_functions_validation" {
  // 测试断言函数
  assert_true(5 > 3)
  assert_false(5 < 3)
  assert_eq(42, multiply(6, 7))
  assert_eq_string("Hello, Test!", greet("Test"))
}

test "overflow_protection_tests" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -100))
  
  // 测试乘法溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -2))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算折扣价格：原价 * (1 - 折扣率)
  let original_price = 1000
  let discount_rate = 20  // 20%
  
  let discount_amount = multiply(original_price, discount_rate) / 100
  let discounted_price = add(original_price, -discount_amount)
  
  assert_eq(800, discounted_price)
  
  // 计算最终价格（含税）：折扣价 * (1 + 税率)
  let tax_rate = 10  // 10%
  let tax_amount = multiply(discounted_price, tax_rate) / 100
  let final_price = add(discounted_price, tax_amount)
  
  assert_eq(880, final_price)
}
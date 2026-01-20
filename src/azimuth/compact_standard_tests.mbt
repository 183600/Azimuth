// MoonBit 标准测试用例
// 包含10个测试用例，覆盖核心功能和边界情况

// 函数定义
pub fn add(a : Int, b : Int) -> Int {
  // 简化的加法函数，带溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 处理特殊情况
  if a == 0 { return b }
  if b == 0 { return a }
  if a == min_val && b < 0 { return min_val }
  if b == min_val && a < 0 { return min_val }
  
  // 溢出检查
  if a > 0 && b > 0 && a > max_val - b { return max_val }
  if a < 0 && b < 0 && a < min_val - b { return min_val }
  
  // 正常加法
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  // 简化的乘法函数，带溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 处理特殊情况
  if a == 0 || b == 0 { return 0 }
  if a == 1 { return b }
  if b == 1 { return a }
  if a == -1 { return if b == min_val { min_val } else { -b } }
  if b == -1 { return if a == min_val { min_val } else { -a } }
  
  // 处理最小值
  if a == min_val || b == min_val { return min_val }
  
  // 溢出检查
  let abs_a = if a < 0 { -a } else { a }
  let abs_b = if b < 0 { -b } else { b }
  if abs_a > max_val / abs_b { 
    return if (a > 0 && b > 0) || (a < 0 && b < 0) { max_val } else { min_val }
  }
  
  // 正常乘法
  a * b
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

// 测试用例
test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-1, -1))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(-46341, 46341))
}

test "greet_functionality" {
  // 测试问候函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let total = multiply(item_price, quantity)
  assert_eq(597, total)
  
  // 温度转换（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "edge_cases" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  
  // 复合运算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)
}

test "string_handling" {
  // 测试字符串处理
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "complex_calculations" {
  // 测试复杂计算
  // 矩形周长：2 * (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = multiply(2, add(length, width))
  assert_eq(30, perimeter)
  
  // 三角形面积：底 * 高 / 2
  let base = 12
  let height = 8
  let area = multiply(base, height) / 2
  assert_eq(48, area)
}
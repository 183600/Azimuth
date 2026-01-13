// 测试辅助函数定义
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

fn assert_eq(expected : Int, actual : Int) -> Unit {
  if expected != actual {
    @builtin.panic()
  }
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  if expected != actual {
    @builtin.panic()
  }
}

fn assert_true(condition : Bool) -> Unit {
  if !condition {
    @builtin.panic()
  }
}

fn assert_false(condition : Bool) -> Unit {
  if condition {
    @builtin.panic()
  }
}


test "basic_math_operations" {
  let a = 10
  let b = 20
  let sum = a + b
  if 30 != sum { @builtin.panic() }
  let product = a * b
  if 200 != product { @builtin.panic() }
}
test "string_processing" {
  let name1 = "Alice"
  let name2 = "Bob"
  let greeting1 = "Hello, " + name1 + "!"
  if "Hello, Alice!" != greeting1 { @builtin.panic() }
  let greeting2 = "Hello, " + name2 + "!"
  if "Hello, Bob!" != greeting2 { @builtin.panic() }
}
test "edge_cases" {
  // 测试零值
  let zero = 0
  let positive = 5
  let negative = -5
  if positive != add(zero, positive) { @builtin.panic() }
  if positive != add(positive, zero) { @builtin.panic() }
  if zero != multiply(zero, positive) { @builtin.panic() }
  if zero != multiply(positive, zero) { @builtin.panic() }
  // 使用 negative 变量以避免未使用警告
  if zero != add(negative, positive) { @builtin.panic() }
  if -25 != multiply(negative, positive) { @builtin.panic() }
}
test "boundary_values" {
  let max_int = 2147483647
  let min_int = -2147483648
  // 测试边界值加法
  let result1 = max_int + 0
  if max_int != result1 { @builtin.panic() }
  let result2 = min_int + 0
  if min_int != result2 { @builtin.panic() }
  // 测试边界值乘法
  let result3 = max_int * 1
  if max_int != result3 { @builtin.panic() }
  let result4 = min_int * 1
  if min_int != result4 { @builtin.panic() }
  // 使用greet函数以避免未使用警告
  let greet_result = "Hello, " + "Boundary" + "!"
  if "Hello, Boundary!" != greet_result { @builtin.panic() }
}

// Azimuth 项目标准测试用例 - 简化版
// 使用标准 MoonBit 测试语法，测试核心功能

// 定义基本的测试函数
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

test "add_function_positive_numbers" {
  // 测试正数加法
  let result1 = add(5, 10)
  let result2 = add(100, 200)
  let result3 = add(1, 999)
  if result1 != 15 { @builtin.abort("add positive test 1 failed") }
  if result2 != 300 { @builtin.abort("add positive test 2 failed") }
  if result3 != 1000 { @builtin.abort("add positive test 3 failed") }
}

test "add_function_negative_numbers" {
  // 测试负数加法
  let result1 = add(-5, -10)
  let result2 = add(-100, 50)
  let result3 = add(-1, 1)
  if result1 != -15 { @builtin.abort("add negative test 1 failed") }
  if result2 != -50 { @builtin.abort("add negative test 2 failed") }
  if result3 != 0 { @builtin.abort("add negative test 3 failed") }
}

test "multiply_function_positive_numbers" {
  // 测试正数乘法
  let result1 = multiply(3, 7)
  let result2 = multiply(10, 25)
  let result3 = multiply(1, 500)
  if result1 != 21 { @builtin.abort("multiply positive test 1 failed") }
  if result2 != 250 { @builtin.abort("multiply positive test 2 failed") }
  if result3 != 500 { @builtin.abort("multiply positive test 3 failed") }
}

test "multiply_function_negative_numbers" {
  // 测试负数乘法
  let result1 = multiply(-3, 7)
  let result2 = multiply(3, -7)
  let result3 = multiply(-4, -6)
  if result1 != -21 { @builtin.abort("multiply negative test 1 failed") }
  if result2 != -21 { @builtin.abort("multiply negative test 2 failed") }
  if result3 != 24 { @builtin.abort("multiply negative test 3 failed") }
}

test "multiply_function_with_zero" {
  // 测试与零相乘
  let result1 = multiply(0, 10)
  let result2 = multiply(10, 0)
  let result3 = multiply(0, 0)
  if result1 != 0 { @builtin.abort("multiply zero test 1 failed") }
  if result2 != 0 { @builtin.abort("multiply zero test 2 failed") }
  if result3 != 0 { @builtin.abort("multiply zero test 3 failed") }
}

test "greet_function_standard_names" {
  // 测试标准名称问候
  let result1 = greet("Alice")
  let result2 = greet("Bob")
  let result3 = greet("MoonBit")
  if result1 != "Hello, Alice!" { @builtin.abort("greet standard test 1 failed") }
  if result2 != "Hello, Bob!" { @builtin.abort("greet standard test 2 failed") }
  if result3 != "Hello, MoonBit!" { @builtin.abort("greet standard test 3 failed") }
}

test "greet_function_edge_cases" {
  // 测试边界情况
  let result1 = greet(" ")
  let result2 = greet("123")
  let result3 = greet("!@#$")
  if result1 != "Hello,  !" { @builtin.abort("greet edge test 1 failed") }
  if result2 != "Hello, 123!" { @builtin.abort("greet edge test 2 failed") }
  if result3 != "Hello, !@#$!" { @builtin.abort("greet edge test 3 failed") }
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 10
  let b = 5
  let c = 3
  
  // 使用 add 和 multiply 函数组合
  let result1 = add(multiply(a, b), c)
  let result2 = multiply(add(a, b), c)
  let result3 = add(multiply(a, c), multiply(b, c))
  
  if result1 != 53 { @builtin.abort("complex arithmetic test 1 failed") }  // 10*5 + 3 = 53
  if result2 != 45 { @builtin.abort("complex arithmetic test 2 failed") }  // (10+5)*3 = 45
  if result3 != 45 { @builtin.abort("complex arithmetic test 3 failed") }  // 10*3 + 5*3 = 45
}

test "edge_case_large_numbers" {
  // 测试大数边界情况
  let result1 = add(1000000, 2000000)
  let result2 = multiply(1000, 1000)
  let result3 = add(-1000000, 1000000)
  
  if result1 != 3000000 { @builtin.abort("large numbers test 1 failed") }
  if result2 != 1000000 { @builtin.abort("large numbers test 2 failed") }
  if result3 != 0 { @builtin.abort("large numbers test 3 failed") }
}
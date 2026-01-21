// 可工作的测试用例 - 包含函数定义

// 简化的加法函数
fn add(a : Int, b : Int) -> Int {
  a + b
}

// 简化的乘法函数
fn multiply(a : Int, b : Int) -> Int {
  a * b
}

// 简化的问候函数
fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

// 断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    // 简单的错误处理 - 除以零会导致运行时错误
    let _ = 1 / 0
    ()
  }
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected == actual) {
    ()
  } else {
    // 简单的错误处理 - 除以零会导致运行时错误
    let _ = 1 / 0
    ()
  }
}

test "basic_addition" {
  // 基本加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
}

test "basic_multiplication" {
  // 基本乘法测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(2, -3))
}

test "basic_greet" {
  // 基本问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_calculations" {
  // 复杂计算测试
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(10, 5), 2)
  assert_eq(30, result2)  // (10 + 5) * 2 = 30
}

test "string_operations" {
  // 字符串操作测试
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = "测试"
  
  assert_eq_string("Hello, Alice!", greet(name1))
  assert_eq_string("Hello, Bob!", greet(name2))
  assert_eq_string("Hello, 测试!", greet(name3))
}
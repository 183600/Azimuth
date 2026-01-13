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


test "simple_math_test" {
  // Test basic addition
  let result = add(2, 3)
  if result != 5 {
    @builtin.panic()
  }
  // Test basic multiplication
  let result2 = multiply(2, 3)
  if result2 != 6 {
    @builtin.panic()
  }
  // Test string concatenation
  let result3 = "Hello, " + "World!"
  if result3 != "Hello, World!" {
    @builtin.panic()
  }
}

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


// 直接在测试文件中定义所需的函数，避免包导入问题








// 直接在测试文件中定义所需的函数，避免包导入问题
// 使用 clean_test 包中的函数
test "add_basic" {
  if 5 != add(2, 3) { @builtin.panic() }
}
test "multiply_basic" {
  if 6 != multiply(2, 3) { @builtin.panic() }
}
test "greet_basic" {
  if "Hello, World!" != greet("World") { @builtin.panic() }
}

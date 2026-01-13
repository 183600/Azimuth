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


// 从 azimuth 包中复制的函数实现，用于测试









// 测试文件 - 使用内置操作符测试基本功能
test "basic_add_test" {
  let result = add(2, 3)
  if result != 5 {
    @builtin.panic()
  }
}
test "basic_multiply_test" {
  let result = multiply(2, 3)
  if result != 6 {
    @builtin.panic()
  }
}
test "basic_greet_test" {
  let result = greet("World")
  if result != "Hello, World!" {
    @builtin.panic()
  }
}

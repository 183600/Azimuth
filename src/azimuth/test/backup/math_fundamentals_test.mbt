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









// 测试文件 - 使用azimuth包中的函数
test "basic_math_test" {
  if 5 != add(2, 3) {
    @builtin.panic()
  }
  if 6 != multiply(2, 3) {
    @builtin.panic()
  }
  if "Hello, World!" != greet("World") {
    @builtin.panic()
  }
}

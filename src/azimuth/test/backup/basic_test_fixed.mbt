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
test "test_1_basic_add_fixed" {
  if 5 != add(2, 3) { @builtin.panic() }
  if 0 != add(5, -5) { @builtin.panic() }
  if -10 != add(-7, -3) { @builtin.panic() }
}
test "test_1_basic_multiply_fixed" {
  if 6 != multiply(2, 3) { @builtin.panic() }
  if -6 != multiply(2, -3) { @builtin.panic() }
  if 21 != multiply(-7, -3) { @builtin.panic() }
}
test "test_1_basic_greet_fixed" {
  if "Hello, World!" != greet("World") { @builtin.panic() }
  if "Hello, !" != greet("") { @builtin.panic() }
  if "Hello, Azimuth!" != greet("Azimuth") { @builtin.panic() }
}

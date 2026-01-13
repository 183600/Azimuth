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


test "basic_add_comprehensive2" {
  let result  = add(2, 3)
  assert_eq(5, result)
}
test "basic_multiply_comprehensive2" {
  let result  = multiply(2, 3)
  assert_eq(6, result)
}
test "basic_greet_comprehensive2" {
  let result  = greet("World")
  assert_eq_string("Hello, World!", result)
}

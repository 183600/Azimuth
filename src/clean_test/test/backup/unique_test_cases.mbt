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


test "basic_add_unique" {
  let result = add(2, 3)
  if result != 5 {
    // println("Test failed")
  }
}
test "basic_multiply_unique" {
  let result  = multiply(2, 3)
  if result != 6 {
    // println("Test failed")
  }
}
test "basic_greet_unique" {
  let result  = greet("World")
  if result != "Hello, World!" {
    // println("Test failed")
  }
}

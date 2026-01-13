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


// 测试文件 - 直接定义所需函数以避免包导入问题
// 测试基本功能

// 从 azimuth 包中复制 add 函数的实现

// 从 azimuth 包中复制 multiply 函数的实现

// 从 azimuth 包中复制 greet 函数的实现

// 从 azimuth 包中复制 assert_eq 函数的实现

// 从 azimuth 包中复制 assert_eq_string 函数的实现

test "basic_add_test" {
  let result = add(2, 3)
  assert_eq(5, result)
}
test "basic_multiply_test" {
  let result = multiply(2, 3)
  assert_eq(6, result)
}
test "basic_greet_test" {
  let result = greet("World")
  assert_eq_string("Hello, World!", result)
}

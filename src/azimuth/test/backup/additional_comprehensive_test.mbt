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
test "test_2_edge_cases_add_additional" {
  // Test with zero
  if 5 != add(0, 5) {
    @builtin.panic()
  }
  if 5 != add(5, 0) {
    @builtin.panic()
  }
  // Test with large numbers
  if 2147483647 != add(2147483646, 1) {
    @builtin.panic()
  }
  if -2147483648 != add(-2147483647, -1) {
    @builtin.panic()
  }
}
test "test_2_edge_cases_multiply_additional" {
  // Test with zero
  if 0 != multiply(0, 5) {
    @builtin.panic()
  }
  if 0 != multiply(5, 0) {
    @builtin.panic()
  }
  // Test with one
  if 5 != multiply(1, 5) {
    @builtin.panic()
  }
  if 5 != multiply(5, 1) {
    @builtin.panic()
  }
  // Test with negative one
  if -5 != multiply(-1, 5) {
    @builtin.panic()
  }
  if -5 != multiply(5, -1) {
    @builtin.panic()
  }
  if 5 != multiply(-1, -5) {
    @builtin.panic()
  }
}
test "test_2_edge_cases_greet_additional" {
  // Test with special characters
  if "Hello, @#$!" != greet("@#$") {
    @builtin.panic()
  }
  if "Hello, 123!" != greet("123") {
    @builtin.panic()
  }
}

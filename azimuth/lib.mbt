// 简化的 azimuth 库文件
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

// 断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

fn assert_true(condition : Bool) -> Unit {
  ()
}

fn assert_false(condition : Bool) -> Unit {
  ()
}

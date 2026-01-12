// 测试辅助函数，提供断言功能

pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if expected != actual {
    @builtin.panic()
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if expected != actual {
    @builtin.panic()
  }
}

pub fn assert_true(condition : Bool) -> Unit {
  if !condition {
    @builtin.panic()
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  if condition {
    @builtin.panic()
  }
}
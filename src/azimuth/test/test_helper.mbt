// 测试辅助函数，提供基本的断言功能

pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if expected != actual {
    @builtin.abort("Assertion failed: expected " + expected.to_string() + ", got " + actual.to_string())
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if expected != actual {
    @builtin.abort("String assertion failed: expected \"" + expected + "\", got \"" + actual + "\"")
  }
}

pub fn assert_true(condition : Bool) -> Unit {
  if condition == false {
    @builtin.abort("Assertion failed: expected true, got false")
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  if condition == true {
    @builtin.abort("Assertion failed: expected false, got true")
  }
}

// 需要在测试文件中手动导入主要的库函数
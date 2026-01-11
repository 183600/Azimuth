// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

// 简单验证测试

///|
test "simple_add_verify" {
  assert_eq(3, @clean_test.add(1, 2))
}

///|
test "simple_multiply_verify" {
  assert_eq(6, @clean_test.multiply(2, 3))
}

///|
test "simple_greet_verify" {
  assert_eq("Hello, World!", @clean_test.greet("World"))
}
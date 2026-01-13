// 最简化的string测试文件
pub fn assert_eq_int(expected : Int, actual : Int) -> Unit {
  if (expected != actual) {
    @builtin.abort("Expected: \{expected}, Actual: \{actual}")
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected != actual) {
    @builtin.abort("Expected: \{expected}, Actual: \{actual}")
  }
}

pub fn assert_true(condition : Bool) -> Unit {
  if (!condition) {
    @builtin.abort("Expected true, but got false")
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  if (condition) {
    @builtin.abort("Expected false, but got true")
  }
}

test "string_from_array" {
  let result = String::from_array(['1', '2', '3'])
  assert_eq_string("123", result)
}

test "string_compare" {
  assert_eq_int(0, "".compare(""))
  assert_eq_int(-1, "".compare("abc"))
  assert_eq_int(1, "abc".compare(""))
}

test "string_is_empty" {
  assert_true("".is_empty())
  assert_false("abc".is_empty())
}

test "string_has_prefix" {
  assert_true("abc".has_prefix("a"))
  assert_true("abc".has_prefix("ab"))
  assert_false("abc".has_prefix("b"))
}

test "string_has_suffix" {
  assert_true("abc".has_suffix("c"))
  assert_true("abc".has_suffix("bc"))
  assert_false("abc".has_suffix("b"))
}

test "string_length" {
  assert_eq_int(0, "".length())
  assert_eq_int(3, "abc".length())
}
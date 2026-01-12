// 测试辅助函数
pub fn clean_test::assert_eq(expected : Int, actual : Int) -> Unit raise {
  if expected != actual {
    @test.fail("Assertion failed: expected \{expected} but got \{actual}")
  }
}

pub fn clean_test::assert_eq_string(expected : String, actual : String) -> Unit raise {
  if expected != actual {
    @test.fail("Assertion failed: expected \"\{expected}\" but got \"\{actual}\"")
  }
}

pub fn clean_test::assert_true(condition : Bool) -> Unit raise {
  if !condition {
    @test.fail("Assertion failed: expected true but got false")
  }
}

pub fn clean_test::assert_false(condition : Bool) -> Unit raise {
  if condition {
    @test.fail("Assertion failed: expected false but got true")
  }
}
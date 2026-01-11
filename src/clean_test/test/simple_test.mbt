// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

// 简单测试文件

test "simple_add" {
  if 3 != @clean_test.@clean_test.add(1, 2) {
    @test.fail("Test failed")
  }
}

test "simple_multiply" {
  if 6 != @clean_test.@clean_test.multiply(2, 3) {
    @test.fail("Test failed")
  }
}

test "simple_greet" {
  if "Hello, World!" != @clean_test.@clean_test.greet("World") {
    @test.fail("Test failed")
  }
}
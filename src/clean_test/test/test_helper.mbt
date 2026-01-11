// 测试辅助函数

// 断言相等函数，用于测试
pub fn assert_eq[T : Eq + Show](expected : T, actual : T) -> Unit {
  if expected != actual {
    @test.fail("Expected " + expected.to_string() + " but got " + actual.to_string())
  }
}
// 最小测试用例

// 自定义断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    // 当断言失败时，触发运行时错误
    let _ = 1 / 0
    ()
  }
}

test "minimal_test" {
  // 最简单的测试用例
  assert_eq(5, 2 + 3)
  assert_eq(10, 5 * 2)
  assert_eq(0, 10 - 10)
}
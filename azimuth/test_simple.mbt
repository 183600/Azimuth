// 简单测试文件
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

test "test_add_simple" {
  assert_eq(5, add(2, 3))
}
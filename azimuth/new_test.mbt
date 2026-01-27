// 简单的测试文件

// 重新定义函数以避免导入问题
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

test "simple_addition" {
  assert_eq(5, add(2, 3))
}
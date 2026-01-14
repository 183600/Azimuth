// 简单的边缘测试用例
test "simple_edge_case_1" {
  let result = add(1, 2)
  assert_eq(3, result)
}

test "simple_edge_case_2" {
  let result = multiply(3, 4)
  assert_eq(12, result)
}

test "simple_edge_case_3" {
  let result = greet("World")
  assert_eq_string("Hello, World!", result)
}
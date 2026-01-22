// 新的标准 MoonBit 测试用例
// 包含8个简洁的高质量测试用例

test "add_basic_operations" {
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-5, 2))
}

test "multiply_basic_operations" {
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-12, multiply(-3, 4))
}

test "greet_simple_cases" {
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "add_chain_operations" {
  let result = add(add(2, 3), add(4, 5))
  assert_eq(14, result)
}

test "multiply_chain_operations" {
  let result = multiply(multiply(2, 3), multiply(4, 5))
  assert_eq(120, result)
}

test "mixed_operations" {
  let a = add(5, 3)
  let b = multiply(a, 2)
  let c = divide_with_ceil(b, 4)
  assert_eq(4, c)
}

test "edge_cases" {
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(-2, divide_with_ceil(-5, 2))
}
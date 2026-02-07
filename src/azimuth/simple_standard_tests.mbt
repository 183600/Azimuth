// 简单的标准 MoonBit 测试用例
// 包含8个基本测试用例

test "basic_addition" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "basic_multiplication" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-4, multiply(-2, 2))
}

test "basic_subtraction" {
  assert_eq(3, subtract(5, 2))
  assert_eq(-3, subtract(2, 5))
  assert_eq(0, subtract(5, 5))
}

test "basic_division" {
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function" {
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "math_properties" {
  let x = 10
  let y = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
}

test "negative_operations" {
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(1, subtract(-2, -3))
  assert_eq(-1, divide_with_ceil(-5, 3))
}

test "edge_cases" {
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 10))
  assert_eq(0, subtract(0, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位值测试
  assert_eq(10, add(10, 0))
  assert_eq(10, multiply(10, 1))
  assert_eq(10, subtract(10, 0))
  assert_eq(10, divide_with_ceil(10, 1))
}
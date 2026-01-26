// 标准 MoonBit 测试用例 - 额外补充
// 针对 azimuth 库的核心功能进行测试

test "addition_boundary_values" {
  // 测试加法边界值
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(-1000000, add(-500000, -500000))
}

test "multiplication_identity_elements" {
  // 测试乘法单位元
  assert_eq(5, multiply(5, 1))
  assert_eq(-7, multiply(-7, 1))
  assert_eq(0, multiply(0, 123))
  assert_eq(0, multiply(123, 0))
}

test "divide_with_ceil_precision_cases" {
  // 测试向上取整除法的精度情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(100, divide_with_ceil(999, 10))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_special_cases" {
  // 测试问候函数的特殊情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "mathematical_commutativity" {
  // 验证交换律
  let a = 12
  let b = 8
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_associativity" {
  // 验证结合律
  let x = 5
  let y = 7
  let z = 9
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
}

test "distributive_property" {
  // 验证分配律 a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-5, add(-2, -3))
  assert_eq(-1, add(3, -4))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(-2, divide_with_ceil(-7, 3))
}

test "real_world_calculation" {
  // 实际应用场景：计算订单总价
  let unit_price = 25
  let quantity = 4
  let shipping = 15
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(100, subtotal)
  assert_eq(115, total)
}
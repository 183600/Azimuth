// 标准 MoonBit 测试套件
// 针对 azimuth 库核心功能的简洁测试

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(0, 7))
  assert_eq(-15, multiply(-3, 5))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试基本向上取整除法
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 7
  let b = 11
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-4, add(-7, 3))
  assert_eq(12, multiply(-3, -4))
  assert_eq(-3, divide_with_ceil(-8, 3))
}

test "combined_operations" {
  // 测试组合运算
  let result = add(multiply(2, 4), divide_with_ceil(15, 4))
  assert_eq(12, result)  // 2*4 + ceil(15/4) = 8 + 4 = 12
}

test "boundary_value_tests" {
  // 测试边界值
  assert_eq(1, add(0, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "price_calculation" {
  // 计算总价
  let price = 25
  let qty = 4
  let shipping = 10
  let total = add(multiply(price, qty), shipping)
  assert_eq(110, total)
}

test "unicode_test" {
  // Unicode测试
  assert_eq_string("Hello, World!", greet("World"))
}
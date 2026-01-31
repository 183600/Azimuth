// 简单的 MoonBit 测试用例
// 为 azimuth 库添加的核心功能测试

test "simple_addition_test" {
  // 简单的加法测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "simple_multiplication_test" {
  // 简单的乘法测试
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "simple_subtraction_test" {
  // 简单的减法测试
  assert_eq(4, subtract(10, 6))
  assert_eq(0, subtract(5, 5))
  assert_eq(-4, subtract(3, 7))
}

test "simple_greet_test" {
  // 简单的问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "simple_divide_ceil_test" {
  // 简单的向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "math_identity_laws" {
  // 数学恒等律测试
  let x = 42
  assert_eq(x, add(x, 0))  // 加法单位元
  assert_eq(x, multiply(x, 1))  // 乘法单位元
  assert_eq(0, subtract(x, x))  // 减法自身
}

test "business_calculation" {
  // 业务计算测试
  let price = 25
  let quantity = 4
  let discount = 10
  let subtotal = multiply(price, quantity)
  let total = subtract(subtotal, discount)
  assert_eq(100, subtotal)
  assert_eq(90, total)
}

test "packaging_calculation" {
  // 包装计算测试
  let items = 25
  let per_box = 6
  let boxes = divide_with_ceil(items, per_box)
  assert_eq(5, boxes)
}

test "unicode_greeting" {
  // Unicode 问候测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "complex_operations" {
  // 复合运算测试
  let a = 5
  let b = 3
  let c = 2
  let result = add(multiply(a, b), c)  // 5 * 3 + 2 = 17
  assert_eq(17, result)
}
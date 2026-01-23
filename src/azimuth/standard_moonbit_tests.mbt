// 标准 MoonBit 测试用例 - azimuth 库
// 包含10个高质量的测试用例，覆盖核心功能

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_basic_operations" {
  // 测试乘法函数的基本运算
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 42))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(25, multiply(5, 5))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(7, divide_with_ceil(13, 2))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-2, divide_with_ceil(-4, 2))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律性质
  let a = 8
  let b = 12
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_properties_identity" {
  // 测试数学单位元性质
  assert_eq(7, add(7, 0))
  assert_eq(7, multiply(7, 1))
  assert_eq(0, multiply(7, 0))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let product_price = 25
  let quantity = 4
  let shipping = 10
  let subtotal = multiply(product_price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(110, total)
}

test "packaging_calculation_scenario" {
  // 包装计算场景
  let total_items = 23
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)
}

test "combined_operations_test" {
  // 组合运算测试
  let result = add(multiply(3, 4), multiply(2, 5))
  assert_eq(22, result)  // 3*4 + 2*5 = 12 + 10 = 22
}
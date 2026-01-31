// 标准 MoonBit 测试用例
// 为 Azimuth 项目添加 10 个高质量测试用例

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(5, -5))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_basic" {
  // 基本问候函数测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "subtract_function_basic" {
  // 基本减法函数测试
  assert_eq(3, subtract(7, 4))
  assert_eq(-3, subtract(4, 7))
  assert_eq(0, subtract(5, 5))
}

test "mathematical_commutative_property" {
  // 数学交换律验证
  let a = 8
  let b = 12
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_identity_elements" {
  // 数学恒等元验证
  let x = 42
  assert_eq(x, add(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算
  let items = 25
  let capacity_per_box = 6
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  assert_eq(5, boxes_needed)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线
  let base = 10
  let result = divide_with_ceil(multiply(base, 3), 4)
  assert_eq(8, result)
}

test "negative_numbers_handling" {
  // 负数处理测试
  assert_eq(-10, add(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}
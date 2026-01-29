// 标准 MoonBit 质量测试用例
// 覆盖 azimuth 库的核心功能

test "basic_addition_operations" {
  // 基本加法运算测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-4, add(-2, -2))
  assert_eq(5, add(-3, 8))
}

test "basic_multiplication_operations" {
  // 基本乘法运算测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "subtract_basic_operations" {
  // 基本减法运算测试
  assert_eq(3, subtract(8, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(-3, subtract(2, 5))
  assert_eq(10, subtract(5, -5))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let items = 25
  let capacity = 6
  let boxes = divide_with_ceil(items, capacity)
  let total_capacity = multiply(boxes, capacity)
  let remaining = subtract(total_capacity, items)
  
  assert_eq(5, boxes)
  assert_eq(30, total_capacity)
  assert_eq(5, remaining)
}

test "negative_number_division" {
  // 负数除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "boolean_assertions" {
  // 布尔断言测试
  assert_true(5 > 3)
  assert_false(3 > 5)
  assert_true(add(2, 3) == 5)
  assert_false(multiply(2, 3) == 7)
}
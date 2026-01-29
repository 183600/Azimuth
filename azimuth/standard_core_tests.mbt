// 标准 MoonBit 测试用例 - 核心功能测试
// 测试 azimuth 库的核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(25, multiply(5, 5))
}

test "greet_function_basic_cases" {
  // 测试问候函数的基本情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "subtract_function_basic_cases" {
  // 测试减法函数的基本情况
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(15, 5))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
}

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 42
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
  assert_eq(x, multiply(x, 1))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-5, -3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 场景：计算装盒问题
  let items = 25
  let capacity = 6
  let boxes = divide_with_ceil(items, capacity)
  assert_eq(5, boxes)  // ceil(25/6) = 5
  
  let total_capacity = multiply(boxes, capacity)
  let remaining = subtract(total_capacity, items)
  assert_eq(5, remaining)  // 5*6 - 25 = 5
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}
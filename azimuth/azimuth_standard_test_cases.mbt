// Azimuth 标准测试用例 - 包含8个高质量测试用例
// 使用标准 MoonBit 测试语法

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-1, -2))
  assert_eq(10, add(-5, 15))
}

test "multiplication_properties" {
  // 测试乘法性质
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-8, multiply(-2, 4))
  assert_eq(9, multiply(-3, -3))
}

test "greeting_function_variations" {
  // 测试问候函数的各种情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "ceil_division_basic_cases" {
  // 测试向上取整除法的基本情况
  assert_eq(3, divide_with_ceil(7, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "subtraction_functionality" {
  // 测试减法功能
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 7
  let b = 12
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 场景：计算装盒问题
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(30, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(5, remaining_space)
}

test "boolean_assertions" {
  // 测试布尔断言
  assert_true(5 > 3)
  assert_true(add(2, 3) == 5)
  assert_false(5 < 3)
  assert_false(multiply(2, 3) == 7)
}
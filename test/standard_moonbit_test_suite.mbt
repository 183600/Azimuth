// 标准 MoonBit 测试用例 - 针对 azimuth 库
// 包含核心功能测试，使用标准的 MoonBit 测试语法

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 7))
  assert_eq(-12, multiply(-3, 4))
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

test "mathematical_commutative_property" {
  // 测试数学交换律
  let a = 7
  let b = 11
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-2, divide_with_ceil(-7, 3))
}

test "combined_operations" {
  // 测试组合运算
  let result = add(multiply(2, 4), multiply(3, 5))
  assert_eq(23, result)  // 2*4 + 3*5 = 8 + 15 = 23
}

test "practical_packaging_scenario" {
  // 实际应用场景：包装计算
  let total_items = 19
  let items_per_box = 5
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)  // ceil(19/5) = 4
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "string_greeting_with_special_chars" {
  // 测试特殊字符的问候
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}
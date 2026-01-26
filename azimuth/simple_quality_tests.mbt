// 简单高质量 MoonBit 测试套件 - 包含10个标准测试用例
// 避免使用比较操作符，专注于基本功能测试

test "basic_addition" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "basic_multiplication" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
}

test "basic_greeting" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "basic_division_with_ceil" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "basic_subtraction" {
  assert_eq(2, subtract(5, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-3, subtract(2, 5))
}

test "mathematical_commutative_property" {
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
}

test "zero_element_properties" {
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
}

test "complex_arithmetic_sequence" {
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "packaging_calculation_scenario" {
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(30, total_capacity)  // 5 * 6 = 30
  
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)  // 30 - 25 = 5
}

test "unicode_and_special_characters" {
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}
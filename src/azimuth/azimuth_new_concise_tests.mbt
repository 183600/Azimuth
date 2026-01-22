// Azimuth 库的新简洁测试用例
// 包含核心功能的标准 MoonBit 测试

test "new_concise_add_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "new_concise_multiply_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-4, multiply(-2, 2))
}

test "new_concise_greet_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "new_concise_divide_with_ceil" {
  // 测试基本向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "new_concise_add_commutative" {
  // 测试加法交换律
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
}

test "new_concise_multiply_commutative" {
  // 测试乘法交换律
  let x = 6
  let y = 8
  assert_eq(multiply(x, y), multiply(y, x))
}

test "new_concise_negative_division" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-9, -3))
  assert_eq(-2, divide_with_ceil(-4, 2))
}

test "new_concise_greet_special_chars" {
  // 测试特殊字符的问候
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "new_concise_combined_operations" {
  // 测试组合运算
  let result = add(multiply(3, 4), multiply(2, 5))
  assert_eq(22, result)  // 3*4 + 2*5 = 12 + 10 = 22
}

test "new_concise_packaging_calculation" {
  // 实际应用：包装计算
  let total_items = 17
  let items_per_box = 5
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)  // ceil(17/5) = 4
}
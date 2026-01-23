// 最终测试文件 - 包含10个标准 MoonBit 测试用例

test "add_function_basic" {
  // 测试基本加法运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "multiply_function_basic" {
  // 测试基本乘法运算
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "greet_function_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "divide_with_ceil_positive" {
  // 测试向上取整除法的正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "divide_with_ceil_negative" {
  // 测试向上取整除法的负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative" {
  // 测试数学交换律
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
}

test "zero_element_properties" {
  // 测试零元素性质
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
}

test "complex_arithmetic" {
  // 测试复杂算术运算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "packaging_calculation" {
  // 测试包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "unicode_greeting" {
  // 测试 Unicode 问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}
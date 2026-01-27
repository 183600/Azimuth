// 标准 MoonBit 测试用例 - 10个高质量测试用例
// 测试 azimuth 库的核心功能

test "basic_addition_functionality" {
  // 基本加法功能测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(-5, 15))
}

test "multiplication_comprehensive" {
  // 乘法综合测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(100, multiply(10, 10))
}

test "greeting_function_variations" {
  // 问候函数变体测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "division_ceil_positive_numbers" {
  // 正数向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "division_ceil_negative_numbers" {
  // 负数向上取整除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
  assert_eq(add(-5, 10), add(10, -5))
  assert_eq(multiply(-5, 10), multiply(10, -5))
}

test "zero_element_properties" {
  // 零元素性质测试
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(0, multiply(-5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "complex_arithmetic_operations" {
  // 复杂算术运算测试
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let complex_result = divide_with_ceil(add(multiply(3, 4), multiply(2, 5)), 7)
  assert_eq(4, complex_result)  // ceil((12+10)/7) = ceil(22/7) = 4
}

test "real_world_packaging_scenario" {
  // 真实世界包装场景测试
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(30, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}
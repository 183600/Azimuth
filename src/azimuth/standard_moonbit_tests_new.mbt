// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行简洁而全面的测试

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "multiply_commutative_property" {
  // 测试乘法交换律
  assert_eq(12, multiply(3, 4))
  assert_eq(12, multiply(4, 3))
  assert_eq(0, multiply(0, 7))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-9, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let result = add(multiply(2, 4), multiply(3, 5))
  assert_eq(23, result)  // 2*4 + 3*5 = 8 + 15 = 23
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 12
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
}

test "real_world_packaging_calculation" {
  // 实际应用场景：包装计算
  let total_items = 28
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(28/6) = 5
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "unicode_greeting_functionality" {
  // 测试 Unicode 字符的问候功能
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, áéíóú!", greet("áéíóú"))
}
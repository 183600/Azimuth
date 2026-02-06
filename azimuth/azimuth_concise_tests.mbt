// Azimuth 简洁测试用例 - 标准 MoonBit 测试语法
// 测试核心功能：基本数学运算和字符串处理

test "add_function_basic" {
  // 基本加法功能测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
}

test "multiply_function_basic" {
  // 基本乘法功能测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
}

test "greet_function_basic" {
  // 基本问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive" {
  // 向上取整除法正数测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative" {
  // 向上取整除法负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "subtract_function_basic" {
  // 基本减法功能测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}

test "mathematical_commutative" {
  // 数学交换律测试
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, items_per_box)
  let remaining = subtract(total_capacity, total_items)
  assert_eq(5, remaining)
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let a = add(5, 3)
  let b = multiply(a, 2)
  let c = divide_with_ceil(b, 4)
  assert_eq(4, c)  // ((5+3)*2)/4 = 16/4 = 4
}

test "unicode_greeting" {
  // Unicode 字符问候测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}
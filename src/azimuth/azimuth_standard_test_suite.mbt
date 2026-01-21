// Azimuth 项目标准测试套件
// 使用标准 MoonBit 测试语法，包含核心功能测试

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-15, 5))
  assert_eq(1000, add(500, 500))
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(0, 42))
  assert_eq(-20, multiply(-4, 5))
  assert_eq(100, multiply(10, 10))
}

test "greeting_functionality" {
  // 测试问候函数
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "division_ceil_positive" {
  // 测试正数向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(7, divide_with_ceil(13, 2))
}

test "division_ceil_edge_cases" {
  // 测试除法边界情况
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(5, divide_with_ceil(25, 5))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  let price = 25
  let quantity = 4
  let discount = 15
  let subtotal = multiply(price, quantity)
  let total = add(subtotal, -discount)
  assert_eq(85, total)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 8
  let y = 12
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  // 零元素性质
  assert_eq(0, multiply(x, 0))
  assert_eq(x, add(x, 0))
}

test "packaging_calculation" {
  // 测试包装计算场景
  let total_items = 23
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)
}

test "unicode_greeting" {
  // 测试 Unicode 字符串问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, María!", greet("María"))
}
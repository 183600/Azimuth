// Azimuth 项目标准测试用例
// 测试核心功能：add、multiply 和 greet 函数

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-4, add(-2, -2))
}

test "add_with_zero" {
  // 测试与0相加的情况
  assert_eq(7, add(7, 0))
  assert_eq(7, add(0, 7))
  assert_eq(0, add(0, 0))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(0, multiply(5, 0))
}

test "multiply_with_one" {
  // 测试与1相乘的情况
  assert_eq(7, multiply(7, 1))
  assert_eq(7, multiply(1, 7))
  assert_eq(-7, multiply(-7, 1))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 1))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
  assert_eq_string("Hello, emoji😊!", greet("emoji😊"))
}

test "complex_calculation_scenario" {
  // 测试复合计算场景：计算矩形周长
  let length = 8
  let width = 5
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
}

test "real_world_application" {
  // 测试实际应用场景：购物车总价计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  // 总价 = 商品价格 × 数量 + 运费
  let total = add(multiply(item_price, quantity), shipping)
  assert_eq(612, total)
}
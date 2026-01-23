// 标准 MoonBit 测试套件
// 为 azimuth 库提供简洁而全面的测试覆盖

test "add_basic_functionality" {
  // 测试加法基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "multiply_basic_functionality" {
  // 测试乘法基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
}

test "greet_basic_functionality" {
  // 测试问候函数基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  assert_eq(add(7, 13), add(13, 7))  // 加法交换律
  assert_eq(multiply(7, 13), multiply(13, 7))  // 乘法交换律
  assert_eq(5, add(5, 0))  // 加法单位元
  assert_eq(0, multiply(5, 0))  // 乘法零元
}

test "complex_calculations" {
  // 测试复合计算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "business_logic_scenario" {
  // 测试业务逻辑场景：装盒问题
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "unicode_support" {
  // 测试Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "edge_cases" {
  // 测试边缘情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}
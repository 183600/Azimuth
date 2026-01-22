// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_test" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(7, 3))
}

test "multiply_function_basic_test" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(3, 5))
}

test "greet_function_test" {
  // 测试问候函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_function_test" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "mathematical_properties_test" {
  // 测试数学性质
  // 加法交换律
  assert_eq(add(5, 3), add(3, 5))
  // 乘法交换律
  assert_eq(multiply(4, 6), multiply(6, 4))
  // 零元素性质
  assert_eq(0, multiply(5, 0))
  assert_eq(5, add(5, 0))
}

test "edge_cases_test" {
  // 测试边界情况
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "combined_operations_test" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(3, 4), 2)
  assert_eq(14, result2)  // (3 + 4) * 2 = 7 * 2 = 14
}

test "practical_application_test" {
  // 测试实际应用场景
  // 计算购物车总价
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let total = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  assert_eq(105, total)  // 25*3 + 15*2 = 75 + 30 = 105
  
  // 计算需要的包装箱数量
  let total_items = 17
  let items_per_box = 5
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)  // ceil(17/5) = 4
}

test "unicode_and_special_characters_test" {
  // 测试 Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, 123!", greet("123"))
}
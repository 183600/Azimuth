// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能测试
// 包含基础功能和边界情况的测试

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(25, multiply(5, 5))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  // 加法交换律
  assert_eq(add(5, 3), add(3, 5))
  // 乘法交换律
  assert_eq(multiply(4, 6), multiply(6, 4))
  // 零元素性质
  assert_eq(0, multiply(5, 0))
  assert_eq(5, add(5, 0))
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况和特殊值
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
}

test "complex_combined_operations" {
  // 测试复杂的组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = divide_with_ceil(add(multiply(10, 3), 5), 7)
  assert_eq(5, result2)  // ceil((10*3 + 5)/7) = ceil(35/7) = 5
  
  let result3 = add(divide_with_ceil(25, 4), multiply(3, 2))
  assert_eq(13, result3)  // ceil(25/4) + 3*2 = 7 + 6 = 13
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let total = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  assert_eq(105, total)  // 25*3 + 15*2 = 75 + 30 = 105
  
  // 包装计算
  let total_items = 17
  let items_per_box = 5
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)  // ceil(17/5) = 4
}

test "string_and_unicode_handling" {
  // 测试字符串和Unicode处理
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, 123!", greet("123"))
}
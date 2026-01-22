// 标准 MoonBit 单元测试用例
// 针对 azimuth 库核心功能的标准测试

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-8, multiply(-2, 4))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试基本向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-5, add(-2, -3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "edge_case_operations" {
  // 测试边界情况
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(3, 4), 2)
  assert_eq(14, result2)  // (3 + 4) * 2 = 7 * 2 = 14
}

test "practical_applications" {
  // 测试实际应用场景
  // 包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 价格计算
  let price = 15
  let quantity = 3
  let total_cost = multiply(price, quantity)
  assert_eq(45, total_cost)
}

test "string_and_unicode_support" {
  // 测试字符串和Unicode支持
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, María!", greet("María"))
}
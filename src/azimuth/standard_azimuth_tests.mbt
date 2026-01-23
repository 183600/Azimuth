// Azimuth 库的标准测试用例
// 测试核心功能：加法、乘法、问候和向上取整除法

test "add_function_basic" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "greet_function_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试基本向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
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
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "edge_cases" {
  // 测试边界情况
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(101, 100))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(10000, multiply(100, 100))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = add(divide_with_ceil(10, 3), multiply(2, 2))
  assert_eq(8, result2)  // ceil(10/3) + 2*2 = 4 + 4 = 8
}

test "practical_scenarios" {
  // 测试实际应用场景
  // 包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 购物车总价
  let item1_price = 29
  let item1_qty = 2
  let item2_price = 15
  let item2_qty = 3
  let total = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  assert_eq(103, total)  // 29*2 + 15*3 = 58 + 45 = 103
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, 123!", greet("123"))
}
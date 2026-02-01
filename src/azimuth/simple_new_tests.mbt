// 简单的新 MoonBit 测试用例
// 针对 azimuth 库核心功能的基础测试

test "simple_add_test" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "simple_multiply_test" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-4, multiply(-2, 2))
}

test "simple_greet_test" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "simple_divide_ceil_test" {
  // 测试基本向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "simple_math_properties" {
  // 测试基本数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "simple_business_calculation" {
  // 测试简单业务计算
  let price = 25
  let quantity = 4
  let total = multiply(price, quantity)
  
  assert_eq(100, total)
}

test "simple_packaging_test" {
  // 测试包装计算
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "simple_unicode_test" {
  // 测试 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "simple_negative_test" {
  // 测试负数运算
  assert_eq(-5, add(-2, -3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "simple_edge_cases" {
  // 测试边界情况
  assert_eq(1, add(0, 1))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, divide_with_ceil(1, 100))
}
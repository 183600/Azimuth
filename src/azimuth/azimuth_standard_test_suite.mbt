// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能测试
// 包含10个简洁但全面的测试用例

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(25, multiply(5, 5))
}

test "greet_function_basic_cases" {
  // 测试基本问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic_positive" {
  // 测试正数的向上取整除法
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "divide_with_ceil_negative_cases" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "mathematical_properties" {
  // 测试基本数学性质
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

test "combined_operations_simple" {
  // 测试简单的组合运算
  let result = add(multiply(2, 4), multiply(3, 3))
  assert_eq(17, result)  // 2*4 + 3*3 = 8 + 9 = 17
}

test "practical_packaging_scenario" {
  // 实际包装场景测试
  let total_items = 23
  let items_per_box = 5
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)  // ceil(23/5) = 5
}

test "unicode_greeting_support" {
  // 测试Unicode字符支持
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Español!", greet("Español"))
}

test "edge_case_operations" {
  // 边界情况测试
  // 测试1的运算
  assert_eq(1, multiply(1, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(5, multiply(5, 1))
  
  // 测试相等数的除法
  assert_eq(1, divide_with_ceil(7, 7))
  assert_eq(1, divide_with_ceil(-7, -7))
  
  // 测试加法逆元
  assert_eq(0, add(10, -10))
  assert_eq(0, add(-15, 15))
}
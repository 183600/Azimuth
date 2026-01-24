// 标准 MoonBit 测试用例 - 针对 azimuth 库核心功能
// 包含 10 个简洁高效的测试用例

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 7))
  assert_eq(-12, multiply(-3, 4))
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
  let a = 6
  let b = 9
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-5, -3))
  assert_eq(-2, add(-5, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "zero_handling" {
  // 测试零的处理
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "combined_operations" {
  // 测试组合运算
  let result = add(multiply(2, 4), multiply(3, 5))
  assert_eq(23, result)  // 2*4 + 3*5 = 8 + 15 = 23
}

test "practical_packaging_scenario" {
  // 实际应用场景：包装计算
  let total_items = 28
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(28/6) = 5
}

test "string_and_number_integration" {
  // 测试字符串和数字的集成
  let numeric_name = "User123"
  let greeting = greet(numeric_name)
  let user_id = divide_with_ceil(123, 10)
  assert_eq_string("Hello, User123!", greeting)
  assert_eq(13, user_id)  // ceil(12.3) = 13
}
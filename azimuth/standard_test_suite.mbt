// 标准 MoonBit 测试套件 - 包含10个高质量测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_basic_functionality" {
  // 测试乘法函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_string_operations" {
  // 测试问候函数的字符串操作
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-2, divide_with_ceil(-4, 2))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律性质
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "zero_element_properties" {
  // 测试零元素性质
  assert_eq(5, add(5, 0))
  assert_eq(-3, add(-3, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 100))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let complex_result = multiply(add(10, 5), divide_with_ceil(25, 4))
  assert_eq(56, complex_result)  // 15 * ceil(25/4) = 15 * 7 = 105
}

test "real_world_packaging_calculation" {
  // 实际应用场景：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  let total_cost = multiply(boxes_needed, 10)  // 每个箱子10元
  assert_eq(50, total_cost)
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 123!", greet("123"))
}
// 标准 MoonBit 质量测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_basic_operations" {
  // 测试加法的基本操作
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_comprehensive" {
  // 测试乘法的全面功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(25, multiply(5, 5))
}

test "divide_with_ceil_various_cases" {
  // 测试向上取整除法的各种情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "greet_function_different_inputs" {
  // 测试问候函数的不同输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = divide_with_ceil(add(10, 5), 3)
  assert_eq(5, result2)  // ceil(15/3) = 5
  
  let result3 = multiply(add(7, 3), divide_with_ceil(20, 4))
  assert_eq(50, result3)  // 10 * 5 = 50
}

test "edge_case_operations" {
  // 测试边界情况操作
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "negative_number_operations" {
  // 测试负数操作
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-9, -3))
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let total = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  assert_eq(105, total)  // 25*3 + 15*2 = 75 + 30 = 105
  
  // 包装计算
  let total_items = 23
  let items_per_box = 5
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(23/5) = 5
}

test "string_processing_with_greet" {
  // 测试字符串处理与问候函数
  let first_name = "John"
  let last_name = "Doe"
  let full_name = first_name + " " + last_name
  let greeting = greet(full_name)
  assert_eq_string("Hello, John Doe!", greeting)
  
  // 测试特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
  
  // 分配律 a * (b + c) = a * b + a * c
  let c = 5
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)  // 7 * (13 + 5) = 7 * 13 + 7 * 5 = 126
}
// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_addition_functionality" {
  // 基本加法功能测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(2, -3))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 基本乘法功能测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(3, -5))
  assert_eq(16, multiply(4, 4))
}

test "basic_subtraction_functionality" {
  // 基本减法功能测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(10, 10))
  assert_eq(15, subtract(5, -10))
}

test "divide_with_ceil_basic_cases" {
  // 向上取整除法基本测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_standard_cases" {
  // 问候函数标准测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, Test!", greet("Test"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let base = 20
  let multiplier = 3
  let addend = 15
  
  let result1 = multiply(base, multiplier)
  let result2 = add(result1, addend)
  let result3 = divide_with_ceil(result2, 5)
  
  assert_eq(60, result1)
  assert_eq(75, result2)
  assert_eq(15, result3)
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 减法验证
  assert_eq(add(subtract(a, b), b), a)
}

test "business_calculation_scenario" {
  // 业务计算场景测试
  let items_per_box = 12
  let total_items = 35
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(3, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, items_per_box)
  let remaining_space = subtract(total_capacity, total_items)
  
  assert_eq(36, total_capacity)
  assert_eq(1, remaining_space)
}

test "error_handling_validation" {
  // 错误处理验证测试
  // 验证除零错误的安全处理
  assert_eq(0, divide_with_ceil(50, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 验证字符串函数的边界处理
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}
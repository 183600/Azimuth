// 用户标准测试用例 - 2024年新增10个高质量 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_basic_functionality" {
  // 测试加法基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "multiply_basic_functionality" {
  // 测试乘法基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "subtract_basic_functionality" {
  // 测试减法基本功能
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_basic_functionality" {
  // 测试问候函数基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 10
  let y = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
}

test "negative_numbers_handling" {
  // 测试负数处理
  assert_eq(-1, add(-3, 2))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-5, subtract(0, 5))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "edge_cases" {
  // 测试边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, add(1, 0))
  assert_eq(5, multiply(5, 1))
}

test "complex_calculation" {
  // 测试复杂计算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "business_logic_scenario" {
  // 测试业务逻辑场景
  let items = 25
  let capacity = 6
  let boxes_needed = divide_with_ceil(items, capacity)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}
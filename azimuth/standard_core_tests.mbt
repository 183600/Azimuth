// 标准 MoonBit 测试用例 - azimuth 核心功能测试
// 测试文件：standard_core_tests.mbt

test "add_function_positive_numbers" {
  // 测试加法函数的正数运算
  assert_eq(5, add(2, 3))
  assert_eq(10, add(7, 3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(0, multiply(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(25, multiply(5, 5))
  assert_eq(-15, multiply(-3, 5))
}

test "greet_function_different_inputs" {
  // 测试问候函数的不同输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, Test!", greet("Test"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "subtract_function_simple_cases" {
  // 测试减法函数的简单情况
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let base = 10
  let doubled = multiply(base, 2)
  let added = add(doubled, 5)
  let divided = divide_with_ceil(added, 3)
  assert_eq(9, divided)  // ceil((10*2+5)/3) = ceil(25/3) = 9
}

test "mathematical_commutative_law" {
  // 测试数学交换律
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "edge_case_zero_operations" {
  // 测试零运算的边缘情况
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(5, subtract(5, 0))
  assert_eq(-5, subtract(0, 5))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-5, add(-2, -3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "real_world_application_test" {
  // 真实世界应用测试：计算购物车和包装
  let items = 23
  let box_capacity = 5
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  // 验证需要的箱子数量
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(25, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(2, remaining_space)
}
// 简洁的核心功能测试用例
// 专注于测试 add, multiply, greet 三个核心函数

// 测试用例1: 基本加法功能测试
test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(5, azimuth_lib::add(2, 3))
  assert_eq(0, azimuth_lib::add(-2, 2))
  assert_eq(-5, azimuth_lib::add(-2, -3))
}

// 测试用例2: 零值处理测试
test "add_zero_handling" {
  // 测试零值处理
  assert_eq(5, azimuth_lib::add(5, 0))
  assert_eq(5, azimuth_lib::add(0, 5))
  assert_eq(0, azimuth_lib::add(0, 0))
}

// 测试用例3: 边界值测试
test "add_boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth_lib::add(max_val, 1))
  assert_eq(min_val, azimuth_lib::add(min_val, -1))
  assert_eq(-1, azimuth_lib::add(max_val, min_val))
}

// 测试用例4: 基本乘法功能测试
test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, azimuth_lib::multiply(2, 3))
  assert_eq(-6, azimuth_lib::multiply(-2, 3))
  assert_eq(6, azimuth_lib::multiply(-2, -3))
}

// 测试用例5: 零和一乘法测试
test "multiply_zero_and_one" {
  // 测试零和一的乘法
  assert_eq(0, azimuth_lib::multiply(5, 0))
  assert_eq(5, azimuth_lib::multiply(5, 1))
  assert_eq(5, azimuth_lib::multiply(1, 5))
}

// 测试用例6: 乘法边界值测试
test "multiply_boundary_values" {
  // 测试乘法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, azimuth_lib::multiply(max_val, 0))
  assert_eq(min_val, azimuth_lib::multiply(min_val, -1))
  assert_eq(max_val, azimuth_lib::multiply(max_val, 1))
}

// 测试用例7: 基本问候功能测试
test "greet_basic_functionality" {
  // 测试基本的问候功能
  assert_eq_string("Hello, World!", azimuth_lib::greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth_lib::greet("MoonBit"))
}

// 测试用例8: 问候函数边界情况测试
test "greet_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, !", azimuth_lib::greet(""))
  assert_eq_string("Hello, A!", azimuth_lib::greet("A"))
  assert_eq_string("Hello, 123!", azimuth_lib::greet("123"))
}

// 测试用例9: 复合计算测试
test "complex_calculation" {
  // 测试复合计算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result = azimuth_lib::multiply(azimuth_lib::add(a, b), c)
  assert_eq(900, result)
  
  // a * b + c
  let result2 = azimuth_lib::add(azimuth_lib::multiply(a, b), c)
  assert_eq(230, result2)
}

// 测试用例10: 实际应用场景测试
test "real_world_scenario" {
  // 测试实际应用场景
  // 计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = azimuth_lib::multiply(item_price, quantity)
  let total = azimuth_lib::add(subtotal, shipping)
  
  assert_eq(617, total)
}
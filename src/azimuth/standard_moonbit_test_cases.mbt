// 标准 MoonBit 测试用例 - 专业级测试覆盖
// 测试 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 基础算术运算测试
  assert_eq(add(2, 3), 5)
  assert_eq(multiply(4, 5), 20)
  assert_eq(subtract(10, 3), 7)
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
}

test "string_operations" {
  // 字符串操作测试
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
}

test "edge_case_arithmetic" {
  // 边界条件算术测试
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 100), 0)
  assert_eq(subtract(0, 0), 0)
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(5, 1), 5)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(add(-5, 3), -2)
  assert_eq(multiply(-2, 3), -6)
  assert_eq(subtract(-5, -3), -2)
  assert_eq(divide_with_ceil(-7, 3), -2)
  assert_eq(divide_with_ceil(-6, -3), 2)
}

test "division_precision" {
  // 除法精度测试
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(10, 4), 3)
  assert_eq(divide_with_ceil(15, 5), 3)
  assert_eq(divide_with_ceil(100, 99), 2)
  assert_eq(divide_with_ceil(99, 100), 1)
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let step1 = add(5, 3)
  let step2 = multiply(step1, 2)
  let step3 = subtract(step2, 4)
  let step4 = divide_with_ceil(step3, 3)
  
  assert_eq(step1, 8)
  assert_eq(step2, 16)
  assert_eq(step3, 12)
  assert_eq(step4, 4)
}

test "boundary_values" {
  // 边界值测试
  assert_eq(add(1000, 1000), 2000)
  assert_eq(multiply(100, 100), 10000)
  assert_eq(subtract(10000, 1), 9999)
  assert_eq(divide_with_ceil(10000, 3), 3334)
}

test "string_edge_cases" {
  // 字符串边界情况测试
  let long_name = "VeryLongNameThatTestsStringHandling"
  assert_eq_string(greet(long_name), "Hello, " + long_name + "!")
  
  let special_chars = "!@#$%^&*()"
  assert_eq_string(greet(special_chars), "Hello, " + special_chars + "!")
}

test "mathematical_properties" {
  // 数学性质验证测试
  // 加法交换律
  assert_eq(add(5, 3), add(3, 5))
  // 乘法交换律
  assert_eq(multiply(4, 6), multiply(6, 4))
  // 减法与加法的关系
  assert_eq(subtract(10, 3), add(10, -3))
  // 除法与乘法的关系（近似）
  assert_eq(divide_with_ceil(multiply(7, 3), 3), 7)
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  // 除零安全处理
  assert_eq(divide_with_ceil(5, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(-5, 0), 0)
  
  // 大数运算
  assert_eq(add(50000, 50000), 100000)
  assert_eq(multiply(1000, 100), 100000)
}
// Enhanced Azimuth Test Suite - 高级测试用例集合
// 测试 azimuth 库的核心功能和边界条件

///|
test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(add(5, 3), 8)
  assert_eq(multiply(4, 6), 24)
  assert_eq(subtract(10, 4), 6)
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
}

///|
test "divide_with_ceil_edge_cases" {
  // 测试除法向上取整的边界情况
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(5, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), -3)
  assert_eq(divide_with_ceil(-10, -3), 4)
}

///|
test "string_operations" {
  // 测试字符串操作
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
}

///|
test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let step1 = add(10, 5)
  let step2 = multiply(step1, 2)
  let step3 = subtract(step2, 8)
  let step4 = divide_with_ceil(step3, 3)
  
  assert_eq(step1, 15)
  assert_eq(step2, 30)
  assert_eq(step3, 22)
  assert_eq(step4, 8)
}

///|
test "mathematical_series" {
  // 测试数学级数计算
  let sum1 = add(add(add(1, 2), 3), 4)
  let sum2 = add(sum1, 5)
  let product = multiply(2, multiply(3, 4))
  let average = divide_with_ceil(sum2, 5)
  
  assert_eq(sum1, 10)
  assert_eq(sum2, 15)
  assert_eq(product, 24)
  assert_eq(average, 3)
}

///|
test "boundary_value_analysis" {
  // 测试边界值分析
  assert_eq(divide_with_ceil(1, 1), 1)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(99, 100), 1)
  assert_eq(divide_with_ceil(100, 100), 1)
  assert_eq(divide_with_ceil(101, 100), 2)
}

///|
test "negative_number_operations" {
  // 测试负数运算
  assert_eq(add(-5, 3), -2)
  assert_eq(add(-5, -3), -8)
  assert_eq(multiply(-4, 6), -24)
  assert_eq(multiply(-4, -6), 24)
  assert_eq(subtract(-10, 4), -14)
  assert_eq(subtract(10, -4), 14)
}

///|
test "large_number_operations" {
  // 测试大数运算
  let large_num = 1000000
  assert_eq(add(large_num, large_num), 2000000)
  assert_eq(multiply(large_num, 2), 2000000)
  assert_eq(subtract(large_num, 500000), 500000)
  assert_eq(divide_with_ceil(large_num, 3), 333334)
}

///|
test "functional_composition" {
  // 测试函数组合
  let compose_add_greet = fn(x : Int) : String {
    greet(x.to_string())
  }
  
  let result1 = compose_add_greet(42)
  let result2 = greet(add(1, 1).to_string())
  
  assert_eq_string(result1, "Hello, 42!")
  assert_eq_string(result2, "Hello, 2!")
}

///|
test "error_handling_scenarios" {
  // 测试错误处理场景
  // 测试除零行为
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(-10, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  
  // 测试极值情况
  let max_int = 2147483647
  let min_int = -2147483648
  
  assert_eq(add(max_int, 0), max_int)
  assert_eq(add(min_int, 0), min_int)
  assert_eq(multiply(max_int, 1), max_int)
  assert_eq(multiply(min_int, 1), min_int)
}
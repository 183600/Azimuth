// 新的综合测试套件 - 10个MoonBit测试用例
// 测试不同的功能和场景，确保代码的正确性和健壮性

test "mathematical_operations_test" {
  // 测试基本数学运算
  assert_eq(15, add(7, 8))
  assert_eq(42, multiply(6, 7))
  assert_eq(0, add(5, -5))
  assert_eq(-24, multiply(-3, 8))
}

test "boundary_value_test" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
}

test "string_processing_test" {
  // 测试字符串处理
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_calculation_test" {
  // 测试复杂计算场景
  let a = 10
  let b = 20
  let c = 5
  
  // 计算 (a + b) * c
  let result = multiply(add(a, b), c)
  assert_eq(150, result)
  
  // 计算 a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(205, result2)
}

test "real_world_scenario_test" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let cart_total = add(total1, total2)
  
  assert_eq(697, cart_total)
}

test "assert_function_test" {
  // 测试断言函数
  assert_true(5 > 3)
  assert_false(5 < 3)
  assert_eq(100, add(50, 50))
  assert_eq_string("Hello, Test!", greet("Test"))
}

test "error_handling_test" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  
  // 确保溢出被正确处理
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
  
  // 测试负数运算
  assert_true(add(-10, -20) == -30)
  assert_true(multiply(-5, 5) == -25)
}

test "sequential_operations_test" {
  // 测试连续运算
  let result = add(add(1, 2), add(3, 4))
  assert_eq(10, result)
  
  let result2 = multiply(multiply(2, 3), multiply(4, 5))
  assert_eq(120, result2)
  
  let result3 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result3)
}

test "special_characters_test" {
  // 测试特殊字符处理
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
}

test "mathematical_properties_test" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
  
  // 结合律
  assert_eq(add(add(a, b), 5), add(a, add(b, 5)))
}
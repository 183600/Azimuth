// 标准 MoonBit 测试用例
// 覆盖核心功能的各种场景

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(50, 50))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(0, multiply(0, 123))
  assert_eq(1, multiply(1, 1))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 张三!", greet("张三"))
}

test "mathematical_operations_combination" {
  // 测试数学运算的组合
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let sum = add(a, b)
  let product = multiply(sum, c)
  assert_eq(16, product)
  
  // a * b + c
  let product2 = multiply(a, b)
  let sum2 = add(product2, c)
  assert_eq(17, sum2)
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  let very_long_name = "ThisIsAVeryLongNameThatContainsManyCharactersAndShouldStillWorkProperly"
  let result = greet(very_long_name)
  assert_eq_string("Hello, " + very_long_name + "!", result)
  
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result2 = greet(special_chars)
  assert_eq_string("Hello, " + special_chars + "!", result2)
}

test "real_world_calculation_scenarios" {
  // 测试实际应用场景的计算
  // 购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let cart_total = add(total1, total2)
  
  assert_eq(697, cart_total)
}

test "assertion_functions_verification" {
  // 测试断言函数的正确性
  assert_true(5 > 3)
  assert_false(3 > 5)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "complex_calculation_pipeline" {
  // 测试复杂的计算流水线
  let initial_value = 100
  let multiplier = 3
  let addition = 50
  let final_multiplier = 2
  
  // ((initial_value * multiplier) + addition) * final_multiplier
  let step1 = multiply(initial_value, multiplier)
  let step2 = add(step1, addition)
  let final_result = multiply(step2, final_multiplier)
  
  assert_eq(700, final_result)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保边界值运算不会导致崩溃
  let result1 = add(max_val, max_val)
  let result2 = add(min_val, min_val)
  let result3 = multiply(max_val, max_val)
  let result4 = multiply(min_val, min_val)
  
  // 验证结果在合理范围内
  assert_true(result1 <= max_val)
  assert_true(result2 >= min_val)
  assert_true(result3 <= max_val)
  assert_true(result4 >= min_val)
}
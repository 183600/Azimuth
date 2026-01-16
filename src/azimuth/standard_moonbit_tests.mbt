// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含10个测试用例

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试最小值溢出保护
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试零值加法
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值乘法
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  
  // 测试单位元乘法
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(max_val, multiply(1, max_val))
  assert_eq(min_val, multiply(1, min_val))
  
  // 测试负一乘法（包含最小值特殊情况）
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(-max_val, multiply(max_val, -1))
}

test "greet_function_various_inputs" {
  // 测试greet函数的各种输入情况
  // 测试空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 测试单字符
  assert_eq_string("Hello, A!", greet("A"))
  
  // 测试普通字符串
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, test@123!", greet("test@123"))
  
  // 测试Unicode字符
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 10
  let b = 5
  let c = 3
  
  // 测试 (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(45, result1)
  
  // 测试 a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(53, result2)
  
  // 测试 a + b * c
  let result3 = add(a, multiply(b, c))
  assert_eq(25, result3)
  
  // 测试 (a - b) * c
  let result4 = multiply(add(a, -b), c)
  assert_eq(15, result4)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 12
  let y = 8
  let z = 5
  
  // 验证加法结合律: (x + y) + z = x + (y + z)
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 验证乘法结合律: (x * y) * z = x * (y * z)
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 验证分配律: x * (y + z) = x * y + x * z
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "real_world_calculation_scenarios" {
  // 实际应用场景计算测试
  // 场景1: 计算矩形周长
  let length = 8
  let width = 5
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(26, perimeter)
  
  // 场景2: 计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let total = add(multiply(item1_price, item1_quantity), multiply(item2_price, item2_quantity))
  assert_eq(697, total)
  
  // 场景3: 简单利息计算
  let principal = 1000
  let rate = 5
  let years = 3
  let interest = multiply(principal, multiply(rate, years))
  let total_amount = add(principal, interest)
  assert_eq(16000, total_amount)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringThatTestsTheGreetFunctionWithExtendedInput"
  let result1 = greet(long_string)
  assert_eq_string("Hello, " + long_string + "!", result1)
  
  // 测试包含引号的字符串
  let quoted_string = "\"John Doe\""
  let result2 = greet(quoted_string)
  assert_eq_string("Hello, \"John Doe\"!", result2)
  
  // 测试包含换行符的字符串
  let newline_string = "Line1\nLine2"
  let result3 = greet(newline_string)
  assert_eq_string("Hello, Line1\nLine2!", result3)
  
  // 测试包含制表符的字符串
  let tab_string = "Col1\tCol2"
  let result4 = greet(tab_string)
  assert_eq_string("Hello, Col1\tCol2!", result4)
}

test "boundary_condition_combinations" {
  // 边界条件组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 测试最大值与各种边界值的组合
  assert_eq(max_val, add(max_val, zero))
  assert_eq(max_val, multiply(max_val, one))
  assert_eq(min_val, multiply(max_val, neg_one))
  assert_eq(-1, add(max_val, min_val))
  
  // 测试最小值与各种边界值的组合
  assert_eq(min_val, add(min_val, zero))
  assert_eq(min_val, multiply(min_val, one))
  assert_eq(max_val, multiply(min_val, neg_one))
  assert_eq(min_val, add(min_val, max_val))
  
  // 测试零的特殊性质
  assert_eq(zero, multiply(zero, max_val))
  assert_eq(zero, multiply(zero, min_val))
  assert_eq(max_val, add(zero, max_val))
  assert_eq(min_val, add(zero, min_val))
}

test "sequential_operations_test" {
  // 连续操作测试
  // 测试连续加法
  let sum1 = add(add(1, 2), add(3, 4))
  assert_eq(10, sum1)
  
  let sum2 = add(add(add(1, 2), 3), 4)
  assert_eq(10, sum2)
  
  // 测试连续乘法
  let product1 = multiply(multiply(2, 3), multiply(4, 5))
  assert_eq(120, product1)
  
  let product2 = multiply(multiply(multiply(2, 3), 4), 5)
  assert_eq(120, product2)
  
  // 测试混合连续操作
  let complex1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, complex1)
  
  let complex2 = multiply(add(2, 3), add(4, 5))
  assert_eq(45, complex2)
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试溢出保护的正确性
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 测试正常运算的正确性
  assert_true(add(5, 3) == 8)
  assert_true(multiply(4, 6) == 24)
  assert_true(greet("Test") == "Hello, Test!")
}
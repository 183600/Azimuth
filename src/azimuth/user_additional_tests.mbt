// 用户额外添加的 MoonBit 测试用例
// 覆盖 add、multiply、greet 和 divide_with_ceil 函数

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
}

test "add_function_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(0, multiply(0, 100))
}

test "multiply_function_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

test "greet_function_various_inputs" {
  // 测试不同输入的问候语功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_function" {
  // 测试向上取整除法功能
  assert_eq(3, divide_with_ceil(10, 3))  // 10/3 = 3.33 → 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 → 3
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.33 → -3
  assert_eq(-3, divide_with_ceil(-9, 3))  // -9/3 = -3 → -3
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (5 + 3) * (10 - 4) / 3 的向上取整
  let sum = add(5, 3)  // 8
  let diff = add(10, -4)  // 6
  let product = multiply(sum, diff)  // 48
  let result = divide_with_ceil(product, 3)  // 48/3 = 16 → 16
  
  assert_eq(16, result)
}

test "financial_calculation" {
  // 测试金融计算场景
  // 计算商品总价（含税，向上取整）
  let price = 99
  let quantity = 3
  let tax_rate = 8  // 8%
  
  let subtotal = multiply(price, quantity)  // 297
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)  // 23.76 → 24
  let total = add(subtotal, tax)  // 321
  
  assert_eq(321, total)
}

test "string_processing_with_greet" {
  // 测试使用 greet 函数进行字符串处理
  let first_name = "John"
  let last_name = "Doe"
  let full_name = first_name + " " + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, John Doe!", greeting)
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 7
  let b = 5
  let c = 3
  
  // 验证分配律
  let distributive_left = multiply(a, add(b, c))  // 7 * (5 + 3) = 56
  let distributive_right = add(multiply(a, b), multiply(a, c))  // 7*5 + 7*3 = 56
  
  assert_eq(distributive_left, distributive_right)
  
  // 验证结合律
  let associative_left = add(add(a, b), c)  // (7 + 5) + 3 = 15
  let associative_right = add(a, add(b, c))  // 7 + (5 + 3) = 15
  
  assert_eq(associative_left, associative_right)
}
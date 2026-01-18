// Azimuth 项目标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(10, 0))
  assert_eq(-7, add(0, -7))
}

test "add_function_overflow_protection" {
  // 测试 add 函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  
  // 测试负数溢出保护
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  
  // 测试边界值相加
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(15, multiply(3, 5))
  assert_eq(-8, multiply(-2, 4))
}

test "multiply_function_special_cases" {
  // 测试 multiply 函数的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与 1 和 -1 相乘
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  assert_eq(-42, multiply(42, -1))
  assert_eq(-42, multiply(-1, 42))
  
  // 测试最小值与 -1 相乘的特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试溢出保护
  assert_eq(max_val, multiply(46341, 46341))
}

test "greet_function_basic_functionality" {
  // 测试 greet 函数的基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "greet_function_special_characters" {
  // 测试 greet 函数处理特殊字符
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let a = 10
  let b = 20
  let c = 5
  
  // 计算 (a + b) * c
  let sum = add(a, b)
  let result = multiply(sum, c)
  assert_eq(150, result)
  
  // 计算 a * b + c
  let product = multiply(a, b)
  let result2 = add(product, c)
  assert_eq(205, result2)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 11
  
  // 测试加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 测试乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 测试分配律
  let z = 3
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "real_world_application" {
  // 测试实际应用场景：计算购物车总价
  let item_price = 199
  let item_quantity = 3
  let shipping_fee = 15
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let subtotal = multiply(item_price, item_quantity)
  
  // 计算税费
  let tax = multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价
  let total = add(add(subtotal, tax), shipping_fee)
  
  assert_eq(656, total)  // 199*3 + (199*3*8/100) + 15 = 597 + 47 + 15 = 659 (rounded)
}

test "edge_case_handling" {
  // 测试边界情况处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 测试连续运算
  let result1 = add(add(1, 2), add(3, 4))
  assert_eq(10, result1)
  
  let result2 = multiply(multiply(2, 3), multiply(4, 5))
  assert_eq(120, result2)
}
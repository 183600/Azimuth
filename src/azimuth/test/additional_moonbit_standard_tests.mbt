// 额外的 MoonBit 标准测试用例
// 测试 azimuth 包的基本功能

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-1, -1))
}

test "add_function_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  
  // 测试零值
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(5, multiply(5, 1))
  assert_eq(-6, multiply(2, -3))
}

test "multiply_function_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试特殊值
  assert_eq(min_val, multiply(min_val, 1))
}

test "greet_function_standard" {
  // 测试标准的 greet 功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
}

test "greet_function_special_cases" {
  // 测试 greet 函数的特殊情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "complex_calculations" {
  // 测试复杂计算场景
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let sum_ab = add(a, b)
  let result = multiply(sum_ab, c)
  assert_eq(900, result)
  
  // a * b + c
  let product_ab = multiply(a, b)
  let result2 = add(product_ab, c)
  assert_eq(230, result2)
}

test "assert_functions_validation" {
  // 测试断言函数的有效性
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试复合条件
  let x = 5
  let y = 10
  assert_true(x < y)
  assert_false(x > y)
  assert_eq(add(x, y), 15)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let price = 199
  let quantity = 3
  let shipping = 15
  let total = add(multiply(price, quantity), shipping)
  assert_eq(612, total)
  
  // 温度转换（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 交换律
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 结合律
  let c = 5
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}
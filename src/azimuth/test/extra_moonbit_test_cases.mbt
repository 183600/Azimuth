// 额外的 MoonBit 测试用例
// 测试 azimuth 包中的核心功能

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
  assert_eq(0, multiply(0, 12345))
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
  // 测试greet函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试123!", greet("测试123"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum = add(a, b)  // 15
  let product = multiply(sum, c)  // 45
  let result = add(product, -d)  // 38
  
  assert_eq(38, result)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
}

test "practical_application" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let cart_total = add(total1, total2)
  
  assert_eq(697, cart_total)  // 199*2 + 299*1 = 398 + 299 = 697
}

test "assert_functions_validation" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  
  // 测试各种边界组合
  assert_eq(max_val, add(max_val, zero))
  assert_eq(min_val, add(min_val, zero))
  assert_eq(zero, multiply(max_val, zero))
  assert_eq(zero, multiply(min_val, zero))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
}
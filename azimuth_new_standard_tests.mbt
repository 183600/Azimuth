// 新的标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_function_basic" {
  // 基本加法功能测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(-3, 3))
  assert_eq(-10, add(-4, -6))
}

test "add_function_edge_cases" {
  // 加法边界情况测试
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
  
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
}

test "multiply_function_basic" {
  // 基本乘法功能测试
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(7, 0))
  assert_eq(1, multiply(1, 7))
}

test "multiply_function_edge_cases" {
  // 乘法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(0, multiply(min_val, 0))
}

test "greet_function_variations" {
  // greet 函数变体测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试：计算 (a + b) * c
  let a = 4
  let b = 6
  let c = 3
  
  let sum = add(a, b)
  let result = multiply(sum, c)
  assert_eq(30, result)
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 7
  let y = 9
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
}

test "real_world_application" {
  // 实际应用场景测试：计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(617, total)
}

test "boundary_value_combinations" {
  // 边界值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "assert_function_verification" {
  // 断言功能验证测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}
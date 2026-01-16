// 标准 MoonBit 测试用例示例
// 测试 azimuth 库的核心功能

test "add_function_basic" {
  // 基本加法测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
}

test "add_function_edge_cases" {
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic" {
  // 基本乘法测试
  assert_eq(12, multiply(3, 4))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(0, 100))
}

test "multiply_function_edge_cases" {
  // 乘法边界测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_variations" {
  // greet 函数变体测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算 (a + b) * c - d
  let a = 8
  let b = 7
  let c = 3
  let d = 10
  
  let sum = add(a, b)
  let product = multiply(sum, c)
  let result = add(product, -d)
  
  assert_eq(41, result)
}

test "mathematical_properties" {
  // 数学性质验证
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 2)), add(multiply(x, y), multiply(x, 2)))
}

test "real_world_application" {
  // 实际应用：计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(617, total)
}

test "divide_with_ceil_function" {
  // 向上取整除法测试
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(2, 5))
}
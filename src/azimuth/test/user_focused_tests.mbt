// 用户定制的 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(7, add(3, 4))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-3, -2))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(12, multiply(3, 4))
  assert_eq(-8, multiply(-2, 4))
  assert_eq(6, multiply(-2, -3))
  assert_eq(0, multiply(0, 5))
}

test "multiply_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

test "greet_function_tests" {
  // 测试问候函数
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation_test" {
  // 测试复合计算
  // 计算 (a + b) * c
  let a = 5
  let b = 3
  let c = 4
  
  let sum_ab = add(a, b)  // 8
  let result = multiply(sum_ab, c)  // 32
  
  assert_eq(32, result)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 6
  let y = 9
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
}

test "real_world_application" {
  // 测试实际应用场景
  // 计算购物车总价：商品价格 × 数量 + 运费
  let price = 25
  let quantity = 3
  let shipping = 10
  
  let subtotal = multiply(price, quantity)  // 75
  let total = add(subtotal, shipping)  // 85
  
  assert_eq(85, total)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  
  // 边界值与零的运算
  assert_eq(max_val, add(max_val, zero))
  assert_eq(min_val, add(min_val, zero))
  assert_eq(zero, multiply(max_val, zero))
  assert_eq(zero, multiply(min_val, zero))
}
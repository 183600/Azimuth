// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(1000, multiply(10, 100))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边界情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "divide_with_ceil_function_boundary" {
  // 测试向上取整除法函数的边界情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "greet_function_various_names" {
  // 测试问候函数的各种名字输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 用户!", greet("用户"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let base = 100
  let multiplier = 3
  let addition = 15
  let divisor = 4
  
  let result = divide_with_ceil(add(multiply(base, multiplier), addition), divisor)
  assert_eq(79, result)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元
  assert_eq(add(a, 0), a)
  
  // 乘法单位元
  assert_eq(multiply(a, 1), a)
}

test "negative_number_operations_comprehensive" {
  // 负数运算综合测试
  assert_eq(-8, add(-5, -3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-2, subtract(-5, -3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(2, divide_with_ceil(-5, -3))
}

test "large_number_operations_stability" {
  // 大数运算稳定性测试
  let large_num = 10000
  assert_eq(20000, add(large_num, large_num))
  assert_eq(100000000, multiply(large_num, large_num))
  assert_eq(0, subtract(large_num, large_num))
  assert_eq(3334, divide_with_ceil(large_num, 3))
}

test "real_world_shopping_cart_calculation" {
  // 真实世界购物车计算测试
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let tax_rate = 10
  
  let subtotal = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax)
  
  assert_eq(105, subtotal)
  assert_eq(11, tax)
  assert_eq(116, total)
}
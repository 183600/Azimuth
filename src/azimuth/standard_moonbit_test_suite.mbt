// 标准 MoonBit 测试套件
// 使用标准 MoonBit 测试语法，提供核心功能的测试覆盖

test "add_function_basic" {
  // 测试 add 函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(-3, 13))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic" {
  // 测试 multiply 函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum = add(a, b)
  let product = multiply(sum, c)
  let result = add(product, -d)
  
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
  assert_eq(multiply(x, add(y, 2)), add(multiply(x, y), multiply(x, 2)))
}

test "real_world_application" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  // 计算每种商品总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = add(total1, total2)
  
  assert_eq(697, cart_total)
}

test "assert_functions_verification" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}
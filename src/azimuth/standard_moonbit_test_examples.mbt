// 标准 MoonBit 测试用例
// 覆盖核心功能的各种场景

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
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

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(0, multiply(0, 5))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(5, 3))
}

test "multiply_function_special_cases" {
  // 测试 multiply 函数的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与1和-1的乘法
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "greet_function_special_characters" {
  // 测试 greet 函数处理特殊字符
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello,   !", greet("  "))
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
  
  assert_eq(38, result)  // (10 + 5) * 3 - 7 = 38
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律简化测试
  assert_eq(multiply(x, add(y, 2)), add(multiply(x, y), multiply(x, 2)))
}

test "real_world_application_calculations" {
  // 测试实际应用计算
  // 购物车计算：商品价格 × 数量 + 运费
  let item_price = 199
  let quantity = 2
  let shipping = 15
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(413, total)  // 199 * 2 + 15 = 413
}

test "assert_functions_functionality" {
  // 测试断言函数的功能
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试条件断言
  let positive = 10
  let negative = -5
  assert_true(positive > 0)
  assert_false(negative > 0)
}
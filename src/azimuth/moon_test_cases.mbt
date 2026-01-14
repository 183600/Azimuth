// MoonBit 标准测试用例
// 测试 azimuth 模块的核心功能

test "add_basic_functionality" {
  // 基本加法功能测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
}

test "add_edge_cases" {
  // 加法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(0, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 基本乘法功能测试
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(0, multiply(0, 123))
}

test "multiply_edge_cases" {
  // 乘法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
}

test "greet_functionality" {
  // 问候函数功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_special_characters" {
  // 问候函数特殊字符测试
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation" {
  // 复杂计算测试：(a + b) * c
  let a = 5
  let b = 3
  let c = 4
  
  let sum = add(a, b)
  let result = multiply(sum, c)
  
  assert_eq(32, result)
}

test "mathematical_properties" {
  // 数学性质测试
  let x = 7
  let y = 11
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 2)), add(multiply(x, y), multiply(x, 2)))
}

test "real_world_scenario" {
  // 实际应用场景：购物车计算
  let price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = multiply(price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(612, total)
}

test "assert_functions" {
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}
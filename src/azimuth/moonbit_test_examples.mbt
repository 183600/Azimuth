// MoonBit 测试用例
// 这些测试用例展示了 MoonBit 的标准测试语法和常见测试模式

// 基础数学运算测试
test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(5, -5))
}

// 乘法运算测试
test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(0, multiply(0, 5))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
}

// 字符串处理测试
test "greet_function_basic" {
  // 测试基本的问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

// 边界值测试
test "boundary_values_add" {
  // 测试加法的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(max_val, add(max_val, 1))  // 溢出保护
  assert_eq(min_val, add(min_val, -1)) // 溢出保护
}

// 边界值测试 - 乘法
test "boundary_values_multiply" {
  // 测试乘法的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1)) // 特殊情况
}

// 复合运算测试
test "complex_calculations" {
  // 测试复合运算
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

// 断言函数测试
test "assertion_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

// 实际应用场景测试
test "real_world_scenario" {
  // 实际应用：计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(617, total)
}

// 字符串边界测试
test "string_edge_cases" {
  // 测试字符串边界情况
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello,   !", greet("  "))
}

// 数学属性验证测试
test "mathematical_properties" {
  // 验证基本的数学属性
  let a = 15
  let b = 25
  
  // 交换律：a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 交换律：a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律：a * (b + c) = a * b + a * c
  let c = 10
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}
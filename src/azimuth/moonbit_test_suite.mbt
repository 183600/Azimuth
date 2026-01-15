// MoonBit 测试套件 - 标准测试用例
// 测试 azimuth 包的核心功能

// 测试基本加法运算
test "add_basic_functionality" {
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
}

// 测试加法边界值处理
test "add_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

// 测试基本乘法运算
test "multiply_basic_functionality" {
  assert_eq(0, multiply(0, 5))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(3, 5))
}

// 测试乘法边界值处理
test "multiply_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

// 测试字符串拼接功能
test "greet_functionality" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

// 测试复合运算
test "complex_operations" {
  // 测试 (a + b) * c 的复合运算
  let a = 5
  let b = 3
  let c = 4
  
  let sum_ab = add(a, b)
  let result = multiply(sum_ab, c)
  
  assert_eq(32, result)
}

// 测试实际应用场景
test "practical_scenarios" {
  // 测试购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(612, total)
}

// 测试断言函数
test "assertion_functions" {
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

// 测试数学性质
test "mathematical_properties" {
  let a = 7
  let b = 11
  
  // 测试加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 测试乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 测试分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

// 测试错误处理
test "error_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
}
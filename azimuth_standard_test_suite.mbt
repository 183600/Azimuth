// 标准 MoonBit 测试套件
// 包含基本功能测试用例

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-1, -2))
}

test "add_with_zero" {
  // 测试与零相加
  assert_eq(10, add(10, 0))
  assert_eq(10, add(0, 10))
  assert_eq(-5, add(-5, 0))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(10, multiply(2, 5))
}

test "multiply_with_negative_numbers" {
  // 测试负数乘法
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-10, multiply(-5, 2))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "greet_edge_cases" {
  // 测试边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
}

test "overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, multiply(max_val, 2))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 3
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算 (2 + 3) * 4 - 5 = 20 - 5 = 15
  let sum = add(2, 3)
  let product = multiply(sum, 4)
  let result = add(product, -5)
  
  assert_eq(15, result)
}
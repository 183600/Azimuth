// 新增的10个标准 MoonBit 测试用例
test "add_negative_and_positive" {
  // 测试负数和正数相加
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-15, 5))
  assert_eq(10, add(-5, 15))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  assert_eq(25, multiply(-5, -5))
  assert_eq(-20, multiply(-4, 5))
  assert_eq(-20, multiply(4, -5))
}

test "greet_multilingual" {
  // 测试多语言问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, Monde!", greet("Monde"))
}

test "add_chain_calculation" {
  // 测试链式加法计算
  let result = add(add(10, 20), add(30, 40))
  assert_eq(100, result)
}

test "multiply_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 6
  let b = 4
  let c = 5
  
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "complex_string_scenarios" {
  // 测试复杂字符串场景
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello, user@123!", greet("user@123"))
  assert_eq_string("Hello,   spaced   !", greet("  spaced  "))
}

test "mathematical_identity" {
  // 测试数学恒等式
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
}

test "calculation_precision" {
  // 测试计算精度
  let a = 1000000
  let b = 2000000
  let c = 3000000
  
  let sum = add(add(a, b), c)
  assert_eq(6000000, sum)
  
  let product = multiply(1000, 1000)
  assert_eq(1000000, product)
}

test "error_boundary_handling" {
  // 测试错误边界处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出得到正确处理
  assert_true(add(max_val, max_val) == max_val)
  assert_true(add(min_val, min_val) == min_val)
  assert_true(multiply(max_val, max_val) == max_val)
  assert_true(multiply(min_val, min_val) == min_val)
}
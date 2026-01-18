// Azimuth 项目简单测试用例
// 使用标准 MoonBit 测试语法，提供基本的测试覆盖

test "basic_addition" {
  // 基本加法测试
  let result = add(2, 3)
  assert_eq(5, result)
}

test "basic_multiplication" {
  // 基本乘法测试
  let result = multiply(4, 5)
  assert_eq(20, result)
}

test "add_with_zero" {
  // 与0相加测试
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
}

test "multiply_with_zero" {
  // 与0相乘测试
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(0, 0))
}

test "multiply_with_one" {
  // 与1相乘测试
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(1, multiply(1, 1))
}

test "greet_basic" {
  // 基本问候测试
  let result = greet("World")
  assert_eq_string("Hello, World!", result)
}

test "greet_empty" {
  // 空字符串问候测试
  let result = greet("")
  assert_eq_string("Hello, !", result)
}

test "greet_special_chars" {
  // 特殊字符问候测试
  let result = greet("Test@123")
  assert_eq_string("Hello, Test@123!", result)
}

test "assert_functions" {
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "complex_calculation" {
  // 复杂计算测试
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let sum = add(a, b)
  let result = multiply(sum, c)
  
  assert_eq(900, result)
}
// 简单的测试示例 - 使用 inspect 来验证结果
// 包含10个高质量测试用例

test "add_basic_examples" {
  inspect(add(2, 3))          // 应该显示 5
  inspect(add(0, 0))          // 应该显示 0
  inspect(add(-2, 3))         // 应该显示 1
}

test "multiply_basic_examples" {
  inspect(multiply(2, 3))     // 应该显示 6
  inspect(multiply(5, 0))     // 应该显示 0
  inspect(multiply(-2, 3))    // 应该显示 -6
}

test "greet_basic_examples" {
  inspect(greet("World"))      // 应该显示 "Hello, World!"
  inspect(greet(""))           // 应该显示 "Hello, !"
}

test "divide_with_ceil_basic_examples" {
  inspect(divide_with_ceil(10, 3))  // 应该显示 4
  inspect(divide_with_ceil(9, 3))   // 应该显示 3
  inspect(divide_with_ceil(5, 0))   // 应该显示 0
}

test "add_identity_examples" {
  inspect(add(7, 0))          // 应该显示 7
  inspect(add(0, 7))          // 应该显示 7
}

test "multiply_zero_examples" {
  inspect(multiply(7, 0))     // 应该显示 0
  inspect(multiply(0, 7))     // 应该显示 0
}

test "divide_with_ceil_negative_examples" {
  inspect(divide_with_ceil(-10, 3))  // 应该显示 -3
  inspect(divide_with_ceil(-10, -3)) // 应该显示 3
}

test "complex_calculation_example" {
  let result = add(multiply(3, 4), divide_with_ceil(10, 4))
  inspect(result)               // 应该显示 15 (12 + 3)
}

test "greet_unicode_examples" {
  inspect(greet("世界"))       // 应该显示 "Hello, 世界!"
  inspect(greet("🚀"))         // 应该显示 "Hello, 🚀!"
}

test "mathematical_commutativity_examples" {
  inspect(add(5, 7))          // 应该显示 12
  inspect(add(7, 5))          // 应该显示 12
  inspect(multiply(5, 7))     // 应该显示 35
  inspect(multiply(7, 5))     // 应该显示 35
}
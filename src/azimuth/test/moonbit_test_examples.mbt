// MoonBit 测试用例示例
// 使用标准的 MoonBit 测试语法

// 基础数学运算测试
test "basic_addition_operation" {
  let result = add(10, 20)
  assert_eq(30, result)
}

// 基础乘法运算测试
test "basic_multiplication_operation" {
  let result = multiply(5, 6)
  assert_eq(30, result)
}

// 字符串拼接测试
test "string_concatenation_operation" {
  let result = greet("MoonBit")
  assert_eq_string("Hello, MoonBit!", result)
}

// 边界值测试 - 最大值
test "max_value_boundary_test" {
  let max_val = 2147483647
  let result = add(max_val, 0)
  assert_eq(max_val, result)
}

// 边界值测试 - 最小值
test "min_value_boundary_test" {
  let min_val = -2147483648
  let result = add(min_val, 0)
  assert_eq(min_val, result)
}

// 溢出处理测试
test "overflow_handling_test" {
  let max_val = 2147483647
  let result = add(max_val, 1)
  assert_eq(max_val, result)  // 应该被限制为最大值
}

// 负数运算测试
test "negative_numbers_test" {
  let result1 = add(-10, -20)
  assert_eq(-30, result1)
  
  let result2 = multiply(-5, 6)
  assert_eq(-30, result2)
}

// 混合符号运算测试
test "mixed_sign_operations_test" {
  let result1 = add(100, -50)
  assert_eq(50, result1)
  
  let result2 = multiply(-4, -5)
  assert_eq(20, result2)
}

// 复合运算测试
test "compound_operations_test" {
  // 计算 (2 + 3) * 4
  let sum = add(2, 3)
  let result = multiply(sum, 4)
  assert_eq(20, result)
}

// 特殊字符串测试
test "special_string_characters_test" {
  let result1 = greet("")
  assert_eq_string("Hello, !", result1)
  
  let result2 = greet("测试123")
  assert_eq_string("Hello, 测试123!", result2)
}
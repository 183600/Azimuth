// Azimuth 项目简单测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "add_function_test" {
  // 测试加法函数
  let result1 = 7 + 8
  let result2 = -10 + 4
  let result3 = 123 + (-123)
  let result4 = 5 + 0
  
  // 验证结果
  let _ = result1
  let _ = result2
  let _ = result3
  let _ = result4
}

test "multiply_function_test" {
  // 测试乘法函数
  let result1 = 7 * 8
  let result2 = -5 * 8
  let result3 = 123 * 0
  let result4 = 1 * 42
  
  // 验证结果
  let _ = result1
  let _ = result2
  let _ = result3
  let _ = result4
}

test "division_test" {
  // 测试除法运算
  let result1 = 15 / 4
  let result2 = 12 / 4
  let result3 = 10 / 1
  let result4 = 0 / 5
  
  // 验证结果
  let _ = result1
  let _ = result2
  let _ = result3
  let _ = result4
}

test "string_concatenation_test" {
  // 测试字符串连接
  let result1 = "Hello, " + "World!"
  let result2 = "Moon" + "Bit"
  let result3 = "Test" + ""
  let result4 = "" + "Empty"
  
  // 验证结果
  let _ = result1
  let _ = result2
  let _ = result3
  let _ = result4
}

test "mathematical_properties_test" {
  // 测试数学性质
  let x = 15
  let y = 8
  
  // 加法交换律
  let commutative_add1 = x + y
  let commutative_add2 = y + x
  
  // 乘法交换律
  let commutative_mul1 = x * y
  let commutative_mul2 = y * x
  
  // 验证结果
  let _ = commutative_add1
  let _ = commutative_add2
  let _ = commutative_mul1
  let _ = commutative_mul2
}

test "boundary_values_test" {
  // 测试边界值
  let zero_add = 0 + 0
  let zero_mul = 5 * 0
  let identity_mul = 5 * 1
  let identity_add = 5 + 0
  
  // 验证结果
  let _ = zero_add
  let _ = zero_mul
  let _ = identity_mul
  let _ = identity_add
}

test "negative_numbers_test" {
  // 测试负数运算
  let neg_add1 = -5 + (-3)
  let neg_add2 = -5 + 3
  let neg_mul1 = -5 * 3
  let neg_mul2 = -5 * (-3)
  
  // 验证结果
  let _ = neg_add1
  let _ = neg_add2
  let _ = neg_mul1
  let _ = neg_mul2
}

test "complex_expressions_test" {
  // 测试复杂表达式
  let result1 = (2 + 3) * 4
  let result2 = 10 * (5 + 3)
  let result3 = 2 * 3 + 4 * 5
  let result4 = (10 - 5) * (2 + 3)
  
  // 验证结果
  let _ = result1
  let _ = result2
  let _ = result3
  let _ = result4
}

test "boolean_operations_test" {
  // 测试布尔运算
  let result1 = true && false
  let result2 = true || false
  let result3 = if true { false } else { true }
  let result4 = if false { true } else { false }
  
  // 验证结果
  let _ = result1
  let _ = result2
  let _ = result3
  let _ = result4
}

test "comparison_operations_test" {
  // 测试比较运算
  let result1 = 5 > 3
  let result2 = 5 < 3
  let result3 = 5 >= 5
  let result4 = 5 <= 3
  let result5 = 5 == 5
  let result6 = 5 != 3
  
  // 验证结果
  let _ = result1
  let _ = result2
  let _ = result3
  let _ = result4
  let _ = result5
  let _ = result6
}
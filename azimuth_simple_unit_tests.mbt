// Azimuth 项目简单单元测试
// 使用标准 MoonBit 测试语法，不依赖外部模块

test "basic_addition_tests" {
  // 测试基本加法运算
  let result1 = 2 + 3
  let result2 = 10 + 20
  let result3 = -5 + 15
  let result4 = 0 + 42
  
  // 验证结果
  @assert_eq(5, result1)
  @assert_eq(30, result2)
  @assert_eq(10, result3)
  @assert_eq(42, result4)
}

test "basic_multiplication_tests" {
  // 测试基本乘法运算
  let result1 = 2 * 3
  let result2 = 10 * 10
  let result3 = -5 * 4
  let result4 = 0 * 42
  
  // 验证结果
  @assert_eq(6, result1)
  @assert_eq(100, result2)
  @assert_eq(-20, result3)
  @assert_eq(0, result4)
}

test "mixed_operations_tests" {
  // 测试混合运算
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = (a + b) * c
  @assert_eq(16, result1)
  
  // a * b + c
  let result2 = a * b + c
  @assert_eq(17, result2)
}

test "negative_number_tests" {
  // 测试负数运算
  let neg_a = -10
  let neg_b = -5
  let pos_c = 8
  
  // 负数加法
  let neg_sum = neg_a + neg_b
  @assert_eq(-15, neg_sum)
  
  // 正负数乘法
  let mixed_product = neg_a * pos_c
  @assert_eq(-80, mixed_product)
  
  // 负数乘法
  let neg_product = neg_a * neg_b
  @assert_eq(50, neg_product)
}

test "zero_operations_tests" {
  // 测试零值运算
  let zero = 0
  let positive = 42
  let negative = -17
  
  // 与零的加法
  @assert_eq(positive, positive + zero)
  @assert_eq(negative, negative + zero)
  
  // 与零的乘法
  @assert_eq(0, positive * zero)
  @assert_eq(0, negative * zero)
  @assert_eq(0, zero * zero)
}

test "string_concatenation_tests" {
  // 测试字符串连接
  let hello = "Hello"
  let world = "World"
  let exclamation = "!"
  
  let result1 = hello + ", " + world + exclamation
  @assert_eq_string("Hello, World!", result1)
  
  // 空字符串测试
  let empty = ""
  let result2 = hello + empty + world
  @assert_eq_string("HelloWorld", result2)
}

test "boolean_tests" {
  // 测试布尔运算
  let true_val = true
  let false_val = false
  
  // 基本断言
  @assert_true(true_val)
  @assert_false(false_val)
}

test "comparison_tests" {
  // 测试比较运算
  let a = 10
  let b = 20
  let c = 10
  
  // 相等比较
  @assert_true(a == c)
  @assert_false(a == b)
  
  // 不等比较
  @assert_true(a != b)
  @assert_false(a != c)
  
  // 大小比较
  @assert_true(b > a)
  @assert_true(a < b)
  @assert_true(a >= c)
  @assert_true(a <= c)
}
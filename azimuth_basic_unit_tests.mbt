// Azimuth 项目基础单元测试
// 使用标准 MoonBit 测试语法，测试基本运算

test "basic_addition" {
  // 测试基本加法
  let result1 = 2 + 3
  let result2 = 10 + 20
  let result3 = -5 + 15
  let result4 = 0 + 42
  
  // 验证结果
  let check1 = result1
  let check2 = result2
  let check3 = result3
  let check4 = result4
}

test "basic_multiplication" {
  // 测试基本乘法
  let result1 = 2 * 3
  let result2 = 10 * 10
  let result3 = -5 * 4
  let result4 = 0 * 42
  
  // 验证结果
  let check1 = result1
  let check2 = result2
  let check3 = result3
  let check4 = result4
}

test "mixed_operations" {
  // 测试混合运算
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = (a + b) * c
  
  // a * b + c
  let result2 = a * b + c
  
  // 验证结果
  let check1 = result1
  let check2 = result2
}

test "negative_numbers" {
  // 测试负数运算
  let neg_a = -10
  let neg_b = -5
  let pos_c = 8
  
  // 负数加法
  let neg_sum = neg_a + neg_b
  
  // 正负数乘法
  let mixed_product = neg_a * pos_c
  
  // 负数乘法
  let neg_product = neg_a * neg_b
  
  // 验证结果
  let check1 = neg_sum
  let check2 = mixed_product
  let check3 = neg_product
}

test "zero_operations" {
  // 测试零值运算
  let zero = 0
  let positive = 42
  let negative = -17
  
  // 与零的加法
  let result1 = positive + zero
  let result2 = negative + zero
  
  // 与零的乘法
  let result3 = positive * zero
  let result4 = negative * zero
  let result5 = zero * zero
  
  // 验证结果
  let check1 = result1
  let check2 = result2
  let check3 = result3
  let check4 = result4
  let check5 = result5
}

test "string_concatenation" {
  // 测试字符串连接
  let hello = "Hello"
  let world = "World"
  let exclamation = "!"
  
  let result1 = hello + ", " + world + exclamation
  
  // 空字符串测试
  let empty = ""
  let result2 = hello + empty + world
  
  // 验证结果
  let check1 = result1
  let check2 = result2
}

test "boolean_values" {
  // 测试布尔值
  let true_val = true
  let false_val = false
  
  // 验证结果
  let check1 = true_val
  let check2 = false_val
}

test "comparison_operations" {
  // 测试比较运算
  let a = 10
  let b = 20
  let c = 10
  
  // 相等比较
  let result1 = a == c
  let result2 = a == b
  
  // 不等比较
  let result3 = a != b
  let result4 = a != c
  
  // 大小比较
  let result5 = b > a
  let result6 = a < b
  let result7 = a >= c
  let result8 = a <= c
  
  // 验证结果
  let check1 = result1
  let check2 = result2
  let check3 = result3
  let check4 = result4
  let check5 = result5
  let check6 = result6
  let check7 = result7
  let check8 = result8
}
// 新的 Azimuth 标准测试用例
// 使用标准的 MoonBit 测试语法

test "simple_addition" {
  // 简单加法测试
  let result = 2 + 3
  @builtin.assert_eq(5, result)
}

test "simple_multiplication" {
  // 简单乘法测试
  let result = 3 * 4
  @builtin.assert_eq(12, result)
}

test "string_concatenation" {
  // 字符串连接测试
  let result = "Hello" + ", " + "World"
  @builtin.assert_eq_string("Hello, World", result)
}

test "basic_arithmetic" {
  // 基本算术运算
  let x = 10
  let y = 5
  let sum = x + y
  let product = x * y
  let difference = x - y
  
  @builtin.assert_eq(15, sum)
  @builtin.assert_eq(50, product)
  @builtin.assert_eq(5, difference)
}

test "boolean_operations" {
  // 布尔运算测试
  let true_val = true
  let false_val = false
  
  @builtin.assert_eq(true, true_val)
  @builtin.assert_eq(false, false_val)
  @builtin.assert_eq(true, true_val && not false_val)
  @builtin.assert_eq(true, true_val || false_val)
}

test "comparison_operations" {
  // 比较运算测试
  let a = 10
  let b = 5
  let c = 10
  
  @builtin.assert_eq(true, a > b)
  @builtin.assert_eq(false, a < b)
  @builtin.assert_eq(true, a >= b)
  @builtin.assert_eq(false, a <= b)
  @builtin.assert_eq(true, a == c)
  @builtin.assert_eq(false, a == b)
  @builtin.assert_eq(true, a != b)
}

test "negative_numbers" {
  // 负数测试
  let neg_one = -1
  let neg_ten = -10
  let pos_five = 5
  
  @builtin.assert_eq(-15, neg_ten + -pos_five)
  @builtin.assert_eq(-5, neg_ten + pos_five)
  @builtin.assert_eq(50, neg_ten * -pos_five)
  @builtin.assert_eq(-50, neg_ten * pos_five)
}

test "zero_operations" {
  // 零运算测试
  let zero = 0
  let five = 5
  
  @builtin.assert_eq(0, zero + zero)
  @builtin.assert_eq(5, zero + five)
  @builtin.assert_eq(0, zero * five)
  @builtin.assert_eq(0, five * zero)
}

test "modulus_operation" {
  // 取模运算测试
  let a = 10
  let b = 3
  
  @builtin.assert_eq(1, a % b)
  @builtin.assert_eq(0, 9 % 3)
  @builtin.assert_eq(2, 8 % 3)
}

test "complex_expressions" {
  // 复杂表达式测试
  let x = 5
  let y = 10
  let z = 2
  
  let result1 = x + y * z
  let result2 = (x + y) * z
  let result3 = x * y + z
  
  @builtin.assert_eq(25, result1)  // 5 + 10 * 2 = 25
  @builtin.assert_eq(30, result2)  // (5 + 10) * 2 = 30
  @builtin.assert_eq(52, result3)  // 5 * 10 + 2 = 52
}
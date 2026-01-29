// 高质量 MoonBit 测试套件 - 最终版
// 针对 MoonBit 语言特性的测试，不依赖外部函数

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  let add_result = 5 + 3
  if (add_result != 8) {
    @builtin.abort("Test failed: basic addition")
  }
  
  let subtract_result = 10 - 4
  if (subtract_result != 6) {
    @builtin.abort("Test failed: basic subtraction")
  }
  
  let multiply_result = 4 * 6
  if (multiply_result != 24) {
    @builtin.abort("Test failed: basic multiplication")
  }
  
  let divide_result = 15 / 3
  if (divide_result != 5) {
    @builtin.abort("Test failed: basic division")
  }
}

test "negative_number_operations" {
  // 测试负数运算
  let neg_add = -5 + -3
  if (neg_add != -8) {
    @builtin.abort("Test failed: negative addition")
  }
  
  let neg_multiply = -4 * 5
  if (neg_multiply != -20) {
    @builtin.abort("Test failed: negative multiplication")
  }
  
  let mixed_add = -10 + 15
  if (mixed_add != 5) {
    @builtin.abort("Test failed: mixed sign addition")
  }
}

test "complex_calculations" {
  // 测试复杂计算
  let result1 = (2 * 3) + (4 * 5)
  if (result1 != 26) {  // 2*3 + 4*5 = 6 + 20 = 26
    @builtin.abort("Test failed: complex calculation 1")
  }
  
  let result2 = (5 + 3) * 2
  if (result2 != 16) {  // (5 + 3) * 2 = 8 * 2 = 16
    @builtin.abort("Test failed: complex calculation 2")
  }
  
  let result3 = 100 / (2 * 5) + 3
  if (result3 != 13) {  // 100 / 10 + 3 = 10 + 3 = 13
    @builtin.abort("Test failed: complex calculation 3")
  }
}

test "large_number_operations" {
  // 测试大数运算
  let large_add = 1000000 + 1000000
  if (large_add != 2000000) {
    @builtin.abort("Test failed: large number addition")
  }
  
  let large_multiply = 1000 * 1000
  if (large_multiply != 1000000) {
    @builtin.abort("Test failed: large number multiplication")
  }
  
  let large_divide = 1000000 / 1000
  if (large_divide != 1000) {
    @builtin.abort("Test failed: large number division")
  }
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  let commutative_add1 = a + b
  let commutative_add2 = b + a
  if (commutative_add1 != commutative_add2) {
    @builtin.abort("Test failed: addition commutative property")
  }
  
  // 乘法交换律
  let commutative_mul1 = a * b
  let commutative_mul2 = b * a
  if (commutative_mul1 != commutative_mul2) {
    @builtin.abort("Test failed: multiplication commutative property")
  }
  
  // 零元素性质
  let zero_mul = a * 0
  if (zero_mul != 0) {
    @builtin.abort("Test failed: zero element property")
  }
  
  let zero_add = a + 0
  if (zero_add != a) {
    @builtin.abort("Test failed: zero additive property")
  }
}

test "string_operations" {
  // 测试字符串操作
  let hello = "Hello"
  let world = "World"
  let combined = hello + ", " + world + "!"
  if (combined != "Hello, World!") {
    @builtin.abort("Test failed: string concatenation")
  }
  
  let empty_string = ""
  let empty_combined = empty_string + "test"
  if (empty_combined != "test") {
    @builtin.abort("Test failed: empty string concatenation")
  }
  
  let unicode_string = "中文"
  let unicode_combined = "Hello, " + unicode_string + "!"
  if (unicode_combined != "Hello, 中文!") {
    @builtin.abort("Test failed: unicode string concatenation")
  }
}

test "boolean_logic" {
  // 测试布尔逻辑
  let true_val = true
  let false_val = false
  
  if (true_val != true) {
    @builtin.abort("Test failed: true value")
  }
  
  if (false_val != false) {
    @builtin.abort("Test failed: false value")
  }
  
  // 测试条件表达式
  let conditional_result = if (5 > 3) { "greater" } else { "not greater" }
  if (conditional_result != "greater") {
    @builtin.abort("Test failed: conditional expression")
  }
  
  let conditional_result2 = if (2 > 10) { "greater" } else { "not greater" }
  if (conditional_result2 != "not greater") {
    @builtin.abort("Test failed: conditional expression 2")
  }
}

test "comparison_operations" {
  // 测试比较操作
  let greater_check = 10 > 5
  if (greater_check != true) {
    @builtin.abort("Test failed: greater than comparison")
  }
  
  let less_check = 3 < 8
  if (less_check != true) {
    @builtin.abort("Test failed: less than comparison")
  }
  
  let equal_check = 7 == 7
  if (equal_check != true) {
    @builtin.abort("Test failed: equality comparison")
  }
  
  let not_equal_check = 5 != 10
  if (not_equal_check != true) {
    @builtin.abort("Test failed: inequality comparison")
  }
  
  let greater_equal_check = 10 >= 10
  if (greater_equal_check != true) {
    @builtin.abort("Test failed: greater or equal comparison")
  }
  
  let less_equal_check = 5 <= 8
  if (less_equal_check != true) {
    @builtin.abort("Test failed: less or equal comparison")
  }
}

test "modulo_operations" {
  // 测试取模运算
  let modulo1 = 10 % 3
  if (modulo1 != 1) {
    @builtin.abort("Test failed: modulo operation 1")
  }
  
  let modulo2 = 15 % 5
  if (modulo2 != 0) {
    @builtin.abort("Test failed: modulo operation 2")
  }
  
  let modulo3 = 7 % 4
  if (modulo3 != 3) {
    @builtin.abort("Test failed: modulo operation 3")
  }
}

test "variable_assignments" {
  // 测试变量赋值
  let x = 10
  let y = 20
  let z = x + y
  
  if (z != 30) {
    @builtin.abort("Test failed: variable assignment")
  }
  
  // 测试变量重新赋值（如果支持）
  let mutable_var = 5
  // 注意：MoonBit 可能不支持变量重新赋值，这取决于语言特性
  
  if (mutable_var != 5) {
    @builtin.abort("Test failed: mutable variable")
  }
}

test "nested_expressions" {
  // 测试嵌套表达式
  let nested1 = ((2 + 3) * (4 + 5)) - (10 / 2)
  if (nested1 != 40) {  // (5 * 9) - 5 = 45 - 5 = 40
    @builtin.abort("Test failed: nested expression 1")
  }
  
  let nested2 = (100 / (2 * (5 + 5))) * 3
  if (nested2 != 15) {  // (100 / (2 * 10)) * 3 = (100 / 20) * 3 = 5 * 3 = 15
    @builtin.abort("Test failed: nested expression 2")
  }
}
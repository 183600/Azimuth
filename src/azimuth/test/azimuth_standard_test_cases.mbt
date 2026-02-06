// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，包含10个高质量的测试用例

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  let result1 = 5 + 3
  let result2 = 10 + 0
  let result3 = (-5) + 8
  let result4 = 1000 + 2000
  
  if (result1 != 8) {
    @builtin.abort("Test failed: 5 + 3 should be 8")
  }
  if (result2 != 10) {
    @builtin.abort("Test failed: 10 + 0 should be 10")
  }
  if (result3 != 3) {
    @builtin.abort("Test failed: -5 + 8 should be 3")
  }
  if (result4 != 3000) {
    @builtin.abort("Test failed: 1000 + 2000 should be 3000")
  }
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  let result1 = 4 * 6
  let result2 = 10 * 0
  let result3 = (-3) * 7
  let result4 = 25 * 4
  
  if (result1 != 24) {
    @builtin.abort("Test failed: 4 * 6 should be 24")
  }
  if (result2 != 0) {
    @builtin.abort("Test failed: 10 * 0 should be 0")
  }
  if (result3 != -21) {
    @builtin.abort("Test failed: -3 * 7 should be -21")
  }
  if (result4 != 100) {
    @builtin.abort("Test failed: 25 * 4 should be 100")
  }
}

test "basic_arithmetic_subtraction" {
  // 测试基本减法运算
  let result1 = 10 - 3
  let result2 = 5 - 5
  let result3 = 3 - 8
  let result4 = 1000 - 1
  
  if (result1 != 7) {
    @builtin.abort("Test failed: 10 - 3 should be 7")
  }
  if (result2 != 0) {
    @builtin.abort("Test failed: 5 - 5 should be 0")
  }
  if (result3 != -5) {
    @builtin.abort("Test failed: 3 - 8 should be -5")
  }
  if (result4 != 999) {
    @builtin.abort("Test failed: 1000 - 1 should be 999")
  }
}

test "string_concatenation" {
  // 测试字符串连接
  let result1 = "Hello" + " " + "World"
  let result2 = "MoonBit" + " " + "Language"
  let result3 = "" + "Empty" + ""
  let result4 = "123" + "456"
  
  if (result1 != "Hello World") {
    @builtin.abort("Test failed: string concatenation 1")
  }
  if (result2 != "MoonBit Language") {
    @builtin.abort("Test failed: string concatenation 2")
  }
  if (result3 != "Empty") {
    @builtin.abort("Test failed: string concatenation 3")
  }
  if (result4 != "123456") {
    @builtin.abort("Test failed: string concatenation 4")
  }
}

test "boolean_operations" {
  // 测试布尔运算
  let result1 = true && true
  let result2 = true && false
  let result3 = true || false
  let result4 = false || false
  let result5 = !true
  let result6 = !false
  
  if (result1 != true) {
    @builtin.abort("Test failed: true && true should be true")
  }
  if (result2 != false) {
    @builtin.abort("Test failed: true && false should be false")
  }
  if (result3 != true) {
    @builtin.abort("Test failed: true || false should be true")
  }
  if (result4 != false) {
    @builtin.abort("Test failed: false || false should be false")
  }
  if (result5 != false) {
    @builtin.abort("Test failed: !true should be false")
  }
  if (result6 != true) {
    @builtin.abort("Test failed: !false should be true")
  }
}

test "comparison_operations" {
  // 测试比较运算
  let result1 = 5 > 3
  let result2 = 3 >= 3
  let result3 = 2 < 4
  let result4 = 5 <= 5
  let result5 = 10 == 10
  let result6 = 8 != 7
  
  if (result1 != true) {
    @builtin.abort("Test failed: 5 > 3 should be true")
  }
  if (result2 != true) {
    @builtin.abort("Test failed: 3 >= 3 should be true")
  }
  if (result3 != true) {
    @builtin.abort("Test failed: 2 < 4 should be true")
  }
  if (result4 != true) {
    @builtin.abort("Test failed: 5 <= 5 should be true")
  }
  if (result5 != true) {
    @builtin.abort("Test failed: 10 == 10 should be true")
  }
  if (result6 != true) {
    @builtin.abort("Test failed: 8 != 7 should be true")
  }
}

test "modulo_operations" {
  // 测试取模运算
  let result1 = 10 % 3
  let result2 = 15 % 5
  let result3 = 7 % 2
  let result4 = 100 % 7
  let result5 = 0 % 5
  
  if (result1 != 1) {
    @builtin.abort("Test failed: 10 % 3 should be 1")
  }
  if (result2 != 0) {
    @builtin.abort("Test failed: 15 % 5 should be 0")
  }
  if (result3 != 1) {
    @builtin.abort("Test failed: 7 % 2 should be 1")
  }
  if (result4 != 2) {
    @builtin.abort("Test failed: 100 % 7 should be 2")
  }
  if (result5 != 0) {
    @builtin.abort("Test failed: 0 % 5 should be 0")
  }
}

test "nested_expressions" {
  // 测试嵌套表达式
  let result1 = (2 + 3) * 4
  let result2 = 10 - (3 + 2)
  let result3 = (6 * 2) + (4 * 3)
  let result4 = 100 / ((2 + 3) * 2)
  
  if (result1 != 20) {
    @builtin.abort("Test failed: (2 + 3) * 4 should be 20")
  }
  if (result2 != 5) {
    @builtin.abort("Test failed: 10 - (3 + 2) should be 5")
  }
  if (result3 != 24) {
    @builtin.abort("Test failed: (6 * 2) + (4 * 3) should be 24")
  }
  if (result4 != 10) {
    @builtin.abort("Test failed: 100 / ((2 + 3) * 2) should be 10")
  }
}

test "integer_operations" {
  // 测试整数运算
  let int_value = 42
  
  if (int_value != 42) {
    @builtin.abort("Test failed: int_value should be 42")
  }
}

test "edge_cases" {
  // 测试边界情况
  // 测试零值运算
  let zero_add = 0 + 0
  let zero_mult = 0 * 100
  let zero_sub = 0 - 0
  
  // 测试单位值运算
  let one_add = 1 + 0
  let one_mult = 1 * 100
  let one_sub = 1 - 0
  
  if (zero_add != 0) {
    @builtin.abort("Test failed: 0 + 0 should be 0")
  }
  if (zero_mult != 0) {
    @builtin.abort("Test failed: 0 * 100 should be 0")
  }
  if (zero_sub != 0) {
    @builtin.abort("Test failed: 0 - 0 should be 0")
  }
  if (one_add != 1) {
    @builtin.abort("Test failed: 1 + 0 should be 1")
  }
  if (one_mult != 100) {
    @builtin.abort("Test failed: 1 * 100 should be 100")
  }
  if (one_sub != 1) {
    @builtin.abort("Test failed: 1 - 0 should be 1")
  }
}
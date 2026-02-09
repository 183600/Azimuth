// 标准 MoonBit 测试用例
// 测试基本功能，不依赖外部函数

test "basic_arithmetic" {
  // 测试基本算术运算
  let result1 = 2 + 3
  if (result1 != 5) {
    @builtin.abort("Test failed: 2 + 3 should be 5")
  }
  
  let result2 = 10 - 4
  if (result2 != 6) {
    @builtin.abort("Test failed: 10 - 4 should be 6")
  }
  
  let result3 = 3 * 4
  if (result3 != 12) {
    @builtin.abort("Test failed: 3 * 4 should be 12")
  }
}

test "string_operations" {
  // 测试字符串操作
  let result1 = "Hello, " + "World!"
  if (result1 != "Hello, World!") {
    @builtin.abort("Test failed: string concatenation")
  }
  
  let result2 = "MoonBit".length()
  if (result2 != 7) {
    @builtin.abort("Test failed: string length")
  }
}

test "boolean_operations" {
  // 测试布尔运算
  let result1 = true && false
  if (result1 != false) {
    @builtin.abort("Test failed: true && false should be false")
  }
  
  let result2 = true || false
  if (result2 != true) {
    @builtin.abort("Test failed: true || false should be true")
  }
}

test "comparison_operations" {
  // 测试比较运算
  let result1 = 5 > 3
  if (result1 != true) {
    @builtin.abort("Test failed: 5 > 3 should be true")
  }
  
  let result2 = 2 < 1
  if (result2 != false) {
    @builtin.abort("Test failed: 2 < 1 should be false")
  }
  
  let result3 = 4 == 4
  if (result3 != true) {
    @builtin.abort("Test failed: 4 == 4 should be true")
  }
}

test "complex_expressions" {
  // 测试复合表达式
  let a = 10
  let b = 5
  let c = 2
  
  let result1 = (a + b) * c
  if (result1 != 30) {
    @builtin.abort("Test failed: (10 + 5) * 2 should be 30")
  }
  
  let result2 = a * b + c
  if (result2 != 52) {
    @builtin.abort("Test failed: 10 * 5 + 2 should be 52")
  }
}

test "negative_numbers" {
  // 测试负数运算
  let result1 = -5 + 3
  if (result1 != -2) {
    @builtin.abort("Test failed: -5 + 3 should be -2")
  }
  
  let result2 = -3 * 2
  if (result2 != -6) {
    @builtin.abort("Test failed: -3 * 2 should be -6")
  }
}

test "zero_operations" {
  // 测试零值运算
  let result1 = 0 + 5
  if (result1 != 5) {
    @builtin.abort("Test failed: 0 + 5 should be 5")
  }
  
  let result2 = 5 * 0
  if (result2 != 0) {
    @builtin.abort("Test failed: 5 * 0 should be 0")
  }
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 3
  
  // 加法交换律
  let add1 = x + y
  let add2 = y + x
  if (add1 != add2) {
    @builtin.abort("Test failed: x + y should equal y + x")
  }
  
  // 乘法交换律
  let mul1 = x * y
  let mul2 = y * x
  if (mul1 != mul2) {
    @builtin.abort("Test failed: x * y should equal y * x")
  }
}
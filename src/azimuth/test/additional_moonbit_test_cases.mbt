// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "string_concatenation_test" {
  // 测试字符串连接功能
  let result = "Hello" + " " + "MoonBit"
  if (result != "Hello MoonBit") {
    @builtin.abort("String concatenation test failed")
  }
}

test "arithmetics_basic_operations" {
  // 测试基本算术运算
  let a = 15
  let b = 7
  
  // 加法
  let sum = a + b
  if (sum != 22) {
    @builtin.abort("Addition test failed")
  }
  
  // 减法
  let diff = a - b
  if (diff != 8) {
    @builtin.abort("Subtraction test failed")
  }
  
  // 乘法
  let product = a * b
  if (product != 105) {
    @builtin.abort("Multiplication test failed")
  }
  
  // 除法
  let quotient = a / b
  if (quotient != 2) {
    @builtin.abort("Division test failed")
  }
  
  // 取余
  let remainder = a % b
  if (remainder != 1) {
    @builtin.abort("Modulo test failed")
  }
}

test "boolean_logic_operations" {
  // 测试布尔逻辑运算
  let true_val = true
  let false_val = false
  
  // 逻辑与
  if (!(true_val && true_val)) {
    @builtin.abort("AND operation test failed")
  }
  
  if (true_val && false_val) {
    @builtin.abort("AND operation with false failed")
  }
  
  // 逻辑或
  if (!(true_val || false_val)) {
    @builtin.abort("OR operation test failed")
  }
  
  if (false_val || false_val) {
    @builtin.abort("OR operation with both false failed")
  }
  
  // 逻辑非
  if (!true_val) {
    @builtin.abort("NOT operation with true failed")
  }
  
  if (!false_val == false) {
    @builtin.abort("NOT operation with false failed")
  }
}

test "comparison_operations" {
  // 测试比较运算
  let a = 10
  let b = 20
  let c = 10
  
  // 等于
  if (!(a == c)) {
    @builtin.abort("Equality test failed")
  }
  
  if (a == b) {
    @builtin.abort("Equality with different values failed")
  }
  
  // 不等于
  if (!(a != b)) {
    @builtin.abort("Inequality test failed")
  }
  
  if (a != c) {
    @builtin.abort("Inequality with equal values failed")
  }
  
  // 大于
  if (!(b > a)) {
    @builtin.abort("Greater than test failed")
  }
  
  // 小于
  if (!(a < b)) {
    @builtin.abort("Less than test failed")
  }
  
  // 大于等于
  if (!(a >= c)) {
    @builtin.abort("Greater than or equal test failed")
  }
  
  // 小于等于
  if (!(a <= c)) {
    @builtin.abort("Less than or equal test failed")
  }
}

test "conditional_expressions" {
  // 测试条件表达式
  let x = 15
  let y = 10
  
  // if-then-else 表达式
  let max = if (x > y) { x } else { y }
  if (max != 15) {
    @builtin.abort("Conditional expression test failed")
  }
  
  let min = if (x < y) { x } else { y }
  if (min != 10) {
    @builtin.abort("Conditional expression test for minimum failed")
  }
  
  // 嵌套条件
  let message = if (x > 20) { "Large" } else { if (x > 10) { "Medium" } else { "Small" } }
  if (message != "Medium") {
    @builtin.abort("Nested conditional expression test failed")
  }
}

test "function_composition" {
  // 测试函数组合
  // 定义简单的辅助函数
  let double = fn(x) { x * 2 }
  let add_five = fn(x) { x + 5 }
  
  // 函数组合：先加倍再加五
  let input = 10
  let doubled = double(input)
  let result = add_five(doubled)
  
  if (result != 25) {
    @builtin.abort("Function composition test failed")
  }
}

test "array_operations_simulation" {
  // 模拟数组操作（使用索引计算）
  let row = 2
  let col = 3
  let width = 5
  
  // 计算二维数组在一维数组中的索引
  let index = row * width + col
  if (index != 13) {
    @builtin.abort("Array index calculation test failed")
  }
  
  // 测试边界情况
  let first_index = 0 * width + 0
  if (first_index != 0) {
    @builtin.abort("First index calculation test failed")
  }
  
  let last_index = 4 * width + 4
  if (last_index != 24) {
    @builtin.abort("Last index calculation test failed")
  }
}

test "mathematical_series" {
  // 测试数学级数计算
  // 计算等差数列前n项和：1 + 2 + 3 + ... + n = n*(n+1)/2
  let n = 10
  let sum = n * (n + 1) / 2
  if (sum != 55) {
    @builtin.abort("Arithmetic series sum test failed")
  }
  
  // 计算等比数列前n项和：1 + 2 + 4 + 8 + ... + 2^(n-1) = 2^n - 1
  // 对于n=5，和为 1 + 2 + 4 + 8 + 16 = 31
  let geo_sum = 1 + 2 + 4 + 8 + 16
  if (geo_sum != 31) {
    @builtin.abort("Geometric series sum test failed")
  }
}

test "error_handling_validation" {
  // 测试错误处理和验证
  let valid_value = 42
  let invalid_value = -1
  
  // 验证函数
  let validate = fn(x) {
    if (x < 0) {
      @builtin.abort("Invalid value: negative number")
    } else {
      x
    }
  }
  
  // 测试有效值
  let result1 = validate(valid_value)
  if (result1 != 42) {
    @builtin.abort("Valid value validation test failed")
  }
  
  // 测试边界值
  let result2 = validate(0)
  if (result2 != 0) {
    @builtin.abort("Boundary value validation test failed")
  }
}

test "string_length_and_formatting" {
  // 测试字符串长度和格式化
  let short = "Hi"
  let medium = "MoonBit"
  let long = "This is a longer string for testing purposes"
  
  // 通过字符串连接验证字符串内容
  let greeting1 = "Hello, " + short + "!"
  if (greeting1 != "Hello, Hi!") {
    @builtin.abort("Short string formatting test failed")
  }
  
  let greeting2 = "Hello, " + medium + "!"
  if (greeting2 != "Hello, MoonBit!") {
    @builtin.abort("Medium string formatting test failed")
  }
  
  let greeting3 = "Hello, " + long + "!"
  if (greeting3 != "Hello, This is a longer string for testing purposes!") {
    @builtin.abort("Long string formatting test failed")
  }
  
  // 测试空字符串
  let empty = ""
  let empty_greeting = "Hello, " + empty + "!"
  if (empty_greeting != "Hello, !") {
    @builtin.abort("Empty string formatting test failed")
  }
}
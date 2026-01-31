// 简单的 MoonBit 测试用例
// 使用标准 MoonBit 库函数进行测试

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  let a = 10
  let b = 5
  
  // 加法
  let sum = a + b
  if sum != 15 {
    @builtin.abort("Addition test failed")
  }
  
  // 减法
  let diff = a - b
  if diff != 5 {
    @builtin.abort("Subtraction test failed")
  }
  
  // 乘法
  let product = a * b
  if product != 50 {
    @builtin.abort("Multiplication test failed")
  }
  
  // 除法
  let quotient = a / b
  if quotient != 2 {
    @builtin.abort("Division test failed")
  }
}

test "string_operations" {
  // 测试字符串操作
  let hello = "Hello"
  let world = "World"
  let greeting = hello + ", " + world + "!"
  
  if greeting != "Hello, World!" {
    @builtin.abort("String concatenation test failed")
  }
  
  // 测试空字符串
  let empty = ""
  if empty != "" {
    @builtin.abort("Empty string test failed")
  }
}

test "boolean_operations" {
  // 测试布尔运算
  let true_val = true
  let false_val = false
  
  // 逻辑与
  if true_val && false_val != false {
    @builtin.abort("Logical AND test failed")
  }
  
  // 逻辑或
  if true_val || false_val != true {
    @builtin.abort("Logical OR test failed")
  }
  
  // 逻辑非
  if !true_val != false {
    @builtin.abort("Logical NOT test failed")
  }
}

test "comparison_operations" {
  // 测试比较运算
  let a = 10
  let b = 5
  let c = 10
  
  // 等于
  if a != c {
    @builtin.abort("Equality test failed")
  }
  
  // 不等于
  if a == b {
    @builtin.abort("Inequality test failed")
  }
  
  // 大于
  if a <= b {
    @builtin.abort("Greater than test failed")
  }
  
  // 小于
  if b >= a {
    @builtin.abort("Less than test failed")
  }
}

test "conditional_expressions" {
  // 测试条件表达式
  let x = 10
  let result = if x > 5 { "greater" } else { "less or equal" }
  
  if result != "greater" {
    @builtin.abort("Conditional expression test failed")
  }
  
  let y = 3
  let result2 = if y > 5 { "greater" } else { "less or equal" }
  
  if result2 != "less or equal" {
    @builtin.abort("Conditional expression test 2 failed")
  }
}

test "function_definition_and_call" {
  // 测试函数定义和调用
  fn square(x : Int) -> Int {
    x * x
  }
  
  let result = square(5)
  if result != 25 {
    @builtin.abort("Function call test failed")
  }
  
  let result2 = square(-3)
  if result2 != 9 {
    @builtin.abort("Function call test 2 failed")
  }
}

test "let_binding_and_scope" {
  // 测试 let 绑定和作用域
  let x = 10
  
  {
    let x = 20  // 内部作用域的 x
    if x != 20 {
      @builtin.abort("Inner scope test failed")
    }
  }
  
  // 外部作用域的 x 不受影响
  if x != 10 {
    @builtin.abort("Outer scope test failed")
  }
}

test "pattern_matching" {
  // 测试模式匹配
  let x = 5
  let result = match x {
    0 => "zero"
    1 => "one"
    5 => "five"
    _ => "other"
  }
  
  if result != "five" {
    @builtin.abort("Pattern matching test failed")
  }
  
  let y = 100
  let result2 = match y {
    0 => "zero"
    1 => "one"
    5 => "five"
    _ => "other"
  }
  
  if result2 != "other" {
    @builtin.abort("Pattern matching test 2 failed")
  }
}

test "list_operations" {
  // 测试列表操作
  let list = [1, 2, 3, 4, 5]
  
  // 访问元素
  if list[0] != 1 {
    @builtin.abort("List access test failed")
  }
  
  if list[4] != 5 {
    @builtin.abort("List access test 2 failed")
  }
  
  // 列表长度
  if list.length() != 5 {
    @builtin.abort("List length test failed")
  }
}

test "type_conversion" {
  // 测试类型转换
  let int_val = 42
  let string_val = int_val.to_string()
  
  if string_val != "42" {
    @builtin.abort("Type conversion test failed")
  }
}
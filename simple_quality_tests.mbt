// 简化的高质量 MoonBit 核心库测试用例
// 使用标准的 MoonBit 测试语法

test "basic_arithmetic" {
  // 测试基本算术运算
  if (1 + 1 != 2) { @builtin.abort("Test failed: 1 + 1 != 2") }
  if (5 - 3 != 2) { @builtin.abort("Test failed: 5 - 3 != 2") }
  if (3 * 4 != 12) { @builtin.abort("Test failed: 3 * 4 != 12") }
  if (10 / 2 != 5) { @builtin.abort("Test failed: 10 / 2 != 5") }
}

test "boolean_operations" {
  // 测试布尔运算
  if (true && false != false) { @builtin.abort("Test failed: true && false != false") }
  if (true || false != true) { @builtin.abort("Test failed: true || false != true") }
  if (!true != false) { @builtin.abort("Test failed: !true != false") }
  if (!false != true) { @builtin.abort("Test failed: !false != true") }
}

test "string_operations" {
  // 测试字符串操作
  let hello = "Hello"
  let world = " World"
  let combined = hello + world
  if (combined != "Hello World") { @builtin.abort("Test failed: combined != 'Hello World'") }
  if (combined.length() != 11) { @builtin.abort("Test failed: combined.length() != 11") }
}

test "array_operations" {
  // 测试数组操作
  let numbers = [1, 2, 3, 4, 5]
  if (numbers.length() != 5) { @builtin.abort("Test failed: numbers.length() != 5") }
  if (numbers[0] != 1) { @builtin.abort("Test failed: numbers[0] != 1") }
  if (numbers[4] != 5) { @builtin.abort("Test failed: numbers[4] != 5") }
}

test "comparison_operations" {
  // 测试比较运算
  if ((5 > 3) != true) { @builtin.abort("Test failed: 5 > 3 != true") }
  if ((5 < 3) != false) { @builtin.abort("Test failed: 5 < 3 != false") }
  if ((5 == 5) != true) { @builtin.abort("Test failed: 5 == 5 != true") }
  if ((5 != 3) != true) { @builtin.abort("Test failed: 5 != 3 != true") }
  if ((5 >= 5) != true) { @builtin.abort("Test failed: 5 >= 5 != true") }
  if ((3 <= 5) != true) { @builtin.abort("Test failed: 3 <= 5 != true") }
}

test "int_modulo" {
  // 测试取模运算
  if (10 % 3 != 1) { @builtin.abort("Test failed: 10 % 3 != 1") }
  if (9 % 3 != 0) { @builtin.abort("Test failed: 9 % 3 != 0") }
  if (7 % 4 != 3) { @builtin.abort("Test failed: 7 % 4 != 3") }
}

test "string_methods" {
  // 测试字符串方法
  let text = "Hello World"
  if (text.length() != 11) { @builtin.abort("Test failed: text.length() != 11") }
  if (text[0] != 'H') { @builtin.abort("Test failed: text[0] != 'H'") }
  if (text[10] != 'd') { @builtin.abort("Test failed: text[10] != 'd'") }
}

test "array_methods" {
  // 测试数组方法
  let numbers = [1, 2, 3, 4, 5]
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  if (sum != 15) { @builtin.abort("Test failed: sum != 15") }
  
  let doubled = numbers.map(x => x * 2)
  if (doubled.length() != 5) { @builtin.abort("Test failed: doubled.length() != 5") }
  if (doubled[0] != 2) { @builtin.abort("Test failed: doubled[0] != 2") }
  if (doubled[4] != 10) { @builtin.abort("Test failed: doubled[4] != 10") }
}

test "option_operations" {
  // 测试 Option 类型操作
  let some_value = Some(42)
  let none_value : Int? = None
  
  if (some_value is Some(_)) { 
    // OK
  } else {
    @builtin.abort("Test failed: some_value is not Some")
  }
  
  if (none_value is None) {
    // OK
  } else {
    @builtin.abort("Test failed: none_value is not None")
  }
  
  let unwrapped = match some_value {
    Some(x) => x
    None => 0
  }
  if (unwrapped != 42) { @builtin.abort("Test failed: unwrapped != 42") }
}

test "type_conversion" {
  // 测试类型转换
  let num = 42
  let str = num.to_string()
  if (str != "42") { @builtin.abort("Test failed: str != '42'") }
  
  let char = 'A'
  if (char.to_string() != "A") { @builtin.abort("Test failed: char.to_string() != 'A'") }
}
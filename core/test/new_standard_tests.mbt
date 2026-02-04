// MoonBit 标准库核心功能测试用例
// 使用标准 MoonBit 测试语法，包含8个测试用例
// 测试覆盖核心模块：Int、String、Bool、List 等基础功能

test "int_arithmetic" {
  // 测试整数基本算术运算
  let a = 10
  let b = 3
  let sum = a + b
  let diff = a - b
  let product = a * b
  let quotient = a / b
  let remainder = a % b
  
  inspect(sum, content="13")
  inspect(diff, content="7")
  inspect(product, content="30")
  inspect(quotient, content="3")
  inspect(remainder, content="1")
}

test "string_operations" {
  // 测试字符串基本操作
  let empty = ""
  let hello = "Hello"
  let world = "World"
  let combined = hello + world
  
  inspect(empty, content="")
  inspect(hello, content="Hello")
  inspect(world, content="World")
  inspect(combined, content="HelloWorld")
  
  // 测试字符串长度
  inspect(empty.length(), content="0")
  inspect(hello.length(), content="5")
  inspect(combined.length(), content="10")
}

test "boolean_logic" {
  // 测试布尔逻辑操作
  let t_and_t = true && true
  let t_and_f = true && false
  let f_and_t = false && true
  let f_and_f = false && false
  
  let t_or_t = true || true
  let t_or_f = true || false
  let f_or_t = false || true
  let f_or_f = false || false
  
  let not_t = !true
  let not_f = !false
  
  inspect(t_and_t, content="true")
  inspect(t_and_f, content="false")
  inspect(f_and_t, content="false")
  inspect(f_and_f, content="false")
  
  inspect(t_or_t, content="true")
  inspect(t_or_f, content="true")
  inspect(f_or_t, content="true")
  inspect(f_or_f, content="false")
  
  inspect(not_t, content="false")
  inspect(not_f, content="true")
}

test "list_operations" {
  // 测试列表基本操作
  let empty_list = []
  let single_item = [1]
  let multiple_items = [1, 2, 3]
  
  inspect(empty_list.length(), content="0")
  inspect(single_item.length(), content="1")
  inspect(multiple_items.length(), content="3")
  
  inspect(empty_list.is_empty(), content="true")
  inspect(single_item.is_empty(), content="false")
  inspect(multiple_items.is_empty(), content="false")
  
  // 测试列表访问
  inspect(single_item[0], content="1")
  inspect(multiple_items[0], content="1")
  inspect(multiple_items[1], content="2")
  inspect(multiple_items[2], content="3")
}

test "char_operations" {
  // 测试字符基本操作
  let char_a = 'a'
  let char_A = 'A'
  let char_digit = '5'
  
  inspect(char_a, content="'a'")
  inspect(char_A, content="'A'")
  inspect(char_digit, content="'5'")
  
  // 测试字符比较
  inspect(char_a < char_A, content="true")
  inspect(char_A < char_a, content="false")
  inspect('a' == 'a', content="true")
  inspect('a' == 'A', content="false")
}

test "comparison_ops" {
  // 测试比较操作
  inspect(1 < 2, content="true")
  inspect(2 < 1, content="false")
  inspect(1 <= 2, content="true")
  inspect(2 <= 2, content="true")
  
  inspect(2 > 1, content="true")
  inspect(1 > 2, content="false")
  inspect(2 >= 1, content="true")
  inspect(2 >= 2, content="true")
  
  inspect(1 == 1, content="true")
  inspect(1 == 2, content="false")
  inspect(1 != 1, content="false")
  inspect(1 != 2, content="true")
}

test "math_functions" {
  // 测试数学函数
  inspect(Int::max(1, 3), content="3")
  inspect(Int::min(1, 3), content="1")
  inspect(Int::abs(5), content="5")
  inspect(Int::abs(-5), content="5")
  inspect(Int::abs(0), content="0")
}

test "type_conversions" {
  // 测试类型转换
  // Int to String
  inspect(42.to_string(), content="42")
  inspect((-42).to_string(), content="-42")
  inspect(0.to_string(), content="0")
  
  // Char to Int
  inspect('a'.to_int(), content="97")
  inspect('A'.to_int(), content="65")
  inspect('0'.to_int(), content="48")
}
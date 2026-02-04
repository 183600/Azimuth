// MoonBit 标准库核心功能测试用例
// 使用标准 MoonBit 测试语法，包含8个测试用例
// 测试覆盖核心模块：Int、String、Bool、List 等基础功能

test "int_arithmetic" {
  // 测试整数基本算术运算
  assert_eq(10 + 3, 13)
  assert_eq(10 - 3, 7)
  assert_eq(10 * 3, 30)
  assert_eq(10 / 3, 3)
  assert_eq(10 % 3, 1)
  
  // 测试负数运算
  assert_eq(-3 + -7, -10)
  assert_eq(-3 - -7, 4)
  assert_eq(-3 * -7, 21)
  assert_eq(-7 / 3, -2)
}

test "string_operations" {
  // 测试字符串基本操作
  let empty = ""
  let hello = "Hello"
  let world = "World"
  let combined = hello + world
  
  assert_eq(empty, "")
  assert_eq(hello, "Hello")
  assert_eq(world, "World")
  assert_eq(combined, "HelloWorld")
  
  // 测试字符串长度
  assert_eq(empty.length(), 0)
  assert_eq(hello.length(), 5)
  assert_eq(combined.length(), 10)
}

test "boolean_logic" {
  // 测试布尔逻辑操作
  assert_eq(true && true, true)
  assert_eq(true && false, false)
  assert_eq(false && true, false)
  assert_eq(false && false, false)
  
  assert_eq(true || true, true)
  assert_eq(true || false, true)
  assert_eq(false || true, true)
  assert_eq(false || false, false)
  
  assert_eq(!true, false)
  assert_eq(!false, true)
}

test "list_operations" {
  // 测试列表基本操作
  let empty_list = []
  let single_item = [1]
  let multiple_items = [1, 2, 3]
  
  assert_eq(empty_list.length(), 0)
  assert_eq(single_item.length(), 1)
  assert_eq(multiple_items.length(), 3)
  
  assert_eq(empty_list.is_empty(), true)
  assert_eq(single_item.is_empty(), false)
  assert_eq(multiple_items.is_empty(), false)
  
  // 测试列表访问
  assert_eq(single_item[0], 1)
  assert_eq(multiple_items[0], 1)
  assert_eq(multiple_items[1], 2)
  assert_eq(multiple_items[2], 3)
}

test "char_operations" {
  // 测试字符基本操作
  let char_a = 'a'
  let char_A = 'A'
  let char_digit = '5'
  
  assert_eq(char_a, 'a')
  assert_eq(char_A, 'A')
  assert_eq(char_digit, '5')
  
  // 测试字符比较
  assert_eq(char_a < char_A, true)
  assert_eq(char_A < char_a, false)
  assert_eq('a' == 'a', true)
  assert_eq('a' == 'A', false)
}

test "comparison_ops" {
  // 测试比较操作
  assert_eq(1 < 2, true)
  assert_eq(2 < 1, false)
  assert_eq(1 <= 2, true)
  assert_eq(2 <= 2, true)
  
  assert_eq(2 > 1, true)
  assert_eq(1 > 2, false)
  assert_eq(2 >= 1, true)
  assert_eq(2 >= 2, true)
  
  assert_eq(1 == 1, true)
  assert_eq(1 == 2, false)
  assert_eq(1 != 1, false)
  assert_eq(1 != 2, true)
}

test "math_functions" {
  // 测试数学函数
  assert_eq(Int::max(1, 3), 3)
  assert_eq(Int::min(1, 3), 1)
  assert_eq(Int::abs(5), 5)
  assert_eq(Int::abs(-5), 5)
  assert_eq(Int::abs(0), 0)
}

test "type_conversions" {
  // 测试类型转换
  // Int to String
  assert_eq(42.to_string(), "42")
  assert_eq((-42).to_string(), "-42")
  assert_eq(0.to_string(), "0")
  
  // Char to Int
  assert_eq('a'.to_int(), 97)
  assert_eq('A'.to_int(), 65)
  assert_eq('0'.to_int(), 48)
}
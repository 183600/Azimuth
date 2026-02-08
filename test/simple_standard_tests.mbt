// MoonBit 标准库简单测试用例
// 使用标准 MoonBit 测试语法，包含8个测试用例
// 测试覆盖核心功能：Int、String、Bool、List 等基础功能

test "int_arithmetic" {
  // 测试整数基本算术运算
  let a = 10
  let b = 3
  let sum = a + b
  let diff = a - b
  let product = a * b
  let quotient = a / b
  let remainder = a % b
  
  @test.assert_eq(13, sum)
  @test.assert_eq(7, diff)
  @test.assert_eq(30, product)
  @test.assert_eq(3, quotient)
  @test.assert_eq(1, remainder)
}

test "string_operations" {
  // 测试字符串基本操作
  let empty = ""
  let hello = "Hello"
  let world = "World"
  let combined = hello + world
  
  @test.assert_eq("", empty)
  @test.assert_eq("Hello", hello)
  @test.assert_eq("World", world)
  @test.assert_eq("HelloWorld", combined)
  
  // 测试字符串长度
  @test.assert_eq(0, empty.length())
  @test.assert_eq(5, hello.length())
  @test.assert_eq(10, combined.length())
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
  
  @test.assert_eq(true, t_and_t)
  @test.assert_eq(false, t_and_f)
  @test.assert_eq(false, f_and_t)
  @test.assert_eq(false, f_and_f)
  
  @test.assert_eq(true, t_or_t)
  @test.assert_eq(true, t_or_f)
  @test.assert_eq(true, f_or_t)
  @test.assert_eq(false, f_or_f)
  
  @test.assert_eq(false, not_t)
  @test.assert_eq(true, not_f)
}

test "list_operations" {
  // 测试列表基本操作
  let empty_list = []
  let single_item = [1]
  let multiple_items = [1, 2, 3]
  
  @test.assert_eq(0, empty_list.length())
  @test.assert_eq(1, single_item.length())
  @test.assert_eq(3, multiple_items.length())
  
  @test.assert_eq(true, empty_list.is_empty())
  @test.assert_eq(false, single_item.is_empty())
  @test.assert_eq(false, multiple_items.is_empty())
  
  // 测试列表访问
  @test.assert_eq(1, single_item[0])
  @test.assert_eq(1, multiple_items[0])
  @test.assert_eq(2, multiple_items[1])
  @test.assert_eq(3, multiple_items[2])
}

test "char_operations" {
  // 测试字符基本操作
  let char_a = 'a'
  let char_A = 'A'
  let char_digit = '5'
  
  @test.assert_eq('a', char_a)
  @test.assert_eq('A', char_A)
  @test.assert_eq('5', char_digit)
  
  // 测试字符比较
  @test.assert_eq(true, char_a < char_A)
  @test.assert_eq(false, char_A < char_a)
  @test.assert_eq(true, 'a' == 'a')
  @test.assert_eq(false, 'a' == 'A')
}

test "comparison_ops" {
  // 测试比较操作
  @test.assert_eq(true, 1 < 2)
  @test.assert_eq(false, 2 < 1)
  @test.assert_eq(true, 1 <= 2)
  @test.assert_eq(true, 2 <= 2)
  
  @test.assert_eq(true, 2 > 1)
  @test.assert_eq(false, 1 > 2)
  @test.assert_eq(true, 2 >= 1)
  @test.assert_eq(true, 2 >= 2)
  
  @test.assert_eq(true, 1 == 1)
  @test.assert_eq(false, 1 == 2)
  @test.assert_eq(false, 1 != 1)
  @test.assert_eq(true, 1 != 2)
}

test "math_functions" {
  // 测试数学函数
  @test.assert_eq(3, Int::max(1, 3))
  @test.assert_eq(1, Int::min(1, 3))
  @test.assert_eq(5, Int::abs(5))
  @test.assert_eq(5, Int::abs(-5))
  @test.assert_eq(0, Int::abs(0))
}

test "type_conversions" {
  // 测试类型转换
  // Int to String
  @test.assert_eq("42", 42.to_string())
  @test.assert_eq("-42", (-42).to_string())
  @test.assert_eq("0", 0.to_string())
  
  // Char to Int
  @test.assert_eq(97, 'a'.to_int())
  @test.assert_eq(65, 'A'.to_int())
  @test.assert_eq(48, '0'.to_int())
}
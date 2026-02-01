// Copyright 2026 International Digital Economy Academy
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// 标准 MoonBit 测试套件
// 包含 10 个核心功能测试，使用标准 MoonBit 测试语法

test "integer_arithmetic_operations" {
  // 测试整数基本算术运算
  inspect(1 + 2, content="3")
  inspect(10 - 3, content="7")
  inspect(4 * 5, content="20")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
  
  // 测试负数运算
  inspect(-5 + 3, content="-2")
  inspect(-10 * -2, content="20")
  inspect(-15 / 3, content="-5")
  
  // 测试零运算
  inspect(0 + 42, content="42")
  inspect(42 * 0, content="0")
  inspect(0 / 1, content="0")
}

test "string_operations_and_unicode" {
  // 测试字符串基本操作
  let empty = ""
  let hello = "Hello"
  let world = "World"
  
  inspect(empty.length(), content="0")
  inspect(hello.length(), content="5")
  inspect(hello + " " + world, content="Hello World")
  
  // 测试 Unicode 字符串
  let chinese = "你好世界"
  let emoji = "🌟✨"
  
  inspect(chinese.length(), content="4")
  inspect(emoji.length(), content="2")
  inspect(chinese + " " + emoji, content="你好世界 🌟✨")
  
  // 测试字符串比较
  inspect("apple" < "banana", content="true")
  inspect("Hello" == "Hello", content="true")
  inspect("hello" != "Hello", content="true")
}

test "array_functional_operations" {
  // 测试数组创建和基本操作
  let numbers = [1, 2, 3, 4, 5]
  let empty_array : Array[Int] = []
  
  inspect(numbers.length(), content="5")
  inspect(empty_array.length(), content="0")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  
  // 测试数组 map 操作
  let doubled = numbers.map(x => x * 2)
  let squared = numbers.map(x => x * x)
  
  inspect(doubled, content="[2, 4, 6, 8, 10]")
  inspect(squared, content="[1, 4, 9, 16, 25]")
  
  // 测试数组 filter 操作
  let evens = numbers.filter(x => x % 2 == 0)
  let greater_than_three = numbers.filter(x => x > 3)
  
  inspect(evens, content="[2, 4]")
  inspect(greater_than_three, content="[4, 5]")
}

test "boolean_logic_and_comparisons" {
  // 测试基本布尔运算
  let t = true
  let f = false
  
  inspect(t && t, content="true")
  inspect(t && f, content="false")
  inspect(t || f, content="true")
  inspect(f || f, content="false")
  inspect(!t, content="false")
  inspect(!f, content="true")
  
  // 测试数值比较
  inspect(5 > 3, content="true")
  inspect(5 >= 5, content="true")
  inspect(3 < 5, content="true")
  inspect(3 <= 3, content="true")
  inspect(5 == 5, content="true")
  inspect(5 != 3, content="true")
  
  // 测试字符串比较
  inspect("a" < "b", content="true")
  inspect("apple" <= "banana", content="true")
  inspect("z" > "a", content="true")
  inspect("hello" >= "hello", content="true")
}

test "option_type_handling" {
  // 测试 Option 类型基本操作
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
  
  // 测试 Option map 操作
  let doubled = some_value.map(x => x * 2)
  let stringified = some_value.map(x => x.to_string())
  let mapped_none = none_value.map(x => x * 2)
  
  inspect(doubled, content="Some(84)")
  inspect(stringified, content="Some(\"42\")")
  inspect(mapped_none, content="None")
  
  // 测试 unwrap_or 操作
  let unwrapped_some = some_value.unwrap_or(0)
  let unwrapped_none = none_value.unwrap_or(100)
  
  inspect(unwrapped_some, content="42")
  inspect(unwrapped_none, content="100")
}

test "result_type_error_handling" {
  // 测试 Result 类型基本操作
  let success : Result[String, String] = Ok("success")
  let error : Result[String, String] = Err("error message")
  
  inspect(success.is_ok(), content="true")
  inspect(success.is_err(), content="false")
  inspect(error.is_ok(), content="false")
  inspect(error.is_err(), content="true")
  
  // 测试 Result map 操作
  let uppercased = success.map(s => s.uppercase())
  let mapped_error = error.map(s => s.uppercase())
  
  inspect(uppercased, content="Ok(\"SUCCESS\")")
  inspect(mapped_error, content="Err(\"error message\")")
  
  // 测试 Result map_err 操作
  let error_message = error.map_err(e => "Error: " + e)
  inspect(error_message, content="Err(\"Error: error message\")")
}

test "mathematical_functions" {
  // 测试基本数学函数
  inspect(@math.abs(-5.0), content="5")
  inspect(@math.abs(5.0), content="5")
  inspect(@math.abs(0.0), content="0")
  
  // 测试幂运算
  inspect(@math.powf(2.0, 3.0), content="8")
  inspect(@math.powf(3.0, 2.0), content="9")
  inspect(@math.sqrtf(16.0), content="4")
  inspect(@math.sqrtf(9.0), content="3")
  
  // 测试舍入函数
  inspect(@math.floorf(3.7), content="3")
  inspect(@math.ceilf(3.2), content="4")
  inspect(@math.roundf(3.5), content="4")
  inspect(@math.roundf(3.4), content="3")
  
  // 测试最小最大值
  inspect(@math.min(5, 10), content="5")
  inspect(@math.max(5, 10), content="10")
  inspect(@math.min(-5, -10), content="-10")
  inspect(@math.max(-5, -10), content="-5")
}

test "character_operations" {
  // 测试字符基本操作
  let ascii_char = 'A'
  let digit_char = '5'
  let space_char = ' '
  
  inspect(ascii_char.to_string(), content="A")
  inspect(digit_char.to_string(), content="5")
  inspect(space_char.to_string(), content=" ")
  
  // 测试字符比较
  inspect('a' < 'b', content="true")
  inspect('A' < 'a', content="true")
  inspect('0' < '9', content="true")
  inspect(' ' < 'A', content="true")
  
  // 测试 Unicode 字符
  let chinese_char = '中'
  let emoji_char = '🌟'
  
  inspect(chinese_char.to_string(), content="中")
  inspect(emoji_char.to_string(), content="🌟")
  
  // 测试字符相等性
  inspect('x' == 'x', content="true")
  inspect('x' != 'X', content="true")
}

test "type_conversions" {
  // 测试整数类型转换
  let int_val = 42
  let uint_val = int_val.reinterpret_as_uint()
  let back_to_int = uint_val.to_int()
  
  inspect(uint_val > 0, content="true")
  inspect(back_to_int, content="42")
  
  // 测试字节转换
  let byte_val = 65.to_byte()
  let back_from_byte = byte_val.to_int()
  
  inspect(back_from_byte, content="65")
  
  // 测试浮点数转换
  let float_val = 3.14
  let int_from_float = float_val.to_int()
  
  inspect(int_from_float, content="3")
  
  // 测试字符串到数值转换
  let string_num = "123"
  let parsed_int = string_num.parse_int()
  
  inspect(parsed_int, content="Some(123)")
  
  let invalid_num = "abc"
  let parsed_invalid = invalid_num.parse_int()
  
  inspect(parsed_invalid, content="None")
}

test "byte_and_memory_operations" {
  // 测试字节操作
  let num = 0x12345678
  let be_bytes = @int.Int::to_be_bytes(num)
  let le_bytes = @int.Int::to_le_bytes(num)
  
  // 验证字节长度
  inspect(be_bytes.length(), content="4")
  inspect(le_bytes.length(), content="4")
  
  // 验证大端序第一个字节
  inspect(be_bytes[0], content="18")  // 0x12 = 18
  
  // 验证小端序第一个字节
  inspect(le_bytes[0], content="120") // 0x78 = 120
  
  // 测试字节到整数转换
  let single_byte = 255.to_byte()
  inspect(single_byte.to_int(), content="255")
  
  // 测试零值字节
  let zero_byte = 0.to_byte()
  inspect(zero_byte.to_int(), content="0")
}
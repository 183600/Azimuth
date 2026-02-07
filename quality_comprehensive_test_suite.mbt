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

// 高质量的 MoonBit 测试用例集合
// 包含 int、char、math 和 string 模块的综合测试

///|
test "int overflow behavior comprehensive" {
  // Test addition overflow
  assert_eq(2147483647 + 1, -2147483648)
  assert_eq(-2147483648 - 1, 2147483647)
  
  // Test multiplication overflow
  assert_eq(2147483647 * 2, -2)
  assert_eq((2147483647 / 2) * 2, 2147483646)
  
  // Test subtraction overflow
  assert_eq(-2147483648 - 2147483647, 1)
  assert_eq(0 - (-2147483648), -2147483648)
}

///|
test "int bit manipulation advanced" {
  // Test bit counting and manipulation
  assert_eq(0.to_uint().bit_count(), 0)
  assert_eq(1.to_uint().bit_count(), 1)
  assert_eq(15.to_uint().bit_count(), 4)  // 0b1111
  assert_eq(16.to_uint().bit_count(), 1)  // 0b10000
  
  // Test leading zeros
  assert_eq(0.to_uint().leading_zeros(), 32)
  assert_eq(1.to_uint().leading_zeros(), 31)
  
  // Test trailing zeros
  assert_eq(0.to_uint().trailing_zeros(), 32)
  assert_eq(1.to_uint().trailing_zeros(), 0)
  assert_eq(8.to_uint().trailing_zeros(), 3)
}

///|
test "int sign extension and bitwise operations" {
  // Test sign extension in right shifts
  assert_eq(-1 >> 1, -1)
  assert_eq(-2 >> 1, -1)
  assert_eq(-4 >> 2, -1)
  assert_eq(-8 >> 3, -1)
  
  // Test bitwise NOT boundaries
  assert_eq(~2147483647, -2147483648)
  assert_eq(~(-2147483648), 2147483647)
  assert_eq(~0, -1)
  assert_eq(~(-1), 0)
}

///|
test "Unicode character categories comprehensive" {
  // Test various Unicode categories
  let chinese_char = '中'
  let arabic_char = 'ا'
  let russian_char = 'Я'
  let emoji = '😀'
  let currency = '€'
  let math_symbol = '∑'
  
  // Test Unicode properties
  assert_true(chinese_char.is_printable())
  assert_true(arabic_char.is_printable())
  assert_true(russian_char.is_printable())
  assert_true(emoji.is_printable())
  assert_true(currency.is_printable())
  assert_true(math_symbol.is_printable())
  
  // Test non-ASCII characters are not ASCII-specific
  assert_false(chinese_char.is_ascii())
  assert_false(arabic_char.is_ascii())
  assert_false(russian_char.is_ascii())
  assert_false(emoji.is_ascii())
  assert_false(currency.is_ascii())
  assert_false(math_symbol.is_ascii())
}

///|
test "Unicode edge cases and special characters" {
  // Test zero-width characters
  let zero_width_space = '\u{200B}'
  let zero_width_joiner = '\u{200D}'
  let zero_width_non_joiner = '\u{200C}'
  
  assert_false(zero_width_space.is_printable())
  assert_false(zero_width_joiner.is_printable())
  assert_false(zero_width_non_joiner.is_printable())
  
  // Test directional characters
  let left_to_right_mark = '\u{200E}'
  let right_to_left_mark = '\u{200F}'
  
  assert_false(left_to_right_mark.is_printable())
  assert_false(right_to_left_mark.is_printable())
  
  // Test various Unicode spaces
  let em_space = '\u{2003}'
  let en_space = '\u{2002}'
  let thin_space = '\u{2009}'
  
  assert_true(em_space.is_whitespace())
  assert_true(en_space.is_whitespace())
  assert_true(thin_space.is_whitespace())
}

///|
test "Character code points and encoding" {
  // Test various Unicode code points
  assert_eq('A'.to_int(), 65)
  assert_eq('a'.to_int(), 97)
  assert_eq('0'.to_int(), 48)
  assert_eq('中'.to_int(), 20013)
  assert_eq('😀'.to_int(), 128512)
  
  // Test character creation from code points
  assert_eq(65.to_char(), 'A')
  assert_eq(97.to_char(), 'a')
  assert_eq(48.to_char(), '0')
  assert_eq(20013.to_char(), '中')
  assert_eq(128512.to_char(), '😀')
}

///|
test "math trigonometric functions precision" {
  // Test sine function
  assert_true(@math.sin(0.0).is_close(0.0))
  assert_true(@math.sin(@math.pi / 2.0).is_close(1.0))
  assert_true(@math.sin(@math.pi).is_close(0.0))
  assert_true(@math.sin(3.0 * @math.pi / 2.0).is_close(-1.0))
  
  // Test cosine function
  assert_true(@math.cos(0.0).is_close(1.0))
  assert_true(@math.cos(@math.pi / 2.0).is_close(0.0))
  assert_true(@math.cos(@math.pi).is_close(-1.0))
  assert_true(@math.cos(3.0 * @math.pi / 2.0).is_close(0.0))
  
  // Test tangent function
  assert_true(@math.tan(0.0).is_close(0.0))
  assert_true(@math.tan(@math.pi / 4.0).is_close(1.0))
}

///|
test "math exponential and logarithmic functions" {
  // Test exponential function
  assert_true(@math.exp(0.0).is_close(1.0))
  assert_true(@math.exp(1.0).is_close(@math.e))
  assert_true(@math.exp(-1.0).is_close(1.0 / @math.e))
  
  // Test natural logarithm
  assert_true(@math.log(1.0).is_close(0.0))
  assert_true(@math.log(@math.e).is_close(1.0))
  assert_true(@math.log(1.0 / @math.e).is_close(-1.0))
  
  // Test base-10 logarithm
  assert_true(@math.log10(1.0).is_close(0.0))
  assert_true(@math.log10(10.0).is_close(1.0))
  assert_true(@math.log10(100.0).is_close(2.0))
}

///|
test "string Unicode handling comprehensive" {
  // Test Unicode strings
  let chinese = "中文测试"
  let emoji = "🚀🌟💻"
  let mixed = "Hello 世界 🌍"
  
  // Test Unicode string length
  assert_eq(chinese.length(), 4)
  assert_eq(emoji.length(), 3)
  assert_eq(mixed.length(), 9)
  
  // Test Unicode character access
  assert_eq(chinese[0], '中')
  assert_eq(emoji[1], '🌟')
  assert_eq(mixed[6], '世')
  
  // Test Unicode slicing
  assert_eq(chinese.slice(0, 2), "中文")
  assert_eq(emoji.slice(1, 2), "🌟")
  assert_eq(mixed.slice(6, 8), "世界")
}
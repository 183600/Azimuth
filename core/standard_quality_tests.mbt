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

// 高质量标准 MoonBit 测试用例集
// 包含 10 个核心功能测试，使用标准 MoonBit 测试语法

test "int_arithmetic_operations" {
  // 测试整数算术运算
  inspect(15 + 27, content="42")
  inspect(100 - 58, content="42")
  inspect(6 * 7, content="42")
  inspect(84 / 2, content="42")
  inspect(85 % 43, content="42")
  
  // 测试负数运算
  inspect(-10 + 52, content="42")
  inspect(50 - 92, content="-42")
  inspect(-6 * 7, content="-42")
  inspect(-84 / 2, content="-42")
}

test "string_advanced_operations" {
  // 测试字符串高级操作
  let text = "MoonBit Programming"
  let empty = ""
  
  // 测试长度和索引
  inspect(text.length(), content="18")
  inspect(empty.length(), content="0")
  inspect(text.char_at(0), content="'M'")
  inspect(text.char_at(7), content="'B'")
  
  // 测试字符串比较
  inspect("hello" == "hello", content="true")
  inspect("hello" != "world", content="true")
  inspect("apple" < "banana", content="true")
  
  // 测试字符串连接
  inspect("Moon" + "Bit", content="MoonBit")
  inspect("a" + "b" + "c", content="abc")
}

test "array_comprehensive_operations" {
  // 测试数组综合操作
  let numbers = [10, 20, 30, 40, 50]
  let empty = []
  
  // 测试基本属性
  inspect(numbers.length(), content="5")
  inspect(empty.length(), content="0")
  inspect(numbers[0], content="10")
  inspect(numbers[4], content="50")
  
  // 测试数组操作
  let first = numbers.unsafe_get(0)
  let last = numbers.unsafe_get(4)
  inspect(first, content="10")
  inspect(last, content="50")
  
  // 测试数组包含
  let contains_30 = numbers.fold(false, (acc, x) => acc || x == 30)
  let contains_99 = numbers.fold(false, (acc, x) => acc || x == 99)
  inspect(contains_30, content="true")
  inspect(contains_99, content="false")
}

test "option_type_pattern_matching" {
  // 测试 Option 类型的模式匹配
  let some_number : Int? = Some(123)
  let no_number : Int? = None
  let some_text : String? = Some("hello")
  
  // 测试 is_some 和 is_none
  inspect(some_number.is_some(), content="true")
  inspect(some_number.is_none(), content="false")
  inspect(no_number.is_some(), content="false")
  inspect(no_number.is_none(), content="true")
  
  // 测试 map 操作
  let doubled = some_number.map(x => x * 2)
  let mapped_none = no_number.map(x => x * 2)
  let uppercased = some_text.map(s => s.uppercase())
  
  inspect(doubled, content="Some(246)")
  inspect(mapped_none, content="None")
  inspect(uppercased, content="Some(\"HELLO\")")
  
  // 测试 unwrap_or
  let safe_value = some_number.unwrap_or(0)
  let default_value = no_number.unwrap_or(999)
  
  inspect(safe_value, content="123")
  inspect(default_value, content="999")
}

test "result_type_error_handling" {
  // 测试 Result 类型的错误处理
  let success_int : Result[Int, String] = Ok(200)
  let error_int : Result[Int, String] = Err("Invalid input")
  let success_str : Result[String, Int] = Ok("success")
  
  // 测试 is_ok 和 is_err
  inspect(success_int.is_ok(), content="true")
  inspect(success_int.is_err(), content="false")
  inspect(error_int.is_ok(), content="false")
  inspect(error_int.is_err(), content="true")
  
  // 测试 map 和 map_err
  let doubled = success_int.map(x => x * 2)
  let mapped_err = error_int.map(x => x * 2)
  let err_length = error_int.map_err(e => e.length())
  
  inspect(doubled, content="Ok(400)")
  inspect(mapped_err, content="Err(\"Invalid input\")")
  inspect(err_length, content="Err(12)")
  
  // 测试 unwrap_or
  let safe_ok = success_int.unwrap_or(0)
  let safe_err = error_int.unwrap_or(0)
  
  inspect(safe_ok, content="200")
  inspect(safe_err, content="0")
}

test "boolean_logical_expressions" {
  // 测试布尔逻辑表达式
  let t = true
  let f = false
  
  // 基本逻辑运算
  inspect(t && t, content="true")
  inspect(t && f, content="false")
  inspect(f && t, content="false")
  inspect(f && f, content="false")
  
  inspect(t || t, content="true")
  inspect(t || f, content="true")
  inspect(f || t, content="true")
  inspect(f || f, content="false")
  
  inspect(!t, content="false")
  inspect(!f, content="true")
  
  // 复合逻辑表达式
  inspect((t && f) || t, content="true")
  inspect((t || f) && f, content="false")
  inspect(!(t && f), content="true")
  inspect(!t || !f, content="true")
}

test "char_unicode_operations" {
  // 测试字符和 Unicode 操作
  let ascii_a = 'a'
  let ascii_Z = 'Z'
  let digit_5 = '5'
  let space = ' '
  let unicode_世 = '世'
  let emoji_心 = '❤'
  
  // 测试字符转换
  inspect(ascii_a.to_string(), content="a")
  inspect(ascii_Z.to_string(), content="Z")
  inspect(digit_5.to_string(), content="5")
  inspect(space.to_string(), content=" ")
  inspect(unicode_世.to_string(), content="世")
  inspect(emoji_心.to_string(), content="❤")
  
  // 测试字符比较
  inspect('a' < 'b', content="true")
  inspect('A' < 'a', content="true")
  inspect('0' < '9', content="true")
  inspect(' ' < 'a', content="true")
  
  // 测试特殊字符
  inspect('\n'.to_string(), content="\n")
  inspect('\t'.to_string(), content="\t")
}

test "numeric_type_conversions" {
  // 测试数值类型转换
  let positive_int = 42
  let negative_int = -42
  let zero_int = 0
  
  // 测试 uint 转换
  let positive_uint = positive_int.reinterpret_as_uint()
  let negative_uint = negative_int.reinterpret_as_uint()
  let zero_uint = zero_int.reinterpret_as_uint()
  
  inspect(positive_uint.to_int(), content="42")
  inspect(zero_uint.to_int(), content="0")
  inspect(positive_uint > zero_uint, content="true")
  
  // 测试边界值
  inspect(@int.max_value > 0, content="true")
  inspect(@int.min_value < 0, content="true")
  inspect(@int.max_value > @int.min_value, content="true")
  
  // 测试绝对值
  inspect(Int::abs(positive_int), content="42")
  inspect(Int::abs(negative_int), content="42")
  inspect(Int::abs(zero_int), content="0")
}

test "functional_array_operations" {
  // 测试函数式数组操作
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let words = ["apple", "banana", "cherry", "date"]
  
  // 测试 map
  let squared = numbers.map(x => x * x)
  let doubled = numbers.map(x => x * 2)
  let lengths = words.map(w => w.length())
  
  inspect(squared[0], content="1")
  inspect(squared[9], content="100")
  inspect(doubled[4], content="10")
  inspect(lengths, content="[5, 6, 6, 4]")
  
  // 测试 filter
  let evens = numbers.filter(x => x % 2 == 0)
  let odds = numbers.filter(x => x % 2 == 1)
  let long_words = words.filter(w => w.length() > 5)
  
  inspect(evens, content="[2, 4, 6, 8, 10]")
  inspect(odds, content="[1, 3, 5, 7, 9]")
  inspect(long_words, content="[\"banana\", \"cherry\"]")
  
  // 测试 fold
  let sum = numbers.fold(0, (acc, x) => acc + x)
  let product = numbers.fold(1, (acc, x) => acc * x)
  let concatenated = words.fold("", (acc, w) => acc + w)
  
  inspect(sum, content="55")
  inspect(product, content="3628800")
  inspect(concatenated, content="applebananacherrydate")
}

test "edge_cases_and_special_values" {
  // 测试边界情况和特殊值
  let max_int = @int.max_value
  let min_int = @int.min_value
  
  // 测试边界值运算
  inspect(max_int > 0, content="true")
  inspect(min_int < 0, content="true")
  inspect(max_int + min_int, content="-1")  // 溢出测试
  
  // 测试零值运算
  inspect(0 + 0, content="0")
  inspect(0 - 0, content="0")
  inspect(0 * 0, content="0")
  inspect(0 * 42, content="0")
  inspect(42 * 0, content="0")
  
  // 测试单位类型
  let unit1 : Unit = ()
  let unit2 : Unit = ()
  inspect(unit1, content="()")
  inspect(unit2, content="()")
  
  // 测试空数组
  let empty_array : Array[Int] = []
  inspect(empty_array.length(), content="0")
  
  // 测试空字符串
  let empty_string = ""
  inspect(empty_string.length(), content="0")
  inspect(empty_string == "", content="true")
}
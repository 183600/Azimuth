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

// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，包含 10 个核心功能测试

test "integer_arithmetic_operations" {
  // 测试整数算术运算
  let a = 15
  let b = 7
  
  // 基本运算
  inspect(a + b, content="22")
  inspect(a - b, content="8")
  inspect(a * b, content="105")
  inspect(a / b, content="2")
  inspect(a % b, content="1")
  
  // 边界值测试
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  
  // 绝对值测试
  inspect(Int::abs(-42), content="42")
  inspect(Int::abs(0), content="0")
  inspect(Int::abs(42), content="42")
}

test "string_manipulation_functions" {
  // 测试字符串操作函数
  let str = "Hello, MoonBit!"
  let empty = ""
  
  // 长度测试
  inspect(str.length(), content="15")
  inspect(empty.length(), content="0")
  
  // 字符串连接
  let greeting = "Hello"
  let target = "World"
  let message = greeting + ", " + target + "!"
  inspect(message, content="Hello, World!")
  
  // 默认字符串
  inspect(String::default(), content="")
  
  // 字符串比较
  inspect("abc" == "abc", content="true")
  inspect("abc" == "def", content="false")
}

test "list_functional_operations" {
  // 测试列表函数式操作
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  let empty_list : @list.List[Int] = @list.empty()
  
  // 基本操作
  inspect(numbers.length(), content="5")
  inspect(empty_list.is_empty(), content="true")
  inspect(numbers.is_empty(), content="false")
  
  // 头部和尾部
  inspect(numbers.head(), content="Some(1)")
  inspect(numbers.last(), content="Some(5)")
  inspect(empty_list.head(), content="None")
  
  // 映射操作
  let doubled = numbers.map(x => x * 2)
  inspect(doubled, content="@list.from_array([2, 4, 6, 8, 10])")
  
  // 过滤操作
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens, content="@list.from_array([2, 4])")
  
  // 连接操作
  let more = @list.from_array([6, 7, 8])
  let combined = numbers.concat(more)
  inspect(combined, content="@list.from_array([1, 2, 3, 4, 5, 6, 7, 8])")
}

test "array_comprehensive_operations" {
  // 测试数组综合操作
  let arr = [10, 20, 30, 40, 50]
  let empty : Array[Int] = []
  
  // 基本操作
  inspect(arr.length(), content="5")
  inspect(arr[0], content="10")
  inspect(arr[4], content="50")
  inspect(empty.length(), content="0")
  
  // 映射操作
  let squared = arr.map(x => x * x)
  inspect(squared, content="[100, 400, 900, 1600, 2500]")
  
  // 过滤操作
  let large = arr.filter(x => x > 25)
  inspect(large, content="[30, 40, 50]")
  
  // 折叠操作
  let sum = arr.fold(0, (acc, x) => acc + x)
  let product = arr.fold(1, (acc, x) => acc * x)
  inspect(sum, content="150")
  inspect(product, content="12000000")
  
  // 包含测试
  inspect(arr.contains(30), content="true")
  inspect(arr.contains(99), content="false")
}

test "option_type_safety" {
  // 测试 Option 类型安全性
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  let some_string : String? = Some("Hello")
  
  // 基本检查
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
  
  // 映射操作
  let doubled = some_value.map(x => x * 2)
  let mapped_none = none_value.map(x => x * 2)
  inspect(doubled, content="Some(84)")
  inspect(mapped_none, content="None")
  
  // 默认值操作
  let unwrapped = some_value.unwrap_or(0)
  let default_value = none_value.unwrap_or(0)
  inspect(unwrapped, content="42")
  inspect(default_value, content="0")
  
  // 链式操作
  let chained = some_string.map(s => s.length()).unwrap_or(0)
  inspect(chained, content="5")
}

test "result_error_handling" {
  // 测试 Result 错误处理
  let success : Result[String, String] = Ok("Operation successful")
  let failure : Result[String, String] = Err("Network error")
  
  // 基本检查
  inspect(success.is_ok(), content="true")
  inspect(success.is_err(), content="false")
  inspect(failure.is_ok(), content="false")
  inspect(failure.is_err(), content="true")
  
  // 映射操作
  let uppercased = success.map(s => s.uppercase())
  let mapped_err = failure.map(s => s.uppercase())
  inspect(uppercased, content="Ok(\"OPERATION SUCCESSFUL\")")
  inspect(mapped_err, content="Err(\"Network error\")")
  
  // 错误映射
  let error_mapped = failure.map_err(e => "Error: " + e)
  inspect(error_mapped, content="Err(\"Error: Network error\")")
  
  // 链式操作
  let chained_result = success.and_then(s => Ok("Processed: " + s))
  inspect(chained_result, content="Ok(\"Processed: Operation successful\")")
}

test "mathematical_functions" {
  // 测试数学函数
  let pi = 3.14159265359
  let e = 2.71828182846
  
  // 基本数学函数
  inspect(@math.absf(-3.14), content="3.14")
  inspect(@math.sqrtf(16.0), content="4")
  inspect(@math.powf(2.0, 3.0), content="8")
  
  // 三角函数
  inspect(@math.sinf(0.0), content="0")
  inspect(@math.cosf(0.0), content="1")
  inspect(@math.tanf(0.0), content="0")
  
  // 对数函数
  inspect(@math.logf(@math.expf(1.0)), content="1")
  inspect(@math.log10f(100.0), content="2")
  
  // 特殊值
  inspect(@math.hypotf(3.0, 4.0), content="5")
  inspect(@math.cbrtf(27.0), content="3")
  inspect(@math.floorf(3.7), content="3")
  inspect(@math.ceilf(3.2), content="4")
}

test "character_and_unicode" {
  // 测试字符和 Unicode
  let ascii_char = 'A'
  let digit_char = '5'
  let unicode_char = '世'
  
  // 字符转换
  inspect(ascii_char.to_string(), content="A")
  inspect(digit_char.to_string(), content="5")
  inspect(unicode_char.to_string(), content="世")
  
  // 字符比较
  inspect('a' < 'b', content="true")
  inspect('A' < 'a', content="true")
  inspect('0' < '9', content="true")
  
  // Unicode 字符串
  let unicode_str = "Hello 世界 🌍"
  inspect(unicode_str.length(), content="12")
  
  // 字符串索引访问
  let simple = "ABCDE"
  inspect(simple.char_at(0), content="'A'")
  inspect(simple.char_at(4), content="'E'")
}

test "boolean_logical_operations" {
  // 测试布尔逻辑操作
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
  
  // 复合逻辑
  inspect((t && f) || (t || f), content="true")
  inspect((f || f) && (t && t), content="false")
  
  // 布尔值转换
  inspect(t.to_int(), content="1")
  inspect(f.to_int(), content="0")
}

test "type_conversion_safety" {
  // 测试类型转换安全性
  let int_val = 42
  let uint_val = 42U
  let float_val = 3.14
  
  // 整数转换
  inspect(int_val.reinterpret_as_uint(), content="42")
  inspect(uint_val.to_int(), content="42")
  
  // 浮点数转换
  inspect(float_val.to_int(), content="3")
  inspect(int_val.to_float(), content="42")
  
  // 字符串数值转换
  inspect("123".to_int(), content="123")
  inspect("3.14".to_float(), content="3.14")
  inspect(123.to_string(), content="123")
  inspect(3.14.to_string(), content="3.14")
  
  // 边界转换
  let max_int = @int.max_value
  let converted_back = max_int.reinterpret_as_uint().to_int()
  inspect(converted_back, content="2147483647")
  
  // 特殊浮点值
  let inf = @float.infinity
  inspect(inf.is_inf(), content="true")
  inspect(inf > 0.0, content="true")
  inspect(@float.not_a_number.is_nan(), content="true")
}
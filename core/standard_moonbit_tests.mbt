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

// 高质量 MoonBit 标准库测试用例
// 包含 10 个核心功能测试，使用标准 MoonBit 测试语法

test "int_boundary_values" {
  // 测试整数边界值
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  
  // 测试绝对值函数
  inspect(Int::abs(42), content="42")
  inspect(Int::abs(-42), content="42")
  inspect(Int::abs(0), content="0")
}

test "string_basic_operations" {
  // 测试字符串基本操作
  let empty = ""
  let hello = "Hello"
  let world = "World"
  
  inspect(empty.length(), content="0")
  inspect(hello.length(), content="5")
  inspect(hello + " " + world, content="Hello World")
  inspect(String::default(), content="")
}

test "array_zip_with_function" {
  // 测试数组的 zip_with 函数
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let add = (a, b) => a + b
  let multiply = (a, b) => a * b
  
  let summed = @array.zip_with(arr1, arr2, add)
  let multiplied = @array.zip_with(arr1, arr2, multiply)
  
  inspect(summed, content="[5, 7, 9]")
  inspect(multiplied, content="[4, 10, 18]")
  
  // 测试不同长度数组的 zip
  let short = [1, 2]
  let long = [3, 4, 5, 6]
  let zipped = @array.zip_with(short, long, add)
  inspect(zipped, content="[4, 6]")
}

test "math_trigonometric_functions" {
  // 测试数学函数
  inspect(@math.cbrtf(1.0), content="1")
  inspect(@math.cbrtf(8.0), content="2")
  inspect(@math.cbrtf(27.0), content="3")
  inspect(@math.cbrtf(-1.0), content="-1")
  
  // 测试斜边计算
  inspect(@math.hypotf(3.0, 4.0), content="5")
  inspect(@math.hypotf(6.0, 8.0), content="10")
  inspect(@math.hypotf(0.0, 0.0), content="0")
}

test "option_type_operations" {
  // 测试 Option 类型操作
  let some_val : Int? = Some(100)
  let none_val : Int? = None
  
  inspect(some_val.is_some(), content="true")
  inspect(some_val.is_none(), content="false")
  inspect(none_val.is_some(), content="false")
  inspect(none_val.is_none(), content="true")
  
  // 测试 map 操作
  let doubled = some_val.map(x => x * 2)
  let mapped_none = none_val.map(x => x * 2)
  
  inspect(doubled, content="Some(200)")
  inspect(mapped_none, content="None")
  
  // 测试 unwrap_or
  let unwrapped = some_val.unwrap_or(0)
  let default_val = none_val.unwrap_or(0)
  
  inspect(unwrapped, content="100")
  inspect(default_val, content="0")
}

test "result_type_error_handling" {
  // 测试 Result 类型错误处理
  let success : Result[String, Int] = Ok("success")
  let failure : Result[String, Int] = Err(404)
  
  inspect(success.is_ok(), content="true")
  inspect(success.is_err(), content="false")
  inspect(failure.is_ok(), content="false")
  inspect(failure.is_err(), content="true")
  
  // 测试 map 操作
  let uppercased = success.map(s => s.uppercase())
  let mapped_err = failure.map(s => s.uppercase())
  
  inspect(uppercased, content="Ok(\"SUCCESS\")")
  inspect(mapped_err, content="Err(404)")
}

test "byte_conversion_operations" {
  // 测试字节转换操作
  let num = 305419896  // 0x12345678
  let be_bytes = @int.Int::to_be_bytes(num)
  let le_bytes = @int.Int::to_le_bytes(num)
  
  // 验证字节长度
  inspect(be_bytes.length(), content="4")
  inspect(le_bytes.length(), content="4")
  
  // 验证大端序字节 (应该是 [0x12, 0x34, 0x56, 0x78])
  inspect(be_bytes[0], content="18")  // 0x12 = 18
  inspect(be_bytes[1], content="52")  // 0x34 = 52
  inspect(be_bytes[2], content="86")  // 0x56 = 86
  inspect(be_bytes[3], content="120") // 0x78 = 120
  
  // 验证小端序字节 (应该是 [0x78, 0x56, 0x34, 0x12])
  inspect(le_bytes[0], content="120") // 0x78 = 120
  inspect(le_bytes[1], content="86")  // 0x56 = 86
  inspect(le_bytes[2], content="52")  // 0x34 = 52
  inspect(le_bytes[3], content="18")  // 0x12 = 18
}

test "char_and_unicode_operations" {
  // 测试字符和 Unicode 操作
  let ascii_char = 'A'
  let unicode_char = '世'
  let emoji_char = '😊'
  
  inspect(ascii_char.to_string(), content="A")
  inspect(unicode_char.to_string(), content="世")
  inspect(emoji_char.to_string(), content="😊")
  
  // 测试字符比较
  inspect('a' < 'b', content="true")
  inspect('A' < 'a', content="true")
  inspect('0' < '9', content="true")
  
  // 测试 Unicode 字符串
  let unicode_str = "Hello 世界"
  inspect(unicode_str.length(), content="8")
  inspect(unicode_str.char_at(0), content="'H'")
  inspect(unicode_str.char_at(6), content="'世'")
}

test "array_functional_operations" {
  // 测试数组函数式操作
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // 测试 map
  let squared = numbers.map(x => x * x)
  inspect(squared.length(), content="10")
  inspect(squared[0], content="1")
  inspect(squared[9], content="100")
  
  // 测试 filter
  let evens = numbers.filter(x => x % 2 == 0)
  let odds = numbers.filter(x => x % 2 == 1)
  
  inspect(evens, content="[2, 4, 6, 8, 10]")
  inspect(odds, content="[1, 3, 5, 7, 9]")
  
  // 测试 fold
  let sum = numbers.fold(0, (acc, x) => acc + x)
  let product = numbers.fold(1, (acc, x) => acc * x)
  
  inspect(sum, content="55")
  inspect(product, content="3628800")
}

test "boolean_logic_and_type_conversions" {
  // 测试布尔逻辑和类型转换
  let t = true
  let f = false
  
  // 基本逻辑运算
  inspect(t && t, content="true")
  inspect(t && f, content="false")
  inspect(t || f, content="true")
  inspect(f || f, content="false")
  inspect(!t, content="false")
  inspect(!f, content="true")
  
  // 测试数值类型转换
  let int_val = 42
  let uint_val = int_val.reinterpret_as_uint()
  
  inspect(uint_val > 0, content="true")
  inspect(uint_val.to_int(), content="42")
  
  // 测试浮点数特殊值
  let inf = @float.infinity
  let neg_inf = @float.neg_infinity
  let nan = @float.not_a_number
  
  inspect(inf.is_inf(), content="true")
  inspect(neg_inf.is_neg_inf(), content="true")
  inspect(nan.is_nan(), content="true")
  inspect(inf > 0.0, content="true")
  inspect(neg_inf < 0.0, content="true")
}
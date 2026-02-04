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

// 标准 MoonBit 核心库测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "int_arithmetic_operations" {
  // 测试整数算术运算
  inspect(1 + 2, content="3")
  inspect(10 - 3, content="7")
  inspect(4 * 5, content="20")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
  
  // 测试负数运算
  inspect(-5 + 3, content="-2")
  inspect(-10 * 2, content="-20")
  inspect(-15 / 3, content="-5")
  
  // 测试大数运算
  inspect(1000000 + 2000000, content="3000000")
  inspect(1000 * 1000, content="1000000")
}

test "string_manipulation_functions" {
  // 测试字符串操作函数
  let hello = "Hello"
  let world = "World"
  let empty = ""
  
  // 字符串连接
  inspect(hello + " " + world, content="Hello World")
  inspect(hello + empty, content="Hello")
  
  // 字符串长度
  inspect(hello.length(), content="5")
  inspect(world.length(), content="5")
  inspect(empty.length(), content="0")
  
  // 字符串比较
  inspect(hello == "Hello", content="true")
  inspect(hello != world, content="true")
  inspect(empty == "", content="true")
  
  // 字符串大小写转换
  let mixed = "HelloWorld"
  inspect(mixed.uppercase(), content="HELLOWORLD")
  inspect(mixed.lowercase(), content="helloworld")
}

test "array_functional_operations" {
  // 测试数组函数式操作
  let numbers = [1, 2, 3, 4, 5]
  let empty = []
  
  // 数组长度
  inspect(numbers.length(), content="5")
  inspect(empty.length(), content="0")
  
  // 数组访问
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  
  // map 操作
  let doubled = numbers.map(x => x * 2)
  inspect(doubled, content="[2, 4, 6, 8, 10]")
  
  // filter 操作
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens, content="[2, 4]")
  
  // fold 操作
  let sum = numbers.fold(0, (acc, x) => acc + x)
  inspect(sum, content="15")
}

test "boolean_logic_operations" {
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
  
  // 布尔比较
  inspect(t == true, content="true")
  inspect(f == false, content="true")
  inspect(t != f, content="true")
}

test "option_type_handling" {
  // 测试 Option 类型处理
  let some_val : Int? = Some(42)
  let none_val : Int? = None
  
  // is_some 和 is_none
  inspect(some_val.is_some(), content="true")
  inspect(some_val.is_none(), content="false")
  inspect(none_val.is_some(), content="false")
  inspect(none_val.is_none(), content="true")
  
  // map 操作
  let doubled = some_val.map(x => x * 2)
  let mapped_none = none_val.map(x => x * 2)
  inspect(doubled, content="Some(84)")
  inspect(mapped_none, content="None")
  
  // unwrap_or 操作
  let unwrapped = some_val.unwrap_or(0)
  let default_val = none_val.unwrap_or(0)
  inspect(unwrapped, content="42")
  inspect(default_val, content="0")
}

test "result_type_error_handling" {
  // 测试 Result 类型错误处理
  let success : Result[String, String] = Ok("success")
  let failure : Result[String, String] = Err("error")
  
  // is_ok 和 is_err
  inspect(success.is_ok(), content="true")
  inspect(success.is_err(), content="false")
  inspect(failure.is_ok(), content="false")
  inspect(failure.is_err(), content="true")
  
  // map 操作
  let uppercased = success.map(s => s.uppercase())
  let mapped_err = failure.map(s => s.uppercase())
  inspect(uppercased, content="Ok(\"SUCCESS\")")
  inspect(mapped_err, content="Err(\"error\")")
  
  // map_err 操作
  let err_message = failure.map_err(e => e.uppercase())
  inspect(err_message, content="Err(\"ERROR\")")
}

test "mathematical_functions" {
  // 测试数学函数
  let pi = @math.pi_f
  let e = @math.e_f
  
  // 基本数学常数
  inspect(pi > 3.14 && pi < 3.15, content="true")
  inspect(e > 2.71 && e < 2.72, content="true")
  
  // 幂运算
  inspect(@math.powf(2.0, 3.0), content="8")
  inspect(@math.powf(3.0, 2.0), content="9")
  inspect(@math.sqrtf(16.0), content="4")
  inspect(@math.sqrtf(25.0), content="5")
  
  // 三角函数
  inspect(@math.sinf(0.0), content="0")
  inspect(@math.cosf(0.0), content="1")
  
  // 绝对值
  inspect(@math.fabsf(5.0), content="5")
  inspect(@math.fabsf(-5.0), content="5")
}

test "character_and_unicode_operations" {
  // 测试字符和 Unicode 操作
  let ascii_char = 'A'
  let digit_char = '5'
  let unicode_char = '世'
  
  // 字符转字符串
  inspect(ascii_char.to_string(), content="A")
  inspect(digit_char.to_string(), content="5")
  inspect(unicode_char.to_string(), content="世")
  
  // 字符比较
  inspect('a' > 'A', content="true")
  inspect('A' < 'B', content="true")
  inspect('0' < '9', content="true")
  
  // Unicode 字符串
  let unicode_str = "Hello 世界 🌟"
  inspect(unicode_str.length(), content="12")
  inspect(unicode_str.char_at(0), content="'H'")
  inspect(unicode_str.char_at(6), content="'世'")
  inspect(unicode_str.char_at(10), content="'🌟'")
}

test "type_conversion_operations" {
  // 测试类型转换操作
  let int_val = 42
  let uint_val = int_val.reinterpret_as_uint()
  let float_val = @float.from_int(int_val)
  
  // int 到 uint 转换
  inspect(uint_val.to_int(), content="42")
  inspect(uint_val > 0, content="true")
  
  // int 到 float 转换
  inspect(float_val, content="42")
  inspect(float_val > 40.0, content="true")
  
  // 字符串到数值转换
  inspect("123".to_int().unwrap(), content="123")
  inspect("456".parse_int().unwrap(), content="456")
  
  // 数值到字符串转换
  inspect(int_val.to_string(), content="42")
  inspect(uint_val.to_string(), content="42")
}

test "boundary_and_edge_cases" {
  // 测试边界和边缘情况
  // 整数边界值
  inspect(@int.max_value > 0, content="true")
  inspect(@int.min_value < 0, content="true")
  
  // 零值操作
  inspect(0 + 0, content="0")
  inspect(0 * 5, content="0")
  inspect(5 * 0, content="0")
  inspect(0 / 1, content="0")
  
  // 空集合操作
  let empty_array = []
  let empty_string = ""
  let empty_option : Int? = None
  
  inspect(empty_array.length(), content="0")
  inspect(empty_string.length(), content="0")
  inspect(empty_option.is_none(), content="true")
  
  // 极值运算
  let large_num = 1000000
  inspect(large_num + large_num, content="2000000")
  inspect(large_num / 1000, content="1000")
  
  // 特殊浮点值
  let inf = @float.infinity
  let neg_inf = @float.neg_infinity
  let nan = @float.not_a_number
  
  inspect(inf.is_inf(), content="true")
  inspect(neg_inf.is_neg_inf(), content="true")
  inspect(nan.is_nan(), content="true")
}
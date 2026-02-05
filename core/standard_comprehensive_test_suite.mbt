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
// 测试覆盖 int、list、array、string、option、result 等核心模块

test "int_absolute_values" {
  // 测试整数的绝对值功能
  inspect(Int::abs(5), content="5")
  inspect(Int::abs(-5), content="5")
  inspect(Int::abs(0), content="0")
  inspect(Int::abs(@int.min_value), content="2147483648")  // 注意：min_value的绝对值会溢出
}

test "list_construction_and_basic_operations" {
  // 测试列表的构造和基本操作
  let empty_list = @list.empty()
  inspect(empty_list.length(), content="0")
  inspect(empty_list.is_empty(), content="true")
  
  let list1 = @list.from_array([1, 2, 3])
  inspect(list1.length(), content="3")
  inspect(list1.is_empty(), content="false")
  inspect(list1.head(), content="Some(1)")
  inspect(list1.last(), content="Some(3)")
}

test "list_functional_operations" {
  // 测试列表的函数式操作
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // map 操作
  let doubled = numbers.map(x => x * 2)
  inspect(doubled.length(), content="5")
  inspect(doubled.head(), content="Some(2)")
  
  // filter 操作
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens.length(), content="2")
  
  // fold 操作
  let sum = numbers.fold(0, (acc, x) => acc + x)
  inspect(sum, content="15")
}

test "array_basic_operations" {
  // 测试数组的基本操作
  let arr = [1, 2, 3, 4, 5]
  inspect(arr.length(), content="5")
  inspect(arr[0], content="1")
  inspect(arr[4], content="5")
  
  // 数组切片
  let slice = arr[1:3]
  inspect(slice.length(), content="2")
  inspect(slice[0], content="2")
}

test "array_zip_with_operation" {
  // 测试数组的 zip_with 操作
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let sum_arr = @array.zip_with(arr1, arr2, (a, b) => a + b)
  inspect(sum_arr.length(), content="3")
  inspect(sum_arr[0], content="5")
  inspect(sum_arr[1], content="7")
  inspect(sum_arr[2], content="9")
}

test "string_basic_operations" {
  // 测试字符串的基本操作
  let s1 = "Hello"
  let s2 = "World"
  let combined = s1 + " " + s2
  inspect(combined, content="Hello World")
  inspect(combined.length(), content="11")
  
  // 字符串比较
  inspect("abc" == "abc", content="true")
  inspect("abc" != "def", content="true")
  inspect("apple" < "banana", content="true")
}

test "option_type_operations" {
  // 测试 Option 类型的操作
  let some_value = Some(42)
  let none_value : Option[Int] = None
  
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
  
  // Option 的 map 操作
  let mapped = some_value.map(x => x * 2)
  inspect(mapped, content="Some(84)")
  
  let mapped_none = none_value.map(x => x * 2)
  inspect(mapped_none, content="None")
}

test "result_type_operations" {
  // 测试 Result 类型的操作
  let ok_result = Ok(42)
  let err_result = Err("Error message")
  
  inspect(ok_result.is_ok(), content="true")
  inspect(ok_result.is_err(), content="false")
  inspect(err_result.is_ok(), content="false")
  inspect(err_result.is_err(), content="true")
  
  // Result 的 map 操作
  let mapped_ok = ok_result.map(x => x * 2)
  inspect(mapped_ok, content="Ok(84)")
  
  let mapped_err = err_result.map(x => x * 2)
  inspect(mapped_err, content="Err(\"Error message\")")
}

test "char_operations" {
  // 测试字符操作
  let char_a = 'a'
  let char_A = 'A'
  let char_b = 'b'
  let char_digit = '5'
  
  inspect(char_a.to_string(), content="\"a\"")
  inspect(char_A.to_string(), content="\"A\"")
  inspect(char_digit.to_string(), content="\"5\"")
  
  // 字符比较
  inspect(char_a < char_b, content="true")
  inspect(char_A == char_A, content="true")
}

test "mathematical_calculations" {
  // 测试数学计算
  let x = 10.0
  let y = 20.0
  
  // 基本数学运算
  inspect(x + y, content="30")
  inspect(y - x, content="10")
  inspect(x * y, content="200")
  inspect(y / x, content="2")
  
  // 数学函数
  inspect(@math.sqrt(16.0), content="4")
  inspect(@math.pow(2.0, 3.0), content="8")
  inspect(@math.cbrtf(27.0), content="3")
}
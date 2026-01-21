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

// 标准MoonBit测试用例集合 - 10个核心功能测试

// 导入必要的包
test

///|
test "list_basic_operations" {
  // 测试列表基本操作
  let empty_list : @list.List[Int] = @list.empty()
  inspect(empty_list.length(), content="0")
  
  let list1 = @list.from_array([1, 2, 3])
  inspect(list1.length(), content="3")
  
  let list2 = list1.cons(0)
  inspect(list2.length(), content="4")
  
  let head_value = list2.head()
  inspect(head_value, content="Some(0)")
}

///|
test "list_map_and_filter" {
  // 测试列表映射和过滤功能
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  let doubled = numbers.map(x => x * 2)
  inspect(doubled.to_array(), content="[2, 4, 6, 8, 10]")
  
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens.to_array(), content="[2, 4]")
  
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  inspect(sum, content="15")
}

///|
test "array_zip_operations" {
  // 测试数组zip操作
  let arr1 = [1, 2, 3]
  let arr2 = ["a", "b", "c"]
  
  let zipped = @array.zip_with(arr1, arr2, (x, y) => "\{y}\{x}")
  inspect(zipped, content="[\"a1\", \"b2\", \"c3\"]")
  
  // 测试不同长度数组的zip
  let arr3 = [10, 20]
  let zipped2 = @array.zip_with(arr1, arr3, (x, y) => x + y)
  inspect(zipped2, content="[11, 22]")
}

///|
test "string_operations" {
  // 测试字符串操作
  let str1 = "Hello"
  let str2 = "World"
  let combined = str1 + " " + str2
  inspect(combined, content="Hello World")
  
  // 测试字符串长度
  inspect(str1.length(), content="5")
  inspect(combined.length(), content="11")
  
  // 测试字符串比较
  let equal_check = str1 == "Hello"
  inspect(equal_check, content="true")
}

///|
test "int_arithmetic_operations" {
  // 测试整数算术运算
  let a = 10
  let b = 3
  
  inspect(a + b, content="13")
  inspect(a - b, content="7")
  inspect(a * b, content="30")
  inspect(a / b, content="3")
  inspect(a % b, content="1")
  
  // 测试绝对值函数
  inspect(Int::abs(-5), content="5")
  inspect(Int::abs(5), content="5")
  inspect(Int::abs(0), content="0")
}

///|
test "boolean_logic" {
  // 测试布尔逻辑操作
  let t = true
  let f = false
  
  inspect(t && f, content="false")
  inspect(t || f, content="true")
  inspect(!t, content="false")
  inspect(!f, content="true")
  
  // 测试布尔相等性
  inspect(t == true, content="true")
  inspect(f == false, content="true")
  inspect(t != f, content="true")
}

///|
test "option_type_operations" {
  // 测试Option类型操作
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  // 测试is_some和is_none
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
  
  // 测试unwrap_or
  inspect(some_value.unwrap_or(0), content="42")
  inspect(none_value.unwrap_or(0), content="0")
}

///|
test "result_type_operations" {
  // 测试Result类型操作
  let ok_result : Result[String, Int] = Ok("success")
  let err_result : Result[String, Int] = Err(404)
  
  // 测试is_ok和is_err
  inspect(ok_result.is_ok(), content="true")
  inspect(ok_result.is_err(), content="false")
  inspect(err_result.is_ok(), content="false")
  inspect(err_result.is_err(), content="true")
  
  // 测试unwrap_or
  inspect(ok_result.unwrap_or("default"), content="success")
  inspect(err_result.unwrap_or("default"), content="default")
}

///|
test "tuple_operations" {
  // 测试元组操作
  let pair = (10, "hello")
  let triple = (1, 2, 3)
  
  // 测试元组访问
  inspect(pair.0, content="10")
  inspect(pair.1, content="hello")
  inspect(triple.0, content="1")
  inspect(triple.1, content="2")
  inspect(triple.2, content="3")
  
  // 测试元组相等性
  let same_pair = (10, "hello")
  let different_pair = (20, "world")
  inspect(pair == same_pair, content="true")
  inspect(pair == different_pair, content="false")
}

///|
test "comparison_operations" {
  // 测试比较操作
  let numbers = [5, 3, 8, 1, 9]
  
  // 测试基本比较
  inspect(5 > 3, content="true")
  inspect(3 < 8, content="true")
  inspect(5 >= 5, content="true")
  inspect(8 <= 9, content="true")
  inspect(5 == 5, content="true")
  inspect(3 != 8, content="true")
  
  // 测试最大值和最小值
  let max_val = numbers.fold(init=numbers[0], (acc, x) => if x > acc { x } else { acc })
  inspect(max_val, content="9")
  
  let min_val = numbers.fold(init=numbers[0], (acc, x) => if x < acc { x } else { acc })
  inspect(min_val, content="1")
}
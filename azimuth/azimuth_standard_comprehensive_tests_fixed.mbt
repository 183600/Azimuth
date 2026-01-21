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

///|
test "list_basic_operations" {
  // 测试列表基本操作
  let empty_list = []
  inspect(empty_list.length(), content="0")
  
  let list1 = [1, 2, 3]
  inspect(list1.length(), content="3")
  
  let list2 = 0 :: list1
  inspect(list2.length(), content="4")
  
  let head_value = list2[0]
  inspect(head_value, content="0")
}

///|
test "array_basic_operations" {
  // 测试数组基本操作
  let empty_array : Array[Int] = []
  inspect(empty_array.length(), content="0")
  
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  
  let first = numbers[0]
  inspect(first, content="1")
  
  let last = numbers[4]
  inspect(last, content="5")
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
  
  // 测试绝对值
  let abs_neg = if -5 < 0 { -(-5) } else { -5 }
  inspect(abs_neg, content="5")
  
  let abs_pos = if 5 < 0 { -5 } else { 5 }
  inspect(abs_pos, content="5")
  
  let abs_zero = if 0 < 0 { -0 } else { 0 }
  inspect(abs_zero, content="0")
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
  
  // 测试Option匹配
  let is_some_some = match some_value {
    Some(_) => true
    None => false
  }
  inspect(is_some_some, content="true")
  
  let is_some_none = match none_value {
    Some(_) => true
    None => false
  }
  inspect(is_some_none, content="false")
  
  // 测试unwrap_or
  let unwrap_some = match some_value {
    Some(v) => v
    None => 0
  }
  inspect(unwrap_some, content="42")
  
  let unwrap_none = match none_value {
    Some(v) => v
    None => 0
  }
  inspect(unwrap_none, content="0")
}

///|
test "result_type_operations" {
  // 测试Result类型操作
  let ok_result : Result[String, Int] = Ok("success")
  let err_result : Result[String, Int] = Err(404)
  
  // 测试Result匹配
  let is_ok_ok = match ok_result {
    Ok(_) => true
    Err(_) => false
  }
  inspect(is_ok_ok, content="true")
  
  let is_ok_err = match err_result {
    Ok(_) => true
    Err(_) => false
  }
  inspect(is_ok_err, content="false")
  
  // 测试unwrap_or
  let unwrap_ok = match ok_result {
    Ok(v) => v
    Err(_) => "default"
  }
  inspect(unwrap_ok, content="success")
  
  let unwrap_err = match err_result {
    Ok(v) => v
    Err(_) => "default"
  }
  inspect(unwrap_err, content="default")
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

///|
test "array_zip_operations" {
  // 测试数组zip操作
  let arr1 = [1, 2, 3]
  let arr2 = ["a", "b", "c"]
  
  // 手动实现zip_with功能
  let min_length = if arr1.length() < arr2.length() { arr1.length() } else { arr2.length() }
  let result = Array::make(min_length, "")
  for i = 0; i < min_length; i = i + 1 {
    result[i] = arr2[i] + arr1[i].to_string()
  }
  inspect(result, content="[\"a1\", \"b2\", \"c3\"]")
  
  // 测试不同长度数组的zip
  let arr3 = [10, 20]
  let min_length2 = if arr1.length() < arr3.length() { arr1.length() } else { arr3.length() }
  let result2 = Array::make(min_length2, 0)
  for i = 0; i < min_length2; i = i + 1 {
    result2[i] = arr1[i] + arr3[i]
  }
  inspect(result2, content="[11, 22]")
}
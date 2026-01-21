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

test "string_basic_operations" {
  // 测试字符串基本操作
  let str1 = "Hello"
  let str2 = "World"
  let combined = str1 + " " + str2
  inspect(combined, "Hello World")?
  
  // 测试字符串长度
  inspect(str1.length(), 5)?
  inspect(combined.length(), 11)?
  
  // 测试字符串比较
  let equal_check = str1 == "Hello"
  inspect(equal_check, true)?
}

test "int_arithmetic_operations" {
  // 测试整数算术运算
  let a = 10
  let b = 3
  
  inspect(a + b, 13)?
  inspect(a - b, 7)?
  inspect(a * b, 30)?
  inspect(a / b, 3)?
  inspect(a % b, 1)?
  
  // 测试绝对值
  let abs_neg = if -5 < 0 { -(-5) } else { -5 }
  inspect(abs_neg, 5)?
  
  let abs_pos = if 5 < 0 { -5 } else { 5 }
  inspect(abs_pos, 5)?
  
  let abs_zero = if 0 < 0 { -0 } else { 0 }
  inspect(abs_zero, 0)?
}

test "boolean_logic" {
  // 测试布尔逻辑操作
  let t = true
  let f = false
  
  inspect(t && f, false)?
  inspect(t || f, true)?
  inspect(!t, false)?
  inspect(!f, true)?
  
  // 测试布尔相等性
  inspect(t == true, true)?
  inspect(f == false, true)?
  inspect(t != f, true)?
}

test "option_type_operations" {
  // 测试Option类型操作
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  // 测试Option匹配
  let is_some_some = match some_value {
    Some(_) => true
    None => false
  }
  inspect(is_some_some, true)?
  
  let is_some_none = match none_value {
    Some(_) => true
    None => false
  }
  inspect(is_some_none, false)?
  
  // 测试unwrap_or
  let unwrap_some = match some_value {
    Some(v) => v
    None => 0
  }
  inspect(unwrap_some, 42)?
  
  let unwrap_none = match none_value {
    Some(v) => v
    None => 0
  }
  inspect(unwrap_none, 0)?
}

test "result_type_operations" {
  // 测试Result类型操作
  let ok_result : Result[String, Int] = Ok("success")
  let err_result : Result[String, Int] = Err(404)
  
  // 测试Result匹配
  let is_ok_ok = match ok_result {
    Ok(_) => true
    Err(_) => false
  }
  inspect(is_ok_ok, true)?
  
  let is_ok_err = match err_result {
    Ok(_) => true
    Err(_) => false
  }
  inspect(is_ok_err, false)?
  
  // 测试unwrap_or
  let unwrap_ok = match ok_result {
    Ok(v) => v
    Err(_) => "default"
  }
  inspect(unwrap_ok, "success")?
  
  let unwrap_err = match err_result {
    Ok(v) => v
    Err(_) => "default"
  }
  inspect(unwrap_err, "default")?
}

test "tuple_operations" {
  // 测试元组操作
  let pair = (10, "hello")
  let triple = (1, 2, 3)
  
  // 测试元组访问
  inspect(pair.0, 10)?
  inspect(pair.1, "hello")?
  inspect(triple.0, 1)?
  inspect(triple.1, 2)?
  inspect(triple.2, 3)?
  
  // 测试元组相等性
  let same_pair = (10, "hello")
  let different_pair = (20, "world")
  inspect(pair == same_pair, true)?
  inspect(pair == different_pair, false)?
}

test "array_basic_operations" {
  // 测试数组基本操作
  let empty_array : Array[Int] = []
  inspect(empty_array.length(), 0)?
  
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), 5)?
  
  let first = numbers[0]
  inspect(first, 1)?
  
  let last = numbers[4]
  inspect(last, 5)?
}

test "comparison_operations" {
  // 测试比较操作
  // 测试基本比较
  inspect(5 > 3, true)?
  inspect(3 < 8, true)?
  inspect(5 >= 5, true)?
  inspect(8 <= 9, true)?
  inspect(5 == 5, true)?
  inspect(3 != 8, true)?
  
  // 测试最大值和最小值
  let numbers = [5, 3, 8, 1, 9]
  let max_val = numbers.fold(init=numbers[0], (acc, x) => if x > acc { x } else { acc })
  inspect(max_val, 9)?
  
  let min_val = numbers.fold(init=numbers[0], (acc, x) => if x < acc { x } else { acc })
  inspect(min_val, 1)?
}

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
  inspect(result[0], "a1")?
  inspect(result[1], "b2")?
  inspect(result[2], "c3")?
  
  // 测试不同长度数组的zip
  let arr3 = [10, 20]
  let min_length2 = if arr1.length() < arr3.length() { arr1.length() } else { arr3.length() }
  let result2 = Array::make(min_length2, 0)
  for i = 0; i < min_length2; i = i + 1 {
    result2[i] = arr1[i] + arr3[i]
  }
  inspect(result2[0], 11)?
  inspect(result2[1], 22)?
}

test "fold_operations" {
  // 测试折叠操作
  let numbers = [1, 2, 3, 4, 5]
  
  // 测试求和
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  inspect(sum, 15)?
  
  // 测试求积
  let product = numbers.fold(init=1, (acc, x) => acc * x)
  inspect(product, 120)?
  
  // 测试字符串连接
  let words = ["Hello", " ", "World", "!"]
  let sentence = words.fold(init="", (acc, x) => acc + x)
  inspect(sentence, "Hello World!")?
}
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

// 导入必要的函数
using @builtin { inspect }

// MoonBit 标准库增强测试用例
// 包含10个高质量测试用例，涵盖多个核心模块的功能

test "int max/min values and overflow" {
  // 整数最大值和最小值测试
  inspect(Int::max_value, content="2147483647")
  inspect(Int::min_value, content="-2147483648")
  
  // 边界值测试
  let max_int = Int::max_value
  let min_int = Int::min_value
  inspect(max_int + 1, content="-2147483648")
  inspect(min_int - 1, content="2147483647")
  
  // 绝对值测试
  inspect(Int::abs(42), content="42")
  inspect(Int::abs(-42), content="42")
  inspect(Int::abs(0), content="0")
}

test "string concatenation and length" {
  // 字符串连接和长度测试
  let empty = ""
  let single = "A"
  let word = "MoonBit"
  
  inspect(empty.length(), content="0")
  inspect(single.length(), content="1")
  inspect(word.length(), content="7")
  
  // 字符串连接
  let result = empty + single + word
  inspect(result, content="\"AMoonBit\"")
  inspect(result.length(), content="8")
  
  // 多次连接
  let phrase = "Hello" + " " + "MoonBit" + " " + "World"
  inspect(phrase, content="\"Hello MoonBit World\"")
}

test "list basic operations" {
  // 列表基本操作测试
  let empty_list : @list.List[Int] = @list.empty()
  inspect(empty_list.length(), content="0")
  inspect(empty_list.is_empty(), content="true")
  
  // 创建列表
  let single_element = @list.from_array([42])
  inspect(single_element.length(), content="1")
  inspect(single_element.head(), content="Some(42)")
  
  // 多元素列表
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  inspect(numbers.length(), content="5")
  inspect(numbers.head(), content="Some(1)")
  inspect(numbers.last(), content="Some(5)")
}

test "list map and filter operations" {
  // 列表映射和过滤操作测试
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // map 操作
  let doubled = numbers.map(x => x * 2)
  inspect(doubled.to_array(), content="[2, 4, 6, 8, 10]")
  
  // filter 操作
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens.to_array(), content="[2, 4]")
  
  // 组合操作
  let transformed = numbers
    .filter(x => x > 2)
    .map(x => x * 3)
  inspect(transformed.to_array(), content="[9, 12, 15]")
}

test "list fold operations" {
  // 列表折叠操作测试
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // 求和
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  inspect(sum, content="15")
  
  // 求积
  let product = numbers.fold(init=1, (acc, x) => acc * x)
  inspect(product, content="120")
  
  // 连接字符串
  let words = @list.from_array(["Hello", "MoonBit", "World"])
  let sentence = words.fold(init="", (acc, word) => acc + word + " ")
  inspect(sentence.trim(), content="\"HelloMoonBitWorld\"")
}

test "list reverse and concat operations" {
  // 列表反转和连接操作测试
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  
  // 反转
  let reversed = list1.rev()
  inspect(reversed.to_array(), content="[3, 2, 1]")
  
  // 连接
  let concatenated = list1.concat(list2)
  inspect(concatenated.to_array(), content="[1, 2, 3, 4, 5, 6]")
  
  // 反转连接
  let rev_concat = list1.rev_concat(list2)
  inspect(rev_concat.to_array(), content="[3, 2, 1, 4, 5, 6]")
}

test "option type operations" {
  // Option 类型操作测试
  let some_value = Some(42)
  let none_value : Int? = None
  
  // 基本操作
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
  
  // 解构
  match some_value {
    Some(value) => inspect(value, content="42")
    None => inspect(false, content="true") // 不应该到达这里
  }
  
  // 默认值
  inspect(some_value.unwrap_or(0), content="42")
  inspect(none_value.unwrap_or(0), content="0")
}

test "result type operations" {
  // Result 类型操作测试
  let success_result = Ok(42)
  let error_result = Err("Failed")
  
  // 基本操作
  inspect(success_result.is_ok(), content="true")
  inspect(success_result.is_err(), content="false")
  inspect(error_result.is_ok(), content="false")
  inspect(error_result.is_err(), content="true")
  
  // 解构
  match success_result {
    Ok(value) => inspect(value, content="42")
    Err(_) => inspect(false, content="true") // 不应该到达这里
  }
  
  match error_result {
    Ok(_) => inspect(false, content="true") // 不应该到达这里
    Err(msg) => inspect(msg, content="\"Failed\"")
  }
}

test "boolean logic operations" {
  // 布尔逻辑操作测试
  let true_val = true
  let false_val = false
  
  // 基本逻辑
  inspect(true_val && false_val, content="false")
  inspect(true_val || false_val, content="true")
  inspect(!true_val, content="false")
  inspect(!false_val, content="true")
  
  // 复杂逻辑
  let a = true
  let b = false
  let c = true
  inspect((a && b) || c, content="true")
  inspect(a && (b || c), content="true")
  inspect(!a || (!b && c), content="true")
}

test "array to list conversions" {
  // 数组和列表转换测试
  let int_array = [1, 2, 3, 4, 5]
  let string_array = ["a", "b", "c"]
  
  // 数组转列表
  let int_list = @list.from_array(int_array)
  inspect(int_list.to_array(), content="[1, 2, 3, 4, 5]")
  
  let string_list = @list.from_array(string_array)
  inspect(string_list.to_array(), content="[\"a\", \"b\", \"c\"]")
  
  // 空数组/列表
  let empty_array : Array[Int] = []
  let empty_list = @list.from_array(empty_array)
  inspect(empty_list.length(), content="0")
  inspect(empty_list.is_empty(), content="true")
}
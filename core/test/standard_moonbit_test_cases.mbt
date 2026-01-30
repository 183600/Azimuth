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

// 使用标准的 MoonBit 测试语法编写的测试用例

///|
test "integer_basic_arithmetic" {
  // 测试整数基本算术运算
  inspect(10 + 5, content="15")
  inspect(10 - 5, content="5")
  inspect(10 * 5, content="50")
  inspect(10 / 3, content="3")
  inspect(10 % 3, content="1")
}

///|
test "integer_comparison_operations" {
  // 测试整数比较运算
  inspect(10 > 5, content="true")
  inspect(10 < 5, content="false")
  inspect(10 >= 10, content="true")
  inspect(10 <= 5, content="false")
  inspect(10 == 10, content="true")
  inspect(10 != 5, content="true")
}

///|
test "boolean_logic_operations" {
  // 测试布尔逻辑运算
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(false || false, content="false")
  inspect(!false, content="true")
  inspect(!true, content="false")
}

///|
test "string_basic_operations" {
  // 测试字符串基本操作
  let empty = ""
  let hello = "Hello"
  let world = "World"
  let greeting = hello + ", " + world + "!"
  
  inspect(empty.length(), content="0")
  inspect(hello.length(), content="5")
  inspect(greeting, content="Hello, World!")
  inspect(greeting.length() > hello.length(), content="true")
}

///|
test "string_character_access" {
  // 测试字符串字符访问
  let text = "MoonBit"
  inspect(text[0], content="'M'")
  inspect(text[4], content="'B'")
  inspect(text.contains("Bit"), content="true")
  inspect(text.contains("Byte"), content="false")
}

///|
test "array_basic_operations" {
  // 测试数组基本操作
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  inspect(numbers.length() > 0, content="true")
}

///|
test "option_type_operations" {
  // 测试 Option 类型操作
  let some_value : Option[Int] = Some(42)
  let none_value : Option[Int] = None
  
  let result1 = match some_value {
    Some(v) => v
    None => 0
  }
  
  let result2 = match none_value {
    Some(v) => v
    None => 0
  }
  
  inspect(result1, content="42")
  inspect(result2, content="0")
}

///|
test "result_type_operations" {
  // 测试 Result 类型操作
  let success : Result[String, String] = Ok("success")
  let failure : Result[String, String] = Err("error")
  
  let result1 = match success {
    Ok(v) => v
    Err(_) => "failed"
  }
  
  let result2 = match failure {
    Ok(v) => v
    Err(e) => e
  }
  
  inspect(result1, content="success")
  inspect(result2, content="error")
}

///|
test "tuple_operations" {
  // 测试元组操作
  let pair = (10, "hello")
  let triple = (true, 42, "world")
  
  inspect(pair.0, content="10")
  inspect(pair.1, content="hello")
  inspect(triple.0, content="true")
  inspect(triple.1, content="42")
  inspect(triple.2, content="world")
}

///|
test "boundary_values" {
  // 测试边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  inspect(max_int, content="2147483647")
  inspect(min_int, content="-2147483648")
  
  // 测试溢出行为
  inspect(max_int + 1, content="-2147483648")
  inspect(min_int - 1, content="2147483647")
}
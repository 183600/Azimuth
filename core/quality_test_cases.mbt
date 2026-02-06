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

// 高质量 MoonBit 测试用例集
// 包含 8 个精心设计的测试用例，使用标准 MoonBit 测试语法

using @builtin { inspect }

test "advanced_integer_operations" {
  // 高级整数运算测试
  inspect(Int::max_value, content="2147483647")
  inspect(Int::min_value, content="-2147483648")
  inspect((-42).abs(), content="42")
  inspect(42.abs(), content="42")
  inspect(0.abs(), content="0")
}

test "string_manipulation" {
  // 字符串操作测试
  let text = "MoonBit Programming"
  inspect(text.length(), content="18")
  inspect(text.to_uppercase(), content="\"MOONBIT PROGRAMMING\"")
  inspect(text.to_lowercase(), content="\"moonbit programming\"")
  inspect(text.contains("Bit"), content="true")
  inspect(text.contains("Rust"), content="false")
}

test "array_functional_operations" {
  // 数组函数式操作测试
  let numbers = [1, 2, 3, 4, 5]
  let doubled = numbers.map(x => x * 2)
  inspect(doubled, content="[2, 4, 6, 8, 10]")
  
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens, content="[2, 4]")
  
  let sum = numbers.fold(0, (acc, x) => acc + x)
  inspect(sum, content="15")
}

test "option_type_advanced" {
  // Option 类型高级操作测试
  let maybe_number : Int? = Some(42)
  let empty : Int? = None
  
  let result1 = maybe_number.map(x => x * 2)
  inspect(result1, content="Some(84)")
  
  let result2 = empty.map(x => x * 2)
  inspect(result2, content="None")
  
  let result3 = maybe_number.unwrap_or(0)
  inspect(result3, content="42")
  
  let result4 = empty.unwrap_or(0)
  inspect(result4, content="0")
}

test "result_type_error_handling" {
  // Result 类型错误处理测试
  let success : Result[Int, String] = Ok(100)
  let failure : Result[Int, String] = Err("Invalid input")
  
  let result1 = success.map(x => x * 2)
  inspect(result1, content="Ok(200)")
  
  let result2 = failure.map(x => x * 2)
  inspect(result2, content="Err(\"Invalid input\")")
  
  let result3 = success.unwrap_or(0)
  inspect(result3, content="100")
  
  let result4 = failure.unwrap_or(0)
  inspect(result4, content="0")
}

test "character_encoding" {
  // 字符编码测试
  let char_a = 'a'
  let char_z = 'z'
  let char_A = 'A'
  let char_Z = 'Z'
  
  inspect(char_a.to_string(), content="\"a\"")
  inspect(char_z.to_string(), content="\"z\"")
  inspect(char_A.to_string(), content="\"A\"")
  inspect(char_Z.to_string(), content="\"Z\"")
  
  inspect(char_a > char_A, content="true")
  inspect(char_z < char_Z, content="false")
}

test "mathematical_functions" {
  // 数学函数测试
  let x = 16.0
  let y = 4.0
  
  inspect(x.sqrt(), content="4")
  inspect(y.pow(2.0), content="16")
  inspect(x.max(y), content="16")
  inspect(x.min(y), content="4")
  
  let pi = 3.141592653589793
  inspect(pi > 3.0, content="true")
  inspect(pi < 4.0, content="true")
}

test "complex_data_structures" {
  // 复杂数据结构测试
  type Person = { name : String, age : Int }
  
  let people = [
    { name: "Alice", age: 30 },
    { name: "Bob", age: 25 },
    { name: "Charlie", age: 35 }
  ]
  
  let names = people.map(p => p.name)
  inspect(names, content="[\"Alice\", \"Bob\", \"Charlie\"]")
  
  let total_age = people.fold(0, (acc, p) => acc + p.age)
  inspect(total_age, content="90")
  
  let adults = people.filter(p => p.age >= 18)
  inspect(adults.length(), content="3")
}
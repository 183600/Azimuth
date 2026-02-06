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

// 标准 MoonBit 测试用例 - 10个核心功能测试
// 使用标准的 MoonBit 测试语法，覆盖核心功能

test "integer_arithmetic_operations" {
  // 整数算术运算测试
  inspect(15 + 27, content="42")
  inspect(100 - 37, content="63")
  inspect(12 * 8, content="96")
  inspect(45 / 9, content="5")
  inspect(45 % 8, content="5")
}

test "integer_boundary_values" {
  // 整数边界值测试
  inspect(Int::max_value, content="2147483647")
  inspect(Int::min_value, content="-2147483648")
  inspect(0 + Int::max_value, content="2147483647")
  inspect(0 - Int::max_value, content="-2147483647")
}

test "string_operations" {
  // 字符串操作测试
  let prefix = "Hello"
  let suffix = "MoonBit"
  let combined = prefix + ", " + suffix + "!"
  inspect(combined, content="Hello, MoonBit!")
  inspect(combined.length(), content="17")
  inspect(combined[0], content="72")  // 'H' ASCII码
  inspect(combined[16], content="33") // '!' ASCII码
}

test "array_operations" {
  // 数组操作测试
  let numbers = [10, 20, 30, 40, 50]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="10")
  inspect(numbers[4], content="50")
  inspect(numbers[2] + numbers[3], content="70")
}

test "boolean_logic" {
  // 布尔逻辑测试
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(false || false, content="false")
  inspect(!true, content="false")
  inspect(!false, content="true")
}

test "option_type_operations" {
  // Option 类型操作测试
  let some_value = Some(123)
  let none_value : Option[Int] = None
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
}

test "result_type_operations" {
  // Result 类型操作测试
  let success = Ok("operation completed")
  let failure = Err("network error")
  inspect(success.is_ok(), content="true")
  inspect(success.is_err(), content="false")
  inspect(failure.is_ok(), content="false")
  inspect(failure.is_err(), content="true")
}

test "bitwise_operations" {
  // 位运算测试
  inspect(1 << 4, content="16")
  inspect(16 >> 2, content="4")
  inspect(12 & 10, content="8")
  inspect(12 | 10, content="14")
  inspect(12 ^ 10, content="6")
}

test "comparison_operations" {
  // 比较运算测试
  inspect(15 > 10, content="true")
  inspect(15 >= 15, content="true")
  inspect(10 < 15, content="true")
  inspect(15 <= 15, content="true")
  inspect(15 == 15, content="true")
  inspect(15 != 10, content="true")
}

test "type_conversion_operations" {
  // 类型转换测试
  let number = 456
  let string_num = "789"
  inspect(number.to_string(), content="456")
  inspect(string_num.to_int(), content="Some(789)")
  inspect("invalid".to_int(), content="None")
  inspect(true.to_string(), content="true")
  inspect(false.to_string(), content="false")
}
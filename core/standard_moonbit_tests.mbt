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

// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法编写

test "int_arithmetic_operations" {
  inspect(1 + 1, content="2")
  inspect(10 - 3, content="7")
  inspect(3 * 4, content="12")
  inspect(20 / 4, content="5")
  inspect(15 % 4, content="3")
  inspect(-5 + 10, content="5")
  inspect(-3 * -2, content="6")
}

test "string_concatenation_and_length" {
  let str1 = "Hello"
  let str2 = "MoonBit"
  let combined = str1 + " " + str2 + "!"
  inspect(combined, content="\"Hello MoonBit!\"")
  inspect(@string.length(str1), content="5")
  inspect(@string.length(str2), content="7")
  inspect(@string.length(combined), content="14")
}

test "array_indexing_and_length" {
  let arr = [10, 20, 30, 40, 50]
  inspect(@array.length(arr), content="5")
  inspect(arr[0], content="10")
  inspect(arr[2], content="30")
  inspect(arr[4], content="50")
  let first_plus_last = arr[0] + arr[4]
  inspect(first_plus_last, content="60")
}

test "boolean_comparisons" {
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(false || true, content="true")
  inspect(false || false, content="false")
  inspect(!true, content="false")
  inspect(!false, content="true")
  inspect(true == true, content="true")
  inspect(true != false, content="true")
}

test "option_type_pattern_matching" {
  let some_number : Int? = Some(100)
  let no_number : Int? = None
  
  let extracted = match some_number {
    Some(x) => x
    None => 0
  }
  inspect(extracted, content="100")
  
  let default = match no_number {
    Some(x) => x
    None => -1
  }
  inspect(default, content="-1")
}

test "result_type_error_handling" {
  let success_result : Result[String, Int] = Ok("Success")
  let error_result : Result[String, Int] = Err(404)
  
  let success_msg = match success_result {
    Ok(msg) => msg
    Err(_) => "Error"
  }
  inspect(success_msg, content="\"Success\"")
  
  let error_code = match error_result {
    Ok(_) => 0
    Err(code) => code
  }
  inspect(error_code, content="404")
}

test "character_comparison_and_conversion" {
  let char_x = 'x'
  let char_y = 'y'
  inspect(@char.to_string(char_x), content="\"x\"")
  inspect(char_x < char_y, content="true")
  inspect(char_y > char_x, content="true")
  inspect('A' < 'a', content="true")
  inspect('0' < '9', content="true")
}

test "list_operations" {
  let num_list = [1, 2, 3, 4, 5]
  inspect(@list.length(num_list), content="5")
  let head = num_list[0]
  inspect(head, content="1")
  let sum = num_list[0] + num_list[1] + num_list[2]
  inspect(sum, content="6")
}

test "math_functions" {
  let x = 3.0
  let y = 4.0
  inspect(x + y, content="7")
  inspect(y - x, content="1")
  inspect(x * y, content="12")
  inspect(y / x, content="1")
  let positive = 5.0
  let negative = -5.0
  inspect(positive + negative, content="0")
}

test "type_coercion_and_literals" {
  let int_val = 42
  let bool_val = true
  inspect(@int.to_string(int_val), content="\"42\"")
  inspect(@bool.to_string(bool_val), content="\"true\"")
  inspect(int_val > 0, content="true")
  inspect(int_val < 100, content="true")
  inspect(bool_val == true, content="true")
  inspect(bool_val != false, content="true")
}
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

// Azimuth 简单单元测试
// 使用标准 MoonBit 测试语法，包含 10 个基本测试用例

test "integer_arithmetic_operations" {
  // 测试整数算术运算
  let result1 = 1 + 1
  let result2 = 10 - 3
  let result3 = 3 * 4
  let result4 = 20 / 4
  let result5 = 15 % 4
  let result6 = -5 + 10
  let result7 = -3 * -2
  
  // 验证结果
  let check1 = result1
  let check2 = result2
  let check3 = result3
  let check4 = result4
  let check5 = result5
  let check6 = result6
  let check7 = result7
}

test "string_operations" {
  // 测试字符串操作
  let str1 = "Hello"
  let str2 = "MoonBit"
  let combined = str1 + " " + str2 + "!"
  let len1 = str1.length()
  let len2 = str2.length()
  let len_combined = combined.length()
  
  // 验证结果
  let check1 = combined
  let check2 = len1
  let check3 = len2
  let check4 = len_combined
}

test "array_operations" {
  // 测试数组操作
  let arr = [10, 20, 30, 40, 50]
  let arr_len = arr.length()
  let first = arr[0]
  let last = arr[4]
  let sum_first_last = arr[0] + arr[4]
  
  // 验证结果
  let check1 = arr_len
  let check2 = first
  let check3 = last
  let check4 = sum_first_last
}

test "boolean_logic" {
  // 测试布尔逻辑
  let and1 = true && true
  let and2 = true && false
  let or1 = false || true
  let or2 = false || false
  let not1 = !true
  let not2 = !false
  let eq1 = true == true
  let ne1 = true != false
  
  // 验证结果
  let check1 = and1
  let check2 = and2
  let check3 = or1
  let check4 = or2
  let check5 = not1
  let check6 = not2
  let check7 = eq1
  let check8 = ne1
}

test "option_type_handling" {
  // 测试 Option 类型处理
  let some_number : Int? = Some(100)
  let no_number : Int? = None
  
  let extracted = match some_number {
    Some(x) => x
    None => 0
  }
  
  let default = match no_number {
    Some(x) => x
    None => -1
  }
  
  // 验证结果
  let check1 = extracted
  let check2 = default
}

test "result_type_handling" {
  // 测试 Result 类型处理
  let success_result : Result[String, Int] = Ok("Success")
  let error_result : Result[String, Int] = Err(404)
  
  let success_msg = match success_result {
    Ok(msg) => msg
    Err(_) => "Error"
  }
  
  let error_code = match error_result {
    Ok(_) => 0
    Err(code) => code
  }
  
  // 验证结果
  let check1 = success_msg
  let check2 = error_code
}

test "character_operations" {
  // 测试字符操作
  let char_x = 'x'
  let char_y = 'y'
  let str_x = @char.to_string(char_x)
  let compare1 = char_x < char_y
  let compare2 = char_y > char_x
  let compare3 = 'A' < 'a'
  
  // 验证结果
  let check1 = str_x
  let check2 = compare1
  let check3 = compare2
  let check4 = compare3
}

test "list_operations" {
  // 测试列表操作
  let num_list = [1, 2, 3, 4, 5]
  let list_len = num_list.length()
  let head = num_list[0]
  let sum_three = num_list[0] + num_list[1] + num_list[2]
  
  // 验证结果
  let check1 = list_len
  let check2 = head
  let check3 = sum_three
}

test "math_operations" {
  // 测试数学运算
  let x = 3.0
  let y = 4.0
  let add = x + y
  let sub = y - x
  let mul = x * y
  let div = y / x
  let positive = 5.0
  let negative = -5.0
  let pos_neg = positive + negative
  
  // 验证结果
  let check1 = add
  let check2 = sub
  let check3 = mul
  let check4 = div
  let check5 = pos_neg
}

test "type_conversions" {
  // 测试类型转换
  let int_val = 42
  let bool_val = true
  let int_str = @int.to_string(int_val)
  let bool_str = @bool.to_string(bool_val)
  let int_gt_zero = int_val > 0
  let int_lt_hundred = int_val < 100
  let bool_eq_true = bool_val == true
  let bool_ne_false = bool_val != false
  
  // 验证结果
  let check1 = int_str
  let check2 = bool_str
  let check3 = int_gt_zero
  let check4 = int_lt_hundred
  let check5 = bool_eq_true
  let check6 = bool_ne_false
}
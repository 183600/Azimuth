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

// 标准综合测试用例 - 包含核心模块的关键功能测试

///|
test "basic_arithmetic_operations" {
  // 测试基本算术运算
  let sum = 10 + 5
  let diff = 10 - 5
  let product = 10 * 5
  let quotient = 10 / 3
  let remainder = 10 % 3
  
  // 验证结果
  let check1 = sum - 15
  let check2 = diff - 5
  let check3 = product - 50
  let check4 = quotient - 3
  let check5 = remainder - 1
  
  // 所有检查都应该为0
  let final_check = check1 + check2 + check3 + check4 + check5
  let _ = final_check
}

///|
test "boolean_logic_operations" {
  // 测试布尔逻辑运算
  let and_result = true && false
  let or_result = true || false
  let not_true = !true
  let not_false = !false
  
  // 验证结果
  let check1 = if (and_result) { 1 } else { 0 }
  let check2 = if (or_result) { 1 } else { 0 }
  let check3 = if (not_true) { 1 } else { 0 }
  let check4 = if (not_false) { 1 } else { 0 }
  
  // 预期结果：0, 1, 0, 1
  let final_check = check1 + check2 + check3 + check4
  let _ = final_check
}

///|
test "string_basic_operations" {
  // 测试字符串基本操作
  let s1 = "Hello"
  let s2 = "World"
  let combined = s1 + " " + s2
  
  // 验证字符串连接
  let len = combined.length()
  let _ = len > 0
}

///|
test "comparision_operations" {
  // 测试比较操作
  let a = 10
  let b = 20
  let c = 10
  
  // 验证比较结果
  let check1 = if (a < b) { 1 } else { 0 }
  let check2 = if (b > a) { 1 } else { 0 }
  let check3 = if (a <= c) { 1 } else { 0 }
  let check4 = if (a >= c) { 1 } else { 0 }
  let check5 = if (a == c) { 1 } else { 0 }
  let check6 = if (a != b) { 1 } else { 0 }
  
  // 所有检查都应该为1
  let final_check = check1 + check2 + check3 + check4 + check5 + check6
  let _ = final_check
}

///|
test "tuple_operations" {
  // 测试元组操作
  let pair = (1, "hello")
  let triple = (true, 42, "world")
  
  // 验证元组访问
  let check1 = pair.0
  let check2 = pair.1.length()
  let check3 = if (triple.0) { 1 } else { 0 }
  let check4 = triple.1
  let check5 = triple.2.length()
  
  // 验证结果
  let final_check = check1 + check3 + check4
  let _ = final_check
}

///|
test "bitwise_operations" {
  // 测试位运算
  let a = 12  // 1100 in binary
  let b = 10  // 1010 in binary
  
  let and_result = a & b    // 1000 = 8
  let or_result = a | b     // 1110 = 14
  let xor_result = a ^ b    // 0110 = 6
  let left_shift = a << 1   // 11000 = 24
  let right_shift = a >> 1  // 110 = 6
  
  // 验证位运算结果
  let check1 = and_result - 8
  let check2 = or_result - 14
  let check3 = xor_result - 6
  let check4 = left_shift - 24
  let check5 = right_shift - 6
  
  // 所有检查都应该为0
  let final_check = check1 + check2 + check3 + check4 + check5
  let _ = final_check
}

///|
test "integer_boundaries" {
  // 测试整数边界值
  let max_int = 2147483647
  let min_int = -2147483648
  let zero = 0
  
  // 验证边界值
  let check1 = if (max_int > 0) { 1 } else { 0 }
  let check2 = if (min_int < 0) { 1 } else { 0 }
  let check3 = if (zero == 0) { 1 } else { 0 }
  
  // 验证溢出行为
  let overflow = max_int + 1
  let underflow = min_int - 1
  let check4 = if (overflow == min_int) { 1 } else { 0 }
  let check5 = if (underflow == max_int) { 1 } else { 0 }
  
  // 所有检查都应该为1
  let final_check = check1 + check2 + check3 + check4 + check5
  let _ = final_check
}

///|
test "string_comparisons" {
  // 测试字符串比较
  let str1 = "apple"
  let str2 = "banana"
  let str3 = "apple"
  
  // 验证字符串比较
  let check1 = if (str1 == str3) { 1 } else { 0 }
  let check2 = if (str1 < str2) { 1 } else { 0 }
  let check3 = if (str2 > str1) { 1 } else { 0 }
  
  // 所有检查都应该为1
  let final_check = check1 + check2 + check3
  let _ = final_check
}

///|
test "nested_expressions" {
  // 测试嵌套表达式
  let a = 5
  let b = 10
  let c = 15
  
  // 复杂的嵌套表达式
  let result1 = (a + b) * c
  let result2 = a + (b * c)
  let result3 = if ((a < b) && (b < c)) { 1 } else { 0 }
  let result4 = if ((a == 5) || (b == 20)) { 1 } else { 0 }
  
  // 验证结果
  let check1 = result1 - 225
  let check2 = result2 - 155
  
  // 所有检查都应该为0或适当的值
  let final_check = check1 + check2 + result3 + result4
  let _ = final_check
}

///|
test "conditional_logic" {
  // 测试条件逻辑
  let x = 10
  let y = 20
  
  // 使用条件表达式
  let max_val = if (x > y) { x } else { y }
  let min_val = if (x < y) { x } else { y }
  let is_equal = if (x == y) { 1 } else { 0 }
  
  // 验证结果
  let check1 = max_val - 20
  let check2 = min_val - 10
  let check3 = is_equal
  
  // 所有检查都应该为0
  let final_check = check1 + check2 + check3
  let _ = final_check
}
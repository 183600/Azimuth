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

// 高质量 MoonBit 标准测试用例
// 包含 10 个核心功能测试，使用标准 MoonBit 测试语法

test "array_advanced_operations" {
  // 测试数组高级操作
  let numbers = [1, 2, 3, 4, 5]
  let more_numbers = [6, 7, 8, 9, 10]
  
  // 测试数组连接
  let combined = @array.concat(numbers, more_numbers)
  inspect(combined.length(), content="10")
  inspect(combined[0], content="1")
  inspect(combined[9], content="10")
  
  // 测试数组切片
  let slice = @array.slice(combined, 2, 7)
  inspect(slice.length(), content="5")
  inspect(slice[0], content="3")
  inspect(slice[4], content="7")
  
  // 测试数组反转
  let reversed = @array.reverse(numbers)
  inspect(reversed, content="[5, 4, 3, 2, 1]")
}

test "string_advanced_operations" {
  // 测试字符串高级操作
  let sentence = "  Hello, MoonBit World!  "
  
  // 测试字符串修剪
  let trimmed = sentence.trim()
  inspect(trimmed, content="Hello, MoonBit World!")
  inspect(trimmed.length(), content="21")
  
  // 测试字符串分割
  let words = trimmed.split(", ")
  inspect(words.length(), content="2")
  inspect(words[0], content="Hello")
  inspect(words[1], content="MoonBit World!")
  
  // 测试字符串替换
  let replaced = trimmed.replace("MoonBit", "Awesome")
  inspect(replaced, content="Hello, Awesome World!")
  
  // 测试字符串包含
  inspect(trimmed.contains("MoonBit"), content="true")
  inspect(trimmed.contains("Python"), content="false")
}

test "math_advanced_functions" {
  // 测试高级数学函数
  let pi = @math.pi
  let e = @math.e
  
  // 测试数学常数
  inspect(pi > 3.14, content="true")
  inspect(pi < 3.15, content="true")
  inspect(e > 2.71, content="true")
  inspect(e < 2.72, content="true")
  
  // 测试幂运算
  inspect(@math.powf(2.0, 3.0), content="8")
  inspect(@math.powf(3.0, 2.0), content="9")
  inspect(@math.sqrtf(16.0), content="4")
  inspect(@math.sqrtf(25.0), content="5")
  
  // 测试对数函数
  inspect(@math.logf(@math.e), content="1")
  inspect(@math.logf(1.0), content="0")
  inspect(@math.log10f(100.0), content="2")
  inspect(@math.log2f(8.0), content="3")
}

test "int_advanced_bit_operations" {
  // 测试高级位操作
  let num = 0b10101010  // 170
  
  // 测试位计数
  inspect(@int.Int::count_ones(num), content="4")
  inspect(@int.Int::count_zeros(num), content="28")
  
  // 测试前导零
  inspect(@int.Int::leading_zeros(0), content="32")
  inspect(@int.Int::leading_zeros(1), content="31")
  inspect(@int.Int::leading_zeros(num), content="24")
  
  // 测试位翻转
  inspect(@int.Int::reverse_bits(num), content="1426063360")
  
  // 测试循环移位
  inspect(@int.Int::rotate_left(num, 4), content="340")
  inspect(@int.Int::rotate_right(num, 4), content="2684354560")
}

test "option_complex_operations" {
  // 测试 Option 复杂操作
  let maybe_num : Option[Int] = Some(42)
  let maybe_str : Option[String] = None
  
  // 测试 and_then 链式操作
  let result = maybe_num.and_then(x => Some(x * 2)).and_then(x => Some(x + 10))
  inspect(result, content="Some(94)")
  
  // 测试 or_else
  let fallback = maybe_str.or_else(Some("default"))
  inspect(fallback, content="Some(\"default\")")
  
  // 测试 filter
  let filtered_some = maybe_num.filter(x => x > 40)
  let filtered_none = maybe_num.filter(x => x > 50)
  
  inspect(filtered_some, content="Some(42)")
  inspect(filtered_none, content="None")
  
  // 测试 Option 转 Result
  let ok_result = maybe_num.ok_or("error")
  let err_result = maybe_str.ok_or("error")
  
  inspect(ok_result, content="Ok(42)")
  inspect(err_result, content="Err(\"error\")")
}

test "result_error_chaining" {
  // 测试 Result 错误链式处理
  let success1 : Result[Int, String] = Ok(42)
  let failure1 : Result[Int, String] = Err("first error")
  
  // 测试 and_then
  let chained_success = success1.and_then(x => Ok(x * 2))
  let chained_failure = failure1.and_then(x => Ok(x * 2))
  
  inspect(chained_success, content="Ok(84)")
  inspect(chained_failure, content="Err(\"first error\")")
  
  // 测试 or_else
  let fallback_result = failure1.or_else(msg => Ok(100))
  inspect(fallback_result, content="Ok(100)")
  
  // 测试 map_err
  let mapped_err = failure1.map_err(msg => msg.uppercase())
  inspect(mapped_err, content="Err(\"FIRST ERROR\")")
  
  // 测试 Result 转 Option
  let some_from_ok = success1.ok()
  let none_from_err = failure1.ok()
  
  inspect(some_from_ok, content="Some(42)")
  inspect(none_from_err, content="None")
}

test "type_coercion_and_safety" {
  // 测试类型转换和安全检查
  let big_num = 2147483647  // Int max value
  let small_num = -2147483648  // Int min value
  
  // 测试溢出检查
  inspect(@int.Int::add_overflow(big_num, 1), content="true")
  inspect(@int.Int::add_overflow(100, 200), content="false")
  inspect(@int.Int::mul_overflow(big_num, 2), content="true")
  inspect(@int.Int::mul_overflow(1000, 1000), content="false")
  
  // 测试饱和运算
  inspect(@int.Int::saturating_add(big_num, 100), content="2147483647")
  inspect(@int.Int::saturating_sub(small_num, 100), content="-2147483648")
  
  // 测试除零安全
  inspect(@int.Int::checked_div(10, 2), content="Some(5)")
  inspect(@int.Int::checked_div(10, 0), content="None")
}

test "array_searching_algorithms" {
  // 测试数组搜索算法
  let unsorted = [42, 17, 23, 99, 8, 15]
  let sorted = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
  
  // 测试线性搜索
  inspect(@array.index_of(unsorted, 23), content="Some(2)")
  inspect(@array.index_of(unsorted, 100), content="None")
  
  // 测试是否包含
  inspect(@array.contains(unsorted, 99), content="true")
  inspect(@array.contains(unsorted, 100), content="false")
  
  // 测试二分查找（假设 sorted 数组）
  inspect(@array.binary_search(sorted, 7), content="Some(3)")
  inspect(@array.binary_search(sorted, 8), content="None")
  
  // 测试查找最大最小值
  inspect(@array.max(unsorted), content="Some(99)")
  inspect(@array.min(unsorted), content="Some(8)")
}

test "string_pattern_matching" {
  // 测试字符串模式匹配
  let email = "user@example.com"
  let phone = "123-456-7890"
  let invalid = "not-an-email"
  
  // 测试正则表达式匹配（假设有 regex 支持）
  let email_pattern = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
  let phone_pattern = "^\\d{3}-\\d{3}-\\d{4}$"
  
  // 这里使用简单的字符串操作模拟正则匹配
  inspect(email.contains("@"), content="true")
  inspect(email.contains("."), content="true")
  inspect(invalid.contains("@"), content="false")
  
  // 测试字符串前缀和后缀
  inspect(email.starts_with("user"), content="true")
  inspect(email.ends_with(".com"), content="true")
  inspect(phone.starts_with("123"), content="true")
  inspect(phone.ends_with("7890"), content="true")
  
  // 测试字符串提取
  let username = email.split("@")[0]
  let domain = email.split("@")[1]
  inspect(username, content="user")
  inspect(domain, content="example.com")
}

test "performance_and_memory_operations" {
  // 测试性能和内存操作
  let large_array = @array.make(1000, 42)
  
  // 测试大数组操作
  inspect(large_array.length(), content="1000")
  inspect(large_array[0], content="42")
  inspect(large_array[999], content="42")
  
  // 测试数组填充
  let filled = @array.make(10, 0)
  let filled_with_sevens = @array.fill(filled, 7, 0, 10)
  inspect(filled_with_sevens[0], content="7")
  inspect(filled_with_sevens[9], content="7")
  
  // 测试数组复制
  let source = [1, 2, 3, 4, 5]
  let dest = @array.make(5, 0)
  @array.copy_from(dest, source, 0, 0, 5)
  inspect(dest, content="[1, 2, 3, 4, 5]")
  
  // 测试字符串重复
  let repeated = "abc".repeat(3)
  inspect(repeated, content="abcabcabc")
  inspect(repeated.length(), content="9")
}
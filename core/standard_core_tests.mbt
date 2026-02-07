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

// Azimuth 项目高质量 MoonBit 测试用例
// 包含8个标准测试用例，验证核心功能的正确性和边界条件

///|
test "int basic operations" {
  // 整数基本操作测试
  let a = 15
  let b = 7
  
  assert_eq(a + b, 22)
  assert_eq(a - b, 8)
  assert_eq(a * b, 105)
  assert_eq(a / b, 2)
  assert_eq(a % b, 1)
  
  // 绝对值测试
  assert_eq(Int::abs(42), 42)
  assert_eq(Int::abs(-42), 42)
  assert_eq(Int::abs(0), 0)
}

///|
test "string basic operations" {
  // 字符串基本操作测试
  let s1 = "Hello"
  let s2 = "MoonBit"
  let s3 = s1 + " " + s2
  
  assert_eq(s3, "Hello MoonBit")
  assert_eq(s1.length(), 5)
  assert_eq(s2.length(), 7)
  assert_eq(s3.length(), 12)
  
  // 空字符串测试
  let empty = ""
  assert_eq(empty.length(), 0)
  assert_eq(empty == "", true)
}

///|
test "list basic operations" {
  // 列表基本操作测试
  let empty : @list.List[Int] = @list.empty()
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  let doubled = numbers.map(x => x * 2)
  let evens = numbers.filter(x => x % 2 == 0)
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  
  assert_eq(empty.length(), 0)
  assert_eq(numbers.length(), 5)
  assert_eq(doubled, @list.from_array([2, 4, 6, 8, 10]))
  assert_eq(evens, @list.from_array([2, 4]))
  assert_eq(sum, 15)
}

///|
test "bool logical operations" {
  // 布尔逻辑运算测试
  let t = true
  let f = false
  
  assert_eq(t && t, true)
  assert_eq(t && f, false)
  assert_eq(f && t, false)
  assert_eq(f && f, false)
  
  assert_eq(t || t, true)
  assert_eq(t || f, true)
  assert_eq(f || t, true)
  assert_eq(f || f, false)
  
  assert_eq(!t, false)
  assert_eq(!f, true)
}

///|
test "option type operations" {
  // Option 类型操作测试
  let some_value = Some(42)
  let none_value : Option[Int] = None
  
  assert_eq(some_value.is_some(), true)
  assert_eq(some_value.is_none(), false)
  assert_eq(none_value.is_some(), false)
  assert_eq(none_value.is_none(), true)
  
  // Option 映射
  let mapped = some_value.map(x => x * 2)
  assert_eq(mapped, Some(84))
  
  let none_mapped = none_value.map(x => x * 2)
  assert_eq(none_mapped, None)
}

///|
test "result type operations" {
  // Result 类型操作测试
  let success = Ok(42)
  let error : Result[Int, String] = Err("error")
  
  assert_eq(success.is_ok(), true)
  assert_eq(success.is_err(), false)
  assert_eq(error.is_ok(), false)
  assert_eq(error.is_err(), true)
  
  // Result 映射
  let mapped_ok = success.map(x => x * 2)
  assert_eq(mapped_ok, Ok(84))
  
  let mapped_err = error.map(x => x * 2)
  assert_eq(mapped_err, Err("error"))
}

///|
test "array operations" {
  // 数组操作测试
  let numbers = [1, 2, 3, 4, 5]
  let first = numbers[0]
  let last = numbers[numbers.length() - 1]
  let length = numbers.length()
  
  assert_eq(first, 1)
  assert_eq(last, 5)
  assert_eq(length, 5)
  
  // 数组切片
  let slice = numbers.slice(1, 4)
  assert_eq(slice, [2, 3, 4])
  
  // 数组包含
  let contains_3 = numbers.contains(3)
  let contains_6 = numbers.contains(6)
  assert_eq(contains_3, true)
  assert_eq(contains_6, false)
}

///|
test "complex calculation scenario" {
  // 复杂计算场景测试
  // 场景：计算购物车总价
  let prices = @list.from_array([10, 20, 30, 40, 50])
  let quantities = @list.from_array([2, 1, 3, 1, 2])
  
  // 计算每个商品的总价
  let item_totals = prices.zip(quantities).map((p, q) => p * q)
  
  // 计算购物车总价
  let cart_total = item_totals.fold(init=0, (acc, x) => acc + x)
  
  // 应用折扣
  let discounted_total = if cart_total > 200 { cart_total * 9 / 10 } else { cart_total }
  
  // 计算税费（假设税率为10%）
  let tax = discounted_total / 10
  let final_total = discounted_total + tax
  
  assert_eq(cart_total, 250)  // 10*2 + 20*1 + 30*3 + 40*1 + 50*2 = 250
  assert_eq(discounted_total, 225)  // 250 * 0.9 = 225
  assert_eq(tax, 22)  // 225 / 10 = 22.5 取整为 22
  assert_eq(final_total, 247)  // 225 + 22 = 247
}
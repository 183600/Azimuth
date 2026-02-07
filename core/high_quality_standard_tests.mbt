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
test "int edge cases and boundaries" {
  // 整数边界情况测试
  assert_eq(Int::max_value, 2147483647)
  assert_eq(Int::min_value, -2147483648)
  
  // 绝对值测试
  assert_eq(Int::abs(0), 0)
  assert_eq(Int::abs(42), 42)
  assert_eq(Int::abs(-42), 42)
  assert_eq(Int::abs(Int::min_value), Int::max_value + 1) // 溢出情况
  
  // 零值运算
  assert_eq(0 + 5, 5)
  assert_eq(0 * 42, 0)
  assert_eq(42 - 0, 42)
  assert_eq(0 / 1, 0)
  
  // 比较运算
  assert_eq(5 > 3, true)
  assert_eq(3 >= 3, true)
  assert_eq(2 < 4, true)
  assert_eq(4 <= 4, true)
  assert_eq(7 == 7, true)
  assert_eq(7 != 5, true)
}

///|
test "list advanced operations" {
  // 列表高级操作测试
  let numbers = @list.from_array([5, 2, 8, 1, 9, 3])
  
  // 排序和查找
  let sorted = numbers.sort()
  assert_eq(sorted, @list.from_array([1, 2, 3, 5, 8, 9]))
  
  assert_eq(numbers.contains(8), true)
  assert_eq(numbers.contains(7), false)
  
  // 获取极值
  assert_eq(numbers.maximum(), Some(9))
  assert_eq(numbers.minimum(), Some(1))
  let empty : @list.List[Int] = @list.empty()
  assert_eq(empty.maximum(), None)
  assert_eq(empty.minimum(), None)
  
  // 列表连接和反转
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  let combined = list1 + list2
  assert_eq(combined, @list.from_array([1, 2, 3, 4, 5, 6]))
  
  let reversed = combined.rev()
  assert_eq(reversed, @list.from_array([6, 5, 4, 3, 2, 1]))
}

///|
test "string manipulation and comparison" {
  // 字符串操作和比较测试
  let text = "MoonBit Programming"
  let empty = ""
  
  // 字符串连接
  let greeting = "Hello" + " " + "World"
  assert_eq(greeting, "Hello World")
  
  // 字符串比较
  assert_eq(text == "MoonBit Programming", true)
  assert_eq(text != "moonbit programming", true) // 大小写敏感
  assert_eq(empty == "", true)
  
  // 字符串长度
  assert_eq(text.length(), 19)
  assert_eq(empty.length(), 0)
  
  // 子字符串测试
  let prefix = "MoonBit"
  let suffix = "Programming"
  assert_eq(text.starts_with(prefix), true)
  assert_eq(text.ends_with(suffix), true)
  assert_eq(text.starts_with("Hello"), false)
  assert_eq(text.ends_with("World"), false)
}

///|
test "pattern matching with lists" {
  // 列表模式匹配测试
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // 头部和尾部
  let head_option = numbers.head()
  let tail = numbers.unsafe_tail()
  
  assert_eq(head_option, Some(1))
  assert_eq(tail, @list.from_array([2, 3, 4, 5]))
  
  // 获取第n个元素
  let first = numbers.nth(0)
  let third = numbers.nth(2)
  let out_of_bounds = numbers.nth(10)
  
  assert_eq(first, Some(1))
  assert_eq(third, Some(3))
  assert_eq(out_of_bounds, None)
  
  // 列表分解
  match numbers {
    Empty => assert_eq(false, true) // 不应该执行
    More(x, xs) => {
      assert_eq(x, 1)
      assert_eq(xs.length(), 4)
    }
  }
}

///|
test "functional programming patterns" {
  // 函数式编程模式测试
  let numbers = @list.from_array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  
  // 链式操作
  let result = numbers
    .filter(x => x % 2 == 0)
    .map(x => x * x)
    .filter(x => x > 10)
    .fold(init=0, (acc, x) => acc + x)
  
  // 偶数的平方：4, 16, 36, 64, 100
  // 大于10的：16, 36, 64, 100
  // 求和：216
  assert_eq(result, 216)
  
  // 扁平映射
  let nested = @list.from_array([
    @list.from_array([1, 2]),
    @list.from_array([3, 4]),
    @list.from_array([5, 6])
  ])
  let flattened = nested.flatten()
  assert_eq(flattened, @list.from_array([1, 2, 3, 4, 5, 6]))
  
  // 带索引的映射
  let indexed = numbers.mapi((i, x) => x + i)
  assert_eq(indexed, @list.from_array([1, 3, 5, 7, 9, 11, 13, 15, 17, 19]))
}

///|
test "data transformation scenarios" {
  // 数据转换场景测试
  // 场景：学生成绩统计
  let students = @list.from_array([
    ("Alice", 85),
    ("Bob", 92),
    ("Charlie", 78),
    ("Diana", 96),
    ("Eve", 88)
  ])
  
  // 分离姓名和成绩
  let (names, scores) = students.unzip()
  
  // 计算平均分
  let total_score = scores.fold(init=0, (acc, x) => acc + x)
  let average_score = total_score / scores.length()
  
  // 找出最高分和最低分
  let max_score = scores.unsafe_maximum()
  let min_score = scores.unsafe_minimum()
  
  // 筛选优秀学生（分数 >= 90）
  let excellent_students = students.filter((name, score) => score >= 90)
  let excellent_names = excellent_students.map((name, score) => name)
  
  assert_eq(names, @list.from_array(["Alice", "Bob", "Charlie", "Diana", "Eve"]))
  assert_eq(scores, @list.from_array([85, 92, 78, 96, 88]))
  assert_eq(total_score, 439)
  assert_eq(average_score, 87) // 439 / 5 = 87.8 取整为 87
  assert_eq(max_score, 96)
  assert_eq(min_score, 78)
  assert_eq(excellent_names, @list.from_array(["Bob", "Diana"]))
}

///|
test "error handling and validation" {
  // 错误处理和验证测试
  // 场景：输入验证和处理
  
  let validate_positive = fn(x : Int) -> Result[String, String] {
    if x > 0 {
      Ok("Valid positive number: " + x.to_string())
    } else {
      Err("Number must be positive")
    }
  }
  
  // 测试有效输入
  let valid_result = validate_positive(42)
  assert_eq(valid_result.is_ok(), true)
  assert_eq(valid_result.is_err(), false)
  
  match valid_result {
    Ok(msg) => assert_eq(msg, "Valid positive number: 42")
    Err(_) => assert_eq(false, true) // 不应该执行
  }
  
  // 测试无效输入
  let invalid_result = validate_positive(-5)
  assert_eq(invalid_result.is_ok(), false)
  assert_eq(invalid_result.is_err(), true)
  
  match invalid_result {
    Ok(_) => assert_eq(false, true) // 不应该执行
    Err(msg) => assert_eq(msg, "Number must be positive")
  }
  
  // Result 链式操作
  let chained = validate_positive(10).map(msg => msg.length())
  assert_eq(chained, Ok(25))
  
  let error_chained = validate_positive(0).map(msg => msg.length())
  assert_eq(error_chained, Err("Number must be positive"))
}

///|
test "performance and efficiency patterns" {
  // 性能和效率模式测试
  // 场景：大数据集处理
  
  // 生成大列表
  let large_list = @list.repeat(1000, 1)
  assert_eq(large_list.length(), 1000)
  
  // 高效求和（使用fold而不是循环）
  let sum = large_list.fold(init=0, (acc, x) => acc + x)
  assert_eq(sum, 1000)
  
  // 高效过滤和映射
  let processed = large_list
    .map(x => x * 2)
    .filter(x => x % 4 == 0)
    .take(100)
  
  assert_eq(processed.length(), 100)
  
  // 列表连接效率测试
  let small_lists = @list.from_array([
    @list.from_array([1, 2]),
    @list.from_array([3, 4]),
    @list.from_array([5, 6])
  ])
  
  // 使用fold进行高效连接
  let concatenated = small_lists.fold(init=@list.empty(), (acc, lst) => acc + lst)
  assert_eq(concatenated, @list.from_array([1, 2, 3, 4, 5, 6]))
  
  // 短路求值测试
  let has_large = large_list.any(x => x > 500)
  assert_eq(has_large, false)
  
  let all_positive = large_list.all(x => x > 0)
  assert_eq(all_positive, true)
}
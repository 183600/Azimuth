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

// 标准 MoonBit 综合测试用例
// 包含10个高质量测试用例，涵盖多个核心模块的功能

///|
test "integer boundary values" {
  // 整数边界值测试
  assert_eq(Int::max_value, 2147483647)
  assert_eq(Int::min_value, -2147483648)
  
  // 边界值运算
  let max_val = Int::max_value
  let min_val = Int::min_value
  
  assert_eq(max_val - 1, 2147483646)
  assert_eq(min_val + 1, -2147483647)
  
  // 零值测试
  assert_eq(0 + 0, 0)
  assert_eq(0 * 42, 0)
  assert_eq(42 * 0, 0)
}

///|
test "string manipulation functions" {
  // 字符串操作函数测试
  let original = "MoonBit Programming"
  let upper = original.to_upper_case()
  let lower = original.to_lower_case()
  
  assert_eq(upper, "MOONBIT PROGRAMMING")
  assert_eq(lower, "moonbit programming")
  
  // 字符串比较
  assert_eq(original == "MoonBit Programming", true)
  assert_eq(original != "moonbit programming", true)
  
  // 字符串长度
  assert_eq(original.length(), 19)
  assert_eq("".length(), 0)
  assert_eq("a".length(), 1)
}

///|
test "list transformation operations" {
  // 列表转换操作测试
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // 映射操作
  let squared = numbers.map(x => x * x)
  assert_eq(squared, @list.from_array([1, 4, 9, 16, 25]))
  
  // 过滤操作
  let evens = numbers.filter(x => x % 2 == 0)
  assert_eq(evens, @list.from_array([2, 4]))
  
  // 扁平化映射
  let nested = numbers.map(x => @list.from_array([x, x * 2]))
  let flattened = nested.fold(init=@list.empty[Int](), (acc, lst) => acc.append(lst))
  assert_eq(flattened, @list.from_array([1, 2, 2, 4, 3, 6, 4, 8, 5, 10]))
}

///|
test "option type pattern matching" {
  // Option 类型模式匹配测试
  let some_value = Some(42)
  let none_value : Option[Int] = None
  
  // 处理 Some 值
  let some_result = match some_value {
    Some(x) => x * 2
    None => 0
  }
  assert_eq(some_result, 84)
  
  // 处理 None 值
  let none_result = match none_value {
    Some(x) => x * 2
    None => 0
  }
  assert_eq(none_result, 0)
  
  // 链式操作
  let chained = Some(10)
    .map(x => x + 5)
    .filter(x => x > 10)
    .map(x => x * 2)
  assert_eq(chained, Some(30))
}

///|
test "result type error handling" {
  // Result 类型错误处理测试
  let success = Ok("success")
  let error = Err("failure")
  
  // 处理成功结果
  let success_message = match success {
    Ok(msg) => "Operation " + msg
    Err(err) => "Error: " + err
  }
  assert_eq(success_message, "Operation success")
  
  // 处理错误结果
  let error_message = match error {
    Ok(msg) => "Operation " + msg
    Err(err) => "Error: " + err
  }
  assert_eq(error_message, "Error: failure")
  
  // Result 链式操作
  let chained = Ok(10)
    .map(x => x * 2)
    .map_err(err => err + " (modified)")
  assert_eq(chained, Ok(20))
  
  let error_chained = Err("original error")
    .map(x => x * 2)
    .map_err(err => err + " (modified)")
  assert_eq(error_chained, Err("original error (modified)"))
}

///|
test "array indexing and slicing" {
  // 数组索引和切片测试
  let numbers = [10, 20, 30, 40, 50]
  
  // 索引访问
  assert_eq(numbers[0], 10)
  assert_eq(numbers[2], 30)
  assert_eq(numbers[4], 50)
  
  // 数组长度
  assert_eq(numbers.length(), 5)
  
  // 切片操作
  let first_three = numbers.slice(0, 3)
  assert_eq(first_three, [10, 20, 30])
  
  let middle_two = numbers.slice(1, 3)
  assert_eq(middle_two, [20, 30])
  
  let last_two = numbers.slice(3, 5)
  assert_eq(last_two, [40, 50])
}

///|
test "boolean truth table" {
  // 布尔真值表测试
  let t = true
  let f = false
  
  // AND 运算真值表
  assert_eq(t && t, true)
  assert_eq(t && f, false)
  assert_eq(f && t, false)
  assert_eq(f && f, false)
  
  // OR 运算真值表
  assert_eq(t || t, true)
  assert_eq(t || f, true)
  assert_eq(f || t, true)
  assert_eq(f || f, false)
  
  // NOT 运算
  assert_eq(!t, false)
  assert_eq(!f, true)
  
  // 复合表达式
  assert_eq((t && f) || (t || f), true)
  assert_eq(!t && !f, false)
  assert_eq(!t || !f, true)
}

///|
test "character operations" {
  // 字符操作测试
  let char_a = 'a'
  let char_A = 'A'
  let char_z = 'z'
  let char_Z = 'Z'
  let char_0 = '0'
  let char_9 = '9'
  
  // 字符比较
  assert_eq(char_a < char_b, true)
  assert_eq(char_A < char_B, true)
  assert_eq(char_0 < char_1, true)
  
  // 字符转换
  let a_code = char_a.to_int()
  let A_code = char_A.to_int()
  assert_eq(A_code, a_code - 32)  // ASCII 大小写关系
  
  // 数字字符
  assert_eq(char_0.to_int(), 48)
  assert_eq(char_9.to_int(), 57)
  
  // 字符判断
  assert_eq(char_a.is_alphabetic(), true)
  assert_eq(char_A.is_alphabetic(), true)
  assert_eq(char_0.is_digit(), true)
  assert_eq(char_9.is_digit(), true)
}

///|
test "mathematical functions" {
  // 数学函数测试
  let numbers = [1, 2, 3, 4, 5]
  
  // 最大值和最小值
  assert_eq(@list.max(numbers), Some(5))
  assert_eq(@list.min(numbers), Some(1))
  
  // 空列表的最大最小值
  let empty : @list.List[Int] = @list.empty()
  assert_eq(@list.max(empty), None)
  assert_eq(@list.min(empty), None)
  
  // 数值比较
  assert_eq(@max(3, 7), 7)
  assert_eq(@max(10, 2), 10)
  assert_eq(@min(3, 7), 3)
  assert_eq(@min(10, 2), 2)
  
  // 绝对值
  assert_eq(Int::abs(5), 5)
  assert_eq(Int::abs(-5), 5)
  assert_eq(Int::abs(0), 0)
}

///|
test "complex data processing" {
  // 复杂数据处理测试
  // 场景：处理学生成绩数据
  
  // 学生数据：姓名和成绩
  let students = @list.from_array([
    ("Alice", 85),
    ("Bob", 92),
    ("Charlie", 78),
    ("Diana", 95),
    ("Eve", 88)
  ])
  
  // 过滤出高分学生（成绩 >= 90）
  let high_achievers = students
    .filter((_, score) => score >= 90)
    .map((name, score) => name)
  
  assert_eq(high_achievers, @list.from_array(["Bob", "Diana"]))
  
  // 计算平均分
  let total_score = students
    .map((_, score) => score)
    .fold(init=0, (acc, score) => acc + score)
  
  let average = total_score / students.length()
  assert_eq(average, 87)  // (85+92+78+95+88)/5 = 438/5 = 87.6 取整为 87
  
  // 找出最高分学生
  let top_student = students
    .fold(init=("None", 0), (acc, student) => {
      let (_, acc_score) = acc
      let (_, student_score) = student
      if student_score > acc_score { student } else { acc }
    })
  
  assert_eq(top_student, ("Diana", 95))
  
  // 按成绩分组
  let grades = students.map((name, score) => {
    let grade = if score >= 90 { "A" }
                else if score >= 80 { "B" }
                else if score >= 70 { "C" }
                else { "F" }
    (name, grade)
  })
  
  let a_grades = grades
    .filter((_, grade) => grade == "A")
    .map((name, _) => name)
  
  assert_eq(a_grades, @list.from_array(["Bob", "Diana"]))
}
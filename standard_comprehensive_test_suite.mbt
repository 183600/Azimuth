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

// 标准 MoonBit 综合测试套件
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

///|
test "integer_boundary_values" {
  // 整数边界值测试
  let max_int = @int.max_value
  let min_int = @int.min_value
  
  assert_eq(max_int, 2147483647)
  assert_eq(min_int, -2147483648)
  
  // 边界值运算
  assert_eq(max_int + min_int, -1)
  assert_eq(max_int - max_int, 0)
  assert_eq(min_int - min_int, 0)
  
  // 零值测试
  assert_eq(0 + max_int, max_int)
  assert_eq(0 + min_int, min_int)
  assert_eq(0 * max_int, 0)
  assert_eq(0 * min_int, 0)
}

///|
test "string_manipulation_operations" {
  // 字符串操作测试
  let base = "MoonBit"
  let greeting = "Hello, " + base + "!"
  let repeated = base * 3
  
  assert_eq(greeting, "Hello, MoonBit!")
  assert_eq(repeated, "MoonBitMoonBitMoonBit")
  assert_eq(base.length(), 7)
  assert_eq(greeting.length(), 14)
  
  // 字符串比较
  assert_eq(base == "MoonBit", true)
  assert_eq(base != "moonbit", true)  // 大小写敏感
  assert_eq(base > "Apple", true)
  assert_eq(base < "Python", true)
  
  // 空字符串处理
  let empty = ""
  assert_eq(empty.length(), 0)
  assert_eq(empty == "", true)
  assert_eq(base + empty, base)
}

///|
test "array_comprehensive_operations" {
  // 数组综合操作测试
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let evens = numbers.filter(x => x % 2 == 0)
  let odds = numbers.filter(x => x % 2 != 0)
  let doubled = numbers.map(x => x * 2)
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  
  assert_eq(evens, [2, 4, 6, 8, 10])
  assert_eq(odds, [1, 3, 5, 7, 9])
  assert_eq(doubled, [2, 4, 6, 8, 10, 12, 14, 16, 18, 20])
  assert_eq(sum, 55)
  
  // 数组切片和索引
  let first_five = numbers.slice(0, 5)
  let last_five = numbers.slice(5, 10)
  assert_eq(first_five, [1, 2, 3, 4, 5])
  assert_eq(last_five, [6, 7, 8, 9, 10])
  
  // 数组查找
  assert_eq(numbers.contains(5), true)
  assert_eq(numbers.contains(11), false)
  assert_eq(numbers.index_of(7), 6)
}

///|
test "list_functional_operations" {
  // 列表函数式操作测试
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  let strings = @list.from_array(["apple", "banana", "cherry"])
  
  // 列表映射和过滤
  let squared = numbers.map(x => x * x)
  let filtered = numbers.filter(x => x > 3)
  let lengths = strings.map(s => s.length())
  
  assert_eq(squared, @list.from_array([1, 4, 9, 16, 25]))
  assert_eq(filtered, @list.from_array([4, 5]))
  assert_eq(lengths, @list.from_array([5, 6, 6]))
  
  // 列表归约
  let product = numbers.fold(init=1, (acc, x) => acc * x)
  let concatenated = strings.fold(init="", (acc, s) => acc + s)
  
  assert_eq(product, 120)
  assert_eq(concatenated, "applebananacherry")
  
  // 列表连接
  let combined = numbers.append(@list.from_array([6, 7, 8]))
  assert_eq(combined, @list.from_array([1, 2, 3, 4, 5, 6, 7, 8]))
}

///|
test "option_type_pattern_matching" {
  // Option 类型模式匹配测试
  let some_number : Option[Int] = Some(42)
  let none_number : Option[Int] = None
  let some_string : Option[String] = Some("test")
  let none_string : Option[String] = None
  
  // Option 值提取
  let extracted_number = match some_number {
    Some(n) => n
    None => 0
  }
  assert_eq(extracted_number, 42)
  
  let default_number = match none_number {
    Some(n) => n
    None => -1
  }
  assert_eq(default_number, -1)
  
  // Option 链式操作
  let result = some_number
    .map(x => x * 2)
    .filter(x => x > 50)
    .map(x => x / 2)
  
  assert_eq(result, Some(42))
  
  let empty_result = none_number
    .map(x => x * 2)
    .filter(x => x > 50)
  
  assert_eq(empty_result, None)
}

///|
test "result_type_error_handling" {
  // Result 类型错误处理测试
  let success_int : Result[Int, String] = Ok(100)
  let error_int : Result[Int, String] = Err("division error")
  let success_str : Result[String, Int] = Ok("success")
  let error_str : Result[String, Int] = Err(404)
  
  // Result 值提取
  let success_value = match success_int {
    Ok(n) => n
    Err(_) => 0
  }
  assert_eq(success_value, 100)
  
  let error_value = match error_int {
    Ok(n) => n
    Err(msg) => -1
  }
  assert_eq(error_value, -1)
  
  // Result 映射操作
  let mapped_success = success_int.map(x => x / 2)
  assert_eq(mapped_success, Ok(50))
  
  let mapped_error = error_int.map(x => x / 2)
  assert_eq(mapped_error, Err("division error"))
  
  // Result 错误映射
  let error_mapped = error_str.map_err(code => code + 1)
  assert_eq(error_mapped, Err(405))
}

///|
test "boolean_logic_and_truth_table" {
  // 布尔逻辑和真值表测试
  let t = true
  let f = false
  
  // 基本逻辑运算
  assert_eq(t && t, t)
  assert_eq(t && f, f)
  assert_eq(f && t, f)
  assert_eq(f && f, f)
  
  assert_eq(t || t, t)
  assert_eq(t || f, t)
  assert_eq(f || t, t)
  assert_eq(f || f, f)
  
  assert_eq(!t, f)
  assert_eq(!f, t)
  
  // 复合逻辑表达式
  assert_eq((t && f) || (t || f), t)
  assert_eq((f || f) && (t && t), f)
  assert_eq(!(t && f) && (t || f), t)
  
  // 布尔比较
  assert_eq(t == t, t)
  assert_eq(t == f, f)
  assert_eq(t != f, t)
  assert_eq(f != f, f)
}

///|
test "complex_data_processing_scenario" {
  // 复杂数据处理场景测试
  // 场景：处理学生成绩数据
  
  // 学生数据：[姓名, 数学成绩, 英语成绩, 科学成绩]
  let student_data = [
    ("Alice", 85, 92, 88),
    ("Bob", 76, 84, 79),
    ("Charlie", 92, 89, 95),
    ("Diana", 88, 91, 85),
    ("Eve", 79, 82, 78)
  ]
  
  // 计算每个学生的平均分
  let averages = student_data.map((name, math, english, science) => {
    let total = math + english + science
    (name, total / 3)
  })
  
  // 找出平均分最高的学生
  let top_student = averages.fold(init=("None", 0), (best, current) => {
    let (_, best_score) = best
    let (_, current_score) = current
    if current_score > best_score { current } else { best }
  })
  
  // 计算班级各科平均分
  let math_avg = student_data.fold(init=0, (acc, (_, math, _, _)) => acc + math) / student_data.length()
  let english_avg = student_data.fold(init=0, (acc, (_, _, english, _)) => acc + english) / student_data.length()
  let science_avg = student_data.fold(init=0, (acc, (_, _, _, science)) => acc + science) / student_data.length()
  
  // 筛选平均分超过85分的学生
  let excellent_students = averages.filter((name, avg) => avg >= 85)
  
  // 验证结果
  assert_eq(top_student, ("Charlie", 92))  // Charlie: (92+89+95)/3 = 92
  assert_eq(math_avg, 84)  // (85+76+92+88+79)/5 = 84
  assert_eq(english_avg, 87)  // (92+84+89+91+82)/5 = 87.6 -> 87
  assert_eq(science_avg, 85)  // (88+79+95+85+78)/5 = 85
  assert_eq(excellent_students.length(), 2)  // Alice(88.3) 和 Charlie(92)
}
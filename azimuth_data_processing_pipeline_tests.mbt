// Azimuth 数据处理管道测试
// 专注于测试数据处理管道的各种转换和操作

// 测试1: 数据过滤和转换管道
test "数据过滤和转换管道" {
  // 创建测试数据集
  let input_data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // 定义过滤函数：只保留偶数
  let filter_even = fn(x : Int) -> Bool { x % 2 == 0 }
  
  // 定义转换函数：将数字平方
  let square = fn(x : Int) -> Int { x * x }
  
  // 应用过滤
  let filtered_data = input_data.filter(filter_even)
  assert_eq(filtered_data.length(), 5)
  assert_eq(filtered_data, [2, 4, 6, 8, 10])
  
  // 应用转换
  let transformed_data = filtered_data.map(square)
  assert_eq(transformed_data.length(), 5)
  assert_eq(transformed_data, [4, 16, 36, 64, 100])
  
  // 验证管道操作的组合
  let pipeline_result = input_data
    .filter(filter_even)
    .map(square)
    .reduce(fn(acc, x) { acc + x }, 0)
  
  assert_eq(pipeline_result, 220)
}

// 测试2: 数据聚合操作
test "数据聚合操作" {
  // 创建数值数据集
  let numeric_data = [10, 20, 30, 40, 50]
  
  // 计算平均值
  let sum = numeric_data.reduce(fn(acc, x) { acc + x }, 0)
  let average = sum / numeric_data.length()
  assert_eq(average, 30)
  
  // 查找最大值和最小值
  let max_value = numeric_data.reduce(fn(acc, x) { if x > acc { x } else { acc } }, numeric_data[0])
  let min_value = numeric_data.reduce(fn(acc, x) { if x < acc { x } else { acc } }, numeric_data[0])
  
  assert_eq(max_value, 50)
  assert_eq(min_value, 10)
  
  // 计算标准差
  let variance = numeric_data
    .map(fn(x) { 
      let diff = x - average
      diff * diff 
    })
    .reduce(fn(acc, x) { acc + x }, 0) / numeric_data.length()
  
  assert_eq(variance, 200)
}

// 测试3: 字符串数据处理
test "字符串数据处理" {
  // 创建字符串数据集
  let string_data = ["apple", "banana", "cherry", "date", "elderberry"]
  
  // 过滤长度大于5的字符串
  let long_strings = string_data.filter(fn(s) { s.length() > 5 })
  assert_eq(long_strings.length(), 2)
  assert_eq(long_strings, ["banana", "elderberry"])
  
  // 转换为大写
  let uppercased = string_data.map(fn(s) { s.to_uppercase() })
  assert_eq(uppercased, ["APPLE", "BANANA", "CHERRY", "DATE", "ELDERBERRY"])
  
  // 字符串连接
  let joined = string_data.reduce(fn(acc, s) { acc + ", " + s }, "")
  assert_eq(joined, ", apple, banana, cherry, date, elderberry")
  
  // 提取首字母
  let initials = string_data.map(fn(s) { s[0].to_string() })
  assert_eq(initials, ["a", "b", "c", "d", "e"])
}

// 测试4: 复杂数据结构处理
test "复杂数据结构处理" {
  // 定义记录类型
  type DataPoint {
    id : Int
    value : Double
    category : String
    timestamp : Int
  }
  
  // 创建测试数据点
  let data_points = [
    { id: 1, value: 10.5, category: "A", timestamp: 1000 },
    { id: 2, value: 20.3, category: "B", timestamp: 2000 },
    { id: 3, value: 15.7, category: "A", timestamp: 3000 },
    { id: 4, value: 25.1, category: "C", timestamp: 4000 },
    { id: 5, value: 18.9, category: "B", timestamp: 5000 }
  ]
  
  // 按类别分组
  let grouped_by_category = data_points.reduce(fn(acc, point) {
    let current_list = match acc.get(point.category) {
      Some(list) => list
      None => []
    }
    acc.insert(point.category, current_list.push(point))
    acc
  }, {})
  
  // 验证分组结果
  assert_eq(grouped_by_category.size(), 3)
  
  let category_a = grouped_by_category.get("A")
  match category_a {
    Some(list) => assert_eq(list.length(), 2)
    None => assert_true(false)
  }
  
  // 计算每个类别的平均值
  let category_averages = grouped_by_category.map_values(fn(points) {
    let sum = points.reduce(fn(acc, p) { acc + p.value }, 0.0)
    sum / points.length().to_double()
  })
  
  // 验证类别A的平均值
  let avg_a = category_averages.get("A")
  match avg_a {
    Some(value) => assert_eq(value, 13.1)
    None => assert_true(false)
  }
  
  // 按时间戳排序
  let sorted_by_timestamp = data_points.sort_by(fn(a, b) { a.timestamp - b.timestamp })
  assert_eq(sorted_by_timestamp[0].id, 1)
  assert_eq(sorted_by_timestamp[4].id, 5)
}

// 测试5: 数据验证和清洗
test "数据验证和清洗" {
  // 定义验证函数
  let is_valid_email = fn(email : String) -> Bool {
    email.contains("@") && email.contains(".")
  }
  
  let is_positive_number = fn(num : Int) -> Bool { num > 0 }
  
  // 创建包含无效数据的测试集
  let emails = ["user@example.com", "invalid-email", "test@test.org", "another@invalid"]
  let numbers = [10, -5, 0, 20, -15, 30]
  
  // 验证和清洗邮箱数据
  let valid_emails = emails.filter(is_valid_email)
  assert_eq(valid_emails.length(), 2)
  assert_eq(valid_emails, ["user@example.com", "test@test.org"])
  
  // 清洗数字数据，只保留正数
  let positive_numbers = numbers.filter(is_positive_number)
  assert_eq(positive_numbers.length(), 3)
  assert_eq(positive_numbers, [10, 20, 30])
  
  // 数据标准化
  let normalize_number = fn(num : Int, min : Int, max : Int) -> Double {
    if max == min { 0.0 } else { (num - min).to_double() / (max - min).to_double() }
  }
  
  let min_num = positive_numbers.reduce(fn(acc, x) { if x < acc { x } else { acc } }, positive_numbers[0])
  let max_num = positive_numbers.reduce(fn(acc, x) { if x > acc { x } else { acc } }, positive_numbers[0])
  
  let normalized_numbers = positive_numbers.map(fn(x) { normalize_number(x, min_num, max_num) })
  assert_eq(normalized_numbers, [0.0, 0.5, 1.0])
}
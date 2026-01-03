// Azimuth Enhanced Data Processing and Algorithm Tests
// 高级数据处理和算法测试用例，专注于数据结构、算法效率和数据处理质量

// Test 1: 高级数据结构操作测试
test "高级数据结构操作测试" {
  // 测试栈数据结构
  let mut stack = []
  
  // 压栈操作
  stack = stack + [1]
  stack = stack + [2]
  stack = stack + [3]
  stack = stack + [4]
  stack = stack + [5]
  
  // 验证栈顶元素
  assert_eq(stack[stack.length() - 1], 5)
  
  // 弹栈操作模拟
  let popped_value = stack[stack.length() - 1]
  assert_eq(popped_value, 5)
  stack = stack.slice(0, stack.length() - 1)
  
  // 验证弹栈后的状态
  assert_eq(stack.length(), 4)
  assert_eq(stack[stack.length() - 1], 4)
  
  // 测试队列数据结构
  let mut queue = []
  
  // 入队操作
  queue = queue + ["first"]
  queue = queue + ["second"]
  queue = queue + ["third"]
  
  // 验证队首元素
  assert_eq(queue[0], "first")
  
  // 出队操作模拟
  let dequeued_value = queue[0]
  assert_eq(dequeued_value, "first")
  queue = queue.slice(1, queue.length())
  
  // 验证出队后的状态
  assert_eq(queue.length(), 2)
  assert_eq(queue[0], "second")
}

// Test 2: 排序算法效率测试
test "排序算法效率测试" {
  // 创建测试数据
  let unsorted_data = [64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42]
  let expected_sorted = [11, 12, 22, 25, 34, 42, 50, 64, 76, 88, 90]
  
  // 简单冒泡排序实现
  let mut bubble_sorted = unsorted_data
  let n = bubble_sorted.length()
  
  for i in 0..<(n - 1) {
    for j in 0..<(n - i - 1) {
      if bubble_sorted[j] > bubble_sorted[j + 1] {
        // 交换元素
        let temp = bubble_sorted[j]
        bubble_sorted[j] = bubble_sorted[j + 1]
        bubble_sorted[j + 1] = temp
      }
    }
  }
  
  // 验证排序结果
  assert_eq(bubble_sorted.length(), expected_sorted.length())
  for i in 0..<expected_sorted.length() {
    assert_eq(bubble_sorted[i], expected_sorted[i])
  }
  
  // 测试选择排序
  let mut selection_sorted = unsorted_data
  
  for i in 0..<(n - 1) {
    let mut min_idx = i
    for j in (i + 1)..<n {
      if selection_sorted[j] < selection_sorted[min_idx] {
        min_idx = j
      }
    }
    
    // 交换元素
    if min_idx != i {
      let temp = selection_sorted[i]
      selection_sorted[i] = selection_sorted[min_idx]
      selection_sorted[min_idx] = temp
    }
  }
  
  // 验证排序结果
  assert_eq(selection_sorted.length(), expected_sorted.length())
  for i in 0..<expected_sorted.length() {
    assert_eq(selection_sorted[i], expected_sorted[i])
  }
}

// Test 3: 搜索算法测试
test "搜索算法测试" {
  // 创建有序测试数据
  let sorted_data = [2, 5, 8, 12, 16, 23, 38, 56, 72, 91]
  
  // 线性搜索
  fn linear_search(arr : Array[Int], target : Int) -> Int {
    for i in 0..<arr.length() {
      if arr[i] == target {
        return i
      }
    }
    return -1
  }
  
  // 测试线性搜索
  assert_eq(linear_search(sorted_data, 23), 5)
  assert_eq(linear_search(sorted_data, 2), 0)
  assert_eq(linear_search(sorted_data, 91), 9)
  assert_eq(linear_search(sorted_data, 100), -1)
  
  // 二分搜索
  fn binary_search(arr : Array[Int], target : Int) -> Int {
    let mut left = 0
    let mut right = arr.length() - 1
    
    while left <= right {
      let mid = left + (right - left) / 2
      
      if arr[mid] == target {
        return mid
      } else if arr[mid] < target {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    
    return -1
  }
  
  // 测试二分搜索
  assert_eq(binary_search(sorted_data, 23), 5)
  assert_eq(binary_search(sorted_data, 2), 0)
  assert_eq(binary_search(sorted_data, 91), 9)
  assert_eq(binary_search(sorted_data, 100), -1)
}

// Test 4: 数据聚合和统计测试
test "数据聚合和统计测试" {
  // 创建测试数据集
  let numeric_data = [15, 22, 8, 19, 31, 12, 25, 17, 9, 28, 14, 33]
  
  // 计算平均值
  let mut sum = 0
  for value in numeric_data {
    sum = sum + value
  }
  let average = sum.to_float() / numeric_data.length().to_float()
  assert_true(average > 19.0 && average < 20.0)
  
  // 查找最大值和最小值
  let mut max_value = numeric_data[0]
  let mut min_value = numeric_data[0]
  
  for value in numeric_data {
    if value > max_value {
      max_value = value
    }
    if value < min_value {
      min_value = value
    }
  }
  
  assert_eq(max_value, 33)
  assert_eq(min_value, 8)
  
  // 计算中位数
  let mut sorted_data = numeric_data
  let n = sorted_data.length()
  
  // 简单排序
  for i in 0..<(n - 1) {
    for j in 0..<(n - i - 1) {
      if sorted_data[j] > sorted_data[j + 1] {
        let temp = sorted_data[j]
        sorted_data[j] = sorted_data[j + 1]
        sorted_data[j + 1] = temp
      }
    }
  }
  
  let median = if n % 2 == 0 {
    (sorted_data[n / 2 - 1] + sorted_data[n / 2]).to_float() / 2.0
  } else {
    sorted_data[n / 2].to_float()
  }
  
  assert_true(median > 17.0 && median < 19.0)
  
  // 计算标准差
  let mut variance_sum = 0.0
  for value in numeric_data {
    let diff = value.to_float() - average
    variance_sum = variance_sum + (diff * diff)
  }
  let variance = variance_sum / numeric_data.length().to_float()
  let std_deviation = variance.sqrt()
  
  assert_true(std_deviation > 7.0 && std_deviation < 9.0)
}

// Test 5: 数据转换和映射测试
test "数据转换和映射测试" {
  // 创建字符串数据
  let string_data = ["apple", "banana", "cherry", "date", "elderberry"]
  
  // 映射到字符串长度
  let mut lengths = []
  for fruit in string_data {
    lengths = lengths + [fruit.length()]
  }
  
  assert_eq(lengths, [5, 6, 6, 4, 10])
  
  // 映射到首字母大写
  let mut capitalized = []
  for fruit in string_data {
    if fruit.length() > 0 {
      let first_char = fruit[0].to_uppercase()
      let rest = fruit.slice(1, fruit.length())
      capitalized = capitalized + [first_char + rest]
    } else {
      capitalized = capitalized + [fruit]
    }
  }
  
  assert_eq(capitalized, ["Apple", "Banana", "Cherry", "Date", "Elderberry"])
  
  // 过滤操作 - 筛选长度大于5的字符串
  let mut filtered = []
  for fruit in string_data {
    if fruit.length() > 5 {
      filtered = filtered + [fruit]
    }
  }
  
  assert_eq(filtered, ["banana", "cherry", "elderberry"])
  
  // 数据分组 - 按首字母分组
  let mut groups = []
  let processed = []
  
  for fruit in string_data {
    let first_char = fruit[0]
    
    if !processed.contains(first_char) {
      processed = processed + [first_char]
      
      let mut group = []
      for f in string_data {
        if f[0] == first_char {
          group = group + [f]
        }
      }
      
      groups = groups + [(first_char, group)]
    }
  }
  
  // 验证分组结果
  assert_eq(groups.length(), 5) // 每个首字母一个组
  assert_eq(groups[0], ('a', ["apple"]))
  assert_eq(groups[1], ('b', ["banana"]))
  assert_eq(groups[2], ('c', ["cherry"]))
  assert_eq(groups[3], ('d', ["date"]))
  assert_eq(groups[4], ('e', ["elderberry"]))
}

// Test 6: 递归算法测试
test "递归算法测试" {
  // 阶乘递归实现
  fn factorial(n : Int) -> Int {
    if n <= 1 {
      return 1
    } else {
      return n * factorial(n - 1)
    }
  }
  
  // 测试阶乘
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  assert_eq(factorial(7), 5040)
  
  // 斐波那契数列递归实现
  fn fibonacci(n : Int) -> Int {
    if n <= 1 {
      return n
    } else {
      return fibonacci(n - 1) + fibonacci(n - 2)
    }
  }
  
  // 测试斐波那契数列
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(5), 5)
  assert_eq(fibonacci(7), 13)
  
  // 最大公约数递归实现
  fn gcd(a : Int, b : Int) -> Int {
    if b == 0 {
      return a
    } else {
      return gcd(b, a % b)
    }
  }
  
  // 测试最大公约数
  assert_eq(gcd(48, 18), 6)
  assert_eq(gcd(56, 98), 14)
  assert_eq(gcd(101, 103), 1)
}

// Test 7: 数据结构高级操作测试
test "数据结构高级操作测试" {
  // 创建嵌套数据结构
  let nested_data = [
    ("fruits", ["apple", "banana", "cherry"]),
    ("vegetables", ["carrot", "broccoli", "spinach"]),
    ("grains", ["rice", "wheat", "oats"])
  ]
  
  // 扁平化操作
  let mut flattened = []
  for (category, items) in nested_data {
    for item in items {
      flattened = flattened + [(category, item)]
    }
  }
  
  // 验证扁平化结果
  assert_eq(flattened.length(), 9)
  assert_eq(flattened[0], ("fruits", "apple"))
  assert_eq(flattened[3], ("vegetables", "carrot"))
  assert_eq(flattened[6], ("grains", "rice"))
  
  // 分组操作 - 按类别分组
  let mut grouped = []
  let processed_categories = []
  
  for (category, item) in flattened {
    if !processed_categories.contains(category) {
      processed_categories = processed_categories + [category]
      
      let mut group_items = []
      for (cat, itm) in flattened {
        if cat == category {
          group_items = group_items + [itm]
        }
      }
      
      grouped = grouped + [(category, group_items)]
    }
  }
  
  // 验证分组结果
  assert_eq(grouped.length(), 3)
  assert_eq(grouped[0], ("fruits", ["apple", "banana", "cherry"]))
  assert_eq(grouped[1], ("vegetables", ["carrot", "broccoli", "spinach"]))
  assert_eq(grouped[2], ("grains", ["rice", "wheat", "oats"]))
  
  // 数据转换操作
  let mut transformed = []
  for (category, items) in nested_data {
    let category_info = (category, items.length(), items[0])
    transformed = transformed + [category_info]
  }
  
  // 验证转换结果
  assert_eq(transformed, [
    ("fruits", 3, "apple"),
    ("vegetables", 3, "carrot"),
    ("grains", 3, "rice")
  ])
}

// Test 8: 数据验证和清洗测试
test "数据验证和清洗测试" {
  // 创建包含异常值的数据
  let raw_data = [
    ("name", "John"),
    ("age", "25"),
    ("email", "john@example.com"),
    ("name", ""),
    ("age", "invalid"),
    ("email", "not-an-email"),
    ("name", "Alice"),
    ("age", "30"),
    ("email", "alice@example.com")
  ]
  
  // 数据清洗函数
  fn is_valid_name(name : String) -> Bool {
    name.length() > 0 && name.length() <= 50
  }
  
  fn is_valid_age(age_str : String) -> Bool {
    match age_str.parse_int() {
      Some(age) => age >= 0 && age <= 150
      None => false
    }
  }
  
  fn is_valid_email(email : String) -> Bool {
    email.contains("@") && email.contains(".") && email.length() > 5
  }
  
  // 数据清洗和验证
  let mut cleaned_data = []
  let mut validation_errors = []
  
  for (field_type, value) in raw_data {
    let is_valid = match field_type {
      "name" => is_valid_name(value)
      "age" => is_valid_age(value)
      "email" => is_valid_email(value)
      _ => false
    }
    
    if is_valid {
      cleaned_data = cleaned_data + [(field_type, value)]
    } else {
      validation_errors = validation_errors + [("Invalid " + field_type, value)]
    }
  }
  
  // 验证清洗结果
  assert_eq(cleaned_data.length(), 6)
  assert_eq(validation_errors.length(), 3)
  
  // 验证有效数据
  assert_true(cleaned_data.contains(("name", "John")))
  assert_true(cleaned_data.contains(("age", "25")))
  assert_true(cleaned_data.contains(("email", "john@example.com")))
  assert_true(cleaned_data.contains(("name", "Alice")))
  assert_true(cleaned_data.contains(("age", "30")))
  assert_true(cleaned_data.contains(("email", "alice@example.com")))
  
  // 验证错误数据
  assert_true(validation_errors.contains(("Invalid name", "")))
  assert_true(validation_errors.contains(("Invalid age", "invalid")))
  assert_true(validation_errors.contains(("Invalid email", "not-an-email")))
}

// Test 9: 数据压缩和优化测试
test "数据压缩和优化测试" {
  // 创建重复数据用于压缩测试
  let repetitive_data = [
    "apple", "banana", "apple", "cherry", "banana", 
    "apple", "date", "cherry", "banana", "apple"
  ]
  
  // 简单的运行长度编码
  let mut encoded = []
  if repetitive_data.length() > 0 {
    let mut current = repetitive_data[0]
    let mut count = 1
    
    for i in 1..<repetitive_data.length() {
      if repetitive_data[i] == current {
        count = count + 1
      } else {
        encoded = encoded + [(current, count)]
        current = repetitive_data[i]
        count = 1
      }
    }
    
    encoded = encoded + [(current, count)]
  }
  
  // 验证编码结果
  assert_eq(encoded.length(), 5)
  assert_eq(encoded[0], ("apple", 1))
  assert_eq(encoded[1], ("banana", 1))
  assert_eq(encoded[2], ("apple", 1))
  assert_eq(encoded[3], ("cherry", 1))
  assert_eq(encoded[4], ("banana", 1))
  
  // 数据去重
  let mut unique_items = []
  let processed_items = []
  
  for item in repetitive_data {
    if !processed_items.contains(item) {
      processed_items = processed_items + [item]
      unique_items = unique_items + [item]
    }
  }
  
  // 验证去重结果
  assert_eq(unique_items, ["apple", "banana", "cherry", "date"])
  
  // 数据频率统计
  let mut frequency = []
  let counted_items = []
  
  for item in repetitive_data {
    if !counted_items.contains(item) {
      counted_items = counted_items + [item]
      
      let mut count = 0
      for i in repetitive_data {
        if i == item {
          count = count + 1
        }
      }
      
      frequency = frequency + [(item, count)]
    }
  }
  
  // 验证频率统计
  assert_eq(frequency.length(), 4)
  assert_eq(frequency[0], ("apple", 4))
  assert_eq(frequency[1], ("banana", 3))
  assert_eq(frequency[2], ("cherry", 2))
  assert_eq(frequency[3], ("date", 1))
}

// Test 10: 高级数据处理管道测试
test "高级数据处理管道测试" {
  // 创建原始数据
  let raw_records = [
    ("product", "laptop", "price", "1200", "category", "electronics"),
    ("product", "mouse", "price", "25", "category", "electronics"),
    ("product", "desk", "price", "300", "category", "furniture"),
    ("product", "chair", "price", "150", "category", "furniture"),
    ("product", "monitor", "price", "400", "category", "electronics")
  ]
  
  // 数据解析阶段
  let mut parsed_records = []
  for i in 0..<raw_records.length() {
    if i % 3 == 0 {
      let product_name = raw_records[i].1
      let price_str = raw_records[i + 1].1
      let category = raw_records[i + 2].1
      
      match price_str.parse_int() {
        Some(price) => {
          parsed_records = parsed_records + [(product_name, price, category)]
        }
        None => () // 忽略无效价格
      }
    }
  }
  
  // 验证解析结果
  assert_eq(parsed_records.length(), 5)
  assert_eq(parsed_records[0], ("laptop", 1200, "electronics"))
  assert_eq(parsed_records[1], ("mouse", 25, "electronics"))
  assert_eq(parsed_records[2], ("desk", 300, "furniture"))
  assert_eq(parsed_records[3], ("chair", 150, "furniture"))
  assert_eq(parsed_records[4], ("monitor", 400, "electronics"))
  
  // 数据过滤阶段 - 筛选价格大于100的商品
  let mut filtered_records = []
  for (name, price, category) in parsed_records {
    if price > 100 {
      filtered_records = filtered_records + [(name, price, category)]
    }
  }
  
  // 验证过滤结果
  assert_eq(filtered_records.length(), 4)
  assert_eq(filtered_records[0], ("laptop", 1200, "electronics"))
  assert_eq(filtered_records[1], ("desk", 300, "furniture"))
  assert_eq(filtered_records[2], ("chair", 150, "furniture"))
  assert_eq(filtered_records[3], ("monitor", 400, "electronics"))
  
  // 数据转换阶段 - 添加税后价格
  let tax_rate = 0.1
  let mut transformed_records = []
  
  for (name, price, category) in filtered_records {
    let tax_amount = price.to_float() * tax_rate
    let price_with_tax = price.to_float() + tax_amount
    transformed_records = transformed_records + [(name, price, category, price_with_tax)]
  }
  
  // 验证转换结果
  assert_eq(transformed_records.length(), 4)
  assert_eq(transformed_records[0], ("laptop", 1200, "electronics", 1320.0))
  assert_eq(transformed_records[1], ("desk", 300, "furniture", 330.0))
  assert_eq(transformed_records[2], ("chair", 150, "furniture", 165.0))
  assert_eq(transformed_records[3], ("monitor", 400, "electronics", 440.0))
  
  // 数据聚合阶段 - 按类别统计
  let mut category_stats = []
  let processed_categories = []
  
  for (name, price, category, price_with_tax) in transformed_records {
    if !processed_categories.contains(category) {
      processed_categories = processed_categories + [category]
      
      let mut total_price = 0
      let mut total_price_with_tax = 0.0
      let mut count = 0
      
      for (n, p, c, pt) in transformed_records {
        if c == category {
          total_price = total_price + p
          total_price_with_tax = total_price_with_tax + pt
          count = count + 1
        }
      }
      
      let avg_price = total_price.to_float() / count.to_float()
      let avg_price_with_tax = total_price_with_tax / count.to_float()
      
      category_stats = category_stats + [(category, count, avg_price, avg_price_with_tax)]
    }
  }
  
  // 验证聚合结果
  assert_eq(category_stats.length(), 2)
  
  // 验证电子产品统计
  let electronics_stats = category_stats[0]
  assert_eq(electronics_stats.0, "electronics")
  assert_eq(electronics_stats.1, 3) // 3个电子产品
  assert_true(electronics_stats.2 > 500.0 && electronics_stats.2 < 600.0) // 平均价格
  assert_true(electronics_stats.3 > 700.0 && electronics_stats.3 < 800.0) // 平均含税价格
  
  // 验证家具统计
  let furniture_stats = category_stats[1]
  assert_eq(furniture_stats.0, "furniture")
  assert_eq(furniture_stats.1, 2) // 2个家具产品
  assert_true(furniture_stats.2 > 200.0 && furniture_stats.2 < 250.0) // 平均价格
  assert_true(furniture_stats.3 > 200.0 && furniture_stats.3 < 300.0) // 平均含税价格
}
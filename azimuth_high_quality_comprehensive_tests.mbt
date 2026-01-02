// Azimuth 高质量综合测试用例
// 包含多种测试场景和边界条件验证

// 测试1: 高级数据结构操作
test "高级数据结构操作测试" {
  // 测试嵌套数组操作
  let matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
  let flattened = matrix.reduce(fn(acc, row) { acc.concat(row) }, [])
  assert_eq(flattened, [1, 2, 3, 4, 5, 6, 7, 8, 9])
  
  // 测试矩阵转置
  let transposed = []
  for i in 0..=2 {
    let column = []
    for j in 0..=2 {
      column = column.push(matrix[j][i])
    }
    transposed = transposed.push(column)
  }
  assert_eq(transposed, [[1, 4, 7], [2, 5, 8], [3, 6, 9]])
  
  // 测试字典操作（通过键值对数组模拟）
  let dict = [("name", "Alice"), ("age", "30"), ("city", "Beijing")]
  let keys = dict.map(fn(pair) { pair.0 })
  let values = dict.map(fn(pair) { pair.1 })
  assert_eq(keys, ["name", "age", "city"])
  assert_eq(values, ["Alice", "30", "Beijing"])
}

// 测试2: 字符串高级处理
test "字符串高级处理测试" {
  let text = "Azimuth遥测系统提供全面的监控能力"
  
  // 测试字符统计
  let char_count = text.length()
  assert_eq(char_count, 18)
  
  // 测试子字符串提取
  let substring = text.substring(0, 7)
  assert_eq(substring, "Azimuth遥")
  
  // 测试字符串分割（通过模拟）
  let words = []
  let mut start = 0
  for i in 0..=char_count {
    if i == char_count || text[i] == ' ' {
      if start < i {
        words = words.push(text.substring(start, i))
      }
      start = i + 1
    }
  }
  assert_eq(words.length(), 1)
  assert_eq(words[0], "Azimuth遥测系统提供全面的监控能力")
  
  // 测试字符串反转
  let reversed = ""
  for i in 0..=char_count - 1 {
    reversed = text[i] + reversed
  }
  assert_eq(reversed, "能力控控的全面供提统测遥htuimzA")
}

// 测试3: 递归算法测试
test "递归算法测试" {
  // 阶乘函数
  let factorial = fn(n) {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  assert_eq(factorial(7), 5040)
  
  // 斐波那契数列
  let fibonacci = fn(n) {
    if n <= 1 {
      n
    } else {
      fibonacci(n - 1) + fibonacci(n - 2)
    }
  }
  
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(5), 5)
  assert_eq(fibonacci(8), 21)
  
  // 最大公约数
  let gcd = fn(a, b) {
    if b == 0 {
      a
    } else {
      gcd(b, a % b)
    }
  }
  
  assert_eq(gcd(48, 18), 6)
  assert_eq(gcd(17, 13), 1)
  assert_eq(gcd(100, 25), 25)
}

// 测试4: 错误处理和异常情况
test "错误处理和异常情况测试" {
  // 测试除零错误处理
  let safe_divide = fn(a, b) {
    if b == 0 {
      Err("Division by zero")
    } else {
      Ok(a / b)
    }
  }
  
  match safe_divide(10, 2) {
    Ok(result) => assert_eq(result, 5)
    Err(msg) => assert_true(false)
  }
  
  match safe_divide(10, 0) {
    Ok(result) => assert_true(false)
    Err(msg) => assert_eq(msg, "Division by zero")
  }
  
  // 测试数组越界处理
  let safe_get = fn(arr, index) {
    if index >= 0 && index < arr.length() {
      Ok(arr[index])
    } else {
      Err("Index out of bounds")
    }
  }
  
  let test_array = [10, 20, 30, 40, 50]
  
  match safe_get(test_array, 2) {
    Ok(value) => assert_eq(value, 30)
    Err(msg) => assert_true(false)
  }
  
  match safe_get(test_array, 10) {
    Ok(value) => assert_true(false)
    Err(msg) => assert_eq(msg, "Index out of bounds")
  }
  
  // 测试空值处理
  let safe_string_length = fn(str) {
    match str {
      None => Err("String is null")
      Some(s) => Ok(s.length())
    }
  }
  
  match safe_string_length(Some("hello")) {
    Ok(len) => assert_eq(len, 5)
    Err(msg) => assert_true(false)
  }
  
  match safe_string_length(None) {
    Ok(len) => assert_true(false)
    Err(msg) => assert_eq(msg, "String is null")
  }
}

// 测试5: 性能优化算法
test "性能优化算法测试" {
  // 测试二分查找
  let binary_search = fn(arr, target) {
    let mut left = 0
    let mut right = arr.length() - 1
    
    while left <= right {
      let mid = (left + right) / 2
      if arr[mid] == target {
        return Ok(mid)
      } else if arr[mid] < target {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    
    Err("Element not found")
  }
  
  let sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
  
  match binary_search(sorted_array, 7) {
    Ok(index) => assert_eq(index, 3)
    Err(msg) => assert_true(false)
  }
  
  match binary_search(sorted_array, 8) {
    Ok(index) => assert_true(false)
    Err(msg) => assert_eq(msg, "Element not found")
  }
  
  // 测试快速排序（简化版）
  let quick_sort = fn(arr) {
    if arr.length() <= 1 {
      arr
    } else {
      let pivot = arr[0]
      let less = arr.filter(fn(x) { x < pivot })
      let equal = arr.filter(fn(x) { x == pivot })
      let greater = arr.filter(fn(x) { x > pivot })
      
      quick_sort(less).concat(equal).concat(quick_sort(greater))
    }
  }
  
  let unsorted = [5, 2, 8, 1, 9, 3, 7, 4, 6]
  let sorted = quick_sort(unsorted)
  assert_eq(sorted, [1, 2, 3, 4, 5, 6, 7, 8, 9])
}

// 测试6: 状态机和模式匹配
test "状态机和模式匹配测试" {
  // 定义状态枚举
  enum TrafficLight {
    Red
    Yellow
    Green
  }
  
  // 状态转换函数
  let next_light = fn(light) {
    match light {
      TrafficLight::Red => TrafficLight::Green
      TrafficLight::Yellow => TrafficLight::Red
      TrafficLight::Green => TrafficLight::Yellow
    }
  }
  
  let mut current = TrafficLight::Red
  
  // 测试状态转换序列
  assert_eq(current, TrafficLight::Red)
  current = next_light(current)
  assert_eq(current, TrafficLight::Green)
  current = next_light(current)
  assert_eq(current, TrafficLight::Yellow)
  current = next_light(current)
  assert_eq(current, TrafficLight::Red)
  
  // 测试复杂模式匹配
  enum Result<T, E> {
    Success(T)
    Failure(E)
    Pending
  }
  
  let process_result = fn(result) {
    match result {
      Result::Success(value) => "操作成功: " + value.to_string()
      Result::Failure(error) => "操作失败: " + error
      Result::Pending => "操作进行中..."
    }
  }
  
  let success = Result::Success(42)
  let failure = Result::Failure("网络超时")
  let pending = Result::Pending
  
  assert_eq(process_result(success), "操作成功: 42")
  assert_eq(process_result(failure), "操作失败: 网络超时")
  assert_eq(process_result(pending), "操作进行中...")
}

// 测试7: 数据验证和边界条件
test "数据验证和边界条件测试" {
  // 邮箱验证
  let is_valid_email = fn(email) {
    if !email.contains("@") || !email.contains(".") {
      false
    } else {
      let at_index = email.index_of("@")
      let dot_index = email.index_of(".")
      at_index > 0 && dot_index > at_index + 1 && dot_index < email.length() - 1
    }
  }
  
  assert_true(is_valid_email("user@example.com"))
  assert_true(is_valid_email("test.email+tag@domain.co.uk"))
  assert_false(is_valid_email("invalid.email"))
  assert_false(is_valid_email("@domain.com"))
  assert_false(is_valid_email("user@"))
  assert_false(is_valid_email("user@domain"))
  assert_false(is_valid_email(""))
  
  // 密码强度验证
  let check_password_strength = fn(password) {
    let has_upper = password != password.to_lowercase()
    let has_lower = password != password.to_uppercase()
    let has_digit = password.split("").any(fn(c) { c >= "0" && c <= "9" })
    let has_special = password.split("").any(fn(c) { 
      c == "!" || c == "@" || c == "#" || c == "$" || c == "%" 
    })
    
    has_upper && has_lower && has_digit && has_special && password.length() >= 8
  }
  
  assert_true(check_password_strength("StrongP@ss1"))
  assert_false(check_password_strength("weak"))
  assert_false(check_password_strength("nouppercase1!"))
  assert_false(check_password_strength("NOLOWERCASE1!"))
  assert_false(check_password_strength("NoDigitPass!"))
  assert_false(check_password_strength("NoSpecialPass1"))
  assert_false(check_password_strength("Short1!"))
}

// 测试8: 并发和异步操作模拟
test "并发和异步操作模拟测试" {
  // 模拟任务队列
  let task_queue = []
  let task_results = []
  
  // 添加任务到队列
  let add_task = fn(queue, task_id, priority) {
    let new_task = (task_id, priority)
    let mut updated_queue = queue
    
    // 按优先级插入
    let mut inserted = false
    for i in 0..=queue.length() {
      if i == queue.length() || queue[i].1 > priority {
        updated_queue = updated_queue.slice(0, i).concat([new_task]).concat(updated_queue.slice(i, updated_queue.length()))
        inserted = true
        break
      }
    }
    
    if !inserted {
      updated_queue = updated_queue.push(new_task)
    }
    
    updated_queue
  }
  
  // 处理任务
  let process_tasks = fn(queue) {
    let mut results = []
    let mut remaining_tasks = queue
    
    while remaining_tasks.length() > 0 {
      let task = remaining_tasks[0]
      let task_id = task.0
      let priority = task.1
      
      // 模拟任务处理
      let result = "Task " + task_id.to_string() + " completed with priority " + priority.to_string()
      results = results.push(result)
      
      remaining_tasks = remaining_tasks.slice(1, remaining_tasks.length())
    }
    
    results
  }
  
  // 添加任务
  task_queue = add_task(task_queue, "A", 3)
  task_queue = add_task(task_queue, "B", 1)
  task_queue = add_task(task_queue, "C", 2)
  task_queue = add_task(task_queue, "D", 1)
  
  // 验证队列顺序（按优先级）
  assert_eq(task_queue[0], ("B", 1))
  assert_eq(task_queue[1], ("D", 1))
  assert_eq(task_queue[2], ("C", 2))
  assert_eq(task_queue[3], ("A", 3))
  
  // 处理所有任务
  task_results = process_tasks(task_queue)
  
  // 验证处理结果
  assert_eq(task_results.length(), 4)
  assert_eq(task_results[0], "Task B completed with priority 1")
  assert_eq(task_results[1], "Task D completed with priority 1")
  assert_eq(task_results[2], "Task C completed with priority 2")
  assert_eq(task_results[3], "Task A completed with priority 3")
}

// 测试9: 缓存和记忆化
test "缓存和记忆化测试" {
  // 简单缓存实现
  let cache = []
  
  let cache_get = fn(key) {
    for item in cache {
      if item.0 == key {
        return Some(item.1)
      }
    }
    None
  }
  
  let cache_set = fn(key, value) {
    let updated_cache = []
    let found = false
    
    for item in cache {
      if item.0 == key {
        updated_cache = updated_cache.push((key, value))
        found = true
      } else {
        updated_cache = updated_cache.push(item)
      }
    }
    
    if !found {
      updated_cache = updated_cache.push((key, value))
    }
    
    updated_cache
  }
  
  // 测试缓存操作
  cache = cache_set("user:1", "Alice")
  cache = cache_set("user:2", "Bob")
  cache = cache_set("config:timeout", "30")
  
  match cache_get("user:1") {
    Some(value) => assert_eq(value, "Alice")
    None => assert_true(false)
  }
  
  match cache_get("user:2") {
    Some(value) => assert_eq(value, "Bob")
    None => assert_true(false)
  }
  
  match cache_get("nonexistent") {
    Some(value) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试缓存更新
  cache = cache_set("user:1", "Alice Smith")
  match cache_get("user:1") {
    Some(value) => assert_eq(value, "Alice Smith")
    None => assert_true(false)
  }
  
  // 记忆化斐波那契函数
  let fib_cache = []
  
  let memoized_fib = fn(n) {
    match cache_get(n) {
      Some(value) => value,
      None => {
        let result = if n <= 1 {
          n
        } else {
          memoized_fib(n - 1) + memoized_fib(n - 2)
        }
        
        // 缓存结果
        fib_cache = cache_set(n, result)
        result
      }
    }
  }
  
  assert_eq(memoized_fib(5), 5)
  assert_eq(memoized_fib(10), 55)
}

// 测试10: 数据转换和格式化
test "数据转换和格式化测试" {
  // 数字格式化
  let format_number = fn(num, decimals) {
    let multiplier = 10.0 ^ decimals
    let rounded = (num * multiplier).round() / multiplier
    rounded.to_string()
  }
  
  assert_eq(format_number(3.14159, 2), "3.14")
  assert_eq(format_number(2.71828, 3), "2.718")
  assert_eq(format_number(1.5, 0), "2.0")
  
  // 日期格式化（模拟）
  let format_date = fn(year, month, day) {
    let year_str = year.to_string()
    let month_str = if month < 10 { "0" + month.to_string() } else { month.to_string() }
    let day_str = if day < 10 { "0" + day.to_string() } else { day.to_string() }
    
    year_str + "-" + month_str + "-" + day_str
  }
  
  assert_eq(format_date(2023, 1, 5), "2023-01-05")
  assert_eq(format_date(2023, 12, 25), "2023-12-25")
  assert_eq(format_date(2023, 10, 10), "2023-10-10")
  
  // 数据类型转换
  let safe_parse_int = fn(str) {
    if str.length() == 0 {
      Err("Empty string")
    } else {
      let mut result = 0
      let mut valid = true
      
      for i in 0..=str.length() - 1 {
        let char = str[i]
        if char >= "0" && char <= "9" {
          result = result * 10 + (char.to_int() - 48)
        } else {
          valid = false
          break
        }
      }
      
      if valid {
        Ok(result)
      } else {
        Err("Invalid number format")
      }
    }
  }
  
  match safe_parse_int("123") {
    Ok(value) => assert_eq(value, 123)
    Err(msg) => assert_true(false)
  }
  
  match safe_parse_int("0") {
    Ok(value) => assert_eq(value, 0)
    Err(msg) => assert_true(false)
  }
  
  match safe_parse_int("abc") {
    Ok(value) => assert_true(false)
    Err(msg) => assert_eq(msg, "Invalid number format")
  }
  
  match safe_parse_int("") {
    Ok(value) => assert_true(false)
    Err(msg) => assert_eq(msg, "Empty string")
  }
}
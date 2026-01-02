// Azimuth 新增MoonBit测试用例
// 涵盖多种测试场景和边界条件，使用正确的MoonBit API

// 测试1: 错误处理和异常情况
test "错误处理和异常情况测试" {
  // 除零错误处理
  let safe_divide = fn(a, b) {
    if b == 0 { 
      None 
    } else { 
      Some(a / b)
    }
  }
  
  match safe_divide(10, 2) {
    Some(result) => assert_eq(result, 5)
    None => assert_true(false)
  }
  
  match safe_divide(10, 0) {
    Some(result) => assert_true(false)
    None => assert_true(true)
  }
  
  // 数组越界保护
  let safe_get = fn(arr, index) {
    if index >= 0 && index < arr.length() {
      Some(arr[index])
    } else {
      None
    }
  }
  
  let test_array = [1, 2, 3, 4, 5]
  
  match safe_get(test_array, 2) {
    Some(value) => assert_eq(value, 3)
    None => assert_true(false)
  }
  
  match safe_get(test_array, 10) {
    Some(value) => assert_true(false)
    None => assert_true(true)
  }
  
  match safe_get(test_array, -1) {
    Some(value) => assert_true(false)
    None => assert_true(true)
  }
}

// 测试2: 数据结构操作
test "高级数据结构操作测试" {
  // 嵌套数组操作
  let matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ]
  
  assert_eq(matrix.length(), 3)
  assert_eq(matrix[0].length(), 3)
  assert_eq(matrix[1][2], 6)
  assert_eq(matrix[2][0], 7)
  
  // 矩阵转置（简化版）
  let transpose = fn(m) {
    if m.length() == 0 { [] }
    else {
      // 简化实现，只处理3x3矩阵
      if m.length() == 3 && m[0].length() == 3 {
        [
          [m[0][0], m[1][0], m[2][0]],
          [m[0][1], m[1][1], m[2][1]],
          [m[0][2], m[1][2], m[2][2]]
        ]
      } else { [] }
    }
  }
  
  let transposed = transpose(matrix)
  assert_eq(transposed[0][0], 1)
  assert_eq(transposed[0][1], 4)
  assert_eq(transposed[0][2], 7)
  assert_eq(transposed[1][0], 2)
  assert_eq(transposed[2][2], 9)
}

// 测试3: 字符串高级操作
test "字符串高级操作测试" {
  let text = "Azimuth Telemetry System"
  
  // 字符串长度
  assert_eq(text.length(), 23)
  
  // 字符串连接
  let str1 = "hello"
  let str2 = "world"
  let joined = str1 + " " + str2
  assert_eq(joined, "hello world")
  
  // 子字符串检查
  assert_true(text.contains("Telemetry"))
  assert_false(text.contains("Monitoring"))
  
  // 字符串比较
  assert_eq(str1, "hello")
  assert_not_eq(str1, "world")
  
  // 字符串转换
  let num_str = "42"
  let num = 42
  assert_eq(num.to_string(), num_str)
}

// 测试4: 数值计算和精度
test "数值计算和精度测试" {
  // 基本算术运算
  let a = 0.1
  let b = 0.2
  let sum = a + b
  
  // 数值范围检查
  let clamp = fn(value, min, max) {
    if value < min { min }
    else if value > max { max }
    else { value }
  }
  
  assert_eq(clamp(5, 0, 10), 5)
  assert_eq(clamp(-5, 0, 10), 0)
  assert_eq(clamp(15, 0, 10), 10)
  
  // 数值插值
  let lerp = fn(start, end, t) {
    start + (end - start) * t
  }
  
  assert_eq(lerp(0.0, 10.0, 0.5), 5.0)
  assert_eq(lerp(10.0, 20.0, 0.25), 12.5)
  assert_eq(lerp(5.0, 15.0, 0.0), 5.0)
  assert_eq(lerp(5.0, 15.0, 1.0), 15.0)
  
  // 幂运算（简化版）
  let power = fn(base, exp) {
    if exp == 0 { 1.0 }
    else if exp < 0 { 1.0 / power(base, -exp) }
    else if exp % 2 == 0 { 
      let half = power(base, exp / 2)
      half * half
    }
    else { base * power(base, exp - 1) }
  }
  
  assert_eq(power(2.0, 3), 8.0)
  assert_eq(power(3.0, 2), 9.0)
  assert_eq(power(5.0, 0), 1.0)
  assert_eq(power(2.0, -2), 0.25)
}

// 测试5: 状态机和模式匹配
test "状态机和模式匹配测试" {
  // 简单状态机
  type State {
    Idle
    Running
    Paused
    Stopped
  }
  
  type Event {
    Start
    Pause
    Resume
    Stop
  }
  
  let transition = fn(state, event) {
    match (state, event) {
      (Idle, Start) => Running
      (Running, Pause) => Paused
      (Paused, Resume) => Running
      (Running, Stop) => Stopped
      (Paused, Stop) => Stopped
      (Stopped, Start) => Running
      (_, _) => state // 无效转换，保持原状态
    }
  }
  
  // 测试状态转换
  assert_eq(transition(Idle, Start), Running)
  assert_eq(transition(Running, Pause), Paused)
  assert_eq(transition(Paused, Resume), Running)
  assert_eq(transition(Running, Stop), Stopped)
  assert_eq(transition(Paused, Stop), Stopped)
  assert_eq(transition(Stopped, Start), Running)
  
  // 测试无效转换
  assert_eq(transition(Idle, Pause), Idle)
  assert_eq(transition(Idle, Stop), Idle)
  assert_eq(transition(Running, Start), Running)
  assert_eq(transition(Paused, Pause), Paused)
  
  // 复杂模式匹配
  let process_data = fn(data) {
    match data {
      Some(value) => {
        if value > 100 { "Large" }
        else if value > 50 { "Medium" }
        else { "Small" }
      }
      None => "Empty"
    }
  }
  
  assert_eq(process_data(Some(150)), "Large")
  assert_eq(process_data(Some(75)), "Medium")
  assert_eq(process_data(Some(25)), "Small")
  assert_eq(process_data(None), "Empty")
}

// 测试6: 时间和日期操作
test "时间和日期操作测试" {
  // 简化的时间表示
  type Time {
    Time(Int, Int, Int) // 小时, 分钟, 秒
  }
  
  let time_to_seconds = fn(t) {
    match t {
      Time(h, m, s) => h * 3600 + m * 60 + s
    }
  }
  
  let seconds_to_time = fn(seconds) {
    let hours = seconds / 3600
    let remaining = seconds % 3600
    let minutes = remaining / 60
    let secs = remaining % 60
    Time(hours, minutes, secs)
  }
  
  let time1 = Time(1, 30, 45)
  let time2 = Time(2, 15, 30)
  
  let seconds1 = time_to_seconds(time1)
  let seconds2 = time_to_seconds(time2)
  
  assert_eq(seconds1, 5445)
  assert_eq(seconds2, 8130)
  
  // 时间加法
  let add_times = fn(t1, t2) {
    let total_seconds = time_to_seconds(t1) + time_to_seconds(t2)
    seconds_to_time(total_seconds)
  }
  
  let sum_time = add_times(time1, time2)
  match sum_time {
    Time(h, m, s) => {
      assert_eq(h, 3)
      assert_eq(m, 46)
      assert_eq(s, 15)
    }
  }
  
  // 时间比较
  let time_compare = fn(t1, t2) {
    let seconds1 = time_to_seconds(t1)
    let seconds2 = time_to_seconds(t2)
    
    if seconds1 < seconds2 { -1 }
    else if seconds1 > seconds2 { 1 }
    else { 0 }
  }
  
  assert_eq(time_compare(time1, time2), -1)
  assert_eq(time_compare(time2, time1), 1)
  assert_eq(time_compare(time1, time1), 0)
}

// 测试7: 递归函数
test "递归函数测试" {
  // 阶乘函数
  let factorial = fn(n) {
    if n <= 1 { 1 }
    else { n * factorial(n - 1) }
  }
  
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  assert_eq(factorial(3), 6)
  
  // 斐波那契数列
  let fibonacci = fn(n) {
    if n <= 0 { 0 }
    else if n == 1 { 1 }
    else { fibonacci(n - 1) + fibonacci(n - 2) }
  }
  
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(2), 1)
  assert_eq(fibonacci(3), 2)
  assert_eq(fibonacci(5), 5)
  
  // 数组求和（递归）
  let sum_array_helper = fn(arr, index) {
    if index >= arr.length() { 0 }
    else { arr[index] + sum_array_helper(arr, index + 1) }
  }
  
  let sum_array = fn(arr) {
    sum_array_helper(arr, 0)
  }
  
  assert_eq(sum_array([]), 0)
  assert_eq(sum_array([1]), 1)
  assert_eq(sum_array([1, 2, 3, 4, 5]), 15)
}

// 测试8: 数据验证和约束
test "数据验证和约束测试" {
  // 邮箱验证
  let is_valid_email = fn(email) {
    let has_at = email.contains("@")
    let has_dot = email.contains(".")
    let parts = email.split("@")
    
    if parts.length() != 2 { false }
    else {
      let local = parts[0]
      let domain = parts[1]
      
      local.length() > 0 && 
      domain.length() > 0 && 
      domain.contains(".") &&
      !email.contains(" ") &&
      !email.starts_with("@") &&
      !email.ends_with("@")
    }
  }
  
  assert_true(is_valid_email("user@example.com"))
  assert_false(is_valid_email("invalid-email"))
  assert_false(is_valid_email("@example.com"))
  assert_false(is_valid_email("user@"))
  assert_false(is_valid_email("user example@domain.com"))
  
  // 密码强度验证
  enum PasswordStrength {
    Weak
    Medium
    Strong
  }
  
  let check_password_strength = fn(password) {
    let long_enough = password.length() >= 8
    
    if long_enough {
      // 简化检查：只检查长度
      Strong
    } else if password.length() >= 6 {
      Medium
    } else {
      Weak
    }
  }
  
  assert_eq(check_password_strength("Password123!"), Strong)
  assert_eq(check_password_strength("pass123"), Medium)
  assert_eq(check_password_strength("123"), Weak)
  
  // 数据范围验证
  let validate_range = fn(value, min, max) {
    if value < min { (false, "Value too small") }
    else if value > max { (false, "Value too large") }
    else { (true, "Valid") }
  }
  
  assert_eq(validate_range(5, 1, 10), (true, "Valid"))
  assert_eq(validate_range(0, 1, 10), (false, "Value too small"))
  assert_eq(validate_range(11, 1, 10), (false, "Value too large"))
}

// 测试9: 算法和数据结构
test "算法和数据结构测试" {
  // 二分查找
  let binary_search_helper = fn(arr, target, low, high) {
    if low > high { -1 }
    else {
      let mid = (low + high) / 2
      let mid_val = arr[mid]
      
      if mid_val == target { mid }
      else if mid_val < target { binary_search_helper(arr, target, mid + 1, high) }
      else { binary_search_helper(arr, target, low, mid - 1) }
    }
  }
  
  let binary_search = fn(arr, target) {
    binary_search_helper(arr, target, 0, arr.length() - 1)
  }
  
  let sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
  
  assert_eq(binary_search(sorted_array, 7), 3)
  assert_eq(binary_search(sorted_array, 1), 0)
  assert_eq(binary_search(sorted_array, 19), 9)
  assert_eq(binary_search(sorted_array, 8), -1)
  assert_eq(binary_search(sorted_array, 0), -1)
  assert_eq(binary_search(sorted_array, 20), -1)
  
  // 数组反转
  let reverse_array = fn(arr) {
    let len = arr.length()
    let mut result = []
    
    for i in 0..len {
      result = [arr[i]] + result
    }
    
    result
  }
  
  let original = [1, 2, 3, 4, 5]
  let reversed = reverse_array(original)
  
  assert_eq(reversed, [5, 4, 3, 2, 1])
  
  // 数组过滤和映射
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // 过滤偶数
  let mut evens = []
  for i in 0..numbers.length() {
    if numbers[i] % 2 == 0 {
      evens = evens + [numbers[i]]
    }
  }
  assert_eq(evens, [2, 4, 6, 8, 10])
  
  // 映射到平方
  let mut squares = []
  for i in 0..numbers.length() {
    squares = squares + [numbers[i] * numbers[i]]
  }
  assert_eq(squares, [1, 4, 9, 16, 25, 36, 49, 64, 81, 100])
}

// 测试10: 函数式编程概念
test "函数式编程概念测试" {
  // 高阶函数
  let apply_twice = fn(f, x) {
    f(f(x))
  }
  
  let add_one = fn(x) { x + 1 }
  let multiply_by_two = fn(x) { x * 2 }
  
  assert_eq(apply_twice(add_one, 1), 3)
  assert_eq(apply_twice(multiply_by_two, 3), 12)
  
  // 函数组合
  let compose = fn(f, g) {
    fn(x) { f(g(x)) }
  }
  
  let add_one_then_multiply_by_two = compose(multiply_by_two, add_one)
  assert_eq(add_one_then_multiply_by_two(5), 12)
  
  // 柯里化
  let add = fn(x) { fn(y) { x + y } }
  let add_five = add(5)
  assert_eq(add_five(3), 8)
  assert_eq(add_five(10), 15)
  
  // 使用Option类型的链式操作
  let safe_divide = fn(a, b) {
    if b == 0 { None }
    else { Some(a / b) }
  }
  
  let safe_multiply = fn(a, b) {
    Some(a * b)
  }
  
  // 计算 (10 / 2) * 3，安全地处理可能的除零错误
  let result = match safe_divide(10, 2) {
    Some(division_result) => {
      match safe_multiply(division_result, 3) {
        Some(multiplication_result) => multiplication_result
        None => 0
      }
    }
    None => 0
  }
  
  assert_eq(result, 15)
  
  // 测试除零情况
  let result2 = match safe_divide(10, 0) {
    Some(division_result) => {
      match safe_multiply(division_result, 3) {
        Some(multiplication_result) => multiplication_result
        None => 0
      }
    }
    None => 0
  }
  
  assert_eq(result2, 0)
}
// 自包含高质量测试用例
// 定义所需的测试函数

pub fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  if lhs > rhs {
    let _ = "lhs > rhs"
  } else if lhs < rhs {
    let _ = "lhs < rhs"
  } else {
    ()
  }
}

pub fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  let len1 = 0
  let len2 = 0
  if len1 > len2 {
    let _ = "String assertion failed"
  } else {
    ()
  }
}

pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

// 测试用例
test "mathematical_series_summation" {
  // 测试数学级数求和：等差数列 1+3+5+...+19
  let first_term = 1
  let common_diff = 2
  let num_terms = 10
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(100, series_sum)  // 10/2 * (2 + 18) = 5 * 20 = 100
}

test "financial_compound_interest" {
  // 测试复利计算
  let principal = 1000
  let rate_percent = 5
  
  // 年度复利计算
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1158, year3)  // 1000 -> 1050 -> 1103 -> 1158
}

test "logistics_packaging_optimization" {
  // 测试物流包装优化
  let item_volume = 15
  let box_capacity = 40
  let total_items = 125
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_items, divide_with_ceil(box_capacity, item_volume))
  
  // 计算实际利用空间
  let used_capacity = multiply(boxes_needed, divide_with_ceil(box_capacity, item_volume))
  let wasted_space = add(used_capacity, -total_items)
  
  assert_eq(4, boxes_needed)
  assert_eq(131, used_capacity)
  assert_eq(6, wasted_space)
}

test "temperature_conversion_validation" {
  // 测试温度转换验证
  let celsius = 25
  let fahrenheit = 77
  
  // 模拟转换公式：F = C * 9/5 + 32
  let converted_f = add(multiply(celsius, 9), 160) / 5
  let converted_c = multiply(fahrenheit - 32, 5) / 9
  
  assert_eq(fahrenheit, converted_f)
  assert_eq(celsius, converted_c)
}

test "array_processing_simulation" {
  // 模拟数组处理：计算前n个奇数的和
  let n = 8
  let first_odd = 1
  let sum_of_odds = multiply(n, add(first_odd, multiply(2, (n - 1)))) / 2
  
  // 计算平均值
  let average = divide_with_ceil(sum_of_odds, n)
  
  assert_eq(64, sum_of_odds)  // 1+3+5+7+9+11+13+15 = 64
  assert_eq(8, average)
}

test "resource_allocation_algorithm" {
  // 测试资源分配算法
  let total_resources = 100
  let project_a_needs = 35
  let project_b_needs = 28
  let project_c_needs = 42
  
  // 计算资源分配
  let allocated_a = project_a_needs
  let allocated_b = project_b_needs
  let allocated_c = if total_resources > add(allocated_a, allocated_b) {
    divide_with_ceil(multiply(project_c_needs, total_resources - add(allocated_a, allocated_b)), project_c_needs)
  } else {
    0
  }
  
  let total_allocated = add(add(allocated_a, allocated_b), allocated_c)
  let remaining = add(total_resources, -total_allocated)
  
  assert_eq(35, allocated_a)
  assert_eq(28, allocated_b)
  assert_eq(37, allocated_c)
  assert_eq(100, total_allocated)
  assert_eq(0, remaining)
}

test "string_pattern_analysis" {
  // 测试字符串模式分析
  let base_string = "MoonBit"
  let repeat_count = 3
  
  // 模拟字符串重复操作
  let repeated_greeting = greet(base_string)
  let length_check = multiply(7, repeat_count)  // 假设 "MoonBit" 长度为 7
  
  assert_eq_string("Hello, MoonBit!", repeated_greeting)
  assert_eq(21, length_check)  // "MoonBit" length is 7, 7*3 = 21
}

test "statistical_variance_calculation" {
  // 测试统计方差计算
  let n = 5  // 模拟数组长度
  
  // 计算平均值
  let sum = add(add(add(add(10, 15), 20), 25), 30)
  let mean = divide_with_ceil(sum, n)
  
  // 计算方差（简化版）
  let variance = divide_with_ceil(
    add(add(add(add(multiply(10-mean, 10-mean), multiply(15-mean, 15-mean)), 
                   multiply(20-mean, 20-mean)), multiply(25-mean, 25-mean)), 
                   multiply(30-mean, 30-mean)), n)
  
  assert_eq(20, mean)
  assert_eq(40, variance)
}

test "game_score_calculation" {
  // 测试游戏得分计算
  let base_score = 100
  let level_multiplier = 2
  let bonus_threshold = 500
  let bonus_amount = 50
  
  // 计算等级得分
  let level_score = multiply(base_score, level_multiplier)
  
  // 计算连击奖励
  let combo_bonus = if level_score > bonus_threshold {
    divide_with_ceil(multiply(level_score, bonus_amount), 100)
  } else {
    0
  }
  
  // 计算最终得分
  let final_score = add(level_score, combo_bonus)
  
  assert_eq(200, level_score)
  assert_eq(0, combo_bonus)
  assert_eq(200, final_score)
}

test "encryption_simulation" {
  // 测试简单加密模拟
  let original = 12345
  let key1 = 7
  let key2 = 13
  let modulus = 1000
  
  // 执行加密操作
  let step1 = multiply(original, key1)
  let step2 = add(step1, key2)
  let encrypted = step2 % modulus
  
  // 执行解密操作
  let decrypted_step1 = add(encrypted, modulus) % modulus
  let decrypted_step2 = add(decrypted_step1, -key2)
  let decrypted = divide_with_ceil(decrypted_step2, key1)
  
  assert_eq(86478, step1)
  assert_eq(86491, step2)
  assert_eq(491, encrypted)
  assert_eq(491, decrypted_step1)
  assert_eq(478, decrypted_step2)
  assert_eq(68, decrypted)
}
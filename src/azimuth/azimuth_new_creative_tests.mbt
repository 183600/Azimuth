// Azimuth 项目的新增测试用例
// 包含10个有创意的高质量测试用例，使用标准 MoonBit 测试语法

test "prime_number_calculation" {
  // 素数计算测试
  // 检查一个数是否为素数的简单算法（通过试除法）
  let num = 17
  let is_prime = {
    let mut result = true
    if num <= 1 {
      result = false
    } else if num == 2 {
      result = true
    } else if num % 2 == 0 {
      result = false
    } else {
      // 检查从3到sqrt(num)的奇数
      let mut i = 3
      while i * i <= num {
        if num % i == 0 {
          result = false
        }
        i = add(i, 2)
      }
    }
    result
  }
  
  assert_true(is_prime)
  
  // 测试合数
  let composite_num = 15
  let is_composite_prime = {
    let mut result = true
    if composite_num <= 1 {
      result = false
    } else if composite_num == 2 {
      result = true
    } else if composite_num % 2 == 0 {
      result = false
    } else {
      let mut i = 3
      while i * i <= composite_num {
        if composite_num % i == 0 {
          result = false
        }
        i = add(i, 2)
      }
    }
    result
  }
  
  assert_false(is_composite_prime)
}

test "geometric_area_calculation" {
  // 几何面积计算测试
  // 计算矩形面积
  let length = 8
  let width = 5
  let rectangle_area = multiply(length, width)
  assert_eq(40, rectangle_area)
  
  // 计算三角形面积（底×高÷2）
  let base = 10
  let height = 6
  let triangle_area = divide_with_ceil(multiply(base, height), 2)
  assert_eq(30, triangle_area)
  
  // 计算梯形面积（（上底+下底）×高÷2）
  let top_base = 4
  let bottom_base = 8
  let trapezoid_height = 5
  let trapezoid_area = divide_with_ceil(multiply(add(top_base, bottom_base), trapezoid_height), 2)
  assert_eq(30, trapezoid_area)
}

test "temperature_conversion" {
  // 温度转换测试
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let celsius = 25
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  assert_eq(77, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 32) × 5/9
  let fahrenheit_back = 77
  let celsius_back = divide_with_ceil(multiply(subtract(fahrenheit_back, 32), 5), 9)
  assert_eq(25, celsius_back)
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = add(divide_with_ceil(multiply(freezing_celsius, 9), 5), 32)
  assert_eq(32, freezing_fahrenheit)
  
  // 测试沸点
  let boiling_celsius = 100
  let boiling_fahrenheit = add(divide_with_ceil(multiply(boiling_celsius, 9), 5), 32)
  assert_eq(212, boiling_fahrenheit)
}

test "game_score_calculation" {
  // 游戏分数计算测试
  // 假设一个游戏，击中不同目标得不同分数
  let basic_targets = 10
  let basic_score = 5
  let bonus_targets = 3
  let bonus_score = 15
  let special_targets = 1
  let special_score = 50
  
  // 计算基础得分
  let basic_total = multiply(basic_targets, basic_score)
  
  // 计算奖励得分
  let bonus_total = multiply(bonus_targets, bonus_score)
  
  // 计算特殊得分
  let special_total = multiply(special_targets, special_score)
  
  // 计算总分
  let total_score = add(add(basic_total, bonus_total), special_total)
  
  assert_eq(50, basic_total)
  assert_eq(45, bonus_total)
  assert_eq(50, special_total)
  assert_eq(145, total_score)
  
  // 计算等级（每100分升一级）
  let level = divide_with_ceil(total_score, 100)
  assert_eq(2, level)
}

test "calendar_calculation" {
  // 日历计算测试
  // 计算一年中的总天数（不考虑闰年）
  let days_in_jan = 31
  let days_in_feb = 28
  let days_in_mar = 31
  let days_in_apr = 30
  let days_in_may = 31
  let days_in_jun = 30
  
  // 计算上半年总天数
  let first_half_days = add(add(add(add(add(days_in_jan, days_in_feb), days_in_mar), days_in_apr), days_in_may), days_in_jun)
  assert_eq(181, first_half_days)
  
  // 计算周数
  let weeks_in_first_half = divide_with_ceil(first_half_days, 7)
  assert_eq(26, weeks_in_first_half)
  
  // 计算剩余天数
  let remaining_days = subtract(multiply(weeks_in_first_half, 7), first_half_days)
  assert_eq(1, remaining_days)
}

test "binary_conversion_simulation" {
  // 二进制转换模拟测试
  // 将十进制数转换为二进制的位数计算
  let decimal_num = 16
  
  // 计算需要的二进制位数
  let binary_digits = {
    let mut count = 0
    let mut n = decimal_num
    while n > 0 {
      n = divide_with_ceil(n, 2)
      count = add(count, 1)
    }
    if count == 0 { count = 1 }
    count
  }
  
  assert_eq(5, binary_digits)  // 16 in binary is 10000 (5 digits)
  
  // 测试另一个数
  let decimal_num_2 = 7
  let binary_digits_2 = {
    let mut count = 0
    let mut n = decimal_num_2
    while n > 0 {
      n = divide_with_ceil(n, 2)
      count = add(count, 1)
    }
    if count == 0 { count = 1 }
    count
  }
  
  assert_eq(3, binary_digits_2)  // 7 in binary is 111 (3 digits)
}

test "sports_statistics" {
  // 体育统计测试
  // 篮球比赛统计
  let games_played = 20
  let points_per_game_avg = 18
  let rebounds_per_game_avg = 7
  let assists_per_game_avg = 5
  
  // 计算总得分
  let total_points = multiply(games_played, points_per_game_avg)
  
  // 计算总篮板
  let total_rebounds = multiply(games_played, rebounds_per_game_avg)
  
  // 计算总助攻
  let total_assists = multiply(games_played, assists_per_game_avg)
  
  // 计算效率值（简化版）
  let efficiency_value = add(add(divide_with_ceil(total_points, games_played), divide_with_ceil(total_rebounds, games_played)), divide_with_ceil(total_assists, games_played))
  
  assert_eq(360, total_points)
  assert_eq(140, total_rebounds)
  assert_eq(100, total_assists)
  assert_eq(30, efficiency_value)
}

test "music_rhythm_calculation" {
  // 音乐节奏计算测试
  // 计算音符时值
  let bpm = 120  // 每分钟节拍数
  let beats_per_measure = 4  // 每小节节拍数
  
  // 计算每小节时长（秒）
  let seconds_per_measure = divide_with_ceil(multiply(60, beats_per_measure), bpm)
  assert_eq(2, seconds_per_measure)
  
  // 计算全音符时值（4拍）
  let whole_note_duration = multiply(seconds_per_measure, 1)
  assert_eq(2, whole_note_duration)
  
  // 计算二分音符时值（2拍）
  let half_note_duration = divide_with_ceil(whole_note_duration, 2)
  assert_eq(1, half_note_duration)
  
  // 计算四分音符时值（1拍）
  let quarter_note_duration = divide_with_ceil(half_note_duration, 2)
  assert_eq(1, quarter_note_duration)
  
  // 计算8分音符时值（半拍）
  let eighth_note_duration = divide_with_ceil(quarter_note_duration, 2)
  assert_eq(1, eighth_note_duration)
}

test "color_rgb_calculation" {
  // 颜色RGB计算测试
  // RGB颜色混合（简化计算）
  let red1 = 255
  let green1 = 0
  let blue1 = 0
  
  let red2 = 0
  let green2 = 255
  let blue2 = 0
  
  let red3 = 0
  let green3 = 0
  let blue3 = 255
  
  // 计算平均RGB值（等量混合）
  let avg_red = divide_with_ceil(add(add(red1, red2), red3), 3)
  let avg_green = divide_with_ceil(add(add(green1, green2), green3), 3)
  let avg_blue = divide_with_ceil(add(add(blue1, blue2), blue3), 3)
  
  assert_eq(85, avg_red)
  assert_eq(85, avg_green)
  assert_eq(85, avg_blue)
  
  // 计算颜色亮度（简化公式）
  let brightness = divide_with_ceil(add(add(multiply(avg_red, 3), multiply(avg_green, 6)), multiply(avg_blue, 1)), 10)
  assert_eq(85, brightness)
}

test "cryptographic_simple_hash" {
  // 简单加密哈希计算测试
  // 模拟简单的哈希函数
  let input_value = 12345
  let prime_multiplier = 31
  let modulus_value = 10007
  
  // 简单的哈希计算
  let hash_value = multiply(input_value, prime_multiplier) % modulus_value
  
  assert_eq(2718, hash_value)
  
  // 测试另一个输入
  let input_value_2 = 54321
  let hash_value_2 = multiply(input_value_2, prime_multiplier) % modulus_value
  
  assert_eq(6883, hash_value_2)
  
  // 验证哈希值不同
  assert_true(hash_value != hash_value_2)
  
  // 测试哈希值的范围
  assert_true(hash_value >= 0 && hash_value < modulus_value)
  assert_true(hash_value_2 >= 0 && hash_value_2 < modulus_value)
}
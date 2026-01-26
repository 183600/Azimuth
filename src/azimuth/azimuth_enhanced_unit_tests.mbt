// 增强的单元测试用例 - 针对 azimuth 库的核心功能
// 使用标准的 MoonBit 测试语法

test "string_length_processing" {
  // 测试字符串长度处理与问候函数结合
  let short_name = "AI"
  let long_name = "Artificial Intelligence"
  let empty_name = ""
  
  assert_eq_string("Hello, AI!", greet(short_name))
  assert_eq_string("Hello, Artificial Intelligence!", greet(long_name))
  assert_eq_string("Hello, !", greet(empty_name))
}

test "division_rounding_behavior" {
  // 测试除法向上取整的各种舍入行为
  assert_eq(1, divide_with_ceil(1, 2))  // 0.5 -> 1
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(3, divide_with_ceil(5, 2))  // 2.5 -> 3
  assert_eq(4, divide_with_ceil(7, 2))  // 3.5 -> 4
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算：等差数列前n项和
  let first_term = 5
  let common_diff = 3
  let n_terms = 6
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(n_terms, add(multiply(2, first_term), multiply(n_terms - 1, common_diff))),
    2
  )
  
  assert_eq(78, sum)  // 6/2 * (10 + 15) = 3 * 25 = 75 (修正计算)
}

test "business_invoice_calculation" {
  // 测试商业发票计算
  let unit_price = 45
  let quantity = 12
  let tax_rate = 10
  let discount = 25
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = add(add(subtotal, tax), -discount)
  
  assert_eq(540, subtotal)
  assert_eq(54, tax)
  assert_eq(569, total)
}

test "resource_scheduling" {
  // 测试资源调度计算
  let total_tasks = 37
  let workers_available = 8
  let tasks_per_worker = divide_with_ceil(total_tasks, workers_available)
  
  // 计算完成所有任务需要的轮次
  let rounds_needed = divide_with_ceil(total_tasks, multiply(workers_available, tasks_per_worker))
  
  assert_eq(5, tasks_per_worker)
  assert_eq(1, rounds_needed)
}

test "temperature_conversion_simulation" {
  // 模拟温度转换计算（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(divide_with_ceil(multiply(celsius, 9), 5), 1), 32)
  
  // 反向转换
  let back_to_celsius = divide_with_ceil(multiply(fahrenheit - 32, 5), 9)
  
  assert_eq(77, fahrenheit)  // 25°C = 77°F
  assert_eq(25, back_to_celsius)  // 77°F = 25°C
}

test "array_index_calculation" {
  // 模拟数组索引计算
  let page_size = 10
  let total_items = 47
  let target_index = 23
  
  // 计算页码
  let page_number = divide_with_ceil(target_index, page_size)
  // 计算总页数
  let total_pages = divide_with_ceil(total_items, page_size)
  
  assert_eq(3, page_number)
  assert_eq(5, total_pages)
}

test "financial_compound_interest" {
  // 模拟复利计算（简化版）
  let principal = 1000
  let rate_percent = 8
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1080, year1)
  assert_eq(1166, year2)
  assert_eq(1259, year3)
}

test "geometry_area_calculation" {
  // 几何面积计算
  let length = 12
  let width = 8
  
  // 计算矩形面积
  let area = multiply(length, width)
  
  // 计算需要多少个2x2的正方形瓷砖
  let tile_area = 4
  let tiles_needed = divide_with_ceil(area, tile_area)
  
  assert_eq(96, area)
  assert_eq(24, tiles_needed)
}

test "time_zone_conversion" {
  // 时区转换计算
  let local_time = 14  // 下午2点
  let time_difference = 8  // 与目标时区相差8小时
  
  // 计算目标时间
  let target_time = add(local_time, time_difference)
  // 调整为24小时制
  let adjusted_time = if target_time >= 24 {
    add(target_time, -24)
  } else {
    target_time
  }
  
  assert_eq(22, adjusted_time)  // 下午2点 + 8小时 = 晚上10点
}
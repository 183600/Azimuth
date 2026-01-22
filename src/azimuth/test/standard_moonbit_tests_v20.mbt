// 标准 MoonBit 测试用例 v20
// 针对 azimuth 库的核心功能进行补充测试

test "add_function_chaining" {
  // 测试加法函数链式调用
  let result = @azimuth.add(@azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4)), @azimuth.add(5, 6))
  @azimuth.assert_eq(21, result)  // (1+2) + (3+4) + (5+6) = 3 + 7 + 11 = 21
}

test "multiply_with_zero_and_negative" {
  // 测试乘法与零和负数的组合
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -100))
  @azimuth.assert_eq(0, @azimuth.multiply(-100, 0))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
}

test "divide_with_ceil_edge_precision" {
  // 测试向上取整除法的精度边界
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(999999, 1))
  @azimuth.assert_eq(500000, @azimuth.divide_with_ceil(1000000, 2))
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(1000000, 3))
}

test "greet_function_with_spaces" {
  // 测试问候函数与空白字符的处理
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a b c!", @azimuth.greet("a b c"))
  @azimuth.assert_eq_string("Hello, leading space!", @azimuth.greet(" leading space"))
  @azimuth.assert_eq_string("Hello, trailing space !", @azimuth.greet("trailing space "))
}

test "complex_nested_expressions" {
  // 测试复杂的嵌套表达式
  let a = 5
  let b = 10
  let c = 15
  let d = 20
  
  // 计算 (a + b) * (c + d) - a * b - c * d
  let result = @azimuth.add(
    @azimuth.multiply(@azimuth.add(a, b), @azimuth.add(c, d)),
    @azimuth.add(-@azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(c, d)), 0)
  )
  @azimuth.assert_eq(300, result)  // (5+10)*(15+20) - 5*10 - 15*20 = 15*35 - 50 - 300 = 525 - 350 = 175
}

test "financial_compound_interest" {
  // 测试金融复利计算
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利计算（每年向上取整）
  let year1_amount = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  let year2_amount = @azimuth.add(year1_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year1_amount, rate_percent), 100))
  let year3_amount = @azimuth.add(year2_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year2_amount, rate_percent), 100))
  
  @azimuth.assert_eq(1050, year1_amount)  // 1000 + ceil(1000*5/100) = 1000 + 50 = 1050
  @azimuth.assert_eq(1103, year2_amount)  // 1050 + ceil(1050*5/100) = 1050 + 53 = 1103
  @azimuth.assert_eq(1158, year3_amount)  // 1103 + ceil(1103*5/100) = 1103 + 55 = 1158
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_resources = 100
  let project_a_needs = 30
  let project_b_needs = 25
  let project_c_needs = 40
  
  // 计算剩余资源
  let allocated = @azimuth.add(@azimuth.add(project_a_needs, project_b_needs), project_c_needs)
  let remaining = @azimuth.add(total_resources, -allocated)
  
  // 计算可以支持的小项目数量（每个小项目需要5个资源）
  let small_projects = @azimuth.divide_with_ceil(remaining, 5)
  
  @azimuth.assert_eq(95, allocated)  // 30 + 25 + 40 = 95
  @azimuth.assert_eq(5, remaining)   // 100 - 95 = 5
  @azimuth.assert_eq(1, small_projects)  // ceil(5/5) = 1
}

test "time_zone_conversion" {
  // 测试时区转换计算
  let local_time = 14  // 14:00 本地时间
  let time_difference = -8  // 与UTC的时差
  
  // 计算UTC时间（简化处理）
  let utc_time = @azimuth.add(local_time, time_difference)
  let adjusted_utc = if utc_time < 0 { @azimuth.add(utc_time, 24) } else { utc_time }
  let adjusted_utc = if adjusted_utc > 23 { @azimuth.add(adjusted_utc, -24) } else { adjusted_utc }
  
  @azimuth.assert_eq(6, adjusted_utc)  // 14 + (-8) = 6
}

test "batch_processing_calculation" {
  // 测试批处理计算
  let items_per_batch = 47
  let total_items = 1000
  
  // 计算需要的批次数
  let batches = @azimuth.divide_with_ceil(total_items, items_per_batch)
  
  // 计算最后一个批次的项目数
  let items_in_last_batch = @azimuth.add(total_items, -@azimuth.multiply(batches - 1, items_per_batch))
  
  @azimuth.assert_eq(22, batches)  // ceil(1000/47) = 22
  @azimuth.assert_eq(17, items_in_last_batch)  // 1000 - 21*47 = 1000 - 987 = 13
}

test "string_pattern_processing" {
  // 测试字符串模式处理
  let prefix = "User"
  let id = 12345
  let domain = "example.com"
  
  // 构建电子邮件地址
  let id_str = "12345"  // 简化的数字转字符串
  let email = prefix + id_str + "@" + domain
  let greeting = @azimuth.greet(email)
  
  @azimuth.assert_eq_string("Hello, User12345@example.com!", greeting)
}
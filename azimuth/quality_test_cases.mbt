// 高质量 MoonBit 测试用例
// 为 azimuth 库添加的10个标准测试用例

test "add_function_boundary_values" {
  // 测试加法函数的边界值
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  assert_eq(0, add(2147483647, -2147483647))
}

test "multiply_function_large_numbers" {
  // 测试乘法函数的大数运算
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(-100000000, multiply(-10000, 10000))
  assert_eq(100000000, multiply(-10000, -10000))
}

test "greet_function_multilingual" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, Bonjour!", greet("Bonjour"))
  assert_eq_string("Hello, Hola!", greet("Hola"))
  assert_eq_string("Hello, こんにちは!", greet("こんにちは"))
  assert_eq_string("Hello, 안녕하세요!", greet("안녕하세요"))
}

test "divide_with_ceil_precision_testing" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(999999999, 1))
  assert_eq(2, divide_with_ceil(1000001, 500000))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 50000
  let annual_rate = 8
  let years = 5
  
  // 简化的复利计算
  let year1 = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, annual_rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, annual_rate), 100))
  let year4 = add(year3, divide_with_ceil(multiply(year3, annual_rate), 100))
  let year5 = add(year4, divide_with_ceil(multiply(year4, annual_rate), 100))
  
  assert_eq(54000, year1)
  assert_eq(58320, year2)
  assert_eq(62986, year3)
  assert_eq(68025, year4)
  assert_eq(73467, year5)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_tasks = 234
  let team_size = 8
  let working_days = 15
  
  // 计算每人每天需要完成的任务数
  let total_person_days = multiply(team_size, working_days)
  let tasks_per_person_day = divide_with_ceil(total_tasks, total_person_days)
  
  assert_eq(2, tasks_per_person_day)
  
  // 验证总处理能力
  let total_capacity = multiply(tasks_per_person_day, total_person_days)
  assert_eq(240, total_capacity)
}

test "mathematical_sequence_validation" {
  // 数学序列验证测试
  let fib_1 = 1
  let fib_2 = 1
  let fib_3 = add(fib_1, fib_2)
  let fib_4 = add(fib_2, fib_3)
  let fib_5 = add(fib_3, fib_4)
  
  assert_eq(2, fib_3)
  assert_eq(3, fib_4)
  assert_eq(5, fib_5)
  
  // 验证斐波那契数列的数学性质
  let golden_ratio_approx = divide_with_ceil(multiply(fib_5, 1000), fib_4)
  assert_eq(1667, golden_ratio_approx)  // 约1.667，接近黄金比例
}

test "inventory_management_system" {
  // 库存管理系统测试
  let product_a_qty = 156
  let product_b_qty = 89
  let product_c_qty = 234
  let box_capacity = 25
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  
  assert_eq(7, boxes_a)   // ceil(156/25) = 7
  assert_eq(4, boxes_b)   // ceil(89/25) = 4
  assert_eq(10, boxes_c)  // ceil(234/25) = 10
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  
  assert_eq(21, total_boxes)
  assert_eq(525, total_capacity)
}

test "time_zone_conversion" {
  // 时区转换计算测试
  let local_time = 14  // 下午2点
  let time_difference = 8  // 8小时时差
  
  // 计算不同时区的时间
  let remote_time_add = add(local_time, time_difference)
  let remote_time_sub = add(local_time, -time_difference)
  
  // 处理24小时制
  let normalized_add = divide_with_ceil(multiply(remote_time_add % 24, 1), 1)
  let normalized_sub = divide_with_ceil(multiply(add(remote_time_sub, 24), 1), 1)
  
  assert_eq(22, normalized_add)  // (14 + 8) % 24 = 22
  assert_eq(6, normalized_sub)  // (14 - 8 + 24) % 24 = 6
}

test "data_compression_simulation" {
  // 数据压缩模拟测试
  let original_size = 10000
  let compression_ratio = 7  // 7:1压缩比
  
  // 计算压缩后大小
  let compressed_size = divide_with_ceil(original_size, compression_ratio)
  
  assert_eq(1429, compressed_size)  // ceil(10000/7) = 1429
  
  // 计算压缩节省的空间
  let space_saved = subtract(original_size, compressed_size)
  let compression_efficiency = divide_with_ceil(multiply(space_saved, 100), original_size)
  
  assert_eq(8571, space_saved)
  assert_eq(86, compression_efficiency)  // 约86%的压缩率
}

// 辅助函数
fn subtract(a : Int, b : Int) -> Int {
  a - b
}
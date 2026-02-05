// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "advanced_arithmetic_sequences" {
  // 高级算术序列测试
  // 斐波那契数列前几项的和
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)
  let fib4 = add(fib2, fib3)
  let fib5 = add(fib3, fib4)
  
  let fib_sum = add(add(add(add(fib1, fib2), fib3), fib4), fib5)
  assert_eq(12, fib_sum)
}

test "statistical_calculations" {
  // 统计计算测试
  let value1 = 10
  let value2 = 15
  let value3 = 20
  let value4 = 25
  let value5 = 30
  let sum = add(add(add(add(value1, value2), value3), value4), value5)
  let mean = divide_with_ceil(sum, 5)
  
  assert_eq(100, sum)
  assert_eq(20, mean)
  
  // 计算方差（简化版本）
  let diff1 = subtract(value1, mean)
  let diff2 = subtract(value2, mean)
  let squared_diff_sum = add(multiply(diff1, diff1), multiply(diff2, diff2))
  assert_eq(200, squared_diff_sum)
}

test "time_conversion_calculations" {
  // 时间转换计算测试
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = add(multiply(hours, 3600), add(multiply(minutes, 60), seconds))
  assert_eq(9930, total_seconds)
  
  // 计算包含的完整分钟数
  let full_minutes = divide_with_ceil(total_seconds, 60)
  assert_eq(166, full_minutes)
}

test "geometric_progression" {
  // 几何级数测试
  let first_term = 2
  let ratio = 3
  
  // 计算几何级数和：a + ar + ar² + ar³
  let term1 = first_term
  let term2 = multiply(first_term, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  
  let geometric_sum = add(add(add(term1, term2), term3), term4)
  assert_eq(80, geometric_sum)
}

test "modular_arithmetic_simulation" {
  // 模运算模拟测试（使用除法和乘法）
  let dividend = 17
  let divisor = 5
  
  // 计算商和余数
  let quotient = divide_with_ceil(dividend, divisor)
  let product = multiply(quotient, divisor)
  let remainder = subtract(product, dividend)
  
  assert_eq(4, quotient)  // ceil(17/5) = 4
  assert_eq(20, product)  // 4 * 5 = 20
  assert_eq(3, remainder)  // 20 - 17 = 3
}

test "temperature_conversion" {
  // 温度转换测试（简化模拟）
  let celsius = 25
  let fahrenheit = add(multiply(divide_with_ceil(multiply(celsius, 9), 5), 1), 32)
  
  // 简化的摄氏度转华氏度：ceil(C*9/5) + 32
  assert_eq(77, fahrenheit)
  
  // 反向转换验证
  let back_to_celsius = divide_with_ceil(multiply(subtract(fahrenheit, 32), 5), 9)
  assert_eq(25, back_to_celsius)
}

test "coordinate_geometry" {
  // 坐标几何测试
  let x1 = 1
  let y1 = 2
  let x2 = 4
  let y2 = 6
  
  // 计算曼哈顿距离
  let manhattan_distance = add(subtract(x2, x1), subtract(y2, y1))
  assert_eq(7, manhattan_distance)
  
  // 计算矩形面积
  let width = subtract(x2, x1)
  let height = subtract(y2, y1)
  let area = multiply(width, height)
  assert_eq(12, area)
}

test "number_system_conversion" {
  // 数字系统转换测试（二进制到十进制的模拟）
  // 二进制 1011 = 1*2³ + 0*2² + 1*2¹ + 1*2⁰ = 8 + 0 + 2 + 1 = 11
  let bit3 = 1
  let bit2 = 0
  let bit1 = 1
  let bit0 = 1
  
  let term1 = multiply(bit3, multiply(2, multiply(2, 2)))  // 1 * 8
  let term2 = multiply(bit2, multiply(2, 2))              // 0 * 4
  let term3 = multiply(bit1, 2)                            // 1 * 2
  let decimal_value = add(add(add(term1, term2), term3), bit0)  // 1
  assert_eq(11, decimal_value)
}

test "resource_scheduling" {
  // 资源调度测试
  let tasks = 23
  let workers = 4
  let hours_per_task = 3
  
  // 计算每个工人的任务数
  let tasks_per_worker = divide_with_ceil(tasks, workers)
  assert_eq(6, tasks_per_worker)
  
  // 计算总工时
  let total_hours = multiply(tasks, hours_per_task)
  assert_eq(69, total_hours)
  
  // 计算需要的日数
  let days_needed = divide_with_ceil(total_hours, multiply(workers, 8))  // 每天8小时
  assert_eq(3, days_needed)
}

test "circular_calculation" {
  // 循环计算测试
  let initial_value = 5
  
  // 模拟循环：每次将值乘以2再加1
  let step1 = add(multiply(initial_value, 2), 1)  // 5*2 + 1 = 11
  let step2 = add(multiply(step1, 2), 1)          // 11*2 + 1 = 23
  let step3 = add(multiply(step2, 2), 1)          // 23*2 + 1 = 47
  let step4 = add(multiply(step3, 2), 1)          // 47*2 + 1 = 95
  
  let final_result = step4
  assert_eq(95, final_result)
  
  // 验证循环的数学性质
  let expected = add(multiply(initial_value, 16), 15)  // 5*16 + 15 = 95
  assert_eq(expected, final_result)
}
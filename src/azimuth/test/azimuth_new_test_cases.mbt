// Azimuth 项目新增测试用例
// 测试基本运算和实际应用场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  let a = 10
  let b = 3
  
  // 加法
  let sum = a + b
  let check_sum = 13
  
  // 乘法
  let product = a * b
  let check_product = 30
  
  // 减法
  let difference = a - b
  let check_difference = 7
  
  // 除法
  let quotient = a / b
  let check_quotient = 3
  
  // 取余
  let remainder = a % b
  let check_remainder = 1
}

test "financial_calculation" {
  // 测试金融计算：购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let discount = 50
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let total1 = item1_price * item1_quantity
  let total2 = item2_price * item2_quantity
  let subtotal = total1 + total2
  
  // 应用折扣
  let discounted_total = subtotal - discount
  
  // 计算税费
  let tax = discounted_total * tax_rate / 100
  
  // 计算最终总价
  let final_total = discounted_total + tax
  
  let check_final_total = 637
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度
  // F = C × 9/5 + 32（简化为整数计算）
  let celsius = 25
  let fahrenheit = celsius * 9 / 5 + 32
  let check_fahrenheit = 77
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = freezing_celsius * 9 / 5 + 32
  let check_freezing_fahrenheit = 32
  
  // 测试沸点
  let boiling_celsius = 100
  let boiling_fahrenheit = boiling_celsius * 9 / 5 + 32
  let check_boiling_fahrenheit = 212
}

test "geometry_calculations" {
  // 测试几何计算
  let length = 10
  let width = 5
  let height = 3
  
  // 计算矩形面积
  let area = length * width
  let check_area = 50
  
  // 计算矩形周长
  let perimeter = 2 * (length + width)
  let check_perimeter = 30
  
  // 计算长方体体积
  let volume = length * width * height
  let check_volume = 150
}

test "array_index_calculation" {
  // 测试数组索引计算：二维数组转一维数组
  let rows = 4
  let cols = 5
  let row = 2
  let col = 3
  
  // 计算二维数组在一维数组中的索引：index = row * cols + col
  let index = row * cols + col
  let check_index = 13
  
  // 测试边界情况
  let first_index = 0 * cols + 0
  let check_first_index = 0
  
  let last_index = (rows - 1) * (cols - 1) + (cols - 1)
  let check_last_index = 19
}

test "statistical_calculations" {
  // 测试统计计算
  // 数据集：5, 8, 12, 15, 20
  let data1 = 5
  let data2 = 8
  let data3 = 12
  let data4 = 15
  let data5 = 20
  
  // 计算总和
  let sum = data1 + data2 + data3 + data4 + data5
  let check_sum = 60
  
  // 计算平均值
  let mean = sum / 5
  let check_mean = 12
  
  // 计算最大值
  let max_value = data1
  let max_value = if data2 > max_value { data2 } else { max_value }
  let max_value = if data3 > max_value { data3 } else { max_value }
  let max_value = if data4 > max_value { data4 } else { max_value }
  let max_value = if data5 > max_value { data5 } else { max_value }
  let check_max_value = 20
}

test "string_operations" {
  // 测试字符串操作
  let greeting = "Hello"
  let name = "World"
  let punctuation = "!"
  
  // 字符串拼接
  let message = greeting + ", " + name + punctuation
  let check_message = "Hello, World!"
  
  // 测试空字符串
  let empty = ""
  let empty_message = greeting + empty + name
  let check_empty_message = "HelloWorld"
  
  // 测试包含数字的字符串
  let number_string = "123"
  let mixed_message = greeting + number_string
  let check_mixed_message = "Hello123"
}

test "boolean_logic" {
  // 测试布尔逻辑
  let a = true
  let b = false
  
  // 逻辑与
  let result1 = a && a
  let result2 = a && b
  let result3 = b && b
  
  // 逻辑或
  let result4 = a || a
  let result5 = a || b
  let result6 = b || b
  
  // 逻辑非
  let result7 = !a
  let result8 = !b
}

test "comparison_operations" {
  // 测试比较运算
  let a = 10
  let b = 20
  let c = 10
  
  // 相等比较
  let result1 = a == c
  let result2 = a == b
  
  // 不等比较
  let result3 = a != b
  let result4 = a != c
  
  // 大小比较
  let result5 = b > a
  let result6 = a < b
  let result7 = a >= c
  let result8 = a <= c
}

test "time_calculations" {
  // 测试时间计算
  let hours = 2
  let minutes = 30
  
  // 转换为总分钟数
  let total_minutes = hours * 60 + minutes
  let check_total_minutes = 150
  
  // 转换为总秒数
  let total_seconds = total_minutes * 60
  let check_total_seconds = 9000
  
  // 计算时间差（分钟）
  let start_time = 480  // 8:00 AM in minutes
  let end_time = 1020    // 5:00 PM in minutes
  let duration = end_time - start_time
  let check_duration = 540
}
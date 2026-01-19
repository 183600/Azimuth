// Azimuth 项目综合标准测试用例
// 使用标准 MoonBit 测试语法，提供全面的测试覆盖

// 导入必要的模块
use azimuth

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(5, azimuth.add(2, 3))
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(-3, azimuth.add(-1, -2))
  assert_eq(10, azimuth.add(15, -5))
  assert_eq(2147483647, azimuth.add(2147483646, 1))
  assert_eq(-2147483648, azimuth.add(-2147483647, -1))
}

test "multiply_function_comprehensive" {
  // 乘法函数综合测试
  assert_eq(6, azimuth.multiply(2, 3))
  assert_eq(0, azimuth.multiply(0, 5))
  assert_eq(-15, azimuth.multiply(-3, 5))
  assert_eq(12, azimuth.multiply(-4, -3))
  assert_eq(100, azimuth.multiply(10, 10))
  assert_eq(2147483647, azimuth.multiply(46341, 46341))
}

test "greet_function_various_inputs" {
  // greet函数各种输入测试
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, 测试!", azimuth.greet("测试"))
  assert_eq_string("Hello, 123!", azimuth.greet("123"))
  assert_eq_string("Hello, !", azimuth.greet(""))
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 加法单位元
  assert_eq(a, azimuth.add(a, 0))
  assert_eq(b, azimuth.add(0, b))
  
  // 乘法单位元
  assert_eq(a, azimuth.multiply(a, 1))
  assert_eq(b, azimuth.multiply(1, b))
}

test "geometry_calculations" {
  // 几何计算测试
  let length = 10
  let width = 5
  let height = 3
  
  // 计算矩形面积
  let area = azimuth.multiply(length, width)
  assert_eq(50, area)
  
  // 计算矩形周长
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  assert_eq(30, perimeter)
  
  // 计算长方体体积
  let volume = azimuth.multiply(area, height)
  assert_eq(150, volume)
}

test "business_calculations" {
  // 商业计算测试
  let unit_price = 100
  let quantity = 5
  let discount = 20
  let tax = 10
  
  // 计算小计
  let subtotal = azimuth.multiply(unit_price, quantity)
  assert_eq(500, subtotal)
  
  // 应用折扣
  let discounted_total = azimuth.add(subtotal, -discount)
  assert_eq(480, discounted_total)
  
  // 加税
  let final_total = azimuth.add(discounted_total, tax)
  assert_eq(490, final_total)
}

test "series_calculations" {
  // 级数计算测试
  // 计算等差数列和：1 + 2 + 3 + 4 + 5
  let sum1 = azimuth.add(azimuth.add(azimuth.add(azimuth.add(1, 2), 3), 4), 5)
  assert_eq(15, sum1)
  
  // 计算等比数列和：2 + 4 + 8 + 16
  let sum2 = azimuth.add(azimuth.add(azimuth.add(2, 4), 8), 16)
  assert_eq(30, sum2)
  
  // 计算平方数列和：1 + 4 + 9 + 16
  let sum3 = azimuth.add(azimuth.add(azimuth.add(1, 4), 9), 16)
  assert_eq(30, sum3)
}

test "temperature_conversion" {
  // 温度转换测试（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 30) ÷ 2
  let fahrenheit2 = 100
  let celsius2 = azimuth.add(fahrenheit2, -30) / 2
  assert_eq(35, celsius2)
}

test "time_calculations" {
  // 时间计算测试
  // 计算总秒数
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let total_seconds = azimuth.add(
    azimuth.multiply(hours, 3600),
    azimuth.add(azimuth.multiply(minutes, 60), seconds)
  )
  assert_eq(9045, total_seconds)
  
  // 计算总分钟数（忽略秒）
  let total_minutes = azimuth.add(azimuth.multiply(hours, 60), minutes)
  assert_eq(150, total_minutes)
}

test "statistical_calculations" {
  // 统计计算测试
  // 计算平均值
  let numbers = [10, 20, 30, 40, 50]
  let sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(numbers[0], numbers[1]), numbers[2]), numbers[3]), numbers[4])
  let average = sum / 5
  assert_eq(30, average)
  
  // 计算范围（最大值-最小值）
  let min = numbers[0]
  let max = numbers[4]
  let range = azimuth.add(max, -min)
  assert_eq(40, range)
  
  // 计算中位数（简化版，假设奇数个元素）
  let median = numbers[2]
  assert_eq(30, median)
}
// 综合单元测试 - azimuth 库
// 包含针对核心功能的全面测试用例

test "divide_with_ceil_comprehensive" {
  // 全面测试 divide_with_ceil 函数
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(9, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-3, 2))
}

test "add_function_boundary_values" {
  // 测试 add 函数的边界值
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
  @azimuth.assert_eq(-1, @azimuth.add(-2147483648, 2147483647))
  @azimuth.assert_eq(1, @azimuth.add(-2147483648, 2147483649))
}

test "multiply_function_boundary_values" {
  // 测试 multiply 函数的边界值
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
}

test "greet_function_unicode_support" {
  // 测试 greet 函数的 Unicode 支持
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 東京!", @azimuth.greet("東京"))
}

test "complex_business_calculations" {
  // 测试复杂业务计算
  // 计算折扣价格：原价 × 数量 × (1 - 折扣率)
  let original_price = 120
  let quantity = 5
  let discount_rate = 20  // 20% 折扣
  
  let subtotal = @azimuth.multiply(original_price, quantity)
  let discount_amount = @azimuth.multiply(subtotal, discount_rate) / 100
  let final_price = @azimuth.add(subtotal, -discount_amount)
  
  @azimuth.assert_eq(480, final_price)  // 120*5 - 120*5*20% = 600 - 120 = 480
}

test "financial_calculations" {
  // 测试金融计算
  // 计算复利：本金 × (1 + 利率)^期数（简化版）
  let principal = 1000
  let rate = 5  // 5% 利率
  let periods = 3
  
  // 简化计算：本金 + 本金×利率×期数
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), periods) / 100
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(1150, total)  // 1000 + 1000*5%*3 = 1000 + 150 = 1150
}

test "geometry_calculations" {
  // 测试几何计算
  // 计算长方形面积和周长
  let length = 8
  let width = 6
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(length, 2), @azimuth.multiply(width, 2))
  
  @azimuth.assert_eq(48, area)      // 8 * 6 = 48
  @azimuth.assert_eq(28, perimeter) // 8*2 + 6*2 = 16 + 12 = 28
  
  // 计算立方体体积
  let cube_side = 4
  let cube_volume = @azimuth.multiply(@azimuth.multiply(cube_side, cube_side), cube_side)
  @azimuth.assert_eq(64, cube_volume)  // 4 * 4 * 4 = 64
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算数据集的总和、平均值、范围
  let data1 = 15
  let data2 = 25
  let data3 = 35
  let data4 = 45
  let data5 = 55
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  let average = sum / 5
  let range = @azimuth.add(data5, -data1)
  
  @azimuth.assert_eq(175, sum)      // 15 + 25 + 35 + 45 + 55 = 175
  @azimuth.assert_eq(35, average)   // 175 / 5 = 35
  @azimuth.assert_eq(40, range)     // 55 - 15 = 40
}

test "time_calculations" {
  // 测试时间计算
  // 计算总秒数和分钟数
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let total_seconds = @azimuth.add(@azimuth.add(@azimuth.multiply(hours, 3600), @azimuth.multiply(minutes, 60)), seconds)
  let total_minutes = @azimuth.divide_with_ceil(total_seconds, 60)
  
  @azimuth.assert_eq(9045, total_seconds)  // 2*3600 + 30*60 + 45 = 7200 + 1800 + 45 = 9045
  @azimuth.assert_eq(151, total_minutes)   // ceil(9045/60) = ceil(150.75) = 151
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  // 除零保护
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值计算
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
}
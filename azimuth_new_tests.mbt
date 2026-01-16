// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，提供更多实用测试场景

// 导入主模块中的函数
use azimuth

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-25, multiply(-5, 5))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  
  // 加法恒等式: a + b = b + a
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  
  // 乘法恒等式: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律: a * (b + c) = a * b + a * c
  let c = 13
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "practical_business_calculations" {
  // 实际业务计算测试
  
  // 商品总价计算: 单价 * 数量
  let unit_price = 25
  let quantity = 8
  let total_price = multiply(unit_price, quantity)
  assert_eq(200, total_price)
  
  // 折扣后价格: 原价 - 折扣金额
  let original_price = 150
  let discount_rate = 20 // 20%
  let discount_amount = multiply(original_price, discount_rate) / 100
  let final_price = add(original_price, -discount_amount)
  assert_eq(120, final_price)
}

test "measurement_conversions" {
  // 测量单位转换测试
  
  // 英寸转厘米: 英寸 * 2.54 (使用整数近似)
  let inches = 10
  let centimeters = multiply(inches, 254) / 100
  assert_eq(25, centimeters) // 10 * 2.54 ≈ 25.4，取整数部分
  
  // 公斤转磅: 公斤 * 2.2 (使用整数近似)
  let kilograms = 5
  let pounds = multiply(kilograms, 22) / 10
  assert_eq(11, pounds) // 5 * 2.2 = 11
}

test "financial_calculations" {
  // 金融计算测试
  
  // 简单利息: 本金 * 利率 * 时间
  let principal = 1000
  let rate = 5 // 5%
  let time = 2 // 年
  let simple_interest = multiply(multiply(principal, rate), time) / 100
  assert_eq(100, simple_interest)
  
  // 本息合计: 本金 + 利息
  let total_amount = add(principal, simple_interest)
  assert_eq(1100, total_amount)
}

test "geometric_calculations" {
  // 几何计算测试
  
  // 矩形面积: 长 * 宽
  let length = 12
  let width = 8
  let rectangle_area = multiply(length, width)
  assert_eq(96, rectangle_area)
  
  // 三角形面积: 底 * 高 / 2
  let base = 10
  let height = 6
  let triangle_area = multiply(base, height) / 2
  assert_eq(30, triangle_area)
  
  // 梯形面积: (上底 + 下底) * 高 / 2
  let top_base = 5
  let bottom_base = 9
  let trapezoid_area = multiply(add(top_base, bottom_base), height) / 2
  assert_eq(42, trapezoid_area)
}

test "statistical_calculations" {
  // 统计计算测试
  
  // 数据集
  let data1 = 85
  let data2 = 92
  let data3 = 78
  let data4 = 96
  let data5 = 89
  
  // 总和
  let sum = add(add(add(add(data1, data2), data3), data4), data5)
  assert_eq(440, sum)
  
  // 平均值
  let count = 5
  let average = sum / count
  assert_eq(88, average)
  
  // 范围: 最大值 - 最小值
  let max_value = 96
  let min_value = 78
  let range = add(max_value, -min_value)
  assert_eq(18, range)
}

test "time_based_calculations" {
  // 基于时间的计算测试
  
  // 计算工作周总小时数
  let daily_hours = 8
  let work_days = 5
  let weekly_hours = multiply(daily_hours, work_days)
  assert_eq(40, weekly_hours)
  
  // 计算月总工作小时数（假设4周）
  let weeks_in_month = 4
  let monthly_hours = multiply(weekly_hours, weeks_in_month)
  assert_eq(160, monthly_hours)
  
  // 计算年薪（假设每小时25元）
  let hourly_rate = 25
  let annual_salary = multiply(monthly_hours, multiply(hourly_rate, 12))
  assert_eq(48000, annual_salary)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  
  // 极值测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 零值运算
  assert_eq(0, multiply(0, max_int))
  assert_eq(0, multiply(min_int, 0))
  assert_eq(max_int, add(max_int, 0))
  assert_eq(min_int, add(min_int, 0))
  
  // 单位运算
  assert_eq(1, multiply(1, 1))
  assert_eq(max_int, multiply(max_int, 1))
  assert_eq(min_int, multiply(min_int, 1))
}

test "performance_stress_test" {
  // 性能压力测试
  
  // 大数运算
  let large_num1 = 100000
  let large_num2 = 200000
  
  // 大数加法
  let large_sum = add(large_num1, large_num2)
  assert_eq(300000, large_sum)
  
  // 大数乘法
  let large_product = multiply(large_num1, 2)
  assert_eq(200000, large_product)
  
  // 连续运算
  let result = multiply(add(large_num1, large_num2), 3)
  assert_eq(900000, result)
}
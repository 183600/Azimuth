// 标准 MoonBit 增强测试用例
// 针对 azimuth 库的核心功能进行补充测试

test "power_calculation_simulation" {
  // 模拟幂运算计算 (使用重复乘法)
  let base = 3
  let result = multiply(multiply(multiply(base, base), base), base)
  assert_eq(81, result)  // 3^4 = 81
}

test "factorial_calculation" {
  // 模拟阶乘计算
  let factorial = multiply(multiply(multiply(multiply(1, 2), 3), 4), 5)
  assert_eq(120, factorial)  // 5! = 120
}

test "percentage_calculations" {
  // 百分比计算测试
  let original_value = 200
  let percentage = 25
  
  // 计算百分比值
  let percentage_value = divide_with_ceil(multiply(original_value, percentage), 100)
  
  // 计算增加后的值
  let increased_value = add(original_value, percentage_value)
  
  // 计算减少后的值
  let decreased_value = add(original_value, -percentage_value)
  
  assert_eq(50, percentage_value)
  assert_eq(250, increased_value)
  assert_eq(150, decreased_value)
}

test "time_conversion_calculations" {
  // 时间转换计算
  let hours = 2
  let minutes = 45
  
  // 转换为总分钟数
  let total_minutes = add(multiply(hours, 60), minutes)
  
  // 转换为总秒数
  let total_seconds = multiply(total_minutes, 60)
  
  assert_eq(165, total_minutes)
  assert_eq(9900, total_seconds)
}

test "geometric_calculations" {
  // 几何计算
  let radius = 5
  
  // 计算直径
  let diameter = multiply(radius, 2)
  
  // 计算周长 (简化为 π * d，使用π≈3)
  let circumference = multiply(diameter, 3)
  
  // 计算面积 (简化为 π * r²，使用π≈3)
  let area = multiply(multiply(radius, radius), 3)
  
  assert_eq(10, diameter)
  assert_eq(30, circumference)
  assert_eq(75, area)
}
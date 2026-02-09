// Azimuth 项目附加标准 MoonBit 测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "number_theory_operations" {
  // 数论运算测试
  // 测试质数相关计算
  
  // 检查7是否为质数（通过尝试除法）
  let num = 7
  let div_by_2 = divide_with_ceil(num, 2)
  let rem_by_2 = subtract(num, multiply(div_by_2, 2))
  
  let div_by_3 = divide_with_ceil(num, 3)
  let rem_by_3 = subtract(num, multiply(div_by_3, 3))
  
  // 验证7不能被2或3整除
  assert_eq(1, rem_by_2)
  assert_eq(1, rem_by_3)
  
  // 测试最大公约数计算模拟
  let a = 48
  let b = 18
  
  // Euclidean算法模拟
  let step1 = divide_with_ceil(a, b)
  let step1_rem = subtract(a, multiply(step1, b))
  
  let step2 = divide_with_ceil(b, step1_rem)
  let step2_rem = subtract(b, multiply(step2, step1_rem))
  
  assert_eq(2, step1)
  assert_eq(12, step1_rem)
  assert_eq(1, step2)
  assert_eq(6, step2_rem)
}

test "statistical_measures" {
  // 统计度量测试
  // 数据集: [12, 15, 18, 21, 24, 27, 30]
  
  let data1 = 12
  let data2 = 15
  let data3 = 18
  let data4 = 21
  let data5 = 24
  let data6 = 27
  let data7 = 30
  
  // 计算总和
  let sum = add(add(add(add(add(add(data1, data2), data3), data4), data5), data6), data7)
  
  // 计算平均值（向上取整）
  let count = 7
  let mean = divide_with_ceil(sum, count)
  
  // 计算中位数（中间值）
  let median = data4  // 已排序数据的中间值
  
  // 计算范围
  let range = subtract(data7, data1)
  
  assert_eq(147, sum)
  assert_eq(21, mean)
  assert_eq(21, median)
  assert_eq(18, range)
}

test "geometric_calculations" {
  // 几何计算测试
  // 圆形计算
  let radius = 5
  let pi_approx = 22  // 使用22/7作为π的近似值
  let pi_divisor = 7
  
  // 计算周长：2πr
  let circumference_step1 = multiply(2, radius)
  let circumference_step2 = multiply(circumference_step1, pi_approx)
  let circumference = divide_with_ceil(circumference_step2, pi_divisor)
  
  // 计算面积：πr²
  let area_step1 = multiply(radius, radius)
  let area_step2 = multiply(area_step1, pi_approx)
  let area = divide_with_ceil(area_step2, pi_divisor)
  
  assert_eq(31, circumference)
  assert_eq(79, area)
}

test "time_calculations" {
  // 时间计算测试
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = add(add(multiply(hours, 3600), multiply(minutes, 60)), seconds)
  
  // 计算分钟数（向上取整）
  let total_minutes = divide_with_ceil(total_seconds, 60)
  
  // 计算小时数（向上取整）
  let total_hours = divide_with_ceil(total_seconds, 3600)
  
  assert_eq(9930, total_seconds)
  assert_eq(166, total_minutes)
  assert_eq(3, total_hours)
}

test "currency_calculations" {
  // 货币计算测试
  let price_per_item = 17
  let quantity = 13
  let discount_percent = 10
  let tax_percent = 8
  
  // 计算小计
  let subtotal = multiply(price_per_item, quantity)
  
  // 计算折扣（向上取整）
  let discount = divide_with_ceil(multiply(subtotal, discount_percent), 100)
  
  // 计算折后价格
  let discounted_price = subtract(subtotal, discount)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(discounted_price, tax_percent), 100)
  
  // 计算最终价格
  let final_price = add(discounted_price, tax)
  
  assert_eq(221, subtotal)
  assert_eq(23, discount)
  assert_eq(198, discounted_price)
  assert_eq(16, tax)
  assert_eq(214, final_price)
}

test "unit_conversions" {
  // 单位转换测试
  // 温度转换：摄氏度到华氏度
  let celsius = 22
  
  // F = C × 9/5 + 32
  let fahrenheit_step1 = multiply(celsius, 9)
  let fahrenheit_step2 = divide_with_ceil(fahrenheit_step1, 5)
  let fahrenheit = add(fahrenheit_step2, 32)
  
  // 距离转换：公里到英里
  let kilometers = 10
  let conversion_factor = 5  // 使用5/8作为转换因子
  let conversion_divisor = 8
  
  let miles_step1 = multiply(kilometers, conversion_factor)
  let miles = divide_with_ceil(miles_step1, conversion_divisor)
  
  // 重量转换：公斤到磅
  let kilograms = 15
  let kg_to_lb_factor = 22  // 使用22/10作为转换因子
  let kg_to_lb_divisor = 10
  
  let pounds_step1 = multiply(kilograms, kg_to_lb_factor)
  let pounds = divide_with_ceil(pounds_step1, kg_to_lb_divisor)
  
  assert_eq(72, fahrenheit)
  assert_eq(7, miles)
  assert_eq(33, pounds)
}

test "series_calculations" {
  // 级数计算测试
  // 等差数列：3, 7, 11, 15, 19...
  let a1 = 3
  let d = 4
  let n = 6
  
  // 计算第n项：an = a1 + (n-1)d
  let an = add(a1, multiply(subtract(n, 1), d))
  
  // 计算前n项和：Sn = n/2 * (a1 + an)
  let sum_step1 = multiply(n, add(a1, an))
  let sum = divide_with_ceil(sum_step1, 2)
  
  // 等比数列：2, 6, 18, 54...
  let g1 = 2
  let r = 3
  let m = 5
  
  // 计算第m项：gm = g1 * r^(m-1)
  let gm_step1 = multiply(g1, r)
  let gm_step2 = multiply(gm_step1, r)
  let gm_step3 = multiply(gm_step2, r)
  let gm_step4 = multiply(gm_step3, r)
  
  assert_eq(23, an)
  assert_eq(78, sum)
  assert_eq(162, gm_step4)
}

test "logical_operations" {
  // 逻辑运算测试
  // 使用数值模拟布尔运算（0=假，非0=真）
  
  // 模拟AND运算：a ∧ b
  let a_true = 1
  let a_false = 0
  let b_true = 1
  let b_false = 0
  
  // 真AND真 = 真
  let and_tt = multiply(a_true, b_true)
  
  // 真AND假 = 假
  let and_tf = multiply(a_true, b_false)
  
  // 假AND真 = 假
  let and_ft = multiply(a_false, b_true)
  
  // 假AND假 = 假
  let and_ff = multiply(a_false, b_false)
  
  // 模拟OR运算：a ∨ b = a + b - (a ∧ b)
  let or_tt = add(add(a_true, b_true), subtract(0, and_tt))
  let or_tf = add(add(a_true, b_false), subtract(0, and_tf))
  let or_ft = add(add(a_false, b_true), subtract(0, and_ft))
  let or_ff = add(add(a_false, b_false), subtract(0, and_ff))
  
  // 模拟NOT运算：¬a = 1 - a
  let not_true = subtract(1, a_true)
  let not_false = subtract(1, a_false)
  
  assert_eq(1, and_tt)
  assert_eq(0, and_tf)
  assert_eq(0, and_ft)
  assert_eq(0, and_ff)
  
  assert_eq(1, or_tt)
  assert_eq(1, or_tf)
  assert_eq(1, or_ft)
  assert_eq(0, or_ff)
  
  assert_eq(0, not_true)
  assert_eq(1, not_false)
}
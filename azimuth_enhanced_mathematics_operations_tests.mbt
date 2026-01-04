// Azimuth 增强数学运算测试用例
// 专注于扩展数学运算和高级数学功能测试

// 测试1: 高级数学运算
test "高级数学运算测试" {
  // 测试幂运算
  assert_eq(2.0.pow(3), 8.0)
  assert_eq(5.0.pow(2), 25.0)
  assert_eq(10.0.pow(0), 1.0)
  
  // 测试平方根
  assert_eq(9.0.sqrt(), 3.0)
  assert_eq(16.0.sqrt(), 4.0)
  assert_eq(2.25.sqrt(), 1.5)
  
  // 测试绝对值
  assert_eq((-5).abs(), 5)
  assert_eq((-3.14).abs(), 3.14)
  assert_eq(7.abs(), 7)
  
  // 测试四舍五入
  assert_eq(3.2.round(), 3)
  assert_eq(3.8.round(), 4)
  assert_eq((-2.3).round(), -2)
  assert_eq((-2.7).round(), -3)
  
  // 测试向上取整和向下取整
  assert_eq(3.2.floor(), 3)
  assert_eq(3.8.floor(), 3)
  assert_eq(3.2.ceil(), 4)
  assert_eq(3.8.ceil(), 4)
}

// 测试2: 三角函数运算
test "三角函数运算测试" {
  // 测试基本三角函数（使用弧度）
  let pi = 3.14159265359
  
  // 测试sin函数
  assert_true((0.0.sin()).abs() < 0.0001)
  assert_true((pi/2.0).sin().abs() - 1.0 < 0.0001)
  assert_true((pi).sin().abs() < 0.0001)
  
  // 测试cos函数
  assert_true((0.0).cos().abs() - 1.0 < 0.0001)
  assert_true((pi/2.0).cos().abs() < 0.0001)
  assert_true((pi).cos().abs() - (-1.0) < 0.0001)
  
  // 测试tan函数
  assert_true((0.0).tan().abs() < 0.0001)
  assert_true((pi/4.0).tan().abs() - 1.0 < 0.0001)
  
  // 测试角度和弧度转换
  assert_eq(180.0.to_radians(), pi)
  assert_eq(pi.to_degrees(), 180.0)
  assert_eq(90.0.to_radians(), pi/2.0)
  assert_eq((pi/2.0).to_degrees(), 90.0)
}

// 测试3: 对数和指数运算
test "对数和指数运算测试" {
  // 测试自然指数
  assert_true((0.0.exp()).abs() - 1.0 < 0.0001)
  assert_true((1.0).exp().abs() - 2.71828 < 0.001)
  
  // 测试自然对数
  assert_true((1.0).ln().abs() < 0.0001)
  assert_true((2.71828).ln().abs() - 1.0 < 0.001)
  
  // 测试以10为底的对数
  assert_true((10.0).log10().abs() - 1.0 < 0.0001)
  assert_true((100.0).log10().abs() - 2.0 < 0.0001)
  assert_true((1.0).log10().abs() < 0.0001)
  
  // 测试以2为底的对数
  assert_true((2.0).log2().abs() - 1.0 < 0.0001)
  assert_true((8.0).log2().abs() - 3.0 < 0.0001)
  assert_true((16.0).log2().abs() - 4.0 < 0.0001)
}

// 测试4: 统计数学运算
test "统计数学运算测试" {
  // 测试平均值计算
  let numbers1 = [1, 2, 3, 4, 5]
  let avg1 = numbers1.reduce(fn(acc, x) { acc + x }, 0) / numbers1.length()
  assert_eq(avg1, 3)
  
  let numbers2 = [2.5, 3.5, 4.5, 5.5]
  let avg2 = numbers2.reduce(fn(acc, x) { acc + x }, 0.0) / numbers2.length().to_float()
  assert_true(avg2.abs() - 4.0 < 0.0001)
  
  // 测试中位数计算
  let sorted_numbers = [1, 3, 5, 7, 9]
  let median_odd = sorted_numbers[sorted_numbers.length() / 2]
  assert_eq(median_odd, 5)
  
  let even_numbers = [2, 4, 6, 8]
  let median_even = (even_numbers[even_numbers.length()/2 - 1] + even_numbers[even_numbers.length()/2]) / 2
  assert_eq(median_even, 5)
  
  // 测试最大值和最小值
  assert_eq(sorted_numbers.reduce(fn(acc, x) { if x > acc { x } else { acc } }, sorted_numbers[0]), 9)
  assert_eq(sorted_numbers.reduce(fn(acc, x) { if x < acc { x } else { acc } }, sorted_numbers[0]), 1)
}

// 测试5: 数值范围和边界测试
test "数值范围和边界测试" {
  // 测试最大值和最小值常量
  let int_max = 2147483647
  let int_min = -2147483648
  
  // 测试边界运算
  assert_eq(int_max + 1, int_min) // 溢出测试
  assert_eq(int_min - 1, int_max) // 下溢测试
  
  // 测试浮点数边界
  let float_max = 3.4028235e38
  let float_min = -3.4028235e38
  
  assert_true(float_max > 0.0)
  assert_true(float_min < 0.0)
  
  // 测试无穷大
  let inf = 1.0 / 0.0
  assert_true(inf.is_infinite())
  assert_true(inf > 0.0)
  
  let neg_inf = -1.0 / 0.0
  assert_true(neg_inf.is_infinite())
  assert_true(neg_inf < 0.0)
  
  // 测试NaN
  let nan = 0.0 / 0.0
  assert_true(nan.is_nan())
  assert_false(nan == nan) // NaN不等于自身
}

// 测试6: 随机数和概率运算
test "随机数和概率运算测试" {
  // 测试随机数生成（模拟）
  let seed = 12345
  let random1 = pseudo_random(seed)
  let random2 = pseudo_random(seed + 1)
  
  // 验证随机数范围
  assert_true(random1 >= 0.0 && random1 <= 1.0)
  assert_true(random2 >= 0.0 && random2 <= 1.0)
  
  // 验证不同种子产生不同随机数
  assert_true(random1.abs() - random2.abs() > 0.0001)
  
  // 测试随机整数生成
  let rand_int1 = random_int_range(seed, 1, 10)
  let rand_int2 = random_int_range(seed + 1, 1, 10)
  
  assert_true(rand_int1 >= 1 && rand_int1 <= 10)
  assert_true(rand_int2 >= 1 && rand_int2 <= 10)
  
  // 测试概率计算
  let probability = 0.75
  let success_count = 0
  let total_trials = 1000
  
  for i in 1..=total_trials {
    let rand_val = pseudo_random(seed + i)
    if rand_val < probability {
      success_count = success_count + 1
    }
  }
  
  let actual_probability = success_count.to_float() / total_trials.to_float()
  assert_true(actual_probability > 0.7 && actual_probability < 0.8)
}

// 测试7: 数值格式化和精度处理
test "数值格式化和精度处理测试" {
  // 测试小数精度控制
  let pi = 3.14159265359
  
  let pi_2_decimal = round_to_precision(pi, 2)
  assert_true(pi_2_decimal.abs() - 3.14 < 0.001)
  
  let pi_4_decimal = round_to_precision(pi, 4)
  assert_true(pi_4_decimal.abs() - 3.1416 < 0.0001)
  
  // 测试科学计数法
  let large_number = 1234567.89
  let scientific = to_scientific_notation(large_number)
  assert_eq(scientific, "1.23456789e6")
  
  let small_number = 0.00012345
  let small_scientific = to_scientific_notation(small_number)
  assert_eq(small_scientific, "1.2345e-4")
  
  // 测试百分比格式化
  let ratio = 0.7543
  let percentage = to_percentage(ratio, 1)
  assert_eq(percentage, "75.4%")
  
  let ratio2 = 0.075
  let percentage2 = to_percentage(ratio2, 2)
  assert_eq(percentage2, "7.50%")
}

// 测试8: 数值验证和安全检查
test "数值验证和安全检查测试" {
  // 测试数值有效性检查
  assert_true(is_valid_number(42))
  assert_true(is_valid_number(-3.14))
  assert_true(is_valid_number(0))
  assert_false(is_valid_number(0.0 / 0.0)) // NaN
  assert_false(is_valid_number(1.0 / 0.0)) // Infinity
  
  // 测试数值范围检查
  assert_true(is_in_range(5, 1, 10))
  assert_true(is_in_range(1, 1, 10))
  assert_true(is_in_range(10, 1, 10))
  assert_false(is_in_range(0, 1, 10))
  assert_false(is_in_range(11, 1, 10))
  
  // 测试数值类型转换安全性
  let safe_int = safe_float_to_int(3.7)
  assert_eq(safe_int, 3)
  
  let safe_int_neg = safe_float_to_int(-2.3)
  assert_eq(safe_int_neg, -2)
  
  // 测试除零安全检查
  let safe_div1 = safe_divide(10, 2)
  assert_eq(safe_div1, Some(5))
  
  let safe_div2 = safe_divide(10, 0)
  assert_eq(safe_div2, None)
  
  let safe_div3 = safe_divide(0, 5)
  assert_eq(safe_div3, Some(0))
}

// 辅助函数定义（实际实现中这些函数应该来自标准库或实现模块）
fn pseudo_random(seed : Int) -> Float {
  // 简单的伪随机数生成器（线性同余法）
  let a = 1664525
  let c = 1013904223
  let m = 4294967296
  let next = (a * seed + c) % m
  (next.to_float() / m.to_float())
}

fn random_int_range(seed : Int, min : Int, max : Int) -> Int {
  let rand_val = pseudo_random(seed)
  min + (rand_val * (max - min + 1).to_float()).to_int()
}

fn round_to_precision(value : Float, precision : Int) -> Float {
  let factor = (10.0).pow(precision.to_float())
  (value * factor).round() / factor
}

fn to_scientific_notation(value : Float) -> String {
  if value == 0.0 { return "0e0" }
  
  let abs_val = value.abs()
  let exponent = abs_val.log10().floor().to_int()
  let mantissa = value / (10.0).pow(exponent.to_float())
  
  mantissa.to_string() + "e" + exponent.to_string()
}

fn to_percentage(value : Float, precision : Int) -> String {
  let percentage = value * 100.0
  let rounded = round_to_precision(percentage, precision)
  rounded.to_string() + "%"
}

fn is_valid_number(value : Float) -> Bool {
  !value.is_nan() && !value.is_infinite()
}

fn is_in_range(value : Int, min : Int, max : Int) -> Bool {
  value >= min && value <= max
}

fn safe_float_to_int(value : Float) -> Int {
  if value >= 0.0 { value.floor().to_int() } else { value.ceil().to_int() }
}

fn safe_divide(numerator : Int, denominator : Int) -> Option[Int] {
  if denominator == 0 { None } else { Some(numerator / denominator) }
}
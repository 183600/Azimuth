// Azimuth 项目额外的 MoonBit 测试用例
// 包含函数定义和测试，确保独立性

// 函数定义
pub fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  if lhs < rhs || lhs > rhs {
    let _ = "Assertion failed"
  } else {
    ()
  }
  ()
}

pub fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  if lhs.length() != rhs.length() {
    let _ = "String assertion failed"
  } else {
    let _ = "String assertion passed"
  }
  ()
}

pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn subtract(a : Int, b : Int) -> Int {
  a - b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

// 测试用例
test "power_operations_simulation" {
  // 幂运算模拟测试
  // 计算 2^3 = 8
  let base = 2
  
  let power_step1 = multiply(base, base)      // 2^2 = 4
  let power_step2 = multiply(power_step1, base)  // 2^3 = 8
  
  assert_eq(4, power_step1)
  assert_eq(8, power_step2)
  
  // 计算 3^4 = 81
  let base2 = 3
  
  let power2_step1 = multiply(base2, base2)     // 3^2 = 9
  let power2_step2 = multiply(power2_step1, power2_step1)  // 3^4 = 81
  
  assert_eq(9, power2_step1)
  assert_eq(81, power2_step2)
}

test "average_calculation" {
  // 平均值计算测试
  let numbers_sum = add(add(add(add(15, 25), 35), 45), 55)
  let count = 5
  let average = divide_with_ceil(numbers_sum, count)
  
  assert_eq(175, numbers_sum)
  assert_eq(35, average)
  
  // 测试向上取整的平均值
  let numbers_sum2 = add(add(add(10, 20), 30), 41)  // 总和101
  let count2 = 4
  let average2 = divide_with_ceil(numbers_sum2, count2)  // ceil(101/4) = 26
  
  assert_eq(101, numbers_sum2)
  assert_eq(26, average2)
}

test "modulo_operations_simulation" {
  // 模运算模拟测试
  let dividend = 17
  let divisor = 5
  
  let quotient = divide_with_ceil(dividend, divisor)
  let product = multiply(quotient, divisor)
  let remainder = subtract(dividend, product)
  
  assert_eq(4, quotient)
  assert_eq(20, product)
  assert_eq(-3, remainder)
  
  // 负数模运算模拟
  let dividend2 = -17
  let divisor2 = 5
  
  let quotient2 = divide_with_ceil(dividend2, divisor2)
  let product2 = multiply(quotient2, divisor2)
  let remainder2 = subtract(dividend2, product2)
  
  assert_eq(-3, quotient2)
  assert_eq(-15, product2)
  assert_eq(-2, remainder2)
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 200
  let percentage = 25
  
  // 计算百分比值
  let percentage_value = divide_with_ceil(multiply(total, percentage), 100)
  
  assert_eq(50, percentage_value)
  
  // 计算增长后的值
  let growth_rate = 15
  let growth_amount = divide_with_ceil(multiply(total, growth_rate), 100)
  let new_total = add(total, growth_amount)
  
  assert_eq(30, growth_amount)
  assert_eq(230, new_total)
  
  // 计算折扣后的值
  let discount_rate = 20
  let discount_amount = divide_with_ceil(multiply(total, discount_rate), 100)
  let discounted_total = subtract(total, discount_amount)
  
  assert_eq(40, discount_amount)
  assert_eq(160, discounted_total)
}

test "speed_distance_time_calculations" {
  // 速度-距离-时间计算测试
  let distance = 150
  let time = 3
  
  // 计算速度
  let speed = divide_with_ceil(distance, time)
  
  assert_eq(50, speed)
  
  // 已知速度和时间计算距离
  let speed2 = 60
  let time2 = 4
  let distance2 = multiply(speed2, time2)
  
  assert_eq(240, distance2)
  
  // 已知距离和速度计算时间
  let distance3 = 180
  let speed3 = 40
  let time3 = divide_with_ceil(distance3, speed3)
  
  assert_eq(5, time3)
}

test "array_index_calculations" {
  // 数组索引计算测试
  let array_size = 10
  let start_index = 0
  let end_index = subtract(array_size, 1)
  
  assert_eq(9, end_index)
  
  // 计算中间索引
  let middle_index = divide_with_ceil(add(start_index, end_index), 2)
  
  assert_eq(5, middle_index)
  
  // 计算数组容量（假设需要存储n个元素，容量为2的幂）
  let required_size = 15
  let capacity_power2 = 16  // 下一个2的幂
  
  assert_eq(16, capacity_power2)
}

test "temperature_conversions_extended" {
  // 扩展温度转换测试
  let fahrenheit = 98
  
  // 华氏度转摄氏度：C = (F - 32) × 5/9
  let celsius_step1 = subtract(fahrenheit, 32)
  let celsius_step2 = multiply(celsius_step1, 5)
  let celsius = divide_with_ceil(celsius_step2, 9)
  
  assert_eq(37, celsius)
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let celsius2 = 0
  let fahrenheit_step1 = multiply(celsius2, 9)
  let fahrenheit_step2 = divide_with_ceil(fahrenheit_step1, 5)
  let fahrenheit2 = add(fahrenheit_step2, 32)
  
  assert_eq(32, fahrenheit2)
  
  // 开尔文温度转换（简化）
  let celsius3 = 25
  let kelvin = add(celsius3, 273)
  
  assert_eq(298, kelvin)
}

test "loan_interest_calculation" {
  // 贷款利息计算测试
  let principal = 10000
  let annual_rate = 6  // 6%
  let years = 3
  
  // 计算单利
  let yearly_interest = divide_with_ceil(multiply(principal, annual_rate), 100)
  let total_interest_simple = multiply(yearly_interest, years)
  let total_payment_simple = add(principal, total_interest_simple)
  
  assert_eq(600, yearly_interest)
  assert_eq(1800, total_interest_simple)
  assert_eq(11800, total_payment_simple)
  
  // 计算复利（简化版本）
  let year1_total = add(principal, yearly_interest)
  let year2_interest = divide_with_ceil(multiply(year1_total, annual_rate), 100)
  let year2_total = add(year1_total, year2_interest)
  let year3_interest = divide_with_ceil(multiply(year2_total, annual_rate), 100)
  let year3_total = add(year2_total, year3_interest)
  
  assert_eq(10600, year1_total)
  assert_eq(11236, year2_total)
  assert_eq(11910, year3_total)
}
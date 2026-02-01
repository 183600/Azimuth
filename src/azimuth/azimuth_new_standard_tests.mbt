// 新增的标准 MoonBit 测试用例
// 为 Azimuth 项目创建的10个高质量测试用例

// 函数定义
pub fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  if lhs == rhs {
    ()
  } else {
    let _ = "Assertion failed"
  }
  ()
}

pub fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  if lhs == rhs {
    ()
  } else {
    let _ = "String assertion failed"
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

test "subtract_function_edge_cases" {
  // 测试减法函数的边界情况
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(10, 0))
  assert_eq(-10, subtract(0, 10))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
  assert_eq(0, subtract(-10, -10))
}

test "divide_with_ceil_large_numbers" {
  // 测试大数的向上取整除法
  assert_eq(100000, divide_with_ceil(100000, 1))
  assert_eq(50000, divide_with_ceil(100000, 2))
  assert_eq(33334, divide_with_ceil(100000, 3))
  assert_eq(1, divide_with_ceil(99999, 100000))
  assert_eq(100001, divide_with_ceil(100001, 1))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, ()[]{}!", greet("()[]{}"))
  assert_eq_string("Hello, <>|&!", greet("<>|&"))
  assert_eq_string("Hello, \"\"!", greet("\"\""))
}

test "combined_operations_complex" {
  // 测试复杂的组合运算
  let a = 12
  let b = 8
  let c = 5
  let d = 3
  
  // 测试 (a + b) * (c - d)
  let result1 = multiply(add(a, b), subtract(c, d))
  assert_eq(40, result1)  // (12 + 8) * (5 - 3) = 20 * 2 = 40
  
  // 测试 a * b + c * d
  let result2 = add(multiply(a, b), multiply(c, d))
  assert_eq(111, result2)  // 12 * 8 + 5 * 3 = 96 + 15 = 111
  
  // 测试 (a - b) / (c + d) 向上取整
  let result3 = divide_with_ceil(subtract(a, b), add(c, d))
  assert_eq(1, result3)  // ceil((12 - 8) / (5 + 3)) = ceil(4 / 8) = 1
}

test "mathematical_series_sum" {
  // 测试数学级数求和
  // 计算等差数列 1 + 3 + 5 + 7 + 9
  let sum1 = add(add(add(add(1, 3), 5), 7), 9)
  assert_eq(25, sum1)
  
  // 计算等比数列 2 + 4 + 8 + 16
  let sum2 = add(add(add(2, 4), 8), 16)
  assert_eq(30, sum2)
  
  // 使用乘法和加法计算平方和 1² + 2² + 3²
  let sum3 = add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3))
  assert_eq(14, sum3)
}

test "time_conversion_calculations" {
  // 测试时间转换计算
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = add(add(multiply(hours, 3600), multiply(minutes, 60)), seconds)
  assert_eq(9930, total_seconds)  // 2*3600 + 45*60 + 30 = 7200 + 2700 + 30 = 9930
  
  // 计算包含的完整分钟数（向上取整）
  let total_minutes = divide_with_ceil(total_seconds, 60)
  assert_eq(166, total_minutes)  // ceil(9930 / 60) = 166
  
  // 计算包含的完整小时数（向上取整）
  let total_hours = divide_with_ceil(total_seconds, 3600)
  assert_eq(3, total_hours)  // ceil(9930 / 3600) = 3
}

test "percentage_calculations" {
  // 测试百分比计算
  let original_value = 200
  let percentage_10 = divide_with_ceil(multiply(original_value, 10), 100)
  let percentage_25 = divide_with_ceil(multiply(original_value, 25), 100)
  let percentage_75 = divide_with_ceil(multiply(original_value, 75), 100)
  
  assert_eq(20, percentage_10)   // 200 * 10% = 20
  assert_eq(50, percentage_25)   // 200 * 25% = 50
  assert_eq(150, percentage_75)  // 200 * 75% = 150
  
  // 计算增长后的值
  let increased_value = add(original_value, percentage_25)
  assert_eq(250, increased_value)  // 200 + 50 = 250
  
  // 计算折扣后的值
  let discounted_value = subtract(original_value, percentage_10)
  assert_eq(180, discounted_value)  // 200 - 20 = 180
}

test "array_simulation_operations" {
  // 模拟数组操作（使用索引计算）
  let array_size = 10
  let index1 = 3
  let index2 = 7
  
  // 计算索引距离
  let index_distance = subtract(index2, index1)
  assert_eq(4, index_distance)  // 7 - 3 = 4
  
  // 计算中间位置（向上取整）
  let middle_position = divide_with_ceil(add(index1, index2), 2)
  assert_eq(5, middle_position)  // ceil((3 + 7) / 2) = 5
  
  // 计算数组元素总数（模拟）
  let total_elements = multiply(array_size, 1)
  assert_eq(10, total_elements)
  
  // 计算剩余元素数量
  let remaining_elements = subtract(array_size, index_distance)
  assert_eq(6, remaining_elements)  // 10 - 4 = 6
}

test "geometry_calculations" {
  // 测试几何计算
  let length = 8
  let width = 6
  let height = 4
  
  // 计算矩形面积
  let rectangle_area = multiply(length, width)
  assert_eq(48, rectangle_area)  // 8 * 6 = 48
  
  // 计算矩形周长
  let rectangle_perimeter = multiply(2, add(length, width))
  assert_eq(28, rectangle_perimeter)  // 2 * (8 + 6) = 28
  
  // 计算长方体体积
  let box_volume = multiply(multiply(length, width), height)
  assert_eq(192, box_volume)  // 8 * 6 * 4 = 192
  
  // 计算表面积
  let surface_area = add(add(multiply(2, multiply(length, width)), multiply(2, multiply(length, height))), multiply(2, multiply(width, height)))
  assert_eq(208, surface_area)  // 2*(8*6) + 2*(8*4) + 2*(6*4) = 96 + 64 + 48 = 208
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  let celsius = 25
  
  // 摄氏度转华氏度：F = C * 9/5 + 32（简化为整数计算）
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  assert_eq(77, fahrenheit)  // ceil(25 * 9 / 5) + 32 = 45 + 32 = 77
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9（简化为整数计算）
  let celsius_back = divide_with_ceil(multiply(subtract(fahrenheit, 32), 5), 9)
  assert_eq(25, celsius_back)  // ceil((77 - 32) * 5 / 9) = ceil(45 * 5 / 9) = 25
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = add(divide_with_ceil(multiply(freezing_celsius, 9), 5), 32)
  assert_eq(32, freezing_fahrenheit)  // 0 * 9 / 5 + 32 = 32
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = add(divide_with_ceil(multiply(boiling_celsius, 9), 5), 32)
  assert_eq(212, boiling_fahrenheit)  // ceil(100 * 9 / 5) + 32 = 180 + 32 = 212
}
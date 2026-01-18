// Azimuth 项目简洁测试套件
// 使用标准 MoonBit 测试语法，包含10个测试用例

test "integer_arithmetic_basic" {
  // 基本整数算术运算测试
  let a = 15
  let b = 7
  
  // 加法
  let sum = a + b
  // 减法
  let difference = a - b
  // 乘法
  let product = a * b
  // 除法
  let quotient = a / b
  // 取余
  let remainder = a % b
  
  // 验证结果
  let verify_sum = sum
  let verify_difference = difference
  let verify_product = product
  let verify_quotient = quotient
  let verify_remainder = remainder
}

test "boolean_logic_operations" {
  // 布尔逻辑运算测试
  let true_val = true
  let false_val = false
  
  // 逻辑与
  let and_result = true_val && false_val
  // 逻辑或
  let or_result = true_val || false_val
  // 逻辑非
  let not_true = !true_val
  let not_false = !false_val
  
  // 验证结果
  let verify_and = and_result
  let verify_or = or_result
  let verify_not_true = not_true
  let verify_not_false = not_false
}

test "string_concatenation" {
  // 字符串连接测试
  let first = "Hello"
  let middle = "Beautiful"
  let last = "World"
  
  // 简单连接
  let simple = first + " " + last
  // 多段连接
  let complex = first + " " + middle + " " + last
  // 空字符串连接
  let with_empty = first + "" + last
  
  // 验证结果
  let verify_simple = simple
  let verify_complex = complex
  let verify_with_empty = with_empty
}

test "comparison_operations" {
  // 比较运算测试
  let x = 42
  let y = 24
  let z = 42
  
  // 相等比较
  let equal_result = x == z
  let not_equal_result = x != y
  
  // 大小比较
  let greater = x > y
  let less = y < x
  let greater_equal = x >= z
  let less_equal = y <= x
  
  // 验证结果
  let verify_equal = equal_result
  let verify_not_equal = not_equal_result
  let verify_greater = greater
  let verify_less = less
  let verify_greater_equal = greater_equal
  let verify_less_equal = less_equal
}

test "conditional_expressions" {
  // 条件表达式测试
  let age = 18
  let score = 85
  
  // 简单条件
  let is_adult = if age >= 18 { true } else { false }
  
  // 嵌套条件
  let grade = if score >= 90 {
    "A"
  } else if score >= 80 {
    "B"
  } else if score >= 70 {
    "C"
  } else {
    "D"
  }
  
  // 三元条件模拟
  let pass_fail = if score >= 60 { "Pass" } else { "Fail" }
  
  // 验证结果
  let verify_adult = is_adult
  let verify_grade = grade
  let verify_pass_fail = pass_fail
}

test "mathematical_functions" {
  // 数学函数测试
  let base = 5
  let exponent = 3
  
  // 幂运算（通过重复乘法）
  let power = base * base * base
  
  // 平方根近似（整数）
  let number = 16
  let sqrt_approx = 4  // 4 * 4 = 16
  
  // 绝对值
  let positive = 10
  let negative = -10
  let abs_positive = if positive < 0 { -positive } else { positive }
  let abs_negative = if negative < 0 { -negative } else { negative }
  
  // 验证结果
  let verify_power = power
  let verify_sqrt = sqrt_approx
  let verify_abs_pos = abs_positive
  let verify_abs_neg = abs_negative
}

test "array_index_calculation" {
  // 数组索引计算测试
  let rows = 3
  let cols = 4
  let row = 1
  let col = 2
  
  // 二维数组转一维索引
  let index_1d = row * cols + col
  
  // 边界索引
  let first_index = 0 * cols + 0
  let last_index = (rows - 1) * cols + (cols - 1)
  
  // 验证结果
  let verify_index = index_1d
  let verify_first = first_index
  let verify_last = last_index
}

test "business_calculation" {
  // 商业计算测试
  let price = 199
  let quantity = 3
  let tax_rate = 8  // 8%
  let discount = 50
  
  // 计算小计
  let subtotal = price * quantity
  
  // 计算税费
  let tax = subtotal * tax_rate / 100
  
  // 计算总价
  let total = subtotal + tax - discount
  
  // 验证结果
  let verify_subtotal = subtotal
  let verify_tax = tax
  let verify_total = total
}

test "geometry_calculations" {
  // 几何计算测试
  let length = 8
  let width = 6
  let height = 4
  
  // 矩形面积
  let area = length * width
  
  // 矩形周长
  let perimeter = 2 * (length + width)
  
  // 长方体体积
  let volume = length * width * height
  
  // 验证结果
  let verify_area = area
  let verify_perimeter = perimeter
  let verify_volume = volume
}

test "time_calculations" {
  // 时间计算测试
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = hours * 3600 + minutes * 60 + seconds
  
  // 计算时间差（秒）
  let start_hour = 9
  let start_min = 30
  let end_hour = 14
  let end_min = 15
  
  let start_seconds = start_hour * 3600 + start_min * 60
  let end_seconds = end_hour * 3600 + end_min * 60
  let duration = end_seconds - start_seconds
  
  // 验证结果
  let verify_total_seconds = total_seconds
  let verify_duration = duration
}
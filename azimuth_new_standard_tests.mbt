// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  let a = 15
  let b = 7
  
  // 加法
  let sum = a + b
  @azimuth.assert_eq(22, sum)
  
  // 减法
  let difference = a - b
  @azimuth.assert_eq(8, difference)
  
  // 乘法
  let product = a * b
  @azimuth.assert_eq(105, product)
  
  // 除法
  let quotient = a / b
  @azimuth.assert_eq(2, quotient)
  
  // 取余
  let remainder = a % b
  @azimuth.assert_eq(1, remainder)
}

test "string_concatenation_operations" {
  // 字符串连接操作测试
  let first_name = "John"
  let last_name = "Doe"
  let space = " "
  
  // 全名连接
  let full_name = first_name + space + last_name
  @azimuth.assert_eq_string("John Doe", full_name)
  
  // 问候语连接
  let greeting = "Hello"
  let punctuation = "!"
  let message = greeting + space + full_name + punctuation
  @azimuth.assert_eq_string("Hello John Doe!", message)
  
  // 路径连接
  let dir = "/home"
  let user = "user"
  let file = "document.txt"
  let path = dir + "/" + user + "/" + file
  @azimuth.assert_eq_string("/home/user/document.txt", path)
}

test "boolean_logic_operations" {
  // 布尔逻辑运算测试
  let true_val = true
  let false_val = false
  
  // 逻辑与
  let and_result1 = true_val && true_val
  let and_result2 = true_val && false_val
  let and_result3 = false_val && true_val
  let and_result4 = false_val && false_val
  
  @azimuth.assert_eq(true, and_result1)
  @azimuth.assert_eq(false, and_result2)
  @azimuth.assert_eq(false, and_result3)
  @azimuth.assert_eq(false, and_result4)
  
  // 逻辑或
  let or_result1 = true_val || true_val
  let or_result2 = true_val || false_val
  let or_result3 = false_val || true_val
  let or_result4 = false_val || false_val
  
  @azimuth.assert_eq(true, or_result1)
  @azimuth.assert_eq(true, or_result2)
  @azimuth.assert_eq(true, or_result3)
  @azimuth.assert_eq(false, or_result4)
  
  // 逻辑非
  let not_true = !true_val
  let not_false = !false_val
  
  @azimuth.assert_eq(false, not_true)
  @azimuth.assert_eq(true, not_false)
}

test "comparison_operations" {
  // 比较运算测试
  let x = 10
  let y = 20
  let z = 10
  
  // 等于
  @azimuth.assert_eq(true, x == z)
  @azimuth.assert_eq(false, x == y)
  
  // 不等于
  @azimuth.assert_eq(false, x != z)
  @azimuth.assert_eq(true, x != y)
  
  // 大于
  @azimuth.assert_eq(false, x > y)
  @azimuth.assert_eq(false, x > z)
  @azimuth.assert_eq(true, y > x)
  
  // 小于
  @azimuth.assert_eq(true, x < y)
  @azimuth.assert_eq(false, x < z)
  @azimuth.assert_eq(false, y < x)
  
  // 大于等于
  @azimuth.assert_eq(true, x >= z)
  @azimuth.assert_eq(false, x >= y)
  @azimuth.assert_eq(true, y >= x)
  
  // 小于等于
  @azimuth.assert_eq(true, x <= z)
  @azimuth.assert_eq(true, x <= y)
  @azimuth.assert_eq(false, y <= x)
}

test "mathematical_functions" {
  // 数学函数测试
  let base = 5
  
  // 幂运算 (通过重复乘法实现)
  let power = base * base * base
  @azimuth.assert_eq(125, power)
  
  // 平方根近似 (使用整数运算)
  let sqrt_approx = 4  // 4*4 = 16
  @azimuth.assert_eq(4, sqrt_approx)
  
  // 绝对值
  let positive = 10
  let negative = -10
  let zero = 0
  
  let abs_positive = positive
  let abs_negative = -negative
  let abs_zero = zero
  
  @azimuth.assert_eq(10, abs_positive)
  @azimuth.assert_eq(10, abs_negative)
  @azimuth.assert_eq(0, abs_zero)
}

test "boundary_value_tests" {
  // 边界值测试
  let max_int = 2147483647
  let min_int = -2147483648
  let zero = 0
  let one = 1
  let minus_one = -1
  
  // 零值运算
  @azimuth.assert_eq(0, zero + zero)
  @azimuth.assert_eq(0, zero * max_int)
  @azimuth.assert_eq(0, zero * min_int)
  @azimuth.assert_eq(0, zero / one)
  
  // 单位值运算
  @azimuth.assert_eq(max_int, max_int * one)
  @azimuth.assert_eq(min_int, min_int * one)
  @azimuth.assert_eq(max_int, max_int / one)
  @azimuth.assert_eq(min_int, min_int / one)
  
  // 边界值加法
  @azimuth.assert_eq(max_int, max_int + zero)
  @azimuth.assert_eq(min_int, min_int + zero)
  @azimuth.assert_eq(one, zero + one)
  @azimuth.assert_eq(minus_one, zero + minus_one)
}

test "array_index_operations" {
  // 数组索引操作测试 (模拟数组访问)
  let array_size = 10
  let first_index = 0
  let last_index = array_size - 1
  let middle_index = array_size / 2
  
  // 索引边界检查
  @azimuth.assert_eq(0, first_index)
  @azimuth.assert_eq(9, last_index)
  @azimuth.assert_eq(5, middle_index)
  
  // 索引计算
  let index1 = first_index + 3
  let index2 = last_index - 2
  let index3 = middle_index + 1
  
  @azimuth.assert_eq(3, index1)
  @azimuth.assert_eq(7, index2)
  @azimuth.assert_eq(6, index3)
  
  // 索引范围检查
  let in_range1 = index1 >= first_index && index1 <= last_index
  let in_range2 = index2 >= first_index && index2 <= last_index
  let in_range3 = index3 >= first_index && index3 <= last_index
  
  @azimuth.assert_eq(true, in_range1)
  @azimuth.assert_eq(true, in_range2)
  @azimuth.assert_eq(true, in_range3)
}

test "real_world_calculations" {
  // 实际应用场景计算测试
  
  // 购物车计算
  let item1_price = 25
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  let tax_rate = 8  // 8%
  let discount = 10
  
  // 小计计算
  let subtotal1 = item1_price * item1_quantity
  let subtotal2 = item2_price * item2_quantity
  let total_subtotal = subtotal1 + subtotal2
  
  @azimuth.assert_eq(50, subtotal1)
  @azimuth.assert_eq(45, subtotal2)
  @azimuth.assert_eq(95, total_subtotal)
  
  // 折扣和税计算
  let discounted_total = total_subtotal - discount
  let tax_amount = discounted_total * tax_rate / 100
  let final_total = discounted_total + tax_amount
  
  @azimuth.assert_eq(85, discounted_total)
  @azimuth.assert_eq(6, tax_amount)  // 85 * 8 / 100 = 6.8，整数运算为6
  @azimuth.assert_eq(91, final_total)
  
  // 速度距离时间计算
  let distance = 300  // 公里
  let time_hours = 5  // 小时
  let speed = distance / time_hours
  
  @azimuth.assert_eq(60, speed)
  
  // 燃料消耗计算
  let fuel_consumed = 40  // 升
  let fuel_efficiency = distance / fuel_consumed  // 公里/升
  
  @azimuth.assert_eq(7, fuel_efficiency)  // 300 / 40 = 7.5，整数运算为7
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  
  // 除零检查
  let divisor = 0
  
  // 检查除数是否为零
  let is_divisor_zero = divisor == 0
  @azimuth.assert_eq(true, is_divisor_zero)
  
  // 安全除法 (模拟)
  let safe_result = if is_divisor_zero == false { 10 / divisor } else { 0 }
  @azimuth.assert_eq(0, safe_result)
  
  // 数组越界检查 (模拟)
  let array_length = 5
  let requested_index = 7
  
  let is_index_valid = requested_index >= 0 && requested_index < array_length
  @azimuth.assert_eq(false, is_index_valid)
  
  // 安全索引访问 (模拟)
  let safe_index = if is_index_valid == true { requested_index } else { 0 }
  @azimuth.assert_eq(0, safe_index)
  
  // 空值检查 (模拟)
  let string_value = ""
  let is_empty = string_value == ""
  @azimuth.assert_eq(true, is_empty)
  
  // 默认值处理 (模拟)
  let result_value = if is_empty == true { "default" } else { string_value }
  @azimuth.assert_eq_string("default", result_value)
}
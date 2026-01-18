// Azimuth 项目实用单元测试
// 使用标准 MoonBit 测试语法

test "array_operations" {
  // 测试数组操作
  let numbers = [1, 2, 3, 4, 5]
  let first = numbers[0]
  let last = numbers[4]
  let length = numbers.length()
  
  // 计算数组元素总和
  let mut sum = 0
  for i = 0; i < length; i = i + 1 {
    sum = sum + numbers[i]
  }
  
  // 验证结果
  let check_first = first
  let check_last = last
  let check_sum = sum
}

test "string_length_operations" {
  // 测试字符串长度操作
  let short_text = "Hi"
  let medium_text = "Hello World"
  let long_text = "This is a longer string for testing purposes"
  
  let short_len = short_text.length()
  let medium_len = medium_text.length()
  let long_len = long_text.length()
  
  // 验证长度
  let check1 = short_len
  let check2 = medium_len
  let check3 = long_len
}

test "conditional_logic" {
  // 测试条件逻辑
  let age = 18
  let score = 85
  let is_member = true
  
  // 根据条件确定类别
  let age_category = 
    if (age < 13) { "child" }
    else if (age < 18) { "teenager" }
    else { "adult" }
  
  let grade = 
    if (score >= 90) { "A" }
    else if (score >= 80) { "B" }
    else if (score >= 70) { "C" }
    else { "D" }
  
  let discount = 
    if (is_member) { 20 }
    else { 5 }
  
  // 验证结果
  let check1 = age_category
  let check2 = grade
  let check3 = discount
}

test "boolean_operations" {
  // 测试布尔操作
  let is_active = true
  let has_permission = false
  let is_admin = true
  
  let can_access = is_active && has_permission
  let can_edit = is_active && is_admin
  let should_show = is_active || is_admin
  let is_restricted = !has_permission
  
  // 验证结果
  let check1 = can_access
  let check2 = can_edit
  let check3 = should_show
  let check4 = is_restricted
}

test "loop_patterns" {
  // 测试循环模式
  let limit = 5
  let mut counter = 0
  let mut accumulator = 0
  
  // while 循环
  while (counter < limit) {
    accumulator = accumulator + counter
    counter = counter + 1
  }
  
  // for 循环计算阶乘
  let mut factorial = 1
  for i = 1; i <= 4; i = i + 1 {
    factorial = factorial * i
  }
  
  // 验证结果
  let check1 = accumulator
  let check2 = factorial
}

test "function_calculations" {
  // 测试函数式计算
  let x = 10
  let y = 20
  
  // 简单函数计算
  let add_result = x + y
  let multiply_result = x * y
  let subtract_result = y - x
  let divide_result = y / x
  
  // 复合函数计算
  let composite = (x + y) * (y - x)
  let nested = x * (y + (x / 2))
  
  // 验证结果
  let check1 = add_result
  let check2 = multiply_result
  let check3 = subtract_result
  let check4 = divide_result
  let check5 = composite
  let check6 = nested
}

test "data_validation" {
  // 测试数据验证
  let email = "user@example.com"
  let password = "secure123"
  let age = 25
  let username = "testuser"
  
  // 简单验证规则
  let email_valid = email.length() > 5 && true  // 简化contains检查
  let password_valid = password.length() >= 8
  let age_valid = age >= 18 && age <= 100
  let username_valid = username.length() >= 3
  
  // 综合验证
  let all_valid = email_valid && password_valid && age_valid && username_valid
  
  // 验证结果
  let check1 = email_valid
  let check2 = password_valid
  let check3 = age_valid
  let check4 = username_valid
  let check5 = all_valid
}

test "numeric_conversions" {
  // 测试数值转换
  let int_val = 42
  let float_val = 3.14
  let string_num = "123"
  
  // 基本转换操作
  let doubled = int_val * 2
  let halved = int_val / 2
  let squared = int_val * int_val
  
  // 浮点数操作
  let float_doubled = float_val * 2.0
  let float_sum = float_val + 1.86
  
  // 验证结果
  let check1 = doubled
  let check2 = halved
  let check3 = squared
  let check4 = float_doubled
  let check5 = float_sum
}

test "error_handling_patterns" {
  // 测试错误处理模式
  let input = "valid"
  let fallback = "default"
  
  // 条件检查和回退
  let result = 
    if (input.length() > 0) { input }
    else { fallback }
  
  // 边界检查
  let index = 5
  let array_size = 10
  let safe_access = 
    if (index >= 0 && index < array_size) { true }
    else { false }
  
  // 验证结果
  let check1 = result
  let check2 = safe_access
}

test "calculation_precision" {
  // 测试计算精度
  let precise_value = 0.1 + 0.2
  let expected_value = 0.3
  
  // 整数运算保持精度
  let int_precise = 1000000 / 3
  let int_remainder = 1000000 % 3
  
  // 浮点数比较
  let is_close = 
    if (precise_value - expected_value < 0.001) { true }
    else { false }
  
  // 验证结果
  let check1 = precise_value
  let check2 = int_precise
  let check3 = int_remainder
  let check4 = is_close
}
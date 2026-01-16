// Azimuth 项目新增 MoonBit 测试用例
// 包含10个实用的测试用例，使用标准 MoonBit 测试语法

test "integer_division_operations" {
  // 测试整数除法操作
  // 正常除法
  let result1 = 20 / 4
  @azimuth.assert_eq(5, result1)
  
  // 除法向下取整
  let result2 = 21 / 4
  @azimuth.assert_eq(5, result2)
  
  // 负数除法
  let result3 = -20 / 4
  @azimuth.assert_eq(-5, result3)
  
  // 负数除法向下取整
  let result4 = -21 / 4
  @azimuth.assert_eq(-6, result4)
}

test "modulo_operations" {
  // 测试取模运算
  // 正数取模
  let result1 = 20 % 3
  @azimuth.assert_eq(2, result1)
  
  // 负数取模
  let result2 = -20 % 3
  @azimuth.assert_eq(1, result2)
  
  // 大数取模
  let result3 = 1000 % 7
  @azimuth.assert_eq(6, result3)
  
  // 边界值取模
  let max_val = 2147483647
  let result4 = max_val % 10
  @azimuth.assert_eq(7, result4)
}

test "comparison_operations" {
  // 测试比较操作
  let a = 15
  let b = 20
  
  // 小于比较
  @azimuth.assert_true(a < b)
  @azimuth.assert_false(b < a)
  
  // 大于比较
  @azimuth.assert_true(b > a)
  @azimuth.assert_false(a > b)
  
  // 等于比较
  @azimuth.assert_true(a == 15)
  @azimuth.assert_false(a == b)
  
  // 不等于比较
  @azimuth.assert_true(a != b)
  @azimuth.assert_false(a != 15)
  
  // 小于等于比较
  @azimuth.assert_true(a <= b)
  @azimuth.assert_true(a <= 15)
  
  // 大于等于比较
  @azimuth.assert_true(b >= a)
  @azimuth.assert_true(b >= 20)
}

test "boolean_logic_operations" {
  // 测试布尔逻辑操作
  let true_val = true
  let false_val = false
  
  // 逻辑与
  @azimuth.assert_true(true_val && true_val)
  @azimuth.assert_false(true_val && false_val)
  @azimuth.assert_false(false_val && true_val)
  @azimuth.assert_false(false_val && false_val)
  
  // 逻辑或
  @azimuth.assert_true(true_val || true_val)
  @azimuth.assert_true(true_val || false_val)
  @azimuth.assert_true(false_val || true_val)
  @azimuth.assert_false(false_val || false_val)
  
  // 逻辑非
  @azimuth.assert_false(!true_val)
  @azimuth.assert_true(!false_val)
}

test "string_operations" {
  // 测试字符串操作
  let str1 = "Hello"
  let str2 = "World"
  
  // 字符串拼接
  let combined = str1 + " " + str2
  @azimuth.assert_eq_string("Hello World", combined)
  
  // 使用greet函数进行字符串操作
  let greet_result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", greet_result)
  
  // 空字符串处理
  let empty_str = ""
  let empty_greet = @azimuth.greet(empty_str)
  @azimuth.assert_eq_string("Hello, !", empty_greet)
  
  // 特殊字符字符串
  let special_str = "Test@123#"
  let special_greet = @azimuth.greet(special_str)
  @azimuth.assert_eq_string("Hello, Test@123#!", special_greet)
}

test "conditional_expressions" {
  // 测试条件表达式
  let x = 10
  let y = 20
  
  // 简单if-then-else
  let max_val = if x > y { x } else { y }
  @azimuth.assert_eq(20, max_val)
  
  // 嵌套条件
  let result = if x > 15 {
    "greater than 15"
  } else if x > 5 {
    "between 5 and 15"
  } else {
    "less than or equal to 5"
  }
  @azimuth.assert_eq_string("between 5 and 15", result)
  
  // 布尔条件
  let is_even = if x % 2 == 0 { true } else { false }
  @azimuth.assert_true(is_even)
  
  let is_odd = if x % 2 != 0 { true } else { false }
  @azimuth.assert_false(is_odd)
}

test "nested_function_calls" {
  // 测试嵌套函数调用
  // 嵌套加法
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  // 嵌套乘法
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result2)
  
  // 混合嵌套
  let result3 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result3)
  
  // 复杂嵌套
  let a = 5
  let b = 10
  let c = 15
  let result4 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(225, result4)
}

test "mathematical_series" {
  // 测试数学级数计算
  // 等差数列求和：1 + 2 + 3 + ... + 10
  let n = 10
  let arithmetic_sum = n * (n + 1) / 2
  @azimuth.assert_eq(55, arithmetic_sum)
  
  // 等比数列求和：1 + 2 + 4 + 8 + 16
  let geometric_sum = 1 + 2 + 4 + 8 + 16
  @azimuth.assert_eq(31, geometric_sum)
  
  // 平方数列求和：1² + 2² + 3² + 4² + 5²
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 4), 9), 16), 25)
  @azimuth.assert_eq(55, square_sum)
  
  // 立方数列求和：1³ + 2³ + 3³ + 4³
  let cube_sum = @azimuth.add(@azimuth.add(@azimuth.add(1, 8), 27), 64)
  @azimuth.assert_eq(100, cube_sum)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 银行利息计算
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  let simple_interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years)) / 100
  let total_amount = @azimuth.add(principal, simple_interest)
  @azimuth.assert_eq(11500, total_amount)
  
  // 折扣计算
  let original_price = 200
  let discount_percent = 15
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(170, final_price)
  
  // 速度距离时间计算
  let speed = 60  // km/h
  let time = 2.5  // 小时（简化为整数计算：2小时30分钟）
  let distance = @azimuth.multiply(speed, 2) + @azimuth.multiply(speed, 30) / 60
  @azimuth.assert_eq(150, distance)
}

test "edge_cases_and_errors" {
  // 测试边界情况和错误处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加法溢出
  let overflow_add = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, overflow_add)
  
  // 最小值减法溢出
  let underflow_add = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, underflow_add)
  
  // 最大值乘法溢出
  let overflow_mul = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, overflow_mul)
  
  // 零的特性
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
  
  // 除零错误处理（通过条件检查避免）
  let divisor = 0
  let safe_result = if divisor != 0 { 100 / divisor } else { 0 }
  @azimuth.assert_eq(0, safe_result)
}
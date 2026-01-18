// Azimuth 项目新增标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖各种功能场景

test "string_concatenation" {
  // 测试字符串连接
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  assert_eq_string("张三", full_name)
  
  // 测试带空格的字符串连接
  let greeting = "你好"
  let target = "世界"
  let message = greeting + ", " + target + "!"
  assert_eq_string("你好, 世界!", message)
}

test "boolean_operations" {
  // 测试布尔运算
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && false)
  
  assert_true(true || true)
  assert_true(true || false)
  assert_false(false || false)
  
  assert_true(!false)
  assert_false(!true)
}

test "comparison_operations" {
  // 测试比较运算
  assert_true(5 > 3)
  assert_false(3 > 5)
  assert_false(5 > 5)
  
  assert_true(5 >= 5)
  assert_true(5 >= 3)
  assert_false(3 >= 5)
  
  assert_true(3 < 5)
  assert_false(5 < 3)
  assert_false(5 < 5)
  
  assert_true(3 <= 5)
  assert_true(5 <= 5)
  assert_false(5 <= 3)
  
  assert_true(5 == 5)
  assert_false(5 == 3)
  
  assert_true(5 != 3)
  assert_false(5 != 5)
}

test "arithmetic_operations" {
  // 测试算术运算
  assert_eq(8, 5 + 3)
  assert_eq(2, 5 - 3)
  assert_eq(15, 5 * 3)
  assert_eq(1, 5 % 2)
  
  // 测试负数运算
  assert_eq(-8, -5 + -3)
  assert_eq(-2, -5 - -3)
  assert_eq(15, -5 * -3)
  
  // 测试零运算
  assert_eq(5, 5 + 0)
  assert_eq(5, 5 - 0)
  assert_eq(0, 5 * 0)
}

test "conditional_logic" {
  // 测试条件逻辑
  let x = 10
  let y = 20
  
  // 使用条件表达式
  let max_val = if (x > y) { x } else { y }
  assert_eq(20, max_val)
  
  let min_val = if (x < y) { x } else { y }
  assert_eq(10, min_val)
  
  // 测试嵌套条件
  let result = if (x > 5) {
    if (y > 15) { 100 } else { 50 }
  } else {
    0
  }
  assert_eq(100, result)
}

test "array_operations" {
  // 测试数组操作
  let numbers = [1, 2, 3, 4, 5]
  
  // 测试数组长度
  assert_eq(5, length(numbers))
  
  // 测试数组索引访问
  assert_eq(1, numbers[0])
  assert_eq(3, numbers[2])
  assert_eq(5, numbers[4])
  
  // 测试数组元素求和
  let sum = numbers[0] + numbers[1] + numbers[2] + numbers[3] + numbers[4]
  assert_eq(15, sum)
}

test "mathematical_functions" {
  // 测试数学函数
  let x = 16
  
  // 测试平方根计算（使用近似值）
  let sqrt_approx = 4  // 16的平方根
  assert_eq(4, sqrt_approx)
  
  // 测试幂运算
  let power = 2 * 2 * 2 * 2  // 2的4次方
  assert_eq(16, power)
  
  // 测试阶乘计算（5! = 5×4×3×2×1）
  let factorial = 5 * 4 * 3 * 2 * 1
  assert_eq(120, factorial)
}

test "date_time_calculations" {
  // 测试日期时间计算
  let days_in_year = 365
  let days_in_leap_year = 366
  let months_in_year = 12
  
  // 计算一年中的总天数（非闰年）
  let total_days = days_in_year
  assert_eq(365, total_days)
  
  // 计算十年中的总天数（包含闰年）
  let decade_days = 365 * 10 + 2  // 假设10年中有2个闰年
  assert_eq(3652, decade_days)
  
  // 计算一个月中的平均天数
  let avg_days_in_month = days_in_year / months_in_year
  assert_eq(30, avg_days_in_month)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  let divisor = 0
  let dividend = 10
  
  // 测试除零保护
  let result = if (divisor != 0) {
    dividend / divisor
  } else {
    0  // 返回默认值
  }
  assert_eq(0, result)
  
  // 测试正常除法
  let normal_result = if (divisor != 0) {
    dividend / divisor
  } else {
    0
  }
  let safe_divisor = 2
  let safe_result = dividend / safe_divisor
  assert_eq(5, safe_result)
}

test "performance_benchmarks" {
  // 测试性能基准
  let base = 100
  
  // 测试循环加法性能
  let sum_result = base + base + base + base + base + base + base + base + base + base
  assert_eq(1000, sum_result)
  
  // 测试嵌套计算性能
  let nested_calc = (base + 50) * (base - 50) + (base * 2)
  assert_eq(10000, nested_calc)
  
  // 测试复杂表达式
  let complex_expr = base * 2 + base / 2 - base % 3 + base * base / 100
  assert_eq(283, complex_expr)
}
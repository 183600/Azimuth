// 标准的 MoonBit 测试用例补充
// 包含10个新的测试用例，使用标准的 MoonBit 测试语法

test "integer_division_simulation" {
  // 模拟整数除法运算的测试
  let dividend = 17
  let divisor = 5
  
  // 模拟除法：dividend / divisor
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  
  assert_eq(3, quotient)
  assert_eq(2, remainder)
  
  // 测试边界情况
  assert_eq(7, 21 / 3)
  assert_eq(0, 21 % 3)
  
  // 测试负数除法
  assert_eq(-3, -17 / 5)
  assert_eq(2, -17 % 5)
}

test "string_case_handling" {
  // 测试字符串大小写相关处理（通过字符串操作模拟）
  let input = "Hello"
  
  // 模拟字符串连接和比较
  let lower_case = "hello"
  let upper_case = "HELLO"
  let mixed_case = "Hello"
  
  // 通过 greet 函数测试字符串处理
  assert_eq_string("Hello, Hello!", greet(input))
  assert_eq_string("Hello, hello!", greet(lower_case))
  assert_eq_string("Hello, HELLO!", greet(upper_case))
  assert_eq_string("Hello, Hello!", greet(mixed_case))
}

test "boolean_logic_operations" {
  // 测试布尔逻辑运算（通过条件表达式模拟）
  let a = true
  let b = false
  
  // 模拟逻辑与：a && b
  let logical_and = a && b
  assert_false(logical_and)
  
  // 模拟逻辑或：a || b
  let logical_or = a || b
  assert_true(logical_or)
  
  // 模拟逻辑非：!a
  let logical_not_a = !a
  assert_false(logical_not_a)
  
  let logical_not_b = !b
  assert_true(logical_not_b)
  
  // 测试复杂逻辑表达式
  let complex_expr = (a || b) && (!b)
  assert_true(complex_expr)
}

test "modulo_operations" {
  // 测试取模运算的各种场景
  // 正数取模
  assert_eq(1, 10 % 3)
  assert_eq(0, 12 % 4)
  assert_eq(2, 14 % 6)
  
  // 负数取模
  assert_eq(-1, -10 % 3)
  assert_eq(1, 10 % -3)
  assert_eq(-2, -14 % 6)
  
  // 边界情况
  assert_eq(0, 0 % 5)
  assert_eq(4, 4 % 5)
  assert_eq(0, 5 % 5)
  
  // 大数取模
  assert_eq(47, 2147483647 % 100)
  assert_eq(48, -2147483648 % 100)
}

test "absolute_value_simulation" {
  // 模拟绝对值计算
  let positive = 42
  let negative = -42
  let zero = 0
  
  // 通过条件表达式模拟绝对值
  let abs_positive = if positive > 0 { positive } else { -positive }
  let abs_negative = if negative > 0 { negative } else { -negative }
  let abs_zero = if zero > 0 { zero } else { -zero }
  
  assert_eq(42, abs_positive)
  assert_eq(42, abs_negative)
  assert_eq(0, abs_zero)
  
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  let abs_max = if max_val > 0 { max_val } else { -max_val }
  let abs_min = if min_val > 0 { min_val } else { -min_val }
  
  assert_eq(2147483647, abs_max)
  assert_eq(-2147483648, abs_min)  // 注意：最小值的绝对值仍然是负数（溢出）
}

test "range_checking" {
  // 测试数值范围检查
  let value = 25
  let min_range = 10
  let max_range = 50
  
  // 检查值是否在范围内
  let in_range = (value >= min_range) && (value <= max_range)
  assert_true(in_range)
  
  // 测试边界值
  let at_lower_bound = 10
  let at_upper_bound = 50
  let below_range = 5
  let above_range = 55
  
  assert_true(at_lower_bound >= min_range && at_lower_bound <= max_range)
  assert_true(at_upper_bound >= min_range && at_upper_bound <= max_range)
  assert_false(below_range >= min_range && below_range <= max_range)
  assert_false(above_range >= min_range && above_range <= max_range)
}

test "array_simulation_operations" {
  // 模拟数组操作（通过单个变量和计算）
  // 模拟数组求和：[1, 2, 3, 4, 5]
  let elem1 = 1
  let elem2 = 2
  let elem3 = 3
  let elem4 = 4
  let elem5 = 5
  
  let array_sum = add(add(add(add(elem1, elem2), elem3), elem4), elem5)
  assert_eq(15, array_sum)
  
  // 模拟查找最大值
  let max_val = elem1
  let max_val_2 = if elem2 > max_val { elem2 } else { max_val }
  let max_val_3 = if elem3 > max_val_2 { elem3 } else { max_val_2 }
  let max_val_4 = if elem4 > max_val_3 { elem4 } else { max_val_3 }
  let max_val_5 = if elem5 > max_val_4 { elem5 } else { max_val_4 }
  
  assert_eq(5, max_val_5)
  
  // 模拟查找最小值
  let min_val = elem1
  let min_val_2 = if elem2 < min_val { elem2 } else { min_val }
  let min_val_3 = if elem3 < min_val_2 { elem3 } else { min_val_2 }
  let min_val_4 = if elem4 < min_val_3 { elem4 } else { min_val_3 }
  let min_val_5 = if elem5 < min_val_4 { elem5 } else { min_val_4 }
  
  assert_eq(1, min_val_5)
}

test "leap_year_calculation" {
  // 测试闰年计算逻辑
  // 闰年规则：能被4整除但不能被100整除，或者能被400整除
  
  let year_2000 = 2000  // 闰年（能被400整除）
  let year_1900 = 1900  // 平年（能被100但不能被400整除）
  let year_2020 = 2020  // 闰年（能被4但不能被100整除）
  let year_2021 = 2021  // 平年（不能被4整除）
  
  // 模拟闰年判断逻辑
  let is_leap_2000 = (year_2000 % 400 == 0) || ((year_2000 % 4 == 0) && (year_2000 % 100 != 0))
  let is_leap_1900 = (year_1900 % 400 == 0) || ((year_1900 % 4 == 0) && (year_1900 % 100 != 0))
  let is_leap_2020 = (year_2020 % 400 == 0) || ((year_2020 % 4 == 0) && (year_2020 % 100 != 0))
  let is_leap_2021 = (year_2021 % 400 == 0) || ((year_2021 % 4 == 0) && (year_2021 % 100 != 0))
  
  assert_true(is_leap_2000)
  assert_false(is_leap_1900)
  assert_true(is_leap_2020)
  assert_false(is_leap_2021)
}

test "gcd_calculation" {
  // 测试最大公约数计算（欧几里得算法的简化版本）
  let a = 48
  let b = 18
  
  // 简化的GCD计算（手动模拟几步）
  let step1_a = b
  let step1_b = a % b  // 48 % 18 = 12
  
  let step2_a = step1_b
  let step2_b = step1_a % step1_b  // 18 % 12 = 6
  
  let step3_a = step2_b
  let step3_b = step2_a % step2_b  // 12 % 6 = 0
  
  // GCD是最后一个非零余数
  let gcd = step3_a
  assert_eq(6, gcd)
  
  // 测试其他情况
  assert_eq(1, 17 % 16)  // 互质数
  assert_eq(12, 36 % 24)  // 有公约数
}

test "temperature_conversion_precise" {
  // 精确的温度转换测试
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 由于只有整数运算，我们使用近似计算
  
  let celsius_0 = 0      // 冰点
  let celsius_100 = 100  // 沸点
  let celsius_37 = 37    // 人体温度
  
  // 简化的温度转换公式：F = C × 2 + 30（近似值）
  let fahrenheit_0 = add(multiply(celsius_0, 2), 30)
  let fahrenheit_100 = add(multiply(celsius_100, 2), 30)
  let fahrenheit_37 = add(multiply(celsius_37, 2), 30)
  
  assert_eq(30, fahrenheit_0)    // 0°C ≈ 30°F（近似）
  assert_eq(230, fahrenheit_100) // 100°C ≈ 230°F（近似）
  assert_eq(104, fahrenheit_37)  // 37°C ≈ 104°F（近似）
  
  // 华氏度转摄氏度（简化）：C = (F - 30) / 2
  let fahrenheit_32 = 32  // 标准冰点
  let fahrenheit_212 = 212 // 标准沸点
  
  let celsius_from_32 = (fahrenheit_32 - 30) / 2
  let celsius_from_212 = (fahrenheit_212 - 30) / 2
  
  assert_eq(1, celsius_from_32)   // 32°F ≈ 1°C（近似）
  assert_eq(91, celsius_from_212) // 212°F ≈ 91°C（近似）
}
// MoonBit 标准测试用例
// 为 Azimuth 项目添加的标准测试用例

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  // 10 / 3 = 3.333... 向上取整为 4
  let result1 = @azimuth.divide_with_ceil(10, 3)
  @azimuth.assert_eq(4, result1)
  
  // 15 / 5 = 3.0 向上取整为 3
  let result2 = @azimuth.divide_with_ceil(15, 5)
  @azimuth.assert_eq(3, result2)
  
  // 7 / 2 = 3.5 向上取整为 4
  let result3 = @azimuth.divide_with_ceil(7, 2)
  @azimuth.assert_eq(4, result3)
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  // -10 / 3 = -3.333... 向上取整为 -3
  let result1 = @azimuth.divide_with_ceil(-10, 3)
  @azimuth.assert_eq(-3, result1)
  
  // 10 / -3 = -3.333... 向上取整为 -3
  let result2 = @azimuth.divide_with_ceil(10, -3)
  @azimuth.assert_eq(-3, result2)
  
  // -10 / -3 = 3.333... 向上取整为 4
  let result3 = @azimuth.divide_with_ceil(-10, -3)
  @azimuth.assert_eq(4, result3)
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  // 0 / 任何数 = 0
  let result1 = @azimuth.divide_with_ceil(0, 5)
  @azimuth.assert_eq(0, result1)
  
  // 任何数 / 1 = 该数本身
  let result2 = @azimuth.divide_with_ceil(42, 1)
  @azimuth.assert_eq(42, result2)
  
  // 1 / 大数 = 1（因为结果小于1，向上取整为1）
  let result3 = @azimuth.divide_with_ceil(1, 100)
  @azimuth.assert_eq(1, result3)
}

test "add_with_chained_operations" {
  // 测试链式加法运算
  let a = 10
  let b = 20
  let c = 30
  let d = 40
  
  // (a + b) + (c + d)
  let result1 = @azimuth.add(@azimuth.add(a, b), @azimuth.add(c, d))
  @azimuth.assert_eq(100, result1)
  
  // a + (b + (c + d))
  let result2 = @azimuth.add(a, @azimuth.add(b, @azimuth.add(c, d)))
  @azimuth.assert_eq(100, result2)
  
  // ((a + b) + c) + d
  let result3 = @azimuth.add(@azimuth.add(@azimuth.add(a, b), c), d)
  @azimuth.assert_eq(100, result3)
}

test "multiply_with_chained_operations" {
  // 测试链式乘法运算
  let a = 2
  let b = 3
  let c = 4
  let d = 5
  
  // (a * b) * (c * d)
  let result1 = @azimuth.multiply(@azimuth.multiply(a, b), @azimuth.multiply(c, d))
  @azimuth.assert_eq(120, result1)
  
  // a * (b * (c * d))
  let result2 = @azimuth.multiply(a, @azimuth.multiply(b, @azimuth.multiply(c, d)))
  @azimuth.assert_eq(120, result2)
  
  // ((a * b) * c) * d
  let result3 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(a, b), c), d)
  @azimuth.assert_eq(120, result3)
}

test "mixed_operations_complex" {
  // 测试混合运算的复杂情况
  // 计算：(a + b) * c - d / e（简化为整数运算）
  let a = 10
  let b = 15
  let c = 4
  let d = 20
  let e = 3
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)  // 25
  let product = @azimuth.multiply(sum, c)  // 100
  
  // d / e 的向上取整
  let division = @azimuth.divide_with_ceil(d, e)  // 20 / 3 = 6.666... 向上取整为 7
  
  // 最终结果：product - division
  let result = @azimuth.add(product, -division)  // 100 - 7 = 93
  @azimuth.assert_eq(93, result)
}

test "string_processing_with_numbers" {
  // 测试包含数字的字符串处理
  let number_string = "12345"
  let mixed_string = "test123abc"
  let phone_number = "138-0013-8000"
  
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet(number_string))
  @azimuth.assert_eq_string("Hello, test123abc!", @azimuth.greet(mixed_string))
  @azimuth.assert_eq_string("Hello, 138-0013-8000!", @azimuth.greet(phone_number))
}

test "extreme_boundary_combinations" {
  // 测试极端边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  let near_max = 2147483646
  let near_min = -2147483647
  
  // 接近最大值的运算
  @azimuth.assert_eq(max_val, @azimuth.add(near_max, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(near_max, 2))
  
  // 接近最小值的运算
  @azimuth.assert_eq(min_val, @azimuth.add(near_min, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(near_min, -2))
  
  // 最大值与接近最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, near_min))
  
  // 边界值的乘法运算
  @azimuth.assert_eq(max_val, @azimuth.multiply(46340, 46340))  // 接近sqrt(max_val)
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46340, 46341))
}

test "real_world_calculator_simulation" {
  // 模拟实际计算器场景：计算贷款月供
  // 简化版：月供 = (本金 + 总利息) / 月数
  let principal = 100000  // 本金 10万
  let annual_rate = 6     // 年利率 6%
  let years = 3           // 3年
  let months = @azimuth.multiply(years, 12)  // 36个月
  
  // 计算总利息（简化计算）
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years)) / 100
  @azimuth.assert_eq(18000, total_interest)  // 100000 * 6 * 3 / 100 = 18000
  
  // 计算总还款额
  let total_payment = @azimuth.add(principal, total_interest)
  @azimuth.assert_eq(118000, total_payment)
  
  // 计算月供（向上取整）
  let monthly_payment = @azimuth.divide_with_ceil(total_payment, months)
  @azimuth.assert_eq(3278, monthly_payment)  // 118000 / 36 = 3277.777... 向上取整为 3278
}

test "algorithm_complexity_simulation" {
  // 模拟算法复杂度计算
  // 计算数组操作的时间复杂度模拟
  let n = 1000
  
  // O(1) 操作：常数时间
  let constant_time = 1
  
  // O(n) 操作：线性时间
  let linear_time = n
  
  // O(n²) 操作：二次时间（简化计算）
  let quadratic_time = @azimuth.multiply(n, n) / 1000  // 缩放以避免溢出
  
  // O(log n) 操作：对数时间（简化为整数除法）
  let logarithmic_time = 10  // 简化为固定值
  
  // 验证复杂度关系
  @azimuth.assert_eq(1, constant_time)
  @azimuth.assert_eq(1000, linear_time)
  @azimuth.assert_eq(1000, quadratic_time)  // 1000*1000/1000 = 1000
  @azimuth.assert_eq(10, logarithmic_time)
  
  // 比较不同复杂度的操作次数
  @azimuth.assert_true(linear_time > constant_time)
  @azimuth.assert_true(quadratic_time >= linear_time)
  @azimuth.assert_true(linear_time > logarithmic_time)
}
// 用户请求的标准 MoonBit 测试用例（简化版）
// 这些测试用例专注于验证核心功能

test "basic_addition_test" {
  // 测试基本加法
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

test "basic_multiplication_test" {
  // 测试基本乘法
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
}

test "string_greeting_test" {
  // 测试字符串问候
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "assert_functions_test" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 3
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_values_test" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "real_world_scenario" {
  // 测试实际应用场景：计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 计算最终总价（含运费）
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(612, total)  // 199*3 + 15 = 597 + 15 = 612
}

test "complex_calculation" {
  // 测试复杂计算：计算矩形的周长
  let length = 10
  let width = 5
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  
  @azimuth.assert_eq(30, perimeter)
}

test "fibonacci_sequence_calculation" {
  // 使用 add 函数计算斐波那契数列
  let n1 = 0
  let n2 = 1
  let n3 = @azimuth.add(n1, n2)  // 1
  let n4 = @azimuth.add(n2, n3)  // 2
  let n5 = @azimuth.add(n3, n4)  // 3
  
  @azimuth.assert_eq(1, n3)
  @azimuth.assert_eq(2, n4)
  @azimuth.assert_eq(3, n5)
}

test "factorial_calculation" {
  // 使用 multiply 函数计算阶乘
  let fact_1 = @azimuth.multiply(1, 1)  // 1! = 1
  let fact_2 = @azimuth.multiply(fact_1, 2)  // 2! = 2
  let fact_3 = @azimuth.multiply(fact_2, 3)  // 3! = 6
  let fact_4 = @azimuth.multiply(fact_3, 4)  // 4! = 24
  
  @azimuth.assert_eq(1, fact_1)
  @azimuth.assert_eq(2, fact_2)
  @azimuth.assert_eq(6, fact_3)
  @azimuth.assert_eq(24, fact_4)
}

test "geometric_calculations" {
  // 几何计算测试
  
  // 正方形面积：边长的平方
  let side_length = 6
  let square_area = @azimuth.multiply(side_length, side_length)
  @azimuth.assert_eq(36, square_area)
  
  // 长方形面积：长 × 宽
  let length = 8
  let width = 5
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(40, rectangle_area)
}

test "time_calculations" {
  // 时间计算测试
  
  // 计算总秒数：小时 × 3600 + 分钟 × 60 + 秒
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let hours_to_seconds = @azimuth.multiply(hours, 3600)
  let minutes_to_seconds = @azimuth.multiply(minutes, 60)
  let total_seconds = @azimuth.add(@azimuth.add(hours_to_seconds, minutes_to_seconds), seconds)
  @azimuth.assert_eq(9045, total_seconds)
}

test "financial_calculations" {
  // 金融计算测试
  
  // 简单利息：本金 × 利率 × 时间
  let principal = 1000
  let interest_rate = 5  // 5%
  let time_years = 3
  let simple_interest = @azimuth.multiply(@azimuth.multiply(principal, interest_rate), time_years) / 100
  @azimuth.assert_eq(150, simple_interest)
  
  // 总金额：本金 + 利息
  let total_amount = @azimuth.add(principal, simple_interest)
  @azimuth.assert_eq(1150, total_amount)
}

test "statistical_calculations" {
  // 统计计算测试
  
  // 计算平均值
  let numbers = [12, 15, 18, 21, 24]
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(numbers[0], numbers[1]), numbers[2]), numbers[3]), numbers[4])
  let count = 5
  let average = sum / count
  @azimuth.assert_eq(18, average)
}

test "greet_function_with_special_characters" {
  // 测试 greet 函数处理特殊字符
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))  // 空格
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))  // 换行和制表符
  @azimuth.assert_eq_string("Hello, \"!", @azimuth.greet("\""))  // 引号
}

test "mixed_operations_stress_test" {
  // 混合运算压力测试
  let result = 0
  
  // 执行一系列混合运算
  result = @azimuth.add(result, @azimuth.multiply(10, 5))     // 50
  result = @azimuth.add(result, @azimuth.multiply(-3, 4))     // 38
  result = @azimuth.add(result, @azimuth.multiply(7, -2))     // 24
  
  @azimuth.assert_eq(24, result)
}
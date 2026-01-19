// Azimuth 项目实用单元测试
// 使用标准 MoonBit 测试语法，提供实用的测试用例

test "add_function_comprehensive" {
  // 全面测试加法函数
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(10, @azimuth.add(5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "divide_with_ceil_function_comprehensive" {
  // 全面测试向上取整除法函数
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-12, 3))
}

test "mathematical_expressions_complex" {
  // 测试复杂的数学表达式
  let a = 6
  let b = 8
  let c = 10
  
  // 验证勾股定理
  let a_squared = @azimuth.multiply(a, a)
  let b_squared = @azimuth.multiply(b, b)
  let c_squared = @azimuth.multiply(c, c)
  let sum_of_squares = @azimuth.add(a_squared, b_squared)
  @azimuth.assert_eq(c_squared, sum_of_squares)
  
  // 测试分配律
  let x = 5
  let y = 3
  let z = 2
  let distributive1 = @azimuth.multiply(x, @azimuth.add(y, z))
  let distributive2 = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(distributive1, distributive2)
}

test "business_calculation_scenarios" {
  // 测试商业计算场景
  // 计算订单总价
  let unit_price = 99
  let quantity = 7
  let shipping_fee = 15
  let discount = 20
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total_before_shipping = @azimuth.add(subtotal, -discount)
  let final_total = @azimuth.add(total_before_shipping, shipping_fee)
  
  // 99 * 7 = 693, 693 - 20 = 673, 673 + 15 = 688
  @azimuth.assert_eq(688, final_total)
}

test "string_processing_scenarios" {
  // 测试字符串处理场景
  let name = "John"
  
  // 测试基本问候
  let first_name_greeting = @azimuth.greet(name)
  @azimuth.assert_eq_string("Hello, John!", first_name_greeting)
  
  // 测试完整名称
  let full_name = "John Doe"
  let full_name_greeting = @azimuth.greet(full_name)
  @azimuth.assert_eq_string("Hello, John Doe!", full_name_greeting)
}

test "algorithm_basic_sorting" {
  // 测试基本排序算法概念
  // 模拟冒泡排序的比较次数
  let array_size = 4
  // 最坏情况比较次数：n*(n-1)/2
  let max_comparisons = @azimuth.multiply(array_size, @azimuth.add(array_size, -1)) / 2
  @azimuth.assert_eq(6, max_comparisons)
  
  // 模拟选择排序的交换次数（最坏情况）
  let max_swaps = @azimuth.add(array_size, -1)
  @azimuth.assert_eq(3, max_swaps)
}

test "boundary_and_edge_cases" {
  // 测试边界和边缘情况
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试加法边界
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 0))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, 0))
  @azimuth.assert_eq(@azimuth.add(max_int, min_int), -1)
  
  // 测试乘法边界
  @azimuth.assert_eq(0, @azimuth.multiply(max_int, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_int, 0))
  @azimuth.assert_eq(max_int, @azimuth.multiply(max_int, 1))
  @azimuth.assert_eq(min_int, @azimuth.multiply(min_int, 1))
}

test "real_world_calculation_examples" {
  // 测试实际世界的计算示例
  // 计算平均分
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 96
  let score5 = 88
  
  let total_score = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4), score5)
  let average_score = total_score / 5
  @azimuth.assert_eq(87, average_score)  // (85+92+78+96+88)/5 = 439/5 = 87.8 -> 87 (整数除法)
  
  // 计算温度转换 (华氏度转摄氏度: (F-32)*5/9)
  let fahrenheit = 100
  let celsius = @azimuth.multiply(@azimuth.add(fahrenheit, -32), 5) / 9
  @azimuth.assert_eq(37, celsius)  // (100-32)*5/9 = 68*5/9 = 340/9 = 37.7 -> 37 (整数除法)
}
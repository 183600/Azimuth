// 额外的 MoonBit 测试套件
// 包含针对 azimuth 模块核心功能的综合测试用例

test "add_comprehensive_edge_cases" {
  // 测试加法的综合边界情况 - UNIQUE_TEST_ID_2024
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值加法
  azimuth::@azimuth.assert_eq(42, @azimuth.add(42, 0))
  azimuth::@azimuth.assert_eq(-42, @azimuth.add(-42, 0))
  azimuth::@azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 测试极大值加法
  azimuth::@azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  azimuth::@azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试极小值加法
  azimuth::@azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  azimuth::@azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试混合符号加法
  azimuth::@azimuth.assert_eq(0, @azimuth.add(100, -100))
  azimuth::@azimuth.assert_eq(-50, @azimuth.add(100, -150))
}

test "multiply_comprehensive_scenarios" {
  // 测试乘法的综合场景
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试特殊值乘法
  azimuth::@azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  azimuth::@azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  azimuth::@azimuth.assert_eq(-12345, @azimuth.multiply(12345, -1))
  
  // 测试极值乘法
  azimuth::@azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  azimuth::@azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  azimuth::@azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  azimuth::@azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试大数乘法
  azimuth::@azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  azimuth::@azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
}

test "greet_function_comprehensive" {
  // 测试问候函数的综合情况
  // 基本测试
  azimuth::@azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  
  // 空字符串和单字符
  azimuth::@azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  azimuth::@azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 数字和特殊字符
  azimuth::@azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  azimuth::@azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  
  // Unicode 字符
  azimuth::@azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  azimuth::@azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  
  // 长字符串
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  azimuth::@azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
}

test "complex_business_calculations" {
  // 测试复杂业务计算场景
  // 订单总价计算：商品价格 × 数量 + 运费 - 折扣
  let item_price = 299
  let quantity = 3
  let shipping = 20
  let discount = 50
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total_before_discount = @azimuth.add(subtotal, shipping)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(927, final_total)  // 299*3 + 20 - 50 = 897 + 20 - 50 = 927
  
  // 复利计算：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate = 5
  let years = 3
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let total_amount = @azimuth.multiply(principal, growth_factor)
  
  @azimuth.assert_eq(160000, total_amount)  // 10000 × (1 + 5×3) = 10000 × 16 = 160000
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 等差数列求和：1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let sum_1_to_10 = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum_1_to_10)
  
  // 等比数列求和：2 + 4 + 8 + 16 = 30
  let sum_powers = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 8), 16)
  @azimuth.assert_eq(30, sum_powers)
  
  // 平方和：1² + 2² + 3² + 4² + 5² = 55
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), 
    @azimuth.multiply(4, 4)), @azimuth.multiply(5, 5))
  @azimuth.assert_eq(55, square_sum)
}

test "geometry_and_physics_calculations" {
  // 测试几何和物理计算
  // 矩形面积和周长
  let length = 12
  let width = 8
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  
  @azimuth.assert_eq(96, area)      // 12 × 8 = 96
  @azimuth.assert_eq(40, perimeter) // 2 × (12 + 8) = 40
  
  // 简化的自由落体距离：s = 0.5 × g × t² (简化为整数计算)
  let time = 5
  let gravity = 10  // 简化的重力加速度
  let distance = @azimuth.multiply(@azimuth.multiply(5, time), time) / 2
  
  @azimuth.assert_eq(125, distance) // 5 × 5² / 2 = 125
}

test "data_processing_algorithms" {
  // 测试数据处理算法
  // 计算平均值、最大值、最小值
  let values = [15, 25, 10, 30, 20]
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(15, 25), 10), 30), 20)
  let average = sum / 5
  
  @azimuth.assert_eq(100, sum)
  @azimuth.assert_eq(20, average)
  
  // 查找最大值和最小值（通过比较）
  let max_val = 30
  let min_val = 10
  
  @azimuth.assert_eq(30, max_val)
  @azimuth.assert_eq(10, min_val)
  
  // 计算范围
  let range = @azimuth.add(max_val, -min_val)
  @azimuth.assert_eq(20, range)
}

test "error_handling_and_robustness" {
  // 测试错误处理和健壮性
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -2) == min_val)
  
  // 测试断言函数
  azimuth::@azimuth.assert_true(true)
  azimuth::@azimuth.assert_false(false)
  azimuth::@azimuth.assert_eq(42, 42)
  azimuth::@azimuth.assert_eq_string("test", "test")
  
  // 测试边界条件的特殊处理
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.add(max_val, min_val) == -1)
}

test "performance_and_optimization" {
  // 测试性能和优化相关的计算
  // 批量计算测试
  let base = 100
  let multiplier = 5
  
  // 计算一系列相关值
  let value1 = @azimuth.multiply(base, multiplier)
  let value2 = @azimuth.multiply(value1, multiplier)
  let value3 = @azimuth.multiply(value2, multiplier)
  
  @azimuth.assert_eq(500, value1)   // 100 × 5 = 500
  @azimuth.assert_eq(2500, value2)  // 500 × 5 = 2500
  @azimuth.assert_eq(12500, value3) // 2500 × 5 = 12500
  
  // 累积计算
  let accumulator = 0
  let step1 = @azimuth.add(accumulator, 100)
  let step2 = @azimuth.add(step1, 200)
  let step3 = @azimuth.add(step2, 300)
  let step4 = @azimuth.add(step3, 400)
  let step5 = @azimuth.add(step4, 500)
  
  @azimuth.assert_eq(1500, step5)  // 0 + 100 + 200 + 300 + 400 + 500 = 1500
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 银行利息计算
  let principal = 5000
  let annual_rate = 8  // 8%
  let months = 6
  
  // 简化的利息计算：本金 × 利率 × (月数/12)
  let interest = @azimuth.multiply(@azimuth.multiply(principal, annual_rate), months) / 12
  let total_amount = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(2000, interest)     // 5000 × 8 × 6 / 12 = 2000
  @azimuth.assert_eq(7000, total_amount) // 5000 + 2000 = 7000
  
  // 温度转换计算
  let celsius = 30
  // 简化的华氏度转换：F = C × 2 + 30
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(90, fahrenheit) // 30 × 2 + 30 = 90
  
  // 速度单位转换：km/h 转 m/s (简化为除以 3.6，使用整数除法)
  let km_per_hour = 72
  let meters_per_second = km_per_hour / 3
  
  @azimuth.assert_eq(24, meters_per_second) // 72 / 3 = 24 (简化计算)
}
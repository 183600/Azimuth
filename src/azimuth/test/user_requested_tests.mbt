// 用户请求的标准 MoonBit 测试用例
// 这些测试用例专注于验证核心功能的边界条件和实际应用场景

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(8, 4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 4))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-4, 4))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.divide_with_ceil(max_val, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.divide_with_ceil(min_val, 1))
}

test "addition_chain_optimization" {
  // 测试加法链优化：多个数相加的不同顺序
  let numbers = [10, 20, 30, 40, 50]
  
  // 从左到右相加
  let left_to_right = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(numbers[0], numbers[1]), numbers[2]), numbers[3]), numbers[4])
  
  // 从右到左相加
  let right_to_left = @azimuth.add(numbers[4], @azimuth.add(numbers[3], @azimuth.add(numbers[2], @azimuth.add(numbers[0], numbers[1]))))
  
  // 分组相加
  let grouped = @azimuth.add(@azimuth.add(numbers[0], numbers[4]), @azimuth.add(numbers[1], @azimuth.add(numbers[2], numbers[3])))
  
  @azimuth.assert_eq(left_to_right, right_to_left)
  @azimuth.assert_eq(left_to_right, grouped)
  @azimuth.assert_eq(150, left_to_right)
}

test "multiplication_precision_test" {
  // 测试乘法精度和边界情况
  let test_cases = [
    (1000, 1000, 1000000),
    (12345, 6789, 83810205),
    (100, 200, 20000),
    (7, 8, 56),
    (15, 15, 225)
  ]
  
  for i = 0; i < test_cases.length(); i = i + 1 {
    let (a, b, expected) = test_cases[i]
    @azimuth.assert_eq(expected, @azimuth.multiply(a, b))
  }
}

test "string_processing_internationalization" {
  // 测试字符串处理的国际化场景
  let greetings = [
    ("English", "Hello, English!"),
    ("中文", "Hello, 中文!"),
    ("Español", "Hello, Español!"),
    ("Français", "Hello, Français!"),
    ("日本語", "Hello, 日本語!"),
    ("العربية", "Hello, العربية!"),
    ("한국어", "Hello, 한국어!")
  ]
  
  for i = 0; i < greetings.length(); i = i + 1 {
    let (name, expected) = greetings[i]
    @azimuth.assert_eq_string(expected, @azimuth.greet(name))
  }
}

test "mathematical_series_fibonacci_style" {
  // 测试类似斐波那契数列的计算（使用现有函数模拟）
  // F(n) = F(n-1) + F(n-2)，其中 F(1) = 1, F(2) = 1
  let f1 = 1
  let f2 = 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  let f8 = @azimuth.add(f6, f7)  // 21
  
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
  @azimuth.assert_eq(13, f7)
  @azimuth.assert_eq(21, f8)
}

test "real_world_calculations_bmi" {
  // 测试实际应用场景：BMI计算（简化版）
  // BMI = 体重(kg) / (身高(m))^2
  // 为避免浮点数，我们使用：BMI指数 = 体重(kg) × 10000 / (身高(cm))^2
  
  let weight_kg = 70  // 70公斤
  let height_cm = 175 // 175厘米
  
  // 计算BMI指数（整数）
  let bmi_numerator = @azimuth.multiply(weight_kg, 10000)
  let bmi_denominator = @azimuth.multiply(height_cm, height_cm)
  let bmi_index = bmi_numerator / bmi_denominator
  
  // 预期BMI值：70 × 10000 / (175 × 175) = 700000 / 30625 ≈ 22.85
  // 整数部分应为22
  @azimuth.assert_eq(22, bmi_index)
  
  // 测试另一个案例
  let weight_kg2 = 60
  let height_cm2 = 160
  
  let bmi_numerator2 = @azimuth.multiply(weight_kg2, 10000)
  let bmi_denominator2 = @azimuth.multiply(height_cm2, height_cm2)
  let bmi_index2 = bmi_numerator2 / bmi_denominator2
  
  // 预期BMI值：60 × 10000 / (160 × 160) = 600000 / 25600 ≈ 23.43
  // 整数部分应为23
  @azimuth.assert_eq(23, bmi_index2)
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟：O(n²) vs O(n log n)（简化版）
  let n = 100
  
  // 模拟O(n²)算法：n × n
  let quadratic_time = @azimuth.multiply(n, n)
  
  // 模拟O(n log n)算法：n × log₂(n)，这里简化为 n × 7（因为 log₂(100) ≈ 6.6）
  let n_log_n_time = @azimuth.multiply(n, 7)
  
  // 验证复杂度关系
  @azimuth.assert_true(quadratic_time > n_log_n_time)
  @azimuth.assert_eq(10000, quadratic_time)
  @azimuth.assert_eq(700, n_log_n_time)
}

test "data_structure_array_operations" {
  // 测试数据结构操作：模拟数组操作
  // 创建一个表示数组的累加和
  let array_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, array_sum)
  
  // 计算数组元素的平均值
  let array_length = 5
  let array_average = array_sum / array_length
  @azimuth.assert_eq(3, array_average)
  
  // 计算数组的最大值（通过比较）
  let max_val = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_true(max_val >= 1 && max_val >= 2 && max_val >= 3 && max_val >= 4 && max_val >= 5)
}
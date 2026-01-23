// 高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行深度测试

test "mathematical_identity_verification" {
  // 验证数学恒等式：加法单位元和乘法单位元
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(-1, 0))
  assert_eq(-1, add(0, -1))
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  
  assert_eq(0, multiply(0, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款分期计算
  let principal = 100000
  let annual_rate = 6
  let years = 30
  
  let total_months = multiply(years, 12)
  let monthly_rate = divide_with_ceil(annual_rate, 12)
  let total_interest = divide_with_ceil(multiply(principal, multiply(monthly_rate, total_months)), 100)
  let total_payment = add(principal, total_interest)
  let monthly_payment = divide_with_ceil(total_payment, total_months)
  
  assert_eq(360, total_months)
  assert_eq(6, monthly_rate)
  assert_eq(216000, total_interest)
  assert_eq(316000, total_payment)
  assert_eq(878, monthly_payment)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟：不同数据规模下的操作计数
  let n1 = 100
  let operations1 = multiply(n1, n1)
  let processing_time1 = divide_with_ceil(operations1, 10000)
  assert_eq(10000, operations1)
  assert_eq(1, processing_time1)
  
  let n2 = 1000
  let operations2 = multiply(n2, n2)
  let processing_time2 = divide_with_ceil(operations2, 10000)
  assert_eq(1000000, operations2)
  assert_eq(100, processing_time2)
  
  let n3 = 10000
  let operations3 = multiply(n3, n3)
  let processing_time3 = divide_with_ceil(operations3, 10000)
  assert_eq(100000000, operations3)
  assert_eq(10000, processing_time3)
}

test "string_processing_pipeline" {
  // 字符串处理流水线：构建复杂的用户标识
  let greeting1 = greet("user123@engineering.techcorp")
  assert_eq_string("Hello, user123@engineering.techcorp!", greeting1)
  
  let greeting2 = greet("admin#1")
  assert_eq_string("Hello, admin#1!", greeting2)
}

test "resource_optimization_problem" {
  // 资源优化问题：装箱问题简化版
  let total_weight = add(add(add(add(15, 25), 30), 20), 10)
  let box_capacity = 50
  
  let boxes_needed = divide_with_ceil(total_weight, box_capacity)
  let used_capacity = multiply(boxes_needed, box_capacity)
  let remaining_capacity = add(used_capacity, -total_weight)
  
  assert_eq(100, total_weight)
  assert_eq(2, boxes_needed)
  assert_eq(0, remaining_capacity)
}

test "statistical_analysis_simulation" {
  // 统计分析模拟：计算均值、方差等统计量
  let sum = add(add(add(add(10, 15), 20), 25), 30)
  let data_count = 5
  
  let mean = divide_with_ceil(sum, data_count)
  
  let diff1 = add(10, -mean)
  let diff2 = add(15, -mean)
  let diff3 = add(20, -mean)
  let diff4 = add(25, -mean)
  let diff5 = add(30, -mean)
  
  let variance_sum = add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), multiply(diff3, diff3)), add(multiply(diff4, diff4), multiply(diff5, diff5)))
  let variance = divide_with_ceil(variance_sum, data_count)
  
  assert_eq(100, sum)
  assert_eq(20, mean)
  assert_eq(50, variance)
}

test "cryptography_hash_simulation" {
  // 密码学哈希模拟：简化的哈希函数
  let h1 = multiply(0, 31) + 72
  let h2 = multiply(h1, 31) + 101
  let h3 = multiply(h2, 31) + 108
  let h4 = multiply(h3, 31) + 108
  let h5 = multiply(h4, 31) + 111
  
  assert_eq(72, h1)
  assert_eq(2333, h2)
  assert_eq(72441, h3)
  assert_eq(2245699, h4)
  assert_eq(69616680, h5)
}

test "physics_engine_calculation" {
  // 物理引擎计算：抛物运动模拟
  let initial_velocity = 20
  let gravity = 10
  
  let height1 = add(multiply(initial_velocity, 1), -divide_with_ceil(multiply(gravity, multiply(1, 1)), 2))
  let height2 = add(multiply(initial_velocity, 2), -divide_with_ceil(multiply(gravity, multiply(2, 2)), 2))
  let height3 = add(multiply(initial_velocity, 3), -divide_with_ceil(multiply(gravity, multiply(3, 3)), 2))
  
  assert_eq(15, height1)
  assert_eq(20, height2)
  assert_eq(15, height3)
}

test "database_query_optimization" {
  // 数据库查询优化：索引选择模拟
  let table_size = 1000000
  
  let scanned_rows1 = divide_with_ceil(table_size, 1)
  assert_eq(1000000, scanned_rows1)
  
  let scanned_rows2 = divide_with_ceil(table_size, 10)
  assert_eq(100000, scanned_rows2)
  
  let scanned_rows3 = divide_with_ceil(table_size, 100)
  assert_eq(10000, scanned_rows3)
  
  let scanned_rows4 = divide_with_ceil(table_size, 1000)
  assert_eq(1000, scanned_rows4)
}

test "machine_learning_gradient_descent" {
  // 机器学习梯度下降：线性回归简化版
  let weight1 = 0
  let bias1 = 0
  let target_slope = 2
  let target_intercept = 5
  
  let weight_gradient1 = add(target_slope, -weight1)
  let bias_gradient1 = add(target_intercept, -bias1)
  let weight2 = add(weight1, divide_with_ceil(multiply(1, weight_gradient1), 1))
  let bias2 = add(bias1, divide_with_ceil(multiply(1, bias_gradient1), 1))
  
  let weight_gradient2 = add(target_slope, -weight2)
  let bias_gradient2 = add(target_intercept, -bias2)
  let weight3 = add(weight2, divide_with_ceil(multiply(1, weight_gradient2), 2))
  let bias3 = add(bias2, divide_with_ceil(multiply(1, bias_gradient2), 2))
  
  assert_eq(2, weight3)
  assert_eq(5, bias3)
}
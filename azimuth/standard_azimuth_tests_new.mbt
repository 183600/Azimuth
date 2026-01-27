// Azimuth 标准测试用例 - 10个高质量测试用例
// 使用标准 MoonBit 测试语法

test "add_function_edge_cases" {
  // 测试加法函数的边缘情况
  assert_eq(0, add(0, 0))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(-1000000, add(-500000, -500000))
  assert_eq(0, add(-500000, 500000))
  assert_eq(2147483647, add(2147483646, 1))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(0, multiply(0, 123456))
  assert_eq(123456, multiply(1, 123456))
  assert_eq(-123456, multiply(-1, 123456))
  assert_eq(123456, multiply(-1, -123456))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1000000, divide_with_ceil(1999999, 2))
  assert_eq(666667, divide_with_ceil(2000000, 3))
  assert_eq(333334, divide_with_ceil(1000000, 3))
  assert_eq(0, divide_with_ceil(0, 1000))
}

test "greet_function_multilingual" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 42
  let b = 17
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证不同数值组合
  assert_eq(add(100, 200), add(200, 100))
  assert_eq(multiply(15, 20), multiply(20, 15))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 场景：计算项目资源需求
  let team_size = 12
  let tasks_per_person = 8
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 25
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(96, total_tasks)  // 12 * 8 = 96
  assert_eq(4, sprints_needed)  // ceil(96/25) = 4
  
  // 验证总容量
  let total_capacity = multiply(sprints_needed, tasks_per_sprint)
  assert_eq(100, total_capacity)
}

test "subtract_function_validation" {
  // 测试减法函数的验证
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(10, 10))
  assert_eq(1000000, subtract(1500000, 500000))
  assert_eq(-1000000, subtract(500000, 1500000))
  assert_eq(0, subtract(0, 0))
}

test "resource_allocation_scenario" {
  // 测试资源分配场景
  // 场景：计算活动安排
  let participants = 156
  let tables = 15
  let chairs_per_table = 12
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(13, tables_needed)  // ceil(156/12) = 13
  
  // 计算可用座位数
  let available_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(156, available_seats)
  
  // 验证座位充足性
  let remaining_seats = subtract(available_seats, participants)
  assert_eq(0, remaining_seats)
}

test "error_handling_comprehensive" {
  // 测试错误处理综合情况
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "algorithm_pipeline_optimization" {
  // 测试算法流水线优化
  let input_data = 100
  let processing_factor = 3
  let batch_size = 8
  let optimization_factor = 2
  
  // 第一阶段：数据处理
  let stage1_result = multiply(input_data, processing_factor)
  assert_eq(300, stage1_result)
  
  // 第二阶段：批处理
  let stage2_result = divide_with_ceil(stage1_result, batch_size)
  assert_eq(38, stage2_result)  // ceil(300/8) = 38
  
  // 第三阶段：优化
  let final_result = multiply(stage2_result, optimization_factor)
  assert_eq(76, final_result)
  
  // 验证整个流水线的效率
  let efficiency_ratio = divide_with_ceil(final_result, input_data)
  assert_eq(1, efficiency_ratio)  // ceil(76/100) = 1
}
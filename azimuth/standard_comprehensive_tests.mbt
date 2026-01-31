// 标准 MoonBit 测试用例 - 10个高质量测试用例
// 覆盖 azimuth 库的核心功能和边缘情况

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
  assert_eq(0, subtract(0, 0))
  assert_eq(1000000, subtract(1000000, 0))
}

test "all_functions_integration_test" {
  // 所有函数的集成测试
  let a = 15
  let b = 4
  
  // 组合运算测试
  let sum = add(a, b)
  let difference = subtract(a, b)
  let product = multiply(a, b)
  let quotient = divide_with_ceil(a, b)
  
  assert_eq(19, sum)
  assert_eq(11, difference)
  assert_eq(60, product)
  assert_eq(4, quotient)
  
  // 验证数学关系
  assert_eq(a, add(difference, b))
  assert_eq(a, subtract(sum, b))
}

test "divide_with_ceil_comprehensive_edge_cases" {
  // 向上取整除法的全面边缘情况测试
  // 正数除法
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数除法
  assert_eq(-2, divide_with_ceil(-10, 4))
  assert_eq(-3, divide_with_ceil(10, -4))
  assert_eq(3, divide_with_ceil(-10, -4))
  
  // 零值处理
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 25
  let y = 7
  let z = 3
  
  // 加法性质
  assert_eq(add(x, y), add(y, x))  // 交换律
  assert_eq(add(add(x, y), z), add(x, add(y, z)))  // 结合律
  assert_eq(x, add(x, 0))  // 单位元
  
  // 乘法性质
  assert_eq(multiply(x, y), multiply(y, x))  // 交换律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))  // 结合律
  assert_eq(x, multiply(x, 1))  // 单位元
  assert_eq(0, multiply(x, 0))  // 零元
  
  // 分配律
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "business_logistics_calculation" {
  // 物流计算业务场景测试
  let total_weight = 1250  // 总重量(kg)
  let max_weight_per_truck = 300  // 每辆卡车最大载重(kg)
  let trucks_needed = divide_with_ceil(total_weight, max_weight_per_truck)
  
  assert_eq(5, trucks_needed)  // ceil(1250/300) = 5
  
  // 计算实际载重能力
  let total_capacity = multiply(trucks_needed, max_weight_per_truck)
  assert_eq(1500, total_capacity)
  
  // 计算剩余载重能力
  let remaining_capacity = subtract(total_capacity, total_weight)
  assert_eq(250, remaining_capacity)
  
  // 计算载重利用率
  let utilization_percent = divide_with_ceil(multiply(total_weight, 100), total_capacity)
  assert_eq(84, utilization_percent)  // floor(125000/1500) = 83.33% -> 84%
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  // 基本测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 特殊字符测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  
  // Unicode 字符测试
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  
  // 数字和符号测试
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目成本估算
  let team_size = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 15
  let overhead_rate = 20  // 20% 管理费用
  
  // 计算基础工时
  let total_hours = multiply(team_size, multiply(hours_per_day, project_days))
  assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  
  // 计算基础成本
  let base_cost = multiply(total_hours, hourly_rate)
  assert_eq(72000, base_cost)  // 960 * 75 = 72000
  
  // 计算管理费用
  let overhead_cost = divide_with_ceil(multiply(base_cost, overhead_rate), 100)
  assert_eq(14400, overhead_cost)  // ceil(72000 * 0.2) = 14400
  
  // 计算总成本
  let total_cost = add(base_cost, overhead_cost)
  assert_eq(86400, total_cost)
  
  // 计算每阶段预算（假设分4个阶段）
  let phases = 4
  let budget_per_phase = divide_with_ceil(total_cost, phases)
  assert_eq(21600, budget_per_phase)  // ceil(86400/4) = 21600
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  
  // 极值运算测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 加法极值测试（避免溢出）
  assert_eq(2147483646, add(2147483645, 1))
  assert_eq(-2147483647, add(-2147483646, -1))
  
  // 乘法极值测试（使用较小的数避免溢出）
  assert_eq(46340, multiply(46340, 1))  // sqrt(INT_MAX)
  assert_eq(-46340, multiply(-46340, 1))
  
  // 减法极值测试
  assert_eq(2147483647, subtract(2147483647, 0))
  assert_eq(-2147483648, subtract(-2147483648, 0))
}

test "resource_management_optimization" {
  // 资源管理优化测试
  // 场景：服务器资源分配
  let total_cpu_cores = 48
  let total_memory_gb = 128
  let applications = 5
  
  // CPU 分配
  let cpu_per_app = divide_with_ceil(total_cpu_cores, applications)
  assert_eq(10, cpu_per_app)  // ceil(48/5) = 10
  
  let total_allocated_cpu = multiply(cpu_per_app, applications)
  assert_eq(50, total_allocated_cpu)
  
  let cpu_waste = subtract(total_allocated_cpu, total_cpu_cores)
  assert_eq(2, cpu_waste)
  
  // 内存分配
  let memory_per_app = divide_with_ceil(total_memory_gb, applications)
  assert_eq(26, memory_per_app)  // ceil(128/5) = 26
  
  let total_allocated_memory = multiply(memory_per_app, applications)
  assert_eq(130, total_allocated_memory)
  
  let memory_waste = subtract(total_allocated_memory, total_memory_gb)
  assert_eq(2, memory_waste)
  
  // 计算资源利用率
  let cpu_utilization = divide_with_ceil(multiply(total_cpu_cores, 100), total_allocated_cpu)
  let memory_utilization = divide_with_ceil(multiply(total_memory_gb, 100), total_allocated_memory)
  
  assert_eq(96, cpu_utilization)    // floor(4800/50) = 96%
  assert_eq(99, memory_utilization)  // floor(12800/130) = 98.46% -> 99%
}

test "performance_and_scalability_test" {
  // 性能和可扩展性测试
  // 场景：大数据量处理性能测试
  let data_size = 100000
  let batch_size = 1000
  let processing_time_per_batch = 5  // 秒
  
  // 计算批次数
  let batches_needed = divide_with_ceil(data_size, batch_size)
  assert_eq(100, batches_needed)  // ceil(100000/1000) = 100
  
  // 计算总处理时间
  let total_processing_time = multiply(batches_needed, processing_time_per_batch)
  assert_eq(500, total_processing_time)  // 100 * 5 = 500秒
  
  // 计算每秒处理量
  let throughput = divide_with_ceil(data_size, total_processing_time)
  assert_eq(200, throughput)  // ceil(100000/500) = 200
  
  // 计算并行处理的优势（假设4个并行处理器）
  let parallel_processors = 4
  let parallel_batches_needed = divide_with_ceil(batches_needed, parallel_processors)
  assert_eq(25, parallel_batches_needed)  // ceil(100/4) = 25
  
  let parallel_total_time = multiply(parallel_batches_needed, processing_time_per_batch)
  assert_eq(125, parallel_total_time)  // 25 * 5 = 125秒
  
  // 计算性能提升
  let performance_improvement = divide_with_ceil(total_processing_time, parallel_total_time)
  assert_eq(4, performance_improvement)  // ceil(500/125) = 4倍提升
  
  // 计算加速效率
  let theoretical_speedup = parallel_processors
  let actual_efficiency = divide_with_ceil(multiply(performance_improvement, 100), theoretical_speedup)
  assert_eq(100, actual_efficiency)  // (4*100)/4 = 100% 效率
}
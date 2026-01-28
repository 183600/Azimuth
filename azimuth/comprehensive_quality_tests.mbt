// 综合质量测试用例 - 为 Azimuth 项目创建
// 包含 10 个高质量的测试用例，使用标准 MoonBit 测试语法
// 测试覆盖：基本功能、边界情况、错误处理、业务逻辑和数学性质

test "add_function_comprehensive_coverage" {
  // 全面测试加法函数的各种情况
  assert_eq(8, add(5, 3))           // 正数加法
  assert_eq(0, add(0, 0))            // 零值加法
  assert_eq(-8, add(-5, -3))         // 负数加法
  assert_eq(2, add(-5, 7))           // 混合符号加法
  assert_eq(1000000, add(500000, 500000))  // 大数加法
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边缘情况
  assert_eq(0, multiply(0, 12345))    // 零乘法
  assert_eq(12345, multiply(1, 12345)) // 单位元乘法
  assert_eq(-12345, multiply(-1, 12345)) // 负单位元乘法
  assert_eq(12345, multiply(-1, -12345)) // 负负得正
  assert_eq(1000000, multiply(1000, 1000)) // 大数乘法
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "divide_with_ceil_logistics_calculation" {
  // 测试向上取整除法的物流计算场景
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)  // ceil(47/10) = 5
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(50, total_capacity)
  
  // 验证剩余运力
  let remaining_capacity = subtract(total_capacity, packages)
  assert_eq(3, remaining_capacity)
}

test "subtract_function_comprehensive_test" {
  // 全面测试减法函数
  assert_eq(2, subtract(5, 3))        // 正数减法
  assert_eq(0, subtract(5, 5))        // 相等数减法
  assert_eq(-2, subtract(3, 5))       // 负结果减法
  assert_eq(10, subtract(5, -5))      // 减负数
  assert_eq(-10, subtract(-5, 5))     // 负数减正数
}

test "complex_business_workflow" {
  // 测试复杂业务工作流程
  // 场景：项目资源规划和预算分配
  let team_members = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 15
  let budget_per_sprint = 10000
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, project_days))
  assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  
  // 计算总预算
  let total_budget = multiply(total_hours, hourly_rate)
  assert_eq(72000, total_budget)  // 960 * 75 = 72000
  
  // 计算需要的冲刺数量
  let sprints_needed = divide_with_ceil(total_budget, budget_per_sprint)
  assert_eq(8, sprints_needed)  // ceil(72000/10000) = 8
}

test "mathematical_properties_verification" {
  // 验证数学性质和恒等式
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
  
  // 结合律验证
  assert_eq(
    add(add(x, y), 5),
    add(x, add(y, 5))
  )
}

test "error_handling_and_boundary_conditions" {
  // 测试错误处理和边界条件
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 边界值测试
  assert_eq(2147483647, add(2147483646, 1))  // 接近最大值
  assert_eq(-2147483648, add(-2147483647, -1))  // 接近最小值
}

test "resource_optimization_scenario" {
  // 测试资源优化场景
  // 场景：服务器集群资源分配
  let total_requests = 10000
  let servers = 6
  let requests_per_server = divide_with_ceil(total_requests, servers)
  assert_eq(1667, requests_per_server)  // ceil(10000/6) = 1667
  
  // 计算实际处理能力
  let actual_capacity = multiply(requests_per_server, servers)
  assert_eq(10002, actual_capacity)
  
  // 计算资源利用率
  let utilization_percentage = divide_with_ceil(
    multiply(total_requests, 100),
    actual_capacity
  )
  assert_eq(100, utilization_percentage)  // ceil(1000000/10002) = 100
  
  // 优化分配策略
  let optimized_servers = add(servers, 1)
  let optimized_requests_per_server = divide_with_ceil(total_requests, optimized_servers)
  assert_eq(1430, optimized_requests_per_server)  // ceil(10000/7) = 1430
}

test "data_validation_and_string_processing" {
  // 测试数据验证和字符串处理
  // 测试各种字符串输入
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !@#$%^!", greet("!@#$%^"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello,   spaces!", greet("  spaces  "))
  
  // 复杂数据验证场景
  let valid_username = "user123"
  let greeting_result = greet(valid_username)
  let expected_length = add(7, 6)  // "Hello, " + "user123!"
  assert_eq_string("Hello, user123!", greeting_result)
}
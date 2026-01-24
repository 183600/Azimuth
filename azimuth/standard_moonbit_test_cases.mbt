// 标准 MoonBit 测试用例
// 为 azimuth 库添加的高质量测试用例

test "add_function_various_scenarios" {
  // 测试加法函数的各种场景
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-5, 6))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  assert_eq(10000, multiply(100, 100))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_business_scenarios" {
  // 测试向上取整除法的业务场景
  // 场景：计算需要的运输车辆
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // 场景：计算需要的存储空间
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(5, boxes_needed)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_members = 6
  let tasks_per_member = 4
  let sprint_capacity = 10
  
  // 计算总任务数
  let total_tasks = multiply(team_members, tasks_per_member)
  assert_eq(24, total_tasks)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(3, sprints_needed)
  
  // 计算总容量
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(30, total_capacity)
}

test "error_handling_and_boundaries" {
  // 错误处理和边界条件测试
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1, divide_with_ceil(100, 100))
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 场景：仓库存储优化
  let total_items = 89
  let container_capacity = 15
  
  // 计算需要的容器数量
  let containers_needed = divide_with_ceil(total_items, container_capacity)
  assert_eq(6, containers_needed)
  
  // 计算总存储容量
  let total_capacity = multiply(containers_needed, container_capacity)
  assert_eq(90, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !@#$%^!", greet("!@#$%^"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "sequential_operations_chain" {
  // 连续操作链测试
  let initial_value = 5
  
  // 连续计算链
  let step1 = add(initial_value, 3)        // 8
  let step2 = multiply(step1, 2)           // 16
  let step3 = divide_with_ceil(step2, 5)   // 4
  let step4 = add(step3, 1)                // 5
  
  assert_eq(5, step4)
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}
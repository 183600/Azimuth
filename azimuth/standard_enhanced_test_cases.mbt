// 标准 MoonBit 测试用例 - 为 azimuth 库创建的高质量测试
// 包含基本功能测试、边界条件测试和实际应用场景测试

test "add_function_comprehensive" {
  // 全面测试加法函数的各种情况
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-3, 5))
  assert_eq(100000, add(50000, 50000))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 999))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(1, multiply(1, 999999))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符和特殊符号支持
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_business_scenarios" {
  // 测试向上取整除法的实际业务场景
  // 场景1: 计算需要的车辆数量
  let passengers = 23
  let seats_per_vehicle = 6
  let vehicles_needed = divide_with_ceil(passengers, seats_per_vehicle)
  assert_eq(4, vehicles_needed)
  
  // 场景2: 计算存储空间需求
  let files = 87
  let files_per_folder = 15
  let folders_needed = divide_with_ceil(files, files_per_folder)
  assert_eq(6, folders_needed)
}

test "complex_workflow_calculation" {
  // 测试复杂工作流程计算
  // 项目资源计算场景
  let team_size = 7
  let tasks_per_member = 8
  let total_tasks = multiply(team_size, tasks_per_member)  // 56个任务
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(3, sprints_needed)  // 需要3个冲刺
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(60, total_capacity)
}

test "mathematical_laws_verification" {
  // 验证基本数学定律
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 5)),
    add(multiply(x, y), multiply(x, 5))
  )
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边缘情况
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 场景：仓库货物装箱
  let product_a = 35  // 产品A数量
  let product_b = 42  // 产品B数量
  let box_capacity = 20
  
  let boxes_a = divide_with_ceil(product_a, box_capacity)  // ceil(35/20) = 2
  let boxes_b = divide_with_ceil(product_b, box_capacity)  // ceil(42/20) = 3
  let total_boxes = add(boxes_a, boxes_b)
  
  assert_eq(2, boxes_a)
  assert_eq(3, boxes_b)
  assert_eq(5, total_boxes)
  
  // 验证总容量
  let total_capacity = multiply(total_boxes, box_capacity)
  assert_eq(100, total_capacity)
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  assert_eq_string("Hello, user123!", greet("user123"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, @#$%^&*!", greet("@#$%^&*"))
  assert_eq_string("Hello, MixedCase!", greet("MixedCase"))
  assert_eq_string("Hello,   !", greet("  "))
}
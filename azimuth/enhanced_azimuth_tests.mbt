// 增强的 Azimuth 测试用例
// 包含10个高质量的 MoonBit 测试用例，覆盖各种场景和边缘情况

test "add_function_comprehensive_coverage" {
  // 全面测试加法函数的各种情况
  assert_eq(10, add(7, 3))  // 基本正数加法
  assert_eq(0, add(0, 0))   // 零值测试
  assert_eq(-10, add(-7, -3))  // 负数加法
  assert_eq(4, add(-7, 11))  // 混合符号加法
  assert_eq(1000000, add(500000, 500000))  // 大数加法
}

test "multiply_function_extensive_testing" {
  // 广泛测试乘法函数
  assert_eq(6, multiply(2, 3))  // 基本正数乘法
  assert_eq(0, multiply(5, 0))  // 零乘法
  assert_eq(-15, multiply(-5, 3))  // 负数乘法
  assert_eq(15, multiply(-5, -3))  // 负负得正
  assert_eq(1000000, multiply(1000, 1000))  // 大数乘法
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))  // 中文字符
  assert_eq_string("Hello, Москва!", greet("Москва"))  // 西里尔字符
  assert_eq_string("Hello, café!", greet("café"))  // 带重音符号
  assert_eq_string("Hello, 🚀!", greet("🚀"))  // Emoji支持
  assert_eq_string("Hello, ñoño!", greet("ñoño"))  // 特殊拉丁字符
}

test "divide_with_ceil_real_world_scenarios" {
  // 测试向上取整除法的真实世界场景
  // 场景1: 计算需要的运输车辆
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // 场景2: 计算需要的存储容器
  let items = 156
  let container_capacity = 25
  let containers_needed = divide_with_ceil(items, container_capacity)
  assert_eq(7, containers_needed)
}

test "complex_calculation_workflows" {
  // 测试复杂计算工作流
  // 场景：项目资源规划
  let team_members = 8
  let daily_hours = 8
  let project_days = 15
  let total_tasks = 120
  
  // 计算总工作小时数
  let total_hours = multiply(team_members, multiply(daily_hours, project_days))
  assert_eq(960, total_hours)
  
  // 计算每人需要完成的任务数
  let tasks_per_person = divide_with_ceil(total_tasks, team_members)
  assert_eq(15, tasks_per_person)
  
  // 计算完成任务需要的天数
  let days_needed = divide_with_ceil(total_tasks, multiply(team_members, 2))
  assert_eq(8, days_needed)
}

test "mathematical_properties_validation" {
  // 验证数学性质和恒等式
  let x = 42
  let y = 17
  let z = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, z)),
    add(multiply(x, y), multiply(x, z))
  )
  
  // 结合律验证
  assert_eq(
    add(add(x, y), z),
    add(x, add(y, z))
  )
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 边界值测试
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(-1, divide_with_ceil(-3, 2))
  assert_eq(-2, divide_with_ceil(3, -2))
}

test "resource_optimization_problems" {
  // 资源优化问题测试
  // 场景：仓库存储优化
  let total_items = 347
  let small_box_capacity = 15
  let large_box_capacity = 50
  
  // 使用小箱子
  let small_boxes = divide_with_ceil(total_items, small_box_capacity)
  assert_eq(24, small_boxes)
  
  // 使用大箱子
  let large_boxes = divide_with_ceil(total_items, large_box_capacity)
  assert_eq(7, large_boxes)
  
  // 验证存储容量
  let small_capacity = multiply(small_boxes, small_box_capacity)
  let large_capacity = multiply(large_boxes, large_box_capacity)
  
  assert_eq(360, small_capacity)
  assert_eq(350, large_capacity)
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello,   !", greet("   "))  // 空格字符串
  assert_eq_string("Hello, single!", greet("single"))  // 单个字符
  assert_eq_string("Hello, space test!", greet("space test"))  // 包含空格
  assert_eq_string("Hello, 12345!", greet("12345"))  // 数字字符串
  assert_eq_string("Hello, !@#$%^!", greet("!@#$%^"))  // 特殊字符
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))  // 大小写混合
}

test "business_calculation_comprehensive" {
  // 综合业务计算测试
  // 场景：零售业库存管理
  let product_a_qty = 125
  let product_b_qty = 87
  let product_c_qty = 234
  
  let box_capacity = 24
  
  // 计算每种产品需要的箱子数
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  
  assert_eq(6, boxes_a)  // ceil(125/24) = 6
  assert_eq(4, boxes_b)  // ceil(87/24) = 4
  assert_eq(10, boxes_c)  // ceil(234/24) = 10
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  
  assert_eq(20, total_boxes)
  assert_eq(480, total_capacity)
  
  // 计算剩余容量
  let total_products = add(add(product_a_qty, product_b_qty), product_c_qty)
  let remaining_capacity = subtract(total_capacity, total_products)
  
  assert_eq(34, remaining_capacity)
}

// 辅助函数
fn subtract(a : Int, b : Int) -> Int {
  a - b
}
// Azimuth 标准测试用例
// 包含8个高质量的标准 MoonBit 测试用例

// 导入 azimuth 库
use azimuth

test "basic_arithmetic_commutativity" {
  // 测试基本算术运算的交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 验证具体数值
  assert_eq(42, azimuth.add(15, 27))
  assert_eq(405, azimuth.multiply(15, 27))
}

test "divide_with_ceil_precision_scenarios" {
  // 测试向上取整除法的精度场景
  // 场景：计算资源分配
  
  // 云服务器实例分配
  let total_instances = 47
  let instances_per_node = 8
  let nodes_needed = azimuth.divide_with_ceil(total_instances, instances_per_node)
  assert_eq(6, nodes_needed)
  
  // 验证总容量
  let total_capacity = azimuth.multiply(nodes_needed, instances_per_node)
  assert_eq(48, total_capacity)
  
  // 验证剩余容量
  let remaining_capacity = azimuth.subtract(total_capacity, total_instances)
  assert_eq(1, remaining_capacity)
}

test "string_greeting_internationalization" {
  // 测试字符串问候的国际化支持
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, Universe!", azimuth.greet("Universe"))
  assert_eq_string("Hello, Café!", azimuth.greet("Café"))
  assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
  assert_eq_string("Hello, Москва!", azimuth.greet("Москва"))
}

test "mathematical_boundary_conditions" {
  // 测试数学边界条件
  // 零值测试
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(0, azimuth.multiply(5, 0))
  assert_eq(0, azimuth.divide_with_ceil(0, 10))
  assert_eq(0, azimuth.divide_with_ceil(10, 0))
  
  // 单位值测试
  assert_eq(10, azimuth.add(10, 0))
  assert_eq(10, azimuth.multiply(10, 1))
  assert_eq(1, azimuth.divide_with_ceil(10, 10))
  
  // 负数边界测试
  assert_eq(-10, azimuth.add(-5, -5))
  assert_eq(10, azimuth.multiply(-5, -2))
  assert_eq(-2, azimuth.divide_with_ceil(-15, 8))
}

test "complex_business_workflow" {
  // 测试复杂业务工作流
  // 场景：电商订单处理
  
  let order_items = 23
  let items_per_box = 6
  let boxes_needed = azimuth.divide_with_ceil(order_items, items_per_box)
  assert_eq(4, boxes_needed)
  
  // 计算运输成本
  let cost_per_box = 15
  let total_shipping_cost = azimuth.multiply(boxes_needed, cost_per_box)
  assert_eq(60, total_shipping_cost)
  
  // 计算包装材料
  let packaging_per_box = 2
  let total_packaging = azimuth.multiply(boxes_needed, packaging_per_box)
  assert_eq(8, total_packaging)
  
  // 计算总成本
  let total_cost = azimuth.add(total_shipping_cost, total_packaging)
  assert_eq(68, total_cost)
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 除零错误的各种情况
  assert_eq(0, azimuth.divide_with_ceil(0, 0))
  assert_eq(0, azimuth.divide_with_ceil(100, 0))
  assert_eq(0, azimuth.divide_with_ceil(-100, 0))
  
  // 极值运算测试
  let large_positive = 100000
  let large_negative = -100000
  
  assert_eq(0, azimuth.multiply(large_positive, 0))
  assert_eq(0, azimuth.multiply(large_negative, 0))
  assert_eq(large_positive, azimuth.add(large_positive, 0))
  assert_eq(large_negative, azimuth.add(large_negative, 0))
}

test "algorithm_efficiency_validation" {
  // 测试算法效率验证
  // 场景：比较不同算法的时间复杂度
  
  let input_size = 1000
  
  // 模拟 O(n) 线性算法
  let linear_operations = azimuth.multiply(input_size, 3)
  assert_eq(3000, linear_operations)
  
  // 模拟 O(log n) 对数算法（简化为除法运算）
  let log_operations = azimuth.divide_with_ceil(input_size, 100)
  assert_eq(10, log_operations)
  
  // 模拟 O(1) 常数算法
  let constant_operations = 5
  assert_eq(5, constant_operations)
  
  // 验证效率差异
  let linear_vs_log_ratio = azimuth.divide_with_ceil(linear_operations, log_operations)
  assert_eq(300, linear_vs_log_ratio)
}

test "real_world_project_planning" {
  // 测试真实世界项目规划场景
  // 场景：软件开发项目资源规划
  
  let story_points = 157
  let developer_capacity = 8
  let sprint_weeks = 2
  
  // 计算需要的开发人员数量
  let developers_needed = azimuth.divide_with_ceil(story_points, developer_capacity)
  assert_eq(20, developers_needed)
  
  // 计算项目持续时间（周）
  let project_duration_weeks = azimuth.multiply(sprint_weeks, developers_needed)
  assert_eq(40, project_duration_weeks)
  
  // 计算项目持续时间（月）
  let project_duration_months = azimuth.divide_with_ceil(project_duration_weeks, 4)
  assert_eq(10, project_duration_months)
  
  // 计算总工作量
  let total_effort = azimuth.multiply(developers_needed, developer_capacity)
  assert_eq(160, total_effort)
  
  // 验证工作量覆盖
  assert_true(total_effort >= story_points)
  
  // 计算额外工作量
  let extra_effort = azimuth.subtract(total_effort, story_points)
  assert_eq(3, extra_effort)
}
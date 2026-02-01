// 标准 MoonBit 测试用例 - Azimuth 项目增强版
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "mathematical_operations_comprehensive" {
  // 综合数学运算测试
  // 测试加法的各种情况
  assert_eq(10, add(7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  assert_eq(1000, add(999, 1))
  
  // 测试乘法的各种情况
  assert_eq(21, multiply(7, 3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(12, multiply(-4, -3))
  assert_eq(0, multiply(1000, 0))
  
  // 测试减法的各种情况
  assert_eq(4, subtract(7, 3))
  assert_eq(8, subtract(5, -3))
  assert_eq(-7, subtract(-4, 3))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法的综合场景测试
  // 基本正向除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "string_processing_international_support" {
  // 字符串处理国际化支持测试
  // 基本英文测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // Unicode字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  
  // 特殊字符和符号测试
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, @user!", greet("@user"))
  
  // 边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "business_logistics_packaging_optimization" {
  // 业务物流包装优化测试
  // 场景：电商仓库包装优化
  let product_a = 47
  let product_b = 63
  let product_c = 29
  let box_capacity = 12
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a, box_capacity)
  let boxes_b = divide_with_ceil(product_b, box_capacity)
  let boxes_c = divide_with_ceil(product_c, box_capacity)
  
  assert_eq(4, boxes_a)  // ceil(47/12) = 4
  assert_eq(6, boxes_b)  // ceil(63/12) = 6
  assert_eq(3, boxes_c)  // ceil(29/12) = 3
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  
  assert_eq(13, total_boxes)
  assert_eq(156, total_capacity)
  
  // 计算总产品数
  let total_products = add(add(product_a, product_b), product_c)
  assert_eq(139, total_products)
  
  // 验证容量充足
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(17, remaining_space)
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 15
  let y = 7
  let z = 3
  
  // 加法交换律：a + b = b + a
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律：a * b = b * a
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律：(a + b) + c = a + (b + c)
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律：(a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律：a * (b + c) = a * b + a * c
  assert_eq(
    multiply(x, add(y, z)),
    add(multiply(x, y), multiply(x, z))
  )
}

test "project_management_resource_allocation" {
  // 项目管理资源分配测试
  // 场景：多团队项目资源计算
  let team_a_size = 8
  let team_b_size = 6
  let team_c_size = 5
  let tasks_per_person = 4
  let sprint_capacity = 50
  
  // 计算每个团队的任务数
  let tasks_a = multiply(team_a_size, tasks_per_person)
  let tasks_b = multiply(team_b_size, tasks_per_person)
  let tasks_c = multiply(team_c_size, tasks_per_person)
  
  assert_eq(32, tasks_a)  // 8 * 4 = 32
  assert_eq(24, tasks_b)  // 6 * 4 = 24
  assert_eq(20, tasks_c)  // 5 * 4 = 20
  
  // 计算总任务数
  let total_tasks = add(add(tasks_a, tasks_b), tasks_c)
  assert_eq(76, total_tasks)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(2, sprints_needed)  // ceil(76/50) = 2
  
  // 验证总容量
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(100, total_capacity)
  
  // 计算剩余容量
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(24, remaining_capacity)
}

test "error_handling_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(100000, divide_with_ceil(large_num, 1))
  
  // 零元素性质测试
  assert_eq(large_num, add(large_num, 0))
  assert_eq(0, multiply(large_num, 0))
  assert_eq(large_num, subtract(large_num, 0))
}

test "complex_calculation_workflow_integration" {
  // 复杂计算工作流集成测试
  // 场景：生产成本计算
  
  // 原材料成本计算
  let material_a_price = 25
  let material_b_price = 18
  let material_a_quantity = 150
  let material_b_quantity = 200
  
  let material_a_cost = multiply(material_a_price, material_a_quantity)
  let material_b_cost = multiply(material_b_price, material_b_quantity)
  
  assert_eq(3750, material_a_cost)  // 25 * 150 = 3750
  assert_eq(3600, material_b_cost)  // 18 * 200 = 3600
  
  // 总材料成本
  let total_material_cost = add(material_a_cost, material_b_cost)
  assert_eq(7350, total_material_cost)
  
  // 人工成本计算
  let workers = 12
  let daily_wage = 120
  let working_days = 15
  let labor_cost = multiply(workers, multiply(daily_wage, working_days))
  
  assert_eq(21600, labor_cost)  // 12 * 120 * 15 = 21600
  
  // 总生产成本
  let total_production_cost = add(total_material_cost, labor_cost)
  assert_eq(28950, total_production_cost)
  
  // 包装和运输成本计算
  let items_per_box = 24
  let total_items = add(material_a_quantity, material_b_quantity)
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let shipping_cost_per_box = 35
  let total_shipping_cost = multiply(boxes_needed, shipping_cost_per_box)
  
  assert_eq(350, total_items)  // 150 + 200 = 350
  assert_eq(15, boxes_needed)  // ceil(350/24) = 15
  assert_eq(525, total_shipping_cost)  // 15 * 35 = 525
  
  // 最终总成本
  let final_total_cost = add(total_production_cost, total_shipping_cost)
  assert_eq(29475, final_total_cost)
}
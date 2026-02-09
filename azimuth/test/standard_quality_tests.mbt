// 标准 MoonBit 测试用例 - Azimuth 项目高质量测试集
// 包含10个全面的测试用例，覆盖核心功能和边界情况

test "quality_basic_arithmetic_operations" {
  // 基本算术运算测试
  // 测试加法的多种情况
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(1, @azimuth.add(-5, 6))
  
  // 测试乘法的多种情况
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  
  // 测试减法的多种情况
  @azimuth.assert_eq(5, @azimuth.subtract(12, 7))
  @azimuth.assert_eq(-5, @azimuth.subtract(7, 12))
  @azimuth.assert_eq(0, @azimuth.subtract(8, 8))
}

test "quality_divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数除法测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  
  // 边界情况测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  
  // 负数除法测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "quality_string_greeting_functionality" {
  // 字符串问候功能测试
  // 基本问候测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  
  // 特殊输入测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  
  // Unicode 和特殊字符测试
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, émoji 🚀!", @azimuth.greet("émoji 🚀"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
}

test "quality_mathematical_properties" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 交换律验证
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 恒等式验证
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  
  // 减法性质
  @azimuth.assert_eq(0, @azimuth.subtract(a, a))
  @azimuth.assert_eq(a, @azimuth.subtract(a, 0))
}

test "quality_business_packaging_scenario" {
  // 业务包装场景测试
  // 场景：计算装盒所需的盒子数量
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  // 验证盒子数量
  @azimuth.assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  @azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(1, remaining_space)
}

test "quality_complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源规划
  let team_members = 7
  let tasks_per_member = 6
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 10
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  let total_sprint_capacity = @azimuth.multiply(sprints_needed, sprint_capacity)
  let buffer_tasks = @azimuth.subtract(total_sprint_capacity, total_tasks)
  
  // 验证计算结果
  @azimuth.assert_eq(42, total_tasks)  // 7 * 6 = 42
  @azimuth.assert_eq(5, sprints_needed)  // ceil(42/10) = 5
  @azimuth.assert_eq(50, total_sprint_capacity)  // 5 * 10 = 50
  @azimuth.assert_eq(8, buffer_tasks)  // 50 - 42 = 8
}

test "quality_edge_cases_and_boundaries" {
  // 边界情况和极值测试
  // 大数运算测试
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  @azimuth.assert_eq(1000, @azimuth.divide_with_ceil(1000000, 1000))
  
  // 边界值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1000))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
  
  // 极值组合测试
  let extreme_result = @azimuth.add(@azimuth.multiply(1000, 1000), @azimuth.divide_with_ceil(999999, 1000))
  @azimuth.assert_eq(1000999, extreme_result)
}

test "quality_negative_number_operations" {
  // 负数运算测试
  // 负数基本运算
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(2, @azimuth.add(-3, 5))
  @azimuth.assert_eq(0, @azimuth.add(-10, 10))
  
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(5, -3))
  
  @azimuth.assert_eq(-2, @azimuth.subtract(-5, -3))
  @azimuth.assert_eq(-8, @azimuth.subtract(-5, 3))
  @azimuth.assert_eq(8, @azimuth.subtract(3, -5))
  
  // 负数除法测试
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-2, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-5, -3))
}

test "quality_sequential_operations" {
  // 连续操作测试
  let initial_value = 10
  
  // 连续计算序列
  let step1 = @azimuth.add(initial_value, 5)        // 15
  let step2 = @azimuth.multiply(step1, 3)          // 45
  let step3 = @azimuth.divide_with_ceil(step2, 8)   // 6
  let step4 = @azimuth.subtract(step3, 2)           // 4
  let step5 = @azimuth.multiply(step4, @azimuth.add(2, 3))   // 20
  let final_result = @azimuth.divide_with_ceil(step5, 6)  // 4
  
  @azimuth.assert_eq(4, final_result)
  
  // 验证中间步骤
  @azimuth.assert_eq(15, step1)
  @azimuth.assert_eq(45, step2)
  @azimuth.assert_eq(6, step3)
  @azimuth.assert_eq(4, step4)
  @azimuth.assert_eq(20, step5)
}

test "quality_real_world_application" {
  // 真实世界应用场景测试
  // 场景：电商订单处理和物流计算
  
  // 订单信息
  let order_items = 156
  let items_per_package = 24
  let packages_needed = @azimuth.divide_with_ceil(order_items, items_per_package)
  
  // 成本计算
  let cost_per_item = 25
  let shipping_cost_per_package = 15
  let item_cost_total = @azimuth.multiply(order_items, cost_per_item)
  let shipping_total = @azimuth.multiply(packages_needed, shipping_cost_per_package)
  let order_total = @azimuth.add(item_cost_total, shipping_total)
  
  // 折扣计算
  let discount_threshold = 3000
  let discount_rate = 10  // 10%
  let discount_amount = 
    if order_total > discount_threshold {
      @azimuth.divide_with_ceil(@azimuth.multiply(order_total, discount_rate), 100)
    } else {
      0
    }
  let final_total = @azimuth.subtract(order_total, discount_amount)
  
  // 验证计算结果
  @azimuth.assert_eq(7, packages_needed)  // ceil(156/24) = 7
  @azimuth.assert_eq(3900, item_cost_total)  // 156 * 25 = 3900
  @azimuth.assert_eq(105, shipping_total)  // 7 * 15 = 105
  @azimuth.assert_eq(4005, order_total)  // 3900 + 105 = 4005
  @azimuth.assert_eq(401, discount_amount)  // ceil(4005 * 10 / 100) = 401
  @azimuth.assert_eq(3604, final_total)  // 4005 - 401 = 3604
}
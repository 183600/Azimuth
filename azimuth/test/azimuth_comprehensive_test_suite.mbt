// Azimuth 综合测试套件
// 包含10个标准的 MoonBit 测试用例，覆盖所有核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(7, @azimuth.add(3, 4))
  @azimuth.assert_eq(-1, @azimuth.add(-5, 4))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  
  @azimuth.assert_eq(2, @azimuth.subtract(6, 4))
  @azimuth.assert_eq(-2, @azimuth.subtract(4, 6))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  
  // 负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 15
  let b = 7
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法恒等式
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(0, a))
  
  // 乘法恒等式
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(a, @azimuth.multiply(1, a))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let items_per_package = 12
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  
  @azimuth.assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(packages_needed, items_per_package)
  @azimuth.assert_eq(60, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(13, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源计算
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(48, total_tasks)  // 8 * 6 = 48
  @azimuth.assert_eq(4, sprints_needed)  // ceil(48/15) = 4
  
  // 验证总工作量
  let total_workload = @azimuth.multiply(sprints_needed, sprint_capacity)
  let extra_capacity = @azimuth.subtract(total_workload, total_tasks)
  @azimuth.assert_eq(12, extra_capacity)  // 4*15 - 48 = 12
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  @azimuth.assert_eq(1001, @azimuth.divide_with_ceil(1000000, 999))
  
  // 特殊减法情况
  @azimuth.assert_eq(1000000, @azimuth.subtract(1000000, 0))
  @azimuth.assert_eq(-1000000, @azimuth.subtract(0, 1000000))
}

test "negative_number_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
  
  @azimuth.assert_eq(-10, @azimuth.subtract(-5, 5))
  @azimuth.assert_eq(0, @azimuth.subtract(-5, -5))
  @azimuth.assert_eq(10, @azimuth.subtract(5, -5))
}

test "string_input_edge_cases" {
  // 字符串输入边界情况测试
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 0!", @azimuth.greet("0"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, leading space!", @azimuth.greet(" leading space"))
  @azimuth.assert_eq_string("Hello, trailing space !", @azimuth.greet("trailing space "))
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  let total_products = 250
  let box_capacity = 30
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  
  @azimuth.assert_eq(9, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(270, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(20, remaining_space)
  
  // 计算空间利用率百分比
  let utilization_percent = @azimuth.divide_with_ceil(
    @azimuth.multiply(total_products, 100), 
    total_capacity
  )
  @azimuth.assert_eq(93, utilization_percent)  // floor(25000/270) = 92%
}
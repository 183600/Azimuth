// 标准 MoonBit 测试用例 - 增强版
// 为 Azimuth 项目添加 8 个高质量测试用例

test "add_function_comprehensive" {
  // 全面测试加法函数
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(5, @azimuth.add(-8, 13))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
}

test "multiply_function_boundary" {
  // 测试乘法函数的边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 999999))
  @azimuth.assert_eq(999999, @azimuth.multiply(1, 999999))
  @azimuth.assert_eq(-999999, @azimuth.multiply(-1, 999999))
  @azimuth.assert_eq(999999, @azimuth.multiply(-1, -999999))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 字符支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, São Paulo!", @azimuth.greet("São Paulo"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
}

test "divide_with_ceil_logistics" {
  // 测试向上取整除法的物流场景
  let packages = 127
  let capacity_per_truck = 15
  let trucks_needed = @azimuth.divide_with_ceil(packages, capacity_per_truck)
  @azimuth.assert_eq(9, trucks_needed)
  
  // 验证总运力
  let total_capacity = @azimuth.multiply(trucks_needed, capacity_per_truck)
  @azimuth.assert_eq(135, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, packages)
  @azimuth.assert_eq(8, remaining_space)
}

test "complex_project_calculation" {
  // 测试复杂项目计算流水线
  let team_members = 6
  let tasks_per_member = 12
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_duration = 10
  let tasks_per_day = 8
  let sprint_capacity = @azimuth.multiply(sprint_duration, tasks_per_day)
  
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(72, total_tasks)    // 6 * 12 = 72
  @azimuth.assert_eq(80, sprint_capacity) // 10 * 8 = 80
  @azimuth.assert_eq(1, sprints_needed)   // ceil(72/80) = 1
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let x = 24
  let y = 18
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, 5)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 5))
  )
  
  // 验证具体数值
  @azimuth.assert_eq(42, @azimuth.add(24, 18))
  @azimuth.assert_eq(432, @azimuth.multiply(24, 18))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 50000
  let departments = 4
  let min_budget_per_dept = @azimuth.divide_with_ceil(total_budget, departments)
  
  @azimuth.assert_eq(12500, min_budget_per_dept)
  
  // 验证分配总额
  let allocated_total = @azimuth.multiply(min_budget_per_dept, departments)
  @azimuth.assert_eq(50000, allocated_total)
  
  // 验证预算利用率
  let utilization = @azimuth.divide_with_ceil(@azimuth.multiply(total_budget, 100), allocated_total)
  @azimuth.assert_eq(100, utilization)
}

test "error_handling_edge_cases" {
  // 错误处理和边缘情况测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  
  // 字符串边缘情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
}
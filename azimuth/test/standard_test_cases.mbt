// Azimuth 项目标准测试用例
// 包含 10 个高质量测试用例，覆盖核心功能和边界情况

using @builtin { inspect }

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  inspect(@azimuth.add(3, 5), content="8")
  inspect(@azimuth.multiply(3, 5), content="15")
  inspect(@azimuth.subtract(3, 5), content="-2")
  inspect(@azimuth.divide_with_ceil(5, 3), content="2")
  inspect(@azimuth.divide_with_ceil(9, 3), content="3")
}

test "mathematical_commutative_laws" {
  // 数学交换律验证
  let a = 12
  let b = 7
  
  // 加法交换律: a + b = b + a
  inspect(@azimuth.add(a, b), content=@azimuth.add(b, a).to_string())
  
  // 乘法交换律: a * b = b * a
  inspect(@azimuth.multiply(a, b), content=@azimuth.multiply(b, a).to_string())
  
  // 验证具体结果
  inspect(@azimuth.add(12, 7), content="19")
  inspect(@azimuth.multiply(12, 7), content="84")
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数情况
  inspect(@azimuth.divide_with_ceil(10, 3), content="4")
  inspect(@azimuth.divide_with_ceil(9, 3), content="3")
  inspect(@azimuth.divide_with_ceil(1, 100), content="1")
  
  // 负数情况
  inspect(@azimuth.divide_with_ceil(-10, 3), content="-3")
  inspect(@azimuth.divide_with_ceil(10, -3), content="-4")
  inspect(@azimuth.divide_with_ceil(-10, -3), content="4")
  
  // 边界情况
  inspect(@azimuth.divide_with_ceil(5, 0), content="0")
  inspect(@azimuth.divide_with_ceil(0, 5), content="0")
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  inspect(@azimuth.greet("World"), content="Hello, World!")
  inspect(@azimuth.greet("MoonBit"), content="Hello, MoonBit!")
  inspect(@azimuth.greet("世界"), content="Hello, 世界!")
  inspect(@azimuth.greet("🌟"), content="Hello, 🌟!")
  inspect(@azimuth.greet("café"), content="Hello, café!")
  inspect(@azimuth.greet(""), content="Hello, !")
  inspect(@azimuth.greet("123"), content="Hello, 123!")
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let items_per_package = 12
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  
  inspect(packages_needed, content="4")
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(packages_needed, items_per_package)
  inspect(total_capacity, content="48")
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  inspect(remaining_space, content="1")
}

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
  let x = 25
  
  // 加法恒等式: x + 0 = x
  inspect(@azimuth.add(x, 0), content=x.to_string())
  inspect(@azimuth.add(0, x), content=x.to_string())
  
  // 乘法恒等式: x * 1 = x, x * 0 = 0
  inspect(@azimuth.multiply(x, 1), content=x.to_string())
  inspect(@azimuth.multiply(1, x), content=x.to_string())
  inspect(@azimuth.multiply(x, 0), content="0")
  
  // 减法恒等式: x - x = 0, x - 0 = x
  inspect(@azimuth.subtract(x, x), content="0")
  inspect(@azimuth.subtract(x, 0), content=x.to_string())
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源管理
  let team_members = 6
  let tasks_per_member = 8
  let sprint_capacity = 15
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  inspect(total_tasks, content="48")  // 6 * 8 = 48
  inspect(sprints_needed, content="4")  // ceil(48/15) = 4
  
  // 验证总容量和剩余空间
  let total_capacity = @azimuth.multiply(sprints_needed, sprint_capacity)
  let remaining_capacity = @azimuth.subtract(total_capacity, total_tasks)
  inspect(total_capacity, content="60")  // 4 * 15 = 60
  inspect(remaining_capacity, content="12")  // 60 - 48 = 12
}

test "boundary_values_and_extremes" {
  // 边界值和极值测试
  // 大数运算
  inspect(@azimuth.add(1000000, 1000000), content="2000000")
  inspect(@azimuth.multiply(1000, 1000), content="1000000")
  
  // 除法精度测试
  inspect(@azimuth.divide_with_ceil(1000000, 3), content="333334")
  inspect(@azimuth.divide_with_ceil(2000000, 3), content="666667")
  
  // 边界除法
  inspect(@azimuth.divide_with_ceil(1, 1), content="1")
  inspect(@azimuth.divide_with_ceil(999999, 999999), content="1")
  inspect(@azimuth.divide_with_ceil(1000000, 1), content="1000000")
}

test "negative_number_operations" {
  // 负数运算测试
  // 加法负数测试
  inspect(@azimuth.add(-5, -5), content="-10")
  inspect(@azimuth.add(-5, 5), content="0")
  inspect(@azimuth.add(-10, 15), content="5")
  
  // 乘法负数测试
  inspect(@azimuth.multiply(-5, -5), content="25")
  inspect(@azimuth.multiply(-5, 5), content="-25")
  inspect(@azimuth.multiply(-5, 0), content="0")
  
  // 减法负数测试
  inspect(@azimuth.subtract(-5, 5), content="-10")
  inspect(@azimuth.subtract(5, -5), content="10")
  inspect(@azimuth.subtract(-5, -5), content="0")
  
  // 除法负数测试
  inspect(@azimuth.divide_with_ceil(-5, 3), content="-2")
  inspect(@azimuth.divide_with_ceil(-5, -3), content="2")
  inspect(@azimuth.divide_with_ceil(-5, 0), content="0")
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试：活动策划
  let participants = 87
  let tables_available = 12
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  inspect(tables_needed, content="11")
  
  // 检查桌子是否足够
  let tables_sufficient = tables_needed <= tables_available
  inspect(tables_sufficient, content="true")
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  inspect(total_seats, content="88")
  
  // 计算剩余座位
  let remaining_seats = @azimuth.subtract(total_seats, participants)
  inspect(remaining_seats, content="1")
  
  // 计算总成本
  let total_cost = @azimuth.multiply(total_seats, cost_per_chair)
  inspect(total_cost, content="2200")
  
  // 验证预算充足
  let budget_sufficient = total_cost <= total_budget
  inspect(budget_sufficient, content="true")
  
  // 计算剩余预算
  let remaining_budget = @azimuth.subtract(total_budget, total_cost)
  inspect(remaining_budget, content="800")
}
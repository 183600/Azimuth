// Azimuth 项目标准测试用例
// 包含 10 个高质量测试用例，覆盖核心功能和边界情况

using @builtin { inspect }

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  inspect(add(3, 5), content="8")
  inspect(multiply(3, 5), content="15")
  inspect(subtract(3, 5), content="-2")
  inspect(divide_with_ceil(5, 3), content="2")
  inspect(divide_with_ceil(9, 3), content="3")
}

test "mathematical_commutative_laws" {
  // 数学交换律验证
  let a = 12
  let b = 7
  
  // 加法交换律: a + b = b + a
  inspect(add(a, b), content=add(b, a).to_string())
  
  // 乘法交换律: a * b = b * a
  inspect(multiply(a, b), content=multiply(b, a).to_string())
  
  // 验证具体结果
  inspect(add(12, 7), content="19")
  inspect(multiply(12, 7), content="84")
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数情况
  inspect(divide_with_ceil(10, 3), content="4")
  inspect(divide_with_ceil(9, 3), content="3")
  inspect(divide_with_ceil(1, 100), content="1")
  
  // 负数情况
  inspect(divide_with_ceil(-10, 3), content="-3")
  inspect(divide_with_ceil(10, -3), content="-4")
  inspect(divide_with_ceil(-10, -3), content="4")
  
  // 边界情况
  inspect(divide_with_ceil(5, 0), content="0")
  inspect(divide_with_ceil(0, 5), content="0")
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  inspect(greet("World"), content="Hello, World!")
  inspect(greet("MoonBit"), content="Hello, MoonBit!")
  inspect(greet("世界"), content="Hello, 世界!")
  inspect(greet("🌟"), content="Hello, 🌟!")
  inspect(greet("café"), content="Hello, café!")
  inspect(greet(""), content="Hello, !")
  inspect(greet("123"), content="Hello, 123!")
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let items_per_package = 12
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  inspect(packages_needed, content="4")
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  inspect(total_capacity, content="48")
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  inspect(remaining_space, content="1")
}

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
  let x = 25
  
  // 加法恒等式: x + 0 = x
  inspect(add(x, 0), content=x.to_string())
  inspect(add(0, x), content=x.to_string())
  
  // 乘法恒等式: x * 1 = x, x * 0 = 0
  inspect(multiply(x, 1), content=x.to_string())
  inspect(multiply(1, x), content=x.to_string())
  inspect(multiply(x, 0), content="0")
  
  // 减法恒等式: x - x = 0, x - 0 = x
  inspect(subtract(x, x), content="0")
  inspect(subtract(x, 0), content=x.to_string())
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源管理
  let team_members = 6
  let tasks_per_member = 8
  let sprint_capacity = 15
  let total_tasks = multiply(team_members, tasks_per_member)
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  inspect(total_tasks, content="48")  // 6 * 8 = 48
  inspect(sprints_needed, content="4")  // ceil(48/15) = 4
  
  // 验证总容量和剩余空间
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  let remaining_capacity = subtract(total_capacity, total_tasks)
  inspect(total_capacity, content="60")  // 4 * 15 = 60
  inspect(remaining_capacity, content="12")  // 60 - 48 = 12
}

test "boundary_values_and_extremes" {
  // 边界值和极值测试
  // 大数运算
  inspect(add(1000000, 1000000), content="2000000")
  inspect(multiply(1000, 1000), content="1000000")
  
  // 除法精度测试
  inspect(divide_with_ceil(1000000, 3), content="333334")
  inspect(divide_with_ceil(2000000, 3), content="666667")
  
  // 边界除法
  inspect(divide_with_ceil(1, 1), content="1")
  inspect(divide_with_ceil(999999, 999999), content="1")
  inspect(divide_with_ceil(1000000, 1), content="1000000")
}

test "negative_number_operations" {
  // 负数运算测试
  // 加法负数测试
  inspect(add(-5, -5), content="-10")
  inspect(add(-5, 5), content="0")
  inspect(add(-10, 15), content="5")
  
  // 乘法负数测试
  inspect(multiply(-5, -5), content="25")
  inspect(multiply(-5, 5), content="-25")
  inspect(multiply(-5, 0), content="0")
  
  // 减法负数测试
  inspect(subtract(-5, 5), content="-10")
  inspect(subtract(5, -5), content="10")
  inspect(subtract(-5, -5), content="0")
  
  // 除法负数测试
  inspect(divide_with_ceil(-5, 3), content="-2")
  inspect(divide_with_ceil(-5, -3), content="2")
  inspect(divide_with_ceil(-5, 0), content="0")
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试：活动策划
  let participants = 87
  let tables_available = 12
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  inspect(tables_needed, content="11")
  
  // 检查桌子是否足够
  let tables_sufficient = tables_needed <= tables_available
  inspect(tables_sufficient, content="true")
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  inspect(total_seats, content="88")
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  inspect(remaining_seats, content="1")
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  inspect(total_cost, content="2200")
  
  // 验证预算充足
  let budget_sufficient = total_cost <= total_budget
  inspect(budget_sufficient, content="true")
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  inspect(remaining_budget, content="800")
}
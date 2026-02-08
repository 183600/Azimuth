// 标准 MoonBit 测试套件 - 为 Azimuth 项目添加
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "add_function_comprehensive" {
  // 加法函数综合测试
  inspect(add(3, 5), content="8")
  inspect(add(0, 0), content="0")
  inspect(add(-5, 3), content="-2")
  inspect(add(50, 50), content="100")
  inspect(add(-50, -50), content="-100")
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  inspect(multiply(3, 5), content="15")
  inspect(multiply(5, 0), content="0")
  inspect(multiply(-3, 5), content="-15")
  inspect(multiply(-3, -5), content="15")
  inspect(multiply(100, 100), content="10000")
}

test "divide_with_ceil_basic_functionality" {
  // 向上取整除法基本功能测试
  inspect(divide_with_ceil(8, 3), content="3")
  inspect(divide_with_ceil(6, 3), content="2")
  inspect(divide_with_ceil(1, 5), content="1")
  inspect(divide_with_ceil(0, 10), content="0")
  inspect(divide_with_ceil(10, 0), content="0")
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  inspect(divide_with_ceil(-8, 3), content="-2")
  inspect(divide_with_ceil(8, -3), content="-3")
  inspect(divide_with_ceil(-8, -3), content="3")
  inspect(divide_with_ceil(-1, 2), content="-1")
  inspect(divide_with_ceil(-1, -2), content="1")
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  inspect(greet("MoonBit"), content="Hello, MoonBit!")
  inspect(greet("World"), content="Hello, World!")
  inspect(greet(""), content="Hello, !")
  inspect(greet("123"), content="Hello, 123!")
  inspect(greet("中文"), content="Hello, 中文!")
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  inspect(subtract(10, 5), content="5")
  inspect(subtract(5, 10), content="-5")
  inspect(subtract(15, 15), content="0")
  inspect(subtract(10, -10), content="20")
  inspect(subtract(-10, 10), content="-20")
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 12
  let b = 7
  
  // 加法交换律
  inspect(add(a, b), content="19")
  inspect(add(b, a), content="19")
  
  // 乘法交换律
  inspect(multiply(a, b), content="84")
  inspect(multiply(b, a), content="84")
}

test "packaging_business_scenario" {
  // 包装业务场景测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  inspect(packages_needed, content="5")
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  inspect(total_capacity, content="40")
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  inspect(remaining_space, content="5")
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  inspect(total_tasks, content="48")  // 6 * 8 = 48
  inspect(sprints_needed, content="4")  // ceil(48/15) = 4
}

test "boundary_values_and_special_cases" {
  // 边界值和特殊情况测试
  // 除法边界情况
  inspect(divide_with_ceil(5, 5), content="1")
  inspect(divide_with_ceil(-5, -5), content="1")
  inspect(divide_with_ceil(0, 5), content="0")
  
  // 大数运算
  inspect(add(1000000, 1000000), content="2000000")
  inspect(multiply(10000, 10000), content="100000000")
  
  // 特殊问候情况
  inspect(greet("  "), content="Hello,   !")
  inspect(greet("special@chars"), content="Hello, special@chars!")
}
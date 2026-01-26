// Azimuth 项目新增标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

// 导入必要的模块
use azimuth

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(15, azimuth.add(8, 7))
  assert_eq(-3, azimuth.add(5, -8))
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(24, azimuth.multiply(6, 4))
  assert_eq(-18, azimuth.multiply(3, -6))
  assert_eq(0, azimuth.multiply(0, 123))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的各种情况
  assert_eq(5, azimuth.divide_with_ceil(14, 3))
  assert_eq(4, azimuth.divide_with_ceil(12, 3))
  assert_eq(1, azimuth.divide_with_ceil(2, 2))
  assert_eq(0, azimuth.divide_with_ceil(0, 10))
  assert_eq(0, azimuth.divide_with_ceil(25, 0))
  assert_eq(-4, azimuth.divide_with_ceil(-11, 3))
  assert_eq(5, azimuth.divide_with_ceil(-14, -3))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, 123!", azimuth.greet("123"))
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, 🌟!", azimuth.greet("🌟"))
  assert_eq_string("Hello, space test!", azimuth.greet("space test"))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 15
  let b = 9
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 测试负数情况
  assert_eq(azimuth.add(-7, 12), azimuth.add(12, -7))
  assert_eq(azimuth.multiply(-5, 8), azimuth.multiply(8, -5))
}

test "inventory_management_scenario" {
  // 库存管理场景测试
  let total_products = 158
  let box_capacity = 25
  
  // 计算需要的箱子数量
  let boxes_needed = azimuth.divide_with_ceil(total_products, box_capacity)
  assert_eq(7, boxes_needed)  // ceil(158/25) = 7
  
  // 计算总容量
  let total_capacity = azimuth.multiply(boxes_needed, box_capacity)
  assert_eq(175, total_capacity)
  
  // 计算剩余空间
  let remaining_space = azimuth.subtract(total_capacity, total_products)
  assert_eq(17, remaining_space)
}

test "project_planning_calculation" {
  // 项目规划计算测试
  let project_tasks = 67
  let team_members = 5
  let tasks_per_person_per_day = 3
  
  // 计算每日完成能力
  let daily_capacity = azimuth.multiply(team_members, tasks_per_person_per_day)
  
  // 计算需要的天数
  let days_needed = azimuth.divide_with_ceil(project_tasks, daily_capacity)
  assert_eq(5, days_needed)  // ceil(67/(5*3)) = 5
  
  // 验证总处理能力
  let total_capacity = azimuth.multiply(days_needed, daily_capacity)
  assert_eq(75, total_capacity)
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let x = 8
  let y = 6
  let z = 4
  
  // 分配律：x * (y + z) = x * y + x * z
  let left_side = azimuth.multiply(x, azimuth.add(y, z))
  let right_side = azimuth.add(
    azimuth.multiply(x, y),
    azimuth.multiply(x, z)
  )
  assert_eq(left_side, right_side)
  
  // 验证具体数值：8 * (6 + 4) = 8 * 6 + 8 * 4 = 80
  assert_eq(80, left_side)
  assert_eq(80, right_side)
}

test "error_handling_boundary_cases" {
  // 测试错误处理和边界情况
  // 除零错误处理
  assert_eq(0, azimuth.divide_with_ceil(15, 0))
  assert_eq(0, azimuth.divide_with_ceil(-15, 0))
  assert_eq(0, azimuth.divide_with_ceil(0, 0))
  
  // 边界值测试
  assert_eq(1, azimuth.divide_with_ceil(1, 1))
  assert_eq(-1, azimuth.divide_with_ceil(-1, -1))
  assert_eq(0, azimuth.divide_with_ceil(0, 1))
  assert_eq(0, azimuth.divide_with_ceil(0, -1))
}
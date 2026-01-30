// 标准 Azimuth 单元测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "azimuth_unit_basic_arithmetic" {
  // 基础算术运算综合测试
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-2, subtract(3, 5))
  assert_eq(2, divide_with_ceil(7, 4))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "azimuth_unit_math_properties" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
}

test "azimuth_unit_negative_operations" {
  // 负数运算测试
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-3, 5))
  assert_eq(-2, add(3, -5))
  
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-15, multiply(3, -5))
  
  assert_eq(8, subtract(3, -5))
  assert_eq(-8, subtract(-3, 5))
  assert_eq(2, subtract(-3, -5))
}

test "azimuth_unit_divide_ceil" {
  // 向上取整除法综合测试
  // 正数除法
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(1, divide_with_ceil(1, 4))
  
  // 负数除法
  assert_eq(-2, divide_with_ceil(-10, 4))
  assert_eq(-3, divide_with_ceil(10, -4))
  assert_eq(3, divide_with_ceil(-10, -4))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "azimuth_unit_greet_function" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "azimuth_unit_packaging_logic" {
  // 业务逻辑：包装计算测试
  let total_items = 47
  let items_per_package = 8
  
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  assert_eq(6, packages_needed)
  
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(48, total_capacity)
  
  let unused_space = subtract(total_capacity, total_items)
  assert_eq(1, unused_space)
}

test "azimuth_unit_workflow_calculation" {
  // 复杂计算工作流测试
  // 场景：项目管理计算
  let team_members = 6
  let tasks_per_member = 5
  let working_days = 8
  let tasks_per_day = 4
  
  // 计算总任务数
  let total_tasks = multiply(team_members, tasks_per_member)
  assert_eq(30, total_tasks)
  
  // 计算每日处理能力
  let daily_capacity = multiply(team_members, tasks_per_day)
  assert_eq(24, daily_capacity)
  
  // 计算需要的天数
  let days_needed = divide_with_ceil(total_tasks, daily_capacity)
  assert_eq(2, days_needed)
  
  // 验证在规定工作日内完成
  assert_true(days_needed <= working_days)
}

test "azimuth_unit_boundary_cases" {
  // 边界情况和极值测试
  // 零值运算
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  
  // 单位运算
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, add(1, 0))
  assert_eq(1, subtract(1, 0))
  
  // 相等数运算
  assert_eq(10, add(5, 5))
  assert_eq(25, multiply(5, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(1, divide_with_ceil(5, 5))
}
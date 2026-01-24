// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

// 导入必要的模块
use azimuth

test "integer_basic_arithmetic" {
  // 测试整数基本算术运算
  assert_eq(10, azimuth.add(7, 3))
  assert_eq(0, azimuth.add(5, -5))
  assert_eq(-8, azimuth.add(-3, -5))
  assert_eq(15, azimuth.multiply(3, 5))
  assert_eq(-15, azimuth.multiply(3, -5))
  assert_eq(0, azimuth.multiply(0, 42))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, azimuth.divide_with_ceil(10, 3))
  assert_eq(3, azimuth.divide_with_ceil(9, 3))
  assert_eq(1, azimuth.divide_with_ceil(1, 1))
  assert_eq(0, azimuth.divide_with_ceil(0, 5))
  assert_eq(0, azimuth.divide_with_ceil(10, 0))
  assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  assert_eq(4, azimuth.divide_with_ceil(-10, -3))
}

test "greet_function_comprehensive" {
  // 测试问候函数的综合功能
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 测试不同数值组合
  assert_eq(azimuth.add(-5, 10), azimuth.add(10, -5))
  assert_eq(azimuth.multiply(-4, 7), azimuth.multiply(7, -4))
}

test "business_calculation_scenario" {
  // 测试商业计算场景
  // 场景：计算项目预算分配
  let total_budget = 50000
  let team_members = 8
  let equipment_cost = 15000
  
  // 计算人均预算
  let budget_per_person = azimuth.divide_with_ceil(
    azimuth.subtract(total_budget, equipment_cost), 
    team_members
  )
  
  // 验证计算结果
  assert_eq(4375, budget_per_person)  // ceil((50000-15000)/8) = ceil(35000/8) = 4375
  
  // 验证总分配不超过剩余预算
  let total_allocated = azimuth.multiply(budget_per_person, team_members)
  assert_eq(35000, total_allocated)
}

test "edge_cases_and_boundaries" {
  // 测试边缘情况和边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 边界值加法
  assert_eq(max_int, azimuth.add(max_int, 0))
  assert_eq(min_int, azimuth.add(min_int, 0))
  
  // 边界值乘法
  assert_eq(max_int, azimuth.multiply(max_int, 1))
  assert_eq(min_int, azimuth.multiply(min_int, 1))
  assert_eq(0, azimuth.multiply(max_int, 0))
  
  // 特殊除法情况
  assert_eq(1, azimuth.divide_with_ceil(max_int, max_int))
  assert_eq(1, azimuth.divide_with_ceil(min_int, min_int))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算生产流程效率
  let raw_materials = 1000
  let defect_rate = 15  // 15%
  let processing_capacity = 50
  
  // 计算合格产品数量
  let qualified_products = azimuth.subtract(raw_materials, 
    azimuth.divide_with_ceil(azimuth.multiply(raw_materials, defect_rate), 100))
  
  // 计算需要的处理批次
  let batches_needed = azimuth.divide_with_ceil(qualified_products, processing_capacity)
  
  // 验证结果
  assert_eq(850, qualified_products)  // 1000 - ceil(1000*15/100) = 1000 - 150 = 850
  assert_eq(17, batches_needed)  // ceil(850/50) = 17
}

test "string_operations_validation" {
  // 测试字符串操作验证
  // 测试各种特殊字符和格式
  assert_eq_string("Hello,   !", azimuth.greet("  "))
  assert_eq_string("Hello, 12345!", azimuth.greet("12345"))
  assert_eq_string("Hello, @#$%!", azimuth.greet("@#$%"))
  assert_eq_string("Hello, mixedCASE!", azimuth.greet("mixedCASE"))
  assert_eq_string("Hello, \n\t!", azimuth.greet("\n\t"))
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  // 场景：计算服务器资源分配
  let total_requests = 1250
  let server_capacity = 150
  let backup_buffer = 20  // 20% 额外缓冲
  
  // 计算基础服务器数量
  let base_servers = azimuth.divide_with_ceil(total_requests, server_capacity)
  
  // 计算包含缓冲的总服务器数量
  let total_servers = azimuth.divide_with_ceil(
    azimuth.multiply(base_servers, 100 + backup_buffer), 
    100
  )
  
  // 验证结果
  assert_eq(9, base_servers)  // ceil(1250/150) = 9
  assert_eq(11, total_servers)  // ceil(9*120/100) = ceil(1080/100) = 11
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let x = 24
  let y = 18
  let z = 6
  
  // 分配律验证：x * (y + z) = x * y + x * z
  let distributive_left = azimuth.multiply(x, azimuth.add(y, z))
  let distributive_right = azimuth.add(
    azimuth.multiply(x, y), 
    azimuth.multiply(x, z)
  )
  assert_eq(distributive_left, distributive_right)
  
  // 结合律验证：(x + y) + z = x + (y + z)
  let associative_left = azimuth.add(azimuth.add(x, y), z)
  let associative_right = azimuth.add(x, azimuth.add(y, z))
  assert_eq(associative_left, associative_right)
  
  // 零元素和单位元验证
  assert_eq(x, azimuth.add(x, 0))
  assert_eq(x, azimuth.multiply(x, 1))
  assert_eq(0, azimuth.multiply(x, 0))
}
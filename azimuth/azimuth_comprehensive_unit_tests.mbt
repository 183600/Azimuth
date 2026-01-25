// Azimuth 项目综合单元测试
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

// 导入必要的模块
use azimuth

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(15, azimuth.add(8, 7))
  assert_eq(-3, azimuth.add(5, -8))
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(24, azimuth.multiply(6, 4))
  assert_eq(-18, azimuth.multiply(6, -3))
  assert_eq(0, azimuth.multiply(0, 42))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的各种情况
  assert_eq(4, azimuth.divide_with_ceil(10, 3))
  assert_eq(3, azimuth.divide_with_ceil(9, 3))
  assert_eq(1, azimuth.divide_with_ceil(1, 1))
  assert_eq(0, azimuth.divide_with_ceil(0, 5))
  assert_eq(0, azimuth.divide_with_ceil(10, 0))
  assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  assert_eq(4, azimuth.divide_with_ceil(-10, -3))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
  assert_eq_string("Hello, 123!", azimuth.greet("123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 分配律
  assert_eq(
    azimuth.multiply(a, azimuth.add(b, 3)),
    azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, 3))
  )
}

test "business_calculation_scenario" {
  // 测试商业计算场景
  // 场景：计算项目预算分配
  let total_budget = 50000
  let team_members = 8
  let equipment_cost = 15000
  
  // 计算人均预算
  let remaining_budget = azimuth.add(total_budget, -equipment_cost)
  let budget_per_person = azimuth.divide_with_ceil(remaining_budget, team_members)
  
  // 验证计算结果
  assert_eq(4375, budget_per_person)  // ceil((50000-15000)/8) = ceil(35000/8) = 4375
  
  // 验证总分配
  let total_allocated = azimuth.multiply(budget_per_person, team_members)
  assert_eq(35000, total_allocated)
}

test "edge_cases_and_boundaries" {
  // 测试边缘情况和边界值
  // 大数运算
  assert_eq(2000000, azimuth.add(1000000, 1000000))
  assert_eq(1000000, azimuth.multiply(1000, 1000))
  
  // 边界值测试
  assert_eq(1, azimuth.divide_with_ceil(1, 1))
  assert_eq(1, azimuth.divide_with_ceil(-1, -1))
  assert_eq(0, azimuth.divide_with_ceil(0, 1))
  assert_eq(0, azimuth.divide_with_ceil(0, -1))
  
  // 除零处理
  assert_eq(0, azimuth.divide_with_ceil(10, 0))
  assert_eq(0, azimuth.divide_with_ceil(-10, 0))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算生产流程效率
  let raw_materials = 1000
  let defect_rate = 15  // 15%
  let processing_capacity = 50
  
  // 计算合格产品数量
  let defective_products = azimuth.divide_with_ceil(
    azimuth.multiply(raw_materials, defect_rate), 
    100
  )
  let qualified_products = azimuth.add(raw_materials, -defective_products)
  
  // 计算需要的处理批次
  let batches_needed = azimuth.divide_with_ceil(qualified_products, processing_capacity)
  
  // 验证结果
  assert_eq(850, qualified_products)  // 1000 - ceil(1000*15/100) = 1000 - 150 = 850
  assert_eq(17, batches_needed)  // ceil(850/50) = 17
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
  let total_capacity_needed = azimuth.multiply(base_servers, 100 + backup_buffer)
  let total_servers = azimuth.divide_with_ceil(total_capacity_needed, 100)
  
  // 验证结果
  assert_eq(9, base_servers)  // ceil(1250/150) = 9
  assert_eq(11, total_servers)  // ceil(9*120/100) = ceil(1080/100) = 11
}